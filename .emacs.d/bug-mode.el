;; bug-mode.el --- Arista bugs UI front-end for emacs

;; Copyright (c) 2012 Arista Networks, Inc.  All rights reserved.
;; Arista Networks, Inc. Confidential and Proprietary.

;; Author: Paulo Gallo <pgallo@aristanetworks.com>
;; Keywords: convenience a4-bugs bugzilla

;; Commentary:

;; Bugs:

;; Todo / Wish List:
;;
;;  - new features:
;;
;;    - options for sorting failure summaries by the different fields
;;      (dut, test, project, hits, etc.)
;;
;;    - open mail mode for
;;      - By: user@domain pattern
;;
;;    - global history list for bug navigation (same as to web browser history, but for bugs)
;;
;;    - undo stack for modification operations
;;
;;    - new interactive operations
;;      - change# => trace (or could it be trace + describe?)
;;
;;    - support for calendar functions
;;
;;    - prompt for comment when blocking a bug ("Would you like to add 
;;      a comment about this xxx operation?")
;;
;;    - add font-lock for:
;;       - logs (?)
;;       - python tracebacks (?)
;;       - anything else?
;;
;;    - bug list mode (e.g. for 'a4 bugs -u <user>' )
;;      - sort by different fields
;;
;;    - minor mode (e.g. for shell, just like compiler minor mode for shell?)
;;
;;; Code:

(defvar bug-mode-version "1.0.3" "The Current Bug-Mode Revision.")

(require 'p4)
(require 'atest-mode)

(eval-when-compile (require 'cl))

(defgroup bug-mode ()
  "Major mode for managing bugs at Arista."
  :version "21.1"
  :group 'tools
  :group 'bug)

(defcustom bug-default-quiet-mode nil
  "If non-nil, any bug changes do not generate email notification."
  :type 'boolean
  :group 'bug-mode)

;; dependency related customizable variables

(defcustom bug-default-deps-on-p nil
  "If non-nil, dependencies section is shown by default."
  :type 'boolean
  :group 'bug-mode)

(defcustom bug-default-deps-display-type "flat"
  "Default deps-display type:
  \"flat\"     = flat display
  \"graph\"    = graph display
  \"altGraph\" = alternative graph display"
  :type 'string
  :group 'bug-mode)

(defcustom bug-default-deps-open-only-p t
  "If non-nil, show only open blocking bugs."
  :type 'boolean
  :group 'bug-mode)

(defcustom bug-default-deps-direct-only-p t
  "If non-nil, show only directly blocking bugs."
  :type 'boolean
  :group 'bug-mode)

(defcustom bug-cur-project-name nil
  "If non-nil, contains name of current project for project management 
functionality purposes."
  :type 'boolean
  :group 'bug-mode)

(defcustom bug-tmp-dir "/tmp"
  "Temporary directory where to create temporary files (e.g. download of log files,
etc."
  :type 'string
  :group 'bug-mode)

(defcustom bug-auto-atest-mode t
  "If non-nil, automatically set autotest-mode when opening an autotest log URL."
  :type 'string
  :group 'bug-mode)

(defcustom bug-mode-hook nil
  "Run after setting up the `bug-mode' major mode."
  :type 'hook
  :group 'bug-mode)

(defcustom bug-beautify-comments-p nil
  "If non-nil, beautify auotmatically generated comments."
  :type 'boolean
  :group 'bug-mode)

;;; --------------------------------------------------------------------
;;; global vars

(defvar bug-users-history '(""))
(defvar bug-fixed-by-history '(""))
(defvar bug-bug-id-history '(""))
(defvar bug-bug-version-history '(""))
(defvar bug-bug-deadline-history '(""))

(defvar bug-a4-bug-cmd "a4 bug")

(defvar bug-emacsclient-cmd "emacsclient23")

(defvar bug-debug-p nil "If non-nil, enable debugging mode.")

(defvar bug-last-id-requested "")

(defvar bug-a4-bugs-wide-checked-p nil)
(defvar bug-a4-bugs-wide-supported-p nil)

;;; --------------------------------------------------------------------
;;; buffer-local variables

(defvar bug-cur-bug-id "0")
(defvar bug-cur-bug-desc "")
(defvar bug-cur-bug-alias nil)
(defvar bug-cur-assigned-to nil)

(defvar bug-show-all-comments-p nil)
(defvar bug-show-proj-comments-p nil)
(defvar bug-show-activity-p nil)
(defvar bug-show-fsummary-by-dut-p nil)
(defvar bug-show-fsummary-by-test-p nil)
(defvar bug-show-fsummary-by-proj-p nil)
(defvar bug-show-det-fsummary-p nil)
(defvar bug-link-list nil)
(defvar bug-proj-ref-list nil)
(defvar bug-quiet-mode bug-default-quiet-mode)

(defvar bug-deps-on-p bug-default-deps-on-p)
(defvar bug-deps-display-type bug-default-deps-display-type)
(defvar bug-deps-open-only-p bug-default-deps-open-only-p)
(defvar bug-deps-direct-only-p bug-default-deps-direct-only-p)
(defvar bug-deps-status-str "")

;; buffer positions

(defvar bug-header-section-end nil)

(defvar bug-deps-section-start nil)
(defvar bug-deps-section-end nil)

(defvar bug-blocks-section-start nil)
(defvar bug-blocks-section-end nil)

(defvar bug-activity-section-start nil)
(defvar bug-activity-section-end nil)

(defvar bug-fsummary-section-start nil)
(defvar bug-fsummary-section-end nil)

(defvar bug-fsummary-by-dut-start nil)
(defvar bug-fsummary-by-dut-end nil)

(defvar bug-fsummary-by-proj-start nil)
(defvar bug-fsummary-by-proj-end nil)

(defvar bug-fsummary-by-test-start nil)
(defvar bug-fsummary-by-test-end nil)

;;; --------------------------------------------------------------------
;;; keymap for bug-mode

(easy-mmode-defmap bug-mode-shared-map
  '(("n" . bug-next-section)
    ("p" . bug-prev-section)
    ("e" . bug-last-section)
    ("a" . bug-toggle-all-comments)
    ("P" . bug-toggle-proj-comments)
    ("A" . bug-toggle-activity)
    ("s" . bug-fsummary-menu)
    ("S" . bug-det-fsummary-menu)
    ("d" . bug-deps-menu)
    ("r" . bug-refresh)
    ("?" . bug-quick-help)
    ("h" . bug-help)
    ("Q" . bug-toggle-quiet-mode)
    ("t" . bug-trace-changelist)
    ("l" . bug-show-logre)
    ("q" . bug-kill-buffer)

    ("\C-ck" . bug-kill-all-other-buffers)
    ("\C-c\C-b" . bug-copy-bug-BUG-id)
    ("\C-c\C-w" . bug-copy-bug-id)
    ("\C-c\C-q" . bug-copy-bug-id-and-desc)
    ("\C-cm" . bug-mail)

    ("\C-i" . bug-next-link)
    (":" . bug-main-menu)
    )
  "Basic keymap for `bug-mode', bound to various prefix keys.")

(easy-mmode-defmap bug-mode-map
  `(;("\e" . ,bug-mode-shared-map)
    ("n" . bug-next-section)
    (" " . bug-next-section)
    ("p" . bug-prev-section)
    ("e" . bug-last-section)
    ("a" . bug-toggle-all-comments)
    ("P" . bug-toggle-proj-comments)
    ("A" . bug-toggle-activity)
    ("s" . bug-fsummary-menu)
    ("S" . bug-det-fsummary-menu)
    ("d" . bug-deps-menu)
    ("r" . bug-refresh)
    ("?" . bug-quick-help)
    ("h" . bug-help)
    ("Q" . bug-toggle-quiet-mode)
    ("t" . bug-trace-changelist)
    ("l" . bug-show-logre)
    ("q" . bug-kill-buffer)

    ("\C-ck" . bug-kill-all-other-buffers)
    ("\C-c\C-b" . bug-copy-bug-BUG-id)
    ("\C-c\C-w" . bug-copy-bug-id)
    ("\C-c\C-q" . bug-copy-bug-id-and-desc)
    ("\C-cm" . bug-mail)

    ("\C-i" . bug-next-link)
    (":" . bug-main-menu)
    )
  "Keymap for `bug-mode'.  See also `bug-mode-shared-map'.")

(defcustom bug-minor-mode-prefix "\C-c="
  "Prefix key for `bug-minor-mode' commands."
  :type '(choice (string "\e") (string "C-c=") string)
  :group 'bug-mode)

(easy-mmode-defmap bug-minor-mode-map
  `((,bug-minor-mode-prefix . ,bug-mode-shared-map))
  "Keymap for `bug-minor-mode'.  See also `bug-mode-shared-map'.")

(defun bug-kill-buffer ()
  (interactive)
  (kill-buffer nil))

;;; ==========================================================================
;;; font-lock support

(defface bug-section-header
  '((((class color) (min-colors 88) (background light))
     :background "grey80")
    (((class color) (min-colors 88) (background dark))
     :background "#333333")
    (((class color) (background light))
     :background "white")
    (((class color) (background dark))
     :background "white")
    (t :weight bold))
  "`bug-mode' face for a4 bugs output section headers."
  :group 'bug-mode)
;;(define-obsolete-face-alias 'bug-section-header-face 'bug-section-header "22.1")
(defvar bug-section-header-face 'bug-section-header)

(defface bug-de-emphasize
  '((((class color) (min-colors 88) (background light))
     :foreground "dark grey")
    (((class color) (min-colors 88) (background dark))
     :foreground "dark grey")
    (((class color) (background light))
     :foreground "white")
    (((class color) (background dark))
     :foreground "white")
    (t :weight bold))
  "`bug-mode' face for things to be de-emphasized."
  :group 'bug-mode)
;;(define-obsolete-face-alias 'bug-de-emphasize-face 'bug-de-emphasize "22.1")
(defvar bug-de-emphasize-face 'bug-de-emphasize)

(defvar bug-bold-face 'bold)

(defvar bug-section-header-re "^------+\\s-\\(.*\\)\\s--+------\n")

(defalias 'bug-bug-id-face 'font-lock-function-name-face)
(defalias 'bug-change-num-face 'font-lock-function-name-face)

;(defvar 
(setq bug-font-lock-keywords
  `((,bug-section-header-re . bug-section-header-face)
    ("^------+\\s-BUG\\s-+[0-9]+\\s--+------\n\\([0-9]+\\s-+.*\\)$"
     (1 bug-bold-face))
    ; ("^[0-9]+\\s-+.*$" . bug-bold-face)
    ("^By:\\s-*.*@.*$" . font-lock-variable-name-face)
    ("^----*\\s-+Comments\\s-+[-0-9].*suppressed.*$" . bug-de-emphasize-face)
    ("^----*\\s-+Run with --all-comments.*$" . bug-de-emphasize-face)
    ("^----*\\s-+Type: a-all comments.*$" . bug-de-emphasize-face)
    (" | " . bug-de-emphasize-face)
    ("\\(\\\\_\\)\\s-+\\([0-9]+\\)"
     (1 bug-de-emphasize-face) (2 font-lock-function-name-face))
    ("\\(\\\\_\\)\\s-+\\([0-9]+\\)"
     (1 bug-de-emphasize-face) (2 font-lock-function-name-face))
    ))

(defconst bug-font-lock-defaults
  '(bug-font-lock-keywords t nil nil nil (font-lock-multiline . nil)))

(defvar bug-imenu-generic-expression
  '((nil "------+\\s-\\(.*\\)\\s--+------\n" 1)
    ))

;;; ==========================================================================
;;; help related functions

(defun bug-help ()
  (interactive)
  (describe-function 'bug-mode))

(defun bug-quick-help ()
  (interactive)
  (message "Quick help: :-menu  a-toggle-all-comments  h-help  d-deps  n-ext-section  TAB-next-link"))


;;; ==========================================================================
;;; "menu" related stuff

;;; --------------------------------------------------------------------
;;; main menu

(setq bug-main-menu-op-alist
      '(("a" . bug-add-operations)
        ("d" . bug-del-operations)
        ("s" . bug-set-operations)
        ("p" . bug-proj-operations)
        ))
(defun bug-main-menu (selection)
  (interactive "cMenu:  a-add  d-delete  s-set  p-project-ops")
  (bug-execute-op selection bug-main-menu-op-alist))

;-----------------------------------------------
; add menu

(setq bug-add-op-alist
      '(("b" . (("prompt" . "Add to blocked list:")
                ("option" . "--blocks")
                ("history" . bug-bug-id-history)
                ;;("comma-sep" . t)  ???
                ))
        ("c" . bug-add-comment)
        ("C" . (("prompt" . "Add to CC: list (default domain @aristanetworks.com):")
                ("option" . "--cc")
                ("history" . bug-users-history)
                ("is-email" . t)
                ))
        ("f" . (("prompt" . "Add changelist # to fix-list:")
                ("option" . "--fixed-by")
                ("history" . bug-fixed-by-history)
                ;;("comma-sep" . t)  ???
                ))
        ("r" . bug-op-not-implemented)
        ("v" . (("prompt" . "Add version(s) to version-introduced list (e.g. 4.8.8):")
                ("option" . "--version-introduced")
                ("history" . bug-version-history)
                ("comma-sep" . t)
                ))
        ("V" . (("prompt" . "Add version(s) to version-fixed list (e.g. 4.8.8):")
                ("option" . "--version-fixed")
                ("history" . bug-version-history)
                ("comma-sep" . t)
                ))
        ))
(defun bug-add-operations ()
  (call-interactively 'bug-do-add-operations))
(defun bug-do-add-operations (selection)
  (interactive "cAdd:  b-locked  c-omment  C-CC  f-ixedby  r-RN  v-introduced  V-fixed")
;;  (interactive "cAdd: (b)block (c)comment (C)CC (f)fixedby (r)RN (v)ver-introd (V)ver-fixed")
;;  (interactive "cAdd: b-block c-comment C-CC f-fixedby r-RN v-introd V-fixed")
  (bug-execute-op selection bug-add-op-alist))

;-----------------------------------------------
; delete operations

(setq bug-del-op-alist
      '(("b" . (("prompt" . "Remove from blocked list (all=remove all):")
                ("option" . "--does-not-block")
                ("history" . bug-bug-id-history)
                ;;("comma-sep" . t)  ???
                ))
        ("C" . (("prompt" . "Remove from CC: list (default domain @aristanetworks.com):")
                ("option" . "--do-not-cc")
                ("history" . bug-users-history)
                ("is-email" . t)
                ))
        ("d" . (("prompt" . "Remove from CC: list (default domain @aristanetworks.com):")
                ("option" . "--do-not-cc")
                ("history" . bug-users-history)
                ("is-email" . t)
                ))
        ("f" . (("prompt" . "Remove changelist # from fix-list (all=remove all):")
                ("option" . "--not-fixed-by")
                ("history" . bug-fixed-by-history)
                ;;("comma-sep" . t)  ???
                ))
        ("r" . bug-op-not-implemented)
        ("v" . (("prompt" . "Remove version(s) from version-introduced list (all=remove all):")
                ("option" . "--no-version-introduced")
                ("history" . bug-version-history)
                ("comma-sep" . t)
                ))
        ("V" . (("prompt" . "Remove version(s) from version-fixed list (all=remove all):")
                ("option" . "--no-version-fixed")
                ("history" . bug-version-history)
                ("comma-sep" . t)
                ))
        ))
(defun bug-del-operations ()
  (call-interactively 'bug-do-del-operations))
(defun bug-do-del-operations (selection)
  (interactive "cDelete:  b-locked  C-CC  d-upe  f-ixedby  r-RN  v-introduced  V-fixed")
  (bug-execute-op selection bug-del-op-alist))

;-----------------------------------------------
; set operations

(defun bug-get-sev-selection (selection)
  (interactive "cSeverity: u-unspec  n-non-escape  1-sev1  2-sev2  3-sev3")
  (char-to-string selection))

(defun bug-get-prio-selection (selection)
  (interactive "cPriority: 1-df1  2-df2  3-df3  j-jail  m-mu  t-tvl  w-tw")
  (char-to-string selection))

(defun bug-filter-deadline (option value)
  "Filter parameters for the 'set deadline' operation.
If value is 'no', change option to --no-deadline
Return: list of (option value)"
  (when (string= value "no")
    (setq option "--no-deadline"
          value nil))
  (list option value)
  )

(setq bug-set-op-alist
      '(
        ("a" . (("prompt" . "Alias:")
                ("option" . "--alias")
                ))
        ("d" . (("prompt" . "Duplicate of:")
                ("option" . "--status=r/dup --dup-of")
                ))
        ("e" . (("prompt" . "Estimate:")
                ("option" . "--estimate")
                ))
        ("p" . (("prompt" . "Product:")
                ("option" . "-p")
                ))
        ("P" . (("option" . "--priority")
                ("selection-map" . 
                 (("1" . "df1")
                  ("2" . "df2")
                  ("3" . "df3")
                  ("j" . "jail")
                  ("m" . "mu")
                  ("t" . "tvl")
                  ("w" . "tw")))
                ("get-sel-func" . bug-get-prio-selection)
                ))
        ("s" . (("prompt" . "Summary:")
                ("option" . "--summary")
                ("initial-input-func" . bug-get-cur-bug-desc)
                ))
        ("S" . (("prompt" . "Status (one of: a, c/dup, c/f, c/wf, c/wfm, n, xr/dup, r/f, r/wf, r/wfm, ro, u):")
                ("option" . "--status")
                ))
        ("u" . (("prompt" . "Assign bug to:")
                ("option" . "--user")
                ))
        ("y" . (("option" . "--severity")
                ("selection-map" . 
                 (("u" . "u")
                  ("n" . "n")
                  ("1" . "sev1")
                  ("2" . "sev2")
                  ("3" . "sev3")))
                ("get-sel-func" . bug-get-sev-selection)
                ))
        ("D" . (("prompt" . "Deadline (YYYY-MM-DD or 'no' for none):")
                ("option" . "--deadline")
                ("history" . bug-deadline-history)
                ("filter-func" . bug-filter-deadline)
                ))
        ))
(defun bug-set-operations ()
  (call-interactively 'bug-do-set-operations))
(defun bug-do-set-operations (selection)
  (interactive "cSet:  a-lias  d-upe  e-stimate  p-roduct  P-rio  s-ummary  S-tatus  u-ser  y-sev  D-eadline")
  (bug-execute-op selection bug-set-op-alist))

;-----------------------------------------------
; project operations

(setq bug-proj-op-alist
      '(("s" . bug-set-proj)
        ("i" . bug-proj-ignore)
        ("l" . bug-proj-looking)
        ("m" . bug-proj-mustfix)
        ("t" . bug-proj-triage)
        ("n" . bug-proj-none)
        ))
(defun bug-proj-operations ()
  (call-interactively 'bug-do-proj-operations))
(defun bug-do-proj-operations (selection)
  (interactive "cProject ops:  s-set-cur-proj |  i-ignore  l-looking  m-mustfix  t-triage  n-none")
  (bug-execute-op selection bug-proj-op-alist))

(defun bug-proj-move-to-list (lst)
  (let (cmd)
    (if (not bug-cur-project-name)
        (setq bug-cur-project-name
              (read-string "Current project not set. Enter project name:")))
    (setq cmd (bug-a4-bug-cmd-from-settings))
    ; add list to block, if provided
    (if (not (string= lst ""))
        (setq cmd (concat cmd (format " -b %s-%s"  bug-cur-project-name lst))))
    ; unblock all other lists
    (if (not (string= lst "ignore"))
        (setq cmd (concat cmd (format " -B %s-ignore" bug-cur-project-name))))
    (if (not (string= lst "looking"))
        (setq cmd (concat cmd (format " -B %s-looking" bug-cur-project-name))))
    (if (not (string= lst "mustfix"))
        (setq cmd (concat cmd (format " -B %s-mustfix" bug-cur-project-name))))
    (if (not (string= lst "triage"))
        (setq cmd (concat cmd (format " -B %s-triage" bug-cur-project-name))))
    (bug-invoke-cmd cmd)
    ))

(defun bug-proj-ignore ()
  (bug-proj-move-to-list "ignore")
  )

(defun bug-proj-looking ()
  (bug-proj-move-to-list "looking")
  )

(defun bug-proj-mustfix ()
  (bug-proj-move-to-list "mustfix")
  )

(defun bug-proj-triage ()
  (bug-proj-move-to-list "triage")
  )

(defun bug-proj-none ()
  (bug-proj-move-to-list "")
  )

;;; --------------------------------------------------------------------
;;; dependencies menu

(setq bug-deps-menu-op-alist
      '(("o" . bug-deps-off)
        ("d" . bug-deps-toggle-direct-only)
        ("g" . bug-deps-toggle-graph)
        ("a" . bug-deps-toggle-show-all)))
(defun bug-deps-menu (selection)
  (interactive "cDependencies:  o-on/off-toggle  d-direct/all  g-graph/flat  a-open/all")
  (bug-execute-op selection bug-deps-menu-op-alist))

(defun bug-deps-update (new-on-p new-type new-open-only-p new-direct-only-p)
  (let ((prev-deps-on-p bug-deps-on-p)
        deps-section-start)
    (when (or (not (eq bug-deps-on-p new-on-p))
              (not (string= bug-deps-display-type new-type))
              (not (eq bug-deps-open-only-p new-open-only-p))
              (not (eq bug-deps-direct-only-p new-direct-only-p))
              )
      (setq bug-deps-on-p new-on-p)
      (setq bug-deps-display-type new-type)
      (setq bug-deps-open-only-p new-open-only-p)
      (setq bug-deps-direct-only-p new-direct-only-p)
      (bug-refresh)
      ; if we're showing dependencies, go to the beginning of deps section
      (when bug-deps-on-p
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward "^-------+\\s-Dependencies\\s-+-+------$"
                                 nil t)
              (setq deps-section-start (line-beginning-position))
            ))
        (if deps-section-start
            (goto-char deps-section-start))
        )
      )
    ))

(defun bug-deps-off ()
  (bug-deps-update (not bug-deps-on-p) 
                   bug-deps-display-type
                   bug-deps-open-only-p
                   bug-deps-direct-only-p)
  )

(defun bug-deps-toggle-graph ()
  (let (new-type)
    (setq new-type (if (not bug-deps-on-p)
                       "graph"
                     (if (string= bug-deps-display-type "flat") "graph" "flat")))
    (bug-deps-update t
                     new-type
                     bug-deps-open-only-p
                     (if (string= new-type "graph") nil bug-deps-direct-only-p))
    ))

(defun bug-deps-toggle-show-all ()
  (bug-deps-update t
                   bug-deps-display-type
                   (if (not bug-deps-on-p)
                       t
                     (not bug-deps-open-only-p))
                   bug-deps-direct-only-p)
  )

(defun bug-deps-toggle-direct-only ()
  (let (new-direct-only-p)
    (setq new-direct-only-p (if (not bug-deps-on-p)
                                t
                              (not bug-deps-direct-only-p)))
    (bug-deps-update t
                     bug-deps-display-type
                     bug-deps-open-only-p
                     new-direct-only-p)
    ))

;;; --------------------------------------------------------------------
;;; failure summary menus

(setq bug-fsummary-menu-op-alist
      '(("d" . bug-show-fsummary-by-dut)
        ("t" . bug-show-fsummary-by-test)
        ("p" . bug-show-fsummary-by-proj)
        ("a" . bug-show-fsummary-all)
        ("n" . bug-show-fsummary-none)
        ))

(defun bug-fsummary-menu (selection)
  (interactive "cShow failure summary by:  d-dut  t-test  p-project  (a-all  n-none)")
  (bug-execute-op selection bug-fsummary-menu-op-alist))

(defun bug-show-fsummary-by-dut ()
  (setq bug-show-fsummary-by-dut-p t)
  (setq bug-show-fsummary-by-test-p nil)
  (setq bug-show-fsummary-by-proj-p nil)
  (setq bug-show-det-fsummary-p nil)
  (bug-refresh)
  (bug-goto-section "BUG FAILURES BY DUT")
  )

(defun bug-show-fsummary-by-test ()
  (setq bug-show-fsummary-by-dut-p nil)
  (setq bug-show-fsummary-by-test-p t)
  (setq bug-show-fsummary-by-proj-p nil)
  (setq bug-show-det-fsummary-p nil)
  (bug-refresh)
  (bug-goto-section "BUG FAILURES BY TEST")
  )

(defun bug-show-fsummary-by-proj ()
  (setq bug-show-fsummary-by-dut-p nil)
  (setq bug-show-fsummary-by-test-p nil)
  (setq bug-show-fsummary-by-proj-p t)
  (setq bug-show-det-fsummary-p nil)
  (bug-refresh)
  (bug-goto-section "BUG FAILURES BY TEST")
  )

(defun bug-show-fsummary-all ()
  (setq bug-show-fsummary-by-dut-p t)
  (setq bug-show-fsummary-by-test-p t)
  (setq bug-show-fsummary-by-proj-p t)
  (setq bug-show-det-fsummary-p nil)
  (bug-refresh)
  (bug-goto-section "BUG FAILURES BY DUT")
  )

(defun bug-show-fsummary-none ()
  (setq bug-show-fsummary-by-dut-p nil)
  (setq bug-show-fsummary-by-test-p nil)
  (setq bug-show-fsummary-by-proj-p nil)
  (setq bug-show-det-fsummary-p nil)
  (bug-refresh)
  )

(setq bug-det-fsummary-menu-op-alist
      '(("d" . bug-show-det-fsummary-by-dut)
        ("t" . bug-show-det-fsummary-by-test)
        ("p" . bug-show-det-fsummary-by-proj)
        ("a" . bug-show-det-fsummary-all)
        ("n" . bug-show-fsummary-none)
        ))

(defun bug-det-fsummary-menu (selection)
  (interactive "cShow *detailed* failure summary by:  d-dut  t-test  p-project  (a-all  n-none)")
  (bug-execute-op selection bug-det-fsummary-menu-op-alist))

(defun bug-show-det-fsummary-by-dut ()
  (setq bug-show-fsummary-by-dut-p t)
  (setq bug-show-fsummary-by-test-p nil)
  (setq bug-show-fsummary-by-proj-p nil)
  (setq bug-show-det-fsummary-p t)
  (bug-refresh)
  (bug-goto-section "BUG FAILURES BY DUT")
  )

(defun bug-show-det-fsummary-by-test ()
  (setq bug-show-fsummary-by-dut-p nil)
  (setq bug-show-fsummary-by-test-p t)
  (setq bug-show-fsummary-by-proj-p nil)
  (setq bug-show-det-fsummary-p t)
  (bug-refresh)
  (bug-goto-section "BUG FAILURES BY TEST")
  )

(defun bug-show-det-fsummary-by-proj ()
  (setq bug-show-fsummary-by-dut-p nil)
  (setq bug-show-fsummary-by-test-p nil)
  (setq bug-show-fsummary-by-proj-p t)
  (setq bug-show-det-fsummary-p t)
  (bug-refresh)
  (bug-goto-section "BUG FAILURES BY PROJECT")
  )

(defun bug-show-det-fsummary-all ()
  (setq bug-show-fsummary-by-dut-p t)
  (setq bug-show-fsummary-by-test-p t)
  (setq bug-show-fsummary-by-proj-p t)
  (setq bug-show-det-fsummary-p t)
  (bug-refresh)
  (bug-goto-section "BUG FAILURES BY DUT")
  )

(defun bug-show-fsummary-needed ()
  (or bug-show-fsummary-by-dut-p bug-show-fsummary-by-test-p bug-show-fsummary-by-proj-p)
  )

(defun bug-goto-section (section-re)
  (let (pos-found)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward (format "^----+\\s-%s\\s-+-+----$" section-re) nil t)
          (setq pos-found (line-beginning-position))
        )
      )
    (if pos-found
        (goto-char pos-found))
    ))

;;; --------------------------------------------------------------------
;;; operation related definitions/functions

(defun bug-execute-op (selection op-alist)
  (let (op prompt a4-bug-option history is-email 
           selection-map get-sel-func initial-input-func initial-input value tmp-list)
    (setq selection (char-to-string selection))
    (setq op (cdr (assoc selection op-alist)))
    (if (not op)
        (error (format "Invalid selection: %s" selection)))
    (if (functionp op)
        ; function, call it directly
        (funcall op)

      ; not a function, then it's an alist w/ parameters for 'a4 bug'
      ; if it has a set of valid options, get the option using interactive "c"
      (setq prompt (cdr (assoc "prompt" op)))
      (setq a4-bug-option (cdr (assoc "option" op)))
      (setq history (cdr (assoc "history" op)))
      (setq comma-sep (cdr (assoc "comma-sep" op)))
      (setq is-email (cdr (assoc "is-email" op)))
      (setq selection-map (cdr (assoc "selection-map" op)))
      (setq get-sel-func (cdr (assoc "get-sel-func" op)))
      (setq filter-func (cdr (assoc "filter-func" op)))
      (setq initial-input-func (cdr (assoc "initial-input-func" op)))
      (setq initial-input (if initial-input-func (funcall initial-input-func) ""))
      
      (if selection-map
          ; has a selection map, get value from it
          (progn
            (setq selection (call-interactively get-sel-func))
            (setq value (cdr (assoc selection selection-map)))
            (if (not value)
                (error "Invalid selection: %s" selection))
            )
        ; no selection-map, get value directly from prompt
        (setq value (read-string (concat prompt " ") initial-input history))
        (if (and is-email (not (string-match "@" value)))
            (setq value (concat value "@aristanetworks.com")))
        )

      (when filter-func
        (setq ret-list (funcall filter-func a4-bug-option value))
        (setq a4-bug-option (car ret-list))
        (setq value (nth 1 ret-list))
        )

      ; XXX: must handle the case of comma separatable options

      (setq cmd (format "%s %s %s"
                        (bug-a4-bug-cmd-from-settings bug-cur-bug-id)
                        a4-bug-option
                        (if value (format "'%s'" value) "")))
      (bug-invoke-cmd cmd)
      )
    ))

(defun bug-op-not-implemented ()
  (message "Operation not implemented yet"))

;;; ==========================================================================
;;; XXX: a4-nav functions to be moved to a4-nav.el

(defun a4-nav-imenu-begin-cur ()
  (let ((chr (char-before)))
    (if (and chr (= chr 10))
        (goto-char (+ (point) 1)))
    (a4-nav-imenu-prev)
    ))

(defun a4-nav-imenu-refresh ()
  (when (and (boundp 'imenu--index-alist)
             (null imenu--index-alist))
    (imenu--make-index-alist t))
  (funcall imenu-create-index-function))

(defun a4-nav-imenu-last ()
  (interactive)
  (let ((idx (a4-nav-imenu-cur-idx))
        imenu-list-len)
    (setq imenu-list-len (length imenu--index-alist))
    (if (> imenu-list-len 0)
        (a4-nav-imenu-goto-idx (- imenu-list-len 1)))
  ))

;;; ==========================================================================
;;; mail related functions

(defun bug-mail ()
  (interactive)
  (let ((subject (bug-bug-id-and-desc-str))
        (assigned-to bug-cur-assigned-to)
        )
    (mail)
    (when assigned-to
      (mail-to)
      (insert assigned-to)
      )
    (mail-subject)
    (insert subject)
    (mail-text)
    ))

;;; ==========================================================================
;;; helper functions

(defun bug-get-cur-section-name ()
  (interactive)
  (save-excursion 
    (let ((section-name "Description") (comment-number "0"))
      (a4-nav-imenu-begin-cur)
      (when (re-search-forward bug-section-header-re nil t)
        (setq section-name (match-string-no-properties 1))
        (goto-char (match-beginning 1))
        (if (re-search-forward "Comment\\s-+\\([0-9]+\\)" (match-end 1) t)
            (setq comment-number (match-string-no-properties 1)))
        ;;(message (concat section-name " -- " comment-number))
        )
      (list section-name comment-number)
      )))

(defun bug-get-cur-bug-id ()
  "Determine the current bug id for this bug from the buffer contents."
  (save-excursion 
    (setq bug-cur-bug-id nil)
    (goto-char (point-min))
    (when (re-search-forward "------+\\s-+BUG\\s-\\([0-9]+\\)\\s-+-+------$" 
                             (line-end-position) t)
      (setq bug-cur-bug-id (match-string-no-properties 1))
      ; get also the bug short description (aka summary)
      (if (and (eolp) (not (eobp)))
          (forward-char))
      (when (re-search-forward "^[0-9]+\\s-+\\(.+\\)$" (line-end-position) t)
        (setq bug-cur-bug-desc (match-string-no-properties 1))
        )
      )
    ; try to find bug alias
    (setq bug-cur-bug-alias nil)
    (setq bug-cur-assigned-to nil)
    (setq bug-header-section-end nil)
    (setq bug-blocks-section-start nil)
    (setq bug-blocks-section-end nil)
    (when bug-cur-bug-id
      ; find end of "header" section
      (goto-char (point-min))
      (forward-line)
      (when (re-search-forward "------+\\s-+.*\\s-+-+------$" (point-max) t)
        (setq bug-header-section-end (line-beginning-position))

        ; extract alias, if present
        (goto-char (point-min))
        (when (re-search-forward "\\s-+alias:\\s-+\\(.+\\)$"
                                 bug-header-section-end t)
          (setq bug-cur-bug-alias (match-string-no-properties 1)))

        ; extract assigned-to, if present
        (goto-char (point-min))
        (when (re-search-forward "\\s-+assigned-to:\\s-+\\(.+\\)$"
                                 bug-header-section-end t)
          (setq bug-cur-assigned-to (match-string-no-properties 1)))

        ; find region w/ "[indirectly-]blocks bug ..." lines, if present
        (goto-char (point-min))
        (when (re-search-forward "^\\s-+\\(indirectly-\\)?blocks bug"
                                 bug-header-section-end t)
          (setq bug-blocks-section-start (line-beginning-position))
          (let ((last-blocks-line bug-blocks-section-start))
            (while (re-search-forward "^\\s-+\\(indirectly-\\)?blocks bug"
                                      bug-header-section-end t)
              (setq last-blocks-line (line-beginning-position)))
            (goto-char last-blocks-line)
            (setq bug-blocks-section-end (line-end-position))
            )
          )
        )
      )
    bug-cur-bug-id
    ))

(defun bug-copy-bug-BUG-id ()
  (interactive)
  (kill-new (concat "BUG" bug-cur-bug-id)))

(defun bug-copy-bug-id ()
  (interactive)
  (kill-new bug-cur-bug-id))

(defun bug-get-cur-bug-desc ()
  bug-cur-bug-desc)

(defun bug-bug-id-and-desc-str ()
  (format "BUG%s (%s)" bug-cur-bug-id bug-cur-bug-desc))

(defun bug-copy-bug-id-and-desc ()
  (interactive)
  (kill-new (bug-bug-id-and-desc-str)))

(defun bug-a4-bugs-wide-supported ()
  (let (exit-code output)
    (when (not bug-a4-bugs-wide-checked-p)
      (save-excursion
        (setq ret-list (bug-do-invoke-cmd "a4 bugs --help")))
      (setq exit-code (car ret-list))
      (setq output (nth 1 ret-list))
      (when (eq exit-code 0)
        (setq bug-a4-bugs-wide-checked-p t)
        (setq bug-a4-bugs-wide-supported-p (string-match "--wide" output))
        )
      )
    bug-a4-bugs-wide-supported-p
    ))

(defun bug-a4-bugs-cmd-from-settings (&optional bug-id)
  "Return a4 bugs command to be executed, according to our settings.
Notice that this is different form the 'a4 bug' (singular) command."
  ; if bug-id not provided, use bug-id from current buffer
  (let ((cmd (concat "a4 bugs " (if bug-id bug-id bug-cur-bug-id))))
    (if bug-show-all-comments-p
        (setq cmd (concat cmd " --ac"))
      (if (and bug-show-proj-comments-p bug-cur-project-name)
          (setq cmd (concat cmd " --rc " (concat bug-cur-project-name "@"))))
      )
    (if bug-show-activity-p
        (setq cmd (concat cmd " -A")))
    (if (bug-show-fsummary-needed)
        (if bug-show-det-fsummary-p
            (setq cmd (concat cmd " --dfs"))
          (setq cmd (concat cmd " --fs"))
          )
      )
    (if (bug-a4-bugs-wide-supported)
        (setq cmd (concat cmd " --wide")))
    cmd
    ))

(defun bug-a4-bug-cmd-from-settings (&optional bug-id)
  "Return a4 bugs command to be executed, according to our settings.
Notice that this is different form the 'a4 bugs' (plural) command."
  ; if bug-id not provided, use bug-id from current buffer
  (let ((cmd (format "%s %s" bug-a4-bug-cmd (if bug-id bug-id bug-cur-bug-id))))
    (if bug-quiet-mode
        (setq cmd (concat cmd " --quiet")))
    cmd
    ))

(defun bug-buf-name (bug-id)
  "Return the name for the buffer, given a bug-id"
  (format "[BUG %s]" bug-id))

;;; ==========================================================================
;;; "around point" related functions

(defun bug-word-around-point ()
  "Return the word around the point as a string."
  (save-excursion
    (if (not (eobp))
	(forward-char 1))
    (forward-word -1)
    (forward-word 1)
    (forward-sexp -1)
    (buffer-substring-no-properties (point) (progn
                                              (forward-sexp 1)
                                              (point)))))


;(defvar
(setq bug-alias-delim-re "\\([*()={}:\"',\n\t\v ]\\|\\[\\|\\]\\)")

(defun bug-region-for-alias-around-point ()
  "Return region for the expression around point."
  (let (start end)
    (save-excursion
      (if (not (eobp))
          (forward-char 1))
      (forward-word -1)
      (forward-word 1)
      (if (re-search-backward bug-alias-delim-re (point-min) t)
          (progn 
            (forward-char)
            ; ignore initial '.'
            (if (looking-at "\\.")
                (forward-char))
            (setq start (point)))
        (setq start (point-min))
        (goto-char start)
        )
      (if (re-search-forward bug-alias-delim-re (point-max) t)
          (progn
            (backward-char 2)
            ; ignore trailing '.'
            (if (not (looking-at "\\."))
                (forward-char))
            (setq end (point)))
        (setq end (point-max)))
      (list start end)
      )
    ))

(defun bug-alias-around-point ()
  "Return the word or alias around the point as a string."
  (let ((reg (bug-region-for-alias-around-point)))
    (buffer-substring (car reg) (nth 1 reg))
    ))

;;; ==========================================================================
;;; core functions

(defun bug-create-change-num-link (start end change-num)
  (let (prop-list)
    (setq prop-list (list (cons 'face 'font-lock-function-name-face)
                          (cons 'action "goto-change-num")
                          (cons 'value change-num)))
    (bug-create-active-link start end prop-list)
    ))

(defun bug-create-bug-link (start end bug-id)
  (let (prop-list)
    ; create link (but not for ourselves)
    (when (not (string= bug-id bug-cur-bug-id))
      (setq prop-list (list (cons 'face 'font-lock-function-name-face)
                            (cons 'action "goto-bug")
                            (cons 'value bug-id)))
      (bug-create-active-link start end prop-list))
    ))

(defun bug-create-url-link (start end url)
  (let (prop-list)
    (setq prop-list (list (cons 'face 'font-lock-function-name-face)
                          (cons 'action "goto-url")
                          (cons 'value url)))
    (bug-create-active-link start end prop-list)
    ))

(defun bug-create-date-link (start end date)
  (let (prop-list)
    (setq prop-list (list (cons 'face 'font-lock-variable-name-face)
                          (cons 'action "goto-date")
                          (cons 'value date)))
    (bug-create-active-link start end prop-list)
    ))

(defun bug-create-proj-link (start end proj)
  (let (prop-list)
    (setq prop-list (list (cons 'face 'font-lock-doc-face)
                          (cons 'action "set-cur-proj")
                          (cons 'value proj)))
    (bug-create-active-link start end prop-list)
    ))

(defun bug-create-test-file-link (start end test-file)
  (let (prop-list)
    (setq prop-list (list (cons 'face 'font-lock-function-name-face)
                          (cons 'action "goto-test-file")
                          (cons 'value test-file)))
    (bug-create-active-link start end prop-list)
    ))

(defun bug-create-dut-link (start end dut-name)
  (let (prop-list)
    (setq prop-list (list (cons 'face 'font-lock-function-name-face)
                          (cons 'action "show-dut-info")
                          (cons 'value dut-name)))
    (bug-create-active-link start end prop-list)
    ))

(defun bug-activate-buffer ()
  "Scan current buffer checking for special patterns (e.g. bug names / ids, change 
number, etc.) to be navigable."
  (let (start end limit delim value sav-point)
    (save-excursion
      (setq bug-link-list nil)
      (setq buffer-read-only nil)

      ; sort "[indirectly-]blocks bug ..." lines, if any
      (bug-sort-blocks-region)

      ; activate activity section (if present)
      (bug-activate-activity-section)

      ; activate failure-summary section (if present)
      (bug-activate-fsummary-section)

      ; activate dependencies section (if any)
      (bug-activate-deps-section)

      ;; find change numbers
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*fixed-by:" bug-header-section-end t)
        (setq limit (line-end-position))
        (while (re-search-forward "\\([0-9]+\\)[,\n]?" limit t)
          (bug-create-change-num-link (match-beginning 1)
                                      (match-end 1)
                                      (match-string-no-properties 1))
          ))
      (goto-char bug-header-section-end)
      (while (re-search-forward "\\s-\\(@\\|change\\s-+\\)\\([0-9]+\\)" (point-max) t)
        (bug-create-change-num-link (match-beginning 2)
                                    (match-end 2)
                                    (match-string-no-properties 2))
        )

      ;; find bug #'s
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*\\(superseded-by\\|supersedes\\):" bug-header-section-end t)
        (setq limit (line-end-position))
        (while (re-search-forward "\\([0-9]+\\)[,\n]?" limit t)
          (bug-create-bug-link (match-beginning 1)
                               (match-end 1)
                               (match-string-no-properties 1))
          ))
      (goto-char (point-min))
      (while (re-search-forward "\\(\\(BUG\\|bug\\|Bug\\|This bug has been marked as a duplicate of\\)\\(\\s-?\\)\\([0-9]+\\)\\)" (point-max) t)
        ; if bugXXXX highlight the whole thing; if space in the midle highlight just the number
        (setq start (if (= 0 (length (match-string 3)))
                        (match-beginning 1)
                      (match-beginning 4)))
        (setq end (match-end 1))
        (bug-create-bug-link start end (match-string-no-properties 4))
        )

      ;; activate URLs
      (bug-activate-urls)

      ;; activate rdams
      (bug-activate-duts)

      ;; activate project references, if in project mode
      (bug-activate-proj-refs)

      (setq buffer-read-only t)
      ; sort list of links found in the page
      (setq bug-link-list (sort bug-link-list '<))
      )))

(defun bug-display-buffer (bug-id cmd other-window-p pending-msg refresh-p)
  "Lower level function that creates the buffer, invokes the a4 bugs command
to fill its contents and finally sets buffer to bug-mode."
  (interactive)
  (let ((buf-name (bug-buf-name bug-id))
        (sav-show-all-comments-p bug-show-all-comments-p)
        (sav-show-proj-comments-p bug-show-proj-comments-p)
        (sav-show-activity-p bug-show-activity-p)
        (sav-show-fsummary-by-dut-p bug-show-fsummary-by-dut-p)
        (sav-show-fsummary-by-test-p bug-show-fsummary-by-test-p)
        (sav-show-fsummary-by-proj-p bug-show-fsummary-by-proj-p)
        (sav-show-det-fsummary-p bug-show-det-fsummary-p)
        buf output deps-on-p deps-display-type deps-open-only-p deps-direct-only-p)
    (if refresh-p
        (progn
          ; refresh, get new values from buffer-local vars
          (setq deps-on-p bug-deps-on-p)
          (setq deps-display-type bug-deps-display-type)
          (setq deps-open-only-p bug-deps-open-only-p)
          (setq deps-direct-only-p bug-deps-direct-only-p))
      ; not a refresh, use default values
      (setq bug-quiet-mode bug-default-quiet-mode)
      (setq deps-on-p bug-default-deps-on-p)
      (setq deps-display-type bug-default-deps-display-type)
      (setq deps-open-only-p bug-default-deps-open-only-p)
      (setq deps-direct-only-p bug-default-deps-direct-only-p)
      (setq sav-show-all-comments-p nil)
      (setq sav-show-proj-comments-p nil)
      (setq sav-show-activity-p nil)
      (setq sav-show-fsummary-by-dut-p nil)
      (setq sav-show-fsummary-by-test-p nil)
      (setq sav-show-fsummary-by-proj-p nil)
      (setq sav-show-det-fsummary-p nil)
      )

    ;;(get-buffer-create buf-name) ;; We do these two lines
    ;;(kill-buffer buf-name)       ;; to ensure no duplicates
    (setq buf (get-buffer-create buf-name))
    (set-buffer buf)
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max))

    (message pending-msg)
    (setq output (shell-command-to-string cmd))
    (message (format "%s...Done." pending-msg))
    (insert output)

    (if bug-debug-p
        (print (format "CMD: %s" cmd)))

    (goto-char (point-min))
    (setq bug-last-id-requested bug-id)
    (bug-mode)

    ; update buffer-local variables after we got into bug-mode
    (if (not refresh-p)
        (setq deps-on-p (bug-deps-heuristics deps-on-p)))
    (setq bug-deps-on-p deps-on-p)
    (setq bug-deps-display-type deps-display-type)
    (setq bug-deps-open-only-p deps-open-only-p)
    (setq bug-deps-direct-only-p deps-direct-only-p)

    (setq bug-show-activity-p sav-show-activity-p)
    (setq bug-show-fsummary-by-dut-p sav-show-fsummary-by-dut-p)
    (setq bug-show-fsummary-by-test-p sav-show-fsummary-by-test-p)
    (setq bug-show-fsummary-by-proj-p sav-show-fsummary-by-proj-p)
    (setq bug-show-det-fsummary-p sav-show-det-fsummary-p)

    (setq buffer-read-only nil)
    (bug-move-activity-section)
    (bug-move-fsummary-sections)
    (bug-insert-deps-section bug-id 
                             deps-on-p
                             deps-display-type
                             deps-open-only-p
                             deps-direct-only-p)

    ; replace some strings before putting buffer in read-only mode
    (replace-string 
     "Run with --all-comments to see them."
     "Type: a-all comments, P-selected project comments only.")
;;     "Type: a-show all comments, P-show comments for selected project.")
    (message "")

    (setq buffer-read-only t)

    ; bug-mode resets bug-show-all-comments-p b/c it's a per-buffer local var
    ; if we're in all-comments mode, we must explicitly set it again and
    ; refresh the mode-line
    (setq bug-show-all-comments-p sav-show-all-comments-p)
    (setq bug-show-proj-comments-p sav-show-proj-comments-p)
    (when (or bug-show-all-comments-p bug-show-proj-comments-p bug-show-activity-p
              (bug-show-fsummary-needed))
      (force-mode-line-update))

    (when other-window-p
      ; XXX: shouldn't go to other window if buffer was already displayed
      (pop-to-buffer buf)
      )

    ; finally, activate the buffer (i.e. create actionable links, etc.)
    (bug-activate-buffer)
    (goto-char (point-min))
    ))

(defun bug-refresh (&optional bug-id)
  "Refresh bug information by invoking the a4 bugs command again. After 
redisplaying the information, try go back to the same section as before.
If inside a 'Comment #' section that's no longer present (e.g. in the case of 
toggling out 'all-comments' mode), go to the closest comment section preceding
the current section."
  (interactive)
  (let ((cur-section-and-comment (bug-get-cur-section-name))
        (found nil)
        cur-section-name cur-comment-number re cmd)
    ; if bug-id not provided, get the current one for this buffer
    (if (not bug-id)
        (setq bug-id bug-cur-bug-id))
    (setq cur-section-name (car cur-section-and-comment))
    (setq cur-comment-number (string-to-number (nth 1 cur-section-and-comment)))

    (setq cmd (bug-a4-bugs-cmd-from-settings bug-id))
    (bug-display-buffer bug-id cmd nil "Refreshing contents..." t)

    ; select window where buffer is
    (select-window (get-buffer-window (bug-buf-name bug-id)))
    (goto-char (point-min))
    (setq re (concat "^-------+\\s-" cur-section-name "\\s-+-+------$"))
    (if (re-search-forward re nil t)
        (beginning-of-line)
      ; couldn't find the original section (could have gone away), search 
      ; backwards for nearest comment
      (when (not (or (string-match "Description" cur-section-name)
                     (string-match "BUG " cur-section-name)))
        (if (not (bug-find-closest-comment cur-comment-number -1))
            (if (re-search-forward "^-------+\\s-Description\\s-+-+------$" nil t)
                (beginning-of-line))
          )
        )
      )
    ))

(defun bug-find-closest-comment (cur-comment-number incr)
  (let ((found nil))
    (goto-char (point-min))
    (while (and (> cur-comment-number 0) (not found))
      (setq re (concat "^-------+\\s-Comment\\s-*"
                       (number-to-string cur-comment-number)
                       "\\s-+-+------$"))
      (if (not (re-search-forward re nil t))
          (setq cur-comment-number (+ cur-comment-number incr))
        (setq found t)
        (beginning-of-line))
      )
    found
    ))

(defun bug-toggle-all-comments ()
  "Toggle the display of all commments (or just the interesting ones)."
  (interactive)
  (setq bug-show-all-comments-p (not bug-show-all-comments-p))
  (if bug-show-all-comments-p
      (setq bug-show-proj-comments-p nil))
  (bug-refresh))

(defun bug-toggle-proj-comments ()
  "Toggle the display of commments for current project."
  (interactive)
  (setq bug-show-proj-comments-p (not bug-show-proj-comments-p))
  (if bug-show-proj-comments-p
      (setq bug-show-all-comments-p nil))
  (bug-refresh))

(defun bug-toggle-auto-atest-mode ()
  "Toggle automatic setting of autotest mode when opening autotest log URLs."
  (interactive)
  (setq bug-auto-atest-mode (not bug-auto-atest-mode))
  (bug-refresh))

(defun bug-toggle-activity ()
  "Toggle the display of activity history."
  (interactive)
  (setq bug-show-activity-p (not bug-show-activity-p))
  (bug-refresh)
  (if bug-show-activity-p
      (bug-goto-section "BUG ACTIVITY"))
  )

(defun bug-toggle-fsummary ()
  "Toggle the display of detailed failure summary."
  (interactive)
  ;; make it mutually exclusive w/ detailed failure summary
  (setq bug-show-det-fsummary-p nil)
  (setq bug-show-fsummary-p (not bug-show-fsummary-p))
  (bug-refresh))

(defun bug-toggle-det-fsummary ()
  "Toggle the display of detailed failure summary."
  (interactive)
  ;; make it mutually exclusive w/ failure summary
  (setq bug-show-fsummary-p nil)
  (setq bug-show-det-fsummary-p (not bug-show-det-fsummary-p))
  (bug-refresh))

(defun bug-return-key (pnt)
  (interactive "d")
  (let ((action (get-char-property pnt 'action))
	(value (get-char-property pnt 'value)))
    (cond
     ((string= action "goto-change-num")
      (bug-describe-change-internal value)
      )
     ((string= action "goto-bug")
      (bug-internal value)
      )
     ((string= action "goto-url")
      (bug-goto-url value)
      )
     ((string= action "goto-date")
      (bug-goto-date value)
      )
     ((string= action "set-cur-proj")
      (bug-set-cur-proj value)
      )
     ((string= action "goto-test-file")
      (bug-goto-test-file value)
      )
     ((string= action "show-dut-info")
      (bug-show-dut-info value)
      )
     (t
      (error (concat "Invalid action: " action))
      )
     )
    ))

(defun bug-trace-changelist (pnt)
  (interactive "d")
  (let ((action (get-char-property pnt 'action))
	(value (get-char-property pnt 'value)))
    (if (string= action "goto-change-num")
        (bug-invoke-cmd (format "a4 trace %s" value) nil t t t)
      (error "Trace: not a changelist")
     )
    ))

(defun bug-next-list-elm (pnt lst)
  "Go to closest element after current position."
  (let (found (elm lst))
    (while (and elm (not found))
      (when (> (car elm) pnt)
        (goto-char (car elm))
        (setq found t)
        )
      (setq elm (cdr elm))
      )))

(defun bug-prev-list-elm (pnt lst)
  "Go to closest proj-ref before current position."
  (let* (found (tmp-list (reverse lst)) (elm tmp-list))
    (while (and elm (not found))
      (when (< (car elm) pnt)
        (goto-char (car elm))
        (setq found t)
        )
      (setq elm (cdr elm))
      )))

(defun bug-next-link (pnt)
  "Go to closest link after current position. (Current position passed to
us through arg pnt because of interactive 'd' argument.)"
  (interactive "d")
  (bug-next-list-elm pnt bug-link-list)
  )

(defun bug-prev-link (pnt)
  "Go to closest link before current position. (Current position passed to
us through arg pnt because of interactive 'd' argument.)"
  (interactive "d")
  (bug-prev-list-elm pnt bug-link-list)
  )

(defun bug-next-section ()
  (interactive)
  (a4-nav-imenu-next))

(defun bug-prev-section ()
  (interactive)
  (a4-nav-imenu-prev))

(defun bug-last-section ()
  (interactive)
  (a4-nav-imenu-last)
  )

(defun bug-internal (bug-id)
  (if (string-match "^\\(fixes=bug\\|bug\\|rfe\\)\\([0-9]+\\)"
                    (downcase bug-id))
      (setq bug-id (match-string-no-properties 2 bug-id))
    (if (string-match "^!\\([0-9]+\\)!" bug-id)
        (setq bug-id (match-string-no-properties 1 bug-id))
      ))
  (setq cmd (concat "a4 bugs " bug-id))
  (bug-display-buffer bug-id cmd t "Loading contents..." nil)
  (message "Press ? for quick help")
  )

(defun bug-goto-bug-id (bug-id)
  (bug-internal bug-id)
  )

(defun bug-goto ()
  "Open buffer in bug-mode for (supposed) bug-id under cursor."
  (interactive)
  (bug-internal (bug-alias-around-point)))

(defun bug ()
  "Prompt for bug id and open buffer in bug-mode for it."
  (interactive)
  (bug-internal (read-string "Bug id or alias: " "" 'bug-bug-id-history)))

(defun a4-bug ()
  "Prompt for bug id and open buffer in bug-mode for it."
  (interactive)
  (call-interactively 'bug)
  )

(defun bug-toggle-quiet-mode ()
  (interactive)
  (setq bug-quiet-mode (not bug-quiet-mode))
  (message (format "Email notifications %s." (if bug-quiet-mode "disabled" "enabled")))
  (force-mode-line-update))

(defun bug-toggle-default-quiet-mode ()
  "Toggle default quiet mode. New setting does *not* impact bug bufferes already open,
only those open from now on."
  (interactive)
  (setq bug-default-quiet-mode (not bug-default-quiet-mode))
  (message (format "Default bug quiet mode set to: %s" 
                   (if bug-default-quiet-mode "True" "False")))
  )

(defun bug-show-logre ()
  "Display the logre entry for the current bug."
  (interactive)
  (let (logre-buf (bug-id bug-cur-bug-id) (sav-cur-buf (current-buffer)))
    (save-excursion
      (if (not (file-readable-p "/eng/etc/logre"))
          (message "ERROR: Cannot read file '/eng/etc/logre' (Did you do 'a4 choose +eng'?)")
        ;; open /eng/etc/logre file and search for the bug
        (setq logre-buf (find-file-noselect "/eng/etc/logre"))
        (set-buffer logre-buf)
        (goto-char (point-min))
        (pop-to-buffer logre-buf)
        (when (not (re-search-forward bug-id nil t))
          (message 
           (format "ERROR: Cannot find entry for BUG%s in '/eng/etc/logre' (Is it up to date? Try 'a4 sync /eng/etc/logre')" bug-id))
          (goto-char (point-max))
          )
        )
      )))

;;; ==========================================================================
;;; url related functions

(defun bug-activate-urls ()
  (let ()
    (goto-char (point-min))
    (while (re-search-forward "\\(http://.*\\.log\\)" (point-max) t)
;;    (while (re-search-forward "\\(http://.*\\)\\(\\s-\\|$\\)" (point-max) t)
      (bug-create-url-link (match-beginning 1) (match-end 1) 
                           (match-string-no-properties 1))
      )
    ))

(defun bug-activate-duts ()
  (let ()
    (when (bug-goto-section "Description")
      (while (re-search-forward "\\(rdam.*://\\([a-z0-9_]+\\)\\)" (point-max) t)
        (bug-create-dut-link (match-beginning 1) (match-end 1) 
                             (match-string-no-properties 2))
        )
      )
    ))

(defun bug-goto-url (url)
  (let ((file-name url) cmd)
    (while (string-match "/\\(.*\\)$" file-name)
      (setq file-name (match-string-no-properties 1 file-name)))
    (setq file-name (concat bug-tmp-dir "/" file-name))
    (when (not (file-exists-p file-name))
;; file names containing '%' mess up (message ...) because it interprets '%s'
;; as a printf format indicator; we must escape the file name before shoing it
;;      (message (concat "Downloading file '" file-name "'..."))
      (message "Downloading file...")
      (setq cmd (concat "wget -O " file-name " " url))
      (when (not (= 0 (bug-invoke-cmd cmd nil t)))
;;        (error (concat "Error downloading file '" file-name "'")))
        (error (concat "Error downloading file")))
      (message "")
      )
    (find-file-other-window file-name)
    (if (and bug-auto-atest-mode (string-match "//atest" url))
        (atest-mode))
    ;;(message cmd)
    ))

(defun bug-goto-date (date)
  (let (re)
    (setq re (concat "By:\\s-+[^ ]+\\s-+" date))
    (if (re-search-forward re (point-max) t)
        (beginning-of-line)
      (message (format "Couldn't find comment for date/time '%s' - try enabling all-comments / proj-comments mode" date))
      )
  ))

(defun bug-goto-test-file (file-name)
  (let (pkg)
    ;; if test w/ arguments, keep only file name
    (if (string-match "\\([^ ]+\\) " file-name)
        (setq file-name (match-string-no-properties 1 file-name)))
    (when (string-match "\\([^/]+\\)/\\(.+\\)" file-name)
      (setq pkg (match-string-no-properties 1 file-name))
      (setq file-name (match-string-no-properties 2 file-name))
      (setq file-name (format "/src/%s/ptest/%s" pkg file-name))
      )
    ;;  (setq file-name (concat "/usr/share/ptest/" file-name))
    (find-file-read-only-other-window file-name)
  ))

(defun bug-show-dut-info (dut-name)
  (let (cmd)
    (if (string-match "rdam\\(mlag\\)?://\\(.*\\)" dut-name)
        (setq dut-name (match-string-no-properties 2 dut-name)))
    (setq cmd (format "Art info --detail %s" dut-name))
    (message (format "Invoking command '%s'..." cmd))
    (bug-invoke-cmd cmd nil t t t)
    (pop-to-buffer bug-cmd-output-buf-name)
    (message "")
    ))

;;; ==========================================================================
;;; project related functions

(defun bug-set-proj ()
  (interactive)
  (bug-set-cur-proj "")
  )

(defun bug-set-cur-proj (proj)
  (setq bug-cur-project-name (read-string "Set current project: " proj))
  (setq bug-show-proj-comments-p t)
  (setq bug-show-all-comments-p nil)
  (bug-refresh)
  (message (format "Displaying only comments about '%s'" proj))
  )

(defun bug-activate-proj-refs ()
  (let (proj-regexp)
    (when (and bug-cur-project-name
               (not (string= bug-cur-project-name "")))
      (setq proj-regexp (regexp-quote bug-cur-project-name))
      (goto-char (point-min))
      (while (re-search-forward proj-regexp (point-max) t)
        (bug-create-proj-ref (match-beginning 0) (match-end 0))
        )
      )
    ))

(defun bug-next-proj-ref (pnt)
  "Go to closest project reference after current position.
Current position passed to us through arg pnt because of interactive
'd' argument.)"
  (interactive "d")
  (bug-next-list-elm pnt bug-proj-ref-list))

(defun bug-prev-proj-ref (pnt)
  "Go to closest project reference before current position.
Current position passed to us through arg pnt because of interactive
'd' argument.)"
  (interactive "d")
  (bug-prev-list-elm pnt bug-proj-ref-list))

;;; ==========================================================================
;;; activity section related functions

(defun bug-move-activity-section ()
  "Move activity section (if present) from the end to the beginning."
  (let (start end lines)
    (setq bug-activity-section-start nil)
    (setq bug-activity-section-end nil)
    (when bug-show-activity-p
      ; search for the beginning of the BUG ACTIVITY section
      (goto-char (point-min))
      (if (not (re-search-forward "------+\\s-+BUG ACTIVITY\\s-+-+------$" (point-max) t))
          (message "BUG ACTIVITY section not found")
        (setq start (line-beginning-position))
        (insert "\n")
        ;; XXX: should go to (point-max) - problem if it's not the last section
        (goto-char (point-max))
        (insert "\n")
        (setq end (point-max))
        ; copy/delete activity section
        (setq lines (buffer-substring-no-properties start end))
        (goto-char start)
        (delete-region start end)

        ; now insert it right before the Description one or at the end if
        ; Description not found
        (goto-char (point-min))
        (if (re-search-forward "------+\\s-+Description\\s-+-+------$" (point-max) t)
            (beginning-of-line)
          (goto-char (point-max)))
        (setq bug-activity-section-start (point))
        (insert lines)
        (setq bug-activity-section-end (point))
        )
      )
    ))

(defun bug-activate-activity-section ()
  "Activate activity section (i.e. search for activity related patterns there)."
  (let (start end line-end)
    (save-excursion
      (when bug-activity-section-start
        ; search/activate bug ids / changelists
        (goto-char bug-activity-section-start)
        (while (re-search-forward "\\(changed from\\| to\\|removed\\) [0-9]+" 
                                  bug-activity-section-end t)
          (beginning-of-line)
          (setq line-end (line-end-position))
          (if (re-search-forward "Fix List" line-end t)
              (progn
                ; it's a "Fix List" entry, activate numbers as changelist links
                (while (re-search-forward "\\([0-9]+\\)[, \n]?" line-end t)
                  (bug-create-change-num-link (match-beginning 1)
                                              (match-end 1)
                                              (match-string-no-properties 1))
                  )
                )
            ; it's not a fix list, assume sequence of numbers is a bug
            ; skip "<date> <time> <user> *Bug*" 
            (beginning-of-line)
            (when (re-search-forward "^[^ ]+\\s-+[^ ]+\\s-+[^ ]+\\s-+[^ ]*Bug[^ ]* " line-end t)
              (while (re-search-forward "\\([0-9]+\\)[, \n]?" line-end t)
                (bug-create-bug-link (match-beginning 1)
                                     (match-end 1)
                                     (match-string-no-properties 1))
                )
              )
            )
          (end-of-line)
          )
        )
      )))

;;; ==========================================================================
;;; activity section related functions

(defun bug-fsummary-find-sub-section (sub-section-name)
  (let (section-header-re start end)
    (setq section-header-re
          (format "-----+ BUG FAILURES BY %s ---+$" sub-section-name))
    (goto-char bug-fsummary-section-start)
    (when (re-search-forward section-header-re bug-fsummary-section-end t)
      (setq start (line-beginning-position))
      (if (re-search-forward "\n-+ [A-Za-z ]+ -+$" bug-fsummary-section-end t)
          (setq end (line-beginning-position))
        (setq end bug-fsummary-section-end)
        )
      )
    ; return list w/ start, end of sub-section
    (list start end)
    ))

(defun bug-move-fsummary-sections ()
  "Move failure summary sections (if present) from the end to the beginning."
  (let (start end by-dut-str by-test-str by-proj-str tmp)
    (setq bug-fsummary-section-start nil)
    (setq bug-fsummary-section-end nil)
    (setq bug-fsummary-by-dut-start nil)
    (setq bug-fsummary-by-dut-end nil)
    (setq bug-fsummary-by-proj-start nil)
    (setq bug-fsummary-by-proj-end nil)
    (setq bug-fsummary-by-test-start nil)
    (setq bug-fsummary-by-test-end nil)
    (when (bug-show-fsummary-needed)
      ; search for the beginning of the BUG FAILURES BY TEST section
      (goto-char (point-min))
      (if (not (re-search-forward "------+\\s-+BUG FAILURES BY TEST\\s-+-+------$" (point-max) t))
          (message "BUG FAILURE sections not found")
        (setq bug-fsummary-section-start (line-beginning-position))
        ;; XXX: should go to (point-max) - problem if it's not the last section
        (goto-char (point-max))
        (setq bug-fsummary-section-end (point-max))

        ; find/copy each section
        (setq tmp (bug-fsummary-find-sub-section "TEST"))
        (setq start (car tmp))
        (setq end (nth 1 tmp))
        (setq by-test-str (buffer-substring-no-properties start end))
        
        (setq tmp (bug-fsummary-find-sub-section "DUT"))
        (setq start (car tmp))
        (setq end (nth 1 tmp))
        (setq by-dut-str (buffer-substring-no-properties start end))

        (setq tmp (bug-fsummary-find-sub-section "PROJECT"))
        (setq start (car tmp))
        (setq end (nth 1 tmp))
        (setq by-proj-str (buffer-substring-no-properties start end))
        
        ; delete whole summary section
        (setq lines (buffer-substring-no-properties start end))
        (goto-char bug-fsummary-section-start)
        (delete-region bug-fsummary-section-start bug-fsummary-section-end)

        ; now insert the desired sections right before the Description one or at the end if
        ; Description not found
        (goto-char (point-min))
        (if (re-search-forward "------+\\s-+Description\\s-+-+------$" (point-max) t)
            (beginning-of-line)
          (goto-char (point-max)))

        (setq bug-fsummary-section-start (point))
        (when bug-show-fsummary-by-dut-p
          (setq bug-fsummary-by-dut-start (point))
          (insert by-dut-str)
          (insert "\n")
          (setq bug-fsummary-by-dut-end (point))
          )
        (when bug-show-fsummary-by-test-p
          (setq bug-fsummary-by-test-start (point))
          (insert by-test-str)
          (insert "\n")
          (setq bug-fsummary-by-test-end (point))
          )
        (when bug-show-fsummary-by-proj-p
          (setq bug-fsummary-by-proj-start (point))
          (insert by-proj-str)
          (insert "\n")
          (setq bug-fsummary-by-proj-end (point))
          )
        (setq bug-fsummary-section-end (point))
        )
      )
    ))

(defun bug-activate-fsummary-section ()
  "Activate failure summay section(s)."
  ;;
  ;; We activate:
  ;;   - date/time    => jump to comment
  ;;   - project name => ask to make it current project
  ;;   - test name    => open file
  ;;
  ;; Detailed:
  ;; =========
  ;
  (let (section-start section-end tmp line-end re
        (date-time-re "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]")
        (proj-re "[^ ]+")
        (test-re ".+")
        (dut-re "[^ ]+")
        fmt
        )
    (save-excursion
      (when (bug-show-fsummary-needed)
        ; search/activate bug ids / changelists
        (if (not bug-show-det-fsummary-p)
            ; non-detailed cases
            (progn
              (when bug-fsummary-by-dut-start
                ; activate BY DUT sub-section
                ; -------------------- BUG FAILURES BY DUT --------------------
                ; ...
                ; rdam://ti208         11   2013-02-04 05:18   2011.sandAccele 10091605   IpEth/...
                ; ...
                (setq section-start bug-fsummary-by-dut-start)
                (setq section-end bug-fsummary-by-dut-end)
                ; <dut> <hits> <date-time> <proj> <log-id> <test>
                (goto-char section-start)
                (setq re (format "^\\(%s\\)\\s-+[^ ]+\\s-+\\(%s\\)\\s-+\\(%s\\)\\s-+[^ ]+\\s-+\\(%s\\)\n"
                                 dut-re date-time-re proj-re test-re))
                (while (re-search-forward re section-end t)
                  (bug-create-dut-link (match-beginning 1) (match-end 1)
                                       (match-string-no-properties 1))
                  (bug-create-date-link (match-beginning 2) (match-end 2)
                                        (match-string-no-properties 2))
                  (bug-create-proj-link (match-beginning 3) (match-end 3)
                                        (match-string-no-properties 3))
                  (bug-create-test-file-link (match-beginning 4) (match-end 4)
                                             (match-string-no-properties 4))
                  )
                )

              (when bug-fsummary-by-proj-start
                ; activate BY PROJECT sub-section
                ; -------------------- BUG FAILURES BY PROJECT --------------------
                ; ...
                ; -------------------- ---- ------------------ ---------- -------------------------
                ; 2011.sandAcceleratio 8    2013-02-04 05:18   10091605   rdam://ti208: IpEth/...
                ; ...
                (setq section-start bug-fsummary-by-proj-start)
                (setq section-end bug-fsummary-by-proj-end)
                ; <proj> <hits> <date-time> <log-id> <dut> <test>
                (goto-char section-start)
                (setq re (format "^\\(%s\\)\\s-+[^ ]+\\s-+\\(%s\\)\\s-+[^ ]+\\s-+\\(%s\\):\\s-+\\(%s\\)\n"
                                 proj-re date-time-re dut-re test-re))
                (while (re-search-forward re section-end t)
                  (bug-create-proj-link (match-beginning 1) (match-end 1)
                                        (match-string-no-properties 1))
                  (bug-create-date-link (match-beginning 2) (match-end 2)
                                        (match-string-no-properties 2))
                  (bug-create-dut-link (match-beginning 3) (match-end 3)
                                       (match-string-no-properties 3))
                  (bug-create-test-file-link (match-beginning 4) (match-end 4)
                                             (match-string-no-properties 4))
                  )
                )

              (when bug-fsummary-by-test-start
                ; activate BY TEST sub-section
                ; -------------------- BUG FAILURES BY TEST --------------------
                ; ...
                ; 2013-02-04 05:18   2011.sandAccele 10091605   rdam://ti208    5    IpEth/...
                ; ...
                (setq section-start bug-fsummary-by-test-start)
                (setq section-end bug-fsummary-by-test-end)
                ; <date-time> <proj> <log-id> <dut> <hits> <test>
                (goto-char section-start)
                (setq re (format "^\\(%s\\)\\s-+\\(%s\\)\\s-+[^ ]+\\s-+\\(%s\\)\\s-+[^ ]+\\s-+\\(%s\\)\n"
                                 date-time-re proj-re dut-re test-re))
                (while (re-search-forward re section-end t)
                  (bug-create-date-link (match-beginning 1) (match-end 1)
                                        (match-string-no-properties 1))
                  (bug-create-proj-link (match-beginning 2) (match-end 2)
                                        (match-string-no-properties 2))
                  (bug-create-dut-link (match-beginning 3) (match-end 3)
                                       (match-string-no-properties 3))
                  (bug-create-test-file-link (match-beginning 4) (match-end 4)
                                             (match-string-no-properties 4))
                  )
                )
              )

          ; detailed cases
          (when bug-fsummary-by-test-start
            ; -------------------- BUG FAILURES BY TEST --------------------
            ; ...
            ; Ebra/LinksReliable.py [28]
            ;   2013-01-21 11:04   2010.plkchowdar 9954325    rdam://sq220
            ;   ...
            (setq section-start bug-fsummary-by-test-start)
            (setq section-end bug-fsummary-by-test-end)
            ; <test> [<hits>]
            ;   <date-time> <proj> <log-id> <dut>
            (goto-char section-start)
            (setq re (format 
                      (concat "^\\(\\(%s\\)\\s-+\\[\\(.*\\)\\]\\)"
                              "\\|"
                              "\\s-+\\(%s\\)\\s-+\\(%s\\)\\s-+[^ ]+\\s-+\\(%s\\)")
                      test-re
                      date-time-re proj-re dut-re))
            (while (re-search-forward re section-end t)
              (if (match-beginning 1)
                  (progn
                    (if (not (string= (match-string-no-properties 3) "hits"))
                        (bug-create-test-file-link (match-beginning 2) (match-end 2)
                                                   (match-string-no-properties 2))
                      ))
                (bug-create-date-link (match-beginning 4) (match-end 4)
                                      (match-string-no-properties 4))
                (bug-create-proj-link (match-beginning 5) (match-end 5)
                                           (match-string-no-properties 5))
                (bug-create-dut-link (match-beginning 6) (match-end 6)
                                     (match-string-no-properties 6))
                )
              )
            )

          (when bug-fsummary-by-dut-start
            ; -------------------- BUG FAILURES BY DUT --------------------
            ; ...
            ; rdam://cl207 [25]
            ;   2013-01-23 08:25   2012.smash      9974686    Ebra/LinksReliable.py
            ;   ...
            ;
            (setq section-start bug-fsummary-by-dut-start)
            (setq section-end bug-fsummary-by-dut-end)
            ; <dut> [<hits>]
            ;   <date-time> <proj> <log-id> <test>
            (goto-char section-start)
            (setq re (format 
                      (concat "^\\(\\(%s\\)\\s-+\\[\\(.*\\)\\]\\)"
                              "\\|"
                              "\\s-+\\(%s\\)\\s-+\\(%s\\)\\s-+[^ ]+\\s-+\\(%s\\)")
                      dut-re
                      date-time-re proj-re test-re))
            (while (re-search-forward re section-end t)
              (if (match-beginning 1)
                  (progn
                    (if (not (string= (match-string-no-properties 3) "hits"))
                        (bug-create-dut-link (match-beginning 2) (match-end 2)
                                             (match-string-no-properties 2))
                      ))
                (bug-create-date-link (match-beginning 4) (match-end 4)
                                      (match-string-no-properties 4))
                (bug-create-proj-link (match-beginning 5) (match-end 5)
                                           (match-string-no-properties 5))
                (bug-create-test-file-link (match-beginning 6) (match-end 6)
                                           (match-string-no-properties 6))
                )
              )
            )
          (when bug-fsummary-by-proj-start
            ; -------------------- BUG FAILURES BY PROJECT --------------------
            ; ...
            ; 2012.smash [41]
            ;   2013-01-23 08:25   9974686    rdam://cl207: Ebra/LinksReliable.py
            ;   ... 
            (setq section-start bug-fsummary-by-proj-start)
            (setq section-end bug-fsummary-by-proj-end)
            ; <project> [<hits>]
            ;   <date-time> <id> <dut>: <test>
            (goto-char section-start)
            (setq re (format 
                      (concat "^\\(\\(%s\\)\\s-+\\[\\(.*\\)\\]\\)"
                              "\\|"
                              "\\s-+\\(%s\\)\\s-+[^ ]+\\s-+\\([^ ]+\\):\\s-+\\(%s\\)")
                      proj-re
                      date-time-re test-re))
            (while (re-search-forward re section-end t)
              (if (match-beginning 1)
                  (progn
                    (if (not (string= (match-string-no-properties 3) "hits"))
                        (bug-create-proj-link (match-beginning 2) (match-end 2)
                                              (match-string-no-properties 2))
                      ))
                (bug-create-date-link (match-beginning 4) (match-end 4)
                                      (match-string-no-properties 4))
                (bug-create-dut-link (match-beginning 5) (match-end 5)
                                     (match-string-no-properties 5))
                (bug-create-test-file-link (match-beginning 6) (match-end 6)
                                           (match-string-no-properties 6))
                )
              )
            )
          )
        ))))

;;; ==========================================================================
;;; dependencies section related functions

(defun bug-insert-deps-section (bug-id 
                                deps-on-p
                                deps-display-type 
                                deps-open-only-p
                                deps-direct-only-p)
  "Insert blocking information according to parameters."
  (let (opt-list opt-str cmd status-list status-str start
        ret-list exit-code output tmp-buf-name)
    (setq bug-deps-status-str nil)
    (if (not deps-on-p)
        (progn
          (setq bug-deps-section-start nil)
          (setq bug-deps-section-end nil)
          )
      (setq bug-deps-status-str "Dep:")
      (if deps-direct-only-p
          (progn
            (setq blocking-option "-B")
            (setq bug-deps-status-str (concat bug-deps-status-str "B")))
          (setq blocking-option "-b")
          (setq bug-deps-status-str (concat bug-deps-status-str "b"))
        )
      (if deps-open-only-p
          (push "open-only" status-list)
        (push "-a" opt-list)
        (setq bug-deps-status-str (concat bug-deps-status-str "a")))
      (cond
       ((string= deps-display-type "flat")
        )
       ((string= deps-display-type "graph")
        (push "-g" opt-list)
        (setq bug-deps-status-str (concat bug-deps-status-str "g"))
        )
       ((string= deps-display-type "altGraph")
        (push "-G" opt-list)
        (setq bug-deps-status-str (concat bug-deps-status-str "g"))
        )
       (t
        (error (format "Invalid value for variable bug-deps-display-type: %s"
                       deps-display-type))
        ))
      
      ; (push (format "%s %s" blocking-option bug-id) opt-list)
      ; notice: blocking-option MUST be the last element pushed into opt-list
      ; (b/c push adds to the beginning of the list, and it'll be reversed)
      (push blocking-option opt-list)
      (setq opt-str (mapconcat 'identity (reverse opt-list) " "))
      
      (setq cmd (format "a4 bugs %s %s" opt-str bug-id))
      ;;(print cmd)
      (save-excursion
        (setq ret-list (bug-do-invoke-cmd cmd "*bug-deps-cmd*")))
      (setq exit-code (car ret-list))
      (setq output (nth 1 ret-list))
      (setq tmp-buf-name (nth 2 ret-list))

      ; insert dependencies section right before the Description one
      ; or at the end if Description not found
      (goto-char (point-min))
      (if (re-search-forward "------+\\s-+Description\\s-+-+------$" (point-max) t)
          (beginning-of-line)
        (goto-char (point-max)))

      (setq bug-deps-section-start (point))
      (insert "-------------------- Dependencies --------------------\n\n")
      (insert (concat output "\n"))

      (setq bug-deps-section-end (point))
      (kill-buffer tmp-buf-name)
      )
    ))

(defun bug-deps-heuristics (deps-on-p)
  ; enable dependencies by default if release management bugs (alias: EOS-xxx[-<suffix>]
  (when (and bug-cur-bug-alias
             (string-match
              "EOS-[0-9.]+\\(-ignore\\|-triage\\|-crossfinge\\|-waiting\\|-looking\\|-desirable\\|-crossfingers\\)?" 
              bug-cur-bug-alias))
    (setq deps-on-p t)
    )
  deps-on-p
  )

(defun bug-insert-dep-text (str)
  (insert str)
  (setq bug-deps-section-end (+ bug-deps-section-end (length str)))
  )

(defun bug-delete-dep-text (start end)
  (delete-region start end)
  (setq bug-deps-section-end (- bug-deps-section-end (- end start)))
  )

(defun bug-activate-deps-section ()
  "Activate dependencies section (i.e. search for dependencies related patterns only there)."
  (let (start end line-len delta limit delim value sav-point)
    (save-excursion
      (when bug-deps-section-start
        ;; (print (format "start: %d, end: %s" bug-deps-section-start bug-deps-section-end))

        ; adjust length of all strings here to fit in the window
        (goto-char bug-deps-section-start)
        (while (re-search-forward "^\\( *\\)\\([0-9]+\\)\\([ ,]\\)" bug-deps-section-end t)
          (setq indent (match-string-no-properties 1))
          (setq start (match-beginning 1))

          ; cosmetics: if graph mode double the indentation for better visibility
          (goto-char start)
          (if (not (string= bug-deps-display-type "flat"))
              (bug-insert-dep-text indent))

          (end-of-line)
          (setq line-len (1+ (- (point) start)))
          (when (>= line-len (window-width))
            (setq delta (+ (- line-len (window-width)) 4))
            (setq sav-point (point))
            (goto-char (- (point) delta))
            (bug-delete-dep-text (point) sav-point)
            (setq sav-point (point))
            (bug-insert-dep-text "...")
            (bug-set-face-property sav-point (+ sav-point 3) bug-de-emphasize-face)
            )
          )

        ; search for special regex'es in dependencies section
        (goto-char bug-deps-section-start)
        (while (re-search-forward 
                "^\\( *\\)\\([0-9]+\\)\\([ ,]\\)" bug-deps-section-end t)
          (setq indent (match-string-no-properties 1))
          (setq start (match-beginning 2))
          (setq end (match-end 2))
          (setq value (match-string-no-properties 2))
          (setq delim (match-string-no-properties 3))

          (if (string= delim " ")
              (bug-create-bug-link start end value)
            (bug-set-face-property start (line-end-position) bug-de-emphasize-face))
          ))
      )))

;;; ==========================================================================
;;; blocked bugs related functions

(defun bug-decode-block-line (line)
  "Decode an '[indirectly-]blocks' line into its components (id, description, EOS version, etc.)."
  (let (id desc ver sub1 sub2 sub3 ver-num)
    (when (string-match "\\s-+.*blocks bug \\([0-9]+\\)\\(\\s-+(\\(RFEU:\\s-+\\)?\\(.*\\))\\)?" line)
      (setq id (string-to-int (match-string-no-properties 1 line)))
      (setq desc (match-string-no-properties 4 line))
      (when (and desc (string-match 
                       "EOS-\\([0-9]+\\)\.\\([0-9]+\\)\\(\.\\([0-9]+\\)\\(\.\\([0-9]+\\)\\)?\\)?" desc))
        (setq ver (string-to-int (match-string-no-properties 1 desc)))
        (setq sub1 (string-to-int (match-string-no-properties 2 desc)))
        (setq sub2 (match-string-no-properties 3 desc))
        (setq sub2 (if sub2 (string-to-int sub2) 0))
        (setq sub3 (match-string-no-properties 6 desc))
        (setq sub3 (if sub3 (string-to-int sub3) 0))
        (setq ver-num (+ (* 1000000 ver) (* 10000 sub1) (* 100 sub2) sub3))
        )
      )
    ; (print (list line id desc ver-num))
    (list line id desc ver-num)
    ))

(defun bug-cmp-ver-lines (line-info1 line-info2)
  "Compare two lines w/ version information in them."
  (let (line1 id1 desc1 ver-num1 line2 id2 desc2 ver-num2)
    (setq line1 (car line-info1))
    (setq id1 (nth 1 line-info1))
    (setq desc1 (nth 2 line-info1))
    (setq ver-num1 (nth 3 line-info1))

    (setq line2 (car line-info2))
    (setq id2 (nth 1 line-info2))
    (setq desc2 (nth 2 line-info2))
    (setq ver-num2 (nth 3 line-info2))

    (if (= ver-num1 ver-num2)
        (progn
          (if (string-match "^\\s-+indirectly-" line1)
              nil
            (if (string-match "^\\s-+indirectly-" line2)
                t
              (if (string< desc1 desc2)
                  t
                nil)))
          )
      (if (< ver-num1 ver-num2) t nil))
    ))

(defun bug-cmp-non-ver-lines (line-info1 line-info2)
  "Compare two lines w/ no version information in them."
  (let (id1 id2)
    (setq id1 (nth 1 line-info1))
    (setq id2 (nth 1 line-info2))
    (if (< id1 id2) t nil)
    ))

(defun bug-sort-blocked-lines (lines)
  "Sort blocked lines."
  (let ((ret "")
        line-list ver-lines non-ver-lines line id desc ver-num line-info)
    ; convert lines into list
    (setq line-list (split-string lines "\n"))
    ; split lines w/ version and lines w/ no version
    (while line-list
      (setq line (car line-list))
      (setq line-info (bug-decode-block-line line))
      (setq id (nth 1 line-info))
      (setq desc (nth 2 line-info))
      (setq ver-num (nth 3 line-info))
      (if ver-num 
          (push line-info ver-lines)
        (push line-info non-ver-lines))
      (setq line-list (cdr line-list))
      )
    (setq ver-lines (sort ver-lines 'bug-cmp-ver-lines))
    (setq non-ver-lines (sort non-ver-lines 'bug-cmp-non-ver-lines))
    (when ver-lines
      (setq ret (mapconcat (function (lambda (x) (car x))) ver-lines "\n"))
      (when non-ver-lines)
      (setq ret (concat ret "\n")))
    (when non-ver-lines
      (setq ret (concat ret (mapconcat (function (lambda (x) (car x)))
                                       non-ver-lines
                                       "\n"))))
    ret
    ))

(defun bug-sort-blocks-region ()
  "Sort region of blocked lines based on release versions found in the
bug descriptions."
  (let (lines)
    (when bug-blocks-section-start
      (setq lines (buffer-substring-no-properties bug-blocks-section-start
                                                  bug-blocks-section-end))
      (goto-char bug-blocks-section-start)
      (delete-region bug-blocks-section-start bug-blocks-section-end)
      (insert (bug-sort-blocked-lines lines))
      )
    ))

;;; ==========================================================================
;;; buffer management functions

(defun bug-kill-all-buffers-internal (kill-self-too)
  "Kill all [BUGxxxx] related buffers.

If kill-self-too is t, kill current buffer as well." 
  (let ((buf-list (buffer-list))
        bufname)
    (while buf-list
      (setq bufname (buffer-name (car buf-list)))
      (when (or (string-match "^\\*P4 describe:" bufname)
                (string= bug-cmd-output-buf-name bufname)
                (string-match "^\\[BUG" bufname))
        (when (or kill-self-too
                  (not (string= bufname (bug-buf-name bug-cur-bug-id))))
          (kill-buffer bufname)))
      (setq buf-list (cdr buf-list))
      )
    ))

(defun bug-kill-all-buffers ()
  (interactive)
  (bug-kill-all-buffers-internal t))
  
(defun bug-kill-all-other-buffers ()
  (interactive)
  (bug-kill-all-buffers-internal nil))

;;; --------------------------------------------------------------------
;;; field modifying commands

(defvar bug-cmd-output-buf-name "*bug-cmd-output*")

(defun bug-do-invoke-cmd (cmd &optional buf-name output-only)
  (let (tmp-buf exit-code cmd-tokens output)
    (if (not buf-name)
        (setq buf-name bug-cmd-output-buf-name))
    (setq tmp-buf (get-buffer-create buf-name))

    ;;(setq cmd "a4 bugs 40004")
    ; invoke cmd through shell so that we can set the EDITOR env var to emacsclient
    ;; (setq cmd-tokens (list "bash" "-c"
    ;; (format "EDITOR=%s; %s" bug-emacsclient-cmd cmd)))
    (setq cmd-tokens (list "bash" "-c" cmd))

    ;;(print cmd-tokens)
    ; break command down in separate tokens b/c this is how call-process needs it
    ;; (setq cmd-tokens (delete "" (split-string cmd)))
    (save-excursion
      (set-buffer tmp-buf)
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))
      )

    ; XXX: we should try async process invocation to avoid getting completely
    ; stuck waiting for the bugs database (which happen rather often)  
    (setq exit-code (apply 'call-process (car cmd-tokens) nil tmp-buf nil
                           (cdr cmd-tokens)))
    (save-excursion
      (set-buffer tmp-buf)
      (setq output (buffer-string))
      (when (not output-only)
        (goto-char (point-min))
        (insert (format "Command:\n\n  %s\n\nReturn code: %d\n\nOutput:\n\n"
                        cmd exit-code))
        )
      (goto-char (point-min))
      (setq buffer-read-only t)
      )

    ;; (message (format "a4 bug command eror: %d" exit-code))
    ;; (display-buffer tmp-buf t)

    (list exit-code output buf-name tmp-buf)
    ))

(defun bug-invoke-cmd (cmd &optional buf-name no-bug-buffer-refresh display-buf output-only)
  (interactive)
  (let (ret-list exit-code output tmp-buf)
    (setq ret-list (bug-do-invoke-cmd cmd buf-name output-only))
    (setq exit-code (car ret-list))
    (setq output (nth 1 ret-list))
    (setq tmp-buf (nth 3 ret-list))
    (if (= exit-code 0)
        (progn
          (if (not no-bug-buffer-refresh)
              (bug-refresh))
          (if (or bug-debug-p display-buf)
              (display-buffer tmp-buf t))
          )
      (message (format "'%s' command error: %d" cmd exit-code))
      (display-buffer tmp-buf t)
      )
    exit-code
    ))

(defun bug-add-comment ()
  (interactive)
  ; open separate buffer for editing the comments; calls us back when done."
  (bug-cedit bug-cur-bug-id 'bug-comment-completion-callback))

(defun bug-comment-completion-callback (bug-id comments)
  "This function is invoked when user ends the edition of a comment.
Args:
  - bug-id   - bug-id for which the comments are for 
  - comments - contents of the comment
"
  ; invoke 'a4 bug <bug-id> -c '<comments>'
  ; notice: we must quote all single quotes inside the comment
  (if (= 0 (bug-invoke-cmd (format "%s -c '%s'" 
                                   (bug-a4-bug-cmd-from-settings bug-id)
                                   (replace-regexp-in-string "'" "'\\\\''" comments))))
      (goto-char (point-max)))
    )


(defun bug-set-release-note ()
  (interactive)
  (bug-invoke-cmd (format "%s --release-note" (bug-a4-bug-cmd-from-settings bug-cur-bug-id)))
  )


;;; --------------------------------------------------------------------
;;; p4-* extensions

(defun bug-describe-change-internal (change-num)
  (p4-describe-internal (p4-make-list-from-string 
                         (concat "-du " change-num))))

(defun bug-describe-change-num (change-num)
  (bug-describe-change-internal change-num)
  )

(defp4cmd bug-describe-change ()
  "describe" 
  "To jump a description for a change number, type \\[bug-describe-change].\n"
  (interactive)
  (bug-describe-change-internal (bug-word-around-point)))


;;; --------------------------------------------------------------------
;;; property related functions (adapted from p4.el)

;; Find out what type of emacs we are running in. We will be using this
;; quite a few times in this program.
(eval-and-compile
  (defvar bug-running-emacs nil
    "If the current Emacs is not XEmacs, then, this is non-nil.")
  (defvar bug-running-xemacs nil
    "If the current Emacs is XEmacs/Lucid, then, this is non-nil.")
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      (setq bug-running-xemacs t)
    (setq bug-running-emacs t)))

(defun bug-set-extent-properties (start end prop-list)
  (cond (bug-running-xemacs
	 (let ((ext (make-extent start end)))
	   (while prop-list
	     (set-extent-property ext (caar prop-list) (cdar prop-list))
	     (setq prop-list (cdr prop-list)))))
	(bug-running-emacs
	 (let ((ext (make-overlay start end)))
	   (while prop-list
	     (overlay-put ext (caar prop-list) (cdar prop-list))
	     (setq prop-list (cdr prop-list)))))))

(defun bug-create-active-link (start end prop-list)
  ;; add link to the link-list if it's not already there
  (if (not (member start bug-link-list))
      (setq bug-link-list (append bug-link-list (list start))))
  (bug-set-extent-properties start end
                             (append (list (cons 'face 'bold)
                                           (cons 'mouse-face 'highlight))
                                     prop-list)))

(defun bug-set-face-property (start end face-property)
  (bug-set-extent-properties start end
                             (list (cons 'face face-property))))

(defun bug-create-proj-ref (start end)
  ;; add ref to the proj-ref-list if it's not already there
  (let (prop-list)
    (if (not (member start bug-proj-ref-list))
        (setq bug-proj-ref-list (append bug-proj-ref-list (list start))))
    (bug-set-extent-properties start end
                               (append (list (cons 'face 'font-lock-doc-face)
                                             (cons 'mouse-face 'highlight))
                                       prop-list))
    ))

;;; --------------------------------------------------------------------
;;; mode-line related stuff (copied from which-func.el)

(defface bug-status
  ;; Whether `font-lock-function-name-face' is an appropriate face to
  ;; inherit depends on the mode-line face; define several variants based
  ;; on the default mode-line face.
  '(;; The default mode-line face on a high-color display is a relatively
    ;; light color ("grey75"), and only the light-background variant of
    ;; `font-lock-function-name-face' is visible against it.
    (((class color) (min-colors 88) (background light))
     :inherit font-lock-function-name-face)
    ;; The default mode-line face on other display types is inverse-video;
    ;; it seems that only in the dark-background case is
    ;; `font-lock-function-name-face' visible against it.
    (((class grayscale mono) (background dark))
     :inherit font-lock-function-name-face)
    (((class color) (background light))
     :inherit font-lock-function-name-face)
    ;; If none of the above cases, use an explicit color chosen to contrast
    ;; well with the default mode-line face.
    (((class color) (min-colors 88) (background dark))
     :foreground "Blue1")
    (((background dark))
     :foreground "Blue1")
    (t
     :foreground "LightSkyBlue"))
  "Face used to highlight mode line function names."
  :group 'bug-mode)

(defconst bug-mode-status
  '(:eval (bug-get-mode-status-str)))
;;;###autoload (put 'bug-mode-status 'risky-local-variable t)

(defconst bug-proj-status
  '(:eval (bug-get-proj-status-str)))
;;;###autoload (put 'bug-mode-status 'risky-local-variable t)

(defcustom bug-status-format
  `("("
    (:propertize bug-mode-status
		 face bug-status)
    ")--"

    "["
    (:propertize bug-proj-status
                 face bug-status)
    "]"
    )
  "Format for displaying the bug-mode related status in the mode line."
  :group 'bug-mode
  :type 'sexp)
;;;###autoload (put 'bug-status-format 'risky-local-variable t)

; project related status

(defun bug-get-proj-status-str ()
  (if bug-cur-project-name bug-cur-project-name ""))

(defun bug-get-mode-status-str ()
  "Return status string to be shwon in the mode-line."
  (let (status-list)
    (if bug-show-all-comments-p
        (push "a" status-list))
    (if bug-show-proj-comments-p
        (push "P" status-list))
    (if bug-show-activity-p
        (push "A" status-list))
    (if bug-quiet-mode
        (push "Q" status-list))
    (if (bug-show-fsummary-needed)
        (push "s" status-list))
    (if bug-deps-status-str
        (push bug-deps-status-str status-list))
    ; push insert elements in the beginning of the list, so use
    ; (reverse...) to get the right order
    (mapconcat 'identity (reverse status-list) " ")
    ))

;;; --------------------------------------------------------------------
;;; mode for editing bug comments / release notes

(defun bug-cedit-buf-name (bug-id)
  (format "[BUG %s(c)]" bug-id))

(defun bug-cedit (bug-id completion-callback)
  (interactive)
  (let ((buf-name (bug-cedit-buf-name bug-id))
        (tip-str "Type 'C-x #' when done, or 'C-c C-c' to cancel.")
        buf output)
    ; set bug-id to var that will be local to buffer
    (setq bug-cedit-cur-bug-id bug-id)

    ; create / show new buffer
    (setq buf (get-buffer-create buf-name))
    (set-buffer buf)
    (pop-to-buffer buf)
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max))
    (goto-char (point-min))
    
    (insert (format "\nAdd your bug comment here. %s" tip-str))
    (message tip-str)

    (goto-char (point-min))
    (bug-cedit-mode)
    ; we're in bug-cedit mode now, so set the completion callback in buffer-local var
    (setq bug-cedit-completion-callback completion-callback)
    
  ))

(defun bug-cedit-done ()
  (interactive)
  (let ((bug-id bug-cedit-cur-bug-id)
        (completion-callback bug-cedit-completion-callback)
        (contents (buffer-substring-no-properties (point-min) (point-max)))
        ok
        )
    ; kill comment edition comment
    (kill-buffer)
    ; go back to original buffer and refresh it
    ; XXX: should check if buffer is still there
    (set-buffer (bug-buf-name bug-id))
    ; invoke the completion callback passing back the bug-id and the contents
    ; the comment buffer
    (funcall completion-callback bug-id contents)
    ))

(defun bug-cedit-cancel ()
  (interactive)
  (if (y-or-n-p "Cancel editing of new comment? ")
      (kill-buffer))
  (message "")
  )

;; key-map for cedit mode

(easy-mmode-defmap bug-cedit-mode-map
  `(("\C-c\C-c" . bug-cedit-cancel)
    ("\C-x#" . bug-cedit-done)
    )
  "Keymap for `bug-cedit-mode'.")

;; global variables (used to pass arguments to bug-cedit-mode)
(defvar bug-bug-id-for-cedit "1234")

;; buffer local variables
(defvar bug-cedit-cur-bug-id "")
(defvar bug-cedit-completion-callback nil)

;;;###autoload
(define-derived-mode bug-cedit-mode fundamental-mode "Bug-Comment"
  "Major mode for editing bug comments.

\\{bug-cedit-mode-map}"

  (make-local-variable 'bug-cedit-cur-bug-id)
  (make-local-variable 'bug-cedit-completion-callback)
  )

;;; --------------------------------------------------------------------
;;; actual mode definition

(defun bug-buffer-exists (buf-name)
  (member buf-name (mapcar (function buffer-name) (buffer-list))))

;;;###autoload
(define-derived-mode bug-mode fundamental-mode "Bug"
  "Major mode for bug management. Works as a UI front-end for bug management
commands such as 'a4 bug' and 'a4 bugs'. 

Entering bug-mode:
------------------

  M-x `bug'                 - prompts for bug-id or bug alias and open buffer
                              in bug-mode for it

  M-x `bug-goto'            - open buffer in bug-mode for bug id under cursor

  M-x `bug-describe-change' - open buffer in p4-describe mode for changelist#
                              under cursor

  (see 'Suggested Global Key Bindings' section below)


Navigation:
-----------

  n, SPC           - move to next section
  p                - move to previous section
  C-down           - next project reference
  C-up             - previous project reference
  TAB              - move to next actionable field (e.g. bug-id or changelist#)
  S-TAB            - move to previous actionable field
  ENTER            - execute actionable field (e.g. open new buffer for bug-id,
                     execute `p4-describe' for changelist#, download/open URLs,
                     etc.)
  q                - close buffer

URL Downloads:
--------------

  In the case of URLs, files are first downloaded by default to \"/tmp\". This
  directory can be changes by setting the following variable in your emacs
  init file:

    (setq bug-tmp-dir \"/home/jdoe/tmp\")

  By default, if an URL is identified as an autotest log file, it will be
  automatically set to AutoTest mode (atest-mode). This can be a problem
  if files are very big, as atest-mode may take very long. You can control
  the automatic setting to this mode by either:

  - M-x `bug-toggle-auto-atest-mode' or

  - adding following code to your emacs init file:

    (setq bug-toggle-auto-atest-mode nil)

Display Options:
----------------

  a    - toggle 'all-comments' mode

  P    - toggle 'project-comments' only mode

  A    - toggle 'Activity history' display

  D    - toggle 'Detailed Failure Summary' display

  r    - refresh buffer (useful after changing something through a4 bug
         command on a shell)

  d    - dependencies menu:

         d o  - toggle dependencies display on/off
         d d  - toggle \"directly dependencies only\" flag 
         d a  - toggle \"open only\" flag 

         (see 'Bug-mode Status' section below)

  s    - failure summary menu:

         s d  - toggle display of failure summary by DUT 
         s t  - toggle display of failure summary by Test
         s p  - toggle display of failure summary by project 
         s a  - show all failure summaries
         s n  - show no failure summaries
       
  S    - detailed failure summary menu:

         S d  - toggle display of detailed failure summary by DUT 
         S t  - toggle display of detailed failure summary by Test
         S p  - toggle display of detailed failure summary by project 
         S a  - show all detailed failure summaries
         s n  - show no detailed failure summaries
       

Modification Operations:
------------------------

  :    - modification menu

  This will prompt the available options in the minibuffer, e.g.:

    Menu:  a-add  d-delete  s-set  p-project-ops

  Simply follow the menu options displayed for each selection. 

  For most cases, it's just a matter of following the initials of what
  you're trying to do. For example:

    : a c   - add comment
    : a f   - add fixed-by
    : d r   - delete release-note
    : s a   - set alias
  
    And so on...
 
  Quick explanation of how the operations are organized:

    - add     => applies to multiple-instance and/or deletable fields
                 (e.g. comments, fixed-by entries, release-note, etc.)

    - delete  => applies to multiple-instance / deletable fields
                 (e.g. fixed-by entries, release-note, etc.)

    - set     => applies to single-instance fields
                 (e.g. priority, severity, assigned-to, etc.)

    - project-ops => project related operations (e.g. select current
                     project; move bug to ignore, triage, etc. lists
                     of the current project)


Project Related Operations:
---------------------------

Accessible through ': p' sequence.

Operations:

   : p s  - set current project (automatically shows only autotest/
            autobuild generated comments for this project).
            Current project is shown in the mode bar inside []s.

   : p i  - move bug to <cur-project>-ignore list

   : p l  - move bug to <cur-project>-ignore list

   : p m  - move bug to <cur-project>-mustfix list

   : p t  - move bug to <cur-project>-triage list


Buffer Management Operations:
-----------------------------

  M-x `bug-kill-all-buffers'        - kill all [BUGxxxx] and its related
                                      buffers (e.g. *P4 describe* ones)
                                      (see 'Suggested Global Key Bindings'
                                      section below)

  M-x `bug-kill-all-other-buffers'  - kill all [BUGxxxx] and its related
                                      buffers except for the current
                                      [BUGxxxx] one.

                                      Only applies to buffers in bug mode;
                                      bound to key 'C-c k')

Mail Functions
--------------

  C-c m     - Open buffer in mail-mode w/ pre-filled subject: and to:
              fields.

              C-c C-f C-c adds the CC: field (if you need one).

              To send the email, type C-c C-c when done entering the
              email text.

              For additional help in mail mode, do: 

                M-x describe-function RET mail RET

              From a chroot'ed workspace, you'll have to install the
              mailx package:

                 a4 yum install -y mailx 

              You can have it installed automatically on new workspaces
              by adding the command above to your //user/<userid>/.newtreerc
              file.


Misc Functions:
---------------

  C-c C-b   - copy buffer \"BUG<bug-id>\" into clipboard (handy for pasting it
              into email subject or text)

  C-c C-w   - copy buffer id into clipboard (handy for pasting it into
              email subject or text)

  C-c C-q   - copy buffer id + short description into clipboard
              (handy for pasting them into email subject or text)

  Q         - toggle quiet flag (if set, email notifications are suppressed)

              Default value for quiet flag can be controlled through the
              'bug-default-quiet-mode' variable.

              To disable email notifications by default, set this variable
              to t by adding the following line to your emacs init file:

                (setq bug-default-quiet-mode t) 

  l         - search for bug's logre in //eng/etc/logre


Bug-mode Status on Mode Line:
-----------------------------

You can see status relative to bug-mode in the \"mode-line\" (line at
the bottom of the window that display information such as current buffer name,
line number / column number information, etc.).

Bug-mode status is shown inside the right-most parenthesis in the mode-line.

Here's the status displayed there:

  a           - Shown when \"show all comments\" flag is on

  P           - Shown when \"show project comments\" flag is on

  A           - Shown when \"show activity history\" flag is on

  s           - Shown when any \"show failure summary\" flag is on

  DS          - Shown when \"show detailed failure summary\" flag is on

  Q           - Shown when quiet flag is on (i.e. email notifications
                are supressed)

  Dep:<flags> - Shown when Dependencies section is enabled.
                Flag values:

                d - show direct dependencies only
                a - show all dependencies, not only the open ones
                g - graph mode


Suggested Global Key Bindings:
------------------------------

  C-x g b  or M-u    => `bug'
  C-x g g            => `bug-goto'
  C-x g d            => `bug-describe-change'
  C-x g k            => `bug-kill-all-buffers'
  C-x g q            => `bug-toggle-default-quiet-mode'

  To setup the key bindings above, copy the following code to your 
  emacs init file:

    ;; global key bindings for bug-mode related functions
    (global-set-key (kbd \"M-u\") 'bug)
    (global-set-key (kbd \"C-x g b\") 'bug)
    (global-set-key (kbd \"C-x g g\") 'bug-goto)
    (global-set-key (kbd \"C-x g d\") 'bug-describe-change)
    (global-set-key (kbd \"C-x g k\") 'bug-kill-all-buffers)
    (global-set-key (kbd \"C-x g q\") 'bug-toggle-default-quiet-mode)


Local Key Bindings
------------------

\\{bug-mode-map}"

  (let (buf-name sav-last-mode-line-elm)

    ; before anything else, define / initialize new buffer-local variables
    (set (make-local-variable 'font-lock-defaults) bug-font-lock-defaults)
    (set (make-local-variable 'imenu-generic-expression)
         bug-imenu-generic-expression)

    (make-local-variable 'bug-cur-bug-id)
    (make-local-variable 'bug-cur-bug-desc)
    (make-local-variable 'bug-cur-bug-alias)
    (make-local-variable 'bug-cur-assigned-to)

    (make-local-variable 'bug-show-all-comments-p)
    (make-local-variable 'bug-show-proj-comments-p)
    (make-local-variable 'bug-show-activity-p)
    (setq bug-show-all-comments-p nil)
    (setq bug-show-proj-comments-p nil)
    (setq bug-show-activity-p nil)

    (make-local-variable 'bug-show-fsummary-by-dut-p)
    (make-local-variable 'bug-show-fsummary-by-test-p)
    (make-local-variable 'bug-show-fsummary-by-proj-p)
    (make-local-variable 'bug-show-det-fsummary-p)
    (make-local-variable 'bug-fsummary-by-dut-start)
    (make-local-variable 'bug-fsummary-by-dut-end)
    (make-local-variable 'bug-fsummary-by-test-start)
    (make-local-variable 'bug-fsummary-by-test-end)
    (make-local-variable 'bug-fsummary-by-proj-start)
    (make-local-variable 'bug-fsummary-by-proj-end)
    (setq bug-show-fsummary-by-dut-p nil)
    (setq bug-show-fsummary-by-test-p nil)
    (setq bug-show-fsummary-by-proj-p nil)
    (setq bug-show-det-fsummary-p nil)

    (make-local-variable 'bug-link-list)
    (setq bug-link-list nil)
    (make-local-variable 'bug-proj-ref-list)
    (setq bug-proj-ref-list nil)

    (make-local-variable 'bug-quiet-mode)

    (make-local-variable 'bug-deps-section-start)
    (make-local-variable 'bug-deps-section-end)
    (make-local-variable 'bug-blocks-section-start)
    (make-local-variable 'bug-blocks-section-end)

    (make-local-variable 'bug-header-section-end)

    ; blocking display related local var init
    (make-local-variable 'bug-deps-on-p)
    (make-local-variable 'bug-deps-display-type)
    (make-local-variable 'bug-deps-open-only-p)
    (make-local-variable 'bug-deps-direct-only-p)
    (make-local-variable 'bug-deps-status-str)

    (setq buffer-read-only t)

    ; try to figure out the bug-id from buffer contents, 
    ; (bail out right away if we can't)
    (when (not (bug-get-cur-bug-id))
      (kill-buffer)
      (error (format "ERROR: could not identify bug id/alias '%s'." bug-last-id-requested)))

    ; set local keys for stuff we don't know how to represent yet in
    ; easy-mmode-defmap
    ;;(local-set-key (quote [C-up]) (quote bug-prev-section))
    ;;(local-set-key (quote [C-down]) (quote bug-next-section))
    ;;(local-set-key (quote [C-left]) (quote bug-prev-proj-ref))
    ;;(local-set-key (quote [C-right]) (quote bug-next-proj-ref))
    (local-set-key (quote [C-up]) (quote bug-prev-proj-ref))
    (local-set-key (quote [C-down]) (quote bug-next-proj-ref))
    (local-set-key (quote [return]) (quote bug-return-key))
    (define-key bug-mode-map (kbd "\r") (quote bug-return-key))
    (define-key bug-mode-map (kbd "<S-iso-lefttab>") (quote bug-prev-link))
    (define-key bug-mode-map (kbd "<backtab>") (quote bug-prev-link))
    (define-key bug-mode-map (kbd "<S-tab>") (quote bug-prev-link))
    (define-key bug-mode-map (kbd "M-[ z") (quote bug-prev-link))

    ; if this buffer name is different from actual bug (e.g. [<bug-alias>]
    ; instead of [<bug-id>]) delete buffer w/ bug-id if it already exists,
    ; and rename it to [<bug-id>]
    (setq buf-name (bug-buf-name bug-cur-bug-id))
    (when (not (string= buf-name (buffer-name)))
      (if (bug-buffer-exists buf-name)
          (kill-buffer buf-name))
      (rename-buffer buf-name))

    ; add our bug-mode related status to mode-line
    ; (notice: must insert our stuff right before the last element
    ;  in mode line, otherwise it doesn't get displayed)
    (setq sav-last-mode-line-elm (last mode-line-format))
    (setq mode-line-format (append (butlast mode-line-format)
                                   bug-status-format 
                                   (list sav-last-mode-line-elm)))
    ))

(defun x ()
  (interactive)
  (message bug-cur-project-name))

;;; --------------------------------------------------------------------
;;; provide the package

(provide 'bug-mode)

;;; --- EOF ---
