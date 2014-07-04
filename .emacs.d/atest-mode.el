;; atest-mode.el --- Arista AutoTest log mode

;; Copyright (c) 2012 Arista Networks, Inc.  All rights reserved.
;; Arista Networks, Inc. Confidential and Proprietary.

;; Author: Paulo Gallo <pgallo@aristanetworks.com>
;; Keywords: convenience AutoTest

;; Commentary:

;; Bugs:

;; Todo / Wish List:
;;
;;  - new features:
;;    - identify more error conditions (process restart / C++ traceback)
;;
;;    - identify kernel call trace as a troubled spots
;;        <staml> kernel: [ 2683.372101] Call Trace:
;;        <stamp> kernel: [ 2683.372111]  [<ffffffff81075d66>] ? T.456+0x6c/0x1b8
;;        ...
;;
;;    - scheme to collapse / expand major/regular sections
;;
;;    - traceback "lock" scheme (to make it easy to navigate back & forth through it)
;;
;;

(defvar atest-mode-version "1.0.0" "The Current AutoTest Mode Revision.")

(require 'p4)
(require 'pdbx)

(eval-when-compile (require 'cl))

(defgroup atest-mode ()
  "Major mode for viewing AutoTest logs at Arista."
  :version "21.1"
  :group 'tools
  :group 'atest)

;; customizable variables

(defcustom atest-placeholder-p nil
  "If non-nil, do something."
  :type 'boolean
  :group 'atest-mode)

;;; --------------------------------------------------------------------
;;; global vars

;;; --------------------------------------------------------------------
;;; buffer-local variables

(defvar atest-major-section-list nil)
(defvar atest-section-list nil)
(defvar atest-traceback-list nil)
(defvar atest-link-list nil)
(defvar atest-start-cur-traceback nil)
(defvar atest-end-cur-traceback nil)

;;; --------------------------------------------------------------------
;;; keymap for atest-mode

(easy-mmode-defmap atest-mode-shared-map
  '(
    ("n" . atest-next-section)
    ("p" . atest-prev-section)
    ("b" . atest-prev-section)

    ("N" . atest-next-major-section)
    ("P" . atest-prev-major-section)
    ("B" . atest-prev-major-section)
    ("?" . atest-help)

    ("t" . atest-next-traceback)
    ("r" . atest-prev-traceback)
    ("q" . quit-window)

    ("\C-i" . atest-next-link)
    ("\C-cr" . pdbx-cli-trace-region)
    ("\C-ck" . pdbx-kill-cli-trace)
    ("\C-ct" . atest-toggle-traceback-lock)
    )
  "Basic keymap for `atest-mode', bound to various prefix keys.")

(easy-mmode-defmap atest-mode-map
  `(;("\e" . ,atest-mode-shared-map)
    ("n" . atest-next-section)
    ("p" . atest-prev-section)
    ("b" . atest-prev-section)

    (" " . atest-next-major-section)
    ("N" . atest-next-major-section)
    ("P" . atest-prev-major-section)
    ("B" . atest-prev-major-section)
    ("?" . atest-help)

    ("t" . atest-next-traceback)
    ("r" . atest-prev-traceback)
    ("q" . quit-window)

    ("\C-i" . atest-next-link)
    ("\C-cr" . pdbx-cli-trace-region)
    ("\C-ck" . pdbx-kill-cli-trace)
    ("\C-ct" . atest-toggle-traceback-lock)
    )
  "Keymap for `atest-mode'.  See also `atest-mode-shared-map'.")

(defcustom atest-minor-mode-prefix "\C-c="
  "Prefix key for `atest-minor-mode' commands."
  :type '(choice (string "\e") (string "C-c=") string)
  :group 'atest-mode)

(easy-mmode-defmap atest-minor-mode-map
  `((,atest-minor-mode-prefix . ,atest-mode-shared-map))
  "Keymap for `atest-minor-mode'.  See also `atest-mode-shared-map'.")

;;; ==========================================================================
;;; font-lock support

(defface atest-section-header
  '((((class color) (min-colors 88) (background light))
     :background "grey80")
    (((class color) (min-colors 88) (background dark))
     :background "#333333")
    (((class color) (background light))
     :background "white")
    (((class color) (background dark))
     :background "white")
    (t :weight bold))
  "`atest-mode' face for a4 bugs output section headers."
  :group 'atest-mode)
;;(define-obsolete-face-alias 'atest-section-header-face 'atest-section-header "22.1")
(defvar atest-section-header-face 'atest-section-header)

(defface atest-de-emphasize
  '((((class color) (min-colors 88) (background light))
     :foreground "dark grey")
    (((class color) (min-colors 88) (background dark))
     :foreground "dark grey")
    (((class color) (background light))
     :foreground "white")
    (((class color) (background dark))
     :foreground "white")
    (t :weight bold))
  "`atest-mode' face for things to be de-emphasized."
  :group 'atest-mode)
;;(define-obsolete-face-alias 'atest-de-emphasize-face 'atest-de-emphasize "22.1")
(defvar atest-de-emphasize-face 'atest-de-emphasize)

(defvar atest-bold-face 'bold)

;(defvar
(setq atest-major-section-header-re "\\(^Starting test.*\\|^Art-run-begin.*\\|^Fetching information for .*\\|^Art-run-end.*\\|^Start of /var/log/messages.*\\|^Dut .* private debug state.*\\)")

(defalias 'atest-atest-id-face 'font-lock-function-name-face)
(defalias 'atest-change-num-face 'font-lock-function-name-face)

;(defvar 

(setq atest-font-lock-keywords
   `(
;;     (,atest-section-sub-header-re . atest-section-header-face)
;;     ("\\(^=+ Output from /.*\\)\n[^\n ]" (1 atest-section-header-face))
     ("\\(^Starting test.*\n\\|^Art-run-begin.*\n\\)" . atest-section-header-face)
     ("^\\(\\[[a-zA-Z0-9/-]*]\\)" . 'font-lock-keyword-face)
     ("^\\(\\[[a-zA-Z0-9/-]*] \\)?\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9:]*.[0-9]*\\)\s*\\([0-9]*\\) \\([a-zA-Z0-8_]*\\)"
      (2 'font-lock-variable-name-face)
      (3 'font-lock-type-face)
      (4 'font-lock-type-face) )
;     (" traceback" . 'font-lock-comment-delimiter-face)
     (" Exception" . 'font-lock-comment-delimiter-face)
     ("%[A-Z0-9_-]*". 'font-lock-constant-face )))

;; (setq atest-font-lock-keywords
;;   `((,atest-major-section-header-re . atest-section-header-face)
;;     ("^------+\\s-BUG\\s-+[0-9]+\\s--+------\n\\([0-9]+\\s-+.*\\)$"
;;      (1 atest-bold-face))
;;     ; ("^[0-9]+\\s-+.*$" . atest-bold-face)
;;     ("^By:\\s-*.*@.*$" . font-lock-variable-name-face)
;;     ("^----*\\s-+Comments\\s-+[-0-9].*suppressed.*$" . atest-de-emphasize-face)
;;     ("^----*\\s-+Run with --all-comments.*$" . atest-de-emphasize-face)
;;     (" | " . atest-de-emphasize-face)
;;     ("\\(\\\\_\\)\\s-+\\([0-9]+\\)"
;;      (1 atest-de-emphasize-face) (2 font-lock-function-name-face))
;;     ("\\(\\\\_\\)\\s-+\\([0-9]+\\)"
;;      (1 atest-de-emphasize-face) (2 font-lock-function-name-face))
;;     ))

(defconst atest-font-lock-defaults
  '(atest-font-lock-keywords t nil nil nil (font-lock-multiline . nil)))

;; (defvar atest-imenu-generic-expression
;;   '((nil "\\(^Starting test.*$\\|^Art-run-begin.*\\)" 1)
;;     ))

;;; ==========================================================================
;;; help related functions

(defun atest-help ()
  (interactive)
  (describe-function 'atest-mode))

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
;;; list navigation functions (could be common accross different modes)

(defun atest-next-list-elm (pnt lst)
  "Go to closest element after current position."
  (let (found (elm lst))
    (while (and elm (not found))
      (when (> (car elm) pnt)
        (goto-char (car elm))
        (setq found t)
        )
      (setq elm (cdr elm))
      )))

(defun atest-prev-list-elm (pnt lst)
  "Go to closest proj-ref before current position."
  (let* (found (tmp-list (reverse lst)) (elm tmp-list))
    (while (and elm (not found))
      (when (< (car elm) pnt)
        (goto-char (car elm))
        (setq found t)
        )
      (setq elm (cdr elm))
      )))

(defun atest-next-prop-change (pnt &optional limit)
  (let (next-change done)
    (setq limit (or limit (point-max)))
    (while (not done)
      (setq next-change (next-char-property-change pnt limit))
      (if (or (not next-change) (= next-change limit))
          (progn
            (message "No more active links past this point")
            (setq done t))
        (if (get-char-property next-change 'action)
            (progn
              (goto-char next-change)
              (setq done t))
          (setq pnt next-change))
        )
      )
    ))

(defun atest-prev-prop-change (pnt &optional limit)
  (let (prev-change done)
    (setq limit (or limit (point-min)))
    (while (not done)
      (setq prev-change (previous-char-property-change pnt limit))
      (if (or (not prev-change) (= prev-change limit))
          (progn
            (message "No more active links before this point")
            (setq done t))
        (if (get-char-property prev-change 'action)
            (progn
              (goto-char prev-change)
              (setq done t))
          (setq pnt prev-change))
        )
      )
    ))

(defun atest-next-link (pnt)
  "Go to closest link after current position. (Current position passed to
us through arg pnt because of interactive 'd' argument.)"
  (interactive "d")
  (atest-next-prop-change pnt atest-end-cur-traceback)
  )

(defun atest-prev-link (pnt)
  "Go to closest link before current position. (Current position passed to
us through arg pnt because of interactive 'd' argument.)"
  (interactive "d")
  (atest-prev-prop-change pnt atest-start-cur-traceback)
  )

;;; ==========================================================================
;;; navigation functions

;; (defun atest-next-section ()
;;   (interactive)
;;   (a4-nav-imenu-next))

;; (defun atest-prev-section ()
;;   (interactive)
;;   (a4-nav-imenu-prev))

(defun atest-next-section (pnt)
  "Go to closest section after current position.
Current position passed to us through arg pnt because of interactive
'd' argument.)"
  (interactive "d")
  (atest-next-list-elm pnt atest-section-list))

(defun atest-prev-section (pnt)
  "Go to closest section before current position.
Current position passed to us through arg pnt because of interactive
'd' argument.)"
  (interactive "d")
  (atest-prev-list-elm pnt atest-section-list))

(defun atest-next-major-section (pnt)
  "Go to closest major-section after current position.
Current position passed to us through arg pnt because of interactive
'd' argument.)"
  (interactive "d")
  (atest-next-list-elm pnt atest-major-section-list))

(defun atest-prev-major-section (pnt)
  "Go to closest major-section before current position.
Current position passed to us through arg pnt because of interactive
'd' argument.)"
  (interactive "d")
  (atest-prev-list-elm pnt atest-major-section-list))

(defun atest-next-traceback (pnt)
  "Go to closest traceback after current position.
Current position passed to us through arg pnt because of interactive
'd' argument.)"
  (interactive "d")
  (atest-next-list-elm pnt atest-traceback-list))

(defun atest-prev-traceback (pnt)
  "Go to closest traceback before current position.
Current position passed to us through arg pnt because of interactive
'd' argument.)"
  (interactive "d")
  (atest-prev-list-elm pnt atest-traceback-list))

(defun atest-return-key (pnt)
  (interactive "d")
  (let ((action (get-char-property pnt 'action))
	(value (get-char-property pnt 'value)))
    (cond
     ((string= action "goto-file")
      (atest-goto-file (car value) (nth 1 value))
      )
     (t
      (error (concat "Invalid action: " action))
      )
     )
    ))

;;; ==========================================================================
;;; action functions

(defun atest-goto-file (file linenum)
  (let ()
    (find-file-other-window file)
    (goto-line linenum)
    (end-of-line)
    ;;(message (format "%s, %s" file linenum))
    ))

;;; ==========================================================================
;;; buffer activation functions

;--------------- Nov 29 03:24 SandFabric-Fabric6-2997 [offset 0:68452] ---------------

(defun atest-activate-buffer ()
  (let (start end)
    (save-excursion
      (setq atest-link-list nil)
      (setq buffer-read-only nil)

      ;; hightlight / expand major section headers
      (goto-char (point-min))
      (while (re-search-forward atest-major-section-header-re (point-max) t)
        (beginning-of-line)
        (setq start (point))
;;        (insert "\n")
        (end-of-line)
;;        (insert "\n")
;;        (atest-set-face-property start (+ (point) 1) atest-section-header-face)
        ;; add major section to both major- and regular-section lists
        (setq end (+ (point) 1))
        (atest-add-section-header start end)
        (atest-add-major-section-header start end)
        )

      ;; identify / highlight regular section headers / tracebacks
;;      (setq atest-regular-section-header-re "^\\(-+ [A-Z][a-z][a-z] [0-9]+ [0-9]+:[0-9]+ [A-Za-z]+.*-[0-9]+ .*-+\\)\n[^\n ]\\|^\\(Cleaning up.*\\)\n\\|^\\(Traceback.*\\)\\|^\\(=+ Exception raised in .*\\)\\|^\\(Output of show .*\\)\n\\|^\\(Current state of all running processes.*\\)\n\\|^\\(show platform .*\\)\n\\|^\\(show int counters queue.*\\)\n\\|^\\(Errors from log scan.*\\)\n\\|^\\(.*Assertion `!(true)' failed.\\)\n")

      (setq 
       atest-regular-section-header-re
       (concat
        "^\\(-+ [A-Z][a-z][a-z] [0-9]+ [0-9]+:[0-9]+ [A-Za-z]+.*-[0-9]+ .*-+\\)\n[^\n ]\\|"
        "^\\(Cleaning up.*\\)\n\\|"
        "^\\(Traceback.*\\)\n\\|"
        "^\\(=+ Exception raised in .*\\)\n\\|"
        "^\\(Output of show .*\\)\n\\|"
        "^\\(Current state of all running processes.*\\)\n\\|"
        "^\\(show platform .*\\)\n\\|"
        "^\\(show int counters queue.*\\)\n\\|"
        "^\\(Errors from log scan.*\\)\n\\|"
        "^\\(Assertion `!(true)' failed\\)\n\\|"
        "^\\(Test run ended because of TIMEOUT\\)"
        ))

      (goto-char (point-min))
      (while (re-search-forward atest-regular-section-header-re (point-max) t)
        (cond
         ((match-beginning 1)
          (atest-add-section-header (match-beginning 1) (match-end 1)))
         ((match-beginning 2)
          (atest-add-section-header (match-beginning 2) (match-end 2)))
         ((match-beginning 3)
          (atest-add-traceback (match-beginning 3) (match-end 3)))
         ((match-beginning 4)
          (atest-add-traceback (match-beginning 4) (match-end 4)))
         ((match-beginning 5)
          (atest-add-section-header (match-beginning 5) (match-end 5)))
         ((match-beginning 6)
          (atest-add-section-header (match-beginning 6) (match-end 6)))
         ((match-beginning 7)
          (atest-add-section-header (match-beginning 7) (match-end 7)))
         ((match-beginning 8)
          (atest-add-section-header (match-beginning 8) (match-end 8)))
         ((match-beginning 9)
          (atest-add-traceback (match-beginning 9) (match-end 9)))
         ((match-beginning 10)
          (atest-add-traceback (match-beginning 10) (match-end 10)))
         ((match-beginning 11)
          (atest-add-traceback (match-beginning 11) (match-end 11)))
          )
        )

      ; activate all traceback blocks
      (atest-activate-all-tracebacks)
      
      ; sort section lists
      (setq atest-section-list (sort atest-section-list '<))
      (setq atest-major-section-list (sort atest-major-section-list '<))

      ;; imenu locations have changed, so need refresh it
;      (a4-nav-imenu-refresh)
      (setq buffer-read-only t)
      )
  ))

;;; ==========================================================================
;;; property related functions (adapted from p4.el)

;; Find out what type of emacs we are running in. We will be using this
;; quite a few times in this program.
(eval-and-compile
  (defvar atest-running-emacs nil
    "If the current Emacs is not XEmacs, then, this is non-nil.")
  (defvar atest-running-xemacs nil
    "If the current Emacs is XEmacs/Lucid, then, this is non-nil.")
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      (setq atest-running-xemacs t)
    (setq atest-running-emacs t)))

(defun atest-set-extent-properties (start end prop-list)
  (cond (atest-running-xemacs
	 (let ((ext (make-extent start end)))
	   (while prop-list
	     (set-extent-property ext (caar prop-list) (cdar prop-list))
	     (setq prop-list (cdr prop-list)))))
	(atest-running-emacs
	 (let ((ext (make-overlay start end)))
	   (while prop-list
	     (overlay-put ext (caar prop-list) (cdar prop-list))
	     (setq prop-list (cdr prop-list)))))))

(defun atest-set-face-property (start end face-property)
  (atest-set-extent-properties start end
                               (list (cons 'face face-property))))

(defun atest-create-active-link (start end prop-list)
  ;; add link to the link-list if it's not already there
  (if (not (member start atest-link-list))
      (setq atest-link-list (append atest-link-list (list start))))
  (atest-set-extent-properties start end
                               (append (list (cons 'face 'bold)
                                             (cons 'mouse-face 'highlight))
                                       prop-list)))

(defun atest-create-file-link (start end file linenum)
   (let (prop-list)
     (setq prop-list (list (cons 'face 'font-lock-function-name-face)
                           (cons 'action "goto-file")
                           (cons 'value (list file linenum))))
     (atest-create-active-link start end prop-list)
     ))

(defun atest-add-major-section-header (start end)
  ;; add major-section header
  (let (prop-list)
    (if (not (member start atest-major-section-list))
        (setq atest-major-section-list (append atest-major-section-list (list start))))
    (atest-set-extent-properties start end
                                 (append (list (cons 'face atest-section-header-face)
                                               (cons 'mouse-face 'highlight))
                                         prop-list))
    ))

(defun atest-add-section-header (start end)
  ;; add section header
  (let (prop-list)
    (if (not (member start atest-section-list))
        (setq atest-section-list (append atest-section-list (list start))))
    (atest-set-extent-properties start end
                                 (append (list (cons 'face atest-section-header-face)
                                               (cons 'mouse-face 'highlight))
                                         prop-list))
    ))

(defun atest-add-traceback (start end)
  ;; add traceback
  (let (prop-list)
    (if (not (member start atest-traceback-list))
        (setq atest-traceback-list (append atest-traceback-list (list start))))
    ;; add traceback to major and regular sections list as well
    (if (not (member start atest-major-section-list))
        (setq atest-major-section-list (append atest-major-section-list (list start))))
    (if (not (member start atest-section-list))
        (setq atest-section-list (append atest-section-list (list start))))
    (atest-set-extent-properties start end
;                                 (append (list (cons 'face 'font-lock-comment-delimiter-face)
                                 (append (list (cons 'face 'isearch-fail)
                                               (cons 'mouse-face 'highlight))
                                         prop-list))
    ))

;;; ==========================================================================
;;; traceback related functions

;(setq atest-traceback-start-re "\n\\(Traceback.*:\n\\s-+File \\)")
(setq atest-traceback-start-re
 "^\\(\\(Traceback.*:\n\\s-+File \\)\\|\\(=+ Exception raised in .*\nLocal variables by frame\\)\\)")
(setq atest-traceback-end-re "\n[^ \n]")

(defun atest-activate-traceback (start end)
  "Activate a traceback block"
  (let ()
    (goto-char start)
    (while (re-search-forward "\\s-+\\(File \"\\(.*\\)\", line \\([0-9]+\\)\\), in \\(.*\\)\n" end 1)
;         "\\s-+File \"\\(.*\\)\", line \\([0-9]+\\)" end 1)
;;      (atest-set-face-property (match-beginning 1) (match-end 1) 'font-lock-function-name-face)
;;      (atest-set-face-property (match-beginning 2) (match-end 2) 'lazy-highlight)
      (atest-set-face-property (match-beginning 4) (match-end 4) 'bold)

      (atest-create-file-link (match-beginning 1) (match-end 1)
                              (match-string-no-properties 2)
                              (string-to-number (match-string-no-properties 3)))
      (message (format "File: %s Line: %s" 
                       (match-string-no-properties 2)
                       (match-string-no-properties 3)
                       ))
      )
    ))

(defun x ()
  (interactive)
  (atest-activate-all-tracebacks)
  )

(defun atest-activate-all-tracebacks ()
  "Activate all python tracebacks found in the buffer."
  (let (start)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward atest-traceback-start-re (point-max) t)
        (setq start (match-beginning 1))
        (re-search-forward atest-traceback-end-re (point-max) 1)
        (atest-activate-traceback start (line-end-position))
        (beginning-of-line)
        )
      )
    ))

(defun atest-lock-traceback ()
  (let (start (cur-pos (point)))
    (save-excursion
      (when (re-search-backward atest-traceback-start-re (point-min) t)
        (setq start (match-beginning 1))
        (when (re-search-forward atest-traceback-end-re (point-max) 1)
          (when (<= cur-pos (point))
            (setq atest-start-cur-traceback start)
            (setq atest-end-cur-traceback (point))
            ))
        )
      )
    ))

(defun atest-unlock-traceback ()
  (setq atest-start-cur-traceback nil)
  (setq atest-end-cur-traceback nil)
  )

(defun atest-toggle-traceback-lock ()
  (interactive)
  (if atest-start-cur-traceback
      (atest-unlock-traceback)
    (atest-lock-traceback))
  )

;;; ==========================================================================
;;; actual mode definition

;;;###autoload
(define-derived-mode atest-mode fundamental-mode "Atest"
  "Major mode for AutoTest log viewing.

Entering atest-mode:
------------------

  M-x `atest-mode'          - enter atest-mode

  (see 'Suggested Global Key Bindings' section below)

Navigation:
-----------

  N, SPC           - move to next *major* section (e.g. log begin/end, 
                     individual tests begin/end, troubled spots, etc.)
  P                - move to previous *major* section

  n                - move to next section (all major sections, plus start
                     of fetched log files, etc.)
  p                - move to previous section

  t                - move to next \"troubled spot\" (e.g. tracebacks, unexpected
                     log entries, C++ assertions, etc.)
  r                - move to previous troubled spot

  TAB              - move to next actionable field (e.g. file link)
  S-TAB            - move to previous actionable field
  ENTER            - execute actionable field (e.g. open file link)

  ?                - help
  q                - quit

Cli Trace Decoding:
-------------------

  C-c r            - Decode CLI pexpect trace in region
  C-c k            - Kill CLI trace decode buffer

Suggested Global Key Bindings:
------------------------------

  M-s a => `atest-mode'   

  To setup the key bindings above, copy the following code to your 
  emacs init file:

    ;; global key bindings for atest-mode related functions
    (global-set-key (kbd \"M-s a\") 'atest-mode)

Local Key Bindings
------------------

\\{atest-mode-map}"

  (let (buf-name sav-last-mode-line-elm)

    ; before anything else, define / initialize new buffer-local variables
    (set (make-local-variable 'font-lock-defaults) atest-font-lock-defaults)
;;    (set (make-local-variable 'imenu-generic-expression)
;;         atest-imenu-generic-expression)

    (make-local-variable 'atest-major-section-list)
    (setq atest-major-section-list nil)

    (make-local-variable 'atest-section-list)
    (setq atest-section-list nil)

    (make-local-variable 'atest-traceback-list)
    (setq atest-traceback-list nil)

    (make-local-variable 'atest-link-list)
    (setq atest-link-list nil)

    (make-local-variable 'atest-start-cur-traceback)
    (setq atest-start-cur-traceback nil)

    (make-local-variable 'atest-end-cur-traceback)
    (setq atest-end-cur-traceback nil)

    ; set local keys for stuff we don't know how to represent yet in
    ; easy-mmode-defmap
    ;;(local-set-key (quote [C-up]) (quote atest-prev-section))
    ;;(local-set-key (quote [C-down]) (quote atest-next-section))
    ;;(local-set-key (quote [C-left]) (quote atest-prev-proj-ref))
    ;;(local-set-key (quote [C-right]) (quote atest-next-proj-ref))
    ;(local-set-key (quote [C-up]) (quote atest-prev-proj-ref))
    ;(local-set-key (quote [C-down]) (quote atest-next-proj-ref))

    (define-key atest-mode-map (kbd "<S-iso-lefttab>") (quote atest-prev-link))
    (define-key atest-mode-map (kbd "<backtab>") (quote atest-prev-link))
    (define-key atest-mode-map (kbd "<S-tab>") (quote atest-prev-link))
    (define-key atest-mode-map (kbd "M-[ z") (quote atest-prev-link))

    (define-key atest-mode-map (kbd "\r") (quote atest-return-key))
    (local-set-key (quote [return]) (quote atest-return-key))

    (atest-activate-buffer)
    (setq buffer-read-only t)
    (message "AutoTest Log Mode: press ? for help")
    ))

;;; --------------------------------------------------------------------
;;; provide the package

(provide 'atest-mode)

;;; --- EOF ---
