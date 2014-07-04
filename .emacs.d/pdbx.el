;;; pdbx.el --- Pdb extensions for emacs

;; Copyright (c) 2012 Arista Networks, Inc.  All rights reserved.
;; Arista Networks, Inc. Confidential and Proprietary.

;; Author: Paulo Gallo <pgallo@aristanetworks.com>
;; Keywords: convenience python pdb

;;; Commentary:

;; Provides extensions for pdb usage in emacs, like decoding cli trace from
;; pexepct logs and setting breakpoints and jump points, etc. directly from
;; python files.

;;; Bugs:

;;; Todo / Wish List:
;;
;;  - support for loading commands from a file (e.g. list of breakpoints
;;    be setup w/ a single command / keystroke)

;;; Code:

(defvar pdbx-version "1.0.0" "The Current PDB Extensions Revision.")

;;; --------------------------------------------------------------------
;;; global vars

(defvar pdbx-cur-pdb-buffer "" "Name of buffer w/ currently active pdb session.")

(defvar pdbx-default-pdb-buffer "*shell*<1>")

;;; --------------------------------------------------------------------
;;; public functions

(defun pdbx-set-cur-pdb-buffer ()
  "Set current buffer as the pdb buffer for pdbx commands."
  (interactive)
  (let ((buf-name (buffer-name)))
    (setq pdbx-cur-pdb-buffer buf-name)
    (message (format "Buffer %s set as pdbx's current pdb buffer." buf-name))
    ))

(defun pdbx-tbreak ()
  "Set temporary breakpoint in python file."
  (interactive)
  (pdbx-set-xbreak "tbreak"))

(defun pdbx-break ()
  "Set breakpoint in python file."
  (interactive)
  (pdbx-set-xbreak "break"))

(defun pdbx-jump ()
  "Set jump point in python file."
  (interactive)
  (pdbx-send-cmd (format "jump %d" (line-number-at-pos)) t)
  )

(defun pdbx-print ()
  "Print expression under cursor."
  (interactive)
  (pdbx-send-cmd (concat "pp " (pdbx-var-around-point))))

(defun pdbx-step ()
  (interactive)
  (pdbx-send-cmd "s" t)
  ; (pdbx-send-cmd (pdbx-display-cmd "topology"))
  )

(defun pdbx-display-cmd (var)
  (format "!exec( \"try:\\n   %s\\nexcept NameError:\\n   pass\\nelse:\\n   print; import sys; import pprint; sys.stdout.write( '%s = ' ); pprint.pprint( %s )\" )"
          var var var)
  )

(defun pdbx-next ()
  (interactive)
  (pdbx-send-cmd "n" t)
  ; (pdbx-send-cmd (pdbx-display-cmd "options"))
  )

(defun pdbx-cont ()
  (interactive)
  (pdbx-send-cmd "c" t))

(defun pdbx-up ()
  (interactive)
  (pdbx-send-cmd "up" t))

(defun pdbx-down ()
  (interactive)
  (pdbx-send-cmd "down" t))

;;; --------------------------------------------------------------------
;;; public functions

(defun pdbx-cli-trace ()
  "Decode CLI trace from logging from the last issued pdb command
through current position."
  (interactive)
  (let ((start (point-min)) 
        (end (point))
        hdr)
    (save-excursion
      (when (re-search-backward "\n" (point-min))
        (setq end (point))
        (when (re-search-backward "^(Pdb)" (point-min) t)
          (setq start (point))
          ; search for information about ast pdb command issued, e.g.:
          ;   ' > /usr/lib/python2.7/site-packages/StaticRoutingMtuTests.py(101)__init__()'
          ;   '-> self.ctx.clearArpEntries()'
          (when (re-search-backward "^> " (point-min) t)
            (setq hdr (buffer-substring (point) start)))
          )))
    (save-selected-window
      (pdbx-cli-trace-internal start end hdr))
    ))

(defun pdbx-cli-trace-region ()
  "Decode CLI trace from logging on selected region."
  (interactive)
  (pdbx-cli-trace-internal (region-beginning) (region-end)))

(defun pdbx-kill-cli-trace ()
  (interactive)
  (let ((buf (get-buffer pdbx-ctrace-buf-name))
        window)
    (when buf
      (setq window (get-buffer-window buf))
      ;;(if window (delete-window window))
      (kill-buffer buf)
      )
    ))

(defun pdbx-help()
"Help for pdbx module - Pdb Extensions for Emacs.
========================================================================

This module provides the following functionality:

  - Cli trace decoding from pexpect messages

  - New commands to set breakpoints, jump points, and print variable values 
    from inside Python files

  - Global commands to step/next/cont (no need to be inside a Pdb buffer)


Cli Trace Commands
------------------

  M-x `pdbx-cli-trace'

       Decode cli trace from pexpect messages between cursor position and last
       (Pdb) prompt, and display it on separate buffer.

       This buffer is set to Cli-Trace major mode (see details below), which
       supports font-locking and some navigation related key bindings.

  M-x `pdbx-cli-trace-region'

       Same as `pdbx-cli-trace' but decodes the contents of the entire selected
       region instead of contents betwee cursor position previous (Pdb) prompt.

       Tip: use shell mode's C-c C-p / C-c C-n key bindings to navigate through
            previous / next command to decode trace accross multiple (pdb)
            commands.

  M-x `pdbx-kill-cli-trace'

       Kill cli trace buffer.


New Commands for Python Buffers
-------------------------------

  M-x `pdbx-tbreak' - Set temporary breakpoint on current line in a Python file

  M-x `pdbx-break'  - Set breakpoint on current line in a Python file

  M-x `pdbx-print'  - Print value for variable under cursor in a Python file

  M-x `pdbx-jump'   - Set next line to be executed in a Python file.

  Notice: these commands will prompt for the name of the (Pdb) buffer (e.g.
          *shell*<1>) if it's not set yet already (see `pdbx-set-cur-pdb-buffer').


New Global Commands
-------------------

  M-x `pdbx-step'   - Send 'step' command to (Pdb) buffer

  M-x `pdbx-break'  - Send 'next' command to (Pdb) buffer

  M-x `pdbx-cont'   - Send 'cont' command to (Pdb) buffer

  M-x `pdbx-down'   - Send 'down' command to (Pdb) buffer

  M-x `pdbx-up'     - Send 'up' command to (Pdb) buffer

  Notice: pdbx-step/next commands cause the (Pdb) buffer to be selected


Misc Commands
-------------

  M-x `pdbx-set-cur-pdb-buffer'

       Mark current (Pdb) buffer as the \"pdbx's current buffer\", i.e.
       the buffer to where pdb commands are sent to.

       Useful for when you start Pdb debugging from a different buffer.


Cli-Trace Mode
--------------

The M-x `pdbx-cli-trace' and M-x `pdbx-cli-trace-region' commands decode CLI trace
received from the DUTs from pexpect messages and display them in a separate buffer.

This buffer is set in \"Cli-Trace\" mode, which is set as read-only and supports:

  - font-locking: e.g. shows prompts, commands, etc. using different font faces

  - Following key-bindings:

    SPC, n, C-c C-n    - go to next command
    p, C-c C-p         - go to previous command
    RET                - jump to original pexpect trace in the (Pdb) buffer
    q                  - kill/exit cli trace buffer

Transition of output from different DUTs (e.g. common in the case Mlag DUTs) is
identified by repeating the last output from that given dut using a special font.

Cli-Trace Mode Key Bindings:

\\{pdbx-ctrace-mode-map}
The cli-trace can be killed through the M-x `pdbx-kill-cli-trace' command.


Suggested Key Bindings
----------------------

Here are the suggested key-bindings for the pdbx commands:

- New Commands for Pdb Buffer (local key bindings for Shell mode):

  C-c l    => `pdbx-cli-trace'
  C-c r    => `pdbx-cli-trace-region'
  C-c k    => `pdbx-kill-cli-trace'
  C-c x    => `pdbx-set-cur-pdb-buffer'

- New commands for python files (local key bindings for Python mode):

  C-c p b  => `pdbx-break'
  C-c p t  => `pdbx-tbreak'
  C-c p j  => `pdbx-jump'
  C-c p p  => `pdbx-print'

- New global commands:

  [f7]             => `pdbx-step'
  [f8]             => `pdbx-next'
  [f9]             => `pdbx-cont'

  [f11], [M-PgUp]  => `pdbx-up'
  [f12], [M-PgDn]  => `pdbx-down'


Sample code for suggested key bindings above, to be added to you emacs
init file:

    (add-hook 'shell-mode-hook
              (lambda ()
                (local-set-key (kbd \"C-c l\") 'pdbx-cli-trace)
                (local-set-key (kbd \"C-c r\") 'pdbx-cli-trace-region)
                (local-set-key (kbd \"C-c k\") 'pdbx-kill-cli-trace)
                (local-set-key (kbd \"C-c x\") 'pdbx-set-cur-pdb-buffer)
                ))

    (add-hook 'python-mode-hook
              (lambda ()
                (local-set-key (kbd \"C-c p b\") 'pdbx-break)
                (local-set-key (kbd \"C-c p t\") 'pdbx-tbreak)
                (local-set-key (kbd \"C-c p j\") 'pdbx-jump)
                (local-set-key (kbd \"C-c p p\") 'pdbx-print)
                ))

    (global-set-key (quote [f7]) (quote pdbx-step))
    (global-set-key (quote [f8]) (quote pdbx-next))
    (global-set-key (quote [f9]) (quote pdbx-cont))
    (global-set-key (quote [f11]) (quote pdbx-up))
    (global-set-key (quote [f12]) (quote pdbx-down))
    ; following keys (M-PgUp/PgDn) only work on native/VNC modes
    (global-set-key (quote [M-next]) (quote pdbx-down))
    (global-set-key (quote [M-prior]) (quote pdbx-up))
"
  (interactive)
  (describe-function 'pdbx-help)
  )

;;; --------------------------------------------------------------------
;;; private functions

(defun pdbx-send-cmd (cmd &optional select-pdb-buf)
  "Send command to pdb buffer.

Prompts user for current buffer if it's not currently set (or was deleted).

If optional argument 'select-pdb-buf' is non-nil, select pdb buffer's
current window before sending the command (useful for commands like step, 
next, etc.)." 
  (let (window)
    (when (or (string= pdbx-cur-pdb-buffer "")
              (not (get-buffer pdbx-cur-pdb-buffer)))
      (setq pdbx-cur-pdb-buffer
            (read-string (format "Buffer w/ pdb session (%s): " pdbx-default-pdb-buffer)
                         nil nil pdbx-default-pdb-buffer)))
    (if select-pdb-buf
        (progn
          (setq window (get-buffer-window pdbx-cur-pdb-buffer))
          (if window (select-window window))
          (set-buffer pdbx-cur-pdb-buffer)
          (insert cmd)
          (comint-send-input)
          )
      (save-selected-window
        (set-buffer pdbx-cur-pdb-buffer)
        (goto-char (point-max))
        (insert cmd)
        (comint-send-input))
      )
    (message (format "Command: '%s' sent to buffer %s" cmd pdbx-cur-pdb-buffer))
    ))

(defun pdbx-get-cur-module ()
  (let ((file-name (buffer-file-name)))
    (if (not file-name)
        (error "Must be on a Python file to use this command."))
    (if (string-match "site-packages/\\(.*$\\)" file-name)
        (match-string-no-properties 1 file-name)
      (file-name-nondirectory file-name))
    ))


(defun pdbx-set-xbreak (brk-type)
  (pdbx-send-cmd 
   (format "%s %s:%d" brk-type (pdbx-get-cur-module) (line-number-at-pos))
   t
   ))

;(defvar
(setq pdbx-expr-delim-re "\\([*()={}:\"',\n\t\v ]\\|\\[\\|\\]\\)")

(defun pdbx-region-for-var-around-point ()
  "Return region for the expression around point."
  (let (start end)
    (save-excursion
      (if (not (eobp))
          (forward-char 1))
      (forward-word -1)
      (forward-word 1)
      (if (re-search-backward pdbx-expr-delim-re (point-min) t)
          (progn 
            (forward-char)
            (setq start (point)))
        (setq start (point-min))
        (goto-char start)
        )
      (if (re-search-forward pdbx-expr-delim-re (point-max) t)
          (progn
            (backward-char)
            (setq end (point)))
        (setq end (point-max)))
      (list start end)
      )
    ))

(defun pdbx-var-around-point ()
  "Return the expression around the point as a string."
  (let ((reg (pdbx-region-for-var-around-point)))
    (buffer-substring (car reg) (nth 1 reg))
    ))

;; cli-trace related functions

(defun pdbx-cli-trace-internal (start end &optional hdr)
  (interactive)
  (let ((buf-name pdbx-ctrace-buf-name)
        pkg time-stamp pid dut text
        text-elm text-list cur-dut
        dut-last-text-hash last-text
        text-start text-end trans-str
        )
    (setq pdbx-ctrace-orig-buffer (buffer-name))
    ; capture lines of interest
    (save-excursion
      (goto-char start)
      (while (re-search-forward
              "^\\(\\[[a-zA-Z0-9/-]*] \\)?\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9:]*.[0-9]*\\)\s*\\([0-9]*\\)\\s-+pexpect\\s-+[0-9]+\\s-+\\([^:]:\\)?\\(.+\\)#[^ ]* received: ['\"]\\(.*\\)['\"]$"
              end t)
        (setq pkg (match-string 1))
        (setq time-stamp (match-string 2))
        (setq pid (match-string 3))
        (setq dut (match-string 5))
        ; replace \n in text for newline character
        (setq text (replace-regexp-in-string "\\\\n" "\n" (match-string 6)))
        ;(setq text (match-string 6))
        ;(while (string-match "\\\\n" text)
        ;   (setq text (replace-match "\n" t t text)))
        (setq text-elm (list text dut (line-beginning-position)))
        (push text-elm text-list)
        ))
    ; get/create new buffer
    (setq buf (get-buffer-create buf-name))
    (set-buffer buf)
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max))

    ; insert text there
    (goto-char (point-min))
    (if hdr (insert hdr))
    (setq text-list (reverse text-list))
    (setq cur-dut "")
    (setq dut-last-text-alist nil)
    (setq dut-last-text-hash (make-hash-table :test 'equal))
    (while text-list
      (setq text-elm (car text-list))
      (setq text (car text-elm))
      (setq dut (nth 1 text-elm))
      (setq orig-pos (nth 2 text-elm))
      (setq text-start (point))
      ; if output is from a different dut, insert last text (w/ special font)
      (when (not (string= dut cur-dut))
        (setq cur-dut dut)
;        (setq last-text (gethash dut dut-last-text-hash (format "(%s)" dut)))
        (setq last-text (gethash dut dut-last-text-hash (format "" dut)))
        ; if last-text starts /w the dut name, no need to insert transition string
        (if (string-match (concat "^" dut) last-text)
            (setq trans-str "")
          (setq trans-str (format "(>%s)" dut)))
        ; insert transition string (if needed) and last text for the new dut
        (insert (format "\n%s%s..." trans-str last-text))
        (setq text-end (point))
        ; mark it w/ special font
        (pdbx-ctrace-set-face-property text-start text-end pdbx-ctrace-de-emphasize-face)
        )
      ; insert text and store its original position as a property
      (insert text)
      (pdbx-ctrace-store-orig-pos text-start (point) orig-pos)

      ; store last text for this dut
      (puthash dut text dut-last-text-hash)
      ; go to next element in the list
      (setq text-list (cdr text-list))
      )

    (pdbx-ctrace-mode)
    (pop-to-buffer buf)
    (goto-char (point-max))
    ))

;;; ====================================================================
;;; Definitions from this point on are for cli trace mode

(defgroup pdbx-ctrace-mode ()
  "Major mode for viewing CLI traces."
  :version "21.1"
  :group 'tools
  :group 'log)

;;; --------------------------------------------------------------------
;;; customizible vars

(defcustom pdbx-ctrace-foo nil
  "If non-nil, do something."
  :type 'boolean
  :group 'pdbx-ctrace-mode)

;;; --------------------------------------------------------------------
;;; global vars

(defvar pdbx-ctrace-orig-buffer nil)
(defvar pdbx-ctrace-buf-name "*cli-trace*")

;;; --------------------------------------------------------------------
;;; keymap definitions

(easy-mmode-defmap pdbx-ctrace-mode-map
  `(("n" . pdbx-ctrace-next-cmd)
    (" " . pdbx-ctrace-next-cmd)
    ("p" . pdbx-ctrace-prev-cmd)
    ("q" . pdbx-ctrace-quit)
    ("\C-c\C-p" . pdbx-ctrace-prev-cmd)
    ("\C-c\C-n" . pdbx-ctrace-next-cmd)
    ("\r" . pdbx-ctrace-return-key)
    )
  "Keymap for `pdbx-ctrace-mode'.")


;;; --------------------------------------------------------------------
;;; font-lock / imenu definitions

(defface pdbx-ctrace-de-emphasize
  '((((class color) (min-colors 88) (background light))
     :foreground "dark grey")
    (((class color) (min-colors 88) (background dark))
     :foreground "dark grey")
    (((class color) (background light))
     :foreground "white")
    (((class color) (background dark))
     :foreground "white")
    (t :weight bold))
  "`pdbx-ctrace-mode' face for things to be de-emphasized."
  :group 'pdbx-ctrace-mode)
;;(define-obsolete-face-alias 'pdbx-ctrace-de-emphasize-face 'pdbx-ctrace-de-emphasize "22.1")
(defvar pdbx-ctrace-de-emphasize-face 'pdbx-ctrace-de-emphasize)

; copied from comint-highlight-prompt
(defface pdbx-ctrace-highlight-prompt
  '((((min-colors 88) (background dark)) (:foreground "cyan1"))
    (((background dark)) (:foreground "cyan"))
    (t (:foreground "dark blue")))
  "Face to use to highlight prompts."
  :group 'pdbx-ctrace-mode)
(defvar pdbx-ctrace-highlight-prompt-face 'pdbx-ctrace-highlight-prompt)

(defvar pdbx-ctrace-bold-face 'bold)

;(defvar 
(setq pdbx-ctrace-font-lock-keywords
  `(("^\\([^ \n]+[#>]\\)\\(.*\\)$" (1 pdbx-ctrace-highlight-prompt-face) (2 pdbx-ctrace-bold-face))
    ))

(defconst pdbx-ctrace-font-lock-defaults
  '(pdbx-ctrace-font-lock-keywords t nil nil nil (font-lock-multiline . nil)))


(defvar pdbx-ctrace-imenu-generic-expression
  '((nil "^\\([^ ]+#\\)" 1)
    ))


;;; --------------------------------------------------------------------
;;; pdbx-ctrace-mode fucntions

(defun pdbx-ctrace-next-cmd ()
  (interactive)
  (a4-nav-imenu-next))

(defun pdbx-ctrace-prev-cmd ()
  (interactive)
  (a4-nav-imenu-prev))

(defun pdbx-ctrace-quit ()
  (interactive)
  (let (window)
    (when (get-buffer pdbx-ctrace-orig-buffer)
      (setq window (get-buffer-window pdbx-ctrace-orig-buffer))
      (if window
          (select-window window))
      )
    (pdbx-kill-cli-trace)
    ))

(defun pdbx-ctrace-return-key (pnt)
  (interactive "d")
  (let ((orig-pos (get-char-property pnt 'orig-pos)))
    (when orig-pos
      (pop-to-buffer pdbx-ctrace-orig-buffer)
      (goto-char orig-pos)
      (re-search-forward "received: ['\"]" (line-end-position) t)
      )
    ))


;;; --------------------------------------------------------------------
;;; property related functions (adapted from p4.el)

;; Find out what type of emacs we are running in. We will be using this
;; quite a few times in this program.
(eval-and-compile
  (defvar pdbx-ctrace-running-emacs nil
    "If the current Emacs is not XEmacs, then, this is non-nil.")
  (defvar pdbx-ctrace-running-xemacs nil
    "If the current Emacs is XEmacs/Lucid, then, this is non-nil.")
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      (setq pdbx-ctrace-running-xemacs t)
    (setq pdbx-ctrace-running-emacs t)))

(defun pdbx-ctrace-set-extent-properties (start end prop-list)
  (cond (pdbx-ctrace-running-xemacs
	 (let ((ext (make-extent start end)))
	   (while prop-list
	     (set-extent-property ext (caar prop-list) (cdar prop-list))
	     (setq prop-list (cdr prop-list)))))
	(pdbx-ctrace-running-emacs
	 (let ((ext (make-overlay start end)))
	   (while prop-list
	     (overlay-put ext (caar prop-list) (cdar prop-list))
	     (setq prop-list (cdr prop-list)))))))

(defun pdbx-ctrace-set-face-property (start end face-property)
  (pdbx-ctrace-set-extent-properties start end
                                (list (cons 'face face-property))))

(defun pdbx-ctrace-store-orig-pos (start end orig-pos)
  (let (prop-list)
    (setq prop-list (list (cons 'orig-pos orig-pos)))
    (pdbx-ctrace-set-extent-properties start end prop-list)
    ))

;;; --------------------------------------------------------------------
;;; mode definition

;;;###autoload
(define-derived-mode pdbx-ctrace-mode fundamental-mode "Cli-Trace"
  "Major mode for viewing CLI traces. 

Local Key Bindings
------------------

\\{pdbx-ctrace-mode-map}"

  (let ()
    ; before anything else, define / initialize new buffer-local variables
    (set (make-local-variable 'font-lock-defaults) pdbx-ctrace-font-lock-defaults)
    (set (make-local-variable 'imenu-generic-expression) 
         pdbx-ctrace-imenu-generic-expression)

    ; set buffer in read-only mode
    (setq buffer-read-only t)

    ; set local keys for stuff we don't know how to represent yet in
    ; easy-mmode-defmap
    (local-set-key (quote [C-up]) (quote pdbx-ctrace-prev-cmd))
    (local-set-key (quote [C-down]) (quote pdbx-ctrace-next-cmd))
    (local-set-key (quote [return]) (quote pdbx-ctrace-return-key))
    ))


(defun x ()
  (interactive)
  ;(local-set-key (kbd "C-c p") 'cli-trace-pdb)
  ;(local-set-key (kbd "C-c r") 'cli-trace-region)
  ;(local-set-key (kbd "C-c k") 'cli-trace-kill-buffer)
  (define-key pdbx-ctrace-mode-map (kbd " ") (quote pdbx-ctrace-next-cmd))
  (local-set-key (quote [return]) (quote pdbx-ctrace-return-key))
  )


;;; --------------------------------------------------------------------
;;; provide the package

(provide 'pdbx)

;;; --- EOF ---
