;; cp-utils.el --- Misc helper command to copy things (word under cursor,
;;                 current buffer name, which-function, etc.) into kill-ring.

;; Copyright (c) 2012 Arista Networks, Inc.  All rights reserved.
;; Arista Networks, Inc. Confidential and Proprietary.

;; Author: Paulo Gallo <pgallo@aristanetworks.com>

;; Todo / Wish List:

;;; Code:

(defvar bug-mode-version "1.0.2" "The Current Bug-Mode Revision.")

;; ---------------------------------------------------------------------
;; helper functions

(defun cp-get-region-for-word-around-point ()
  "Return region for the word around point."
  (save-excursion
    (if (not (eobp))
	(forward-char 1))
    (forward-word -1)
    (forward-word 1)
    (forward-sexp -1)
    (list (point) (progn (forward-sexp 1) (point)))
    ))

(defun cp-string (str)
  "Copy a string into the kill ring."
  (let (sav-read-only sav-buffer-modified-p start)
    (save-excursion
      (setq sav-read-only buffer-read-only)
      (setq sav-buffer-modified-p (buffer-modified-p))
      (setq buffer-read-only nil)
      (setq start (point))
      (insert str)
      (kill-region start (point))
      (set-buffer-modified-p sav-buffer-modified-p)
      (setq buffer-read-only sav-read-only)
      (message (format "Copied into kill ring: %s" str))
      )
    ))

(defun cp-get-which-func ()
  "Get current which-function (i.e. name of function cursor is in)"
  (let (wf save-cleanup-func)
    ;; disable which-func-cleanup-function so that we get the full string
    (setq save-cleanup-func which-func-cleanup-function)
    (setq which-func-cleanup-function nil)
    (setq wf (which-function))
    (setq which-func-cleanup-function save-cleanup-func)
    (when wf
      (when (and (listp wf) (car wf))
        (setq wf (car wf)))
      (when (stringp wf)
        (setq wf (concat wf "()")))
      )
    wf
  ))

(defun cp-get-cur-file-name (&optional dont-trim-src)
  "Get file name of current buffer (if buffer has a file in it).

If DONT-TRIM-SRC flag is nil and file under a src/... path, returns only
the portion after 'src/' (e.g. ~/myws/src/Pkg/FooBar.tin => Pkg/FooBar.tin)."
  (let ((file-name buffer-file-name))
    (when file-name
      ;; ignore everything before ../src
      (when (and (not dont-trim-src)
                 (string-match "/src/\\(.*\\)$" file-name))
        (setq file-name (match-string-no-properties 1 file-name)))
      )
    file-name
  ))

;; ---------------------------------------------------------------------
;; helper functions

(defun cp-cur-word ()
  "Copy word under cursor into kill-ring"
  (interactive)
  (let (str (reg (cp-get-region-for-word-around-point)))
    (copy-region-as-kill (car reg) (nth 1 reg))
    (setq str (buffer-substring (car reg) (nth 1 reg)))
    (message (format "Copied into kill ring: %s" str))
    ))

(defun cp-cur-file-name (prefix-arg)
  "Copy file name of current buffer into kill-ring (if it's a file buffer)

By default, trim all path up to '.../src/', if present. Copy full path
if invoked w/ a prefix argument (e.g. C-u ...)" 
  (interactive "P")
  (let ((file-name (cp-get-cur-file-name prefix-arg)))
    (if file-name (cp-string file-name))
    ))

(defun cp-which-func ()
  "Copy which-function (i.e. name of function cursor is in) into kill-ring"
  (interactive)
  (let ((wf (cp-get-which-func)))
    (if wf (cp-string wf))
    ))

(defun cp-show-full-which-func ()
  "Show which-function (i.e. name of function cursor is in)"
  (interactive)
  (let ((wf (cp-get-which-func)))
    (if wf (message wf))
    ))

(defun cp-which-func-and-cur-file-name ()
  "Copy which-function + current file name into kill-ring"
  (interactive)
  (let ((wf (cp-get-which-func))
        (file-name (cp-get-cur-file-name))
        str)
    (when (and wf file-name)
      (cp-string (format "%s, %s" wf file-name)))
    ))

(defun cp-help ()
  "Help for Miscelaneous copy utility functions (cp-util.el)

This module provides commands to automatically copy word under cursor,
current buffer name, which-function, etc. into the kill-ring, helping
speed up copying and pasting.

Provided Functions:
-------------------

  M-x `cp-cur-word'        - copy word under cursor

  M-x `cp-cur-file-name'   - copy buffer file name (if it's a file buffer);
                             By default, if file under a '.../src/...' path,
                             copy only the portion after '/src/' (e.g.
                             ~/myws/src/Pkg/FooBar.tin => Pkg/FooBar.tin).

                             If invoked w/ a prefix argument (e.g. C-u ...),
                             do not trim the '.../src/' portion.

  M-x `cp-which-func'      - copy which-function (name of function cursor
                             in current in). Only works when which-function
                             is enabled. 

                             See http://www.emacswiki.org/emacs/WhichFuncMode

  M-x `cp-which-func-and-cur-file-name'

                             - copy both which function and file name in a
                               single shot. Just like `cp-cur-file', path up
                               to '.../src/' is stripped by default. Use
                               prefix argument (C-u ...) to get full path.

Suggested Key Bindings:
-----------------------

- If you don't mind losing C-p as the previous-line command:

    C-p w  => `cp-cur-word'
    C-p f  => `cp-cur-file-name'
    C-p h  => `cp-which-func'
    C-p b  => `cp-which-func-and-cur-file-name'

  Code for your emacs init file:

    (defvar cp-map)
    (define-prefix-command 'cp-map)
    (define-key (current-global-map) [(control ?p)] 'cp-map)
    (global-set-key (kbd \"C-p w\") 'cp-cur-word)
    (global-set-key (kbd \"C-p f\") 'cp-cur-file-name)
    (global-set-key (kbd \"C-p h\") 'cp-which-func)
    (global-set-key (kbd \"C-p b\") 'cp-which-func-and-cur-file-name)

- If do you want C-p as the previous-line command:

    C-x c w  => `cp-cur-word'
    C-x c f  => `cp-cur-file-name'
    C-x c h  => `cp-which-func'
    C-x c b  => `cp-which-func-and-cur-file-name'

  Code for your emacs init file:

    (global-set-key (kbd \"C-x c w\") 'cp-cur-word)
    (global-set-key (kbd \"C-x c f\") 'cp-cur-file-name)
    (global-set-key (kbd \"C-x c h\") 'cp-which-func)
    (global-set-key (kbd \"C-x c b\") 'cp-which-func-and-cur-file-name)

- If you run emacs natively on your machine or through VNC:

    C-p     => `cp-cur-word'
    C-S-p   => `cp-cur-file-name'
    C-S-o   => `cp-which-func'
    C-S-M-p => `cp-which-func-and-cur-file-name'

  Code for your emacs init file:

    (global-set-key (kbd \"C-p\") 'cp-cur-word)
    (global-set-key (kbd \"C-S-p\") 'cp-cur-file-name)
    (global-set-key (kbd \"C-S-M-p\") 'cp-which-func-and-cur-file-name)
    (global-set-key (kbd \"C-S-o\") 'cp-which-func)
"
  (interactive)
  (describe-function 'cp-help))

(provide 'cp-utils)

;; --- EOF ---
;; cp-utils.el --- Misc helper command to copy things (word under cursor,
;;                 current buffer name, which-function, etc.) into kill-ring.

;; Copyright (c) 2012 Arista Networks, Inc.  All rights reserved.
;; Arista Networks, Inc. Confidential and Proprietary.

;; Author: Paulo Gallo <pgallo@aristanetworks.com>

;; Todo / Wish List:

;;; Code:

(defvar bug-mode-version "1.0.2" "The Current Bug-Mode Revision.")

;; ---------------------------------------------------------------------
;; helper functions

(defun cp-get-region-for-word-around-point ()
  "Return region for the word around point."
  (save-excursion
    (if (not (eobp))
	(forward-char 1))
    (forward-word -1)
    (forward-word 1)
    (forward-sexp -1)
    (list (point) (progn (forward-sexp 1) (point)))
    ))

(defun cp-string (str)
  "Copy a string into the kill ring."
  (let (sav-read-only sav-buffer-modified-p start)
    (save-excursion
      (setq sav-read-only buffer-read-only)
      (setq sav-buffer-modified-p (buffer-modified-p))
      (setq buffer-read-only nil)
      (setq start (point))
      (insert str)
      (kill-region start (point))
      (set-buffer-modified-p sav-buffer-modified-p)
      (setq buffer-read-only sav-read-only)
      (message (format "Copied into kill ring: %s" str))
      )
    ))

(defun cp-get-which-func ()
  "Get current which-function (i.e. name of function cursor is in)"
  (let (wf save-cleanup-func)
    ;; disable which-func-cleanup-function so that we get the full string
    (setq save-cleanup-func which-func-cleanup-function)
    (setq which-func-cleanup-function nil)
    (setq wf (which-function))
    (setq which-func-cleanup-function save-cleanup-func)
    (when wf
      (when (and (listp wf) (car wf))
        (setq wf (car wf)))
      (when (stringp wf)
        (setq wf (concat wf "()")))
      )
    wf
  ))

(defun cp-get-cur-file-name (&optional dont-trim-src)
  "Get file name of current buffer (if buffer has a file in it).

If DONT-TRIM-SRC flag is nil and file under a src/... path, returns only
the portion after 'src/' (e.g. ~/myws/src/Pkg/FooBar.tin => Pkg/FooBar.tin)."
  (let ((file-name buffer-file-name))
    (when file-name
      ;; ignore everything before ../src
      (when (and (not dont-trim-src)
                 (string-match "/src/\\(.*\\)$" file-name))
        (setq file-name (match-string-no-properties 1 file-name)))
      )
    file-name
  ))

;; ---------------------------------------------------------------------
;; helper functions

(defun cp-cur-word ()
  "Copy word under cursor into kill-ring"
  (interactive)
  (let (str (reg (cp-get-region-for-word-around-point)))
    (copy-region-as-kill (car reg) (nth 1 reg))
    (setq str (buffer-substring (car reg) (nth 1 reg)))
    (message (format "Copied into kill ring: %s" str))
    ))

(defun cp-cur-file-name (prefix-arg)
  "Copy file name of current buffer into kill-ring (if it's a file buffer)

By default, trim all path up to '.../src/', if present. Copy full path
if invoked w/ a prefix argument (e.g. C-u ...)" 
  (interactive "P")
  (let ((file-name (cp-get-cur-file-name prefix-arg)))
    (if file-name (cp-string file-name))
    ))

(defun cp-which-func ()
  "Copy which-function (i.e. name of function cursor is in) into kill-ring"
  (interactive)
  (let ((wf (cp-get-which-func)))
    (if wf (cp-string wf))
    ))

(defun cp-show-full-which-func ()
  "Show which-function (i.e. name of function cursor is in)"
  (interactive)
  (let ((wf (cp-get-which-func)))
    (if wf (message wf))
    ))

(defun cp-which-func-and-cur-file-name ()
  "Copy which-function + current file name into kill-ring"
  (interactive)
  (let ((wf (cp-get-which-func))
        (file-name (cp-get-cur-file-name))
        str)
    (when (and wf file-name)
      (cp-string (format "%s, %s" wf file-name)))
    ))

(defun cp-help ()
  "Help for Miscelaneous copy utility functions (cp-util.el)

This module provides commands to automatically copy word under cursor,
current buffer name, which-function, etc. into the kill-ring, helping
speed up copying and pasting.

Provided Functions:
-------------------

  M-x `cp-cur-word'        - copy word under cursor

  M-x `cp-cur-file-name'   - copy buffer file name (if it's a file buffer);
                             By default, if file under a '.../src/...' path,
                             copy only the portion after '/src/' (e.g.
                             ~/myws/src/Pkg/FooBar.tin => Pkg/FooBar.tin).

                             If invoked w/ a prefix argument (e.g. C-u ...),
                             do not trim the '.../src/' portion.

  M-x `cp-which-func'      - copy which-function (name of function cursor
                             in current in). Only works when which-function
                             is enabled. 

                             See http://www.emacswiki.org/emacs/WhichFuncMode

  M-x `cp-which-func-and-cur-file-name'

                             - copy both which function and file name in a
                               single shot. Just like `cp-cur-file', path up
                               to '.../src/' is stripped by default. Use
                               prefix argument (C-u ...) to get full path.

Suggested Key Bindings:
-----------------------

- If you don't mind losing C-p as the previous-line command:

    C-p w  => `cp-cur-word'
    C-p f  => `cp-cur-file-name'
    C-p h  => `cp-which-func'
    C-p b  => `cp-which-func-and-cur-file-name'

  Code for your emacs init file:

    (defvar cp-map)
    (define-prefix-command 'cp-map)
    (define-key (current-global-map) [(control ?p)] 'cp-map)
    (global-set-key (kbd \"C-p w\") 'cp-cur-word)
    (global-set-key (kbd \"C-p f\") 'cp-cur-file-name)
    (global-set-key (kbd \"C-p h\") 'cp-which-func)
    (global-set-key (kbd \"C-p b\") 'cp-which-func-and-cur-file-name)

- If do you want C-p as the previous-line command:

    C-x c w  => `cp-cur-word'
    C-x c f  => `cp-cur-file-name'
    C-x c h  => `cp-which-func'
    C-x c b  => `cp-which-func-and-cur-file-name'

  Code for your emacs init file:

    (global-set-key (kbd \"C-x c w\") 'cp-cur-word)
    (global-set-key (kbd \"C-x c f\") 'cp-cur-file-name)
    (global-set-key (kbd \"C-x c h\") 'cp-which-func)
    (global-set-key (kbd \"C-x c b\") 'cp-which-func-and-cur-file-name)

- If you run emacs natively on your machine or through VNC:

    C-p     => `cp-cur-word'
    C-S-p   => `cp-cur-file-name'
    C-S-o   => `cp-which-func'
    C-S-M-p => `cp-which-func-and-cur-file-name'

  Code for your emacs init file:

    (global-set-key (kbd \"C-p\") 'cp-cur-word)
    (global-set-key (kbd \"C-S-p\") 'cp-cur-file-name)
    (global-set-key (kbd \"C-S-M-p\") 'cp-which-func-and-cur-file-name)
    (global-set-key (kbd \"C-S-o\") 'cp-which-func)
"
  (interactive)
  (describe-function 'cp-help))

(provide 'cp-utils)

;; --- EOF ---
