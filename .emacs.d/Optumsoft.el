(load-library "Arastra.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Optumsoft specific debugging commands

(defun find-gdb-buffer (buffers)
  "find first gdb buf"
  (when buffers 
  (let (
        (buf-name (buffer-name (car buffers)))
       )
       (if (string-match "^\*gdb" buf-name)
           buf-name 
           (find-gdb-buffer (cdr buffers))
       ) 
   )
   )
)

(defun my-goto-gdb()
  "Switch to gdb buffer"
  (interactive)
  (switch-to-buffer (find-gdb-buffer (buffer-list))
  )
)

(defun my-flip-truncate-line ()
  (interactive)
  (setq truncate-lines 
    (if (null truncate-lines) '1 'nil)
  )
)

(defun my-grep ()
  (interactive)
  (setq grep-command (concat "grep -n "
                             (read-from-minibuffer "grep target:" " *.c *.cpp *.cc *.h *.tac *.tin")
                             ))
  (grep grep-command)
)

(global-set-key 'f9 'gdb-next)
(global-set-key '(shift f9) 'gdb-step)
(global-set-key '(control f9) 'my-goto-gdb)
(global-set-key 'f10 'compile)
(global-set-key '(control f10) 'next-error) ; or next grep
(global-set-key '(meta f10) 'kill-compilation)
(global-set-key '(f11) 'my-flip-truncate-line)
(global-set-key '(f12) 'my-grep) ; captured by VNC sometimes
(global-set-key '(shift f12) 'my-grep)

(defun gdb-next (temp)
  "Do GDB next"
  (interactive "P")
  (set-buffer current-gdb-buffer)
  (goto-char (process-mark (get-buffer-process current-gdb-buffer)))
  (delete-region (point) (point-max))
  (insert "next")
  (comint-send-input)
)
(defun gdb-step (temp)
  "Do GDB step"
  (interactive "P")
  (set-buffer current-gdb-buffer)
  (goto-char (process-mark (get-buffer-process current-gdb-buffer)))
  (delete-region (point) (point-max))
  (insert "step")
  (comint-send-input)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo Arastra specific copyright notices
(remove-hook 'c-mode-common-hook 'arastra-c-mode-common-hook)
(remove-hook 'perl-mode-hook 'arastra-perl-mode-hook)
(remove-hook 'cperl-mode-hook 'arastra-cperl-mode-hook)
(remove-hook 'python-mode-hook 'arastra-indent-python-mode-hook)

;; The default cc-mode style needs to be modified slightly to match Optumsoft 
;; conventions:

(c-add-style "Optumsoft" '("user" 
                         (c-basic-offset . 3)
                         (c-offsets-alist
                          (innamespace . 0)
                          (access-label . -2)
                          (arglist-intro . 6)
                          (case-label . 1)
                          (statement-case-intro . 2)
                          (statement-case-open . 2)
                          (member-init-intro . 6)
                          )) t)

(defun optumsoft-c-mode-common-hook ()
  (c-set-style "Optumsoft")
  ;; Automatically insert copyright notice.
  (if (< (buffer-size) 10) 
      (let ((mod (buffer-modified-p)))
        (save-excursion
          (goto-char (point-min))
          (insert (format "// Copyright (c) %s OptumSoft, Inc.  All rights reserved.\n"
                          (nth 5 (decode-time (current-time)))))
          (insert "// OptumSoft, Inc. Confidential and Proprietary.\n\n")
          (if (string-match "\\([a-zA-Z0-9_]+\\)[/\\\\]\\([a-zA-Z0-9_]+\\)\\.h$" (buffer-file-name))
              (let* ((dir-name (substring (buffer-file-name) (match-beginning 1) (match-end 1)))
                     (file-name (substring (buffer-file-name) (match-beginning 2) (match-end 2)))
                     (symbol (upcase (format "%s_%s_H" dir-name file-name))))
                (insert (format "#ifndef %s\n#define %s\n\n\n#endif // %s\n" symbol symbol symbol)))))
        (set-buffer-modified-p mod))))

(add-hook 'c-mode-common-hook 'optumsoft-c-mode-common-hook)

(custom-set-variables
 '(indent-tabs-mode nil))


(defun optumsoft-perl-mode-hook ()
  (setq perl-indent-level 3)
  (setq perl-continued-statement-offset 3)
  (optumsoft-perl-add-copyright))

(defun optumsoft-perl-add-copyright ()
  (if (< (buffer-size) 10)
      (let ((mod (buffer-modified-p)))
        (save-excursion
          (goto-char (point-min))
          (insert "#!/usr/bin/perl -w\n#\n")
          (insert (format "# Copyright (c) %s OptumSoft, Inc.  All rights reserved.\n"
                          (nth 5 (decode-time (current-time)))))
          (insert "# OptumSoft, Inc. Confidential and Proprietary.\n\n")
          (insert "use strict;\n\n"))
        (set-buffer-modified-p mod))))

(defun optumsoft-cperl-mode-hook ()
  (setq cperl-indent-level 3)
  (setq cperl-continued-statement-offset 3)
  (optumsoft-perl-add-copyright))

(add-hook 'perl-mode-hook 'optumsoft-perl-mode-hook)
(add-hook 'cperl-mode-hook 'optumsoft-cperl-mode-hook)

                  
(defun optumsoft-indent-python-mode-hook ()
  (setq py-indent-offset 3)
  (optumsoft-python-add-copyright))

(defun optumsoft-python-add-copyright ()
  (if (< (buffer-size) 10)
      (let ((mod (buffer-modified-p)))
        (save-excursion
          (goto-char (point-min))
          (insert "#!/usr/bin/env python\n")
          (insert (format "# Copyright (c) %s OptumSoft, Inc.  All rights reserved.\n"
                          (nth 5 (decode-time (current-time)))))
          (insert "# OptumSoft, Inc. Confidential and Proprietary.\n\n"))
        (set-buffer-modified-p mod))))

(add-hook 'python-mode-hook 'optumsoft-indent-python-mode-hook)

;;; Tac indenter stuff
(defun tac-mode ()
 (c++-mode)
 ; fix up "default" indent for Tacc mode, preserve other 
 ; "Optumsoft" style overrides
 (c-add-style "OptumsoftTac" '("user" 
                            (c-basic-offset . 3)
                            (c-offsets-alist
                             (innamespace . 0)
                             (arglist-intro . ++)
                             (case-label . +)
                             (member-init-intro . ++)
                             )) t)
)
(setq auto-mode-alist
      (append '(("\\.tac$" . tac-mode)) auto-mode-alist))

(font-lock-mode)
