
;;; add replace-regexp-in-string for Xemacs.  The behavior is
;;; identical in the cases that we're using it.
(unless (fboundp 'replace-regexp-in-string)
  (defun replace-regexp-in-string (regexp newtext string)
    (replace-in-string string regexp newtext)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Perforce-emacs integration
;;; 

(defun a4-run (argv &optional output-filter)
  (interactive 
   (list 
    (split-string (read-from-minibuffer "Run like: " (cons (concat "a4  " (buffer-file-name)) 4))
                  " ")))
  (with-temp-buffer
    (let* ((result (apply 'call-process (append (list (car argv) nil t nil) (cdr argv))))
           (re "\n$")
           (str (buffer-substring (point-min) (point-max)))
           (ofilt (or output-filter (lambda (x) x)))
           (output (apply ofilt (list (replace-regexp-in-string re "" str)))))
      (if (eq result 0)
          (progn
            (message "%s" output)
            t)
        (message "a4 error: %s" output)
        nil))))

(defun a4-nuke-p4-crud (msg) 
  (replace-regexp-in-string "^\\+ p4 .*\n" "" msg))

(defun a4-edit (file)
  (interactive (list (buffer-file-name)))
  (let ((default-directory (file-name-directory file)))
    (if (a4-run (list "a4" "edit" (file-name-nondirectory file)) 'a4-nuke-p4-crud)
        (setq buffer-read-only nil))))

(defun a4-revert (file)
  (interactive (list (buffer-file-name)))
  (let ((default-directory (file-name-directory file)))
    (if (a4-run (list "a4" "revert" (file-name-nondirectory file)) 'a4-nuke-p4-crud)
        (revert-buffer))))

(defun a4-add (file)
  (interactive (list (buffer-file-name)))
  (let ((default-directory (file-name-directory file)))
    (a4-run (list "a4" "add" (file-name-nondirectory file)) 'a4-nuke-p4-crud)))

(defun a4-submit ()
  (interactive)
  (a4-run (list "a4" "submit")) 'a4-nuke-p4-crud)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; a4-diff / diff-mode related code

(defvar a4-diff-history '("a4 diff"))

(defun a4-diff ()
  (interactive)
  (let* (buf output cmd
        (buf-name "*a4-diff*")    
        (same-buf-p (string= buf-name (buffer-name)))
        )
    (setq cmd (read-string "Command for a4 diff: " 
                           (car a4-diff-history) '(a4-diff-history . 1) "a4 diff"))

    (when (not same-buf-p)
      (get-buffer-create buf-name) ;; We do these two lines
      (kill-buffer buf-name))      ;; to ensure no duplicates
    (setq buf (get-buffer-create buf-name))
    (set-buffer buf)
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max))

    (goto-char (point-min))
    (setq output (shell-command-to-string cmd))
    (insert output)

    (goto-char (point-min))
    ; if not same buffer, open new one as other window (e.g. on the side)
    (if (not same-buf-p)
        (pop-to-buffer buf))
    ; cd to "/" so that cur dir is in sync w/ files shown
    (cd "/")
    (diff-mode)
    (setq buffer-read-only t)
    ))

;; diff-mode re-maps some of Arista default bindings (e.g. M-o and M-k).
;;
;; Code below deals w/ automatically mapping these keys back to Arista's
;; defaults, while making the original diff-mode functions still available
;; through the "C-c" prefix (e.g. "C-c M-k", etc.)
;;
;; Pls see more details in the description of a4-diff-mode-toggle-arista-mappings
;; function below.

(defvar a4-diff-mode-restore-arista-mappings-p t)
(defvar a4-diff-mode-restore-back-fwd-paragraph-p nil)

(defun a4-diff-mode-restore-arista-mappings ()
  (interactive)
  (when (eq major-mode 'diff-mode)
    (local-unset-key (kbd "M-k"))
    (local-unset-key (kbd "M-o"))
    (local-set-key (kbd "C-c M-k") 'diff-hunk-kill)
    (local-set-key (kbd "C-c M-o") 'diff-goto-source)
    (when a4-diff-mode-restore-back-fwd-paragraph-p
      (local-unset-key (kbd "M-p"))
      (local-unset-key (kbd "M-n"))
      (local-set-key (kbd "C-c M-p") 'diff-hunk-prev)
      (local-set-key (kbd "C-c M-n") 'diff-hunk-next)
      )
    ))

(defun a4-diff-mode-set-diff-mappings ()
  (interactive)
  (when (eq major-mode 'diff-mode)
    (local-set-key (kbd "M-k") 'diff-hunk-kill)
    (local-set-key (kbd "M-o") 'diff-goto-source)
    (local-unset-key (kbd "C-c M-k"))
    (local-unset-key (kbd "C-c M-o"))
    (when a4-diff-mode-restore-back-fwd-paragraph-p
      (local-set-key (kbd "M-p") 'diff-hunk-prev)
      (local-set-key (kbd "M-n") 'diff-hunk-next)
      (local-unset-key (kbd "C-c M-p"))
      (local-unset-key (kbd "C-c M-n"))
      )
  ))

(defun a4-diff-mode-map-keys ()
  (if a4-diff-mode-restore-arista-mappings-p
      (a4-diff-mode-restore-arista-mappings)
    (a4-diff-mode-set-diff-mappings)
    ))

(defun a4-diff-mode-toggle-arista-mappings ()
"Toggle re-mapping of some local key bindings in diff-mode to Arista defaults.

Remapping to default bindings can be disabled by setting variable
`ar-diff-mode-restore-arista-mappings-p' to nil. For example, in your emacs
init file:

            (setq ar-diff-mode-restore-arista-mappings-p nil)

*NOTICE*: in diff-mode, M-p and M-x are mapped to diff-hunk-pref/next
          functions, which are *very* handy for navigating the diffs.

          Because of that, by default, M-p and M-n are NOT remapped to
          Arista default mappings (next/prev-paragraph). To have them
          mapped to Arista defaults, set variable 
          `a4-diff-mode-restore-back-fwd-paragraph-p' to t. For example,
          in your emacs init file:

            (setq a4-diff-mode-restore-back-fwd-paragraph-p t)
"
  (interactive)
  (setq a4-diff-mode-restore-arista-mappings-p
        (not a4-diff-mode-restore-arista-mappings-p))
  (a4-diff-mode-map-keys)
  )

;; add our function to diff-mode's hook

(add-hook 'diff-mode-hook 'a4-diff-mode-map-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Build system integration
;;;

(defvar arastra-root (expand-file-name "/"))
(defvar arastra-virgin-environment (copy-alist process-environment))
(defvar arastra-package-list "")
(defvar arastra-package-list-history '(""))
(defvar arastra-gdb-command "sugdb")
(defvar arastra-kgdb-command "sukgdb")
(defvar arastra-make-target "all")
(defvar arastra-make-target-history '("all install"))
(defvar arastra-last-compile-command "a4 make sanity")
(defvar arastra-last-package "")

(defun arastra-prompt-for-root ()
  (setq arastra-root (expand-file-name 
                      (read-file-name "a4 client root: " arastra-root nil t "")))
  (or (string-match "/$" arastra-root)
      (setq arastra-root (concat arastra-root "/"))))

(defun arastra-make (&optional arg)
  (interactive "P")
  (if arg 
      (progn
        (arastra-prompt-for-root)
        (setq arastra-package-list (read-string "List of packages to build: " 
                                                (car arastra-package-list-history)
                                                '(arastra-package-list-history . 1)))
        (setq arastra-make-target (read-string "Final make target: "
                                        (car arastra-make-target-history)
                                        '(arastra-make-target-history . 1)))
        (setq arastra-make-jobs (read-string "Jobs: " "AUTO"))
        (let* ((pkglist (split-string arastra-package-list "[ ,]+"))
               (last (car (last pkglist)))
               (cmd (apply 'concat 
                           (cons "true" 
                                 (mapcar (lambda (x) 
                                           (format " && a4 make -j %s -p %s %s"
                                                   arastra-make-jobs x
                                                   (if (equal x last) 
                                                       arastra-make-target
                                                     "all install")))
                                         pkglist)))))
          (setq arastra-last-package last)
          (arastra-compile cmd)))
    (arastra-compile arastra-last-compile-command)))
    
(defun arastra-compile (cmd)
  (setq arastra-last-compile-command cmd)
  (let ((default-directory arastra-root))
    (compile cmd))
  (let* ((b (get-buffer "*compilation*"))
         (w (and b (get-buffer-window b))))
    (if w
        (progn
          (select-window w)
          (set-buffer b)
          (goto-char (point-max))
          (insert ">")
          (goto-char (point-max))))))

(defun arastra-make-package (pkg)
  (interactive "sPackage to make: ")
  (arastra-compile (format "a4 make -p %s" pkg)))

(defun arastra-make-sanity (&optional arg)
  (interactive "P")
  (if arg
      (arastra-prompt-for-root))
  (arastra-compile "a4 make sanity"))

(defvar arastra-gdb-script nil)
(defvar arastra-gdb-script-history nil)

(defvar arastra-gdb-script-args nil)
(defvar arastra-gdb-script-args-history nil)

(defun arastra-gdb (&optional arg)
  (interactive "P")
  (let* ((srcdir (format "%ssrc/%s/" arastra-root arastra-last-package))
         (blddir (format "%sbld/%s/" arastra-root arastra-last-package))
         (default-directory arastra-root)
         (testdir (concat srcdir "test/"))
         (gdb-command-name arastra-gdb-command))
    (when arg
      (setq arastra-gdb-script (read-file-name "Run gdb on test script: " 
                                               testdir nil t nil
                                               'arastra-gdb-script-history))
      (if arastra-gdb-script
          (setq arastra-gdb-script-args 
                (read-string "Script arguments: " 
                             (car arastra-gdb-script-args-history)
                             '(arastra-gdb-script-args-history . 1) nil))))

    (if (file-is-python-script arastra-gdb-script)
        (progn
          (gdb (search-path-for-file (parse-colon-path (getenv "PATH"))
                                     "python"))
          (let ((proc (get-buffer-process (current-buffer))))
            (process-send-string proc (format "cd %s\n" blddir))
            (setq default-directory blddir)
            (if (file-exists-p ".gdbinit")
                (process-send-string proc "source .gdbinit\n"))
            (if arastra-gdb-script
                (process-send-string proc (format "set args %s %s\n" 
                                                  arastra-gdb-script
                                                  arastra-gdb-script-args)))))
      (gdb arastra-gdb-script))
    (let ((proc (get-buffer-process (current-buffer))))
      (process-send-string proc "set env CATCH_THROW 1\n")
      ;; The above interacts with tacc/libfwk/Exception.cpp to enable exception
      ;; debugging.
      (if (file-exists-p (concat srcdir ".gdbinit"))
          (process-send-string proc (concat "source " srcdir ".gdbinit\n"))))))



(defvar arastra-kgdb-script nil)
(defvar arastra-kgdb-script-history nil)

(defvar arastra-kgdb-module nil)
(defvar arastra-kgdb-module-history nil)

(defun arastra-kgdb (&optional arg)
  (interactive "P")
  (let ((gdb-command-name arastra-kgdb-command))
    (when arg
      (arastra-prompt-for-root)
      (setq arastra-kgdb-script (read-string "Virtual Machine Address: "
                                             (car arastra-kgdb-script-history)
                                             '(arastra-kgdb-script-history . 1) nil))
      (if arastra-kgdb-script
          (setq arastra-kgdb-module 
                (read-file-name "Module File (optional): " 
                                arastra-root "" t (car arastra-kgdb-module-history)
                                arastra-kgdb-module-history))))

    (gdb (format "%s%s" arastra-root arastra-kgdb-script) arastra-kgdb-module )))
                                                
    

(defun file-is-python-script (filename)
  (string-match "\\.py$" filename))

(defun arastra-find-include ()
  (interactive)
  (let* ((include
          (or
           (save-excursion
             (beginning-of-line)
             (if (looking-at ".*include *[\"<]\\([^\">]+\\)")
                 (buffer-substring (match-beginning 1) (match-end 1))))
           (save-excursion
            (skip-chars-backward "^\n <\"")
            (if (looking-at "[^\n <\"]+")
                (buffer-substring (match-beginning 0) (match-end 0))))
           (error "Can't find filename near point")))
         (path (list 
                default-directory
                (concat arastra-root "aroot/usr/include/")
                "/usr/include/"
                "/usr/include/c++/3.4.2/"
                ;; It would be nice to be smarter about this next few.
                ;; For example, we could run "gcc -v" and look for "include".
                "/usr/lib/gcc-lib/i686-pc-linux-gnu/3.3.3/include/"
                "/usr/lib/gcc-lib/i686-pc-linux-gnu/3.3.3/include/g++-v3/"
                "/usr/lib/gcc-lib/i686-pc-linux-gnu/3.3.3/include/g++-v3/i686-pc-linux-gnu/"
                "/usr/lib/gcc-lib/i686-pc-linux-gnu/3.3.4/include/"
                "/usr/lib/gcc-lib/i686-pc-linux-gnu/3.3.4/include/g++-v3/"
                "/usr/lib/gcc-lib/i686-pc-linux-gnu/3.3.4/include/g++-v3/i686-pc-linux-gnu/"
                ))
         (file (or (search-path-for-file path include)
                   (error "Could not locate %s" include))))
    (find-file file)))

(defun search-path-for-file (path file)
  (catch 'found
    (while path
      (let ((full (concat (car path) file)))
        (if (file-exists-p full)
            (throw 'found full)))
      (setq path (cdr path)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Default keybindings
;;;

(let ((bindings '(
                  ("\C-x\C-p" arastra-make-package)
                  ("\C-x\C-k" arastra-make)
                  ("\C-x\C-j" next-error)
                  ("\C-x\C-h" arastra-gdb)
                  ("\C-x\C-n" arastra-kgdb)
                  ("\C-x\C-i" arastra-find-include)
                  )))
  (mapcar (lambda (binding) 
            (condition-case nil
                (define-key global-map (car binding) (car (cdr binding)))
              (error nil)))
          bindings))


