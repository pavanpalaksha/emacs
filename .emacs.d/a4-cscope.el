;;**********************************************************************
;; cscope functions

(provide 'a4-cscope)

(defun agid-cscope-find-global-definition (symbol)
  "Find a symbol's global definition."
  (interactive (list
		(cscope-prompt-for-symbol "Find this global definition: " nil)
		))
   (let ((save-cscope-program cscope-program)
         (save-cscope-extra-options cscope-extra-options)
         options)

     (if (and agid-prev-types (not (string= agid-prev-types "")))
         (setq options
               (append options (list "-t" (concat "'" agid-prev-types "'")))))

     (if (and agid-prev-packages (not (string= agid-prev-packages "")))
         (setq options
               (append options (list "-p" (concat "'" agid-prev-packages "'")))))

     (setq options (append options (list "-C")))

     (setq cscope-program "agid.py")
     (setq cscope-extra-options options)

     (cscope-find-global-definition symbol)

     (setq cscope-program save-cscope-program)
     (setq cscope-extra-options save-cscope-extra-options)
    ))


(defvar test-base-dir "/src")
(defun a4-cscope-goto-single_definition (symbol)
  (interactive)
  (let (tmp-buf
        file
        function-name
        line-number
        line
        buffer
        (options (list "-d" "-f" (concat test-base-dir "/" "cscope.out") "-L" "-1"
                       symbol)))

    (setq tmp-buf (get-buffer-create "*a4-cscope-tmp-buffer*"))
    (set-buffer tmp-buf)
    (delete-region (point-min) (point-max))
    (apply 'call-process "cscope" nil tmp-buf t options)
    (goto-char (point-min))

    (when (re-search-forward
            "^\\([^ \t]+\\)[ \t]+\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\(.*\\)\n")

      (setq file (match-string-no-properties 1)
            line-number (string-to-number (match-string-no-properties 3)))

      (setq file (concat test-base-dir "/" file))

      (unless (file-readable-p file)
        (kill-buffer tmp-buf)
        (error "File '%s' is not readable or exists" file))

      (setq buffer (find-file-noselect file))
      (set-buffer buffer)
      (goto-line line-number)
      )
    (kill-buffer tmp-buf)
    (switch-to-buffer buffer)
    ))

;;**********************************************************************
;; cscope functions

(defvar a4-cscope-base-dir "/src")
(defvar a4-cscope-match-list ())

(defun a4-cscope-internal-find (symbol find-global-def)
"Invokes cscope on a symbol returning a list of matching files/line-number pairs."
  (interactive)
  (let (tmp-buf
        file
        function-name
        line-number
        line
        cscope-options)

    ; setup options for cscope
    ; if asked to find global definition, use option -1, otherwise use -0

    (setq cscope-options (list "-d" "-f" 
                               (concat a4-cscope-base-dir "/" "cscope.out") "-L"))
    (if find-global-def
        (setq cscope-options (append cscope-options (list "-1" symbol)))
      (setq cscope-options (append cscope-options (list "-0" symbol))))

    ; run cscope process w/ options and capture output on temporary buffer

    (setq tmp-buf (get-buffer-create "*a4-cscope-tmp-buffer*"))
    (set-buffer tmp-buf)
    (delete-region (point-min) (point-max))
    (apply 'call-process "cscope" nil t t cscope-options)
    (goto-char (point-min))

    ; reset match list to be returned

    (setq a4-cscope-match-list ())

    ; scan output line by line

    (while (re-search-forward "\\([^\n]+\\)\n" (point-max) t)
      (setq line (match-string-no-properties 1))

      ; if matched expected pattern add pair of (file line-number) to match list
      ;
      ; Expected pattern is (pseudo-EBNF):
      ;
      ;   <file> SPC <function> SPC <line-number> SPC <src-code-line>

      (when
          (string-match
           "^\\([^ \t]+\\)[ \t]+\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\(.*\\)"
           line)

        (setq file (match-string-no-properties 1 line))
        (setq line-number (string-to-number
                           (match-string-no-properties 3 line)))

        (setq a4-cscope-match-list
              (append a4-cscope-match-list (list (list file line-number))))

        )
      )
    (kill-buffer tmp-buf)
    )
  a4-cscope-match-list
  )

(defun a4-cscope-find-file-at-line (file line-number)
  (let (buffer)
    (setq file (concat a4-cscope-base-dir "/" file))

    (unless (file-readable-p file)
      (error "File '%s' is not readable or exists" file))

    (setq buffer (find-file-noselect file))
    (set-buffer buffer)
    (goto-line line-number)
    (if (not (eq (current-buffer) buffer))
        (switch-to-buffer-other-window buffer))
    ))

(defun a4-cscope-find-and-switch-if-single-match (symbol find-global-def)
  (let (match-list first-match file file-number num-matches)

    (setq match-list (a4-cscope-internal-find symbol find-global-def))

    ; if number of matches was not 1, do regular cscope match

    (if (not (= (length match-list) 1))

        (if find-global-def
            (cscope-find-global-definition symbol)
          (cscope-find-this-symbol symbol))

      ; just one match, try to switch to file/line-num

      (setq first-match (car match-list)
            file (car first-match)
            line-number (car (cdr first-match)))

      ;; (print (format "File: %s, line %d" file line-number))

      (setq file (concat a4-cscope-base-dir "/" file))

      (unless (file-readable-p file)
        (error "File '%s' is not readable or exists" file))

      (setq buffer (find-file-noselect file))
      (set-buffer buffer)
      (goto-line line-number)
      (switch-to-buffer buffer)
      )
    ))

