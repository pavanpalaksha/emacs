;; Copyright (c) 2011 Arista Networks, Inc.  All rights reserved.
;; Arista Networks, Inc. Confidential and Proprietary.

(defvar tac-mode-map ()
  "Keymap used in tac-mode buffers.")

(setq tac-font-lock-keywords
  `(,(rx symbol-start (or "TacModule"
                          "CppInclude"
                          "CppInlineInclude"
                          "CppBlock"
                          "CppInlineBlock"
                          "Tac::Namespace"
                          "Tac::Type"
                          "Tac::Enum"
                          "Tac::EnumBoolSetArray"
                          "Tac::Nominal"
                          "Tac::Ordinal"
                          "Tac::Constrainer"
                          "import"
                          "local"
                          "mutableThruConst"
                          "mutable"
                          "const"
                          "initially"
                          "immutable"
                          "final"
                          "extensible"
                          "interface"
                          "dense"
                          "prealloc"
                          "ordered"
                          "overlay"
                          "overridable"
                          "overriding"
                          "overloading"
                          "inline"
                          "extern"
                          "invasive"
                          "in"
                          "out"
                          "inout"
                          "static"
                          "new"
                          "using"
                          "queue"
                          "stack"
                          "array"
                          "sparse"
                          "friend"
                          "coroutine"
                          "embedded"
                          "break"
                          "continue"
                          "if"
                          "else"
                          "while"
                          "do"
                          "for"
                          "switch"
                          "case"
                          "default"
                          "return"
                          "timeout"
                          "wait"
                          "or"
                          "foreach"
                          "bool" "void"
                          "U64" "U32" "U16" "U8"
                          "S64" "S32" "S16" "S8"
                          "true" "false")
         symbol-end)
    ,(rx "`" (or "isNotifyingByDefault"
                 "allowsMultipleNotifiees"
                 "allowsNotifieeImpl"
                 "allowsNotifiee"
                 "hasFunctorInterface"
                 "hasIndexedFunctorInterface"
                 "hasIndexedFunctorConstInterface"
                 "hasOwnPtrInterface"
                 "hasVirtualBaseType"
                 "hasFactoryFunction"
                 "allowsDirInstantiation"
                 "allowsIndirectRef"
                 "isBitSetType"
                 "largerThanPtr"
                 "isDSafeCertified"
                 "hasAttrLog"
                 "hasExternalStrep"
                 "hasPackedRep"
                 "isConnectionInType"
                 "isConnectionOutType"
                 "isAppliedOnInit"
                 "isAppliedOnWrite"
                 "isAppliedInDestructor"
                 "isAppliedOnEnclosingDelete"
                 "isAppliedOnDelete"
                 "isAppliedOnAudit"
                 "isAppliedInConstructor"
                 "isScheduled"
                 "doesReinstantiateOnModify"
                 "wasConstructorParam"
                 "version"
                 "isWeakReference"
                 "hasVirtualMutator"
                 "hasInlinedMutator"
                 "hasInlinedAccessor"
                 "hasExternalMutator"
                 "hasExternalAccessor"
                 "hasTemplateAccessor"
                 "clone"
                 "isDefaultCollection"
                 "isNonconstPtr"
                 "isNetByteOrder"
                 "hasPtrIf"
                 "hasDataMember"
                 "hasNotifyOnUpdate"
                 "allowsScheduledNotification"
                 "isLoggedAttr"
                 "isEqualityIndependentAttr"
                 "period"
                 "timeIndex"
                 "range"
                 "ordering"
                 "timeInputAttr"
                 "iterator"
                 "inverse"
                 "generator") 
         symbol-end)))




(defcustom tac-indent 3
  "Number of columns for a unit of indentation in tac mode."
  :group 'tac
  :type 'integer)

(defcustom tac-mode-hook nil
  "*Hook called by `tac-mode'."
  :type 'hook
  :group 'tac)


(unless tac-mode-map
  (setq tac-mode-map (make-sparse-keymap))
  (substitute-key-definition 'indent-for-tab-command 
 'tac-indent-line-or-region tac-mode-map global-map)
  (define-key tac-mode-map ";" 'tac-electric-semi)
  (define-key tac-mode-map "(" 'tac-electric-lparen)
  (define-key tac-mode-map ")" 'tac-electric-rparen)
  (define-key tac-mode-map "{" 'tac-electric-lbrace)
  (define-key tac-mode-map "}" 'tac-electric-rbrace)
  (define-key tac-mode-map ":" 'tac-electric-colon))

(defun tac-mode ()
  "Major mode for editing TAC files.

Key bindings:
\\{tac-mode-map}
Imenu Support:

  Tac-mode supports imenu, which creates a list of the location of all Type
  definitions in your TAC file. 

  Imenu can be used in the following manner:

    - Types can be searched through through the M-x `imenu' command

    - You can have a menu bar entry for the tac types. For that, you should
      add the following lines to your emacs init file: 

        (add-hook 'tac-mode-hook 
                  (lambda () (imenu-add-to-menubar \"Tac-Types\")))

    - You can also navigate through the next/prev Type in your file
      through the following commands: `a4-nav-imenu-prev' and
      `a4-nav-imenu-next'.

      Syggested keyword mapping is:

        M-p => `a4-nav-imenu-prev'
        M-n => `a4-nav-imenu-next'

      For that, you'll need the following lines in your emacs init file:

        (require 'a4-nav)
        (add-hook 'tac-mode-hook 
                  (lambda () 
                    (local-set-key \"\\M-p\" 'a4-nav-imenu-prev)
                    (local-set-key \"\\M-n\" 'a4-nav-imenu-next)
                    ))

Which Function Mode Support:

  To enable the which-function-mode on TAC files (which in the TAC case 
  displays the current Type you're in), you should add the following lines
  to your emacs init file:

    (require 'which-func)
    (add-to-list 'which-func-modes 'tac-mode t)
"
  (interactive)
  (c-mode)
  (kill-all-local-variables)
  (setq major-mode 'tac-mode
        mode-name "TAC")
  (set-syntax-table c-mode-syntax-table)
  (use-local-map tac-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(tac-font-lock-keywords))
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; This next batch is a hack so "c-beginning-of-statement-1" works.
  ;; This seems a bit fragile.
  ;; There is some chance I'm on the wrong path here, and really tac-mode
  ;; should join pike-mode, awk-mode, java-mode, c++-mode, objc-mode, idl-mode,
  ;; and the host of other modes that are built on top of cc-mode.
  (set (make-local-variable 'c-syntactic-ws-end) "\\s ")
  (set (make-local-variable 'c-stmt-delim-chars) "^;{}?")
  (set (make-local-variable 'c-opt-cpp-start) 
       "how does one construct a regexp that matches nothing?")
  (set (make-local-variable 'c-nonlabel-token-key) ".")
  (set (make-local-variable 'c-keywords-regexp) "\w+")
  ;; set expression for imenu (which is also used in which-func-mode)
  (setq imenu-generic-expression
        '((nil "^\\s-*\\([A-Z]\\([A-Z]\\|[a-z]\\|[0-9]\\|_\\|::\\)*\\)\\s-*:\\s-*.*Tac\\s-*::\\s-*\\(Type\\|Nominal\\|Ordinal\\|Enum\\|EnumBoolSetArray\\)[^{;]*{" 1)))
  (run-mode-hooks 'tac-mode-hook))

(defun tac-backward-ws ()
  (while (and (not (bobp))
              (or 
               (looking-back "[ \t\n\r\f\v]+")
               (looking-back "//.*")))
    (goto-char (match-beginning 0))))

(defun tac-beginning-of-statement ()
  ;; Hopefully, this will work well enough, because this is *not* easy.
  (c-beginning-of-statement-1))

(defun tac-calculate-indentation ()
  (save-excursion
    (back-to-indentation)
    (let* ((start (point))
           (brace (c-most-enclosing-brace (c-parse-state)))
           (close-offset (if (looking-at "}") tac-indent 0))
           (continued-statement-offset 
            (progn (tac-backward-ws) 
                   (if (or (bobp) (looking-back "[;({}]"))
                       0 tac-indent)))
           (in-arglist (and brace (progn (goto-char brace)
                                         (looking-at "("))))
           (arg-indent (and in-arglist
                            (or (progn (forward-char 1)
                                       (skip-chars-forward " \t")
                                       (not (eolp)))
                                (progn (forward-line 1)
                                       (end-of-line)
                                       (and (< (point) start)
                                            (progn (beginning-of-line)
                                                   (back-to-indentation)
                                                   (not (eolp))))))
                            (current-column))))
      (or arg-indent
          (+
           (if brace
               (+ (- close-offset)
                  (progn
                    (goto-char brace)
                    (tac-beginning-of-statement)
                    (back-to-indentation)
                    (- (current-indentation)
                       (if (looking-at ".*\\bnamespace\\b") 
                           (- tac-indent close-offset) 
                         0)))
                  tac-indent)
             0)
           (if in-arglist tac-indent 0)
           continued-statement-offset)))))
  
  
(defun tac-indent-line ()
  (interactive)
  (let ((target (tac-calculate-indentation))
        (cur (current-indentation)))
    (unless (= target cur)
      (save-excursion
        (indent-line-to target)))
    (if (< (current-column) target)
        (back-to-indentation))))

(defun tac-indent-region (beg end)
  (interactive "r")
  (save-excursion 
    (let ((end-mark (copy-marker end)))
      (goto-char beg)
      (beginning-of-line)
      (while (< (point) end-mark)
        (if (not (looking-at "[ \t]*$"))
            (tac-indent-line))
        (forward-line 1)))))

(defun tac-indent-line-or-region (region)
  "If the region is active, re-indent the region; otherwise,
re-indent the current line."
  (interactive (list (use-region-p)))
  (if region 
      (tac-indent-region (region-beginning) (region-end))
    (tac-indent-line)))

(defun tac-electric(str maybe-newline)
   (insert str)
   (tac-indent-line)
   (if (and maybe-newline c-auto-newline)
       (progn
         (insert "\n")
         (tac-indent-line))))

(defun tac-electric-semi()
   (interactive)
   (tac-electric ";" t ))

(defun tac-electric-lparen()
   (interactive)
   (tac-electric "(" nil))

(defun tac-electric-rparen()
   (interactive)
   (tac-electric ")" nil))

(defun tac-electric-lbrace()
   (interactive)
   (tac-electric "{" t))

(defun tac-electric-rbrace()
   (interactive)
   (tac-electric "}" t))

(defun tac-electric-colon()
   (interactive)
   (tac-electric ":" t))
