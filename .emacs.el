;;(load-library "Arastra")
(add-to-list 'load-path "~/.emacs.d/")
;;(load-library "p4")

;;pylint
(require 'compile)
(require 'tramp)
(load-library "python-pylint")
(global-set-key (kbd "C-x y") 'python-pylint)

;;bug
;;(require 'bug-mode)
;;(setq bug-tmp-dir "/home/ppal/bugs_tmp")
;;(global-set-key (kbd "C-x g b") 'bug)
;;(global-set-key (kbd "C-x g g") 'bug-goto)
;;(global-set-key (kbd "C-x g d") 'bug-describe-change)
;;(global-set-key (kbd "C-x g k") 'bug-kill-all-buffers)
;;(global-set-key (kbd "C-x g q") 'bug-toggle-default-quiet-mode)

(require 'cp-utils)
(global-set-key (kbd "M-w") 'cp-cur-word)

;;saves all open buffers and reopens them when you quit and restart emacs
(desktop-save-mode 1) 

;;(require 'autopair)
;;(autopair-global-mode) ;; enable autopair in all buffers

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(indent-tabs-mode nil)
 '(tab-always-indent nil))

;; easy moving for non-tmux sessions
(global-set-key (kbd "S-<left>") 'windmove-left) ; move to left window
(global-set-key (kbd "S-<right>") 'windmove-right) ; move to right window
(global-set-key (kbd "S-<up>") 'windmove-up) ; move to upper window
(global-set-key (kbd "S-<down>") 'windmove-down) ; move to lower window

;; Moving between windows in tmux
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; Delete current line
(global-set-key (kbd "C-d") 'kill-whole-line)

;; Undo and Redo
(global-set-key (kbd "C-z") 'undo) ; ?Ctrl+z?
(global-set-key (kbd "C-Z") 'redo) ; ?Ctrl+Shift+z?

(transient-mark-mode t)         ; make the current 'selection' visible
(delete-selection-mode t)       ; delete the selection area with a keypress

;;line numbers
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%d ")
(setq linum-disabled-modes-list '(eshell-mode wl-summary-mode compilation-mode shell-mode))
(defun linum-on ()
  (unless (or (minibufferp) (member major-mode linum-disabled-modes-list))
    (linum-mode 1)))

;; cut copy and paste
(cua-mode t)
    (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
    (transient-mark-mode 1) ;; No region when it is not highlighted
    (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;;color theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-gnome)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "darkslategrey" :foreground "wheat" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 121 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(font-lock-comment-delimiter-face ((default (:inherit font-lock-comment-face)) (((class color) (min-colors 16)) (:foreground "LightGoldenrod2"))))
 '(font-lock-comment-face ((t (:foreground "#f3f781"))))
 '(ido-first-match ((t (:foreground "#d6d633"))))
 '(ido-incomplete-regexp ((t (:foreground "#ffffff"))))
 '(ido-indicator ((t (:foreground "#ffffff"))))
 '(ido-only-match ((t (:foreground "#ffcc33"))))
 '(ido-subdir ((t (:foreground "#a3a375")))))

;; configure emacs to find local TAGS file automatically
(require 'etags-table)
(setq etags-table-search-up-depth 15)

;; ignore case name while searching for file/buffer name
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq completion-cycle-threshold t)

;; tac and tin files.                        
(require 'find-file "find-file")
(setq-default cc-other-file-alist
              '(
                ("\\.tac$"  (".tin" ".itin"))
                ("\\.tin$"  (".tac" ))

                ("\\.cc$"  (".hh" ".h"))
                ("\\.hh$"  (".cc" ".C"))

                ("\\.c$"   (".h"))
                ("\\.h$"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp"))

                ("\\.cxx$" (".hh" ".h"))
                ("\\.cpp$" (".hpp" ".hh" ".h"))
                ))
(define-key global-map "\C-co" 'ff-find-other-file)

;; Colors log messages in .log files and in your emacs shells. This makes parsing through input SO much easier.
;; Highlights the package i.e. [Ebra], the time stamp, and the agent name.
(define-generic-mode 'log-mode
      '("<<<" "===")
      '()
      '(("^\\(\\[[a-zA-Z0-9/-]*]\\)" . 'font-lock-keyword-face)
        ("^\\(\\[[a-zA-Z0-9/-]*]\\) \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9:]*.[0-9]*\\)\s*\\([0-9]*\\) \\([a-zA-Z]*\\)"                                                                                                                                               
         (2 'font-lock-variable-name-face)
         (3 'font-lock-type-face)
         (4 'font-lock-type-face) )
        (" traceback" . 'font-lock-comment-delimiter-face)
        (" Exception" . 'font-lock-comment-delimiter-face)
        ("%[A-Z0-9_-]*". 'font-lock-constant-face ) )
      '(".log\\'")
      nil)

(font-lock-add-keywords 'shell-mode
    '(("^\\(\\[[a-zA-Z0-9/-]*]\\)" . 'font-lock-keyword-face)
      ("^\\(\\[[a-zA-Z0-9/-]*] \\)?\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9:]*.[0-9]*\\)\s*\\([0-9]*\\) \\([a-zA-Z]*\\)"
       (2 'font-lock-variable-name-face)
       (3 'font-lock-type-face)  
       (4 'font-lock-type-face) )
      (" traceback" . 'font-lock-comment-delimiter-face)
      (" Exception" . 'font-lock-comment-delimiter-face)
      ("%[A-Z0-9_-]*". 'font-lock-constant-face )))

;; Highlight the current line I am editing
;;(global-hl-line-mode 1)
;;(set-face-foreground `highlight nil )
;; To customize the background color
;;(set-face-background 'hl-line "#808080")  ;; Emacs 22 Only

;; highlight other parenthesis
(show-paren-mode t)

(require 'ido)                                                           
(setq ido-enable-flex-matching t)                                                          
(setq ido-everywhere t)                                                                     
(ido-mode 1) 

 ;; Ido face for indicating incomplete regexps. (don't use this either)

;; remap end of documents and start of documents
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

(global-set-key (kbd "C-<") 'beginning-of-buffer)
(global-set-key (kbd "C->") 'end-of-buffer)

;; matching parenthesis
(defun paren-match ()                                                  
  "Tries to jump to the matching parenthesis to the one currently      
  under the point."                                                    
  (interactive)                                                        
  (cond ((looking-at "[{\[\(]") (forward-sexp 1) (backward-char))      
        ((looking-at "[]})]") (forward-char) (backward-sexp 1))))      
(global-set-key (kbd "C-]") 'paren-match)

;; autocomplete mode
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/home/ppal/.emacs.d//ac-dict")
(ac-config-default)
(global-set-key '[S-tab] 'dabbrev-expand)
(global-set-key [(tab)] 'smart-tab)
(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
        (dabbrev-expand nil))
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (if (looking-at "\\_>")
          (dabbrev-expand nil)
        (indent-for-tab-command)))))
