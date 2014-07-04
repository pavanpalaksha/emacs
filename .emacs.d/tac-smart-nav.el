;; Copyright (c) 2012 Arista Networks, Inc.  All rights reserved.
;; Arista Networks, Inc. Confidential and Proprietary.

(provide 'tac-smart-nav)
(require 'a4-nav)
(require 'a4-cscope)
(require 'xcscope)

;;----------------------------------------------------------------------
;; helper functions

(defun tac-islowercase (str)
  (let ((first-char (string-to-char str)))
    ;; must be: "a" <= first-char <= "z"
    (and (>= first-char 97) (<= first-char 122))
    )
  )

(defun tac-isuppercase (str)
  (let ((first-char (string-to-char str)))
    ;; must be: "A" <= first-char <= "Z"
    (and (>= first-char 65) (<= first-char 90))
    )
  )

(defun tac-tolower-first-char (str)
  (if (string= str "")
      str
    (concat (downcase (substring str 0 1)) (substring str 1))
  ))

(defun tac-toupper-first-char (str)
  (if (string= str "")
      str
    (concat (upcase (substring str 0 1)) (substring str 1))
  ))

;;----------------------------------------------------------------------
;; namespace related functions

(defun tac-ns-list-to-str (lst)
"Convert a list of namespaces to a string separated by '::' (empty strings in the list are ignored)."
  (let ((str (car lst))
        (lst (cdr lst)))
    (dolist (el lst str)
      (unless (string= el "")
        (setq str (concat str "::" el))))))

(defun tac-do-get-cur-namespace-list (ns-pattern)
  (interactive)
  (save-excursion 
    (let (search-pattern
          (saved-point (point))
          (ns-list ()))
      (goto-char (point-min))

      ; search for the following options here:
      ;
      ;   - language specific namespace pattern
      ;   - open curly bracket
      ;   - close curly bracket
      ;
      ; (Notice: we need to match on standalone curly brackets so that we
      ;          can determine the point where namespaces end)

      (setq search-pattern (concat "\\(" ns-pattern "\\|\\({\\|}\\)\\)"))

      (while (re-search-forward search-pattern saved-point t)
        (let ((namespace (match-string-no-properties 2))
              (bracket (match-string-no-properties 3)))
          (cond
           ((not (eq namespace nil))
            (setq ns-list (append ns-list (list namespace)))
            )
           ((string= bracket "{")
            (setq ns-list (append ns-list (list "")))
            )
           ((string= bracket "}")
            (setq ns-list (butlast ns-list))
            )
           )
          ))
      ns-list
      )))

;; pattern for C++: 'namespace <ns> {'
(setq tac-ns-pattern-for-cpp "namespace\\s-+\\(\\w+\\)\\s-*{")

;; pattern for TAC: '<ns> : Tac::Namespace {'
(setq tac-ns-pattern-for-tac "\\(\\w+\\)\\s-+:\\s-+Tac\\s-*::\\s-*Namespace\\s-*{")

(defun tac-get-buffer-filename-and-ext ()
  (interactive)
  (let ((ext nil)
        (filename nil)
        (buf-file-name (buffer-file-name))
        )
    (if (string-match "^\\(.*\\)\\.\\(\\w+\\)$" buf-file-name)
        (progn
          (setq filename (match-string-no-properties 1 buf-file-name))
          (setq ext (match-string-no-properties 2 buf-file-name))
          (list filename ext)
          )
      nil
      )
  ))

(defun tac-get-buffer-pkg-filename-and-ext ()
  (interactive)
  (let (filename
        ext
        pkg
        (filename-and-ext (tac-get-buffer-filename-and-ext))
        )
    (if filename-and-ext
        (progn
          (setq filename (car filename-and-ext))
          (setq ext (nth 1 filename-and-ext))
          (if (string-match "/src/\\(\\w+\\)/\\(\\w+\\)$" filename)
              (progn
                (setq pkg (match-string-no-properties 1 filename))
                (setq filename (match-string-no-properties 2 filename))
                (list pkg filename ext)
                )
            nil)
          )
      nil)
    ))

(defun tac-get-buffer-ext ()
  (let ((tmp (tac-get-buffer-filename-and-ext)))
    (if tmp
        (nth 1 tmp)
      nil)
    ))

(defun tac-get-buffer-language ()
  (let (ext
        (buf-lang nil)
        )
    (setq ext (tac-get-buffer-ext))
    (cond
     ((string= ext "tac")
      (setq buf-lang "tac"))
     ((member ext (list "tin" "itin" "cpp" "cxx" "h"))
      (setq buf-lang "c++"))
     )
    buf-lang
    ))

(defun tac-get-ns-pattern-for-cur-buffer ()
  (let ((buf-lang (tac-get-buffer-language))
        (ns-pattern nil)
        )
    (cond
     ((string= buf-lang "tac")
      (setq ns-pattern tac-ns-pattern-for-tac)
      )
     ((string= buf-lang "c++")
      (setq ns-pattern tac-ns-pattern-for-cpp)
      ))))

(defun tac-get-cur-namespace-list ()
  (let ((ns-pattern (tac-get-ns-pattern-for-cur-buffer)))
    (when ns-pattern
      (tac-do-get-cur-namespace-list ns-pattern)
      )
    ))

(defun tac-get-cur-namespace ()
"Return the string corresponding to the namespaces in the current position
in a TAC file buffer.

For example, given the following tac buffer:

   --------------------------------

   // position 1

   Ns1 : Tac::Namespace {

   // position 2

   Ns2 : Tac::Namespace {

   // position 3

   }

   // position 4

   }

   // position 5

  --------------------------------

Or C++ buffer like:

   --------------------------------

   // position 1

   namespace Ns1 {

   // position 2

   namespace Ns2 {

   // position 3

   }

   // position 4

   }

   // position 5

  --------------------------------

This function returns the following string for the different 
positions in the buffer:

  position 1 => ''
  position 2 => 'Ns1'
  position 3 => 'Ns1::Ns2'
  position 4 => 'Ns1'
  position 5 => ''

"
  (interactive)
  (tac-ns-list-to-str (tac-get-cur-namespace-list))
  )

;;----------------------------------------------------------------------
;; TAC id-path related functions

(defvar tac-id-path-delim "[^A-Za-z0-9_:]")

(defun tac-id-path-beg ()
  (re-search-backward tac-id-path-delim)
  (re-search-forward tac-id-path-delim))

(defun tac-id-path-end ()
  (re-search-forward tac-id-path-delim)
  (re-search-backward tac-id-path-delim))

(defun tac-get-id-path-around-point ()
"Return the tac-id-path around the point as a string."
  (save-excursion
    (if (not (eobp))
 	(forward-char 1))
    (forward-word -1)
    (forward-word 1)
    (tac-id-path-beg)
    (buffer-substring-no-properties (point) (progn (tac-id-path-end) (point)))))

;;----------------------------------------------------------------------
;; TAC type related functions

(defvar tac-ptr-pattern "::\\(Valid\\|Raw\\)?Ptr\\(Const\\)?$")

(defun tac-get-type-around-point ()
"Return the tac-type around point (excluding the ptr part, if present)."
  (let ((id-path (tac-get-id-path-around-point))
        (ptr-idx nil))
    ; get rid of ptr part (e.g. "::Ptr", "::PtrConst", etc.) if it's there
    (setq ptr-idx (string-match tac-ptr-pattern id-path))
    (when ptr-idx
      (setq id-path (substring id-path 0 ptr-idx)))
    id-path)
  )

(defun tac-replace-colons (str)
  (while (string-match "::" str)
    (setq str (replace-match "__" t t str)))
  str)

(defun tac-revert-colons (str)
  (while (string-match "__" str)
    (setq str (replace-match "::" t t str)))
  str)

(defun tac-get-type-full-path-candidates (tac-type)
"Return list of full-path candidates for a given type, based on the current
namespace."
  (interactive)
  (let (idx
        ns
        ns-str
        (final-list ())
        (cand-list ())
        (ns-list (tac-get-cur-namespace-list))
        )

    ; if type starts w/ '::' it's already the full namespace, nothing else to do

    (setq idx (string-match "::" tac-type))
    (if (and idx (= idx 0))
        (progn
          ; get rid of initial '::'
          (setq tac-type (replace-match "" t t tac-type))
          (setq cand-list (list tac-type))
          )
      (progn
        ; if already a compound name, put it as the first in the list
        ; (will be the last one when we revert it)
        (if (string-match "::" tac-type)
          (setq cand-list (list tac-type)))

        (dolist (ns ns-list cand-list)
          (unless (string= ns "")
            (if (not ns-str) (setq ns-str ns)
              (setq ns-str (concat ns-str "::" ns)))
            (setq cand-list 
                  (append cand-list (list (concat ns-str "::" tac-type))))
            ))
        ))
    ; reverse the list and replace all colons
    (setq cand-list (reverse cand-list))
    (dolist (tac-type cand-list final-list)
      (setq final-list (append final-list (list (tac-replace-colons tac-type)))))

    ;; (message final-list)
    final-list
  ))


(defun tac-get-type-full-path ()
"Return the full path of a tac type (excluding the ptr part, if present),
adding the current namespace(s) if needed."
  (interactive)
  (let (ns
        idx
        (tac-type (tac-get-type-around-point)))

    ; if type starts w/ '::' treat it as under the global namespace

    (setq idx (string-match "::" tac-type))
    (if (and idx (= idx 0))
        (progn
          (setq tac-type (replace-match "" t t tac-type))
          (while (string-match "::" tac-type)
            (setq tac-type (replace-match "__" t t tac-type)))
          )
      (progn
        ;
        ; If type is already compound, then leave it as is, otherwise add
        ; the current namespace in front of it.
        ;
        ; Example (assuming definitions are inside namespaces Ns1 and Ns2):
        ;
        ;         Abc::Efg           => Abc::Efg
        ;         Abc                => Ns1::Ns2::Abc
        ;         Abc::Ptr           => Ns1::Ns2::Abc
        ;         Abc::Efg::PtrConst => Abc::Efg

        (unless (string-match "::" tac-type)
          (setq ns (tac-get-cur-namespace))
          (when ns
            (setq tac-type (concat ns "::" tac-type))))
        (while (string-match "::" tac-type)
          (setq tac-type (replace-match "__" t t tac-type)))
        )
      )
    tac-type
    (print tac-type)
  ))

(defun tac-show-type ()
  (interactive)
  (message (concat "TAC-TYPE: " (tac-get-type-full-path)))
  )

(defface tac-namespace-face
  '((((class color) (background dark))
     (:foreground "black" :background "white"))
    (((class color) (background light))
     (:foreground "black" :background "gold"))
    (t (:bold t)))
  "Face used show the current namespace in a temporary string.")

;; (set-face-attribute 'tac-namespace-face nil
;;                     :background "gold"
;;                     :foreground "black"
;;                     )

(defvar tac-show-ns-on-find t)
(setq tac-show-ns-on-find nil)

(defun tac-toggle-show-ns ()
  (interactive)
  (setq tac-show-ns-on-find (not tac-show-ns-on-find)))

(defun tac-goto-file-at-line-or-pos (base-dir
                                     file
                                     line-number
                                     pos
                                     old-buffer
                                     try-to-show-ns)
  (interactive)
  (let (new-buffer)
    (if base-dir
        (setq file (concat base-dir "/" file)))

    (unless (file-readable-p file)
      (error "File '%s' is not readable or exists" file))

    (setq new-buffer (find-file-noselect file t))
    (set-buffer new-buffer)
    (if line-number
        (progn
          (goto-char (point-min))
          (forward-line (1- line-number)))
      (goto-char pos))

;;    (goto-line line-number)
;;    (if (eq old-buffer new-buffer)
;;        (switch-to-buffer new-buffer)
;;      (switch-to-buffer-other-window new-buffer)

    ; if not same buffer, open new one as "other" window (e.g. one the side)
    (if (not (eq old-buffer new-buffer))
        (pop-to-buffer new-buffer))

    (if (and tac-show-ns-on-find (not (eq old-buffer new-buffer)) try-to-show-ns)
        (tac-show-cur-namespace))
    ))

(defun tac-type-butlast (tokens)
  ;; rebuild the tac type name w/o the last token
  (setq tokens (butlast tokens 1))
  (when tokens
    (mapconcat 'identity tokens "::")
    )
  )

(defun tac-strip-attr (attr)
  (if (string= (substring attr -2) "Is")
      (substring attr 0 -2)
    attr)
  )

(defun tac-remove-iter (attr)
  (if (string-match "\\(.*\\)Iter\\(\\(Const\\)?\\(OrNext\\)?\\)?$" attr)
      (setq attr (match-string-no-properties 1 attr)))
  attr
  )

(defun tac-split-type-and-attr (tac-type)
  (let (last-token
        attr
        (tokens (split-string tac-type "::")))
    (setq last-token (car (last tokens)))
    (if (tac-islowercase last-token)
        ; last token is lowercase, treat it as an attribute
        (progn
          (setq attr (tac-strip-attr last-token))
          (setq tac-type (tac-type-butlast tokens))
          ; if just a lowercase token, try to find the uppercase Type
          ; e.g. fapPort -> convert to FapPort, and handle some special cases (e.g. Iter)
          (if tac-type
              (list tac-type (tac-remove-iter attr))
            (setq tac-type (tac-toupper-first-char attr))
            (setq attr nil)
            (list tac-type attr)
            )
          )
      ; last token is upper case, check for special cases
      (if (string-match "\\(.*\\)Iterator\\(Const\\)?$" last-token)
          (progn
            (setq attr (match-string-no-properties 1 last-token))
            (setq attr (tac-tolower-first-char attr))
            (list (tac-type-butlast tokens) attr)
            )
        (list tac-type nil)
        )
      )
    ))

(defun tac-do-find-type-def (tac-type)
  (interactive)
  (let (match-list
        num-matches
        first-match
        file
        file-number
        (found nil)
        full-path-list
        text
        (old-buffer (current-buffer))
        (save-file buffer-file-name)
        (save-pos (point))
        type-and-attr
        attr
        )

    ; split type / attribute if attribute present

    (setq type-and-attr (tac-split-type-and-attr tac-type))
    (setq tac-type (car type-and-attr))
    (setq attr (nth 1 type-and-attr))

    (when tac-type
      ;;(message (format "Type: %s, Attribute: %s" tac-type (if attr attr "<none>")))

      ; get original list of full-path candidates for tac-type
      (setq full-path-list (tac-get-type-full-path-candidates tac-type))

      (while (and full-path-list (not found))
        (setq full-path (car full-path-list)
              full-path-list (cdr full-path-list))
        
        (setq match-list (a4-cscope-internal-find full-path t))
        (setq num-matches (length match-list))

        (cond
         ((= num-matches 1)
          (setq first-match (car match-list)
                file (car first-match)
                line-number (car (cdr first-match)))
          (tac-goto-file-at-line-or-pos a4-cscope-base-dir
                                        file line-number nil old-buffer t)
          (when attr
            (re-search-forward (concat "^\\s-+" attr) nil t))

          (setq found t)

          ; XXX we're already scanned the type once, we shouldn't do it again here
          ;     must change function tac-get-type-full-path-candidates to return
          ;     the found type as well
          (setq text (tac-get-type-around-point))
          ; set current file/line into navigation ring
          (a4-nav-ring-add (list save-file save-pos) (tac-revert-colons full-path))
          )

         ((> num-matches 1)
          (cscope-find-global-definition full-path)
          (setq found t))
         ))
      (if (not found)
          (message (concat "Error: Symbol definition for '" tac-type "' not found")))
      )
  ))

(defun tac-get-tac-type-from-cur-function ()
  (save-excursion 
    (let ((tac-type nil)
          (cur-func (a4-nav-imenu-prev)))
      (when (and cur-func (string-match "\\(\\w+\\)::\\w+" cur-func))
        (setq tac-type (match-string-no-properties 1 cur-func))
        ;; (message (concat "CUR-TYPE: " tac-type))
        )
      tac-type
      )))

(defun tac-get-cur-tac-type ()
  (save-excursion 
    ; a4-nav-imenu-prev returns the name of the current tac-type, if any
    (a4-nav-imenu-prev)
    ))

(defun a4-csc (symbol)
  "Find a symbol's global definition."
  (interactive (list
		(cscope-prompt-for-symbol "Find this global definition: " nil)
		))
  (let ( (cscope-adjust t) )	 ;; Use fuzzy matching.
    (setq cscope-symbol symbol)
    (cscope-call (format "Finding global definition: %s" symbol)
		 (list "-0" symbol) nil 'a4-cscope-process-filter
		 'cscope-process-sentinel)
    ))

(defun tac-get-top-level-type ()
"Return name of top level type."
  (interactive)
  (let (match-list
        num-matches
        first-match
        file
        file-number
        (found nil)
        tac-type
        full-path-list
        text
        (old-buffer (current-buffer))
        (save-file buffer-file-name)
        (save-pos (point))
        )

    ; get original tac-type under cursor plus list of full-path candidates for it
    (setq tac-type (tac-get-type-around-point))
    (setq full-path-list (tac-get-type-full-path-candidates tac-type))

    (while (and full-path-list (not found))
      (setq full-path (car full-path-list)
            full-path-list (cdr full-path-list))
      
      (setq match-list (a4-cscope-internal-find full-path t))
      (setq num-matches (length match-list))

      (cond
       ((= num-matches 1)
        (setq first-match (car match-list)
              file (car first-match)
              line-number (car (cdr first-match)))
        (tac-goto-file-at-line-or-pos a4-cscope-base-dir
                                      file line-number nil old-buffer t)
        (setq found t)

        ; XXX we're already scanned the type once, we shouldn't do it again here
        ;     must change function tac-get-type-full-path-candidates to return
        ;     the found type as well
        (setq text (tac-get-type-around-point))
        ; set current file/line into navigation ring
        (a4-nav-ring-add (list save-file save-pos) (tac-revert-colons full-path))
        )

       ((> num-matches 1)
        (cscope-find-global-definition full-path)
        (setq found t))
      ))
    (if (not found)
        (message (concat "Error: Symbol definition for '" tac-type "' not found")))
  ))

;-----------------------------------------------------------------------
; tac- interactive functions

(defun tac-goto-symbol ()
  (interactive)
  (let (tac-type
        full-path-list
        )
    ; get original tac-type under cursor plus list of full-path candidates for it
    (setq tac-type (tac-get-type-around-point))
    (tac-do-find-type-def tac-type)
    ))

(defvar tac-type-history '(""))
(defun tac-goto-symbol-from-prompt ()
  (interactive)
  (let (tac-type)
    (setq tac-type (read-string 
                    "Find TACC symbol: "
                    (tac-get-type-around-point)
                    '(tac-type-history . 1)))
    (tac-do-find-type-def tac-type)
    ))

(defun tac-goto-function-type ()
  (interactive)
  (let (tac-type
        (buf-lang (tac-get-buffer-language))
        )
    (when (string= buf-lang "c++")
      (setq tac-type (tac-get-tac-type-from-cur-function))
      (when tac-type
        (tac-do-find-type-def tac-type)
        )
      )
    ))

(defun tac-show-cur-namespace ()
  (interactive)
  (let (msg
        (cur-namespace (tac-get-cur-namespace)))

    (setq msg "::")
    (when cur-namespace
      (setq msg (concat cur-namespace "::"))

      (put-text-property 0 (length msg) 'face 'tac-namespace-face msg)    

      (momentary-string-display msg (point))

      ;; (message msg)
      )
    ))

;***********************************************************************
; tacnav related functions

(defvar tacnav-browser "firefox")
(defvar tacnav-url "http://bs200:8001/tacnav")
(setq tacnav-url "http://bs200:8001/tacnav")

(defun tacnav-invoke (args)
  (let (url)
    (setq url (concat tacnav-url "?" args))
    (start-process "tacnav" "foo" tacnav-browser url)
    ;; (message url)
    ))

(defun tacnav-go-type (tac-type)
  (interactive)
  (let (full-path-list
        match-list
        (found nil)
        attr
        )
    ; split type / attribute if attribute present

    (setq type-and-attr (tac-split-type-and-attr tac-type))
    (setq tac-type (car type-and-attr))
    (setq attr (nth 1 type-and-attr))

    (when tac-type
      (message (format "Type: %s, attribute: %s" tac-type (if attr attr "<none>")))

      ; get original list of full-path candidates for tac-type
      (setq full-path-list (tac-get-type-full-path-candidates tac-type))

      (while (and full-path-list (not found))
        (setq full-path (car full-path-list)
              full-path-list (cdr full-path-list))
      
        (setq match-list (a4-cscope-internal-find full-path t))

        (when (>= (length match-list) 1)
          (setq found t)
          (tacnav-invoke (concat "classes=" (tac-revert-colons full-path)))
          )
        )
      (if (not found)
          (message (concat "Error: Symbol definition for '" tac-type "' not found")))
      )
    ))

(defun tacnav-go-type-around-cursor ()
  (interactive)
  (let (tac-type
        )
    ; get original tac-type under cursor plus list of full-path candidates for it
    (setq tac-type (tac-get-type-around-point))
    (tacnav-go-type tac-type)
    ))

(defun tacnav-go-cur-func-type ()
  (interactive)
  (let ((tac-type nil)
        (buf-lang (tac-get-buffer-language))
        )
    (cond
     ((string= buf-lang "c++")
      (setq tac-type (tac-get-tac-type-from-cur-function))
      )
     ((string= buf-lang "tac")
      (setq tac-type (tac-get-cur-tac-type))
      )
     )
    (when tac-type
      (tacnav-go-type tac-type)
      )
    ))

(defun tacnav-go-type-from-prompt ()
  (interactive)
  (let (tac-type)
    (setq tac-type (read-string 
                    "Open TacNav for type: "
                    (tac-get-type-around-point)
                    '(tac-type-history . 1)))
    (when tac-type
      (tacnav-go-type tac-type)
      )
    ))

(defun tacnav-go-cur-file ()
  (interactive)
  (let (pkg filename ext
        (tmp (tac-get-buffer-pkg-filename-and-ext))
        )
    (when tmp
      (setq pkg (car tmp))
      (setq filename (nth 1 tmp))
      (setq ext (nth 2 tmp))
      (when (member ext (list "tin" "tac"))
        (setq filename (concat pkg "/" filename ".tac"))
        (tacnav-invoke (concat "targets=" filename))
        )
      )
    ))

;;--- eof ---
