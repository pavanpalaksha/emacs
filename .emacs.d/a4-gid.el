;;----------------------------------------------------------------------
;; Arista gid extensions

;; For detailed description do:
;;
;;    M-x describe-function RET a4-gid RET

;; RFE list:
;; =========
;;
;;  - have the a4-gid-go-back functionality work as a ring
;;  - add encoding scheme to regexes:
;;    - need a way to handle ' in patterns
;;    - add %$ to mean target in the pattern

;;----------------------------------------------------------------------
;; global vars

(provide 'a4-gid)
(require 'a4-nav)

(defvar a4-gid-version "1.1.3" "Current version of the a4-gid packet")

(defvar a4-gid-name "a4-gid")
(defvar a4-gid-command "a4 gid")
(defvar a4-gid-additional-args "" "Additional arguments to the 'a4 gid' command")

(defvar a4-gid-ID-file nil "If non-nil, use this path instad of trying to search backwards in the current path to find the ID file")

(defvar a4-gid-per-target-buffer t
  "If non-nil, open a buffer for each searched target (e.g. *a4-gid <target>* vs. *a4-gid*)")
(setq a4-gid-per-target-buffer t)

(defvar a4-gid-target "")
(defvar a4-gid-target-history '(""))
(defvar a4-gid-packages "")
(defvar a4-gid-prev-packages "")
(defvar a4-gid-packages-history '(""))
(defvar a4-gid-types "")
(defvar a4-gid-prev-types "")
(defvar a4-gid-types-history '(""))
(defvar a4-gid-regexes "")
(defvar a4-gid-prev-regexes "")
(defvar a4-gid-regexes-history '(""))
(defvar a4-gid-cur-cmd "")
(defvar a4-gid-highlight-target nil)
(defvar a4-gid-mixed-mode t)
(defvar a4-gid-search-variations t)

(defvar a4-gid-saved-buffer nil)
(defvar a4-gid-saved-point 0)

(defun a4-gid-toggle-highlight ()
  (interactive)
  (setq a4-gid-highlight-target (not a4-gid-highlight-target))
  (if a4-gid-highlight-target
      (message "a4-gid highlight enabled")
    (message "a4-gid higlight disabled"))
  )

(defun a4-gid-toggle-mixed-mode ()
  (interactive)
  (setq a4-gid-mixed-mode (not a4-gid-mixed-mode))
  (if (a4-gid-disable-mixed-mode-option-supported)
      (if a4-gid-mixed-mode
          (message "a4-gid mixed mode enabled")
        (message "a4-gid mixed mode disabled"))
    (message "ERROR: mixed-mode option not supported by underlying 'a4 gid' command"))
  )

(defun a4-gid-toggle-search-variations ()
  (interactive)
  (setq a4-gid-search-variations (not a4-gid-search-variations))
  (if (a4-gid-disable-search-variation-option-supported)
      (if a4-gid-search-variations
          (message "a4-gid search of tacc-generated variations enabled")
        (message "a4-gid search of tacc-generated variations disabled"))
    (message "ERROR: disabling of search variations option not supported by underlying 'a4 gid' command"))
  )

(setq a4-gid-command-help-output nil)

(defun a4-gid-option-supported (option)
  (let (exit-code tmp-buf ret)
    ;; if we don't have the output of "a4 gid -h" yet, invoke it save the output
    (when (not a4-gid-command-help-output)
      (save-excursion
        (setq ret-list (a4-gid-invoke-cmd (concat a4-gid-command " -h"))))
      (setq exit-code (car ret-list))
      (when (eq exit-code 0)
        (setq a4-gid-command-help-output (nth 1 ret-list))
        )
      (setq tmp-buf (nth 2 ret-list))
      (kill-buffer tmp-buf)
      )
    (when a4-gid-command-help-output
      (setq ret (string-match (format " %s " option)
                              a4-gid-command-help-output)))
    ret
    ))

(defun a4-gid-path-to-id-file-option-supported ()
  (a4-gid-option-supported "-f"))

(defun a4-gid-disable-mixed-mode-option-supported ()
  (a4-gid-option-supported "-M"))

(defun a4-gid-disable-search-variation-option-supported ()
  (a4-gid-option-supported "-V"))

;;; --------------------------------------------------------------------
;;; function to invoke a command on a temporary buffer

(defvar a4-gid-cmd-output-buf-name "*a4-gid-cmd-output*")

(defun a4-gid-invoke-cmd (cmd &optional buf-name output-only)
  (let (tmp-buf exit-code cmd-tokens output)
    (if (not buf-name)
        (setq buf-name a4-gid-cmd-output-buf-name))
    (setq tmp-buf (get-buffer-create buf-name))

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

;;----------------------------------------------------------------------
;; core implementation

(defun a4-gid-internal (defs-mode repeat-params ask-for-regex debug)

  ;; ask for target (get candidate from current cursor position)

  (setq a4-gid-target (read-string 
                     (if defs-mode
                         "Find definitions w/ a4-gid (?=help): "
                       "Run a4-gid (?=help): ")
                     (a4-gid-word-around-point)
                     '(a4-gid-target-history . 1)))

  ;; if '?', just show help

  (if (string= a4-gid-target "?")
      (describe-function 'a4-gid)

    ;; not help, if repeat mode, use previous params

    (if repeat-params
        (progn
          (setq a4-gid-types a4-gid-prev-types)
          (setq a4-gid-packages a4-gid-prev-packages)
          (setq a4-gid-regexes a4-gid-prev-regexes))

      ;; not repeat mode, ask for other parameters; first: file types

      (setq a4-gid-types
            (read-string 
             "File types (aliases: p=py c=C++ t=TAC, -x=exclude x, def=ALL): "
             (if (string= a4-gid-prev-types "") "" (car a4-gid-types-history))
             '(a4-gid-types-history . 1)))
      (setq a4-gid-prev-types a4-gid-types)

      ;; ask for packages

      (setq a4-gid-packages (read-string
                           "Packages (.=current, -x=exclude x, def=ALL): "
                           (if (string= a4-gid-prev-packages "") 
                               ""
                             (car a4-gid-packages-history))
                           '(a4-gid-packages-history . 1)))
      (setq a4-gid-prev-packages a4-gid-packages)

      ;; if needed, ask for regexes

      (if (not ask-for-regex)
          (setq a4-gid-regexes "")
        (setq a4-gid-regexes (read-string
                            "Regexes (-re=exclude re, def=none): "
                            (if (string= a4-gid-prev-regexes "") 
                                ""
                              (car a4-gid-regexes-history))
                            '(a4-gid-regexes-history . 1)))
        (setq a4-gid-prev-regexes a4-gid-regexes))
      )

    (let (compile-command
          (compilation-error-regexp-alist grep-regexp-alist)
          (compilation-buffer-name-function
           '(lambda (mode) (if a4-gid-per-target-buffer
                               (format "*%s %s*" a4-gid-name a4-gid-target)
                             (format "*%s*" a4-gid-name))
              ))
          )

      (progn
        ; add current location to the navigation ring

        (a4-nav-ring-add (list buffer-file-name (point)) a4-gid-target)

        ; build command to execute it

        (setq a4-gid-saved-buffer (current-buffer))
        (setq a4-gid-saved-point (point))
        (setq a4-gid-cur-cmd a4-gid-command)
        (if debug
            (setq a4-gid-cur-cmd (concat a4-gid-cur-cmd " -v0")))
        (if defs-mode
            (setq a4-gid-cur-cmd (concat a4-gid-cur-cmd " -D")))
        (if a4-gid-highlight-target
            (setq a4-gid-cur-cmd (concat a4-gid-cur-cmd " -c")))

        ; pass path to ID option if asked to and supported
        (if (and a4-gid-ID-file (a4-gid-path-to-id-file-option-supported))
            (setq a4-gid-cur-cmd (concat a4-gid-cur-cmd " -f " a4-gid-ID-file)))

        ; disable mixed mode if asked to and supported
        (if (and (not a4-gid-mixed-mode)
                 (a4-gid-disable-mixed-mode-option-supported))
            (setq a4-gid-cur-cmd (concat a4-gid-cur-cmd " -M")))

        ; disable search for variations if asked to and supported
        (if (and (not a4-gid-search-variations)
                 (a4-gid-disable-search-variation-option-supported))
            (setq a4-gid-cur-cmd (concat a4-gid-cur-cmd " -V")))

        (setq a4-gid-cur-cmd (concat a4-gid-cur-cmd
                          " -t '" a4-gid-types 
                          "' -p '" a4-gid-packages
                          "' -r '" a4-gid-regexes
                          "' -g '" a4-gid-target
                          "' "
                          a4-gid-additional-args))
        (grep a4-gid-cur-cmd)
        ))))

;;----------------------------------------------------------------------
;; kill all *a4-gid* buffers

(defun a4-gid-kill ()
"Kill all *a4-gid* buffers.

For detailed description do: M-x describe-function RET a4-gid RET
"
  (interactive)
  (let ((bufs (mapcar (function buffer-name) (buffer-list)))
        (bufname))
    (while bufs
      (setq bufname (car bufs))
      (if (string-match a4-gid-name bufname)
          (kill-buffer bufname))
      (setq bufs (cdr bufs))
      )))

;;----------------------------------------------------------------------
;; main interactive function

(defun a4-gid-help ()
  "Display a4-gid help."
  (interactive)
  (describe-function 'a4-gid))

(defun a4-gid ()
"Arista extended gid.

This description covers the following commands:

  M-x a4-gid
  M-x a4-gid-defs

Their \"repeat\" variations:

  M-x a4-gid-repeat
  M-x a4-gid-defs-repeat

And helper commands like:

  M-x a4-gid-kill
  M-x a4-gid-toggle-highlight

These commands extend the functionality of the regular gid command in the
following manner:

  - Allows for filtering the gid output based on:
    - file types
    - packages
    - regular expressions

  - Search for definitions only (a4-gid-defs)

    NOTICE: the search definition mode (`a4-gid-defs') uses a \"best-effort\"
            approach to determine what a definition is, as it ultimately 
            relies on the gid command, which has no real understanding of 
            the languages syntaxes themselves.

  - Searches for \"Tacc-generated Variations\". For example,
    when looking for a <symbol>, it also looks for:
    - In C++ files:
      - <symbol>Is
      - <symbol>Del
      - <symbol>Iterator[Const]
      - do<Symbol>
      - <symbols>s
    - In Python files:
      - new<Symbol>
      - add<Symbol>
    - etc.

  - Output:
    - organized by packages, files, and listed in ascending line order per file
    - supports optional highlighting of target symbol

  - Navigation Ring

Repeat variations:

   The \"repeat\" variations (M-x a4-gid-repeat / M-x a4-gid-defs-repeat)
   remember the parameters (file types, packages and regexes parameters)
   from your previous search and don't ask for them again.

Navigation Ring:

   The originating location of each search is automatically added to a
   navigation ring, which allows for easy navigation accross locations of
   interest. 

   Notice that it is the originating location that is added to the ring,
   NOT the ones visited from the output.

   The navigation ring itself is independent of the a4-gid command and support
   several features, like the ability to delete individual locations, display
   / reset all ring contents, etc. 

   For details about the navigation ring, please use the following command:

      M-x `a4-nav-ring-help'

Killing all a4-gid buffers:

   The `a4-gid-kill' command kills all buffers generated by a4-gid commands
   (named *a4-gid...*).

Target Highlighting:

   The `a4-gid-toggle-highlight' enables/disables the highlighting of the 
   target symbol in the output. More details below.


User Interface
==============

Just like the gid command, `a4-gid' commands automatically select the word
closest to the cursor and also keeps track of the history of searched words.

In addition to word you're looking for, a4-gid then asks for 3 additional
parameters (described in detail in the sections below):
  - List of file types to include/exclude
  - List of packages to include/exclude
  - List of regular expressions to apply on the found text

 File types(s):
 --------------

  You should enter a list of space separated file types using the following
  syntax:

     [-]<type1>[ [-]<type2>...]

  You can enter a list of file extensions (. is optional), and it also
  accepts the following aliases, which are internally expanded to the
  corresponding list of file types:

     c => cpp h c C tin itin 
     p => py 
     t => tac 
     C => c C h 

  If preceded by '-', files of this type are excluded. If you only enter
  excluded file types, it'll show everything else.

  Dot in individual file types is optional.

  Default (i.e. no input): shows results for all file types

  Examples:

     Input      Semantics
     --------------------------------------------------------------
     c          Show only results for .cpp/.h/.c/.C/.tin/.itin files

     t p        Show only results for .py and .tac files

     -p         Show all results except those for .py files

     c -.C      Show all C/C++ files except for .C ones

     <nothing>  Show results for all file types

  When you try the command again, it'll remember your previous selection
  and keep a history of it.

 Package(s):
 -----------

  Syntax is similar to file types:

     [-]<pkg1>[ [-]<pkg2>...]

  Alias:

     . => current package
 
  This is all very similar to the file types, i.e. '-' means 'exclude the
  package'; it'll remember your last selection and keep a history of it,
  default is \"all packages\".

  Default: all packages

  Examples:

     Input            Semantics
     --------------------------------------------------------------
     .                Show only results for files in the current 
                      package
    
     . Arnet Sand     Show only results for files in the current package,
                      Arnet and Sand

     -EthIntf -Arnet  Show all results except those in EthIntf and Arnet

     <nothing>        Show results for all packages

 Regular Expression(s):
 ----------------------

  Syntax is similar to file types and packages:

     [-]<regex1>[ [-]<regex2>...]

  This is all very similar to the file types, i.e. '-' means 'exclude the
  the lines matching the regular expression'; it'll remember your last 
  selection and keep a history of it.

  Space is the regex separator here, so to have a space in your regular
  expression you need to use \s

  Default: no regular expressions.

ID Database
===========

By default, gid searches the id database (ID file) backwards from the
current directory (and fails if it can't find it). This can be annoying
when the a4-gid-* commands are run from a buffer in directory that's not
under where the database is (typically '/src').

You can override this setting the the following variable in your emacs
init file:

  (setq a4-gid-ID-file \"/src/ID\")

Tacc-generated Variations
=========================

By default, a4-gid searches for tacc-generated variations (e.g <symbol>Is/etc.).
(See 'Mixed Mode' section below to see the different ways of displaying their
results).

To disable search for these variations, add the following line to your init file:

  (setq a4-gid-search-variations nil)

You can also enable/disable this through the following command:

  M-x `a4-gid-toggle-search-variations'

Mixed Mode
==========

By default, search results for the tacc-generated variations (e.g <symbol>Is/etc.)
are shown together with the results for the <symbol> itself. This is called
\"mixed mode\".

When a4-gid's \"mixed mode\" is disable, results for the variations are shown
separately, after the results for the symbol itself.

To disable mixed mode by default, add the following line to your init file:

  (setq a4-gid-mixed-mode nil)

You can also enable/disable mixed mode through the following command:

  M-x `a4-gid-toggle-mixed-mode'

Target Buffers
==============

By default, the results of the a4-gid-* commands are displayed in a target
specific buffer. For example, when searching for symbol 'foo', the results
are displayed in a buffer named \"*a4-diff foo*\". This means that several
buffers are created (which can all be killed at once through the
`a4-gid-kill' command).

It's possible to have a single target buffer, named \"*a4-gid*\" (rewritten
at every search). This is useful for example for making the a4-gid
results buffer \"sticky\" (`sticky-buffer-mode').

To do that, set the following variable in your emacs init file:

  (setq a4-gid-per-target-buffer t)

You can toggle the single/per-target results mode the following command:

  M-x `a4-gid-toggle-per-target-buffer'

Results Navigation
==================

The results can be navigated through the following commands:

  M-x next-error (open buffer/location of next result in the a4-gid output)
  M-x previous-error (open buffer/location of previous result in the a4-gid output)

The following command sends you back to the original buffer/position from
where you invoked the a4-gid command:

  M-x a4-gid-go-back

Suggested Keyboard Bindings
===========================

  M-[      => a4-gid-defs
  M-]      => a4-gid
  M-return => a4-gid-repeat
  M-'      => a4-gid-go-back
  M-;      => a4-gid-kill

  M-down   => next-error
  M-up     => previous-error

The following commands in your emacs init file (e.g. ~/.emacs.el) will do
the bindings above:

  (global-set-key (kbd \"M-[\") (quote a4-gid-defs))
  (global-set-key (kbd \"M-]\") (quote a4-gid))
  (global-set-key (quote [M-return]) (quote a4-gid-repeat))
  (global-set-key (kbd \"M-'\") (quote a4-gid-go-back))
  (global-set-key (kbd \"M-;\") (quote a4-gid-kill))

  (global-set-key (quote [M-up]) (quote previous-error))
  (global-set-key (quote [M-down]) (quote next-error))

Target Highlighting
===================

It is possible to highlight the target symbol in the output (similar to what grep does). 

Highlighting is disabled by default. To enable it at init time you should have the following command at your emacs init file:

  (setq a4-gid-highlight-target t)

You can also enabled/disable highlighting through the following command:

   M-x `a4-gid-toggle-highlight'

The highlight text is displayed through the \"match\" font, which can be customized through the `set-face-attribute' command. For example, to change the background color of the font to 'light blue', you should have the following command in your emacs init file:

  (set-face-attribute 'match nil :background \"light blue\")

Troubleshooting
===============

- The a4-gid commands rely on the 'a4 gid' script, so the 'a4' command must be
  executable as well as part of your $PATH.

- ID file is expected to be generated through the 'a4 mkid' command at
  the [A4_CHROOT]/src directory.

- Commands `a4-gid-dbg' / `a4-gid-defs-dbg' run the the 'a4 gid' script with
  debug messages enabled  
"
  (interactive)
  (a4-gid-internal nil nil t nil))



(defun a4-gid-repeat ()
"Execute a4-gid command w/ previous parameters.

For detailed description do: M-x describe-function RET a4-gid RET
"
  (interactive)
  (a4-gid-internal nil t t nil)
  )

;;----------------------------------------------------------------------
;; search defs version

(defun a4-gid-defs ()
"Search for definitions through using Arista extended gid.

For detailed description do: M-x describe-function RET a4-gid RET
"
  (interactive)
  (a4-gid-internal t nil t nil))

(defun a4-gid-defs-repeat ()
"Search for definitions through using Arista extended gid w/ previous parameters.

For detailed description do: M-x describe-function RET a4-gid RET
"
  (interactive)
  (a4-gid-internal t t nil nil))

(defun a4-gid-toggle-per-target-buffer ()
  (interactive)
  (setq a4-gid-per-target-buffer (not a4-gid-per-target-buffer)))

;;----------------------------------------------------------------------
;; debug versions

(defun a4-gid-dbg ()
"Run a4-gid in debug mode."
  (interactive)
  (a4-gid-internal nil nil t t))

(defun a4-gid-defs-dbg ()
  (interactive)
  (a4-gid-internal t nil nil t))

;;----------------------------------------------------------------------
;; go-back function

(defun a4-gid-go-back ()
"Go back to buffer/position from where the last a4-gid command was invoked."
  (interactive)
  (when a4-gid-saved-buffer
      (switch-to-buffer a4-gid-saved-buffer)
      (goto-char a4-gid-saved-point)))

;;----------------------------------------------------------------------
;; helper functions

(defun a4-gid-word-around-point ()
  "Return the word around the point as a string."
  (save-excursion
    (if (not (eobp))
 	(forward-char 1))
    (forward-word -1)
    (forward-word 1)
    (forward-sexp -1)
    (buffer-substring (point) (progn
 				(forward-sexp 1)
 				(point)))))


