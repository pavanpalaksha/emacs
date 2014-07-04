;;======================================================================
;; navigation ring

;; logic here provides a "navigation ring" that allows files/position
;; pairs to be "bookmarked"
;;
;; For more details, pls see comments in function a4-nav-ring-help below

(provide 'a4-nav)

(defvar a4-nav-ring ())
(defvar a4-nav-ring-text ())
(defvar a4-nav-ring-idx -1)
(defvar a4-nav-ring-allow-wrap-p t)
(defvar a4-nav-ring-go-back-idx nil)
(defvar a4-nav-ring-buffer-name "*navigation-ring*")

(setq a4-nav-ring-allow-wrap-p nil)

;;----------------------------------------------------------------------
;; low level functions

(defun a4-nav-ring-toggle-wrap-around()
  (interactive)
  (setq a4-nav-ring-allow-wrap-p (not a4-nav-ring-allow-wrap-p))
  )

(defun a4-nav-ring-next-idx ()
  (let (new-idx)
    (if (= 0 (length a4-nav-ring))
        (message "Navigation ring is empty")
      (setq a4-nav-ring-go-back-idx nil)
      (setq new-idx (1+ a4-nav-ring-idx))
      (if (< new-idx (length a4-nav-ring))
          (setq a4-nav-ring-idx new-idx)
        (if a4-nav-ring-allow-wrap-p
            (setq a4-nav-ring-idx 0)
          (message "Moved past last entry in navigation ring")
          )
        )
      )
    ))

(defun a4-nav-ring-prev-idx ()
  (let (new-idx)
    (if (= 0 (length a4-nav-ring))
        (message "Navigation ring is empty")

      ;; if "go back" index set, just go for it

      (if a4-nav-ring-go-back-idx
          (progn
            (setq a4-nav-ring-idx a4-nav-ring-go-back-idx)
            (setq a4-nav-ring-go-back-idx nil)
            )

        ;; no "go back" index, go to previous one (if allowed)

        (setq new-idx (1- a4-nav-ring-idx))
        (if (>= new-idx 0)
            (setq a4-nav-ring-idx new-idx)
          (if a4-nav-ring-allow-wrap-p
              (setq a4-nav-ring-idx (1- (length a4-nav-ring)))
            (message "Moved back before first entry in navigation ring")
            )
          )
        )
      )
    ))

(defun a4-nav-goto-file-at-pos (file pos)
  (let (new-buffer)
    (unless (file-readable-p file)
      (error "File '%s' is not readable or exists" file))

    (setq new-buffer (find-file-noselect file t))
    (set-buffer new-buffer)
    (goto-char pos)
    (switch-to-buffer new-buffer)
    ))

(defun a4-nav-ring-goto-loc-at-idx (idx)
  (let (entry)
    (when (<= idx (length a4-nav-ring))
      (setq entry (nth idx a4-nav-ring))
      (a4-nav-goto-file-at-pos (car entry) (nth 1 entry))
      (setq a4-nav-ring-idx idx)
      (a4-nav-ring-update-list-if-visible)
      )
  ))

(defun a4-nav-index-in-list (elm lst)
  (let ((mbr (member elm lst))
        )
    (if mbr
        (- (length lst) (length (member elm lst)))
      nil)
    ))

(defun a4-nav-ring-add (loc text)
  (let ((file (car loc))
        (pos (nth 1 loc))
        cur-idx
        )
    (if (not file)
        (message "Cannot add file-less buffers to navigation ring")

      (setq cur-idx (a4-nav-index-in-list loc a4-nav-ring))

      (if cur-idx

          ;; if already in list, just add cur. index as the "go back" one

          (setq a4-nav-ring-go-back-idx cur-idx)

        ;; not in list yet, add it and adjust new index

        (setq a4-nav-ring (append a4-nav-ring (list loc)))
        (setq a4-nav-ring-text (append a4-nav-ring-text (list text)))

        (if a4-nav-ring-allow-wrap-p
            (setq a4-nav-ring-idx 0)
          (setq a4-nav-ring-idx (1- (length a4-nav-ring)))
          )

          ;; added new location, mark newly added index as the "go back" one

        (setq a4-nav-ring-go-back-idx (1- (length a4-nav-ring)))

        (message (format "%s:%d(%s) added to navigation ring" file pos text))
        (a4-nav-ring-update-list-if-visible)
        )
      )
    ))

(defun a4-nav-delete-nth (n list) 
"Delete n-th element of a list"
  (if (zerop n) 
    (cdr list) 
    (let ((cons (nthcdr (1- n) list))) 
      (if cons 
        (setf (cdr cons) (cddr cons)) 
        cons))))

(defun a4-nav-ring-del (loc)
  (let (idx
        (file (car loc))
        (pos (nth 1 loc))
        text
        )
    (setq a4-nav-ring-go-back-idx nil)
    (if (member loc a4-nav-ring)
        (progn
          (setq idx (a4-nav-index-in-list loc a4-nav-ring))
          (setq text (nth idx a4-nav-ring-text))
          (setq a4-nav-ring (delete loc a4-nav-ring))
          (a4-nav-delete-nth idx a4-nav-ring-text)
          ;; if deleted element was the current index, move index backwards
          ;; reset index if ring is now empty
          (if a4-nav-ring
              (if (= idx a4-nav-ring-idx) (a4-nav-ring-prev-idx))
            (setq a4-nav-ring-idx -1))
          (message (format "%s:%d(%s) deleted from navigation ring" file pos text))
          (a4-nav-ring-update-list-if-visible)
          )
      (message (format "Error: %s:%d not in navigation ring" file pos))
      )
    ))

(defun a4-nav-word-around-point ()
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

(defun a4-nav-ring-update-list-if-visible ()
  (if (get-buffer a4-nav-ring-buffer-name)
      (if (get-buffer-window-list a4-nav-ring-buffer-name)
          (a4-nav-ring-show))
    )
  )

;;----------------------------------------------------------------------
;; interactive functions

(defun a4-nav-ring-reset ()
"Reset navigation ring."
  (interactive)
  (setq a4-nav-ring ())
  (setq a4-nav-ring-text ())
  (setq a4-nav-ring-idx -1)
  ;; if nav-ring list is visible, re-display it
  (a4-nav-ring-update-list-if-visible)
  (message "Navigation ring reset")
  )

(defun a4-nav-add-to-nav-ring ()
"Add current location to navigation ring."
  (interactive)
  (let ((text (a4-nav-word-around-point))
        )
    (a4-nav-ring-add (list buffer-file-name (point)) text)
    ))

(defun a4-nav-del-from-nav-ring ()
"Delete current location from navigation ring."
  (interactive)
  (a4-nav-ring-del (list buffer-file-name (point)))
  )
  
(defun a4-nav-ring-next ()
"Go to next location in navigation ring."
  (interactive)
  (let ()
    (when (> (length a4-nav-ring) 0)
      (a4-nav-ring-next-idx)
      (a4-nav-ring-goto-loc-at-idx a4-nav-ring-idx)
      )
    ))

(defun a4-nav-ring-prev ()
"Go to previous location in navigation ring."
  (interactive)
  (let ((cur-len (length a4-nav-ring))
        )
    (when (> cur-len 0)
      (a4-nav-ring-prev-idx)
      (a4-nav-ring-goto-loc-at-idx a4-nav-ring-idx)
      )
    ))

(defun a4-nav-ring-show ()
"Show navigation ring."
  (interactive)
  (let (ring idx max loc text
        (buf (get-buffer-create a4-nav-ring-buffer-name))
        )
    (set-buffer buf)
    (setq view-read-only nil)
    (delete-region (point-min) (point-max))
    (goto-char (point-min))
    (insert "Navigation Ring:\n\n")
    (setq ring a4-nav-ring
          idx 0
          max (length a4-nav-ring))
    (if (> max 0)
        (while (< idx max)
          (setq loc (nth idx a4-nav-ring))
          (setq text (nth idx a4-nav-ring-text))
          (insert (format "%s%-40s %-5d (%s)\n"
                          (if (= idx a4-nav-ring-idx) "*" " ")
                          (car loc)
                          (nth 1 loc)
                          text))
          (setq idx (1+ idx))
          )
      (insert "<Empty>"))
    (setq view-read-only t)
    (when (not (get-buffer-window-list a4-nav-ring-buffer-name))
      (switch-to-buffer buf)
      (setq view-read-only t))
    )
  )

(defvar a4-nav-ring-killable-ftypes
  '(".c" ".C" ".tin" ".cpp" ".h" ".itin" ".tac" ".py"))

(defun a4-nav-ring-kill-other-buffers ()
"Kill all source code buffers except those in the navigation ring."
  (interactive)
  (let ((my-buf-list (buffer-list))
        (nav-ring-files (mapcar 'car a4-nav-ring))
        ftype 
        )
    (save-excursion
      (while my-buf-list
        (setq buf (car my-buf-list)
              my-buf-list (cdr my-buf-list))
        (set-buffer buf)
        ;; to kill the buffer it must:
        ;;   - have a file name
        ;;   - have a file extension that's part of the killable list
        ;;   - not be in the navigation ring
        (when (and buffer-file-name
                   (string-match "\\(\\..+\\)$" buffer-file-name))
          (setq ftype (match-string 1 buffer-file-name))
          (when (and (member ftype a4-nav-ring-killable-ftypes)
                     (not (member buffer-file-name nav-ring-files)))
            (kill-buffer buf)
            )
          )
        )
      ;; if *Buffer List* is visible, re-display it
      (if (get-buffer-window-list "*Buffer List*")
        (list-buffers))
      )))

(defun a4-nav-ring-help ()
"Display navigation ring help.

As its name implies, the 'navigation ring' allows navigation through
bookmarked locations, through functions `a4-nav-ring-next' and `a4-nav-ring-prev' 
(please see suggested key-mappings bellow).

Locations can be added/deleted to/from the ring through either interactive
functions (`a4-nav-add-to-nav-ring', `a4-nav-del-from-nav-ring')
or programmatically by other packages (e.g. a4-gid) through lower level functions
(`a4-nav-ring-add', `a4-nav-ring-del').

Function Reference
==================

Here's a summary of all supported navigation ring functions organized
by category:

  - Adding/removing locations:

    - `a4-nav-add-to-nav-ring' (interactive function)
    - `a4-nav-del-from-nav-ring' (interactive function)
    - `a4-nav-ring-add' (low level function)
    - `a4-nav-ring-del' (low level function)

  - Navigation:

    - `a4-nav-ring-next' - go to next location
    - `a4-nav-ring-prev' - go to previous location

  - Showing ring contents:

    - `a4-nav-ring-show' - shows contents of navigation ring on a separate buffer

  - Resetting ring contents:

    - `a4-nav-ring-reset' - cleans all locations

  - Killing buffers:

    - `a4-nav-ring-kill-other-buffers' - kill all source code buffers, except
                                         for those in the navigation ring

  - Changing behavior:

    - `a4-nav-ring-toggle-wrap-around' - by default, navingating through
      the ring does NOT wrap around when one tries to goes past the first/last
      location. This function, toggles this \"wrap around\" behavior.

Suggested Keyboard Bindings
===========================

  M-=       => `a4-nav-add-to-nav-ring'
  M--       => `a4-nav-del-from-nav-ring'

  M-right   => `a4-nav-ring-next'
  M-left    => `a4-nav-ring-prev'

  C-c M-l   => `a4-nav-ring-list'
  C-c M--   => `a4-nav-ring-reset'
  C-c M-k   => `a4-nav-ring-kill-other-buffers'

The following commands in your emacs init file (e.g. ~/.emacs.el) will do
the bindings above:

  (global-set-key (kbd \"M-=\") (quote a4-nav-add-to-nav-ring))
  (global-set-key (kbd \"M--\") (quote a4-nav-del-from-nav-ring))

  (global-set-key (quote [M-right]) (quote a4-nav-ring-next))
  (global-set-key (quote [M-left]) (quote a4-nav-ring-prev))

  (global-set-key (kbd \"C-x M--\") (quote a4-nav-ring-reset))
  (global-set-key (kbd \"C-x M-l\") (quote a4-nav-ring-show))
  (global-set-key (kbd \"C-x M-k\") (quote a4-nav-ring-kill-other-buffers))
"
  (interactive)
  (describe-function 'a4-nav-ring-help)
)

;;======================================================================
;; imenu navigation

(require 'imenu)

(defun a4-nav-imenu-cur-idx ()
  (let ((idx 0)
        (found-idx nil)
        (first-pair nil)
        alist offset pair mark last-valid-idx)

  (when (and (boundp 'imenu--index-alist)
             (null imenu--index-alist))
    (imenu--make-index-alist t))

  (when imenu--index-alist
    (setq alist imenu--index-alist)
    (while (and alist (not found-idx))

      (setq pair (car-safe alist)
            alist (cdr-safe alist))

      ;; Elements of alist are either ("name" . marker), or
      ;; ("submenu" ("name" . marker) ... ). The list can be
      ;; arbitrarily nested.
      (cond ((atom pair))     ; skip anything not a cons

            ((imenu--subalist-p pair)) ; skip submenu

            ((number-or-marker-p (setq mark (cdr pair)))
             (when (not first-pair)
               (setq first-pair pair))
             (setq last-valid-idx idx)
             (when (> mark (point))
               (if (= idx 0)
                   (setq alist nil)
                 (setq found-idx (1- idx))
                 )
               ))
            )
      (setq idx (1+ idx))
      )

    ; if there was a valid entry, but didn't find the index, it could be
    ; two different things:
    ;   - before first item
    ;   - after last item
    ; logic below determines which one it is
    (when (and first-pair (not found-idx))
      (if (< (point) (cdr pair))
          (setq found-idx -1)
        (setq found-idx last-valid-idx)))
    )
  found-idx
  ))

(defun a4-nav-imenu-goto-idx (idx)
  (goto-char (cdr (nth idx imenu--index-alist)))
  ; return name of the item
  (car (nth idx imenu--index-alist))
  )

(defun a4-nav-imenu-prev ()
  (interactive)
  (let ((idx (a4-nav-imenu-cur-idx))
        pair)
    (when (and idx (>= idx 0))
      (setq pair (nth idx imenu--index-alist))
      (setq mark (cdr pair))

      ; if right on the beginning of the item, move to the previous one

      (when (and (= (point) mark) (> idx 0))
        (setq idx (1- idx)))

      (a4-nav-imenu-goto-idx idx)
      )
    ))

(defun a4-nav-imenu-next ()
  (interactive)
  (let ((idx (a4-nav-imenu-cur-idx))
        pair)
    (when idx
      (if (= idx -1)
          (a4-nav-imenu-goto-idx 0)
        (when (< idx (1- (length imenu--index-alist)))
          (a4-nav-imenu-goto-idx (1+ idx)))
        )
      )
    ))
