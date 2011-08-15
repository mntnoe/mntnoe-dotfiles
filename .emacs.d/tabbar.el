;; File:     tabbar.el
;; Author:   Mads N Noe <mail (@) madsnoe.dk>
;; License:  as-is

(require 'tabbar)
(require 'cl)
(tabbar-mode 1)

(setf tabbar-background-color    default-bg)
(setf tabbar-buffer-home-button  (quote (("" (:type pbm :data "")) "" (:type pbm :data ""))))
(setf tabbar-home-button         (quote (("" (:type pbm :data "")) "" (:type pbm :data ""))))
(setf tabbar-scroll-left-button  (quote (("" (:type pbm :data "")) "")))
(setf tabbar-scroll-right-button (quote (("" (:type pbm :data "")) "")))
(setf tabbar-separator           (quote ("  ")))
(setf tabbar-use-images          nil)

;(face-spec-set 'tabbar-default `(nil (:background "#00ff00" :foreground ,default-fg :underline nil)) nil)

(custom-theme-set-faces 'user
 `(tabbar-default    ((nil (:background ,default-bg :foreground ,default-fg :underline nil))))
 `(tabbar-highlight  ((t nil)))
 `(tabbar-selected   ((t (:inherit tabbar-default :foreground ,hilight-fg :background ,hilight-bg))))
 `(tabbar-unselected ((t (:inherit tabbar-default))))
 `(tabbar-separator  ((t (:inherit tabbar-default :height 0.5 ))))
 )


(defvar tabbar-hide-emacs-buffers t)
(defun tabbar-toggle-emacs-buffers ()
  "Toggles visibility of emacs-buffers."
  (interactive)
  (setq tabbar-hide-emacs-buffers
        (if tabbar-hide-emacs-buffers
            nil
          t)))

(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to."
  (list
   (cond
;;     ((or (string-equal ".org" (substring (buffer-name) -4 nil))
;;          (string-equal "*Org Agenda*" (buffer-name)))
;;      "org-mode buffers")
    (t
     "all buffers"))))

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups
      tabbar-buffer-list-function 'tabbar-buffer-list)

(defun tabbar-buffer-list ()
  "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space, when they are not
visiting a file.  The current buffer is always included.
Modified to hide emacs buffers too."
  (delq nil
        (mapcar #'(lambda (b)
                    (cond
                     ;; Always include the current buffer.
                     ((eq (current-buffer) b) b)
                     ((string-equal "*Org Agenda*" (buffer-name b)) b)
                     ((string-equal "*Petite Chez Scheme*" (buffer-name b)) b)
                     ((string-equal "TAGS" (buffer-name b)) nil)
                     ((string-equal "gcal-imported-diary" (buffer-name b)) nil)
                     ((buffer-file-name b) b)
                     ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                     ((and tabbar-hide-emacs-buffers
                           (char-equal ?* (aref (buffer-name b) 0)))
                      nil)
                     ((buffer-live-p b) b)))
                (buffer-list))))
