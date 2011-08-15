(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-firefox))
 '(highlight-symbol-idle-delay 1)
 '(initial-scratch-message "")
 '(org-emphasis-alist (quote (("-" bold "<b>" "</b>") ("*" bold "<b>" "</b>") ("/" italic "<i>" "</i>") ("_" underline "<span style=\"text-decoration:underline;\">" "</span>") ("=" org-code "<code>" "</code>" verbatim) ("~" org-verbatim "<code>" "</code>" verbatim) ("+" (:strike-through t) "<del>" "</del>"))))
 '(recentf-exclude (quote ("TAGS" "^/usr/share/emacs/" "^/home/mntnoe/Dropbox/Notes/notes\\.org$")))
 '(recentf-save-file "~/.emacs.d/recentf")
 '(safe-local-variable-values (quote ((TeX-master . "report") (TeX-master . "report.tex") (TeX-master . t)))))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((nil (:foreground "grey50"))))
 '(hl-paren-face ((t (:weight bold))) t)
 '(mode-line ((((class color) (min-colors 88)) (:background "#3c3b37" :foreground "#ffffff"))))
 '(mode-line-highlight ((((class color) (min-colors 88)) nil)))
 '(mode-line-inactive ((default (:inherit mode-line)) (((class color) (min-colors 88) (background light)) (:background "#3c3b37" :foreground "#ffffff" :weight light))))
 '(tabbar-default ((nil (:background "#3c3b37" :foreground "#ffffff" :underline nil))))
 '(tabbar-highlight ((t nil)))
 '(tabbar-selected ((t (:inherit tabbar-default :foreground "#ffffff" :background "#b7ab9b"))))
 '(tabbar-separator ((t (:inherit tabbar-default :height 0.5))))
 '(tabbar-unselected ((t (:inherit tabbar-default)))))

(custom-theme-set-faces 'user
 `(mode-line              ((((class color) (min-colors 88)) (:background ,default-bg :foreground ,default-fg))))
 `(mode-line-highlight    ((((class color) (min-colors 88)) nil)))
 `(mode-line-inactive     ((default (:inherit mode-line)) (((class color) (min-colors 88) (background light)) (:background ,default-bg :foreground ,default-fg :weight light))))
)


;;  '(org-level-1 ((((class color) (min-colors 88) (background light)) (:foreground "Blue1" :height 1.5))))
;;  '(org-level-2 ((((class color) (min-colors 16) (background light)) (:foreground "DarkGoldenrod" :height 1.25))))
;;  '(org-level-3 ((((class color) (min-colors 88) (background light)) (:foreground "Purple" :height 1.125))))
