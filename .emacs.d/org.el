;; org.el

(org-remember-insinuate)

(setq org-default-notes-file "~/Dropbox/Notes/notes.org"
      org-remember-templates '(("General Note"              ?n "\n* %^{Topic} %^g\n%?%i%&" nil "General")
                               ("Tech Note"                 ?t "\n* %^{Topic} %^g\n%?%i%&" nil "Tech"))
      org-hide-emphasis-markers t
      org-hide-leading-stars t
      org-refile-targets '(nil :maxlevel . 2)
      org-startup-folded 'content)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c n") 'org-add-general-note)
(global-set-key (kbd "C-c t") 'org-add-tech-note)

(add-hook 'org-mode-hook 
          (lambda ()
            (define-key org-mode-map "\M-o" 'show-subtree)
            (define-key org-mode-map "\M-u" 'hide-subtree)))

(defun org-add-general-note ()
  "Add general note."
  (interactive)
  (org-remember nil ?n)
  (viper-insert nil))


(defun org-add-tech-note ()
  "Add tech note."
  (interactive)
  (org-remember nil ?t)
  (viper-insert nil))
