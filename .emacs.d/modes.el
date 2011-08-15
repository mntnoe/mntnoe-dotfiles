;; File:     modes.el
;; Author:   Mads N Noe <mntnoe (@) gmail.com>
;; License:  as-is

;; ;; PETITE CHEZ SCHEME

;; (add-to-list 'auto-mode-alist '("\\.scm$" . petite-chez-scheme-mode))
;; (autoload 'petite-chez-scheme-mode "petite-chez-scheme-mode-color" () t)
;; (add-hook 'petite-chez-scheme-mode-hook #'(lambda () (highlight-parentheses-mode)))

;; ;; SML/NJ 

(add-to-list 'auto-mode-alist '("\\.sml$" . sml-mode))

(autoload 'sml-mode "sml-mode-color" () t)
(add-hook 'sml-shell-hook 
  #'(lambda ()  
      (send-string sml-process-name
                   (concat "use \"" my-emacs-dir "/sml/setup.sml\";\n"))
      (send-string sml-process-name
                   (concat "use \"" my-emacs-dir "/sml/print.sml\";\n"))))

(defun flymake-sml-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list (concat my-emacs-dir "/sml/sml") (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.sml$\\|\\.sig" flymake-sml-init))

(add-to-list 'flymake-err-line-patterns
             '( "\\(.*\\):\\([0-9]+\\).\\([0-9]+\\).*? \\(.*\\)"
                1 2 3 4))

