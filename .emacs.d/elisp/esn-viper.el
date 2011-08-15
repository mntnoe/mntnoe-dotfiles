;; Originally by Matt F. Modified by Mads N Noe 

(defgroup esn-viper nil
  "* Options for Viper Color theme."
  )
(defvar esn-viper-last-mode 0
  "* Defines the last mode for Viper")
(defcustom esn-viper-vi-command "(progn (set-face-foreground 'modeline \"grey20\") (set-face-background 'modeline \"grey60\"))"
  "* Defines the command used for VI mode."
  :type 'string
  :group 'esn-viper
  )
(defcustom esn-viper-vi-visual-command "(progn (set-face-foreground 'modeline \"grey20\") (set-face-background 'modeline \"grey60\"))"
  "* Defines the command used for VI visual mode."
  :type 'string
  :group 'esn-viper
  )
(defcustom esn-viper-insert-command "(progn (set-face-foreground 'modeline \"grey60\") (set-face-background 'modeline \"grey20\"))"
  "* Defines the command used for Insert mode."
  :type 'string
  :group 'esn-viper 
  )
(defcustom esn-viper-emacs-command "(progn (set-face-foreground 'modeline \"black\") (set-face-background 'modeline \"grey75\"))"
  "* Defines the command used for Emacs mode."
  :type 'string
  :group 'esn-viper
  )
(defun esn-viper-pre-command-hook ()
  (interactive)
  (let ((deactivate-mark nil) (esn-keybinding 2) )
    (when (not (minibufferp))
      (when (and (= 2 esn-keybinding) (boundp 'viper-current-state) viper-current-state)
	(when (and (not (= 1 esn-viper-last-mode))
		   (eq viper-current-state 'emacs-state)
		   )
	  ;; Emacs
	  (setq esn-viper-last-mode 1)
	  (with-temp-buffer
	    (insert esn-viper-emacs-command)
	    (eval-buffer)
	    )
	  )
	(when (and (not (= 2 esn-viper-last-mode))
		   (eq viper-current-state 'insert-state)
		   )
	  ;; Insert
	  (setq esn-viper-last-mode 2)
	  (with-temp-buffer
	    (insert esn-viper-insert-command)
	    (eval-buffer)
	    )
	  )
	(when (and (not (= 3 esn-viper-last-mode))
		   (eq viper-current-state 'vi-state)
		   (or (not (boundp 'viper-visual-mode))
		       (not viper-visual-mode))
		   )
	  ;; Vi Normal
	  (setq esn-viper-last-mode 3)
	  (with-temp-buffer
	    (insert esn-viper-vi-command)
	    (eval-buffer)
	    )
	  )
	(when (and (not (= 4 esn-viper-last-mode))
		   (eq viper-current-state 'vi-state)
		   (boundp 'viper-visual-mode)
		   viper-visual-mode)
					; Vi visual
	  (setq esn-viper-last-mode 4)
	  (with-temp-buffer
	    (insert esn-viper-vi-visual-command)
	    (eval-buffer)
	    )
	  )
	)
      )
    ))
(add-hook 'pre-command-hook 'esn-viper-pre-command-hook)
(add-hook 'post-command-hook 'esn-viper-pre-command-hook)

(provide 'esn-viper)
