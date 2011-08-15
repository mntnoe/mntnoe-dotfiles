;; File:     viper.el
;; Author:   Mads N Noe <mail (@) madsnoe.dk>
;; License:  as-is
;;
;; Put the following in your init.el to auto-start viper.
;; (setq viper-mode t)
;; (require 'viper)

;;
;; SETTINGS 
;;

(setq viper-inhibit-startup-message 't
      viper-expert-level '5             ; Get rid of irritating bindings and configuration 
                                        ; limitations.

      viper-ex-style-editing nil        ; Allow to wrap around lines when deleting text.
      viper-ex-style-motion t           ; But don't allow it when navigating with hjkl.
                                        ; See viper-forward-char and viper-goto-eol though.

      viper-want-ctl-h-help t           ; Re-enable online help!

      viper-insert-state-cursor-color    "blue"
      viper-replace-overlay-cursor-color "black"

      viper-shift-width 4

      viper-ESC-keyseq-timeout 10)      ; Faster response

;; Faces

(set-face-background 'viper-minibuffer-insert nil) ; I do not use modal editing in the minibuffer, 
                                                   ; so minibuffer colors are not necessary.
(if (not window-system)
    (set-face-foreground 'viper-minibuffer-insert "white")
  (set-face-foreground 'viper-minibuffer-insert "black"))


;; 
;; MAPPINGS
;; 

(define-key viper-vi-global-user-map        "u" 'undo) ; from redo.el
(define-key viper-vi-global-user-map     "\C-r" 'redo)

;; Editing
(define-key viper-vi-global-user-map        "n" 'viper-Append)
(define-key viper-insert-global-user-map "\C-d" 'delete-char)
(define-key viper-vi-global-user-map     "\M-j" 'viper-scroll-screen)
(define-key viper-vi-global-user-map     "\M-k" 'viper-scroll-screen-back)
(define-key viper-insert-global-user-map "\M-j" 'viper-scroll-up-one)
(define-key viper-insert-global-user-map "\M-k" 'viper-scroll-down-one)
(define-key viper-insert-global-user-map "\M-o" 'viper-insert-open-line-below)

(define-key viper-insert-global-user-map "\C-\M-h" 'viper-delete-backward-word)
(define-key viper-vi-global-user-map     "\C-\M-h" 'viper-delete-backward-word)

(define-key viper-vi-global-user-map        "`" 'viper-toggle-case)

(define-key viper-vi-global-user-map "\M-s\M--" 'my-bold-word)
(define-key viper-vi-global-user-map "\M-s\M-/" 'my-italic-word)

;; Navigation
(define-key viper-vi-global-user-map     "\C-v" 'quoted-insert)
(define-key viper-insert-global-user-map "\C-v" 'quoted-insert)
(define-key viper-vi-global-user-map        "g" 'beginning-of-buffer)
(define-key viper-vi-global-user-map        "G" 'end-of-buffer)
(define-key viper-vi-global-user-map        "#" 'highlight-symbol-prev)
(define-key viper-vi-global-user-map        "*" 'highlight-symbol-next)
(define-key viper-vi-global-user-map     "\C-t" 'pop-tag-mark)
(define-key viper-vi-global-user-map        "[" 'backward-page)
(define-key viper-vi-global-user-map        "]" 'forward-page)


;; Clipboard
(define-key viper-insert-global-user-map "\C-w" 'kill-region)
(define-key viper-vi-global-user-map     "\C-y" 'yank)
(viper-record-kbd-macro                     "Y" 'vi-state [y $] 't)
(define-key viper-vi-global-user-map        "v" 'set-mark-command)

;; Misc
(define-key viper-vi-global-user-map        " " 'execute-extended-command)
(define-key minibuffer-local-map         "\M- " 'viper-exit-insert-state)

;; Emacs compatibility
(define-key viper-vi-global-user-map     "\C-u" 'universal-argument)
(define-key viper-insert-global-user-map "\C-u" 'universal-argument)
(define-key viper-vi-global-user-map     "\C-e" 'move-end-of-line)
(define-key viper-insert-global-user-map "\C-e" 'mode-end-of-line)

;; History
(define-key viper-vi-global-user-map        "u" 'undo)
(define-key viper-vi-global-user-map        "U" 'redo)

;; Isearch
(define-key viper-vi-global-user-map        "/" 'isearch-forward-regexp)
(define-key viper-vi-global-user-map        "?" 'isearch-backward-regexp)
(define-key viper-vi-global-user-map        "-" 'isearch-repeat-forward)
(define-key viper-vi-global-user-map        "_" 'isearch-repeat-backward)

(define-key recentf-dialog-mode-map         "/" 'isearch-forward-regexp)
(define-key recentf-dialog-mode-map         "?" 'isearch-backward-regexp)
(define-key recentf-dialog-mode-map         "-" 'isearch-repeat-forward)
(define-key recentf-dialog-mode-map         "_" 'isearch-repeat-backward)
(define-key recentf-dialog-mode-map         "n" 'viper-next-line)
(define-key recentf-dialog-mode-map         "e" 'viper-previous-line)

(define-key isearch-mode-map                "/" 'isearch-repeat-forward)
(define-key isearch-mode-map                "?" 'isearch-repeat-backward)
(define-key isearch-mode-map        [backspace] 'isearch-del-char)
(define-key isearch-mode-map                "	" 'isearch-yank-word) ; tab
(define-key isearch-mode-map             "\C-v" 'isearch-quote-char)
(if window-system
    (define-key isearch-mode-map       [escape] 'viper-intercept-ESC-key-isearch)
  (define-key isearch-mode-map             "\e" 'viper-intercept-ESC-key-isearch))

;; Unbind conflicting bindings
(define-key viper-vi-basic-map          "\C-c/" nil)
(define-key viper-vi-basic-map          "\C-\\" nil)
(define-key viper-insert-basic-map      "\C-\\" nil)



;; 
;; FUNCTIONS
;; 



(defun viper-insert-open-line-below ()
  "Open line below in insert mode."
  (interactive)
   (save-excursion
     (end-of-line)
     (newline)))


(defun viper-forward-char (arg)
  "Move point right ARG characters (left if ARG negative).
On reaching end of line, stop and signal error.
Modified to allow going beyond the last character. 
This works better with regions."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
        (com (viper-getcom arg))
        (onb (if (bolp) nil t)))        ; originally not bolp
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (if viper-ex-style-motion
        (progn
          ;; the boundary condition check gets weird here because
          ;; forward-char may be the parameter of a delete, and 'dl' works
          ;; just like 'x' for the last char on a line, so we have to allow
          ;; the forward motion before the 'viper-execute-com', but, of
          ;; course, 'dl' doesn't work on an empty line, so we have to
          ;; catch that condition before 'viper-execute-com'.
          ;; modified: check for bol instead, and cancel if originally not
          ;; on bol.
          (if (and (eolp) (bolp)) (error "") (forward-char val))
          (if com (viper-execute-com 'viper-forward-char val com))
          (if (and (bolp) onb) (progn (backward-char 1) (error ""))))
      (forward-char val)
      (if com (viper-execute-com 'viper-forward-char val com)))))


(defun viper-goto-eol (arg)
  "Go to end of line.
Modified to allow going beyond the last character.
This works better with regions."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
        (com (viper-getcom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (end-of-line val)
    (if com (viper-execute-com 'viper-goto-eol val com))))


;; Listen to ESC key.
;; If a sequence of keys starting with ESC is issued with very short delays,
;; interpret these keys in Emacs mode, so ESC won't be interpreted as a Vi key.
(defun viper-intercept-ESC-key ()
  "Function that implements ESC key in Viper emulation of Vi.
Modified to exit when in minibuffer and to deactivate the mark."
  (interactive)
  (let ((cmd (or (key-binding (viper-envelop-ESC-key))
		 '(lambda () (interactive) (error "")))))

    ;; call the actual function to execute ESC (if no other symbols followed)
    ;; or the key bound to the ESC sequence (if the sequence was issued
    ;; with very short delay between characters).
    (if (eq cmd 'viper-intercept-ESC-key)
	(setq cmd
	      (cond ((or (viper-is-in-minibuffer) mark-active)
                     'keyboard-escape-quit)
                    ((eq viper-current-state 'vi-state)
		     'viper-ESC)
		    ((eq viper-current-state 'insert-state)
		     'viper-exit-insert-state)
		    ((eq viper-current-state 'replace-state)
		     'viper-replace-state-exit-cmd)
		    (t 'viper-change-state-to-vi)
		    )))
    (call-interactively cmd)))


(defun viper-intercept-ESC-key-isearch ()
  "Cancel Isearch if ESC was not a prefix. Uses timeout functionality from viper."
  (interactive)
  (let ((cmd (or (key-binding (viper-envelop-ESC-key))
		 '(lambda () (interactive) (error "")))))

    ;; call the actual function to execute ESC (if no other symbols followed)
    ;; or the key bound to the ESC sequence (if the sequence was issued
    ;; with very short delay between characters).
    (if (eq cmd 'viper-intercept-ESC-key-isearch)
	(setq cmd 'isearch-cancel))
    (call-interactively cmd)))


;; From http://www.emacswiki.org/emacs/vimpulse.el:
;;
;; This function replaces viper's original viper-exec-change function
;; which is invoked by key sequences starting with 'c'.  When the user
;; requests a command like 'cw', this function calls a sequence like
;; 'dwi' instead.  This stops viper from indicating the change
;; operation with distracting colored overlays and $ signs.  Instead,
;; it simply deletes the text then enters Insert mode, like Vim does.
;; 
;; The function works fine at eol and eob but TODO: understand the
;; original viper-exec-change command and see if mine does everything
;; it does.
(defun viper-exec-change (m-com com)
  (viper-exec-delete m-com com)
  (if (eq m-com 'viper-goto-eol)
      ; use viper-append here since vi's C (change to end of line)
      ; command works differently than c
      (viper-append nil) 
    (viper-insert nil)))



;; ESN viper
(require 'esn-viper)

(defun set-modeline-colors (fg bg)
  "Return a lisp string which when evaluated will set the modeline colors."
  (concat "(progn "
          "(set-face-foreground 'modeline \"" fg "\") "
          "(set-face-background 'modeline \"" bg "\"))"))
 
(setq esn-viper-vi-command        (set-modeline-colors default-fg default-bg)
      esn-viper-vi-visual-command (set-modeline-colors default-fg default-bg)
      esn-viper-insert-command    (set-modeline-colors hilight-fg hilight-bg)
      esn-viper-emacs-command     (set-modeline-colors default-fg default-bg))
