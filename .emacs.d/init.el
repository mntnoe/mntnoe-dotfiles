;; File:     .emacs
;; Author:   Mads N Noe <mntnoe (@) gmail.com>
;; License:  as-is


;; ENVIRONMENT

(defconst text-bg    "#ffffff")
(defconst text-fg    "#000000")
(defconst default-bg "#3c3b37")
(defconst default-fg "#ffffff")
(defconst hilight-bg "#b7ab9b")
(defconst hilight-fg "#ffffff")

(defconst my-emacs-dir           "/home/mntnoe/.emacs.d")
(defconst tags-file-name         (concat my-emacs-dir "/TAGS"))
(defconst abbrev-file-name       (concat my-emacs-dir "/abbrev_defs"))
(defconst viper-custom-file-name (concat my-emacs-dir "/viper.el"))
(defconst custom-file            (concat my-emacs-dir "/custom.el"))
(defconst functions-file         (concat my-emacs-dir "/functions"))
(defconst org-file               (concat my-emacs-dir "/org"))
(defconst tabbar-file            (concat my-emacs-dir "/tabbar"))
(defconst modes-file             (concat my-emacs-dir "/modes"))


;; AUXILIARY FILES 1

(add-to-list 'load-path (concat my-emacs-dir "/elisp"))


;; REQUIRE 1

(require 'cl)
(require 'flymake)


;; AUXILIARY FILES 2

(load custom-file)
(load functions-file)
(load functions-file)
(load org-file)
(load tabbar-file)
(load modes-file)


;; REQUIRE 2

(require 'redo)
(require 'highlight-symbol)
(require 'hideshow)
(require 'highlight-parentheses)
(require 'parenface)
(require 'saveplace)


;; USER INTERFACE

(setq inhibit-splash-screen t)
(global-font-lock-mode 1)
(show-paren-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(recentf-mode 1)
(transient-mark-mode 1)
(set-fringe-style 0)
(when (not window-system)
  (setq frame-background-mode 'dark)
  (set-input-mode t nil t 7)
  (xterm-mouse-mode 1))


;; INDENTING

(setq tab-width 4)
(setq-default indent-tabs-mode nil)


;; SAVING

(prefer-coding-system 'iso-latin-1)
(setq current-language-environment "Latin-1")
(setq auto-save-default nil)
(defvar backup-dir (concat my-emacs-dir "/backups/"))
(setq backup-directory-alist (list (cons "." backup-dir)))


;; MODES

(if window-system (server-start))
(iswitchb-mode 1)


;; MISC

(fset 'yes-or-no-p 'y-or-n-p)
(setq scroll-step 1
      diff-switches "-u"
      fill-column 80
      sentence-end-double-space nil)

;; SavePlace
(setq save-place-file "/home/mntnoe/.emacs.d/saveplace")
(setq-default save-place t)


;;
;; MAPPINGS
;;

(global-set-key "\C-x\C-b" 'iswitchb-buffer) ; overrides!
(global-set-key "\C-x\C-o" 'other-window)    ; overrides!

(global-set-key (kbd "C-_") 'undo)
(global-set-key (kbd "C-x C-_") 'redo)
(global-set-key (kbd "C-/") 'undo)
(global-set-key (kbd "C-x C-/") 'redo)

(global-set-key (kbd "C-M-h") 'backward-kill-word)

(if window-system
    (global-set-key (kbd "M-v") 'clipboard-yank)
  (global-set-key (kbd "M-v") 'term-clipboard-yank))

(if window-system
    (global-set-key (kbd "M-c") 'clipboard-kill-ring-save)
  (global-set-key (kbd "M-c") 'term-clipboard-kill-ring-save))

(global-set-key (kbd "M-a") 'execute-extended-command)
(global-set-key (kbd "C-M-_") 'help)    ; xterm maps C-h to this


;; WINDOW SPLITING

(global-set-key (kbd "M-r") 'move-cursor-next-pane)
(global-set-key (kbd "M-R") 'move-cursor-previous-pane)
(global-set-key (kbd "M-b") 'iswitchb-buffer)


;; BUFFER RELATED

(global-set-key (kbd "C-x C-n") 'new-empty-buffer) ; Open New File
(global-set-key (kbd "M-f") 'find-alternate-file)
(global-set-key (kbd "M-t") 'find-file)
(global-set-key (kbd "M-T") 'recentf-open-files)
(global-set-key (kbd "M-q") 'close-current-buffer)
(global-set-key (kbd "M-`") 'save-buffers-kill-emacs)
(global-set-key (kbd "M-w") 'save-buffer)
(global-set-key (kbd "M-W") 'save-some-buffers)

(global-set-key (kbd "M-l") 'tabbar-forward)
(global-set-key (kbd "M-h") 'tabbar-backward)

(global-set-key (kbd "M-5") 'query-replace)
(global-set-key (kbd "M-%") 'query-replace-regexp)

(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-0") 'delete-window)

(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)

(global-set-key (kbd "M-p") 'extend-selection)


;; FLYMAKE

(global-set-key (kbd "C-,") 'flymake-prev)
(global-set-key (kbd "C-.") 'flymake-next)


;; FOLDING

;; using global-set-key to avoid default upper case action
(global-set-key "\M-u"     'hs-hide-block)
(global-set-key "\M-o"     'hs-show-block)
(global-set-key "\C-x\M-u" 'hs-hide-all)
(global-set-key "\C-x\M-o" 'hs-show-all)
(global-set-key "\C-x\M-U" 'hs-hide-level)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)


;; HIGHLIGHT SYMBOL

(global-set-key [f6]         'highlight-symbol-at-point)
(global-set-key [(shift f6)] 'highlight-symbol-remove-all)
(global-set-key [f8]         'highlight-symbol-next)
(global-set-key [f7]         'highlight-symbol-prev)


;; EDITING

(global-set-key (kbd "M-p") 'fill-paragraph)
(global-set-key (kbd "M-9") 'toggle-letter-case)
(global-set-key (kbd "M-i") 'indent-relative)


;; MINIBUFFER

(define-key minibuffer-local-map (kbd "M-r") 'other-window)
(define-key minibuffer-local-map (kbd "M-u") 'kill-whole-line)
(define-key minibuffer-local-map (kbd "M-t") "~/.emacs.d/")
(define-key minibuffer-local-map (kbd "M-h") "~/")


;; M-SPACE BINDINGS

(defun open-config-emacs () "Open .emacs.d/init.el." (interactive) (find-file "~/.emacs.d/init.el"))
(defun open-config-viper () "Open viper config." (interactive) (find-file "~/.emacs.d/viper.el"))
(defun open-config-xmonad () "Open xmonad config." (interactive) (find-file "~/.xmonad/xmonad.hs"))

(global-unset-key "\M- ")
(global-set-key "\M- \M-e" 'open-config-emacs)
(global-set-key "\M- \M-v" 'open-config-viper)
(global-set-key "\M- \M-w" 'open-config-xmonad)
(global-set-key "\M- \M-i" 'tabbar-toggle-emacs-buffers)
(global-set-key "\M- \M-f" 'hs-minor-mode)
(global-set-key "\M- \M-h" 'highlight-symbol-mode)


;; RECT-MARK

(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-x r v")     'rm-set-mark)
(global-set-key (kbd "C-x r C-x")   'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-w")   'rm-kill-region)
(global-set-key (kbd "C-x r d")     'rm-kill-region)
(global-set-key (kbd "C-x r M-w")   'rm-kill-ring-save)
(global-set-key (kbd "C-x r y")     'rm-kill-ring-save)


;; MOUSE

(defun up-one () (interactive) (scroll-up 2))
(defun down-one () (interactive) (scroll-down 2))
(global-set-key [mouse-4] 'down-one)
(global-set-key [mouse-5] 'up-one)


;; XTERM KEYS

(when (not window-system)
  (define-key function-key-map "\e[1;3A" [M-up])
  (define-key function-key-map "\e[1;3B" [M-down])
  (define-key function-key-map "\e[1;3C" [M-right])
  (define-key function-key-map "\e[1;3D" [M-left])
  (define-key function-key-map "\e[1;3F" [M-end])
  (define-key function-key-map "\e[1;3H" [M-home])
  (define-key function-key-map "\e[2;3~" [M-insert])
  (define-key function-key-map "\e[3;3~" [M-delete])
  (define-key function-key-map "\e[5;3~" [M-prior])
  (define-key function-key-map "\e[6;3~" [M-next])
  (define-key function-key-map "\e[7~" [home])
  (define-key function-key-map "\e[4~" [end])

  ;; hack, was C-M-RET alternative
  (define-key function-key-map "\e[27;13;13~" [M-S-return]))


;; VIPER

(setq viper-mode t)
(require 'viper)


;; COMINT

(add-hook 'comint-mode-hook
  #'(lambda ()  
      (define-key comint-mode-map "\M-r" 'other-window)
      (define-key comint-mode-map "\C-c\C-c" 'kill-buffer-and-close-window)))


;; XTERM TITLE

(when (and (not window-system)
           (string-match "^xterm" (getenv "TERM")))
  (require 'xterm-title)
  (xterm-title-mode 1))

(setq frame-title-format "emacs: %b") ; when using GUI


;; RECT-MARK

(autoload 'rm-set-mark "rect-mark"
  "Set mark for rectangle." t)
(autoload 'rm-exchange-point-and-mark "rect-mark"
  "Exchange point and mark for rectangle." t)
(autoload 'rm-kill-region "rect-mark"
  "Kill a rectangular region and save it in the kill ring." t)
(autoload 'rm-kill-ring-save "rect-mark"
  "Copy a rectangular region to the kill ring." t)


;; AUC-TEC

(setq-default TeX-master nil)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)


;; DISABLED COMMANDS

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

