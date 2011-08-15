;; File:     functions.el
;; Author:   Mads N Noe <mail (@) madsnoe.dk>
;; License:  as-is
;;
;; Auxiliary functions.


(defun bc ()
  "Byte compile current file."
  (interactive)
  (save-buffer)
  (byte-compile-file (buffer-file-name)))

(defun w ()
  "Alias for save-buffer."
  (interactive)
  (save-buffer))

(defun kill-buffer-and-close-window ()
  "Kill-Buffer-And-Close-Window."
  (interactive)
  (close-current-buffer)
  (delete-window))

(defun recentf-open-files-and-isearch ()
  ""
  (interactive)
  (recentf-open-files)
  (isearch-forward))


;;; TEXT SELECTION RELATED

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun semnav-up (arg)
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (decf arg))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (incf arg)))
  (up-list arg))

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun extend-selection (arg &optional incremental)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (and transient-mark-mode mark-active)
                         (eq last-command this-command))))
  (if incremental
      (progn
        (semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

;;; TEXT TRANSFORMATION RELATED

(defun kill-line-backward ()
  "Kill text between the beginning of the line to the cursor position.
If there's no text, delete the previous line ending."
  (interactive)
  (if (looking-back "\n")
      (delete-char -1)
    (kill-line 0)
    )
  )

(defun backward-kill-word (arg)
  "Like backward-kill-word, but if there's no text, delete the
previous line ending."
  (interactive "p")
  (if (looking-back (rx "\n" (* not-wordchar)))
      (kill-line-backward)
    (kill-word (- arg))))

(defun move-cursor-next-pane ()
  "Move cursor to the next pane."
  (interactive)
  (other-window 1)
  )

(defun move-cursor-previous-pane ()
  "Move cursor to the previous pane."
  (interactive)
  (other-window -1)
  )

(defun compact-uncompact-block ()
  "Remove or add line endings on the current block.
This is similar to a toggle for fill-paragraph and unfill-paragraph.

When in text mode, a paragraph is considerd a block. When in programing
language mode, the block is delimited by {} or other delimiters
depending on the language.
Note: The programing language behavior is currently not done."
  (interactive)
  (let (bds numOfChars)
    (save-excursion
      (setq bds (bounds-of-thing-at-point 'line))
      (setq numOfChars (length (buffer-substring-no-properties (car bds) (cdr bds))))
      (setq numOfChars (- numOfChars 1)) ; 'line has eol
      )
    (when (/= numOfChars 0)
      (if (> numOfChars fill-column)
          (progn (fill-paragraph nil))
        (let ((fill-column 4333999))
          (fill-paragraph nil))
        )
      )
    )
  )

(defun remove-whitespaces ()
  "Remove white spaces around cursor.
If current line contains non-white space chars, then remove any whitespace char surrounding cursor.
If current line does not contain non-white space chars, then remove blank lines to just one."
  (interactive)
  (let (
        cursor-point
        line-has-meat-p  ; current line contains non-white space chars
        spaceTabNeighbor-p
        whitespace-begin whitespace-end
        space-or-tab-begin space-or-tab-end
        )
    (save-excursion
      ;; todo: what's consider whitespace should be taken from syntax table, and also consider whitespace chars in unicode if syntax table doesn't already considered it.
      (setq cursor-point (point))

      (setq spaceTabNeighbor-p (if (or (looking-at " \\|\t") (looking-back " \\|\t")) t nil) )
      (setq lineBounds (bounds-of-thing-at-point 'line))

      (setq line-has-meat-p (if (< 0 (count-matches "[[:graph:]]" (car lineBounds) (cdr lineBounds))) t nil) )

      (skip-chars-backward "\t ")
      (setq space-or-tab-begin (point))
      (skip-chars-backward "\t \n")
      (setq whitespace-begin (point))

      (goto-char cursor-point)
      (skip-chars-forward "\t ")
      (setq space-or-tab-end (point))
      (skip-chars-forward "\t \n")
      (setq whitespace-end (point))
      )

    (if line-has-meat-p
        (when spaceTabNeighbor-p
          (progn (delete-region space-or-tab-begin space-or-tab-end)) ; (insert " "))
          )
      (progn (delete-blank-lines))
      ;; todo: possibly code my own delete-blank-lines here for better efficiency, because delete-blank-lines seems complex.
      )
    )
  )

(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles from 3 cases: UPPER CASE, lower case, Title Case,
in that cyclic order."
  (interactive)
  (let (pos1 pos2 (deactivate-mark nil) (case-fold-search nil))
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning)
              pos2 (region-end))
      (setq pos1 (car (bounds-of-thing-at-point 'word))
            pos2 (cdr (bounds-of-thing-at-point 'word))))

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char pos1)
        (cond
         ((looking-at "[[:lower:]]") (put this-command 'state "lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "caps") )
         (t (put this-command 'state "lower") )
         )
        )
      )

    (cond
     ((string= "lower" (get this-command 'state))
      (upcase-initials-region pos1 pos2) (put this-command 'state "caps"))
     ((string= "caps" (get this-command 'state))
      (downcase-region pos1 pos2) (put this-command 'state "lower"))
     )
    )
  )

;;; BUFFER RELATED

(defun new-empty-buffer ()
  "Opens a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))
;; note: emacs won't offer to save a buffer that's
;; not associated with a file,
;; even if buffer-modified-p is true.
;; One work around is to define your own my-kill-buffer function
;; that wraps around kill-buffer, and check on the buffer modification
;; status to offer save
;; This custome kill buffer is close-current-buffer.


(defvar recently-closed-buffers (cons nil nil) "A list of recently closed buffers. The max number to track is controlled by the variable recently-closed-buffers-max.")
(defvar recently-closed-buffers-max 10 "The maximum length for recently-closed-buffers.")

(defun close-current-buffer ()
  "Close the current buffer.

Similar to (kill-buffer (current-buffer)) with the following addition:

- prompt user to save if the buffer has been modified even if the buffer is not associated with a file.
- make sure the buffer shown after closing is a user buffer.
- if the buffer is a file, add the path to the list recently-closed-buffers.

A emacs buffer is one who's name starts with *.
Else it is a user buffer."
  (interactive)
  (let (emacsBuff-p isEmacsBufferAfter)
    (if (string-match "^*" (buffer-name))
        (setq emacsBuff-p t)
      (setq emacsBuff-p nil))

    ;; offer to save buffers that are non-empty and modified, even for non-file visiting buffer. (because kill-buffer does not offer to save buffers that are not associated with files)
    (when (and (buffer-modified-p)
               (not emacsBuff-p)
               (not (string-equal major-mode "dired-mode"))
               (if (equal (buffer-file-name) nil) 
                   (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                 t
                 )
               )
      (if (y-or-n-p
           (concat "Buffer " (buffer-name) " modified; Do you want to save?"))
          (save-buffer)
        (set-buffer-modified-p nil)))

    ;; save to a list of closed buffer
    (when (not (equal buffer-file-name nil))
      (setq recently-closed-buffers
            (cons (cons (buffer-name) (buffer-file-name)) recently-closed-buffers))
      (when (> (length recently-closed-buffers) recently-closed-buffers-max)
        (setq recently-closed-buffers (butlast recently-closed-buffers 1)) 
        )
      )

    ;; close
    (kill-buffer (current-buffer))

    ;; if emacs buffer, switch to a user buffer
    (if (string-match "^*" (buffer-name))
        (setq isEmacsBufferAfter t)
      (setq isEmacsBufferAfter nil))
    (when isEmacsBufferAfter
      (previous-user-buffer))))

(defun previous-user-buffer ()
  "Switch to the previous user buffer in cyclic order.\n
User buffers are those not starting with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))

;;; MISC

;; indent everything
(defun indent-everything ()
  "Indents the whole buffer"
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(defun save-buffers-kill-emacs (&optional arg)
  "Offer to save each buffer, then kill this Emacs process.
With prefix arg, silently save all file-visiting buffers, then kill.
Modified not to ask for 'modified buffers, exit anyway?'."
  (interactive "P")
  (save-some-buffers arg t)
  (and (or (not (fboundp 'process-list))
           ;; process-list is not defined on VMS.
           (let ((processes (process-list))
                 active)
        (while processes
          (and (memq (process-status (car processes)) '(run stop open listen))
               (process-query-on-exit-flag (car processes))
               (setq active t))
          (setq processes (cdr processes)))
        (or (not active)
            (list-processes t)
            (yes-or-no-p "Active processes exist; kill them and exit anyway? "))))
      ;; Query the user for other things, perhaps.
      (run-hook-with-args-until-failure 'kill-emacs-query-functions)
      (kill-emacs)))

(defun term-clipboard-yank ()
  "Call xclip to yank from the X clipboard in an emacs -nw session."
  (interactive)
  (shell-command-on-region (point) (point) "xclip -selection clipboard -o" nil t nil nil))

;; TODO: fix xclip fork issues 
(defun term-clipboard-kill-ring-save ()
  "Call xclip to copy to the X clipboard in an emacs -nw session."
  (interactive)
  (let ((tmp-mark (mark t)))
    (progn
      (setq mark-active nil)
      (message "Paste the contents into another (better) program, and pres C-g to continue")
      (sit-for 0)
      (shell-command-on-region tmp-mark (point) "xclip -selection clipboard -i" nil nil nil nil))))

;; SURROUND (by mina86)

(defun surrounded-by-p (char)
  "Returns t if word is surrounded by given char."
  (save-excursion
    (and (forward-word -1)
         (equal char (char-before))
         (forward-word 1)
         (equal char (char-after)))))
 
(defun surround-word (char &optional force)
  "Surrounds word with given character.  If force is nil and word
 is already surrounded by given character remoevs them."
  (save-excursion
    (if (not (surrounded-by-p char))
        (progn
          (forward-word 1)
          (insert char)
          (forward-word -1)
          (insert char)
          t)
      (forward-word 1)
      (delete-char 1)
      (forward-word -1)
      (delete-char -1)
      nil)))
 
(defun my-bold-word (&optional force)
  (interactive "p")
  (surround-word ?- force))
 
(defun my-italic-word (&optional force)
  (interactive "p")
  (surround-word ?/ force))
 
(defun my-underline-word (&optional force)
  (interactive "p")
  (surround-word ?_ force))

;;
;; FLYMAKE
;;

(defun flymake-next ()
  (interactive)
  (flymake-goto-next-error)
  (flymake-display-err-msg-for-current-line))

(defun flymake-prev ()
  (interactive)
  (flymake-goto-prev-error)
  (flymake-display-err-msg-for-current-line))

(defun flymake-display-err-msg-for-current-line ()
  "ADAPTED FROM flymake-display-err-menu-for-current-line.
Display a msg with errors/warnings for current line if it has errors and/or warnings."
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0
                                   (flymake-find-err-info flymake-err-info line-no)))
         (menu-data           (flymake-make-err-menu-data line-no line-err-info-list))
         (choice              nil))
    (message (caar (cadr menu-data)))))


;; 
;; ADVICES
;; 

;; Auto-wrap isearch
(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

