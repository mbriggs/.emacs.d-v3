(use-package bind-key :ensure t)

(use-package goto-chg
  :ensure t
  :bind (("C-<backspace>" . goto-last-change)
         ("C-S-<backspace>" . goto-last-change-reverse)))

(bind-keys*
 ("M-<left>" . mb/start-of-line)
 ("M-<right>" . mb/end-of-line)
 ("A-<left>" . mb/backwards-word)
 ("A-<right>" . mb/forward-word)
 ("A-<backspace>" . mb/backward-delete-word)
 ("A-S-<backspace>" . mb/delete-word)
 ("M-<backspace>" . mb/delete-whole-line)
 ("M-<return>" . mb/open-line)
 ("S-<return>" . mb/open-line-above)
 ("A-SPC" . rectangle-mark-mode)
 ;; ("C-m" . mb/push-mark)
 ("A-n" . next-error)
 ("A-p" . previous-error)
 ("M-X" . exchange-point-and-mark)
 ("M-d" . mb/duplicate-line-or-region)
 ("M-c" . mb/copy-line-or-region)
 ("M-O" . ido-find-file)
 ("M-v" . yank)
 ("C-o" . pop-global-mark)
 ("M-x" . mb/cut-line-or-region)
 ("M-i" . mb/toolbox)
 ("M-z" . undo-only)
 ("M-Z" . undo)
 ("M-a" . mark-whole-buffer)
 ("M-J" . mb/join-line)
 ("M-F" . mb/fuse-line)
 ("C-p" . mb/scroll-down-line)
 ("C-n" . mb/scroll-up-line)
 ("C-f" . dired)
 ("C-v e" . mc/edit-ends-of-lines)
 ("C-v a" . mc/edit-beginnings-of-lines)
 ("M-j" . other-window)
 ("M-u" . split-window-right-and-move-there)
 ("M-U" . split-window-below-and-move-there)
 ("M-P" . projectile-switch-project)
 ("M-w" . delete-window)
 ("M--" . delete-other-windows)
 ("M-e" . ivy-switch-buffer)
 ("M-E" . ivy-switch-buffer-other-window)
 ("M-m" . imenu)
 ("M-s" . save-buffer)
 ("M-l" . goto-line-with-feedback)
 ("M-q" . save-buffers-kill-emacs)
 ("M-o" . projectile-find-file)
 ("<home>" . beginning-of-buffer)
 ("<end>" . end-of-buffer)
 ("M-/" . mb/comment-or-uncomment-region-or-line)

 ("<f3>" . flycheck-list-errors)
 ("<f5>" . projectile-regenerate-tags))

(bind-keys ("<escape>" . mb/quit))

(bind-keys :map isearch-mode-map
           ("M-f" . isearch-repeat-forward)
           ("M-F" . isearch-repeat-backward))

(define-key query-replace-map "a" 'automatic)

;; utils

(defun mb/scroll-down-line ()
  (interactive)
  (dotimes (i 3)
    (scroll-down-line)))

(defun mb/scroll-up-line ()
  (interactive)
  (dotimes (i 3)
    (scroll-up-line)))

(defun mb/line-beginning-text-position ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (point)))

(defun mb/end-of-previous-word ()
  (save-excursion
    (forward-word -1)
    (forward-word)
    (point)))

(defun mb/start-of-next-word ()
  (save-excursion
    (forward-word)
    (forward-word -1)
    (point)))

(defun mb/pos-eol ()
  (save-excursion
    (end-of-line)
    (point)))

(defun mb/pos-bol ()
  (save-excursion
    (beginning-of-line)
    (point)))


;; commands

(defun mb/comment-or-uncomment-region-or-line ()
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (progn
      (comment-or-uncomment-region (mb/pos-bol) (mb/pos-eol))
      (forward-line))))

(defun mb/quit ()
  (interactive)
  (if (window-minibuffer-p)
      (minibuffer-keyboard-quit)
    (keyboard-quit)))

(defun mb/rename-this-file-and-buffer ()
  (interactive)
  (let ((new-name (read-string "New Name: "))
        (name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file name new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

(defun mb/delete-this-buffer-and-file ()
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))


(defun mb/backwards-word ()
  (interactive "^")
  (let ((end-of-previous-word (mb/end-of-previous-word)))
    (cond
     ((eq (mb/pos-bol) (point))
      (forward-line -1)
      (end-of-line))
     ((< end-of-previous-word (mb/pos-bol))
      (beginning-of-line))
     ((< (point) end-of-previous-word)
      (forward-word -1))
     ((eq (point) end-of-previous-word)
      (forward-word -1))
     (t
      (goto-char end-of-previous-word)))))

(defun mb/forward-word ()
  (interactive "^")
  (let ((start-of-next-word (mb/start-of-next-word)))
    (cond
     ((eq (mb/pos-eol) (point))
      (forward-line)
      (beginning-of-line))
     ((> start-of-next-word (mb/pos-eol))
      (end-of-line))
     ((> (point) start-of-next-word)
      (forward-word 1))
     ((eq (point) start-of-next-word)
      (forward-word 1))
     (t
      (goto-char start-of-next-word)))))

(defun mb/duplicate-line-or-region ()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (let ((deactivate-mark) ; keep the region around after dupe
              (start (region-beginning))
              (end (region-end)))
          (goto-char end)
          (insert (buffer-substring start end)))

                                        ; no region, dupe line
      (let ((line (buffer-substring (point-at-bol)
                                    (point-at-eol))))
        (end-of-line)
        (newline)
        (insert line)))))


(defun mb/join-line ()
  "join the current and next lines, with one space in between them"
  (interactive)
  (save-excursion
    (forward-line 1)
    (beginning-of-line)
    (delete-char -1)
    (just-one-space)))

(defun mb/fuse-line ()
  "join the current and next lines, with no space in between them"
  (interactive)
  (save-excursion
    (forward-line 1)
    (beginning-of-line)
    (delete-char -1)
    (delete-horizontal-space)))

(defun mb/backward-delete-word ()
  "delete by word"
  (interactive)
  (let ((start (point)))
    (mb/backwards-word)
    (delete-region start (point))))

(defun mb/delete-word ()
  (interactive)
  (let ((start (point)))
    (forward-word 1)
    (delete-region start (point))))

(defun mb/end-of-line ()
  (interactive "^")
  (end-of-line))

(defun mb/start-of-line ()
  (interactive "^")

  (if (eq (mb/line-beginning-text-position) (point))
      (beginning-of-line)
    (beginning-of-line-text)))

(defun mb/open-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun mb/open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (forward-line -1)
  (indent-for-tab-command))

(defun mb/delete-whole-line ()
  (interactive)
  (delete-region (line-beginning-position)
                 (line-end-position))

  (if (eq (point) (point-min))
      (progn
        (forward-line 1)
        (delete-char -1))
    (progn
      (delete-char -1)
      (forward-line 1)))
  (beginning-of-line))

(defun mb/copy-line-or-region ()
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-end-position))))

(defun mb/cut-line-or-region ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-whole-line)))

(provide 'mb-editing)
