(use-package dash :ensure t)
(use-package bind-key :ensure t)
(require 'dash)

(bind-keys*
 ("M-<left>" . mb/start-of-line)
 ("M-<right>" . mb/end-of-line)
 ("A-<left>" . mb/backwards-word)
 ("A-<right>" . mb/forward-word)
 ("A-<backspace>" . mb/backward-delete-word)
 ("A-S-<backspace>" . mb/delete-word)
 ("M-<backspace>" . mb/delete-whole-line)
 ("<escape>" . keyboard-escape-quit)
 ("M-<return>" . mb/open-line)
 ("M-S-<return>" . mb/open-line-above)
 ("M-d" . mb/duplicate-line-or-region)
 ("M-c" . mb/copy-line-or-region)
 ("M-v" . yank)
 ("M-f" . isearch-forward)
 ("M-F" . isearch-backward)
 ("M-x" . mb/cut-line-or-region)
 ("M-i" . mb/toolbox)
 ("M-z" . undo-only)
 ("M-Z" . undo)
 ("M-a" . mark-whole-buffer)
 ("M-J" . mb/join-line)
 ("A-J" . mb/fuse-line)
 ("C-p" . scroll-down-line)
 ("C-n" . scroll-up-line)
 ("C-f" . dired)
 ("M-j" . other-window)
 ("A-w" . split-window-right-and-move-there)
 ("A-W" . split-window-below-and-move-there)
 ("M-P" . projectile-switch-project)
 ("M-w" . delete-window)
 ("M--" . delete-other-windows)
 ("M-e" . ido-switch-buffer)
 ("M-E" . ido-switch-buffer-other-window)
 ("M-m" . imenu)
 ("M-s" . save-buffer)
 ("M-l" . goto-line-with-feedback)
 ("M-q" . save-buffers-kill-emacs)
 ("M-o" . projectile-find-file)
 ("M-O" . ido-find-file)

 ("<f3>" . flycheck-list-errors)
 ("<f5>" . projectile-regenerate-tags))

(bind-keys :map isearch-mode-map
           ("M-f" . isearch-repeat-forward)
           ("M-F" . isearch-repeat-backward))

;;; esc ALWAYS quits

(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'keyboard-quit)

;; toolbox

(defvar *mb/tools* '(("log" . magit-log-buffer-file)
                     ("butler" . butler-status)
                     ("buffers" . ibuffer)
                     ("branch" . magit-checkout)
                     ("clean" . clean-up-buffer-or-region)
                     ("align" . align-regexp)
                     ("ag - project" . ag-project)
                     ("ag - location" . ag)
                     ("ag - file type" . ag-project-files)
                     ("gist-list" . yagist-list)
                     ("gist" . yagist-region-or-buffer)
                     ("gist - private" . yagist-region-or-buffer-private)
                     ("erc" . start-erc)
                     ("git time machine" . git-timemachine)
                     ("shell" . shell)
                     ("eshell" . eshell)
                     ("sql - postgres" . sql-postgres)
                     ("open on github" . open-github-from-here)
                     ("prodigy" . prodigy)
                     ("mongo" . inf-mongo)
                     ("delete this file and buffer" . delete-this-buffer-and-file)
                     ("rename this file and buffer" . rename-this-file-and-buffer)
                     ("occur" . occur)
                     ("rubocop" . rubocop-autocorrect-current-file)
                     ("emacs dir" . dired-to-emacs-dir)))

(defun mb/toolbox ()
  (interactive)
  (let ((choice (ido-completing-read "Tool: " (-map 'car *mb/tools*))))
    (--> choice
         (assoc-string it *mb/tools*)
         (cdr it)
         (funcall it))))

;; utils

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

(defun mb/placeholder (msg)
  (message msg))

;; commands

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
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

(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
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
  (interactive)
  (let ((end-of-previous-word (mb/end-of-previous-word)))
    (cond
     ((< (point) end-of-previous-word)
      (forward-word -1))
     ((eq (point) end-of-previous-word)
      (forward-word -1))
     (t
      (goto-char end-of-previous-word)))))

(defun mb/forward-word ()
  (interactive)
  (let ((start-of-next-word (mb/start-of-next-word)))
    (cond
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

(provide 'keybinds)
