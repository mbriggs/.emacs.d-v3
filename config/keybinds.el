(require 'bind-key)

(bind-keys*
 ("M-<left>" . mb/start-of-line)
 ("M-<right>" . mb/end-of-line)
 ("M-<up>" . er/expand-region)
 ("M-<down>" . er/contract-region)
 ("A-<left>" . backward-word)
 ("A-<right>" . forward-word)
 ("A-<backspace>" . mb/backward-delete-word)
 ("A-S-<backspace>" . mb/delete-word)
 ("M-<backspace>" . mb/delete-whole-line)
 ("<escape>" . keyboard-escape-quit)
 ("M-<return>" . mb/open-line)
 ("M-S-<return>" . mb/open-line-above)
 ("M-d" . duplicate-current-line)
 ("M-c" . mb/copy-line-or-region)
 ("M-v" . yank)
 ("M-r" . anzu-query-replace-regexp)
 ("M-f" . isearch-forward-regexp)
 ("M-F" . isearch-backward-regexp)
 ("M-x" . mb/cut-line-or-region)
 ("M-z" . undo-only)
 ("M-Z" . undo)
 ("M-A" . smex)
 ("M-a" . mark-whole-buffer)
 ("M-J" . mb/join-line)
 ("C-p" . scroll-down-line)
 ("C-n" . scroll-up-line)
 ("M-j" . other-window)
 ("M-w" . split-window-right-and-move-there)
 ("M-W" . split-window-below-and-move-there)
 ("A-w" . delete-window)
 ("M--" . delete-other-windows)
 ("M-e" . ido-switch-buffer)
 ("M-E" . ido-switch-buffer-other-window)
 ("M-m" . imenu)
 ("M-s" . save-buffer)
 ("M-l" . goto-line-with-feedback)
 ("M-q" . save-buffers-kill-emacs)
 ("M-o" . projectile-find-file)
 ("M-O" . ido-find-file)

 ("<f1>" . magit-status)
 ("<f8>" . magit-blame))

(bind-keys
 ("M-t" . (apply-partially 'mb/placeholder "no tests defined yet")))

(bind-keys :map isearch-mode-map
           ("M-f" . isearch-repeat-forward)
           ("M-F" . isearch-repeat-backward))

;;; esc ALWAYS quits

(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)


;; tools

(defun mb/line-beginning-text-position ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (point)))

(defun mb/placeholder (msg)
  (message msg))

;; commands

(defun mb/join-line ()
  (interactive)
  (save-excursion
    (forward-line 1)
    (beginning-of-line)
    (delete-char -1)))

(defun mb/backward-delete-word ()
  (interactive)
  (let ((start (point)))
    (forward-word -1)
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

(defun mb/duplicate-line-or-region ()
  (interactive)
  (if (region-active-p)
      (duplicate-region)
    (duplicate-current-line)))

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
                 (line-end-position)))

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
