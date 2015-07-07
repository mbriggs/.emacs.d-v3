(require 'bind-key)

;;; global

(bind-key "M-a" 'mark-whole-buffer)
(bind-key "M-x" 'smex)
(bind-key "C-\\" 'highlight-symbol-at-point)
(bind-key "RET" 'newline-and-indent)
(bind-key "M-j" 'other-window)
(bind-key "M-s" 'save-buffer)
(bind-key "M-r" 'rotate-windows)
(bind-key "M-R" 'toggle-window-split)
(bind-key "M-f" 'split-window-right-and-move-there)
(bind-key "M-F" 'split-window-below-and-move-there)
(bind-key "M-m" 'ido-imenu)
(bind-key "M-w" 'quit-window)
(bind-key "M-W" 'only-current-buffer)
(bind-key "M-q" 'save-buffers-kill-emacs)
(bind-key "<f1>" 'magit-status)
(bind-key "<f8>" 'magit-blame-mode)
(bind-key "M-." 'etags-select-find-tag)
(bind-key "<f3>" 'flycheck-list-errors)
(bind-key "C-f" 'dired)
(bind-key "M-v" 'evil-paste-after)
(bind-key "M-n" 'next-error)
(bind-key "M-P" 'session-jump-to-last-change)
(bind-key "M-p" 'previous-error)
(bind-key "M-t" 'projectile-switch-project)
(bind-key "M-j" 'evil-window-next)
(bind-key "C-SPC" 'comment-or-uncomment-region-or-line)
(bind-key "M-o" 'projectile-find-file)
(bind-key "C-p" 'switch-to-local-project)
(bind-key "M-`" 'other-frame)

;;; normal

(evil-define-key 'normal global-map
  (kbd " m") 'evil-jump-item
  (kbd ",,") 'evil-buffer
  (kbd "-") 'delete-other-windows
  (kbd "b") 'ido-switch-buffer
  (kbd "B") 'ibuffer
  (kbd "e") 'ido-find-file
  (kbd "E") 'ido-find-file-in-project-root
  (kbd "\\") 'evil-repeat-find-char-reverse
  (kbd "H") 'evil-first-non-blank
  (kbd "Y") 'copy-to-end-of-line
  (kbd "L") 'evil-last-non-blank
  (kbd "<tab>") 'indent-for-tab-command
  (kbd "<C-return>") 'new-line-in-normal-mode)


;;; esc ALWAYS quits

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'keyboard-quit)

;;; editing

(bind-key "M-q" 'evil-quit-all)
(evil-define-key 'visual global-map
  (kbd "M-d") 'duplicate-region)

(evil-define-key 'normal global-map
  (kbd "M-<backspace>") 'kill-whole-line
  (kbd "M-d") 'duplicate-current-line)

(evil-define-key 'insert global-map
  (kbd "M-d") 'duplicate-line
  (kbd "M-<backspace>") 'kill-whole-line
  (kbd "A-<backspace>") 'backward-kill-word
  (kbd "M-J") 'evil-join
  (kbd "M-v") 'yank
  (kbd "M-S-<return>") 'evil-open-above
  (kbd "M-<return>") 'evil-open-below)

;;; ex-mode

(defun ex-mode-mapping (cmd)
  (let ((binding (car cmd))
        (fn (cdr cmd)))
    (evil-ex-define-cmd binding fn)))

(mapc 'ex-mode-mapping
      '(("!"                        . shell-command)
        ("log"                      . magit-log)
        ("[buff]ers"                . ibuffer)
        ("[br]branch"               . magit-branch-manager)
        ("deft"                     . deft)
        ("reset-directory"          . reset-current-dir)
        ("log"                      . magit-file-log)
        ("history"                  . git-timemachine)
        ("bundle"                   . bundle-install)
        ("[chan]nel"                . ido-erc-buffer)
        ("clean"                    . clean-up-buffer-or-region)
        ("align"                    . align-regexp)
        ("[er]eval-region"          . eval-region)
        ("[eb]eval-buffer"          . eval-buffer)
        ("ack"                      . ack)
        ("ag"                       . ag-project)
        ("agl"                      . ag)
        ("agf"                      . ag-project-files)
        ("[al]ack-location"         . ack-location)
        ("gist-list"                . yagist-list)
        ("gist"                     . yagist-region-or-buffer)
        ("gistp"                    . yagist-region-or-buffer-private)
        ("erc"                      . start-erc)
        ("sh"                       . ansi-term)
        ("mx"                       . smex)
        ("mysql"                    . sql-mysql)
        ("[gh]github"               . open-github-from-here)
        ("[de]debug-elisp"          . edebug-defun)
        ("[pr]oject"                . switch-to-local-project)
        ("p"                        . prodigy)
        ("sw"                       . swoop)
        ("swm"                      . swoop-multi)
        ("mongo"                    . inf-mongo)
        ("sql"                      . sql-mysql)
        ("delete"                   . delete-this-buffer-and-file)
        ("rename"                   . rename-this-file-and-buffer)
        ("rlog"                     . rails-log-show-development)
        ("occur"                    . occur)
        ("rubo"                     . rubocop-autocorrect-current-file)
        ("[em]acs"                  . dired-to-emacs-dir)))

(provide 'keybinds)
