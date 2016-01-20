(require 'open-github-from-here)
(setq open-github-from-here:command (expand-file-name "~/.emacs.d/make-github-url-from-file"))

(require 'evil)
(push 'git-commit-mode evil-emacs-state-modes)
(push 'text-mode evil-emacs-state-modes)

(global-diff-hl-mode +1)

(provide 'conf-git)
