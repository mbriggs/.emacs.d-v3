(use-package git-timemachine :ensure t)
(use-package yagist :ensure t)
(use-package gh :ensure t)

(use-package open-github-from-here
  :quelpa (open-github-from-here :fetcher github :repo "mbriggs/emacs-open-github-from-here")
  :commands open-github-from-here
  :init
  (setq open-github-from-here:command (expand-file-name "~/.emacs.d/make-github-url-from-file")))

(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode +1))


(use-package magit
  :ensure t
  :bind* (("<f8>" . magit-blame)
          ("<f1>" . magit-status))
  :commands (magit-status
             magit-blame
             magit-checkout
             magit-log-buffer))

(use-package magit-gh-pulls
  :ensure t
  :init
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))


(provide 'conf-git)
