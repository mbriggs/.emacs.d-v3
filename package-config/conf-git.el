(use-package open-github-from-here
             :quelpa (open-github-from-here :fetcher github :repo "mbriggs/emacs-open-github-from-here")
             :commands open-on-github
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

(provide 'conf-git)
