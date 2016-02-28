(use-package yasnippet
             :ensure t
             :init
             (add-hook 'after-init-hook 'yas-global-mode)
             (add-hook 'term-mode-hook #'force-yasnippet-off)
             (add-hook 'shell-mode-hook #'force-yasnippet-off)
             :config
             (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
             (yas-reload-all))


(use-package auto-yasnippet
             :ensure t
             :commands (aya-create aya-expand)
             :bind* (("M-Y" . aya-create)
                     ("M-y" . aya-expand)))


(defun force-yasnippet-off ()
  (setq-local yas-dont-activate t)
  (yas-minor-mode -1))

(provide 'conf-snippets)
