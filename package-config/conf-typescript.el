(use-package typescript-mode
             :ensure t
             :mode "\\.ts$")

(use-package tide
             :ensure t
             :init
             (add-hook 'typescript-mode 'tide-setup)
             :config
             (eldoc-mode +1))


(provide 'conf-typescript)
