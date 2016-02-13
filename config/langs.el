(use-package sass-mode
             :ensure t
             :mode "\\.sass\\.erb"
             :init
             (setq css-indent-offset 2))

(use-package scss-mode
             :ensure t
             :mode "\\.scss\\.erb"
             :init
             (setq css-indent-offset 2))

(provide 'langs)
