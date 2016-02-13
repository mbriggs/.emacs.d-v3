(use-package web-mode
             :ensure t
             :init
             (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
             (setq web-mode-code-indent-offset 2
                   web-mode-markup-indent-offset 2)

             (add-hook 'web-mode-hook
                       (lambda ()
                         (when (string-equal "tsx" (file-name-extension buffer-file-name))
                           (init-ts)))))


(use-package emmet-mode
             :ensure t
             :init
             (add-hook 'web-mode-hook 'emmet-mode)
             (setq emmet-indentation 2))

(provide 'conf-web)
