(use-package markdown-mode
             :ensure t
             :commands (markdown-mode
                        gfm-mode)
             :init
             (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
             (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
             (add-to-list 'auto-mode-alist '("\\.text$" . gfm-mode)))

(provide 'conf-markdown)
