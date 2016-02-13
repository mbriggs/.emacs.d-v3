(use-package imenu-anywhere :ensure t)
(use-package paradox :ensure t)

(use-package expand-region
             :ensure t
             :commands (er/expand-region er/contract-region)
             :bind* (("M-<up>" . er/expand-region)
                     ("M-<down>" . er/contract-region)))

(use-package smex
             :ensure t
             :bind* ("M-A" . smex))

(use-package etags-select
             :ensure t
             :bind ("M-." . etags-select-find-tag))

(use-package anzu
             :ensure t
             :bind* ("M-r" . anzu-query-replace-regexp))

(provide 'general-tools)
