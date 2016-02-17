(use-package imenu-anywhere :ensure t)
(use-package htmlize :ensure t)
(use-package ag :ensure t)
(use-package paradox :ensure t)

(use-package vkill
  :ensure t
  :commands (vkill))

(use-package goto-chg
  :ensure t
  :commands (goto-last-change goto-last-change-reverse)
  :bind (("C-o" . goto-last-change)
         ("C-O" . goto-last-change-reverse)))

(use-package inf-mongo
  :quelpa (inf-mongo
           :fetcher github
           :repo "tobiassvn/inf-mongo"))

(use-package discover-my-major
  :quelpa (discover-my-major
           :fetcher github
           :repo "steckerhalter/discover-my-major"))

(use-package expand-region
  :ensure t
  :commands (er/expand-region er/contract-region)
  :bind* (("M-<up>" . er/expand-region)
          ("M-<down>" . er/contract-region))
  :init
  (require 'expand-region))

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
