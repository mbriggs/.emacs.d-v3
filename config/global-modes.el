(require 'appearance)
(require 'general-tools)

(use-package anzu
  :ensure t
  :init
  (global-anzu-mode +1))

(use-package ws-butler
  :ensure t
  :init
  (ws-butler-global-mode +1))

(use-package fic-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'fic-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("M-'" . mc/mark-next-like-this-word)
         ("M-\"" . mc/skip-to-next-like-thi))
  :init
  (require 'multiple-cursors))

(use-package projectile
  :ensure t
  :init
  (setq projectile-enable-caching nil)
  (projectile-global-mode))

(use-package avy
  :ensure t
  :commands avy-goto-word-1
  :bind ("M-;" . avy-goto-word-1))


(use-package flycheck
  :ensure t
  :init
  (add-hook 'sh-mode-hook 'flycheck-mode)
  (add-hook 'json-mode-hook 'flycheck-mode)
  (add-hook 'nxml-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  (add-hook 'lisp-interaction-mode-hook 'flycheck-mode)
  (add-hook 'js2-mode-hook 'flycheck-mode)
  (add-hook 'ruby-mode-hook 'flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-indication-mode nil)

  :config
  (flycheck-add-mode 'javascript-eslint 'babel-mode))


(use-package flycheck-color-mode-line
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t)
  (with-theme-colors
   (set-face-attribute 'vhl/default-face nil
                       :background darker-gray)))


(use-package super-save
  :ensure t
  :init
  (super-save-mode +1))
(provide 'global-modes)
