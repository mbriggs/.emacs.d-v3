(require 'conf-company)

(use-package inf-ruby :ensure t)
(use-package bundler :ensure t)
(use-package rubocop :ensure t)

(use-package rbenv
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'rbenv-use-corresponding)
  :config
  (rbenv-use-corresponding))

(use-package rspec-mode
  :ensure t
  :init
  (setq rspec-use-rake-flag nil)
  (setq rspec-spec-command "rspec")
  (setq rspec-use-spring-when-possible t)
  (add-hook 'ruby-mode-hook 'rspec-mode)

  :config
  (defadvice rspec-compile (around rspec-compile-around)
    "Use BASH shell for running the specs because of ZSH issues."
    (let ((shell-file-name "/bin/bash"))
      ad-do-it))

  (bind-keys :map rspec-mode-map
             ("<return>" . reindent-then-newline-and-indent)
             ("M-t t" . rspec-verify-single)
             ("M-t l" . rspec-rerun)
             ("M-t f" . rspec-verify)
             ("M-t a" . rspec-verify-all))
  (ad-activate 'rspec-compile))

(use-package robe
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'robe-mode-hook 'eldoc-mode)

  :config
  (push 'company-robe company-backends))

(provide 'conf-ruby)
