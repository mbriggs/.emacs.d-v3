(require 'conf-company)

(use-package inf-ruby :ensure t)
(use-package bundler :ensure t)
(use-package rubocop :ensure t)

(use-package enh-ruby-mode
  :ensure t
  :mode "\\.\\(rb\\|rabl\\|Rakefile\\|Gemfile\\)$"
  :interpreter "ruby")

(use-package rbenv
  :ensure t
  :init
  (add-hook 'enh-ruby-mode-hook 'rbenv-use-corresponding)
  :config
  (rbenv-use-corresponding))

(use-package rake
  :ensure t
  :commands rake)

(use-package rspec-mode
  :ensure t
  :bind (:map ruby-mode-map
              ("<return>" . reindent-then-newline-and-indent)
              ("M-t ," . rspec-toggle-spec-and-target)
              ("M-t t" . rspec-verify-single)
              ("M-t l" . rspec-rerun)
              ("M-t f" . rspec-verify)
              ("M-t a" . rspec-verify-all))
  :init
  (setq rspec-use-rake-flag nil)
  (setq rspec-spec-command "spring rspec")
  (add-hook 'enh-ruby-mode-hook 'rspec-mode)

  :config
  (defadvice rspec-compile (around rspec-compile-around)
    "Use BASH shell for running the specs because of ZSH issues."
    (let ((shell-file-name "/bin/bash"))
      ad-do-it))

  (ad-activate 'rspec-compile))

(use-package robe
  :ensure t
  :init
  (add-hook 'enh-ruby-mode-hook 'robe-mode)
  (add-hook 'robe-mode-hook 'eldoc-mode)

  :config
  (push 'company-robe company-backends))

(provide 'conf-ruby)
