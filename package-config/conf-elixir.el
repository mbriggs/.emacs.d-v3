(require 'conf-parens)

(use-package
  alchemist
  :ensure t
  :init
  (setq alchemist-test-status-modeline nil)
  (add-hook 'alchemist-iex-mode-hook 'evil-insert-state)
  (add-hook 'elixir-mode-hook 'flycheck-mode)
  :config

  (sp-with-modes '(elixir-mode)
                 (sp-local-pair "->" "end"
                                :when '(("RET"))
                                :post-handlers '(:add my-elixir-do-end-close-action)
                                :actions '(insert)))

  (sp-with-modes '(elixir-mode)
                 (sp-local-pair "do" "end"
                                :when '(("SPC" "RET"))
                                :post-handlers '(:add my-elixir-do-end-close-action)
                                :actions '(insert))))

(provide 'conf-elixir)
