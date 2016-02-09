(require 'alchemist)
(require 'conf-parens)

(setq alchemist-test-status-modeline nil)

(evil-ex-define-cmd "iex" 'alchemist-iex-project-run)
(evil-ex-define-cmd "mix" 'alchemist-mix)
(evil-define-key 'normal alchemist-mode-map
  ",ta" 'alchemist-mix-test
  ",tt" 'alchemist-mix-test-at-point
  ",tf" 'alchemist-mix-test-file
  ",tl" 'alchemist-mix-rerun-last-test
  ",t," 'alchemist-project-toggle-file-and-tests)

(add-hook 'alchemist-iex-mode-hook 'evil-insert-state)
(add-hook 'elixir-mode-hook 'flycheck-mode)
(add-hook 'elixir-mode-hook 'flycheck-elixir-dogma-setup)

(sp-with-modes '(elixir-mode)
  (sp-local-pair "->" "end"
                 :when '(("RET"))
                 :post-handlers '(:add my-elixir-do-end-close-action)
                 :actions '(insert)))

(sp-with-modes '(elixir-mode)
  (sp-local-pair "do" "end"
                 :when '(("SPC" "RET"))
                 :post-handlers '(:add my-elixir-do-end-close-action)
                 :actions '(insert)))


(provide 'conf-elixir)
