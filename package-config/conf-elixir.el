(require 'alchemist)

(evil-ex-define-cmd "iex" 'alchemist-iex-project-run)
(evil-ex-define-cmd "mix" 'alchemist-mix)
(evil-define-key 'normal alchemist-mode-map
  ",ta" 'alchemist-mix-test
  ",tt" 'alchemist-mix-test-at-point
  ",tf" 'alchemist-mix-test-file
  ",tl" 'alchemist-mix-rerun-last-test
  ",t," 'alchemist-project-toggle-file-and-tests)

(add-hook 'alchemist-iex-mode-hook 'evil-insert-state)

(provide 'conf-elixir)
