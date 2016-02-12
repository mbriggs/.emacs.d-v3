(add-hook 'after-init-hook 'yas-global-mode)

(eval-after-load "yasnippet"
  '(progn
     (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
     (yas-reload-all)))

(autoload 'aya-create "auto-yasnippet")
(autoload 'aya-expand "auto-yasnippet")

(define-key evil-insert-state-map (kbd "M-y") 'aya-expand)
(define-key evil-visual-state-map (kbd "M-Y") 'aya-create)

(defun force-yasnippet-off ()
  (setq-local yas-dont-activate t)
  (yas-minor-mode -1))

(add-hook 'term-mode-hook #'force-yasnippet-off)
(add-hook 'shell-mode-hook #'force-yasnippet-off)

(provide 'conf-snippets)
