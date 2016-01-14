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

(eval-after-load "flycheck"
  '(progn
     (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
     (flycheck-add-mode 'javascript-eslint 'babel-mode)))

(provide 'conf-flycheck)
