(require 'conf-company)

(use-package "json-mode" :ensure t)
(use-package "json-snatcher" :ensure t)
(use-package "js-doc" :ensure t)

(define-derived-mode babel-mode web-mode "Babel")

(defun mlb/babel-init ()
  (flycheck-mode +1)
  (web-mode-set-content-type "jsx"))

(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . babel-mode))
(add-hook 'babel-mode-hook 'mlb/babel-init)
(add-hook 'babel-mode-hook (lambda () (tern-mode t)))

(provide 'conf-js)
