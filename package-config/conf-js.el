(require 'conf-company)

(use-package json-mode :ensure t)
(use-package json-snatcher :ensure t)
(use-package js-doc :ensure t)
(use-package js2-mode :ensure t :mode "\\.js\\'")

(define-derived-mode react-mode web-mode "Babel")

(defun mlb/react-init ()
  (flycheck-mode +1)
  (web-mode-set-content-type "jsx"))

(add-hook 'react-mode-hook 'mlb/react-init)
(add-hook 'react-mode-hook (lambda () (tern-mode t)))

(provide 'conf-js)
