(use-package json-mode :ensure t)
(use-package json-snatcher :ensure t)
(use-package js-doc :ensure t)

;; (use-package js-mode :mode ".js\\'")

(define-derived-mode babel-mode web-mode "Babel")

(defun mlb/babel-init ()
  (flycheck-mode +1)
  (web-mode-set-content-type "jsx"))

(add-hook 'babel-mode-hook 'mlb/babel-init)
(add-to-list 'auto-mode-alist '(".js\\'" . babel-mode))

(provide 'conf-js)
