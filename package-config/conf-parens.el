(require 'smartparens-config)
(show-smartparens-global-mode +1)
(smartparens-global-mode +1)


(define-key sp-keymap (kbd "M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "M-K") 'sp-backward-kill-sexp)
(define-key sp-keymap (kbd "M-L") 'sp-backward-barf-sexp)
(define-key sp-keymap (kbd "M-H") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "M-h") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "M-l") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<right>") 'sp-beginning-of-next-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-end-of-previous-sexp)
(define-key sp-keymap (kbd "S-<left>") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "S-<right>") 'sp-end-of-sexp)
(evil-define-key 'normal sp-keymap
  (kbd "(") 'wrap-round
  (kbd "\"") 'wrap-quote
  (kbd "[") 'wrap-square
  (kbd "K") 'sp-splice-sexp)

(defun wrap-round ()
  (interactive)
  (sp-wrap-with-pair "("))

(defun wrap-quote ()
  (interactive)
  (sp-wrap-with-pair "\""))

(defun wrap-square ()
  (interactive)
  (sp-wrap-with-pair "["))

(provide 'conf-parens)
