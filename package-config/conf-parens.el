(defun wrap-round ()
  (interactive)
  (sp-wrap-with-pair "("))

(defun wrap-quote ()
  (interactive)
  (sp-wrap-with-pair "\""))

(defun wrap-square ()
  (interactive)
  (sp-wrap-with-pair "["))


(use-package smartparens
  :ensure t
  :init
  (setq
   sp-ignore-modes-list '(minibuffer-inactive-mode
                          markdown-mode)
   sp-autoskip-closing-pair 'always
   blink-matching-paren t)
  (require 'smartparens-config)
  (show-smartparens-global-mode +1)
  (smartparens-global-mode +1)

  (bind-keys :map sp-keymap
             ("M-k" . sp-kill-sexp)
             ("M-K"  . sp-splice-sexp)
             ("A-L" . sp-backward-barf-sexp)
             ("A-H" . sp-backward-slurp-sexp)
             ("A-h" . sp-forward-barf-sexp)
             ("A-l" . sp-forward-slurp-sexp)))


(provide 'conf-parens)
