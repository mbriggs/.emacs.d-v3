(use-package auto-complete
  :ensure t
  :init
  (require 'auto-complete)
  (require 'auto-complete-config)

  (bind-keys :map ac-menu-map
             ("TAB" . nil)
             ("S-TAB" . nil)
             ("M-n" . 'ac-next)
             ("M-p" . 'ac-previous))

  (define-key ac-mode-map (kbd "TAB") nil)
  (define-key ac-completing-map (kbd "TAB") nil)
  (define-key ac-completing-map [tab] nil)

  (global-auto-complete-mode t)
  (setq-default ac-expand-on-auto-complete nil)
  (setq-default ac-auto-show-menu nil)
  (setq-default ac-use-fuzzy t)
  (setq-default ac-dwim nil) ; To get pop-ups with docs even if a word is uniquely completed

  (set-default 'ac-sources
               '(ac-source-imenu
                 ac-source-dictionary
                 ac-source-words-in-buffer
                 ac-source-words-in-same-mode-buffers
                 ac-source-words-in-all-buffer))

  (dolist (mode '(log-edit-mode org-mode text-mode haml-mode
                                git-commit-mode
                                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                                html-mode nxml-mode sh-mode smarty-mode clojure-mode
                                lisp-mode textile-mode markdown-mode tuareg-mode
                                js3-mode css-mode less-css-mode sql-mode
                                sql-interactive-mode
                                inferior-emacs-lisp-mode))
    (add-to-list 'ac-modes mode))


  ;; Exclude very large buffers from dabbrev
  (defun sanityinc/dabbrev-friend-buffer (other-buffer)
    (< (buffer-size other-buffer) (* 1 1024 1024)))

  (setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer))

(provide 'conf-ac)
