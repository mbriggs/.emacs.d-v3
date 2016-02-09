(setq
  ns-use-srgb-colorspace t
  ring-bell-function 'ignore
  create-lockfiles nil
  backup-directory-alist `(("." . "~/.saves"))
  backup-by-copying t
  delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t
  auto-save-default nil
  inhibit-startup-message t
  fill-column 85
  initial-major-mode 'emacs-lisp-mode
  browse-url-generic-program "google-chrome"
  initial-scratch-message nil
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":"
  super-save-auto-save-when-idle t
  require-final-newline t
  indicate-empty-lines nil
  recentf-max-saved-items 80
  dabbrev-case-replace nil
  recentf-exclude '("/tmp/" "/ssh:"))


(setq-default
  indent-tabs-mode nil
  indicate-buffer-boundaries nil
  tab-width 2)

(super-save-mode +1)
(global-auto-revert-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode t)
(show-paren-mode t)
(column-number-mode t)
;; (set-fringe-style '(8 . 0))
(tooltip-mode -1)
(recentf-mode 1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun save-all ()
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

(provide 'settings)
