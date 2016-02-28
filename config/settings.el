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
  minibuffer-message-timeout 0.8
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
  password-cache-expiry (* 60 15)
  dabbrev-case-replace nil
  recentf-exclude '("/tmp/" "/ssh:"))

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(setq-default
  indent-tabs-mode nil
  indicate-buffer-boundaries nil
  tab-width 2)

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

;(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun save-all ()
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

(provide 'settings)
