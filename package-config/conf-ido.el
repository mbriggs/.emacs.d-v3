
(use-package
  flx-ido
  :ensure t
  :bind (:map ido-completion-map
              (("C-n" . ido-next-match)
               ("C-p" . ido-prev-match)))
  :init
  (require 'ido)
  (require 'flx-ido)

  (ido-mode 1)
  (setq flx-ido-threshhold 500)
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-use-faces nil)
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point nil)
  (setq ido-auto-merge-work-directories-length -1)
  (setq ido-use-virtual-buffers t)

  (defun my-minibuffer-setup-hook ()
    (setq gc-cons-threshold most-positive-fixnum))

  (defun my-minibuffer-exit-hook ()
    (setq gc-cons-threshold 800000))

  (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

  ;; Allow the same buffer to be open in different frames
  (setq ido-default-buffer-method 'selected-window))

(use-package ido-vertical-mode
             :ensure t
             :init
             (ido-vertical-mode 1))

(provide 'conf-ido)
