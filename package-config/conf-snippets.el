(use-package yasnippet
  :ensure t
  :init
  (add-hook 'after-init-hook 'yas-global-mode)
  (add-hook 'term-mode-hook #'force-yasnippet-off)
  (add-hook 'shell-mode-hook #'force-yasnippet-off)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-reload-all)

  (defun do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))

  (defun mb/handle-tab ()
    (interactive)
    (cond
     ((minibufferp)
      (minibuffer-complete))
     ((string= mode-name "Magit")
      (magit-section-toggle (magit-current-section)))
     ((string= mode-name "Shell")
      (company-manual-begin))
     (t
      (indent-for-tab-command)
      (if (or (not yas/minor-mode)
              (null (do-yas-expand)))
          (auto-complete)))))

  (define-key yas-minor-mode-map [tab] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  (define-key yas-keymap [tab] 'mb/handle-tab)
  (define-key yas-keymap (kbd "TAB") 'mb/handle-tab)
  (bind-key* "TAB" 'mb/handle-tab)

  ; hax for multiline mirrors
  (defun yas--mirror-update-display (mirror field)
    "Update MIRROR according to FIELD (and mirror transform)."

    (let* ((mirror-parent-field (yas--mirror-parent-field mirror))
           (reflection (and (not (and mirror-parent-field
                                      (yas--field-modified-p mirror-parent-field)))
                            (or (yas--apply-transform mirror field 'empty-on-nil)
                                (yas--field-text-for-display field)))))
      (when (and reflection
                 (not (string= reflection (buffer-substring-no-properties (yas--mirror-start mirror)
                                                                          (yas--mirror-end mirror)))))
        (goto-char (yas--mirror-start mirror))
        (let ((yas--inhibit-overlay-hooks t))
          (insert reflection)
          (let ((start (yas--mirror-start mirror))
                (end (yas--mirror-end mirror)))
            (when (and (eq yas-indent-line 'auto)
                       (not (eq (line-number-at-pos start)
                                (line-number-at-pos end))))
              (indent-region start end))))
        (if (> (yas--mirror-end mirror) (point))
            (delete-region (point) (yas--mirror-end mirror))
          (set-marker (yas--mirror-end mirror) (point))
          (yas--advance-start-maybe (yas--mirror-next mirror) (point))
          ;; super-special advance
          (yas--advance-end-of-parents-maybe mirror-parent-field (point)))))))


(use-package auto-yasnippet
  :ensure t
  :commands (aya-create aya-expand)
  :bind* (("M-Y" . aya-create)
          ("M-y" . aya-expand)))


(defun force-yasnippet-off ()
  (setq-local yas-dont-activate t)
  (yas-minor-mode -1))





(defun mb/ruby-initialize-args (args)
  (string-join (--map (concat "@" it " = " it) (s-split ", " args)) "\n"))

(provide 'conf-snippets)
