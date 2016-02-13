(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
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
        (if (check-expansion)
            (progn
              (company-manual-begin)
              (if (null company-candidates)
                  (progn
                    (company-abort)
                    (indent-for-tab-command)))))))))

(defun tab-complete-or-next-field ()
  (interactive)
  (if (or (not yas/minor-mode)
          (null (do-yas-expand)))
      (if company-candidates
          (company-complete-selection)
        (when (check-expansion)
          (company-manual-begin)
          (when (null company-candidates)
            (company-abort)
            (yas-next-field))
          (yas-next-field)))))

(defun expand-snippet-or-complete-selection ()
  (interactive)
  (if (or (not yas/minor-mode)
          (null (do-yas-expand))
          (company-abort))
      (company-complete-selection)))

(defun abort-company-or-yas ()
  (interactive)
  (if (null company-candidates)
      (yas-abort-snippet)
    (company-abort)))

(use-package yasnippet
             :ensure t
             :config
             (define-key yas-minor-mode-map [tab] nil)
             (define-key yas-minor-mode-map (kbd "TAB") nil)

             (define-key yas-keymap [tab] 'tab-complete-or-next-field)
             (define-key yas-keymap (kbd "TAB") 'tab-complete-or-next-field)
             (define-key yas-keymap [(control tab)] 'yas-next-field)
             (define-key yas-keymap (kbd "C-g") 'abort-company-or-yas))


(use-package company-statistics :ensure t)
(use-package
  company
  :ensure t
  :defer nil
  :bind* (("<tab>" . tab-indent-or-complete)
          ("C-<return>" . company-complete-common))
  :init
  (setq company-dabbrev-downcase nil)
  (global-company-mode)
  (company-statistics-mode))


(provide 'conf-company)
