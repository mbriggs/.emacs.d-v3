(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(defun init-ts ()
  (tide-setup)
  (eldoc-mode +1))

;; sample config
(add-hook 'typescript-mode-hook 'init-ts)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; Tide can be used along with web-mode to edit tsx files
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (init-ts))))

(eval-after-load "tide"
  '(lambda ()
     (bind-key "<f2>" 'tide-documentation-at-point typescript-mode-map)))


(provide 'conf-typescript)
