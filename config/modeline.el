(setq-default mode-line-format
              '(
                (:eval (propertize "%3l" 'face 'mode-line-line-position-face))

                (:eval (propertize "%3c" 'face
                                   (if (>= (current-column) 75)
                                       'mode-line-80col-face
                                     'mode-line-position-face)))


                " "
                (:propertize (:eval (shorten-directory default-directory 10))
                             face mode-line-folder-face)
                (:propertize "%b"
                             face mode-line-filename-face)
                " "
                                        ; read-only or modified status
                (:eval
                 (cond (buffer-read-only
                        (propertize " !RO " 'face 'mode-line-read-only-face))
                       ((buffer-modified-p)
                        (propertize " !** " 'face 'mode-line-modified-face))
                       (t(propertize "  \u2713  " 'face 'mode-line-folder-face))))

                                        ; emacsclient [default -- keep?]
                ;; mode-line-client
                                        ; directory and buffer/file name
                " ("
                (if (eq major-mode 'elixir-mode)
                    mode-name
                  (:eval (propertize mode-name 'face 'mode-line-mode-face)))

                ") "
                (:propertize (vc-mode vc-mode)
                             face mode-line-minor-mode-face)

                (:eval (propertize (format-mode-line minor-mode-alist)
                                   'face 'mode-line-minor-mode-face))
                (:propertize mode-line-process
                             face mode-line-process-face)
                (global-mode-string global-mode-string)
                ))



;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(defun mlb/flycheck-state-face ()
  (cond
   ((flycheck-has-current-errors-p 'error)
    'mode-line-mode-errors-face)
   ((flycheck-has-current-errors-p 'warning)
    'mode-line-mode-warning-face)
   (t 'mode-line-folder-face)))

;; ;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-mode-errors-face)
(make-face 'mode-line-mode-warning-face)
(make-face 'mode-line-mode-no-errors-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(with-theme-colors
 (set-face-attribute 'mode-line nil
                     :foreground fg
                     :background blue-bg
                     :box `(:color ,blue-bg))

 (set-face-attribute 'mode-line-inactive nil
                     :foreground dark-gray
                     :background bg
                     :box `(:color ,bg :style nil))

 (set-face-attribute 'mode-line-read-only-face nil
                     :inherit 'mode-line-face
                     :background bg
                     :foreground red
                     :box `(:color ,bg))

 (set-face-attribute 'mode-line-modified-face nil
                     :inherit 'mode-line-face
                     :foreground cursor
                     :background bg
                     :box `(:color ,bg))

 (set-face-attribute 'mode-line-folder-face nil
                     :inherit 'mode-line-face)

 (set-face-attribute 'mode-line-filename-face nil
                     :inherit 'mode-line-face
                     :foreground yellow)

 (set-face-attribute 'mode-line-position-face nil
                     :foreground gray
                     :inherit 'mode-line-face)

 (set-face-attribute 'mode-line-line-position-face nil
                     :inherit 'mode-line-face)

 (set-face-attribute 'mode-line-mode-face nil
                     :slant 'italic
                     :inherit 'mode-line-face)

 (set-face-attribute 'mode-line-mode-errors-face nil
                     :slant 'italic
                     :foreground red
                     :inherit 'mode-line-face)

 (set-face-attribute 'mode-line-mode-warning-face nil
                     :slant 'italic
                     :foreground yellow
                     :inherit 'mode-line-face)

 (set-face-attribute 'mode-line-mode-no-errors-face nil
                     :slant 'italic
                     :foreground green
                     :inherit 'mode-line-face)

 (set-face-attribute 'mode-line-minor-mode-face nil
                     :foreground gray
                     :slant 'italic
                     :inherit 'mode-line-mode-face)

 (set-face-attribute 'mode-line-process-face nil
                     :inherit 'mode-line-face
                     :foreground dark-green)

 (set-face-attribute 'mode-line-80col-face nil
                     :inherit 'mode-line-position-face
                     :foreground bg
                     :background yellow))

(provide 'modeline)
