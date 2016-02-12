(global-evil-jumper-mode +1)
(avy-setup-default)
(global-set-key (kbd "M-;") 'avy-goto-char-2)
(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-normal-state-map (kbd "C-p") nil)
(define-key evil-normal-state-map (kbd "C-n") nil)
(define-key evil-normal-state-map (kbd "C-S-P") nil)
(define-key evil-insert-state-map (kbd "C-p") nil)
(define-key evil-insert-state-map (kbd "C-n") nil)
(define-key evil-insert-state-map (kbd "C-S-P") nil)
(define-key evil-motion-state-map (kbd "C-f") nil)
(define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-word-mode)
(define-key evil-operator-state-map (kbd "SPC") #'evil-ace-jump-word-mode)

(mapc
 (lambda (mode) (push mode evil-emacs-state-modes))
 '(git-commit-mode
   prodigy-mode
   neotree-mode))

(add-hook 'magit-blame-mode-hook 'evil-insert-state)

;; between

(defgroup evil-textobj-between nil
  "Text object between for Evil"
  :prefix "evil-textobj-between-"
  :group 'evil)

(defcustom evil-textobj-between-i-key "f"
  "Keys for evil-inner-between"
  :type 'string
  :group 'evil-textobj-between)
(defcustom evil-textobj-between-a-key "f"
  "Keys for evil-a-between"
  :type 'string
  :group 'evil-textobj-between)

(defun evil-between-range (count beg end type &optional inclusive)
  (ignore-errors
    (let ((count (abs (or count 1)))
          (beg (and beg end (min beg end)))
          (end (and beg end (max beg end)))
          (ch (evil-read-key))
          beg-inc end-inc)
      (save-excursion
        (when beg (goto-char beg))
        (evil-find-char (- count) ch)
        (setq beg-inc (point)))
      (save-excursion
        (when end (goto-char end))
        (backward-char)
        (evil-find-char count ch)
        (setq end-inc (1+ (point))))
      (if inclusive
          (evil-range beg-inc end-inc)
        (if (and beg end (= (1+ beg-inc) beg) (= (1- end-inc) end))
            (evil-range beg-inc end-inc)
          (evil-range (1+ beg-inc) (1- end-inc)))))))

(evil-define-text-object evil-a-between (count &optional beg end type)
  "Select range between a character by which the command is followed."
  (evil-between-range count beg end type t))
(evil-define-text-object evil-inner-between (count &optional beg end type)
  "Select inner range between a character by which the command is followed."
  (evil-between-range count beg end type))

(define-key evil-outer-text-objects-map evil-textobj-between-a-key
  'evil-a-between)
(define-key evil-inner-text-objects-map evil-textobj-between-i-key
  'evil-inner-between)



;; evil-mc

; (when (fboundp 'add-to-load-path)
;   (add-to-load-path (file-name-directory (buffer-file-name))))
; 
; (require 'evil-mc)
; 
; (evil-define-local-var evil-mc-custom-paused nil
;   "Paused functionality when there are multiple cursors active.")
; 
; (defun evil-mc-pause-smartchr-for-mode (mode)
;   "Temporarily disables the smartchr keys for MODE."
;   (let ((m-mode (if (atom mode) mode (car mode)))
;         (s-mode (if (atom mode) mode (cdr mode))))
;     (let ((init (intern (concat "smartchr/init-" (symbol-name s-mode))))
;           (undo (intern (concat "smartchr/undo-" (symbol-name s-mode)))))
;       (when (eq major-mode m-mode)
;         (funcall undo)
;         (push `(lambda () (,init)) evil-mc-custom-paused)))))
; 
; (defun evil-mc-before-cursors-setup-hook ()
;   "Hook to run before any cursor is created.
; Can be used to temporarily disable any functionality that doesn't
; play well with `evil-mc'."
;   (mapc 'evil-mc-pause-smartchr-for-mode
;         '(web-mode js2-mode java-mode (enh-ruby-mode . ruby-mode) css-mode))
;   (when (boundp whitespace-cleanup-disabled)
;     (setq whitespace-cleanup-disabled t)
;     (push (lambda () (setq whitespace-cleanup-disabled nil)) evil-mc-custom-paused)))
; 
; (defun evil-mc-after-cursors-teardown-hook ()
;   "Hook to run after all cursors are deleted."
;   (dolist (fn evil-mc-custom-paused) (funcall fn))
;   (setq evil-mc-custom-paused nil))
; 
; ;; (add-hook 'evil-mc-before-cursors-created 'evil-mc-before-cursors-setup-hook)
; ;; (add-hook 'evil-mc-after-cursors-deleted 'evil-mc-after-cursors-teardown-hook)
; 
; (defvar evil-mc-mode-line-prefix "ⓜ"
;   "Override of the default mode line string for `evil-mc-mode'.")
; 
; (global-evil-mc-mode 1)

(provide 'conf-evil)
