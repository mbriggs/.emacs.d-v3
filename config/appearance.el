(use-package ample-theme
  :ensure t
  :init
  (load-theme 'ample t t)
  (enable-theme 'ample))

(use-package prettify-symbols
  :init
  (global-prettify-symbols-mode))

(use-package highlight-numbers
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; only turn off menus if not osx
(if (not (eq system-type 'darwin))
    (menu-bar-mode -1))


(let ((font "Operator Mono-18"))
  (set-frame-font font)
  (add-to-list 'default-frame-alist
               `(font . ,font)))

(line-number-at-pos)

(add-hook 'after-change-major-mode-hook
          (lambda ()
            (when (and
                   (not (eq major-mode 'Custom-mode))
                   (not (eq major-mode 'shell-mode))
                   (not (eq major-mode 'emacs-pager-mode))
                   (not (eq major-mode 'term-mode))
                   (not (eq major-mode 'eshell-mode))
                   (not (eq major-mode 'ibuffer-mode))
                   (not (eq major-mode 'rspec-compilation-mode))
                   (not (eq major-mode 'prodigy-mode)))
              (setq show-trailing-whitespace t))))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))


(setq linum-format (lambda (line)
                     (propertize
                      (format (concat " %"
                                      (number-to-string
                                       (length (number-to-string
                                                (line-number-at-pos (point-max)))))
                                      "d ")
                              line)
                      'face 'linum)))

(use-package highlight-cl
             :ensure t
             :init
             (add-hook 'emacs-lisp-mode-hook 'highlight-cl-add-font-lock-keywords))

(defmacro with-theme-colors (&rest body)
  `(let ((green "#6aaf50")
         (dark-green "#057f40")
         (blue "#5180b3")
         (white "#bdbdb3")
         (blue-bg "#102843")
         (light-blue "#528fd1")
         (lighter-blue "#68a5e9")
         (orange "#dF9522")
         (tan "#bdbc61")
         (dark-tan "#7d7c61")
         (yellow "#baba36")
         (bright-yellow "#fffe0a")
         (purple "#ab75c3")
         (gray "#757575")
         (dark-gray "#656565")
         (darker-gray "#454545")
         (darkest-gray "#252525")
         (red "#cd5542")
         (dark-red "#9d2512")

         (cursor "#f57e00")
         (fringe "#1f1f1f")
         (region "#303030")

         (rb0 "#81b0e3")
         (rb1 "#a5a5a5")
         (rb2 "#6190c3")
         (rb3 "#959595")
         (rb4 "#4170a3")
         (rb5 "#757575")

         (bg "gray13")
         (fg "#bdbdb3"))
     ,@body))


(with-theme-colors
 (defface  my-parens       `((((class color)) (:foreground ,dark-gray))) "custom parens"  :group 'faces)
 (defface  my-braces       `((((class color)) (:foreground ,gray))) "custom braces"  :group 'faces)
 (defface  my-brackets     `((((class color)) (:foreground ,gray))) "custom brackets" :group 'faces)
 (defface  my-dot          `((((class color)) (:foreground ,dark-gray))) "custom brackets" :group 'faces)
 (defface  my-semis        `((((class color)) (:foreground ,dark-gray))) "custom semicolons" :group 'faces)
 (defface  my-double-quote `((((class color)) (:foreground ,green))) "custom special" :group 'faces))

(defvar tweak-syntax-blacklist '(magit-status-mode
                                 magit-log-mode
                                 magit-commit-mode
                                 magit-branch-manager-mode
                                 prodigy-mode
                                 prodigy-view-mode
                                 term-mode
                                 eshell-mode
                                 deft-mode
                                 haml-mode
                                 gfm-mode
                                 org-mode
                                 erc-mode))

(defun tweak-syntax ()
  (if (not (member major-mode tweak-syntax-blacklist))
      (mapcar (lambda (x) (font-lock-add-keywords nil x))
              '((("#?['`]*(\\|)" . 'my-parens))
                (("#?\\^?{\\|}" . 'my-braces))
                (("\\[\\|\\]" . 'my-brackets))
                (("\\." . 'my-dot))
                (("; *$" . 'my-semis))
                (("#?\"" 0 'my-double-quote prepend))
                (("#?\'" 0 'my-double-quote prepend))
                (("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 'font-lock-warning-face t))))))



(add-hook 'after-change-major-mode-hook 'tweak-syntax)


;;; parens
(with-theme-colors
 (custom-theme-set-faces 'ample
                         `(web-mode-html-tag-face ((t (:foreground ,purple))))
                         `(web-mode-html-tag-custom-face ((t (:foreground ,blue))))
                         `(web-mode-html-tag-bracket-face ((t (:foreground ,darker-gray))))
                         `(web-mode-html-attr-equal-face ((t (:foreground ,darker-gray))))
                         `(web-mode-html-attr-custom-face ((t (:foreground ,blue))))
                         `(web-mode-variable-name-face ((t (:foreground nil))))
                         `(web-mode-html-attr-name-face ((t (:foreground ,blue))))))


;;; general
(with-theme-colors
 (custom-theme-set-faces 'ample
                         `(trailing-whitespace ((t (:background ,darker-gray))))
                         `(anzu-mode-line ((t (:foreground ,orange))))
                         `(sm-pair-overlay-face ((t (:background "grey13"))))
                         `(column-enforce-face ((t (:underline ,darker-gray))))))



(provide 'appearance)
