
(let ((gc-cons-threshold most-positive-fixnum))

(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))

(push "/usr/local/bin" exec-path)
(push "/usr/bin" exec-path)
(push "/usr/local/share/npm/bin" exec-path)
(push (expand-file-name "~/scripts") exec-path)

(setenv "PATH" (concat "/usr/local/bin:" (expand-file-name "~/scripts") ":" (getenv "PATH")))
(setenv "PAGER" "emacsclient")

;; Setting rbenv path
(setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
(setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims") (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'alt)

(let ((secret-path (expand-file-name "~/Dropbox/secrets.el")))
  (when (file-exists-p secret-path)
    (load-file secret-path)))

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
  tags-revert-without-query 1
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

(use-package ample-theme
  :ensure t
  :init
  (load-theme 'ample t t)
  (enable-theme 'ample))

(use-package highlight-numbers
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package highlight-escape-sequences
  :ensure t
  :init
  (hes-mode))

;; only turn off menus if not osx
(if (not (eq system-type 'darwin))
    (menu-bar-mode -1))

(setq-default cursor-type '(bar . 1))

(let ((font "Operator Mono Light 16"))
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
                         `(web-mode-html-attr-custom-face ((t (:foreground ,blue :slant italic))))
                         `(web-mode-variable-name-face ((t (:foreground nil))))
                         `(web-mode-html-attr-name-face ((t (:foreground ,blue :slant italic))))))


;;; general
(with-theme-colors
 (custom-theme-set-faces 'ample
                         `(trailing-whitespace ((t (:background ,darker-gray))))
                         `(anzu-mode-line ((t (:foreground ,orange))))
                         `(sm-pair-overlay-face ((t (:background "grey13"))))
                         `(column-enforce-face ((t (:underline ,darker-gray))))))

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
                (:propertize mode-name face mode-line-mode-face)
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

;; ;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-line-position-face)
(make-face 'mode-line-mode-face)
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
                     :slant 'italic
                     :inherit 'mode-line-face)

 (set-face-attribute 'mode-line-filename-face nil
                     :slant 'italic
                     :inherit 'mode-line-face
                     :foreground yellow)

 (set-face-attribute 'mode-line-position-face nil
                     :foreground gray
                     :inherit 'mode-line-face)

 (set-face-attribute 'mode-line-line-position-face nil
                     :inherit 'mode-line-face)

 (set-face-attribute 'mode-line-mode-face nil
                     :slant 'italic)

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

(require 'mb-editing)
(require 'mb-start-message)
(require 'mb-toolbox)
(require 'mb-defuns)

(use-package helm :ensure t)
(use-package imenu-anywhere :ensure t)
(use-package htmlize :ensure t)
(use-package ag :ensure t)
(use-package paradox :ensure t)
(use-package esup :ensure t)
(use-package define-word :ensure t)
(use-package help+ :ensure t)
(use-package help-fns+ :ensure t)
(use-package help-mode+ :ensure t)
(use-package inf-mongo :ensure t)
(use-package discover-my-major :ensure t)

(use-package jump-char
  :ensure t
  :bind (("M-g" . jump-char-forward)
         ("M-G" . jump-char-backward)))

(use-package vkill
  :ensure t
  :commands (vkill))

(use-package goto-chg
  :ensure t
  :commands (goto-last-change goto-last-change-reverse)
  :bind (("C-o" . goto-last-change)
         ("C-O" . goto-last-change-reverse)))

(use-package expand-region
  :ensure t
  :commands (er/expand-region er/contract-region)
  :bind* (("M-<up>" . er/expand-region)
          ("M-<down>" . er/contract-region))
  :init
  (require 'expand-region))

(use-package etags-select
  :ensure t
  :bind ("M-." . etags-select-find-tag))

(use-package anzu
  :ensure t
  :bind* ("M-r" . anzu-query-replace-regexp))

(use-package page-break-lines
  :ensure t
  :init
  (global-page-break-lines-mode))

(use-package smart-newline
  :ensure t
  :init
  (smart-newline-mode +1))


(use-package anzu
  :ensure t
  :init
  (global-anzu-mode +1))

(use-package ws-butler
  :ensure t
  :init
  (ws-butler-global-mode +1))

(use-package fic-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'fic-mode))

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching nil)
  (projectile-global-mode))

(use-package avy
  :ensure t
  :commands avy-goto-word-1
  :bind* ("M-;" . avy-goto-word-1))

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t)
  (with-theme-colors
   (set-face-attribute 'vhl/default-face nil
                       :background darker-gray)))

(use-package super-save
  :ensure t
  :init
  (super-save-mode +1))

(use-package coffee-mode :ensure t)
(use-package yaml-mode :ensure t)

(use-package sass-mode
             :ensure t
             :mode "\\.sass\\.erb"
             :init
             (setq css-indent-offset 2))

(use-package scss-mode
             :ensure t
             :mode "\\.scss\\.erb"
             :init
             (setq css-indent-offset 2))

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
                                sql-interactive-mode elixir-mode
                                inferior-emacs-lisp-mode))
    (add-to-list 'ac-modes mode))


  ;; Exclude very large buffers from dabbrev
  (defun sanityinc/dabbrev-friend-buffer (other-buffer)
    (< (buffer-size other-buffer) (* 1 1024 1024)))

  (setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer))

(use-package yasnippet
  :ensure t
  :init
  (add-hook 'after-init-hook 'yas-global-mode)
  (add-hook 'term-mode-hook #'force-yasnippet-off)
  (add-hook 'shell-mode-hook #'force-yasnippet-off)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-reload-all)

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
(use-package company
  :ensure t
  :bind* (("<tab>" . tab-indent-or-complete)
          ("C-<return>" . company-complete-common))
  :init
  (setq company-dabbrev-downcase nil)
  (global-company-mode)
  (company-statistics-mode)
  (bind-keys :map company-active-map
             ("<escape>" . company-abort)))


(provide 'conf-company)

(use-package org-mode
  :bind (("M-L" . org-store-link)
         ("<f2>" . org-todo-list)
         ("<f3>" . org-agenda)
         :map 'org-mode-map
         ("M-=" . org-ctrl-c-ctrl-c)
         ("M-+" . mb/org-ctrl-c-with-arg)
         ("C-l" . org-insert-link)
         ("C-o" . org-open-at-point)
         ("M-t" . org-todo))
  :init
  (setq org-src-fontify-natively t)
  (defun mb/org-ctrl-c-with-arg ()
    (interactive)
    (org-ctrl-c-ctrl-c '(4)))
  (setq org-log-done t)
  (setq org-support-shift-select 'always)
  (setq org-agenda-files '("~/Dropbox/org/personal.org"
                           "~/Dropbox/org/work.org"))
  (add-hook 'org-mode-hook '(lambda ()
                              (org-defkey org-mode-map [(tab)] nil))))

(use-package git-timemachine :ensure t)
(use-package yagist :ensure t)
(use-package gh :ensure t)

(use-package open-github-from-here
  :commands open-github-from-here
  :init
  (setq open-github-from-here:command (expand-file-name "~/.emacs.d/make-github-url-from-file")))

(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode +1))

(use-package magit
  :ensure t
  :bind* (("<f8>" . magit-blame)
          ("<f1>" . magit-status))
  :commands (magit-status
             magit-blame
             magit-checkout
             magit-log-buffer))

(use-package magit-gh-pulls
  :ensure t
  :init
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

(use-package
multiple-cursors
  :ensure t
  :bind (("A-<down>" . mc/mark-next-like-this-word)
         ("A-<up>" . mc/skip-to-next-like-this))
  :init
  (require 'multiple-cursors)
  (bind-keys :map rectangle-mark-mode-map
             ("A-SPC" . mc/edit-lines)
             ("C-<left>" . mc/edit-beginnings-of-lines)
             ("C-<right>" . mc/edit-ends-of-lines)))

(use-package flycheck
  :ensure t
  :init
  (add-hook 'sh-mode-hook 'flycheck-mode)
  (add-hook 'json-mode-hook 'flycheck-mode)
  (add-hook 'nxml-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  (add-hook 'lisp-interaction-mode-hook 'flycheck-mode)
  (add-hook 'js2-mode-hook 'flycheck-mode)
  (add-hook 'ruby-mode-hook 'flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-indication-mode nil)

  :config
  (flycheck-add-mode 'javascript-eslint 'babel-mode))
(use-package flycheck-elm
  :ensure t
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-elm-setup)
  (add-hook 'elm-mode-hook 'flycheck-mode)
  (add-hook 'elm-mode-hook (lambda ()
    (message
    (setq default-directory (locate-dominating-file default-directory "elm-package.json"))))))

(defun wrap-round ()
  (interactive)
  (sp-wrap-with-pair "("))

(defun wrap-quote ()
  (interactive)
  (sp-wrap-with-pair "\""))

(defun wrap-square ()
  (interactive)
  (sp-wrap-with-pair "["))

(use-package smartparens
  :ensure t
  :init
  (setq
   sp-ignore-modes-list '(minibuffer-inactive-mode
                          markdown-mode
                          gfm-mode)
   sp-autoskip-closing-pair 'always
   blink-matching-paren t)
  (require 'smartparens-config)
  (show-smartparens-global-mode +1)
  (smartparens-global-mode +1)
  (bind-keys :map smartparens-mode-map
             ("M-k" . sp-kill-sexp)
             ("M-K"  . sp-splice-sexp)
             ("A-L" . sp-backward-barf-sexp)
             ("A-H" . sp-backward-slurp-sexp)
             ("A-h" . sp-forward-barf-sexp)
             ("A-l" . sp-forward-slurp-sexp)))

(use-package swiper
  :ensure t
  :commands (swiper)
  :bind* ("M-f" . swiper)
  :init
  (require 'ivy)
  (ivy-mode 1)
  (setq
   ivy-use-virtual-buffers t
   magit-completing-read-function 'ivy-completing-read)
  (bind-keys :map ivy-mode-map
             ("M-n" . ivy-next-line)
             ("M-p" . ivy-previous-line))
  (define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done))

(use-package smex
   :bind* ("M-A" . smex)
   :init
   (setq smex-completion-method 'ivy)
   (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))))

(use-package markdown-mode
             :ensure t
             :commands (markdown-mode
                        gfm-mode)
             :init
             (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
             (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
             (add-to-list 'auto-mode-alist '("\\.text$" . gfm-mode)))

(use-package jenkins
  :ensure t
  :config
  (setq
   jenkins-api-token alfred-token
   jenkins-url alfred-url
   jenkins-username alfred-user))

(defvar *mb/jenkins-timer* nil)

(defun mb/auto-refresh-jenkins ()
  (setq *mb/jenkins-timer*
        (run-at-time 0 5 #'mb/revert-jenkins-buffer)))

(defun mb/stop-auto-refreshing-jenkins ()
  (when *mb/jenkins-timer*
    (cancel-timer *mb/jenkins-timer*)
    (setq *mb/jenkins-timer* nil)))

(defun mb/revert-jenkins-buffer ()
  (when (get-buffer "*jenkins-status*")
    (with-current-buffer "*jenkins-status*"
                         (revert-buffer))))

(use-package inf-ruby :ensure t)
(use-package bundler :ensure t)
(use-package rubocop :ensure t)

(add-to-list 'auto-mode-alist '("\\.irbrc\\'" . ruby-mode))

(use-package rbenv
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'rbenv-use-corresponding)
  :config
  (rbenv-use-corresponding))

(use-package rspec-mode
  :ensure t
  :init
  (setq rspec-use-rake-when-possible nil)
  (setq rspec-spec-command "rspec")
  (setq rspec-use-spring-when-possible nil)
  (add-hook 'ruby-mode-hook 'rspec-mode)

  :config
  (defadvice rspec-compile (around rspec-compile-around)
    "Use BASH shell for running the specs because of ZSH issues."
    (let ((shell-file-name "/bin/bash"))
      ad-do-it))

  (bind-keys :map rspec-mode-map
             ("<return>" . reindent-then-newline-and-indent)
             ("M-t ;" . rspec-toggle-spec-and-target)
             ("M-t d" . rspec-disable-example)
             ("M-t e" . rspec-enable-example)
             ("M-t t" . rspec-verify-single)
             ("M-t l" . rspec-rerun)
             ("M-t f" . rspec-verify)
             ("M-t a" . rspec-verify-all))
  (ad-activate 'rspec-compile))

(use-package alchemist
  :ensure t
  :init
  (setq alchemist-test-status-modeline nil)
  (add-hook 'elixir-mode-hook 'flycheck-mode))

(use-package ac-alchemist
   :ensure t
   :init
   (add-hook 'elixir-mode-hook 'ac-alchemist-setup))

(defvar my-shells '("*main-shell*" "*alt-shell*"))
(require 'shell)

(setenv "PAGER" "cat")
(setenv "npm_config_progress" "false")

;; truncate buffers continuously
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

(defun make-my-shell-output-read-only (text)
  "Add to comint-output-filter-functions to make stdout read only in my shells."
  (if (member (buffer-name) my-shells)
      (let ((inhibit-read-only t)
            (output-end (process-mark (get-buffer-process (current-buffer)))))
        (put-text-property comint-last-output-start output-end 'read-only t))))
(add-hook 'comint-output-filter-functions 'make-my-shell-output-read-only)

(defun my-dirtrack-mode ()
  "Add to shell-mode-hook to use dirtrack mode in my shell buffers."
  (when (member (buffer-name) my-shells)
    (shell-dirtrack-mode 0)
    (set-variable 'dirtrack-list '("^.*[^ ]+:\\(.*\\)>" 1 nil))
    (dirtrack-mode 1)))
(add-hook 'shell-mode-hook 'my-dirtrack-mode)

; interpret and use ansi color codes in shell output windows
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun set-scroll-conservatively ()
  "Add to shell-mode-hook to prevent jump-scrolling on newlines in shell buffers."
  (set (make-local-variable 'scroll-conservatively) 10))
(add-hook 'shell-mode-hook 'set-scroll-conservatively)

(defun enter-again-if-enter ()
  "Make the return key select the current item in minibuf and shell history isearch.
An alternate approach would be after-advice on isearch-other-meta-char."
  (when (and (not isearch-mode-end-hook-quit)
             (equal (this-command-keys-vector) [13])) ; == return
    (cond ((active-minibuffer-window) (minibuffer-complete-and-exit))
          ((member (buffer-name) my-shells) (comint-send-input)))))
(add-hook 'isearch-mode-end-hook 'enter-again-if-enter)

(defadvice comint-previous-matching-input
    (around suppress-history-item-messages activate)
  "Suppress the annoying 'History item : NNN' messages from shell history isearch.
If this isn't enough, try the same thing with
comint-replace-by-expanded-history-before-point."
  (let ((old-message (symbol-function 'message)))
    (unwind-protect
      (progn (fset 'message 'ignore) ad-do-it)
    (fset 'message old-message))))

(defadvice comint-send-input (around go-to-end-of-multiline activate)
  "When I press enter, jump to the end of the *buffer*, instead of the end of
the line, to capture multiline input. (This only has effect if
`comint-eol-on-send' is non-nil."
  (flet ((end-of-line () (end-of-buffer)))
    ad-do-it))

;; not sure why, but comint needs to be reloaded from the source (*not*
;; compiled) elisp to make the above advise stick.
(load "comint.el.gz")

 (setq comint-get-old-input (lambda () "")) ; what to run when i press enter on a
                                            ; line above the current prompt

;; for other code, e.g. emacsclient in TRAMP ssh shells and automatically
;; closing completions buffers, see the links above.

(defun mb/load-shells ()
  (interactive)
  (mapc 'shell (reverse my-shells)))

(use-package
  prodigy
  :ensure t
  :init
  (setq prodigy-view-buffer-maximum-size 2048
        prodigy-view-truncate-by-default t)


  (prodigy-define-tag
    :name 'thin
    :ready-message "Listening")

  (prodigy-define-tag
    :name 'sidekiq
    :command "bundle"
    :args '("exec" "sidekiq")
    :ready-message "         sss")

  (prodigy-define-tag
    :name 'webpack
    :ready-message "webpack: bundle is now VALID.")

  (prodigy-define-tag
    :name 'rails
    :command "bundle"
    :args '("exec" "rails" "server"))

  (prodigy-define-tag
    :name 'nph
    :cwd "~/src/nph")

  ;; services

  (prodigy-define-service
    :name "NPH rails server"
    :tags '(nph rails thin))

  (prodigy-define-service
    :name "NPH sidekiq"
    :tags '(nph sidekiq))

  (prodigy-define-service
    :name "NPH webpack"
    :command "npm"
    :args '("run" "webpack")
    :tags '(nph webpack))

  (prodigy-define-service
    :name "NPH consumers"
    :command "bundle"
    :args '("exec" "rake" "messaging:consume")
    :ready-message "=> consuming..."
    :tags '(nph))

  (prodigy-define-service
    :name "Leo"
    :command "bin/leo"
    :ready-message "Leo Started"
    :cwd "~/leo")

  (prodigy-define-service
    :name "LabelBot"
    :command "bin/label-bot"
    :ready-message "LabelBot Started"
    :cwd "~/label-bot"))

(use-package elm-mode
  :ensure t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-elm))
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion))

(use-package web-mode
             :ensure t
             :init
             (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
             (setq web-mode-code-indent-offset 2
                   web-mode-markup-indent-offset 2))


(use-package emmet-mode
             :ensure t
             :init
             (add-hook 'web-mode-hook 'emmet-mode)
             (setq emmet-indentation 2))

(use-package json-mode :ensure t)
(use-package json-snatcher :ensure t)
(use-package js-doc :ensure t)

;; (use-package js-mode :mode ".js\\'")

(define-derived-mode babel-mode web-mode "Babel")

(defun mlb/babel-init ()
  (flycheck-mode +1)
  (web-mode-set-content-type "jsx"))

(add-hook 'babel-mode-hook 'mlb/babel-init)
(add-to-list 'auto-mode-alist '(".js\\'" . babel-mode))

(require 'server)
(unless (server-running-p)
  (server-start))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(modify-all-frames-parameters '((fullscreen . maximized)))

(cd "~")

)
