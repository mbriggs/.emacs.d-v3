(provide 'custom)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-buffer-maximum-size 20000)
 '(comint-completion-addsuffix t)
 '(comint-input-ignoredups t)
 '(comint-input-ring-size 5000)
 '(comint-move-point-for-output nil)
 '(comint-prompt-read-only nil)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(deft-use-filename-as-title t)
 '(deft-use-filter-string-for-filename t)
 '(enh-ruby-deep-indent-paren nil)
 '(exec-path-from-shell-check-startup-files nil)
 '(js-indent-level 2)
 '(js2-language-version 200)
 '(neo-dont-be-alone t)
 '(neo-modern-sidebar t)
 '(neo-smart-open t)
 '(neo-theme (quote ascii))
 '(neo-vc-integration (quote (face)))
 '(package-selected-packages
   (quote
    (js-doc json-mode emmet-mode web-mode elm-mode prodigy ac-alchemist alchemist rspec-mode rbenv rubocop bundler inf-ruby jenkins markdown-mode swiper smartparens flycheck-elm flycheck multiple-cursors magit-gh-pulls magit diff-hl gh yagist git-timemachine company-statistics auto-yasnippet yasnippet auto-complete scss-mode sass-mode yaml-mode coffee-mode super-save volatile-highlights avy projectile fic-mode ws-butler smart-newline page-break-lines anzu etags-select expand-region vkill jump-char discover-my-major inf-mongo help-mode+ help-fns+ help+ define-word esup paradox ag htmlize imenu-anywhere helm s dash goto-chg highlight-cl highlight-escape-sequences highlight-numbers ample-theme use-package)))
 '(paradox-automatically-star t)
 '(protect-buffer-bury-p nil)
 '(safe-local-variable-values
   (quote
    ((eval add-hook
           (quote after-save-hook)
           (lambda nil
             (org-babel-tangle))
           nil t))))
 '(tramp-default-method "ssh"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-confirm-face ((t (:inherit minibuffer-prompt :foreground "#6aaf50"))))
 '(ivy-current-match ((t (:background "#5180b3" :foreground "black"))))
 '(ivy-match-required-face ((t (:inherit minibuffer-prompt :foreground "#cd5542"))))
 '(ivy-minibuffer-match-face-1 ((t (:background "#999"))))
 '(ivy-minibuffer-match-face-2 ((t (:background "#757575" :weight bold))))
 '(ivy-minibuffer-match-face-3 ((t (:background "#528fd1" :weight bold))))
 '(ivy-minibuffer-match-face-4 ((t (:background "#ab75c3" :weight bold))))
 '(mc/cursor-face ((t (:background "#dF9522"))))
 '(mc/region-face ((t (:foreground "#5180b3" :underline "#dF9522" :weight bold))))
 '(paren-face-match ((t (:background "#454545" :foreground "#dF9522" :weight bold))))
 '(paren-face-mismatch ((t (:background "#ab75c3" :foreground "white"))))
 '(paren-face-no-match ((t (:background "#cd5542" :foreground "nil"))))
 '(show-paren-match ((t (:background "nil" :foreground "#dF9522" :weight bold))))
 '(sp-pair-overlay-face ((t nil)))
 '(swiper-line-face ((t (:background "#454545"))))
 '(swiper-match-face-1 ((t (:inherit isearch-lazy-highlight-face)))))
