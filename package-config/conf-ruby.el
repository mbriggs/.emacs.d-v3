(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rabl$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(defun ruby-method-space-replace ()
  "When pressing space while naming a defined method, insert an underscore"
  (interactive)
  (if (and (looking-back "def .+")
           (not (and
                 (looking-at ".*)$")
                 (looking-back "(.*"))))
      (insert "_")
    (insert " ")))

(eval-after-load 'company
  '(push 'company-robe company-backends))
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'eldoc-mode)
(eval-after-load "ruby-mode"
  '(progn
     (rbenv-use-corresponding)
     (evil-ex-define-cmd "robe" 'robe-start)
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (define-key ruby-mode-map (kbd "SPC") 'ruby-method-space-replace)))

(eval-after-load "rspec-mode"
  '(progn
     (defadvice rspec-compile (around rspec-compile-around)
       "Use BASH shell for running the specs because of ZSH issues."
       (let ((shell-file-name "/bin/bash"))
         ad-do-it))

     (ad-activate 'rspec-compile)
     (setq rspec-use-rake-flag nil)
     (setq rspec-spec-command "rspec")
     (setq rspec-use-spring-when-possible t)
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (define-key ruby-mode-map (kbd "SPC") 'ruby-method-space-replace)

     (evil-define-key 'visual ruby-mode-map
       ",rm" 'ruby-refactor-extract-to-method
       ",rv" 'ruby-refactor-extract-local-variable
       ",rl" 'ruby-refactor-extract-to-let)


     (bind-key "M-T" 'rspec-verify-single ruby-mode-map)
     (bind-key "M-t" 'rspec-rerun ruby-mode-map)
     (evil-define-key 'normal ruby-mode-map
       ",t," 'projectile-toggle-between-implementation-and-test
       ",tf" 'rspec-verify
       ",ta" 'rspec-verify-all
       ",tt" 'rspec-verify-single
       ",tl" 'rspec-rerun
       ",rv" 'ruby-refactor-extract-local-variable
       ",ra" 'ruby-refactor-add-parameter
       ",rl" 'ruby-refactor-extract-to-let)))

(provide 'conf-ruby)
