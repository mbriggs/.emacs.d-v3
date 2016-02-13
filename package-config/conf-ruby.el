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
     (bind-keys :map ruby-mode-map
                ("M-t t" . rspec-verify-single)
                ("M-t l" . rspec-rerun)
                ("M-t f" . rspec-verify)
                ("M-t a" . rspec-verify-all))))

(provide 'conf-ruby)
