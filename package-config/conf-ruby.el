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

(eval-after-load "ruby-mode"
  '(progn
    (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
    (define-key ruby-mode-map (kbd "SPC") 'ruby-method-space-replace)))

(eval-after-load "rspec-mode"
  '(progn
     (setq rspec-use-rake-flag nil)
     (setq rspec-spec-command "rspec")
     (setq rspec-use-spring-when-possible t)))

(provide 'conf-ruby)
