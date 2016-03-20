(use-package org-mode
  :bind (("M-L" . org-store-link)
         ("<f2>" . org-todo-list)
         ("<f3>" . org-agenda)
         :map org-mode-map
         ("M-=" . org-ctrl-c-ctrl-c)
         ("M-+" . mb/org-ctrl-c-with-arg)
         ("C-l" . org-insert-link)
         ("C-o" . org-open-at-point)
         ("M-t" . org-todo))
  :init
  (defun mb/org-ctrl-c-with-arg ()
    (interactive)
    (org-ctrl-c-ctrl-c '(4)))
  (setq org-log-done t)
  (setq org-agenda-files '("~/Dropbox/org/personal.org"
                           "~/Dropbox/org/work.org")))



(provide 'conf-org)
