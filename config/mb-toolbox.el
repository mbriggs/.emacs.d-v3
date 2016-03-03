(provide 'mb-toolbox)

(use-package dash :ensure t)
(require 'dash)

(defvar *mb:tools* '(("log"                         . magit-log-buffer-file)
                     ("buffers"                     . ibuffer)
                     ("branch"                      . magit-checkout)
                     ("clean"                       . clean-up-buffer-or-region)
                     ("align"                       . align-regexp)
                     ("ag - project"                . ag-project)
                     ("ag - location"               . ag)
                     ("ag - file type"              . ag-project-files)
                     ("deft"                        . deft)
                     ("eval - last sexp"            . eval-last-sexp)
                     ("eval - buffer"               . eval-buffer)
                     ("eval - defun"                . eval-defun)
                     ("toggle final newline"        . mb/toggle-final-newline)
                     ("customize"                   . customize-group-other-window)
                     ("jenkins"                     . jenkins)
                     ("pivotal"                     . pivotal)
                     ("gist-list"                   . yagist-list)
                     ("gist"                        . yagist-region-or-buffer)
                     ("gist - private"              . yagist-region-or-buffer-private)
                     ("rake"                        . rake)
                     ("rake - regenerate"           . rake-regenerate-cache)
                     ("robe - start"                . robe)
                     ("robe - reload"               . robe-rails-refresh)
                     ("quelpa - upgrade"            . quelpa-self-upgrade)
                     ("bundle install"              . bundle-install)
                     ("erc"                         . start-erc)
                     ("git time machine"            . git-timemachine)
                     ("shell"                       . shell)
                     ("eshell"                      . eshell)
                     ("sql - postgres"              . sql-postgres)
                     ("open on github"              . open-github-from-here)
                     ("prodigy"                     . prodigy)
                     ("mongo"                       . inf-mongo)
                     ("delete this file and buffer" . mb/delete-this-buffer-and-file)
                     ("rename this file and buffer" . mb/rename-this-file-and-buffer)
                     ("packages"                    . paradox-list-packages)
                     ("occur"                       . occur)
                     ("kill"                        . vkill)
                     ("rubocop"                     . rubocop-autocorrect-current-file)
                     ("emacs dir"                   . dired-to-emacs-dir)))

(defun mb/toggle-final-newline ()
  (interactive)
  (if require-final-newline
      (progn
        (setq require-final-newline nil)
        (message "require-final-newline is OFF"))
    (progn
      (setq require-final-newline t)
      (message "require-final-newline is ON"))))

(defvar *mb:prev-tool* nil)
(defun mb/toolbox ()
  (interactive)
  (let* ((prompt (concat "Tool" (when *mb:prev-tool* (concat " (" *mb:prev-tool* ")")) ": "))
         (possible-choices (sort (-map 'car *mb:tools*) 'string<))
         (choice (or (ivy-completing-read prompt possible-choices)
                    *mb:prev-tool*)))
    (setq *mb:prev-tool* choice)
    (--> choice
         (assoc-string it *mb:tools*)
         (cdr it)
         (call-interactively it))))