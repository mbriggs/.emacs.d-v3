(require 'neotree)

(defun show-sidebar ()
  (interactive)
  (neotree-dir (projectile-project-root))
  (neotree-show))

(bind-key "." 'neotree-hidden-file-toggle neotree-mode-map)
(bind-key "R" 'neotree-rename-node neotree-mode-map)
(bind-key "D" 'neotree-delete-node neotree-mode-map)
(bind-key "N" 'neotree-create-node neotree-mode-map)

(provide 'conf-neotree)
