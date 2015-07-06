(global-anzu-mode +1)

(require 'volatile-highlights)
(volatile-highlights-mode t)

(require 'session)
(session-initialize)

(require 'projectile)
(projectile-global-mode)

(require 'ace-jump-mode)

(provide 'global-modes)
