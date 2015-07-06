(global-anzu-mode +1)

(require 'volatile-highlights)
(volatile-highlights-mode t)

(require 'projectile)
(setq projectile-enable-caching t)
(projectile-global-mode)

(require 'ace-jump-mode)

(provide 'global-modes)
