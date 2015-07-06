(global-anzu-mode +1)

(require 'projectile)
(setq projectile-enable-caching t)
(projectile-global-mode)

(require 'ace-jump-mode)

(require 'evil)
(global-evil-surround-mode 1)
(evil-mode 1)

(provide 'global-modes)
