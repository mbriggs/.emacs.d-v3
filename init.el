(add-to-list 'load-path (expand-file-name "~/.emacs.d/package-config"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/config"))

(require 'cl)
(require 'cl-lib)

(push "/usr/local/bin" exec-path)
(push "/usr/bin" exec-path)
(push "/usr/local/share/npm/bin" exec-path)
(push (expand-file-name "~/scripts") exec-path)
(setenv "PATH" (concat "/usr/local/bin:" (expand-file-name "~/scripts") ":" (getenv "PATH")))
(setenv "PAGER" "emacsclient")

;; Setting rbenv path
(setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
(setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims") (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

(cd "~")

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'alt)

(when (memq window-system '(mac ns))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(mapc 'require '(settings
                 appearance
                 global-modes
                 defuns
                 keybinds
                 modeline

                 conf-evil
                 conf-company
                 conf-erc
                 conf-snippets
                 conf-parens
                 conf-flycheck
                 conf-volatile
                 conf-ido
                 conf-neotree
                 conf-markdown
                 conf-sass
                 conf-ruby
                 conf-elixir
                 conf-web
                 conf-js))

(let ((secret-path (expand-file-name "~/Dropbox/secrets.el")))
  (when (file-exists-p secret-path)
    (load-file secret-path)))

(require 'server)
(unless (server-running-p)
  (server-start))
(setq custom-file "~/.emacs.d/config/custom.el")
(load custom-file)
(modify-all-frames-parameters '((fullscreen . maximized)))
