(use-package
  prodigy
  :ensure t
  :init
  (setq prodigy-view-buffer-maximum-size 2048
        prodigy-view-truncate-by-default t)


  (prodigy-define-tag
    :name 'thin
    :ready-message "Listening")

  (prodigy-define-tag
    :name 'sidekiq
    :command "bundle"
    :args '("exec" "sidekiq")
    :ready-message "         sss")

  (prodigy-define-tag
    :name 'webpack
    :ready-message "webpack: bundle is now VALID.")

  (prodigy-define-tag
    :name 'rails
    :command "bundle"
    :args '("exec" "rails" "server"))

  (prodigy-define-tag
    :name 'nph
    :cwd "~/src/nph")

  ;; services

  (prodigy-define-service
    :name "NPH rails server"
    :tags '(nph rails thin))

  (prodigy-define-service
    :name "NPH sidekiq"
    :tags '(nph sidekiq))

  (prodigy-define-service
    :name "NPH webpack"
    :command "npm"
    :args '("run" "webpack")
    :tags '(nph webpack))

  (prodigy-define-service
    :name "NPH consumers"
    :command "bundle"
    :args '("exec" "rake" "messaging:consume")
    :ready-message "=> consuming..."
    :tags '(nph))

  (prodigy-define-service
    :name "Leo"
    :command "bin/leo"
    :ready-message "Leo Started"
    :cwd "~/leo")

  (prodigy-define-service
    :name "LabelBot"
    :command "bin/label-bot"
    :ready-message "LabelBot Started"
    :cwd "~/label-bot"))

(provide 'conf-prodigy)
