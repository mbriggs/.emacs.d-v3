(defvar butler-server-list)

(setq butler-server-list
      `((jenkins "Work"
                 (server-address . ,alfred-url)
                 (server-user . ,alfred-user)
                 (server-password . ,alfred-pass))))


(provide 'conf-butler)
