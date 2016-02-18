(use-package jenkins
  :ensure t
  :config
  (setq
   jenkins-api-token alfred-token
   jenkins-url alfred-url
   jenkins-username alfred-user))


(provide 'conf-butler)
