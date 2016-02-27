(use-package jenkins
  :ensure t
  :config
  (setq
   jenkins-api-token alfred-token
   jenkins-url alfred-url
   jenkins-username alfred-user))

(defvar *mb/jenkins-timer* nil)

(defun mb/auto-refresh-jenkins ()
  (setq *mb/jenkins-timer*
        (run-at-time 0 5 #'mb/revert-jenkins-buffer)))

(defun mb/stop-auto-refreshing-jenkins ()
  (when *mb/jenkins-timer*
    (cancel-timer *mb/jenkins-timer*)
    (setq *mb/jenkins-timer* nil)))

(defun mb/revert-jenkins-buffer ()
  (when (get-buffer "*jenkins-status*")
    (with-current-buffer "*jenkins-status*"
                         (revert-buffer))))

(provide 'conf-butler)
