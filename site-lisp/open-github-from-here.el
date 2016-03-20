;;; open-github-from-here.el ---

;; Copyright (C) 2013 by Yuki SHIBAZAKI

;; Author: Yuki SHIBAZAKI <shibayu36@gmail.com>
;; URL:
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; open-github-from-here is to open github file url from emacs,
;; such as https://github.com/shibayu36/emacs-open-github-from-here/blob/development/open-github-from-here.el#L31..L35.
;; select region, and M-x open-github-from-here

;; To use this package, add these lines to your init.el or .emacs file:
;;     (require 'open-github-from-here)

;;; Code:

(defvar open-github-from-here:command-dir
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(defvar open-github-from-here:command
  (expand-file-name "make-github-url-from-file" open-github-from-here:command-dir))

(defun open-github-from-here ()
  (interactive)
  (let ((github-url))
    (cond ((and (open-github-from-here:git-project-p) (use-region-p))
           (setq github-url (shell-command-to-string
                             (format "%s %s %d %d"
                                     open-github-from-here:command
                                     (file-name-nondirectory (buffer-file-name))
                                     (line-number-at-pos (region-beginning))
                                     (- (line-number-at-pos (region-end)) 1)))))
          ((and (open-github-from-here:git-project-p)
                (eq 1 (line-number-at-pos (point))))
           (setq github-url (shell-command-to-string
                             (format "%s %s"
                                     open-github-from-here:command
                                     (file-name-nondirectory (buffer-file-name))))))
          ((open-github-from-here:git-project-p)
           (setq github-url (shell-command-to-string
                             (format "%s %s %d"
                                     open-github-from-here:command
                                     (file-name-nondirectory (buffer-file-name))
                                     (line-number-at-pos (point)))))))
    (if github-url (browse-url github-url)
      (message "not currently in a git project!!"))))

(defun open-github-from-here:chomp (str)
  (replace-regexp-in-string "[\n\r]+$" "" str))

(defun open-github-from-here:git-project-p ()
  (string=
   (open-github-from-here:chomp
    (shell-command-to-string "git rev-parse --is-inside-work-tree"))
   "true"))

(provide 'open-github-from-here)

;;; open-github-from-here.el ends here
