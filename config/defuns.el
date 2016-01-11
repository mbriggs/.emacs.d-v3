(require 'dash)
(require 's)
(eval-after-load "dash" '(dash-enable-font-lock))

(defun split-window-right-and-move-there ()
  (interactive)
  (split-window-right)
  (windmove-right))

(defun split-window-below-and-move-there ()
  (interactive)
  (split-window-below)
  (windmove-down))


(defun fancy-new-line-p ()
  (let ((blacklist '(term-mode)))
    (not (-contains? blacklist major-mode))))

(defun new-line-dwim ()
  (interactive)

  (if (fancy-new-line-p)
      (let ((break-open-pair (or (and (looking-back "{" 1) (looking-at "}"))
                                 (and (looking-back ">" 1) (looking-at "<"))
                                 (and (looking-back "(" 1) (looking-at ")"))
                                 (and (looking-back "\\[" 1) (looking-at "\\]")))))
        (newline)
        (when (break-open-pair)
          (save-excursion
            (newline)
            (indent-for-tab-command)))
        (indent-for-tab-command))
    (evil-ret)))

(defvar *test-patterns* '(("\\.rb$" . "_spec.rb")
                          ("\\.js$" . "-test.js")))
(defun testfilep (filename)
  )
(defun find-test-file ()
  (find-file (expand-file-name file (projectile-project-root))))


(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun create-scratch-buffer nil
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)))


(defun number-of-lines-in-buffer ()
  (+ 1 (count-lines (point-min) (point-max))))

(defun at-last-line-p ()
  (= (number-of-lines-in-buffer) (current-line-number)))

(defun ido-find-file-in-project-root ()
  (interactive)
  (ido-find-file-in-dir (projectile-project-root)))

(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun duplicate-region (&optional num start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
  (interactive "p")
  (let* ((start (or start (region-beginning)))
         (end (or end (region-end)))
         (region (buffer-substring start end)))
    (goto-char end)
    (dotimes (i num)
      (insert region)))
  (evil-normal-state)
  (evil-visual-restore))

(defun duplicate-current-line (&optional num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (save-excursion
    (when (eq (point-at-eol) (point-max))
      (goto-char (point-max))
      (newline)
      (forward-char -1))
    (duplicate-region num (point-at-bol) (1+ (point-at-eol)))))


(defun one-shot-keybinding (key command)
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd key) command)
     map) t))

(defun clean-up-buffer-or-region ()
  "Untabifies, indents and deletes trailing whitespace from buffer or region."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (untabify (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (delete-trailing-whitespace))))


(defun newline-anywhere ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun newline-on-previous-line-anywhere ()
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent))

(defun current-line-number ()
  (+ 1 (count-lines 1 (point))))

(defun send-current-line-to-next-window ()
  "Send current line to next window"
  (interactive)
  (let ((current-line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
        (target (window-buffer (next-window))))
    (with-current-buffer target
      (insert current-line))))

(defun reset-current-dir ()
  (interactive)
  (let ((dir (file-name-directory (buffer-file-name))))
    (cd dir)
    (message (concat "Set the current buffer directory to " dir))))

(defun what-face ()
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (symbol-name face) "")))

(defun my-delete-backwards ()
  (interactive)
  (delete-region (point) (progn (evil-backward-word-begin) (point))))

(defun add-to-js-globals ()
  (interactive)
  (let ((var (word-at-point)))
    (save-excursion
      (goto-char (point-min))
      (when (not (string-match "^/\\*global " (current-line)))
        (newline)
        (forward-line -1)
        (insert "/*global */"))
      (while (not (string-match "*/" (current-line)))
        (next-line))
      (end-of-line)
      (delete-char -2)
      (insert (concat var " */")))))

(defun new-line-in-normal-mode ()
  "make a new line without moving the cursor or leaving normal mode"
  (interactive)
  (save-excursion
    (evil-insert-newline-below)
    (evil-force-normal-state)))

(defun semi-colonize ()
  (interactive)
  (goto-char (point-min))
  (query-replace-regexp "^ *[^/]+[^;,{}\n.]$" "\\&;"))

(defun format-json ()
  (interactive)
  (let ((cmd "python -mjson.tool"))
    (shell-command-on-region (region-beginning) (region-end) cmd nil t)))

(defun copy-to-end-of-line ()
  (interactive)
  (copy-region-as-kill (point) (point-at-eol)))

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file name new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))


(defun fix-buffer-directory ()
  (interactive)
  (if buffer-file-name
      (setq default-directory
            (file-name-directory buffer-file-name))))

(defun switch-to-local-project ()
  (interactive)
  (let* ((prompt "Switch to project: ")
         (project-dir "~/src")
         (choices (actionable-files-in-directory project-dir))
         (project (ido-completing-read prompt choices nil t)))

    (find-file (concat project-dir "/" project))))

(defun dired-to-emacs-dir ()
  (interactive)
  (dired "~/.emacs.d"))

(defun view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    ;; TODO: switch to nxml/nxhtml mode
    (cond ((search-forward "<?xml" nil t) (xml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))

(defun actionable-files-in-directory (dir)
  (let ((files (directory-files dir))
        (blacklist '(".DS_Store" "." "..")))
    (-difference files blacklist)))


(defmacro allow-line-as-region-for-function (orig-function)
  `(defun ,(intern (concat (symbol-name orig-function) "-or-line"))
       ()
     ,(format "Like `%s', but acts on the current line if mark is not active."
              orig-function)
     (interactive)
     (if mark-active
         (call-interactively (function ,orig-function))
       (save-excursion
         ;; define a region (temporarily) -- so any C-u prefixes etc. are preserved.
         (beginning-of-line)
         (set-mark (point))
         (end-of-line)
         (call-interactively (function ,orig-function))))))

(allow-line-as-region-for-function comment-or-uncomment-region)

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))

         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2)))
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun create-scratch-buffer ()
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)))

(defun only-current-buffer ()
  (interactive)
  (mapc 'kill-buffer
        (-filter
         (lambda (buf) (not (s-starts-with? "*" (buffer-name buf))))
         (cdr (buffer-list (current-buffer))))))

(defun scroll-buffer-to-bottom-when-inactive (buffer-name)
  (let ((buffer (get-buffer buffer-name)))
    (unless (eq (current-buffer) buffer)
      (with-current-buffer buffer
        (scroll-to-bottom)))))

(defun scroll-to-bottom ()
  (goto-char (point-max)))

(provide 'defuns)
