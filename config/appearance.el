(use-package ample-theme
  :ensure t
  :init
  (load-theme 'ample t t)
  (enable-theme 'ample))

(use-package highlight-numbers
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package highlight-escape-sequences
  :ensure t
  :init
  (hes-mode))

;; only turn off menus if not osx
(if (not (eq system-type 'darwin))
    (menu-bar-mode -1))

(setq-default cursor-type '(bar . 1))

(defvar *code-quotes* '("Most of the biggest problems in software are problems of misconception. - Rich Hickey"
                        "Constraints are advantages in disguise. - 37 Signals"
                        "Don't have good ideas if you aren't willing to be responsible for them. - Alan Perlis"
                        "Fools ignore complexity. Pragmatists suffer it. Some can avoid it. Geniuses remove it. - Alan Perlis"
                        "The most important property of a program is whether it accomplishes the intention of its user. - C.A.R. Hoare"
                        "Inside every large program, there is a small program trying to get out. - C.A.R. Hoare"
                        "Organizations which design systems are constrained to produce designs which are copies of the communication structures of these organizations. - Conway's Law"
                        "Just because something is easy to measure doesn't mean it's important. - D.H.H."
                        "A program is like a poem: you cannot write a poem without writing it. - E.W. Dijkstra"
                        "Simplicity is prerequisite for reliability. - E.W. Dijkstra"
                        "Good programmers don't just write programs. They build a working vocabulary. - Guy Steele"
                        "Machines should work. People should think. - IBM Pollyanna Principle"
                        "Good software, like wine, takes time. - Joel Spolsky"
                        "Without great solitude no serious work is possible - Pablo Picasso"
                        "No code is faster than no code. - Merb Motto"
                        "A user interface should be so simple that a beginner in an emergency can understand it within ten seconds. - Ted Nelson"
                        "Make it work first before you make it work fast. - Bruce Whiteside"
                        "If you aren't sure which way to do something, do it both ways and see which works better. - John Carmack"
                        "Premature optimizations can be troublesome to revert, but premature generalizations are often near impossible. - Emil Persson"
                        "In programming, the hard part isn't solving problems, but deciding what problems to solve. - Paul Graham"
                        "Theory and practice sometimes clash. And when that happens, theory loses. Every single time. - Linus Torvalds"
                        "If we want users to like our software we should design it to behave like a likeable person: respectful, generous and helpful. - Alan Cooper"
                        "The lurking suspicion that something could be simplified is the world's richest source of rewarding challenges. - Edsger Dijkstra"
                        "The computing scientist's main challenge is not to get confused by the complexities of his own making. - E. W. Dijkstra"
                        "If testing seems hard, there's something wrong with your design. - Sandi Metz"
                        "Simple things should be simple, complex things should be possible. - Alan Kay"
                        "Simplicity and elegance are unpopular because they require hard work and discipline to achieve and education to be appreciated. - E.W. Dijkstra"
                        "Any fool can write code that a computer can understand.  Good programmers write code that humans can understand. - Martin Fowler"
                        "The purpose of abstraction is not to be vague, but to create a new semantic level in which one can be absolutely precise - Edsger Dijkstra"
                        "Programming is an art form that fights back. - Unknown"
                        "The computer programmer is a creator of universes for which he alone is responsible. - Joseph Weizenbaum"
                        "The effective exploitation of his powers of abstraction [is] one of the most vital activities of a competent programmer. - Edsger Dijkstra"
                        "No matter how slow you are writing clean code, you will always be slower if you make a mess. - Uncle Bob Martin"
                        "Good judgement is the result of experience. Experience is the result of bad judgement. - Fred Brooks"
                        "Anything that can possibly go wrong, will go wrong. - Murphy's Law"
                        "It always takes longer than you expect, even when you take into account Hofstadter's Law. — Hofstadter's Law"
                        "A clever person solves a problem. A wise person avoids it. — Albert Einstein"
                        "One accurate measurement is worth more than a thousand expert opinions. - Admiral Grace Hopper"))

(defun mb/fortune ()
  (nth (random (length *code-quotes*)) *code-quotes*))

(setq-default initial-scratch-message
              (concat ";; " (mb/fortune) "\n\n"))

(let ((font "Operator Mono Light 16"))
  (set-frame-font font)
  (add-to-list 'default-frame-alist
               `(font . ,font)))

(line-number-at-pos)

(add-hook 'after-change-major-mode-hook
          (lambda ()
            (when (and
                   (not (eq major-mode 'Custom-mode))
                   (not (eq major-mode 'shell-mode))
                   (not (eq major-mode 'emacs-pager-mode))
                   (not (eq major-mode 'term-mode))
                   (not (eq major-mode 'eshell-mode))
                   (not (eq major-mode 'ibuffer-mode))
                   (not (eq major-mode 'rspec-compilation-mode))
                   (not (eq major-mode 'prodigy-mode)))
              (setq show-trailing-whitespace t))))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))


(setq linum-format (lambda (line)
                     (propertize
                      (format (concat " %"
                                      (number-to-string
                                       (length (number-to-string
                                                (line-number-at-pos (point-max)))))
                                      "d ")
                              line)
                      'face 'linum)))

(use-package highlight-cl
             :ensure t
             :init
             (add-hook 'emacs-lisp-mode-hook 'highlight-cl-add-font-lock-keywords))

(defmacro with-theme-colors (&rest body)
  `(let ((green "#6aaf50")
         (dark-green "#057f40")
         (blue "#5180b3")
         (white "#bdbdb3")
         (blue-bg "#102843")
         (light-blue "#528fd1")
         (lighter-blue "#68a5e9")
         (orange "#dF9522")
         (tan "#bdbc61")
         (dark-tan "#7d7c61")
         (yellow "#baba36")
         (bright-yellow "#fffe0a")
         (purple "#ab75c3")
         (gray "#757575")
         (dark-gray "#656565")
         (darker-gray "#454545")
         (darkest-gray "#252525")
         (red "#cd5542")
         (dark-red "#9d2512")

         (cursor "#f57e00")
         (fringe "#1f1f1f")
         (region "#303030")

         (rb0 "#81b0e3")
         (rb1 "#a5a5a5")
         (rb2 "#6190c3")
         (rb3 "#959595")
         (rb4 "#4170a3")
         (rb5 "#757575")

         (bg "gray13")
         (fg "#bdbdb3"))
     ,@body))


(with-theme-colors
 (defface  my-parens       `((((class color)) (:foreground ,dark-gray))) "custom parens"  :group 'faces)
 (defface  my-braces       `((((class color)) (:foreground ,gray))) "custom braces"  :group 'faces)
 (defface  my-brackets     `((((class color)) (:foreground ,gray))) "custom brackets" :group 'faces)
 (defface  my-dot          `((((class color)) (:foreground ,dark-gray))) "custom brackets" :group 'faces)
 (defface  my-semis        `((((class color)) (:foreground ,dark-gray))) "custom semicolons" :group 'faces)
 (defface  my-double-quote `((((class color)) (:foreground ,green))) "custom special" :group 'faces))

(defvar tweak-syntax-blacklist '(magit-status-mode
                                 magit-log-mode
                                 magit-commit-mode
                                 magit-branch-manager-mode
                                 prodigy-mode
                                 prodigy-view-mode
                                 term-mode
                                 eshell-mode
                                 deft-mode
                                 haml-mode
                                 gfm-mode
                                 org-mode
                                 erc-mode))

(defun tweak-syntax ()
  (if (not (member major-mode tweak-syntax-blacklist))
      (mapcar (lambda (x) (font-lock-add-keywords nil x))
              '((("#?['`]*(\\|)" . 'my-parens))
                (("#?\\^?{\\|}" . 'my-braces))
                (("\\[\\|\\]" . 'my-brackets))
                (("\\." . 'my-dot))
                (("; *$" . 'my-semis))
                (("#?\"" 0 'my-double-quote prepend))
                (("#?\'" 0 'my-double-quote prepend))
                (("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 'font-lock-warning-face t))))))



(add-hook 'after-change-major-mode-hook 'tweak-syntax)


;;; parens
(with-theme-colors
 (custom-theme-set-faces 'ample
                         `(web-mode-html-tag-face ((t (:foreground ,purple))))
                         `(web-mode-html-tag-custom-face ((t (:foreground ,blue))))
                         `(web-mode-html-tag-bracket-face ((t (:foreground ,darker-gray))))
                         `(web-mode-html-attr-equal-face ((t (:foreground ,darker-gray))))
                         `(web-mode-html-attr-custom-face ((t (:foreground ,blue :slant italic))))
                         `(web-mode-variable-name-face ((t (:foreground nil))))
                         `(web-mode-html-attr-name-face ((t (:foreground ,blue :slant italic))))))


;;; general
(with-theme-colors
 (custom-theme-set-faces 'ample
                         `(trailing-whitespace ((t (:background ,darker-gray))))
                         `(anzu-mode-line ((t (:foreground ,orange))))
                         `(sm-pair-overlay-face ((t (:background "grey13"))))
                         `(column-enforce-face ((t (:underline ,darker-gray))))))



(provide 'appearance)
