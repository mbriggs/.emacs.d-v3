profile:
	open -a /Applications/Emacs.app/Contents/MacOS/Emacs --args \
	-Q -l ~/.emacs.d/site-lisp/profile-dotemacs.el \
	--eval "(setq profile-dotemacs-file (setq load-file-name \"$(abspath mbriggs.el)\"))" \
	-f profile-dotemacs
