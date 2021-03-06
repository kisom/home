(defun ensure-package (package)
  (unless (package-installed-p package)
    (package-install package)))

(unless (file-directory-p "/home/kyle/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(let ((initial-package-list
       '(auto-complete
	 cargo
	 ;; chess
	 cider
	 geiser
	 ;; gnugo
	 go ;; play the game
	 go-autocomplete
	 go-direx
	 go-guru
	 go-mode
	 irfc
	 jedi
         keychain-environment
	 lua-mode
	 luarocks
	 magit
	 markdown-mode
	 paredit
	 pelican-mode
	 projectile
	 racket-mode
	 rust-mode
	 scpaste
	 slime
	 undo-tree)))
  (dolist (package initial-package-list)
    (ensure-package package)))
