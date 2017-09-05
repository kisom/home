(defun ensure-package (package)
  (unless (package-installed-p package)
    (package-install package)))

(let ((initial-package-list
       '(anaconda-mode
	 auto-complete
	 cargo
	 irfc
	 magit
	 markdown-mode
	 rust-mode
	 scpaste
	 undo-tree)))
  (dolist (package initial-package-list)
    (ensure-package package)))
