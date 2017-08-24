(defun ensure-package (package)
  (unless (package-installed-p package)
    (package-install package)))

(let ((initial-package-list
       '(auto-complete
	 cargo
	 magit
	 rust-mode
	 undo-tree)))
  (dolist (package initial-package-list)
    (ensure-package package)))
