;;; startup without syntax highlighting
;;; (global-font-lock-mode 0)

;; set up package handling
(require 'package)

(add-to-list 'package-archives
	     '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(require 'cl)
(let* ((home-dir (getenv "HOME"))
       (ensure-lisp (concatenate 'string home-dir "/.emacs.d/ensure.el")))
  (load ensure-lisp))
 
;; reduce brain damage
(tool-bar-mode 0)
(menu-bar-mode 0)
(setq inhibit-startup-screen t)
(setq display-time-24hr-format t)
(display-time-mode)
(column-number-mode)

;; useful when writing
(global-set-key (kbd "C-c w") 'count-words)

;; hippie-expand is the best
(require 'hippie-exp)
(require 'auto-complete)
(global-auto-complete-mode t)
(ac-set-trigger-key "<C-tab>")
(global-set-key (kbd "<C-tab>") 'ac-expand)

;; eshell is pretty okay
(global-set-key (kbd "C-x m") 'eshell)

;; ido-mode makes finding files way more awesome
;;    note: C-x C-f C-f will kick back to normal find-file for when ido's tab
;;    completion is getting in the way.
(require 'ido)
(ido-mode 1)

;; magit, not yours
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; undo-tree is undo done right
(require 'undo-tree)
(global-undo-tree-mode)

;; i like refilling paragraphs
(global-set-key (kbd "M-q") 'fill-paragraph)

;; i install things to /usr/local
(add-to-list 'exec-path "/home/kyle/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/home/kyle/anaconda3/bin")

;; tell me where i'm at
(column-number-mode)

;;; i like cua-rectangle
(cua-mode t)
(cua-selection-mode 'emacs)
(global-set-key (kbd "M-RET") 'cua-rectangle-mark-mode)

(require 'scpaste)
(setq scpaste-http-destination "https://p.kyleisom.net"
      scpaste-scp-destination "p.kyleisom.net:sites/p/")

;;; useful for writing
(global-set-key (kbd "C-x w") 'count-words)

;;; used with pollen
(global-set-key (kbd "C-c C-d")
		(lambda () (interactive) (insert "\u25ca")))

(require 'irfc)
(require 'markdown-mode)

;; python stuff
(add-hook 'python-mode-hook 'anaconda-mode)

;; golang stuff
(setq gofmt-command "goimports")
(require 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)

(when (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (ensure-package 'slime)
  ;; Replace "sbcl" with the path to your implementation
  (setq inferior-lisp-program "sbcl"))

;;; 
;;;                                                      _:_
;;;                                                     '-.-'
;;;                                            ()      __.'.__
;;;                                         .-:--:-.  |_______|
;;;                                  ()      \____/    \=====/
;;;                                  /\      {====}     )___(
;;;                       (\=,      //\\      )__(     /_____\
;;;       __    |'-'-'|  //  .\    (    )    /____\     |   |
;;;      /  \   |_____| (( \_  \    )__(      |  |      |   |
;;;      \__/    |===|   ))  `\_)  /____\     |  |      |   |
;;;     /____\   |   |  (/     \    |  |      |  |      |   |
;;;      |  |    |   |   | _.-'|    |  |      |  |      |   |
;;;      |__|    )___(    )___(    /____\    /____\    /_____\
;;;     (====)  (=====)  (=====)  (======)  (======)  (=======)
;;;     }===={  }====={  }====={  }======{  }======{  }======={
;;;    (______)(_______)(_______)(________)(________)(_________)
(setq chess-ai-depth 2)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(chess-default-display (quote chess-plain))
 '(custom-safe-themes
   (quote
    ("4561c67b0764aa6343d710bb0a6f3a96319252b2169d371802cc94adfea5cfc9" "5f95ce79b4a8870b3486b04de22ca2e0785b287da8779f512cdd847f42266989" default)))
 '(custom-theme-directory "~/.emacs.d/themes")
 '(global-font-lock-mode nil)
 '(package-selected-packages
   (quote
    (go-imports guile-scheme slime chess pelican-mode gnugo go go-autocomplete go-direx go-guru go-mode anaconda-mode markdown-mode irfc scpaste cargo undo-tree magit auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq +DEFAULT-THEME+ "weyland-yutani")
(defun toggle-fontlock ()
  (if (font-lock-mode)
      (progn
	(message "disabling font-lock-mode")
	(global-font-lock-mode 0))
    (progn
      (message "enabling font-lock-mode")
      (load-theme +DEFAULT-THEME+)
      (global-font-lock-mode t))))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(keychain-refresh-environment)

;;; Support for the fira-code font ligatures.
(when (window-system)
  (set-frame-font "Fira Code")
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
		 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
		 (36 . ".\\(?:>\\)")
		 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
		 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
		 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
		 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
		 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
		 (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
		 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
		 (48 . ".\\(?:x[a-zA-Z]\\)")
		 (58 . ".\\(?:::\\|[:=]\\)")
		 (59 . ".\\(?:;;\\|;\\)")
		 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
		 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
		 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
		 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
		 (91 . ".\\(?:]\\)")
		 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
		 (94 . ".\\(?:=\\)")
		 (119 . ".\\(?:ww\\)")
		 (123 . ".\\(?:-\\)")
		 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
		 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
		 )
	       ))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
			    `([,(cdr char-regexp) 0 font-shape-gstring])))))
