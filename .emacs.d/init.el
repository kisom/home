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


;;; rust stuff
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;;; Project Interaction Library for Emacs
(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-project-search-path '("~/src/" "~/code/"))

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
    ("bf390ecb203806cbe351b966a88fc3036f3ff68cd2547db6ee3676e87327b311" "e1943fd6568d49ec819ee3711c266a8a120e452ba08569045dd8f50cc5ec5dd3" "4561c67b0764aa6343d710bb0a6f3a96319252b2169d371802cc94adfea5cfc9" "5f95ce79b4a8870b3486b04de22ca2e0785b287da8779f512cdd847f42266989" default)))
 '(custom-theme-directory "~/.emacs.d/themes")
 '(global-font-lock-mode t)
 '(package-selected-packages
   (quote
    (company-racer ac-racer racer erlang go-rename blackboard-bold-mode blacken jedi minimal-theme monochrome-theme monotropic-theme nimbus-theme noctilux-theme nord-theme nordless-theme northcode-theme paganini-theme paper-theme melancholy-theme go-imports guile-scheme slime chess pelican-mode gnugo go go-autocomplete go-direx go-guru go-mode anaconda-mode markdown-mode irfc scpaste cargo undo-tree magit auto-complete))))
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

;;; Load fira-code support.
(when (window-system)
  (set-frame-font "Fira Code 12"))
;; (load "~/.emacs.d/fira-code.el")
