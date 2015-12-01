;; cask
(require 'cask "/usr/local/Cellar/cask/0.7.4/cask.el")
(cask-initialize)

;; tabbar
;; (load-file "~/.emacs.d/tabbar.el")

;; hide menu bar
(menu-bar-mode -1)

;; smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; ido
(setq ido-everywhere t)
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-define-keys ()
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)
(setq ido-use-virtual-buffers t)

;; enable paredit-mode for lisp
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)

;; projectile
(projectile-global-mode)

;; highlight-parentheses-mode color
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-paren-colors (quote ("color-196" "color-220" "color-201" "color-46"))))

;; rainbow-delimiters for lisp
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'highlight-parentheses-mode)

;; flx-ido
(flx-ido-mode 1)

;; theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; idle-highlight-mode
(idle-highlight-mode 1)

;; show line number
(global-linum-mode 1)
(setq linum-format "%3d ")

;; auto reload file
(global-auto-revert-mode 1)

;; scrolling
(setq scroll-step 1 scroll-conservatively 10000)
(global-set-key [up] (lambda () (interactive) (scroll-down 1)))
(global-set-key [down] (lambda () (interactive) (scroll-up 1)))
(global-set-key [left] (lambda () (interactive) (scroll-right tab-width t)))
(global-set-key [right] (lambda () (interactive) (scroll-left tab-width t)))

;; don't display 'ls does not support --dired' error message
(setq dired-use-ls-dired nil)

;; auto-complete
(global-auto-complete-mode 1)
(define-key ac-mode-map (kbd "M-SPC") 'auto-complete)

;; imenu-anywhere
(global-set-key (kbd "C-c o") 'imenu-anywhere)

;; don't make backup file
(setq make-backup-files nil)

;; pbcopy for osx
(turn-on-pbcopy)

(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

(custom-set-faces
 '(vertical-border ((t (:foreground "brightblack")))))

(recentf-mode t)

;; clojure-mode
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(setq clojure-defun-style-default-indent t)

;; cider
(setq cider-auto-select-error-buffer nil)
(setq cider-prompt-save-file-on-load nil)
(add-hook 'cider-mode-hook #'eldoc-mode)

;; ac-cider
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

;; rainbow-delimiters
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'highlight-parentheses-mode)

(eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)

;; (eval-after-load 'flycheck
;;   '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(defun clj-refactor-setup ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-v"))

(add-hook 'clojure-mode-hook #'clj-refactor-setup)

;; disable magic requires
(setq cljr-magic-requires nil)
