(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(when (display-graphic-p)
  (progn
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (set-frame-font "Monaco 14")
    (toggle-frame-maximized)))

(setq gc-cons-threshold 50000000)

(setq inhibit-startup-screen t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-auto-revert-mode t)

(global-linum-mode 1)
(setq linum-format "%3d ")

(setq column-number-mode t)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-day t))

(use-package paredit
  :ensure t
  :hook ((clojure-mode cider-repl-mode) . paredit-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (clojure-mode . rainbow-delimiters-mode))

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  (setq cider-save-file-on-load nil)
  (setq cider-auto-select-error-buffer nil)
;;  (setq cider-auto-jump-to-error nil)
  (setq cider-auto-select-test-report-buffer nil)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-repl-display-help-banner nil))

;; (use-package aggressive-indent
;;   :ensure t
;;   :config
;;   (add-hook 'clojure-mode-hook #'aggressive-indent-mode))

(use-package clj-refactor
  :ensure t
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-v"))

(use-package yasnippet
  :hook (clojure-mode . yas-minor-mode))

(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  :config
  (global-company-mode))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-global-mode +1))

(use-package helm
  :ensure t
  :config
  (progn
    (helm-mode 1)
    (setq helm-autoresize-mode t)
    (setq helm-buffer-max-length 40)
    (global-set-key (kbd "M-x") 'helm-M-x)))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package powerline
  :ensure t)

(use-package airline-themes
  :ensure t
  :config
  (load-theme 'airline-light t))

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-*") 'er/expand-region))

(use-package multiple-cursors
  :ensure t
  :config
  (define-key mc/keymap (kbd "<return>") nil))

(use-package graphql-mode
  :ensure t)

(use-package ensime
  :ensure t)

(use-package sbt-mode
  :ensure t)

(use-package scala-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

(defun find-buffer-regex (reg)
  (interactive)
  (remove-if-not #'(lambda (x) (string-match reg x))
                 (mapcar #'buffer-name (buffer-list))))

(defun cider-execute (command)
  (interactive)
  (set-buffer (car (find-buffer-regex "cider-repl.*")))
  (goto-char (point-max))
  (insert command)
  (cider-repl-return))

(defun nrepl-reset ()
  (interactive)
  (cider-execute "(reset)"))
(define-key cider-mode-map (kbd "C-c r") 'nrepl-reset)

(defun nrepl-test ()
  (interactive)
  (cider-execute "(test)"))
(define-key cider-mode-map (kbd "C-c t") 'nrepl-test)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" default)))
 '(package-selected-packages
   (quote
    (helm-ag ensime graphql-mode expand-region airline-themes powerline helm-projectile helm rainbow-delimiters paredit color-theme-sanityinc-tomorrow use-package)))
 '(safe-local-variable-values
   (quote
    ((cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")
     (cider-refresh-after-fn . "integrant.repl/resume")
     (cider-refresh-before-fn . "integrant.repl/suspend")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

