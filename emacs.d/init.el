(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(let ((is-emacs-mac-by-yamamaoto
       (and (boundp 'mac-carbon-version-string)
            (string= window-system "mac"))))
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier nil)
  (setq mac-pass-command-to-system 't)
  (mac-auto-operator-composition-mode)
  (setq ring-bell-function 'ignore))

(unless package-archive-contents
  (package-refresh-contents))

(when (display-graphic-p)
  (progn
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (set-frame-font "Fira Code Retina 13")
    (toggle-frame-maximized)))

(setq inhibit-startup-screen t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-auto-revert-mode t)

(global-linum-mode 1)
(setq linum-format "%3d ")
(set-default 'line-spacing 0.1)
(set-default 'truncate-lines t)

(delete-selection-mode 1)

(setq column-number-mode t)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; (use-package base16-theme
;;   :ensure t
;;   :config
;;   (load-theme 'base16-tomorrow t))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-day t))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode clojure-mode cider-repl-mode) . paredit-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (clojure-mode . rainbow-delimiters-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (progn
    (require 'flycheck-clj-kondo)
    (setq clojure-align-forms-automatically t)
    (setq clojure-align-binding-forms nil)
    (setq clojure-align-cond-forms nil)
    (define-clojure-indent
      (cond-> 0))))

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  (setq cider-save-file-on-load t)
  (setq cider-auto-select-error-buffer nil)
  (setq cider-auto-select-test-report-buffer nil)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-repl-display-help-banner nil)
  (setq cljr-warn-on-eval nil))

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
  (setq company-minimum-prefix-length 1)
  :config
  (global-company-mode))

(use-package company-statistics
  :ensure t
  :config
  (company-statistics-mode))

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
  :bind
  ("C-=" . er/expand-region)
  ("C--". er/contract-region))

(use-package multiple-cursors
  :ensure t
  :config
  (define-key mc/keymap (kbd "<return>") nil))

(use-package command-log-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
        (setq markdown-fontify-code-blocks-natively t))

(use-package edit-indirect
  :ensure t)

(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-set-close-button nil)
  (setq centaur-tabs-set-modified-marker t)
  (centaur-tabs-group-by-projectile-project)
  (setq centaur-tabs-modified-marker "*")
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

(use-package magit
  :ensure t)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (treemacs-resize-icons 16)
  (add-hook 'projectile-after-switch-project-hook 'treemacs-display-current-project-exclusively)
  :bind
  (:map global-map
	("C-x t t" . treemacs)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(require 'cl)

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
  (cider-execute "(reset)")
  (message "Reset"))

(define-key cider-mode-map (kbd "C-c r") 'nrepl-reset)

(defun start-cider-repl-with-profile ()
  (interactive)
  (letrec ((profile (read-string "Enter profile name: "))
           (lein-params (concat "with-profile " profile " repl :headless")))
    (message "lein-params set to: %s" lein-params)
    (set-variable 'cider-lein-parameters lein-params)
    (cider-jack-in '())))

(defun start-cider-with-local-profile ()
  (interactive)
  (set-variable 'cider-lein-parameters "with-profile +db/local,+env/local repl :headless")
  (cider-jack-in '()))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-completion-style (quote emacs))
 '(package-selected-packages
   (quote
    (highlight-symbol flycheck-clj-kondo base16-theme magit helm-ag treemacs-projectile treemacs centaur-tabs edit-indirect markdown-mode command-log-mode expand-region airline-themes powerline helm-projectile helm projectile company-statistics company clj-refactor cider clojure-mode rainbow-delimiters paredit color-theme-sanityinc-tomorrow use-package)))
 '(safe-local-variable-values
   (quote
    ((cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
