;;; init.el --- Eunmin Kim Emacs Configuration
;;; Commentary:

;;; Code:
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

(unless package-archive-contents
        (package-refresh-contents))

(when (display-graphic-p)
  (progn
   (tool-bar-mode -1)
   (set-frame-font "Monaco 14")
   (toggle-frame-maximized)))

(setq load-prefer-newer t)
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(set-face-attribute 'vertical-border nil :foreground "#494949")

(menu-bar-mode -1)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(fset 'yes-or-no-p 'y-or-n-p)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                 (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq require-final-newline t)

(delete-selection-mode t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-auto-revert-mode t)

(global-linum-mode 1)
(setq linum-format "%3d ")
(set-face-attribute 'linum nil :foreground "#555555")

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

(global-set-key (kbd "M-/") #'hippie-expand)

(global-set-key (kbd "C-x C-b") #'ibuffer)

(global-set-key (kbd "C-x \\") #'align-regexp)

(define-key 'help-command (kbd "C-f") #'find-function)
(define-key 'help-command (kbd "C-k") #'find-function-on-key)
(define-key 'help-command (kbd "C-v") #'find-variable)
(define-key 'help-command (kbd "C-l") #'find-library)

(define-key 'help-command (kbd "C-i") #'info-display-manual)

(global-set-key (kbd "s-<") #'beginning-of-buffer)
(global-set-key (kbd "s->") #'end-of-buffer)
(global-set-key (kbd "s-q") #'fill-paragraph)
(global-set-key (kbd "s-x") #'execute-extended-command)

(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

(unless (package-installed-p 'use-package)
        (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(use-package pbcopy
             :ensure t
             :config
             (turn-on-pbcopy))

(use-package lisp-mode
             :config
             (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
             (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
             (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
             (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
             (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
             (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package ielm
             :config
             (add-hook 'ielm-mode-hook #'eldoc-mode)
             (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode))

(use-package zenburn-theme
             :ensure t
             :config
             (load-theme 'zenburn t))

(use-package avy
             :ensure t
             :bind (("s-." . avy-goto-word-or-subword-1)
                    ("s-," . avy-goto-char))
             :config
             (setq avy-background t))

(use-package ag
             :ensure t)

(use-package projectile
             :ensure t
             :config
             (projectile-global-mode +1))

(use-package pt
             :ensure t)

(use-package expand-region
             :ensure t
             :bind ("C-@" . er/expand-region))

(use-package elisp-slime-nav
             :ensure t
             :config
             (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
                     (add-hook hook #'elisp-slime-nav-mode)))

(use-package paredit
             :ensure t
             :config
             (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
             (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
             (add-hook 'ielm-mode-hook #'paredit-mode)
             (add-hook 'lisp-mode-hook #'paredit-mode)
             (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package paren
             :config
             (show-paren-mode +1))

(use-package abbrev
             :config
             (setq save-abbrevs 'silently)
             (setq-default abbrev-mode t))

(use-package uniquify
             :config
             (setq uniquify-buffer-name-style 'forward)
             (setq uniquify-separator "/")
             (setq uniquify-after-kill-buffer-p t)
             (setq uniquify-ignore-buffers-re "^\\*"))

(use-package windmove
             :config
             (windmove-default-keybindings))

(use-package dired
             :config
             (put 'dired-find-alternate-file 'disabled nil)
             (setq dired-recursive-deletes 'always)
             (setq dired-recursive-copies 'always)
             (setq dired-dwim-target t)
             (setq dired-use-ls-dired nil)
             (require 'dired-x))

(use-package anzu
             :ensure t
             :bind (("M-%" . anzu-query-replace)
                    ("C-M-%" . anzu-query-replace-regexp))
             :config
             (global-anzu-mode))

(use-package easy-kill
             :ensure t
             :config
             (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package exec-path-from-shell
             :ensure t
             :config
             (when (memq window-system '(mac ns))
               (exec-path-from-shell-initialize)))

(use-package move-text
             :ensure t
             :bind
             (([(meta shift up)] . move-text-up)
              ([(meta shift down)] . move-text-down)))

(use-package rainbow-delimiters
             :ensure t)

(use-package rainbow-mode
             :ensure t
             :config
             (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package whitespace
             :init
             (dolist (hook '(prog-mode-hook text-mode-hook))
                     (add-hook hook #'whitespace-mode))
             (add-hook 'before-save-hook #'whitespace-cleanup)
             :config
             (setq whitespace-line-column 80)
             (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package clojure-mode
             :ensure t
             :config
             (add-hook 'clojure-mode-hook #'paredit-mode)
             (add-hook 'clojure-mode-hook #'subword-mode)
             (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
             (define-clojure-indent
               (ANY 2)
               (DELETE 2)
               (GET 2)
               (HEAD 2)
               (OPTIONS 2)
               (PATCH 2)
               (POST 2)
               (PUT 2)
               (context 2)
               (defroutes 'defun)
               (let-routes 1)
               (rfn 2)))

(defvar eval-timer nil)

(use-package cider
             :ensure t
             :config
             (defun cider-format-buffer-back ()
               (interactive)
               (let (p)
                 (setq p (point))
                 (cider-format-buffer)
                 (goto-char p)))
             (add-hook 'cider-mode-hook #'eldoc-mode)
             (add-hook 'before-save-hook 'cider-format-buffer-back 't)
             (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
             (add-hook 'cider-repl-mode-hook #'eldoc-mode)
             (add-hook 'cider-repl-mode-hook #'paredit-mode)
             (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
             (defun auto-eval-buffer (begin end length)
               (when (and (boundp 'cider-mode) cider-mode)
                 (when (timerp eval-timer)
                   (cancel-timer eval-timer))
                 (setq eval-timer
                       (run-at-time
                        "0.5 sec"
                        nil
                        '(lambda ()
                                 (when (buffer-file-name)
                                   (cider-eval-buffer)))))))
  ;; (add-hook 'cider-mode-hook
  ;;           '(lambda () (add-hook 'after-change-functions 'auto-eval-buffer)))
             (setq cider-prompt-save-file-on-load nil)
             (setq cider-auto-select-error-buffer nil)
             (setq cider-auto-jump-to-error nil)
             (setq cider-auto-select-test-report-buffer nil)
             (cider-auto-test-mode 1))

(use-package memoize
             :ensure t)

;; (use-package ido
;;   :ensure t
;;   :config
;;   (ido-mode 1))

(use-package ido-vertical-mode
             :ensure t
             :config
             (setq ido-use-faces t)
             (set-face-attribute 'ido-vertical-first-match-face nil
                                 :background nil
                                 :foreground "orange")
             (set-face-attribute 'ido-vertical-only-match-face nil
                                 :background nil
                                 :foreground nil)
             (set-face-attribute 'ido-vertical-match-face nil
                                 :foreground nil)
             (setq ido-vertical-define-keys 'C-n-and-C-p-only)
             (ido-vertical-mode 1))

(use-package flx-ido
             :ensure t
             :config
             (flx-ido-mode 1))

(use-package smex
  ;; Smex is a M-x enhancement for Emacs. Built on top of Ido, it provides
  ;; a convenient interface to your recently and most frequently used commands.
  ;; And to all the other commands, too.
  ;; https://github.com/nonsequitur/smex
             :ensure t
             :bind ("M-x" . smex))

(use-package markdown-mode
             :ensure t)

(use-package cask-mode
             :ensure t)

(use-package company
             :ensure t
             :config
             (global-company-mode)
             (setq company-idle-delay 0)
             (setq company-minimum-prefix-length 1)
             (global-set-key (kbd "TAB") #'company-indent-or-complete-common))

(use-package zop-to-char
             :ensure t
             :bind (("M-z" . zop-up-to-char)
                    ("M-Z" . zop-to-char)))

(use-package imenu-anywhere
             :ensure t
             :bind (("C-c i" . imenu-anywhere)
                    ("s-i" . imenu-anywhere)))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-popup-tip
  :ensure t
  :config
  (eval-after-load 'flycheck (flycheck-popup-tip-mode)))

(use-package flycheck-clojure
  :ensure t
  :config
  (eval-after-load 'flycheck '(flycheck-clojure-setup)))

(use-package diff-hl
             :ensure t
             :config
             (global-diff-hl-mode +1)
             (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
             (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package which-key
             :ensure t
             :config
             (which-key-mode +1))

(use-package undo-tree
             :ensure t
             :config
             (setq undo-tree-history-directory-alist
                   `((".*" . ,temporary-file-directory)))
             (setq undo-tree-auto-save-history t))

(use-package aggressive-indent
             :ensure t
             :config
             (add-hook 'clojure-mode-hook #'aggressive-indent-mode))

(use-package clj-refactor
             :ensure t
             :config
             (yas-minor-mode 1)
             (cljr-add-keybindings-with-prefix "C-c C-v")
             (setq cljr-magic-requires nil)
             (setq cljr-favor-prefix-notation nil)
             (setq cljr-auto-clean-ns nil)
             (add-hook 'clojure-mode-hook #'clj-refactor-mode))

(use-package flx-ido
             :ensure t)

(use-package hindent
             :ensure t)

(use-package ghc
             :ensure t)

(use-package haskell-mode
             :ensure t
             :config
             (autoload 'ghc-init "ghc" nil t)
             (autoload 'ghc-debug "ghc" nil t)
             (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
             (add-hook 'haskell-mode-hook 'company-mode)
             (add-hook 'haskell-mode-hook #'hindent-mode)
             (add-hook 'haskell-mode-hook 'structured-haskell-mode)
             (setq haskell-tags-on-save t))

(use-package company-ghc
             :ensure t
             :config
             (setq company-ghc-show-info t))

(use-package bs
             :ensure t
             :config
             (global-set-key (kbd "C-x <left>") 'bs-cycle-next)
             (global-set-key (kbd "C-x <right>") 'bs-cycle-previous))

(use-package elm-mode
             :ensure t
             :config
             (add-hook 'elm-mode-hook
                       (lambda ()
                               (setq company-backends '(company-elm)))))

(use-package flycheck-elm
             :ensure t
             :config
             (add-hook 'flycheck-mode-hook 'flycheck-elm-setup))

(use-package hungry-delete
             :ensure t
             :pin melpa)

(use-package string-inflection
             :ensure t
             :config
             (global-set-key (kbd "C-c C-i") 'string-inflection-all-cycle))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck-tip elm-oracle elm-mode ghc zop-to-char zenburn-theme yaml-mode which-key use-package undo-tree tabbar super-save smex rainbow-mode rainbow-delimiters pt projectile pbcopy move-text markdown-mode magit inf-ruby imenu-anywhere hindent haskell-mode flycheck flx-ido expand-region exec-path-from-shell erlang elixir-mode elisp-slime-nav easy-kill diff-hl crux company clj-refactor cask-mode avy anzu aggressive-indent ag)))
 '(safe-local-variable-values
   (quote
    ((cider-refresh-after-fn . "integrant.repl/resume")
     (cider-refresh-before-fn . "integrant.repl/suspend")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
