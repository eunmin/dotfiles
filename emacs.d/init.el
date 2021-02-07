;;; init.el -- Todd's Emacs Configuration

;;; Commentary:

;; 이멕스 설정 파일

;;; Code:

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
;; 로컬 캐시가 없을 때 패키지 메타데이터 업데이트
(unless package-archive-contents
  (package-refresh-contents))

;; 항상 새로운 바이트 코드를 읽음
(setq load-prefer-newer t)

;; GC가 자주 일어나지 않도록 크기를 늘림 (100M), 기본 값은 0.76MB
(setq gc-cons-threshold 100000000)

;; 벨 소리 안나도록 설정
(setq ring-bell-function 'ignore)

;; 커서 깜빡이지 않게 하기
(blink-cursor-mode -1)

;; 커서 색상
(set-cursor-color "#EBCB8B")

;; 시작 화면 안나오게 하기
(setq inhibit-startup-screen t)

;; 실행 중인 프로세스가 있어도 그냥 종료
(setq confirm-kill-processes nil)

;; 시작 할 때 풀스크린으로 띄우기
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; 툴바 안나오게 하기
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; 스크롤 자연스럽게 되게 하기
(setq scroll-margin 0
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; 스크롤 바는 표시하지 않기
(when (display-graphic-p)
  (scroll-bar-mode -1))

;; 물어볼 때 yes/no 대신 y/n으로 답할 수 있도록 하기
(fset 'yes-or-no-p 'y-or-n-p)

;; 라인 번호 표시하기
(global-linum-mode t)
(require 'linum)
(setq linum-format "%3d ")

;; 컬럼 번호 표시하기 (윈도우 우측 하단에 표시)
(column-number-mode t)

;; 라인 간격 기본값에서 조금 늘리기
(set-default 'line-spacing 0.1)

;; 저장할 때 뒤에 붙은 공백 지워주고 파일 맨 끝에 엔터 추가하기
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

;; 영역을 선택했을 때 키를 누르면 지워지고 누른 키가 입력
(delete-selection-mode t)

;; 임시 파일들은 tmp 디렉토리에 만들기
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; 임시 파일이 있다면 자동으로 복구
(global-auto-revert-mode t)

;; 항상 utf-8 인코딩 사용하기
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; 버퍼 삭제하면서 창 닫기
(global-set-key (kbd "s-w") 'kill-buffer-and-window)

;; 120 라인이 넘어갈 때만 자동으로 세로로 창 분할하기
(setq split-height-threshold 120)

;; 전체 창이 작아저도 라인이 짤리지 않도록 하기
(set-default 'truncate-lines t)

;; 윈도우 시스템 일 때만
(when (window-system)
  ;; 기본 폰트 설정
  (set-frame-font "Cascadia Code Light 13")

  ;; 맥 키보드 설정
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super))

;; use-package 설치
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; emacsclient 실행 파일 설정
;; (setq-default with-editor-emacsclient-executable "emacsclient")

(require 'use-package)

;; 패키지가 없으면 항상 설치하기
(setq use-package-always-ensure t)

;; 최신 패키지를 자동으로 업데이트 하기
(use-package auto-package-update
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

;; exec-path에 shell path 추가해주기 (외부 바이너리 참조할 때 사용)
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

;; nord 테마 사용하기
(use-package nord-theme
  :config
  (load-theme 'nord t))

;; 상태바 테마
(use-package spaceline
  :config
  (spaceline-spacemacs-theme))

;; 윈도우 전환을 커맨드 + 방향키로 할 수 있도록
(use-package windmove
  :config
  (windmove-default-keybindings 'super))

;; s-exp 괄호 편집을 위한 패키지
(use-package paredit
  :hook ((emacs-lisp-mode
	  clojure-mode
	  clojurescript-mode
	  clojurec-mode
	  cider-repl-mode)
	 . paredit-mode))

;; s-exp 중첩 괄호를 다른 색으로 구분하기 쉽게
(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode
	  clojure-mode
	  clojurescript-mode
	  clojurec-mode
	  cider-repl-mode)
	 . rainbow-delimiters-mode))

;; s-exp 리스트 선택을 쉽게
(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C--". er/contract-region))

;; 다중 커서로 편집하기
(use-package multiple-cursors
  :config
  (global-set-key (kbd "s-m") 'mc/edit-lines))

;; 파일 목록을 트리 형태로 보기
(use-package treemacs
;;  :defer t
  :config
  (treemacs-resize-icons 14)
  :bind
  (:map global-map ("s-t" . treemacs)))

;; Git 지원
(use-package magit)

;; magit treemacs 확장
(use-package treemacs-magit
  :after treemacs magit)

;; Git 마지막 커밋 메시지 표시해주기
(use-package git-timemachine
  :bind (("s-g" . git-timemachine)))

;; 커서 위에 있는 심볼과 같은 심볼을 하이라이트 & 이동
(use-package symbol-overlay
  :config
  (global-set-key (kbd "M-i") 'symbol-overlay-put)
  (global-set-key (kbd "M-n") 'symbol-overlay-jump-next)
  (global-set-key (kbd "M-p") 'symbol-overlay-jump-prev)
  (add-hook 'prog-mode-hook #'symbol-overlay-mode))

;; 미니 버퍼 히스토리 저장
(use-package savehist
  :config
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring)
        savehist-autosave-interval 60)
  (savehist-mode +1))

;; 문법 체크를 위한 패키지
(use-package flycheck
  :hook (after-init . global-flycheck-mode))

;; 문법 체크 결과를 팝업으로 표시
(use-package flycheck-popup-tip
  :hook (flycheck-mode . flycheck-popup-tip-mode)
  :config
  ;; 툴 팁 뜰 때 앞에 구분자를 *로 지정
  (custom-set-variables
   '(flycheck-popup-tip-error-prefix "* ")))

;; 자동 완성
(use-package company
  :config
  ;; 지연 시간 없이 바로 팝업이 뜨도록
  (setq company-idle-delay 0)
  ;; 목록에서 M + 숫자로 선택할 수 있도록
  (setq company-show-numbers t)
  ;; 한 글자 입력하면 자동 완성 팝업이 뜨도록
  (setq company-minimum-prefix-length 1)
  ;; 자동 완성은 항상 켜둠
  (global-company-mode)
  ;; C-n, C-p로 목록 선택할 수 있도록
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  :bind
  ;; 글자 입력 안해도 뜨도록 할 때
  ("M-<tab>" . #'company-complete))

;; 자주 쓰는 자동 완성이 먼저 추천되도록
(use-package company-statistics
  :init
  (company-statistics-mode))

;; 단축키를 누르고 기다리면 다음 단축키를 안내
(use-package which-key
  :config
  (which-key-mode))

;; M-x 커맨드 자동 완성
(use-package helm
  :config
  (progn
    (helm-mode 1)
    (setq helm-autoresize-mode t)
    (global-set-key (kbd "M-x") 'helm-M-x)))

;; 프로젝트 관리 패키지
(use-package projectile
  :init
  (setq projectile-project-search-path '("~/workspace/"))
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;; treemacs projectile 확장
(use-package treemacs-projectile
  :after treemacs projectile)

;; helm projectile 확장
(use-package helm-projectile
  :config
  (helm-projectile-on)
  (global-set-key (kbd "s-o") 'helm-projectile)
  (global-set-key (kbd "s-f") 'helm-projectile-ag))

;; markdown 모드
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t))

;; Clojure

(use-package clojure-mode
  :config
  ;; 맵 값만 수직으로 정렬하기
  (setq clojure-align-forms-automatically t)
  (setq clojure-align-binding-forms nil)
  (setq clojure-align-cond-forms nil)

  ;; 들여쓰기 설정
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (OPTIONS 2)
    (PATCH 2)
    (rfn 2)
    (let-routes 1)
    (context 2)
    (cond-> 0)
    (attempt-all 1)))

;; 들여쓰기 기준이 바뀌면 자동으로 맞춰 줌
(use-package aggressive-indent
  :hook
  (clojure-mode . aggressive-indent-mode))

;; clj-kondo 문법 체크
(use-package flycheck-clj-kondo)

;; Clojrue REPL, 자동완성, 코드 따라가기 등등
(use-package cider
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  ;; (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  (setq cider-prompt-for-symbol nil)
  (setq cider-save-file-on-load t)
  ;; 에러가 났을 때 에러 버퍼로 이동하지 않기
  (setq cider-auto-select-error-buffer nil)
  ;; 테스트 실패 했을 때 테스트 리포트 버퍼로 이동하지 않기
  (setq cider-auto-select-test-report-buffer nil)
  ;; REPL에 pretty print 켜기
  (setq cider-repl-use-pretty-printing t)
  ;; REPL에 도움말 끄기
  (setq cider-repl-display-help-banner nil)
  :bind
  ("C-<return>" . cider-eval-last-sexp)
  ("C-C r" . nrepl-reset)
  ;; ("C-x M-." . #'cider-find-dwim-other-window)
  )

;; Clojure 리팩토링 지원
(use-package clj-refactor
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (setq cljr-warn-on-eval nil)
  (cljr-add-keybindings-with-prefix "C-c C-v"))

;; 등록된 템플릿으로 코드 생성
(use-package yasnippet
  :hook (clojure-mode . yas-minor-mode))

;; repl에 reset 커맨드를 실행 시켜주는 기능
;; (defun find-buffer-regex (reg)
;;   (interactive)
;;   (remove-if-not #'(lambda (x) (string-match reg x))
;;                  (mapcar #'buffer-name (buffer-list))))

;; (defun cider-execute (command)
;;   (interactive)
;;   (set-buffer (car (find-buffer-regex "cider-repl.*")))
;;   (goto-char (point-max))
;;   (insert command)
;;   (cider-repl-return))

;; (defun nr-epl-reset ()
;;   (interactive)
;;   (cider-execute "(reset)")
;;   (message "Reset"))

;; (define-key cider-mode-map (kbd "C-c r") 'nrepl-reset)
;; (global-set-key (kbd "C-S-s") 'isearch-forward-symbol-at-point)

;; (load-file "~/.emacs.d/cider-show-def/cider-show-def.el")
;; (add-hook 'clojure-mode 'cider-show-def-mode)

;; (use-package edit-indirect)

;; (use-package anzu
;;   :config
;;   (global-anzu-mode +1))

;; (defun start-cider-repl-with-profile ()
;;   (interactive)
;;   (letrec ((profile (read-string "Enter profile name: "))
;;            (lein-params (concat "with-profile " profile " repl :headless")))
;;     (message "lein-params set to: %s" lein-params)
;;     (set-variable 'cider-lein-parameters lein-params)
;;     (cider-jack-in '())))

;; (defun start-cider-with-local-profile ()
;;   (interactive)
;;   (set-variable 'cider-lein-parameters "with-profile +db/local,+env/local repl :headless")
;;   (cider-jack-in '()))

;; (defun reformat-ns ()
;;   (save-excursion
;;     (cljr--goto-ns)
;;     (re-search-forward ":refer")
;;     (newline)))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-popup-tip-error-prefix "* ")
 '(package-selected-packages
   '(counsel-projectile counsel ivy-rich ivy zprint-mode winum which-key use-package treemacs-projectile treemacs-magit transpose-frame symbol-overlay spaceline rainbow-delimiters nord-theme markdown-mode helm-projectile gnu-elpa-keyring-update git-timemachine flycheck-popup-tip flycheck-clj-kondo expand-region exec-path-from-shell edit-indirect company-statistics command-log-mode clj-refactor auto-package-update anzu aggressive-indent)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
