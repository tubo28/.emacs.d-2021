;;; package --- Summary
;;; Commentary:
;;; Code:

;; GCを抑制しパフォーマンス向上
(setq gc-cons-percentage (* gc-cons-percentage 4))
(setq gc-cons-threshold (* gc-cons-threshold 4))

;; "Package cl is deprecated" を無視する
(setq byte-compile-warnings '(not cl-functions obsolete))

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

;; custom-set-* で init.el が汚染されるのを防ぐ
(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

;; ファイルが変更されたら自動で再読み込み
(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 1))
  :global-minor-mode global-auto-revert-mode)

;; 選択状態でペーストすると選択していたものを削除し置き換える
(leaf delsel
  :doc "delete selection if you insert"
  :tag "builtin"
  :global-minor-mode delete-selection-mode)

;; 対応するカッコを強調表示する
(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom ((show-paren-delay . 0.1))
  :global-minor-mode show-paren-mode)

(leaf simple
  :doc "basic editing commands for Emacs"
  :tag "builtin" "internal"
  :custom ((kill-ring-max . 100)
           (kill-read-only-ok . t)
           (kill-whole-line . t)
           (eval-expression-print-length . nil)
           (eval-expression-print-level . nil)))

;; ファイルの自動バックアップなどの設定
(leaf files
  :doc "file input and output commands for Emacs"
  :tag "builtin"
  :custom `((auto-save-timeout . 15)
            (auto-save-interval . 60)
            (auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
            (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
                                        (,tramp-file-name-regexp . nil)))
            (version-control . t)
            (delete-old-versions . t)))

(leaf startup
  :doc "process Emacs shell arguments"
  :tag "builtin" "internal"
  :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))

(leaf ivy
  :doc "Incremental Vertical completion"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  :blackout t
  :leaf-defer nil
  :custom ((ivy-initial-inputs-alist . nil)
           (ivy-use-selectable-prompt . t))
  :global-minor-mode t
  :config
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.0"
    :tag "matching" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :bind (("C-s" . swiper)))

  (leaf counsel
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :blackout t
    :bind (("C-S-s" . counsel-imenu)
           ("C-x C-r" . counsel-recentf))
    :custom `((counsel-yank-pop-separator . "\n----------\n")
              (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
    :global-minor-mode t))

(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :custom ((prescient-aggressive-file-save . t))
  :global-minor-mode prescient-persist-mode)

(leaf ivy-prescient
  :doc "prescient.el + Ivy"
  :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :after prescient ivy
  :custom ((ivy-prescient-retain-classic-highlighting . t))
  :global-minor-mode t)


(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :global-minor-mode global-flycheck-mode
  :custom ((flycheck-check-syntax-automatically . '(mode-enabled save))))


(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :blackout t
  :leaf-defer nil
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 1)
           (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode)

(leaf company-c-headers
  :doc "Company mode backend for C/C++ header files"
  :req "emacs-24.1" "company-0.8"
  :tag "company" "development" "emacs>=24.1"
  :added "2020-03-25"
  :emacs>= 24.1
  :ensure t
  :after company
  :defvar company-backends
  :config
  (add-to-list 'company-backends 'company-c-headers))

;; 変更履歴を見やすくする
(leaf undo-tree
  :doc "Visual undo and redo"
  :url "https://github.com/apchamberlain/undo-tree.el"
  :added "2021-05-01"
  :emacs>= 24
  :ensure t
  :bind (("C-z" . undo-tree-undo))
  :global-minor-mode global-undo-tree-mode)

;; 起動画面をよくする
(leaf dashboard
  :doc "An extensible emacs startup screen showing you what’s most important."
  :url "https://github.com/emacs-dashboard/emacs-dashboard"
  :added "2021-05-01"
  :ensure t
  :custom ((dashboard-items . '((recents . 100)
                                (bookmarks . 5))))
  :config
  (dashboard-setup-startup-hook))

;; 最近開いたファイル
(leaf recentf
  :added "2021-05-01"
  :custom ((recentf-max-saved-items . 500)
           (recentf-max-menu-items . 15)
           (recentf-auto-cleanup . 'never))
  :ensure t
  :global-minor-mode t)

;; 貼り付けなどの操作で変更された部分を強調する
(leaf volatile-highlights
  :doc "Minor mode for visual feedback on some operations."
  :url "https://github.com/k-talo/volatile-highlights.el"
  :added "2021-05-01"
  :ensure t
  :global-minor-mode t)

;; カーソル業をハイライトする
(leaf hl-line
  :added "2021-05-01"
  :ensure t
  :custom ((hl-line-face . 'underline))
  :global-minor-mode global-hl-line-mode)

;; 見た目がきれいな置換
(leaf anzu
  :doc "Visualize query-replace"
  :url "https://github.com/emacsorphanage/anzu"
  :added "2021-05-01"
  :ensure t
  :custom
  ((anzu-deactivate-region . t)
   (anzu-search-threshold . 1000)
   (anzu-replace-to-string-separator . " => "))
  :bind (("C-r" . anzu-query-replace)
         ("M-C-r" . anzu-query-replace-regexp)))

;; キーバインドの補助
(leaf which-key
  :added "2021-05-01"
  :ensure t
  :global-minor-mode t)

;; 変更部分を画面左に表示
(leaf git-gutter
  :added "2021-05-01"
  :ensure t
  :config
  (set-face-foreground 'git-gutter:modified "yellow")
  (set-face-foreground 'git-gutter:added "green")
  (set-face-foreground 'git-gutter:deleted "red")
  :custom
  ((git-gutter:update-interval . 2))
  :global-minor-mode global-git-gutter-mode)

(leaf whitespace
  :added "2021-05-01"
  :ensure t
  :custom
  ((whitespace-style . '(face tabs newline trailing tab-mark space-before-tab space-after-tab)))
  :global-minor-mode global-whitespace-mode)

;;;
;;; 言語ごとのminer-mode
;;;
(leaf yaml-mode
  :added "2021-05-01"
  :ensure t
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode)))


(leaf markdown-mode
  :added "2021-05-01"
  :ensure t
  :mode (("\\.md$'" . markdown-mode)
         ("\\.markdown$'" . markdown-mode)))


;;;
;;; leafでインストールするpackageに関係しない設定
;;;

;; 切り取りは選択中のみ行う
(defun cut-if-selected ()
  "Do 'kill-region' only if region is active."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))))
(global-set-key (kbd "C-w") 'cut-if-selected)

;; 選択中でなかったらバッファ全体をコピーする
(defun copy-all-if-not-selected ()
  "Do 'kill-ring-save' for entire buffer if region is not active."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (point-min) (point-max))))
(global-set-key (kbd "M-w") 'copy-all-if-not-selected)

;; UTF-8をデフォルトとする
(set-default-coding-systems 'utf-8-unix)

;;; yes/no -> y/n
(fset 'yes-or-no-p 'y-or-n-p)

;;; 行番号を画面左に表示
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode)
  (progn
    (defvar linum-format "%4d|")
    (defvar linum-delay t) ;;; 表示を高速化
    (defadvice linum-schedule (around my-linum-schedule () activate)
      (run-with-idle-timer 0.2 nil #'linum-update-current))
    (global-linum-mode t)))

;;; カーソルの位置をモードラインに表示
(column-number-mode t)
(line-number-mode t)
;;; 長い行でも右端で折り返さない
(setq-default truncate-lines t)
;;; 画面縦割りしてても
(setq-default truncate-partial-width-windows t)

;;; スクロールするときにカーソルの位置に余裕をもたせる
(setq-default scroll-margin 5)

;;; マウスでのスクロールの制御
(setq-default mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq-default mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq-default mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq-default scroll-step 1) ;; keyboard scroll one line at a time

;; ターミナルではツールバーとメニューバーを非表示
(if (not window-system)
    (progn
      (tool-bar-mode -1)
      (menu-bar-mode -1)))

;;; 改行時にインデント
(global-set-key (kbd "<RET>") 'newline-and-indent)

;;; C-g周辺のキー誤爆防止
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-t") nil)

;;; C-jで補完
(global-set-key (kbd "C-j") 'dabbrev-expand)

;;; C-x C-b = C-x b (switch-to-buffer)
(define-key global-map (kbd "C-x C-b") 'switch-to-buffer)

;;; Macでクリップボードとkill-ringを共通化
;;; OS X 10.10 以降は reattach-to-user-namespace が不要
;;; https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard/issues/66
(if (eq system-type 'darwin)
    (progn
      (defun copy-from-osx ()
        (shell-command-to-string "pbpaste"))
      (defun paste-to-osx (text &optional push)
        (let ((process-connection-type nil))
          (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
            (process-send-string proc text)
            (process-send-eof proc))))
      (setq interprogram-cut-function 'paste-to-osx)
      (setq interprogram-paste-function 'copy-from-osx)))

(provide 'init)

;;; init.el ends here
