(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

(setq package-list
      '(ace-jump-mode
	    smex
	    swiper
	    gruber-darker-theme
	    magit
	    multiple-cursors
	    markdown-mode
	    cider
	    exec-path-from-shell
	    projectile
	    flycheck
	    company
	    typescript-ts-mode
        docker-compose-mode
        lsp-mode
        treesit-auto
        prettier
        lsp-java
        ripgrep))
(unless package-archive-contents (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(add-to-list 'load-path "~/.emacs.local/")

(setq custom-file "~/.emacs.custom.el")
(setq inhibit-startup-message t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq vc-follow-symlinks nil)
(setq use-dialog-box nil)
(setq scroll-margin 10)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq warning-minimum-level :emergency)
(setq make-backup-files nil)
(setq display-line-numbers-type 'relative)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode t)
(global-display-line-numbers-mode 1)
(which-key-mode 1)

;; ido
(setq ido-enable-flex-matching t)
(ido-mode 1)
(ido-everywhere 1)

(require 'recentf)
(setq recentf-max-saved-items 50)
(recentf-mode t)

;; Show current directory
(setq-default mode-line-buffer-identification
              (let ((orig  (car mode-line-buffer-identification)))
                `(:eval (cons (concat ,orig (abbreviate-file-name default-directory))
                              (cdr mode-line-buffer-identification)))))

;; macOS specific
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  ;; Replace Meta with CMD
  (setq mac-option-key-is-meta nil
	    mac-command-key-is-meta t
	    mac-command-modifier 'meta
	    mac-option-modifier 'none)
  ;; Copy/Paste seemeslessly with macOS
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))
  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
	    (process-send-string proc text)
	    (process-send-eof proc))))
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx)
  ;; Fix pixel gap between emacs and other frames
  (setq frame-resize-pixelwise t)
  ;; Open file in finder
  (defun open-file-in-finder ()
    (interactive)
    (shell-command "open -R ."))
  ;; Do not load theme on terminal
  ;; (load-theme 'modus-operandi-tinted)
  (load-theme 'gruber-darker t nil))

;; Configure font size
(set-face-attribute 'default nil
                    :family "Iosevka Nerd Font Mono"
                    :height 160
                    :weight 'normal
                    :width 'normal)

;; Set starting frame position and size
(setq initial-frame-alist '((top . 0) (left . 0) (width . 122) (height . 58)))

;; Keybindings
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-!") 'async-shell-command)

(require 'smex)
(global-set-key (kbd "M-x") 'smex)

(which-key-mode)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-snippet nil)
  (setq company-lsp-enable-snippet nil)
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-which-key-integration t))

;; Automatically launch LSP when mode are activated
(add-hook 'c-ts-mode-hook 'lsp-deferred)
(add-hook 'c++-mode-hook 'lsp-deferred)
(add-hook 'rust-mode-hook 'lsp-deferred)
(add-hook 'js-jsx-mode-hook 'lsp-deferred)
(add-hook 'js-mode-hook 'lsp-deferred)
(add-hook 'typescript-ts-mode-hook 'lsp-deferred)
(add-hook 'tsx-ts-mode-hook 'lsp-deferred)
(require 'lsp-java)
(add-hook 'java-ts-mode-hook 'lsp-deferred)
(add-hook 'java-mode-hook 'lsp-deferred)

;; Prettier
(add-hook 'after-init-hook #'global-prettier-mode)
(add-hook 'typescript-ts-mode-hook 'prettier-mode)
(add-hook 'json-ts-mode-hook 'prettier-mode)

;; Swiper - Powerful search
(use-package swiper :config (global-set-key "\C-s" 'swiper))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-\"") 'mc/skip-to-next-like-this)
  (global-set-key (kbd "C-:") 'mc/skip-to-previous-like-this))

(use-package ace-jump-mode
  :config (define-key global-map (kbd "C-c C-SPC" ) 'ace-jump-mode))

(require 'dired-x)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-dwim-target t)
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-hide-details-mode 1)
(when (string= system-type "darwin") (setq dired-use-ls-dirred nil))
(setq dired-listing-switches "-alh")

;; Projectile
(use-package projectile
  :ensure t
  :init
  (projectile-mode 1)
  :bind (:map projectile-mode-map
	          ("C-x p" . projectile-command-map)
              ("C-x p p" . projectile-dired)
              ("C-x p !" . projectile-run-async-shell-command-in-root)))

;; Clojure Cider
(global-set-key (kbd "C-x C-z") 'cider-jack-in)

;; GitHub Copilot
(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :config
  (global-set-key (kbd "C-c C-c") 'copilot-mode)
  (global-set-key (kbd "C-c C-a") 'copilot-accept-completion))

(defun duplicate-line ()
  (interactive)
  (let ((column (- (point) (point-at-bol)))
	    (line (let ((s (thing-at-point 'line t)))
		        (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))
(global-set-key (kbd "C-,") 'duplicate-line)

(require 'c3-mode)

;; treesit
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(load-file custom-file)
