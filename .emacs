;; Performance: Increase GC threshold during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Reduce startup screen rendering
(setq frame-inhibit-implied-resize t)

;; Native compilation settings
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-jit-compilation t))

(setq custom-file "~/.emacs.custom.el")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

(setq package-list
      '(avy
        consult
        corfu
        orderless
        marginalia
        embark
        cape
        gruber-darker-theme
        magit
        multiple-cursors
        exec-path-from-shell
        treesit-auto
        lsp-mode
        markdown-mode
        typescript-ts-mode
        astro-ts-mode
        docker-compose-mode))
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(add-to-list 'load-path "~/.emacs.local/")

(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq vc-follow-symlinks t)
(setq use-dialog-box nil)
(setq scroll-margin 10)
(setq scroll-conservatively 10000)
(setq warning-minimum-level :emergency)
(setq make-backup-files nil)
(setq display-line-numbers-type 'relative)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-basic-offset 4)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode t)
(global-display-line-numbers-mode 1)
(show-paren-mode 1)
(which-key-mode 1)

(require 'recentf)
(setq recentf-max-saved-items 50)
(recentf-mode t)

;; Window management
(setq display-buffer-alist
      '(("\\*Occur\\*"
         (display-buffer-reuse-mode-window
          display-buffer-below-selected)
         (dedicated . t)
         (window-height . fit-window-to-buffer))))

;; macOS specific
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  ;; Replace Meta with CMD
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'none)
  (setq ns-antialias-text nil)
  ;; Fix pixel gap between emacs and other frames
  (setq frame-resize-pixelwise t)
  ;; Open file in finder
  (defun open-file-in-finder ()
    (interactive)
    (shell-command "open -R .")))

;; Set frame position and size
(setq default-frame-alist '((top . 100) (left . 80) (width . 100) (height . 40)))
(setq initial-frame-alist default-frame-alist)

;; Configure font size
(set-face-attribute 'default nil
                    :family "Iosevka Nerd Font Mono"
                    :height 150)

;; Keybindings
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Compilation with ANSI colors
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; Biome formatting on save
(defun biome-format-buffer ()
  "Format the current buffer with Biome."
  (let ((current-point (point)))
    (shell-command-on-region
     (point-min) (point-max)
     (format "npx @biomejs/biome format --stdin-file-path=%s"
             (shell-quote-argument (or (buffer-file-name) "file.ts")))
     nil t)
    (goto-char current-point)))

(defun biome-format-on-save ()
  (add-hook 'before-save-hook #'biome-format-buffer nil t))

(add-hook 'typescript-ts-mode-hook #'biome-format-on-save)
(add-hook 'tsx-ts-mode-hook #'biome-format-on-save)
(add-hook 'js-mode-hook #'biome-format-on-save)
(add-hook 'js-jsx-mode-hook #'biome-format-on-save)
(add-hook 'json-ts-mode-hook #'biome-format-on-save)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-completion-provider :none)
  ;; Performance optimizations
  (setq lsp-idle-delay 0.5)
  (setq lsp-log-io nil)
  (setq read-process-output-max (* 1024 1024))
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-which-key-integration t)
  :hook
  (c-ts-mode . lsp-deferred)
  (js-jsx-mode . lsp-deferred)
  (js-mode . lsp-deferred)
  (typescript-ts-mode . lsp-deferred)
  (tsx-ts-mode . lsp-deferred)
  (astro-ts-mode . lsp-deferred)
  (rust-ts-mode . lsp-deferred)
  (zig-mode . lsp-deferred))

(defun set-c-indentation ()
  (setq-local c-ts-mode-indent-style 'linux
              c-ts-mode-indent-offset 4
              tab-width 4))
(add-hook 'c-ts-mode-hook 'set-c-indentation)

;; Typescript
(setq typescript-ts-mode-indent-offset 4)

;; Astro
(use-package astro-ts-mode
  :ensure t
  :mode "\\.astro\\'"
  :config
  (setq astro-ts-mode-indent-offset 4))

;; Astro Prettier formatting
(defun astro-format-buffer ()
  "Format the current Astro buffer with Prettier."
  (when (eq major-mode 'astro-ts-mode)
    (let ((current-point (point)))
      (shell-command-on-region
       (point-min) (point-max)
       "npx prettier --parser=astro --stdin-filepath=file.astro"
       nil t)
      (goto-char current-point))))

(add-hook 'astro-ts-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'astro-format-buffer nil t)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  :init
  (global-corfu-mode))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package embark
  :bind
  ("C-." . embark-act)
  ("C-h B" . embark-bindings))

(use-package consult
  :bind
  ("C-s" . consult-line)
  ("C-x b" . consult-buffer)
  ("M-g g" . consult-goto-line)
  ("M-g M-g" . consult-goto-line)
  ("M-s r" . consult-ripgrep))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this)
  ("C-\"" . mc/skip-to-next-like-this)
  ("C-:" . mc/skip-to-previous-like-this))

(use-package avy
  :bind
  ("C-c C-SPC" . avy-goto-char-timer)
  ("M-g w" . avy-goto-word-1)
  ("M-g l" . avy-goto-line))

(require 'dired-x)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-dwim-target t)
(setq dired-kill-when-opening-new-dired-buffer t)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(when (string= system-type "darwin") (setq dired-use-ls-dired nil))
(setq dired-listing-switches "-alh")

(global-set-key (kbd "C-,") 'duplicate-dwim)

;; treesit
(use-package treesit-auto
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(setq split-height-threshold nil)
(setq split-width-threshold most-positive-fixnum)

(setq inferior-lisp-program "sbcl")

;; org
(use-package org
  :ensure nil
  :defer t
  :config
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-insert-heading-respect-content t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-directory "~/code/org")
  (setq org-agenda-files (list org-directory))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)"))))

(use-package savehist
  :init
  (savehist-mode))

;; (load-theme 'gruber-darker t nil)
(load-theme 'modus-operandi-deuteranopia t nil)
;; Reset GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)  ; 16MB
                  gc-cons-percentage 0.1)))

(when (file-exists-p custom-file)
  (load-file custom-file))
