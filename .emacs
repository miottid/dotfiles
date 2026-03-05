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
(package-initialize)

(setq package-list
      '(avy
        consult
        corfu
        orderless
        marginalia
        embark
        embark-consult
        cape
        wgrep
        gruber-darker-theme
        magit
        multiple-cursors
        exec-path-from-shell
        envrc
        vertico
        diff-hl
        vundo
        treesit-auto
        markdown-mode
        markdown-toc
        olivetti
        typescript-ts-mode
        astro-ts-mode))
(defvar package-list-refreshed nil)
(dolist (package package-list)
  (unless (package-installed-p package)
    (unless package-list-refreshed
      (package-refresh-contents)
      (setq package-list-refreshed t))
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
(delete-selection-mode 1)
(electric-pair-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)
(repeat-mode 1)
(pixel-scroll-precision-mode 1)

(require 'recentf)
(setq recentf-max-saved-items 50)
(setq recentf-exclude '("/node_modules/" "/\\.git/" "/elpa/"))
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
  ;; Ensure fnm-managed global node bins are in exec-path
  (let ((node-path (string-trim (shell-command-to-string "node -p 'process.execPath'"))))
    (when (and (not (string-empty-p node-path)) (file-exists-p node-path))
      (let ((bin-dir (file-name-directory node-path)))
        (add-to-list 'exec-path bin-dir)
        (setenv "PATH" (concat bin-dir ":" (getenv "PATH"))))))

  ;; Replace Meta with CMD
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'none)
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

;; Resolve binaries from node_modules/.bin for speed
(defun find-node-modules-binary (name)
  "Find NAME in the nearest node_modules/.bin directory."
  (let ((dir (locate-dominating-file default-directory "node_modules")))
    (when dir
      (let ((binary (expand-file-name (concat "node_modules/.bin/" name) dir)))
        (when (file-executable-p binary)
          binary)))))

;; Biome formatting on save
(defun biome-format-buffer ()
  "Format the current buffer with Biome."
  (let ((current-point (point))
        (biome (or (find-node-modules-binary "biome") "npx @biomejs/biome")))
    (shell-command-on-region
     (point-min) (point-max)
     (format "%s format --stdin-file-path=%s"
             biome
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

(setq read-process-output-max (* 1024 1024))

(use-package eglot
  :ensure nil
  :hook
  (c-ts-mode . eglot-ensure)
  (js-mode . eglot-ensure)
  (js-jsx-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
  (astro-ts-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (zig-mode . eglot-ensure)
  :config
  (setq eglot-autoshutdown t)
  (setq eglot-events-buffer-size 0)
  (setq eglot-send-changes-idle-time 0.5)
  ;; Disable file watchers to avoid lag in large monorepos
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (defun eglot-disable-file-watchers (fn &rest args)
    "Disable eglot file watchers for performance in large repos."
    (cl-letf (((symbol-function 'eglot--register-filewatch) #'ignore))
      (apply fn args)))
  (advice-add 'eglot--managed-mode :around #'eglot-disable-file-watchers)
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode js-mode js-jsx-mode)
                 . ("typescript-language-server" "--stdio")))
  (defun eglot-astro-server (_interactive)
    "Return astro-ls command with tsdk resolved from node_modules."
    (let* ((tsdk-dir (locate-dominating-file default-directory "node_modules/typescript/lib"))
           (tsdk (when tsdk-dir
                   (expand-file-name "node_modules/typescript/lib" tsdk-dir))))
      `("astro-ls" "--stdio"
        :initializationOptions (:typescript (:tsdk ,(or tsdk ""))))))
  (add-to-list 'eglot-server-programs
               '(astro-ts-mode . eglot-astro-server)))

(defun set-c-indentation ()
  (setq-local c-ts-mode-indent-style 'linux
              c-ts-mode-indent-offset 4
              tab-width 4))
(add-hook 'c-ts-mode-hook 'set-c-indentation)

;; Markdown
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :hook
  (markdown-mode . visual-line-mode)
  (markdown-mode . flyspell-mode)
  (markdown-mode . olivetti-mode))

(use-package olivetti
  :custom
  (olivetti-body-width 80))

(use-package markdown-toc)

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
    (let ((current-point (point))
          (prettier (or (find-node-modules-binary "prettier") "npx prettier")))
      (shell-command-on-region
       (point-min) (point-max)
       (format "%s --parser=astro --stdin-filepath=file.astro" prettier)
       nil t)
      (goto-char current-point))))

(defun astro-format-on-save ()
  (add-hook 'before-save-hook #'astro-format-buffer nil t))

(add-hook 'astro-ts-mode-hook #'astro-format-on-save)

(use-package vertico
  :init
  (vertico-mode))

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

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t))

(use-package consult
  :bind
  ("C-x b" . consult-buffer)
  ("M-g g" . consult-goto-line)
  ("M-g M-g" . consult-goto-line)
  ("M-s l" . consult-line)
  ("M-s r" . consult-ripgrep)
  ("M-g i" . consult-imenu))

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
(when (eq system-type 'darwin) (setq dired-use-ls-dired nil))
(setq dired-listing-switches "-alh")

(global-set-key (kbd "C-,") 'duplicate-dwim)

(use-package diff-hl
  :hook
  (prog-mode . diff-hl-mode)
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :custom
  (diff-hl-draw-borders nil))

(use-package vundo
  :bind ("C-x u" . vundo))

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

;; Monorepo support: scope project to nearest package.json using git
(defun project-find-package-json (dir)
  "Find project root by locating the nearest package.json from DIR."
  (let ((root (locate-dominating-file dir "package.json")))
    (when (and root (not (string-match-p "node_modules" root)))
      (cons 'package-json root))))

(cl-defmethod project-root ((project (head package-json)))
  (cdr project))

(cl-defmethod project-files ((project (head package-json)) &optional _dirs)
  "List files using git ls-files scoped to the package directory."
  (let* ((root (project-root project))
         (default-directory root))
    (mapcar (lambda (f) (expand-file-name f root))
            (split-string
             (shell-command-to-string "git ls-files -z --cached --others --exclude-standard")
             "\0" t))))

(add-hook 'project-find-functions #'project-find-package-json)

;; editorconfig
(editorconfig-mode 1)

;; direnv
(use-package envrc
  :init
  (envrc-global-mode))

;; (load-theme 'gruber-darker t nil)
(load-theme 'modus-operandi-deuteranopia t nil)
;; Reset GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)  ; 16MB
                  gc-cons-percentage 0.1)))

(when (file-exists-p custom-file)
  (load-file custom-file))
