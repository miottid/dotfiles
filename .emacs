(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

(setq package-list
      '(ace-jump-mode
        breadcrumb
        smex
        swiper
        consult
        vertico
        corfu
        orderless
        marginalia
        embark
        savehist
        cape
        gruber-darker-theme
        magit
        multiple-cursors
        exec-path-from-shell
        projectile
        flycheck
        company
        treesit-auto
        prettier
        lsp-mode
        markdown-mode
        typescript-ts-mode
        docker-compose-mode
        naysayer-theme
        rainbow-delimiters
        rainbow-identifiers
        autothemer
        kuronami-theme))
(unless package-archive-contents
  (package-refresh-contents))
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
(show-paren-mode 1)

(require 'recentf)
(setq recentf-max-saved-items 50)
(recentf-mode t)

;; Window management
(defun select-body-function (window)
  (select-window window))
(setq display-buffer-alist
      '(
        ("\\*Occur\\*"
         (display-buffer-reuse-mode-window
          display-buffer-below-selected)
         (dedicated . t)
         (window-height . fit-window-to-buffer)
         )
        ))

(setq split-height-threshold 80)
(setq split-width-threshold 125)

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
  (setq ns-antialias-text nil)
  ;; Fix pixel gap between emacs and other frames
  (setq frame-resize-pixelwise t)
  ;; Open file in finder
  (defun open-file-in-finder ()
    (interactive)
    (shell-command "open -R .")))

;; Set starting frame position and size
(setq initial-frame-alist '((top . 0) (left . 0) (width . 126) (height . 70)))

;; Configure font size
(set-face-attribute 'default nil
                    :family "Iosevka Nerd Font Mono"
                    :height 180
                    :weight 'normal
                    :width 'normal)

;; (load-theme 'gruber-darker t nil)
(load-theme 'modus-vivendi-deuteranopia t nil)

;; Keybindings
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x C-b") 'ibuffer)

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
  (lsp-enable-which-key-integration t)
  :hook
  (c-ts-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (rust-mode . lsp-deferred)
  (js-jsx-mode . lsp-deferred)
  (js-mode . lsp-deferred)
  (typescript-ts-mode . lsp-deferred)
  (tsx-ts-mode . lsp-deferred)
  (rust-ts-mode . lsp-deferred)
  (zig-mode . lsp-deferred))

;; Prettier
;; (use-package prettier
;;   :hook
;;   (typescript-ts-mode . prettier-mode)
;;   (ts-mode . prettier-mode)
;;   (tsx-ts-mode . prettier-mode)
;;   (json-ts-mode . prettier-mode))

(use-package lsp-biome
  :vc (:url "https://github.com/cxa/lsp-biome"))

;; Breadcrumb
(use-package breadcrumb
  :init
  (breadcrumb-mode))

(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

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
(when (string= system-type "darwin")
  (setq dired-use-ls-dirred nil))
(setq dired-listing-switches "-alh")

;; Projectile
(use-package projectile
  :ensure t
  :init
  (projectile-mode 1)
  :bind (:map projectile-mode-map
              ("C-x p" . projectile-command-map)
              ("C-x p d" . projectile-dired)
              ("C-x p !" . projectile-run-async-shell-command-in-root)))

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
(add-hook 'c3-mode-hook 'company-mode)

;; treesit
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(setq split-height-threshold nil)
(setq split-width-threshold most-positive-fixnum)

(setq inferior-lisp-program "sbcl")

;; org
(use-package org
  :ensure nil
  :config
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-insert-heading-respect-content t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-directory "~/code/org")
  (setq org-agenda-files (list org-directory))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)"))))

;; Enable vertico
(use-package vertico
  ;; :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(load-file custom-file)
