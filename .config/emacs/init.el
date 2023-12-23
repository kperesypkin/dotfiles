;; -*- lexical-binding: t -*-
;;; Package --- Summary
;;; Comment: Settings for Emacs

;;; Code:

;;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; System-type definition
(defun system-is-linux()
  "Check if the system is GNU/linux."
  (string-equal system-type "gnu/linux"))

(defun system-is-windows()
  "Check if the system is Windows."
  (string-equal system-type "windows-nt"))

;; Start EMACS as a server in linux
(when (system-is-linux)
  (require 'server)
  (unless (server-running-p)
    (server-start)))

;; Set PATH and exec-path variables
(when (system-is-linux)
  (let ((my/paths
        '("/home/kirill/.local/bin"
          "/home/kirill/.ghcup/bin"
          "/home/kirill/.cabal/bin"
          "/home/kirill/.cargo/bin"
          "/home/kirill/.nimble/bin")))
    
    (setenv "PATH" (concat (getenv "PATH") path-separator
                           (mapconcat 'identity my/paths ":")))
    (setq exec-path (append my/paths exec-path))))

;;; Make a shortcut for init.el
;;; and a reload function
(defun open-initfile ()
  "Opens 'user-init-file'."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun load-initfile ()
  "Reloads 'user-init-file'."
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

(global-set-key (kbd "<f9>") 'open-initfile)
(global-set-key (kbd "C-<f9>") 'load-initfile)

;;; Packages
(require 'package)

;;;; Packages sources
(setq package-archives
      `(("melpa" . "https://melpa.org/packages/")
        ,@package-archives))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq tls-checktrust "ask")
(setq tls-program
      '("gnutls-cli --x509cafile /etc/ssl/certs/ca-certificates.crt -p %p %h"))
(package-initialize)

;;;; If 'gnu-epla-keyring-update' is not installed, download and install it
(unless (package-installed-p 'gnu-elpa-keyring-update)
  (setq-default package-check-signature 'allow-unsigned)
  (setq-default package-unsigned-archives '("gnu"))
  (package-refresh-contents)
  (package-install 'gnu-elpa-keyring-update)
  (kill-emacs))

(require 'gnu-elpa-keyring-update)

;;;; If 'use-package' is not installed, download and install it
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package use-package
  :ensure nil

  :custom
  (use-package-always-ensure t)

  :config
  (put 'use-package 'lisp-indent-function 1))

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package diminish)

(use-package gcmh
  :demand 

  :diminish

  :config
  (gcmh-mode 1))

;;;; Simple use-package wrapper
(defmacro def-package (name &rest body)
  "Defines a virtual package."
  `(use-package ,name
     :ensure nil
     :no-require t
     ,@body))

(put 'def-package 'lisp-indent-function 1)

(defmacro setup-package (name &rest body)
  "Configures an internal package (NAME/BODY)."
  `(use-package ,name
     :ensure nil
     ,@body))

(put 'setup-package 'lisp-indent-function 1)

;;; Customization
(setup-package custom
  :custom
  (custom-safe-themes t "Trust all custom themes..."))

(setup-package cus-edit
  :custom
  (custom-file null-device "Don't store customizations"))

;;; Emacs itself
(setup-package emacs
  :init
  (defalias 'yes-or-no-p 'y-or-n-p)

  :custom
  (inhibit-startup-screen t "No startup screen")
  (initial-scratch-message nil)
  (use-dialog-box nil "Disable dialog boxes")
  (search-highlight t)
  (query-replace-highlight t)
  (resize-mini-windows t)
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
  (ring-bell-function #'(lambda ()) "No bells, please!")
  (auto-save-default #'(lambda ()) "No autosaves!")
  (backup-inhibited t)
  (global-hl-line-mode 1)
  (global-linum-mode 1)
  (linum-format "%4d\u2502")
  (electric-pair-mode 1)
  ;; Indents
  (tab-width 4)
  (standart-indent 4)
  (indent-tabs-mode nil)
  (indicate-empty-lines t)
  ;; Scrolling
  (scroll-step 1)
  (scroll-margin 10)
  (scroll-preserve-screen-position 10)
  ;; Window
  (tool-bar-mode nil)
  (scroll-bar-mode nil)
  (menu-bar-mode nil)
  (frame-title-format "Emacs: %b")
  (fringe-mode '(4 . 0))
  ;; Columns and lines numbers
  (line-number-mode t)
  (column-number-mode t)
  ;; Cursor
  (blink-cursor-mode t)
  (x-stretch-cursor t)
  (cursor-type 'bar)
  (shift-select-mode nil "No selection with <shift>")
  ;; Exit confirmation
  ;; (kill-emacs-query-functions
  ;;  (cons (lambda () (yes-or-no-p "Really Quit Emacs? "))
  ;;        kill-emacs-query-functions))

  :config
  (setq global-prettify-symbols-mode t)
  (setq default-cursor-type 'bar)
  (prefer-coding-system 'utf-8)
  (put 'overwrite-mode 'disabled t)
 
  :hook
  (before-save-hook . delete-trailing-whitespace))

;;; UI
;;;; Icons
;;;; After installation run 'M-x all-the-icons-install-fonts'
(use-package all-the-icons
  :if (display-graphic-p))

;;;; Faces
(setup-package faces
  :diminish (buffer-face-mode "")

  :preface
  (setq my/font-height (if (system-is-linux) 110 10))

  :custom
  (face-font-family-alternatives
   '(("Fira Code" "Hack" "Iosevka Term" "Ubuntu Mono")))

  :custom-face
  (variable-pitch ((t (:family "Liberation Sans" :height ,my/font-height))))
  (fixed-pitch ((t (:family "Fira Code" :height ,my/font-height))))
  (default ((t (:family "Fira Code" :height ,my/font-height :weight normal))))
  (mode-line ((t (:height 0.8))))
  (mode-line-inactive ((t (:height 0.8)))))

;;;; Subword-mode
(setup-package subword
  :diminish)

;;;; Whitespaces
(setup-package whitespace
  :diminish

  :preface
  (defun my/whitespace-prog-mode ()
    "Whitespace mode for prog buffers."
    (setq-local whitespace-style '(face lines-tail tab-mark))
    (setq-local whitespace-line-column 80)
    (setq-local truncate-lines t)
    (whitespace-mode t))

  :hook
  (prog-mode . my/whitespace-prog-mode)

  :custom-face
  (whitespace-line ((t (:background nil :foreground nil :underline (:color "Red1" :style wave) :weight normal))))

  :custom
  (indent-tabs-mode nil)
  (tab-width 4)
  (mode-require-final-newline nil))

;;;; Uniquify buffer names
(setup-package uniquify
  :custom
  (uniquify-strip-common-suffix t)
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style 'post-forward-angle-brackets))

;;;; Secondary buffers highlight
(use-package solaire-mode
  :config
  (setq solaire-mode-real-buffer-fn
        (lambda ()
          (or (buffer-file-name (buffer-base-buffer))
              (derived-mode-p 'prog-mode)
              (derived-mode-p 'text-mode))))
  (solaire-global-mode))

;;;; Highlight indentation
(use-package highlight-indent-guides
  :diminish
  
  :hook
  (prog-mode . highlight-indent-guides-mode)
  
  :config
  (setq highlight-indent-guides-method 'bitmap))
  
;;; Selection, indentation, quotes and comments
;;;; Expand region selection
(use-package expand-region
  :bind
  ("M-]" . er/expand-region)
  ("M-[" . er/contract-region))

;;;; Smart commenting
(use-package comment-dwim-2
  :bind
  ("M-;" . comment-dwim-2))

;;;; Quotes
(use-package cycle-quotes
  :bind
  (:map
   mode-specific-map
   ("q" . cycle-quotes)))

;;; Keyboard (bindings, settings, etc.)
;;;; Reverse input method
(use-package reverse-im
  :demand

  :diminish

  :config
  (reverse-im-activate "russian-computer"))

;;;; Disable <insert> key
(define-key global-map [(insert)] nil)

;;;; Hydra
(use-package hydra)

;;;; Which key
(use-package which-key
  :demand

  :diminish

  :bind
  ("C-h C-k" . which-key-show-top-level)

  :custom
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-side-window-max-height 0.25)

  :config
  (which-key-mode))

;;;; Discover my major
(use-package discover-my-major
  :commands
  (discover-my-major discover-my-mode)

  :bind
  ("C-h C-m" . discover-my-major))

;;; Buffers and windows
;;;; Autoreload buffer at file changes
(setup-package autorevert
  :diminish auto-revert-mode

  :config
  (global-auto-revert-mode t))

;;;; Change windows size
(def-package my/window-sizing
  :preface
  (defhydra hydra-window-sizing (:hint nil)
    "
Window sizing keys
------------------------------------------
_h_: decrease horizontally    _=_: equalize
_j_: decrease vertically      _-_: balance
_k_: increase vertically      _q_: quit
_l_: increase horizontally
"
    ("h" shrink-window-horizontally)
    ("l" enlarge-window-horizontally)
    ("j" shrink-window)
    ("k" enlarge-window)
    ("-" balance-windows)
    ("=" balance-windows-area)
    ("q" nil))

  :bind
  ("C-x 4 w" . 'hydra-window-sizing/body))

;;;; Switch windows
(use-package ace-window
  :bind
  ("M-o" . ace-window)

  :custom
  (aw-scope 'frame))

;;;; Fullframe
(use-package fullframe
  :config
  (fullframe list-packages quit-window)
  (fullframe package-list-packages quit-window))

;;; Themes
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-monokai-classic t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

;;;; Modeline
;;(use-package doom-modeline
;;  :hook (after-init . doom-modeline-mode))
;;
(use-package simple-modeline
  :hook
  (after-init . simple-modeline-mode))

;;; Parens & delimiters
;;;; SmartParens & wrapping
(use-package smartparens
  :diminish

  :preface
  (defun my/no-electric-with-startparens ()
    "Disables electric parens with smartparens enabled"
    (electric-pair-local-mode -1))

  :hook
  (smartparens-mode . my/no-electric-with-startparens)

  :bind
  
  (:map
   smartparens-mode-map
   ("M-J". sp-split-sexp)
   ("C-M-J" . sp-join-sexp)
   ("C-(" . sp-backward-slurp-sexp)
   ("C-)" . sp-forward-slurp-sexp)
   ("C-{" . sp-backward-barf-sexp)
   ("C-}" . sp-forward-barf-sexp)
   ("M-<delete>" . sp-unwrap-sexp)
   ("M-<backspace>" . sp-backward-unwrap-sexp))

  :config
  (require 'smartparens-config)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil))

;;;; Neotree
(use-package neotree
  :init
  (setq neo-window-width 35)

  :bind
  ("<f8>" . neotree-toggle)

  :config
  (setq neo-smart-open t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;;;; Undo tree
(use-package undo-tree
  :diminish

  :hook
  (after-init . global-undo-tree-mode)

  :config
  (setq undo-tree-history-directory-alist
        '(("." . "~/.config/emacs/undo"))))

;;;; Colorful delimiters
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;;;; TODO/FIXME/etc keyword highlight
(use-package hl-todo
  :demand

  :custom-face
  (hl-todo ((t (:bold t :inverse-video t))))

  :custom
  (hl-todo-keyword-faces
   '(("TODO" . "#6c71c4")
     ("FIXME" . "#e74c3c")
     ("NOTE" . "#2aa198")))

  :hook
  (after-init . global-hl-todo-mode)

  :bind
  ("M-s t" . hl-todo-occur))

;;;; ElDoc
(setup-package eldoc
  :diminish)

;;; Languages
;;;; LSP
(use-package lsp-mode
  :diminish " LSP"
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp)
         (haskell-mode . lsp)
         (rust-mode . lsp)
         (racket-mode . lsp)
         (nim-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))

(use-package lsp-haskell
  :hook ((haskell-mode . (lambda ()
                          (require 'lsp-haskell)
                          (lsp)))
         (literate-haskell-mode . (lambda ()
                                    (require 'lsp-haskell)
                                    (lsp)))))

;;;; ELisp
(setup-package elisp-mode
  :hook
  (emacs-lisp-mode . eldoc-mode))

;;;; Haskell
(use-package haskell-mode
  :diminish " >>="

  :mode
  ("\\.hs\\'" . haskell-mode)
  ("\\.lhs\\'" . literate-haskell-mode)
  
  :hook
  (haskell-mode . haskell-decl-scan-mode)
  (haskell-mode . subword-mode)
  (haskell-mode . eldoc-mode)
  (haskell-mode . smartparens-mode)
  (haskell-mode . interactive-haskell-mode))

(put 'haskell-stylish-on-save 'safe-local-variable #'booleanp)
(put 'haskell-hayoo-url 'safe-local-variable #'stringp)

(use-package inf-haskell
  :ensure nil)

;;;; Racket
(use-package racket-mode
  :mode
  ("\\.rkt\\'" . racket-mode)

  :config
  (require 'racket-xp)
  
  :hook
  (racket-mode . smartparens-mode)
  (racket-mode . eldoc-mode)
  (racket-mode . racket-xp-mode))

;;;; Scheme
(setup-package scheme-mode
  :mode
  ("\\.scm$" . scheme-mode))

;;;; Python
(use-package python
  :mode
  ("\\.py\\'" . python-mode)

  :hook
  (python-mode . smartparens-mode)
  (python-mode . eldoc-mode)
  (python-mode . flycheck-mode)
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode)
  
  :config
  (setq-default python-indent-offset 4))

(use-package anaconda-mode
  :diminish anaconda-mode)

(use-package company-anaconda
  :after (company anaconda)
  :init (add-to-list 'company-backends 'company-anaconda))

(use-package py-autopep8
  :hook
  (python-mode . py-autopep8-mode))

(use-package py-isort
  :hook
  (python-mode . py-isort-before-save)
  
  :config
  (setq py-isort-options '("-sl")))

(use-package pyvenv
  :config
  (progn
    (defalias 'workon 'pyvenv-workon)
    (defalias 'activate 'pyvenv-activate)
    (defalias 'deactivate 'pyvenv-deactivate))
  (pyvenv-mode t)
  
  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

;;;; Ruby
(setup-package ruby-mode
  :mode
  ("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode)
  ("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

;;;; Rust
(use-package rust-mode
  :mode
  ("\\.rs\\'" . rust-mode)

  :config
  (add-to-list 'company-dabbrev-code-modes 'rust-mode))

(use-package flycheck-rust
  :after (rust-mode)

  :hook
  (rust-mode . my/rust-mode-hook)

  :config
  (defun my/rust-mode-hook ()
    (flycheck-rust-setup)
    (flycheck-mode)))

;;;; Nim
(use-package nim-mode)

;;;; Markdown
(use-package markdown-mode
  :custom
  (markdown-mode gfm-mode)

  :mode
  ("README.*\\.md\\'" . gfm-mode)
  ("\\.md\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode)

  :hook
  (markdown-mode . yas-minor-mode)
  (markdown-mode . smartparens-mode)

  :custom
  (markdown-header-scaling t))

;;;; HTML
(use-package web-mode
    :commands (web-mode)

    :mode
    ("\\.html?\\'" . web-mode)
    ("\\.css\\'" . web-mode)

    :preface
    (defun my/web-mode-hook ()
      (add-hook 'hack-local-variables-hook
                (defun my/web-mode-local-hook ()
                  (when web-mode-engines-alist
                    (web-mode-guess-engine-and-content-type)
                    (unless (string= web-mode-engine "none")
                      (web-mode-set-engine web-mode-engine))))
                0 t))

    :hook
    (web-mode . company-mode)
    (web-mode . my/web-mode-hook)

    :custom
    (web-mode-enable-css-colorization t)
    (web-mode-enable-engine-detection nil)
    (web-mode-code-indent-offset 2)
    (web-mode-markup-indent-offset 2)
    (web-mode-script-padding 2)
    (web-mode-css-indent-offset 2)
    (web-mode-style-padding 2)

    :config
    (add-to-list 'company-backends 'company-css))

  (put 'web-mode-engine 'safe-local-variable #'stringp)
  (put 'web-mode-engines-alist 'safe-local-variable #'listp)

;;;; TOML
(use-package toml-mode
  :mode "\\.toml\\'"

  :hook
  (toml-mode . smartparens-mode))

;;;; CSV
(use-package csv-mode
    :mode "\\.[Cc][Ss][Vv]\\'")

;;;; Shell
(setup-package sh-script
  :mode
  ("\\.sh\\'" . shell-script-mode)
  ("\\.bash\\'" . shell-script-mode))

;;;; VimScript
(use-package vimrc-mode
  :mode "\\.vim\\(rc\\)?\\'"

  :hook
  (vimrc-mode . smartparens-mode))

;;; IDE
;;;; Autocomplete and abbreviation
(setup-package abbrev
  :diminish)

(setup-package dabbrev
  :commands (dabbrev-expand dabbrev-completion)

  :custom
  (dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (dabbrev-abbrev-skip-leading-regexp "\\$\\|\\*\\|/\\|=")
  (dabbrev-backward-only nil)
  (dabbrev-case-distinction nil)
  (dabbrev-case-fold-search t)
  (dabbrev-case-replace nil)
  (dabbrev-check-other-buffers t)
  (dabbrev-eliminate-newlines nil)
  (dabbrev-upcase-means-case-search t))

;;;; Company (autocomplete)
(use-package company
  :demand

  :diminish company-mode

  :hook
  (after-init . global-company-mode)

  :custom
  (company-idle-delay 0.3)
  (company-minimum-prefix-length 1)
  (company-tooltip-minimum 4)
  (company-tooltip-flip-when-above t)
  (company-tooltip-limit 10)
  (company-tooltip-offset-display 'lines)
  (company-tooltip-align-annotations t)
  (company-begin-commands '(self-insert-command))
  (company-selection-wrap-around t)
  (company-show-numbers nil)
  (company-require-match nil)
  (company-transformers '(company-sort-by-backend-importance
                          company-sort-prefer-same-case-prefix
                          company-sort-by-occurrence))

  :bind
  (:map
   company-active-map
   ("<tab>" . company-complete-selection)
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)))

(use-package company-flx
  :after (company)

  :hook
  (company-mode . company-flx-mode))

(use-package company-quickhelp
  :after (company)

  :diminish company-quickhelp-mode

  :bind
  (:map
   company-active-map
   ("C-d" . company-quickhelp-manual-begin)))

;;;; Ivy
(use-package ivy
  :demand

  :diminish

  :custom
  (ivy-use-virtual-buffers t)
  (ivy-initial-inputs-alist nil)

  :config
  (setq ivy-re-builders-alist
        '((swiper           . ivy--regex-plus)
          (counsel-git-grep . ivy--regex-plus)
          (counsel-grep     . ivy--regex-plus)
          (counsel-ag       . ivy--regex-plus)
          (counsel-rg       . ivy--regex-plus)
          (t                . ivy--regex-fuzzy)))

  :hook
  (after-init . ivy-mode)

  :bind
  (:map
   ivy-minibuffer-map
   ("TAB" . ivy-partial)))

(use-package counsel
  :demand

  :diminish

  :bind
  ([remap insert-char] . counsel-unicode-char)
  (:map
   counsel-mode-map
   ("M-g m" . counsel-mark-ring)
   ("M-g o" . counsel-outline))

  :hook
  (after-init . counsel-mode))

(use-package swiper
  :bind
  (:map
   search-map
   ("." . swiper-thing-at-point)
   ("s" . swiper))

  (:map
   isearch-mode-map
   ("M-s s" . swiper-from-isearch)))

(use-package flx)
(use-package ivy-hydra)

;;;; Projectile
(use-package projectile
  :custom
  (projectile-keymap-prefix (kbd "C-c p"))
  (projectile-mode-line-function
   (lambda () (format " [%s]" (projectile-project-name)))))

(use-package counsel-projectile
  :after (projectile counsel ivy)

  :custom
  (projectile-completion-system 'ivy)

  :hook
  (after-init . counsel-projectile-mode))

(use-package projectile-ripgrep
  :if (package-installed-p 'ripgrep)

  :after (projectile)

  :bind
  (:map
   projectile-command-map
   ("s r" . projectile-ripgrep)))

(put 'projectile-tags-file-name 'safe-local-variable #'stringp)
(put 'projectile-globally-ignored-files 'safe-local-variable #'listp)
(put 'projectile-globally-ignored-directories 'safe-local-variable #'listp)

;;;; Yasnippet
(use-package yasnippet
  :diminish (yas-minor-mode . " Y")

  :hook
  (prog-mode . yas-minor-mode)

  :bind
  (:map
   yas-minor-mode-map
   (("C-c <tab>" . yas-expand)
    ("C-c C-n" . yas-new-snippet)))

  :config
  (unbind-key "<tab>" yas-minor-mode-map)
  (unbind-key "TAB" yas-minor-mode-map)

  (let ((my/snippets (concat user-emacs-directory "snippets")))
    (when (file-exists-p my/snippets)
      (add-to-list 'yas/snippet-dirs my/snippets))
    (yas-reload-all)))

;;;; Flymake
(setup-package flymake
  :diminish flymake-mode)

;;;; Flycheck
(use-package flycheck
  :diminish " FC"

  :custom
  (flycheck-check-syntax-automatically
   '(mode-enabled new-line save) "Check on mode enabled, creating new line & save"))

(put 'safe-local-variable-values 'flycheck-checker 'flycheck-ghc)

(use-package flycheck-color-mode-line
  :after (flycheck)

  :hook
  (flycheck-mode . flycheck-color-mode-line-mode))

;;; Git
(use-package magit
  :custom
  (magit-log-arguments '("--graph" "--color" "--decorate"))

  :config
  (setq magit-save-repository-buffers nil)

  :bind
  ("C-x g" . magit-status))

;;; Org-mode
;;;; Org
(use-package org
  :pin gnu

  :mode ("\\.org\\'" . org-mode)

  :bind
  ("<f12>" . my/org-open-notes-file)
  (:map
   mode-specific-map
   ("C" . org-capture)
   ("<backspace>" . org-mark-ring-goto))

  (:map
   org-mode-map
   ("C-c M-RET" . org-insert-heading-after-current))

  :hook
  (org-mode . yas-minor-mode)
  (org-mode . smartparens-mode)
  
  :custom
  (org-directory "~/org")
  (org-default-notes-file "~/org/notes.org")
  (org-edit-src-content-indentstion 0)
  (org-src-preserve-indentation t)
  (org-adapt-indentation nil)
  (org-ellipsis "…")
  (org-enforce-todo-dependencies t)
  (org-outline-path-complete-in-steps nil)
  (org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "DONE")))
  (org-startup-indented 1)
  (org-babel-python-command "python3")
  (org-log-done t)

  :config
  (require 'ob-shell)
  (require 'ob-python)
  (require 'ob-haskell)
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (emacs-lisp . t)
     (python . t)
     (haskell . t)))

  (defun my/org-open-notes-file ()
    (interactive)
    (if (file-exists-p org-default-notes-file)
        (find-file org-default-notes-file)
      (message "%s doesn't exist!" org-default-notes-file))))

(use-package org-appear
  :after (org)

  :hook
  (org-mode . org-appear-mode)

  :custom
  (org-appear-autolinks t))

(use-package org-bullets
  :after (org)

  :hook
  (org-mode . org-bullets-mode)

  :custom
  (org-bullets-bullet-list '("●" "○" "⦿" "⦾")))

(use-package org-roam
  ;:ensure t

  :custom
  (org-roam-directory (file-truename "~/org/roam"))

  ;:init
  ;(setq org-roam-v2-ack t)

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture))

  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
;  :straight
;    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package htmlize
  :after (org))