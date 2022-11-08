;;; package --- Summary
;;; Commentary:
;;; Main EMACS config file, load settings from parts

;;; Code:

;; Increase gc-limit up to 100 mb for boot speedup
(setq gc-cons-threshold 100000000)

;; Make a shortcut for init.el
(defun my/configfile ()
  "Opens 'user-init-file'."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(global-set-key (kbd "M-<f12>") 'my/configfile)

;;; Main section
;;; Fonts settings
(set-face-attribute 'default nil :height 130)
(when (member "Hack" (font-family-list))
  (set-face-attribute 'default nil :font "Hack"))

;;; Enable hotkeys in other keyboard layouts
(defun cfg:reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(cfg:reverse-input-method 'russian-computer)

;;; Packages
(require 'package)

;; Packages sources
(setq package-archives
      `(("melpa" . "https://melpa.org/packages/")
        ,@package-archives))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq tls-checktrust "ask")
(setq tls-program
      '("gnutls-cli --x509cafile /etc/ssl/certs/ca-certificates.crt -p %p %h"))
(package-initialize)

;; If 'gnu-epla-keyring-update' not install download and install it
(unless (package-installed-p 'gnu-elpa-keyring-update)
  (setq-default package-check-signature 'allow-unsigned)
  (setq-default package-unsigned-archives '("gnu"))
  (package-refresh-contents)
  (package-install 'gnu-elpa-keyring-update)
  (kill-emacs))

(require 'gnu-elpa-keyring-update)

;; If 'use-package' does not install download and install it
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

(use-package quelpa
  :custom
  (quelpa-upgrade-interval 14))

(use-package quelpa-use-package
  :config
  (quelpa-use-package-activate-advice))

(use-package diminish)
(use-package bind-key)

;; Macro for 'use-package'
(defmacro def-package (name &rest body)
  "Defines a virtual package (NAME/BODY)."
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

;;;; Overlays (kinda)
(require 'cl-lib)

(defvar my/overlays-file "~/.config/emacs.default/overlays")

(defvar my/overlays
  "A list of enabled package sets (list of strings).

If some name is listed here then corresponding package set will be loaded.
Each overlay is just a :if-condition for the use-package."
  '())

(defun my/overlays-configure ()
  "Reloads the overlay list.

Note: It won't trigger any use-packag'ing!"
  (interactive)
  (when (file-exists-p my/overlays-file)
    (with-temp-buffer
      (insert-file-contents my/overlays-file)
      (let ((overlays (split-string (buffer-string))))
        (set-variable 'my/overlays overlays)))))

(my/overlays-configure)

(defun my/overlay-enabled? (STRING)
  (cl-block nil
    (dolist (o my/overlays)
      (when (string= STRING o)
        (cl-return t)))))

(defmacro overlay (name &rest body)
  "Evaluates the body iff the overlay is enabled."
  `(when (my/overlay-enabled? ,(symbol-name name))
     ,@body))

(put 'overlay 'lisp-indent-function 1)

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
  (put 'narrow-to-region 'disabled nil)

  :custom
  (inhibit-startup-screen t "No startup screen")
  (initial-scratch-message "")
  (indicate-empty-lines t)
  (use-dialog-box nil "Disable dialog boxes")
  (enable-recursive-minibuffers t "Allow minibuffer commands in the minibuffer")
  (save-interprogram-paste-before-kill t)
  (mouse-yank-at-point t "Yank at point using mouse")
  (resize-mini-windows t)
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
  (ring-bell-function #'(lambda ()) "No bells, please!")
  ;; Window
  (tool-bar-mode nil)
  (scroll-bar-mode nil)
  (menu-bar-mode nil)
  (frame-title-format "Emacs: %b")
  ;; Cursor
  (line-number-mode t)
  (column-number-mode t)
  (mode-line-position
   '((mode-line-percent-position "%p ")
     (line-number-mode ("%l" (column-number-mode ":%c")))))
  (blink-cursor-mode nil)
  (x-stretch-cursor t)
  (shift-select-mode nil "No selection with <shift>")
  ;; Exit confirmation
  (kill-emacs-query-functions
   (cons (lambda () (yes-or-no-p "Really Quit Emacs? "))
         kill-emacs-query-functions))

  :config
  (global-prettify-symbols-mode)
  (prefer-coding-system 'utf-8)
  (put 'overwrite-mode 'disabled t))

(setup-package frame
  :bind
  ("C-z" . nil))

(setup-package simple
  :bind
  ([remap capitalize-word] . capitalize-dwim))

;;; Date/Time
(setup-package time
  :preface
  (defun insert-today ()
    (interactive)
    (insert (format-time-string "%Y-%m-%d" (current-time))))

  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  (calendar-week-start-day 1)
  (calendar-date-style 'european))

;;; Files
(setup-package files
  :preface
  (setq-default
   my/backup-directory-per-session
   (format "%sbackups/per-session" user-emacs-directory)

   my/backup-directory-per-save
   (format "%sbackups/per-save" user-emacs-directory))

  (defun my/backup-buffer ()
    (when (not buffer-backed-up)
      (let ((backup-directory-alist
             `((".*" . ,my/backup-directory-per-session)))
            (kept-new-versions 3))
        (backup-buffer)))
    ;; back up unconditionaly
    (let ((buffer-backed-up nil))
      (backup-buffer)))

  (defun my/kill-buffer-file-name ()
    "Kills a file name of the current buffer if any."
    (interactive)
    (if-let ((name (buffer-file-name (current-buffer))))
        (kill-new name)
      (message "This bufer does'n have a file name")))

  :hook
  (before-save . my/backup-buffer)

  :custom
  ;; backup settings
  (backup-directory-alist `((".*" . ,my/backup-directory-per-save)))
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 10)
  (kept-old-versions 0)
  (version-control t)
  (vc-make-backup-files t)
  (create-lockfiles nil)
  ;; autosave
  (auto-save-default nil))

(setup-package autorevert
  :diminish auto-revert-mode

  :config
  (global-auto-revert-mode t))

(use-package f)

(setup-package recentf
  :after (f)

  :preface
  (defconst my/emacs-packages-directory (format "%selpa" user-emacs-directory)
    "A directory where Emacs keeps packages.")

  (defun my/in-packages-directory-p (path)
    "Non-nil if path is inside of the directory where Emacs keeps packages."
    (f-ancestor-of? my/emacs-packages-directory path))

  :custom
  (recentf-exclude (list #'my/in-packages-directory-p)))

(use-package backup-walker
  :commands (backup-walker-start))

;;;; Theme
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium t))

;;; Languages
;;;; LSP
(overlay lsp
  (use-package eglot
    :commands (eglot)))

;;;; ELisp
(setup-package elisp-mode
  :hook
  (emacs-lisp-mode . eldoc-mode)
  (emacs-lisp-mode . aggressive-indent-mode))

(def-package my/helpful-elisp-mode
  :after (elisp-mode helpful)

  :bind
  (:map
   emacs-lisp-mode-map
   ("C-c C-d" . helpful-at-point)))


;;;; Haskell
(overlay haskell
  (use-package haskell-mode
    :diminish haskell-mode

    :magic
    (".*env stack" . haskell-mode)
    (".*env cabal" . haskell-mode)

    :mode
    ("\\.hs\\'" . haskell-mode)
    ("\\.lhs\\'" . literate-haskell-mode)
    ("\\.cabal\\'" . haskell-cabal-mode)
    ("\\.hamlet\\'" . shakespeare-hamlet-mode)
    ("\\.julius\\'" . shakespeare-julius-mode)
    ("routes\\'" . haskell-yesod-parse-routes-mode)

    :bind
    (:map
     haskell-mode-map
     :prefix
     "C-c SPC"
     :prefix-map
     my/haskell-map
     ("v" . haskell-cabal-visit-file)
     ("m" . haskell-auto-insert-module-template)
     ("I" . haskell-sort-imports)
     ("A" . haskell-align-imports)
     ("S" . haskell-mode-stylish-buffer)
     ("y" . haskell-hayoo)
     ("h" . haskell-hide-toggle)
     ("u" . my/haskell-swiper-todos))

    :hook
    (haskell-mode . haskell-decl-scan-mode)
    (haskell-mode . subword-mode)
    (haskell-mode . eldoc-mode)
    (haskell-mode . smartparens-mode)
    (haskell-mode . my/boot-haskell)
    (haskell-mode . interactive-haskell-mode)

    :config
    (defun my/haskell-swiper-todos ()
      "Shows the Swiper for todo-like items."
      (interactive)
      (swiper "undefined\\|TODO\\|FIXME"))

    (defun my/boot-haskell ()
      "Initialize haskell stuff"
      (interactive)
      (setq tags-case-fold-search nil))

    ;; hemmet
    (defun my/hemmet-expand-region (&optional b e)
      (interactive "r")
      (shell-command-on-region
       b e "hemmet bem react-flux" (current-buffer) t "*hemmet error*" t))

    ;; yesod handlers scaffolding
    (defun my/haskell-scaffold-yesod-handlers ()
      "Kills a current line (that containins an Yesod route) and
     scaffolds yesod handler for each of methods"
      (interactive)
      (let* ((p1 (line-beginning-position))
             (p2 (line-end-position))
             (lval (buffer-substring-no-properties p1 p2))
             (tokens (cdr (split-string lval))))
        (if (> 2 (length tokens))
            (message "%s" "Not enough tokens")
          (let ((rname (car tokens))
                (methods (cdr tokens)))
            (progn
              (kill-whole-line)
              (previous-line)
              (loop
               for m in methods do
               (let* ((name (concat (downcase m) rname))
                      (l1 (concat name " :: Handler TypedContent"))
                      (l2 (concat name " = error \"" name " not implemented\"")))
                 (end-of-line)
                 (newline)
                 (insert l1) (newline)
                 (insert l2) (newline)))))))))

  (put 'haskell-stylish-on-save 'safe-local-variable #'booleanp)
  (put 'haskell-hayoo-url 'safe-local-variable #'stringp)

  (use-package hi2
    :after (haskell-mode)

    :diminish

    :hook
    (haskell-mode . hi2-mode)

    :custom
    (hi2-layout-offset 2)
    (hi2-left-offset 2)
    (hi2-where-post-offset 2)

    :bind
    (:map
     hi2-mode-map
     ("<tab>" . hi2-indent-line)))

  (put 'hi2-where-post-offset 'safe-local-variable #'numberp)
  (put 'hi2-left-offset 'safe-local-variable #'numberp)
  (put 'hi2-layout-offset 'safe-local-variable #'numberp)

  (use-package company-cabal
    :after (haskell-mode)

    :config
    (add-to-list 'company-backends 'company-cabal))

  (use-package shakespeare-mode
    :after (haskell-mode))

  (use-package inf-haskell
    :ensure nil))

;;;; Python
(overlay python
  (setup-package python
    :mode
    ("\\.py\\'" . python-mode)

    :hook
    (python-mode . smartparens-mode)

    :bind
    (:map
     python-mode-map
     ("C-c C-c" . compile)))

  (use-package elpy
    :after (python)

    :hook
    (elpy-mode . flycheck-mode)

    :custom
    (elpy-rpc-python-command "python3")
    (elpy-rpc-virtualenv-path 'current)
    (elpy-modules
     '(elpy-module-company
       elpy-module-eldoc
       elpy-module-highlight-indentation
       elpy-module-django))

    :config
    (elpy-enable)
    (unbind-key "<C-left>" elpy-mode-map)
    (unbind-key "<C-right>" elpy-mode-map))

  (def-package my/python
    :custom
    (python-shell-interpreter "python3")
    (lsp-pylsp-configuration-sources ["flake8"])

    :preface
    (defun my/python-mode-hook ()
      (if (executable-find "pylsp")
          (lsp)
        (elpy-mode)))

    :hook
    (python-mode . my/python-mode-hook)))

;;;; Rust
(overlay rust
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
      (flycheck-mode))))

;;;; Markdown
(overlay markdown
  (use-package markdown-mode
    :commands (markdown-mode gfm-mode)

    :mode
    ("README.*\\.md\\'" . gfm-mode)
    ("\\.md\\'" . markdown-mode)
    ("\\.markdown\\'" . markdown-mode)

    :hook
    (markdown-mode . variable-pitch-mode)
    (markdown-mode . yas-minor-mode)
    (markdown-mode . smartparens-mode)

    :custom
    (markdown-command "pandoc")
    (markdown-header-scaling t)

    :config
    (unbind-key "DEL" gfm-mode-map))

  (def-package my/markdown
    :after (markdown-mode my/github)

    :preface
    (defun my/markdown/capture-gh-link (&optional arg)
      "Insert a MD-link for the killed GitHub URL."
      (interactive "p")
      (let* ((url (current-kill 0))
             (match (my/github/match-file-url url))
             (path (alist-get 'path match))
             (line (alist-get 'line match))
             (ref (alist-get 'ref match)))
        (if path
            (if (or (my/github/commit-hash-p ref) (= 4 arg))
                (insert
                 (format "[`%s%s`](%s)"
                         path
                         (if line (format ":%s" line) "")
                         url))
              (message "%s" "Non-local ref!"))
          (message "%s" "Non-github link!"))))

    :bind
    (:map
     markdown-mode-map
     ("C-c l" . my/markdown/capture-gh-link)

     :map
     gfm-mode-map
     ("C-c l" . my/markdown/capture-gh-link)))

  (def-package my/markdown-binding-fixes
    :after (markdown-mode)

    :bind
    (:map
     markdown-mode-map
     ("C-." . undo-tree-undo)
     ("C-," . undo-tree-redo))

    (:map
     gfm-mode-map
     ("C-." . undo-tree-undo)
     ("C-," . undo-tree-redo))))

;;;; Go
(overlay go
  (use-package go-mode
    :mode "\\.go\\'"

    :commands (go-mode)

    :hook (flycheck-mode)

    :config
    (defun my/go-mode-hook ()
      (add-hook 'before-save-hook #'gofmt-before-save))

    (when (executable-find "gofmt")
      (add-hook 'go-mode-hook #'my/go-mode-hook))))
;;;; Web
(overlay web
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
  (put 'web-mode-engines-alist 'safe-local-variable #'listp))


;;;; CSV
(overlay csv
  (use-package csv-mode
    :mode "\\.[Cc][Ss][Vv]\\'"))

;;;; Nix
(overlay nix
  (use-package nix-mode
    :mode "\\.nix\\'"))

;;;; SQL
(setq-default sql-dialect 'sql-postgres)

;;;; Shell
(setup-package sh-script
  :mode
  ("\\.ok\\'" . shell-script-mode)
  ("\\.sh\\'" . shell-script-mode)
  ("\\.bash\\'" . shell-script-mode))


;;; IDE
;;;; Autocompletion and abbreviation
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

(setup-package hippie
  :after (dabbrev)

  :custom
  (hippie-expand-try-functions-list
   '(try-expand-dabbrev-visible
     try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-expand-list-all-buffers
     try-expand-list
     try-expand-line-all-buffers
     try-expand-line
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs))

  :bind
  ([remap dabbrev-expand] . hippie-expand))

(use-package company
  :demand

  :diminish

  :hook
  (after-init . global-company-mode)

  :custom
  (company-minimum-prefix-length 1)
  (company-tooltip-limit 30)
  (company-idle-delay nil)
  (company-begin-commands '(self-insert-command))
  (company-selection-wrap-around t)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-require-match nil)
  (company-transformers '(company-sort-by-backend-importance
                          company-sort-prefer-same-case-prefix
                          company-sort-by-occurrence))

  :bind
  (:map
   mode-specific-map
   ("/" . company-files))
  (:map
   company-mode-map
   ("M-<tab>" . company-manual-begin))
  (:map
   company-active-map
   ("<tab>" . company-complete-selection)
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)))

(use-package company-try-hard
  :after (company)

  :bind
  (:map
   company-active-map
   ("M-<tab>" . company-try-hard)))

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

;;;; Flycheck
(use-package flycheck
  :diminish " FC"

  :custom
  (flycheck-check-syntax-automatically
   '(save mode-enabled) "Only check on save")

  :bind
  (:map
   flycheck-mode-map
   ("<f5>" . flycheck-buffer)))

(put 'safe-local-variable-values 'flycheck-checker 'flycheck-ghc)

(use-package flycheck-color-mode-line
  :after (flycheck)

  :hook
  (flycheck-mode . flycheck-color-mode-line-mode))

;;;; Flymake
(setup-package flymake
  :preface
  (defvar my/flymake-minor-mode-map (make-keymap))
  (define-minor-mode my/flymake-minor-mode
    "Auxiliary minor mode for flymake-minor-mode"
    nil nil 'my/flymake-minor-mode-map)

  :hook
  (flymake-mode . my/flymake-minor-mode)

  :bind
  (:map
   my/flymake-minor-mode-map
   ("M-g p" . flymake-goto-prev-error)
   ("M-g n" . flymake-goto-next-error)
   ("M-g M-p" . flymake-goto-prev-error)
   ("M-g M-n" . flymake-goto-next-error)))

;;;; Yasnippet
(overlay yasnippet
  (use-package yasnippet
    :diminish (yas-minor-mode . " Y")

    :preface
    (setq-default my/yas-map (make-sparse-keymap "My Yasnippet map"))

    :bind
    (:map
     mode-specific-map
     :prefix
     "y"
     :prefix-map
     my/yas-map
     ("<tab>" . company-yasnippet))

    (:map
     yas-minor-mode-map
     ("C-c <tab>" . yas-expand))

    :hook
    (prog-mode . yas-minor-mode)
    (html-mode . yas-minor-mode)

    :config
    (unbind-key "<tab>" yas-minor-mode-map)
    (unbind-key "TAB" yas-minor-mode-map)

    (let ((my-snippets (concat user-emacs-directory "snippets")))
      (when (file-exists-p my-snippets)
        (add-to-list 'yas/snippet-dirs my-snippets)))
    (yas-reload-all))

  (use-package yasnippet-snippets
    :after (yasnippet))

  (use-package yankpad
    :after (yasnippet)

    :custom
    (yankpad-file "~/Dropbox/org/yankpad.org")

    :bind
    (:map
     my/yas-map
     ("m" . yankpad-map)
     ("y" . yankpad-insert)))

  (put 'yankpad-file 'safe-local-variable #'stringp))

;;;; Grep'likes
(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;; sudo apt-get install silversearcher-ag
(use-package ag
  :if (executable-find "ag")
  :custom
  (ag-highlight-search t))

;; install from github
(use-package rg
  :if (executable-find "rg")
  :after (wgrep)

  :custom
  (rg-group-result t)
  (rg-hide-command t)
  (rg-show-columns nil)
  (rg-show-header t)
  (rg-custom-type-aliases nil)
  (rg-default-alias-fallback "all")

  :config
  ;; source: https://protesilaos.com/dotemacs
  (rg-define-search rg/grep-vc-or-dir
    :query ask
    :format regexp
    :files "everything"
    :dir (let ((vc (vc-root-dir)))
           (if vc
               vc                         ; search root project dir
             default-directory))          ; or from the current dir
    :confirm prefix
    :flags ("--hidden -g !*.lock"))

  ;; source: https://protesilaos.com/dotemacs
  (defun rg/save-search-as-name ()
    "Save `rg' buffer, naming it after the current search query."
    (interactive)
    (let ((pattern (car rg-pattern-history)))
      (rg-save-search-as-name (concat "«" pattern "»"))))

  :bind
  ("M-s g" . rg/grep-vc-or-dir)
  (:map
   rg-mode-map
   ("s" . rg/save-search-as-name)
   ("C-n" . next-line)
   ("C-p" . previous-line)
   ("M-n" . rg-next-file)
   ("M-p" . rg-prev-file)))

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

;;;; project.el
(setup-package project
  :preface
  (defun project-try-projectile (dir)
    "Find a super-directory of DIR containing a .projectile file."
    (let ((dir (locate-dominating-file dir ".projectile")))
      (and dir (cons 'projectile dir))))

  (defmethod project-root ((project (head projectile)))
    (cdr project))

  :config
  (add-hook 'project-find-functions
            #'project-try-projectile))

;;;; Terminal here
(use-package terminal-here
  :after (projectile)

  :bind
  (:map
   mode-specific-map
   :prefix
   "t"
   :prefix-map
   my/terminal-here-map
   ("t" . terminal-here-launch)
   ("p" . terminal-here-project-launch))

  :custom
  (terminal-here-project-root-function 'projectile-project-root)
  (terminal-here-terminal-command (list "x-terminal-emulator")))

;;;; RESTclient
(overlay restclient
  (use-package restclient
    :commands
    (restclient-mode))

  (use-package company-restclient
    :after (restclient)

    :hook
    (restclient-mode . my/restclient-mode-hook)

    :config
    (defun my/restclient-mode-hook ()
      (add-to-list 'company-backends
                   'company-restclient))))

;;;; Docker
(overlay docker
  (use-package docker
    :commands (docker)))

;;; Spell Checking
(setup-package ispell
  :if (executable-find "hunspell")

  :preface
  ;; FIXME: remove after the Debian's site-lisp will be fixed
  (unless (boundp 'ispell-menu-map-needed)
    (defvar ispell-menu-map-needed nil))

  :commands
  (ispell-buffer)

  :custom
  (ispell-really-aspell nil)
  (ispell-really-hunspell t)
  (ispell-encoding8-command t)
  (ispell-program-name "hunspell")
  (ispell-dictionary "ru_RU,en_US")

  :config
  (when (executable-find "hunspell-wrapper")
    (setq-default ispell-program-name "hunspell-wrapper"))
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "ru_RU,en_US"))

(setup-package flyspell
  :commands (flyspell-buffer flyspell-mode)

  :bind
  ("M-<f5>" . flyspell-buffer)
  ("M-<f8>" . flyspell-goto-next-error)
  (:map
   mode-specific-map
   ("s" . flyspell-correct-word-before-point)))

;;; Org-mode/Outline
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
   ("C-c M-RET" . org-insert-heading-after-current)
   ([remap org-insert-link] . my/org-insert-link-dwim))

  :hook
  (org-mode . variable-pitch-mode)
  (org-mode . yas-minor-mode)
  (org-mode . smartparens-mode)
  (org-mode . olivetti-mode)
  (org-mode . my/org-mode-hook)

  :custom-face
  (org-link ((t (:inherit link :bold nil))))
  (org-code ((t (:inherit fixed-pitch))))
  (org-block ((t (:inherit fixed-pitch))))
  (org-block-begin-line ((t (:inherit fixed-pitch))))
  (org-block-end-line ((t (:inherit fixed-pitch))))
  (org-table ((t (:inherit fixed-pitch))))
  (org-code ((t (:inherit fixed-pitch))))
  (org-tag ((t (:weight normal :height 0.8))))

  :custom
  (org-directory "~/org")
  (org-default-notes-file "~/org/notes.org")
  (org-edit-src-content-indentation 0)
  (org-ellipsis "…")
  (org-enforce-todo-dependencies t)
  (org-export-use-babel nil)
  (org-export-with-sub-superscripts nil)
  (org-export-with-toc nil)
  (org-hide-leading-stars t)
  (org-html-postamble nil)
  (org-html-preamble nil)
  (org-outline-path-complete-in-steps nil)
  (org-src-preserve-indentation t)
  (org-src-window-setup 'current-window)
  (org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "DONE")))
  (org-use-sub-superscripts nil)
  (org-adapt-indentation nil)
  (org-return-follows-link t)
  (org-catch-invisible-edits 'smart)

  :config
  (require 'ob-shell)
  (require 'ob-python)
  (require 'ob-haskell)

  (defvar my/org-babel-langs
    '((shell . t)
      (emacs-lisp . t)
      (python . t)
      (haskell . t)))

  (defun my/org-find-file (file)
    "Find file, do it in other window witn C-u"
    (if current-prefix-arg
        (find-file-other-window file)
      (find-file file)))

  (defun my/org-open-notes-file ()
    (interactive)
    (if (file-exists-p org-default-notes-file)
        (find-file org-default-notes-file)
      (message "%s doesn't exist!" org-default-notes-file)))

  (setq org-link-frame-setup
        (cons '(file . my/org-find-file)
              (assq-delete-all 'file org-link-frame-setup)))

  (defun my/org-babel-load-langs ()
    (org-babel-do-load-languages
     'org-babel-load-languages
     my/org-babel-langs))

  (defun my/org-mode-hook ()
    "Tweaks an org-mode"
    (setq-local truncate-lines nil))

  ;; src: https://xenodium.com/emacs-dwim-do-what-i-mean/
  (defun my/org-insert-link-dwim ()
    "Like `org-insert-link' but with personal dwim preferences."
    (interactive)
    (let* ((point-in-link (org-in-regexp org-link-any-re 1))
           (clipboard-url (when (string-match-p "^http" (current-kill 0))
                            (current-kill 0)))
           (region-content (when (region-active-p)
                             (buffer-substring-no-properties (region-beginning)
                                                             (region-end)))))
      (cond ((and region-content clipboard-url (not point-in-link))
             (delete-region (region-beginning) (region-end))
             (insert (org-make-link-string clipboard-url region-content)))
            ((and clipboard-url (not point-in-link))
             (insert (org-make-link-string
                      clipboard-url
                      (read-string
                       "title: "
                       (with-current-buffer
                           (url-retrieve-synchronously clipboard-url)
                         (dom-text
                          (car
                           (dom-by-tag (libxml-parse-html-region
                                        (point-min)
                                        (point-max))
                                       'title))))))))
            (t
             (call-interactively 'org-insert-link)))))

  (setq
   org-capture-templates
   '(("t" "Add a daily note" entry
      (file+olp+datetree "" "Daily")
      "* %?\n%i"
      )
     )))

(put 'org-default-notes-file 'safe-local-variable #'stringp)
(put 'org-export-use-babel 'safe-local-variable #'null)

(use-package org-appear
  :after (org)

  :custom
  (org-appear-autolinks t))

(use-package org-bullets
  :after (org)

  :hook
  (org-mode . org-bullets-mode)

  :custom
  (org-bullets-bullet-list '("●" "○" "⦿" "⦾")))

(use-package org-menu
  :after (org)

  :quelpa (org-menu :repo "sheijk/org-menu" :fetcher github)

  :bind
  (:map
   org-mode-map
   ("C-c SPC" . org-menu)))

(use-package htmlize
  :after (org))

(use-package ox-pandoc
  :if (executable-find "pandoc")

  :after (org))

(use-package ox-gfm
  :after (org))

(overlay restclient
  (use-package ob-restclient
    :after (org restclient)

    :commands (org-babel-execute:restclient)

    :config
    (add-to-list 'my/org-babel-langs '(restclient . t))
    (my/org-babel-load-langs)))

(def-package my/org/github
  :after (org my/github)

  :preface
  (defun my/org-capture-gh-link (&optional arg)
    "Insert a MD-link for the killed GitHub URL."
    (interactive "p")
    (let* ((url (current-kill 0))
           (match (my/github/match-file-url url))
           (path (alist-get 'path match))
           (line (alist-get 'line match))
           (ref (alist-get 'ref match)))
      (if path
          (if (or (my/github/commit-hash-p ref) (= 4 arg))
              (insert
               (org-make-link-string
                url
                (format "%s%s" path (if line (format ":%s" line) ""))))
            (message "%s" "Non-local ref!"))
        (message "%s" "Non-github link!"))))

  :bind
  (:map
   org-mode-map
   ("C-c L" . my/org-capture-gh-link)))

(def-package my/org-slideshow
  :after (org)

  :commands
  (my/org-simple-slideshow
   my/org-simple-slideshow-next
   my/org-simple-slideshow-prev)

  :bind
  (:map
   org-mode-map
   ("<f5>" . my/org-simple-slideshow))

  :config
  (defun my/org-renarrow (move)
    (when (buffer-narrowed-p)
      (beginning-of-buffer)
      (widen)
      (let ((pos (funcall move)))
        (when pos
          (org-narrow-to-subtree)
          pos))))

  (defun my/org-try-renarrow (move)
    (when (buffer-narrowed-p)
      (when (save-excursion
              (save-restriction
                (my/org-renarrow move)))
        (my/org-renarrow move))))

  (defun my/org-simple-slideshow-first ()
    (interactive)
    (when (buffer-narrowed-p)
      (widen)
      (beginning-of-buffer)
      (org-next-visible-heading 1)
      (when (org-at-heading-p)
        (org-narrow-to-subtree))))

  (defun my/org-simple-slideshow-next ()
    (interactive)
    (my/org-try-renarrow #'org-get-next-sibling))

  (defun my/org-simple-slideshow-prev ()
    (interactive)
    (my/org-try-renarrow #'org-get-last-sibling))

  (defhydra my/org-simple-slideshow-hydra ()
    "Slideshow"
    ("<home>" my/org-simple-slideshow-first "First")
    ("<prior>" my/org-simple-slideshow-prev "Prev")
    ("<next>" my/org-simple-slideshow-next "Next")
    ("q" widen "Stop" :exit t))

  (defun my/org-simple-slideshow ()
    (interactive)
    (unless (buffer-narrowed-p)
      (org-narrow-to-subtree))
    (when (buffer-narrowed-p)
      (my/org-simple-slideshow-hydra/body))))


;;; Finalization
;; restore GC-limit after timeout
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold 1000000)))

;;; init.el ends here
