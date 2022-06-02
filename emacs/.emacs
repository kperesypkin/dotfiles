;; package --- Summary
;; Commentary:
;; Main EMACS config file, load settings from parts

;; System-type definition
(defun system-is-linux()
  (string-equal system-type "gnu/linux"))
(defun system-is-windows()
  (string-equal system-type "windows-nt"))

;; Start EMACS as a server in linux
(when (system-is-linux)
  (require 'server)
  (unless (server-running-p)
    (server-start)))

;; Main section
;;; Fonts settings
(set-face-attribute 'default nil :height 120)
(when (member "Monospace" (font-family-list))
  (set-face-attribute 'default nil :font "Monospace"))

;;; Hotkeys enable in other keyboard layouts
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

;; Folding blocks of code
(defvar hs-special-modes-alist
  (mapcar 'purecopy
  '((c-mode "{" "}" "/[*/]" nil nil)
    (c++-mode "{" "}" "/[*/]" nil nil)
    (js-mode "{" "}" "/[*/]" nil nil)
    (python-mode "{" "}" "/[*/]" nil nil)
    (elisp-mode "(" ")" nil))))

(require 'hideshow)
(global-set-key (kbd "M-p") 'hs-toggle-hiding)

;; Packages
(require 'package)

;;; Packages sources. Use Melpa instead of Melpa Stable for the oldest versions

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize nil)

;;; If use-package does not install download and install it

(unless (package-installed-p 'use-package)
  (message "EMACS install use-package.el")
  (package-refresh-contents)
  (package-install 'use-package))

;;; Установили, загрузили, указали, что недостающие пакеты нужно
;;; автоматически загружать и устанавливать.
(require 'use-package)
(setq use-package-always-ensure t)

;;; Указываем откуда брать части настроек.
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory) user-emacs-directory)
        ((boundp 'user-init-directory) user-init-directory)
        (t "~/.emacs.d/")))

;;; Функция для загрузки настроек из указанного файла.
(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

;;; Configuration parts
(load-user-file "neotree.el")
(load-user-file "personal.el")
(load-user-file "themes.el")
(load-user-file "company.el")
(load-user-file "python.el")

;;; А здесь EMACS хранит настройки, задаваемые через customize
(setq custom-file "~/.emacs.d/customize.el")
;;(load-user-file "customize.el")

;;; .emacs ends here
