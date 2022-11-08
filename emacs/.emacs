;;; package --- Summary
;;; Commentary:
;;; Main EMACS config file, load settings from parts

;;; Code:

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

;; Removes delay while loading custom font
(modify-frame-parameters nil '((wait-for-wm . nil)))

;; Main section
;;; Fonts settings
(set-face-attribute 'default nil :height 140)
(when (member "Hack" (font-family-list))
  (set-face-attribute 'default nil :font "Hack")) ;Monospace

;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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
  "Load user configuration FILE."
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

;;; Части конфигурации. Порядок не имеет принципиального значения,
;;; однако я рекомендую некоторые базовые вещи помещать в начало,
;;; чтобы не было необходимости вспоминать базовые команды EMACS
;;; если в результате улучшения сломается один из базовых конфигов.
(load-user-file "personal.el")
(load-user-file "dashboard.el")
(load-user-file "neotree.el")
(load-user-file "themes.el")
(load-user-file "company.el")
(load-user-file "python.el")
(load-user-file "flycheck.el")
(load-user-file "haskell.el")
(load-user-file "org.el")
(load-user-file "projectile.el")
(load-user-file "modeline.el")


;;; А здесь EMACS хранит настройки, задаваемые через customize
(setq custom-file "~/.emacs.d/customize.el")
;;(load-user-file "customize.el")

;;; .emacs ends here
