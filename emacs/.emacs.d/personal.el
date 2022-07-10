;;; Package --- Summary
;;; Commentary:
;;; User settings for .emacs

;;; Code:

;;; Закрывать *scratch* при запуске.
(kill-buffer "*scratch*")

;;; Emacs appearance
(setq display-time-day-and-date t
      display-time-24hr-format t
      display-time-interval 10
      display-time-default-load-average nil)
(display-time)

;;; Display battery charge in percent
;(display-battery-mode 1)

;;; Цветные скобочки
(use-package
  rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
        (setq rainbow-delimiters-max-face-count 9))

;;; Scrolling
(setq scroll-step               1) ;; one line
(setq scroll-margin            10) ;; scroll buffer to 10 lines at going to last line
(setq scroll-conservatively 10000)
(setq directory-free-space-args "-Pm")

;; Подсветка результатов поиска и всё такое
(setq search-highlight        t)
(setq query-replace-highlight t)
(setq use-dialog-box nil)   ;; Не нужны нам диалоги, будем всё руками делать
(auto-fill-mode -1)         ;; Не знаю, что за параметр, так и не разобрался
(setq-default tab-width          4) ;; Заменить TAB на 4 пробела
(setq-default standart-indent    4) ;; Стандартный отступ - 4 пробела
(setq backup-inhibited t)           ;; Backup'ы тоже делать не будем
(setq auto-save-default nil)        ;; Автосохранение не нужно
(setq scroll-preserve-screen-position 10) ;; Показывать не менее 10 строк на экране
(setq-default c-basic-offset 4 c-indent-level 4 indent-tabs-mode nil) ;; TAB'ы не нужны
(setq-default save-place t) ;; Помнить, где был курсор в прошлый раз
(setq ring-bell-function 'ignore) ;; Switch off the bell

;;; Нажатие Insert больше не включает режим замены
(define-key global-map [(insert)] nil)

;;; Автоформатирование перед сохранением
(defun format-current-buffer()
  (indent-region (point-min)
                 (point-max)))
(defun untabify-current-buffer()
  (if (not indent-tabs-mode)
      (untabify (point-min)
                (point-max)))
  nil)
(add-to-list 'write-file-functions 'untabify-current-buffer)
(add-to-list 'write-file-functions 'delete-trailing-whitespace)

;;(cua-mode 1)          ;;; Ctrl+C, Ctrl+V! Прямо как в Windows!
(desktop-save-mode -1) ;;; Помнить, какие файлы были открыты в прошлый раз
(fset 'yes-or-no-p 'y-or-n-p) ;;; Вместо yes и no понимать y и n
(global-hl-line-mode 1) ;;; Подсветка текущей строки
(global-linum-mode 1)   ;;;Номера строк
;; Пробелы после номеров строк
(if window-system
    (progn)
  (setq linum-format "%4d \u2502 "))

(menu-bar-mode -1)      ;;; А меню - никогда
(scroll-bar-mode 1)    ;;; Скроллбар нужен
(tool-bar-mode -1)      ;;; Тулбар не нужен

;;; Умные скобочки
(use-package
  smartparens
  :config (smartparens-global-mode 1))

;; Electric pair mode
(electric-pair-mode 1)

;; Показывать отступы во всех режимах
(use-package
  indent-guide
  :config (indent-guide-global-mode 1))

;;; Дерево отмены
(use-package
  undo-tree
  :config (global-undo-tree-mode 1))

;; Папка для сохранения файлов дерева отмены
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;;; personal.el ends here
