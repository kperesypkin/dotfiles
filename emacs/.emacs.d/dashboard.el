;;; package --- Summry:
;;; dashboard.el config file
;;; Commentary:
;;; https://github.com/emacs-dashboard/emacs-dashboard
;;; Dashboard - an extensible Emacs startup screen showing you what’s most important
;;; Code:

;; Dependencies
(use-package all-the-icons)
;(use-package page-break-lines)

(use-package dashboard
  :config
  (setq dashboard-items '((recents . 5) (projects . 5) (agenda . 5)))
  (setq dashboard-banner-logo-title "Welcome to Emacs!")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (dashboard-setup-startup-hook)
  )

;;; dashboard.el ends here
