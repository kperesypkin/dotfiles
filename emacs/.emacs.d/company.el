;;; Package --- Summary
;;; Commentary:
;;; Settings for company

;;; Code:

(use-package  company
  :diminish company-mode
  :config (setq company-backends (remove 'company-ropemacs company-backends) company-tooltip-limit
                20 company-tooltip-align-annotations t)
  (add-hook 'after-init-hook 'global-company-mode))

;;; company.el ends here
