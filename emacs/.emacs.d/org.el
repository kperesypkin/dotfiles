;;; Package --- Summary:
;;; Commentary:
;;; Org-mode settings

;;; Code:

;; Tipestamp at DONE status
(setq org-log-done t)

;; Keyboard settings
;; Org globally keys enabled
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)



;;; org.el ends here
