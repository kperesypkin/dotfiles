;;; neotree.el config file
;;; Comment:
;;; NeoTree - sidebar for file browsing

(use-package
  neotree
  :bind ([f8] . neotree-toggle)
  :init (setq neo-window-width 35)
  :config (setq neo-smart-open t)
  )

;;; neotree.el ends here
