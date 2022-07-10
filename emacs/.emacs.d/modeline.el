;;; Package --- Summary:
;;; Commentary:
;;; Modeline config file

;;; Code:

(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-support-imenu t)
  (setq doom-modeline-height 5)
  (setq doom-modeline-bar-width 4)
  (setq doom-modeline-hud nil)
  (setq doom-modeline-window-width-limit 85)
  (setq doom-modeline-icon t)
  (setq doom-modeline-buffer-encoding t)
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-env-enable-go t)
  (setq doom-modeline-env-python-executable "python")
  (setq doom-modeline-env-go-executable "go")
  :init (doom-modeline-mode 1))


;;; modeline.el ends here
