;; Устанавливаем flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)
