;;; Project --- Summary:
;;; Commentary:
;;; Projectile config file

;;; Code:
(use-package ivy
  :bind
  ;("C-x s" . swiper)
  ("C-x C-r" . ivy-resume)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers nil)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(use-package counsel
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-m" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x c k" . counsel-yank-pop))

(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-project-search-path '("~/develop/"))
  (setq projectile-indexing-method 'alien)
  (setq projectile-completion-system 'ivy)
  (projectile-mode)
  :init
  (projectile-mode +1))



;;; projectile.el ends here
