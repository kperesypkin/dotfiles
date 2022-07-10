;;; Package --- Summary
;;; Commentary:
;;; Settings for Python

;;; Code:

(defun annotate-pdb()
  (interactive)
  (highlight-lines-matching-regexp "import ipdb")
  (highlight-lines-matching-regexp "import pdb")
  (highlight-lines-matching-regexp "set_trace()")
  (highlight-regexp "^TODO ")
  (highlight-phrase "FIXME"))

(use-package
  python
  :mode ("\\.py'" . python-mode)
  :init (progn
          (defalias 'python2-mode 'python-mode)
          (defalias 'python3-mode 'python-mod))
  :config (setq-default py-separator-char 47)       ;; Use spaces instead tab
  (setq-default python-indent-offset 4)             ;; 4 spaces instead 2 for python-mode
  )

(use-package
  company-anaconda
  :ensure t
  :init (add-to-list 'company-backends 'company-anaconda))

(use-package
  py-autopep8
  :init (progn (add-hook 'python-mode-hook 'py-autopep8-mode)))   ;;;-enable-on-save)))

(use-package
  anaconda-mode
  :commands anaconda-mode
  :diminish anaconda-mode
  :init (progn (add-hook 'python-mode-hook 'anaconda-mode)
               (add-hook 'python-mode-hook 'eldoc-mode)
               (add-hook 'python-mode-hook 'annotate-pdb)))

(use-package
  pyvenv
  :config (progn
            (defalias 'workon 'pyvenv-workon)
            (defalias 'activate 'pyvenv-activate)
            (defalias 'deactivate 'pyvenv-deactivate)))

(use-package
  py-isort
  :config (setq py-isort-options '("-sl"))
  :init (progn (add-hook 'python-mode-hook 'py-isort-before-save)))


;;; python.el ends here
