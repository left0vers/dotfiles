;;; config-python.el --- Summary:
;;;
;;; Commentary:
;;;
;;; Code:

(defun config/python ()
  "Set `fill-column' to 80 characters."
  (setq-local fill-column 80))


(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (config/python)
                          (lsp-deferred))))

(provide 'config-python)
;;; config-python.el ends here
