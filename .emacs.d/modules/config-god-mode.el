;;; config-god-mode.el --- Summary:
;;;
;;; Commentary:
;;;
;;; Code:

(use-package god-mode
  :ensure t
  :config
  (define-key god-local-mode-map (kbd "i") #'god-local-mode)
  (global-set-key (kbd "<escape>") #'god-local-mode)
  (god-mode))


(provide 'config-god-mode)
;;; config-god-mode.el ends here
