;;; config-eglot.el --- Summary
;;;
;;; Commentary:
;;;
;;; Code:

;; /////////////////////////////////////////////////////////////////////////////
;;
;; Eglot
;;
(use-package eglot
  :ensure t
  :config
  (general-def 'normal eglot-mode-map
    "SPC m l" '(:ignore t :which-key "Language Server")
    "SPC m l r" 'eglot-rename
    "SPC m l a" 'eglot-code-actions)

  (general-def eglot-mode-map
    "C-c ." 'eglot-code-actions)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))


(provide 'config-eglot)
;;; config-eglot.el ends here
