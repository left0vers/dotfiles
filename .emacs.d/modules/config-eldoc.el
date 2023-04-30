;;; config-eldoc.el --- Summary
;;;
;;; Commentary:
;;;
;;; Code:

;; ElDoc is allowed to put maximum 5 lines in the echo area
(setq eldoc-echo-area-use-multiline-p 5
      eldoc-echo-area-display-truncation-message nil
      eldoc-echo-area-prefer-doc-buffer t)

(setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
;; (add-hook 'after-init-hook 'global-eldoc-mode)

(provide 'config-eldoc)
;;; config-eldoc.el ends here
