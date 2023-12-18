;;; config-yaml --- Summary
;;;
;;; Commentary:
;;;
;;; Code:

(require 'tree-sitter-hl)

(use-package yaml-mode
  :ensure t
  :hook ((yaml-mode . tree-sitter-hl-mode)
         (yaml-mode . eglot-ensure))
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'"  . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))


(provide 'config-yaml)
;;; config-yaml.el ends here
