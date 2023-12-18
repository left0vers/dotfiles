;;; config-tree-sitter.el --- Summary
;;;
;;; Commentary:
;;;
;;; Code:

(use-package tree-sitter
  :diminish
  :defer t)

(use-package tree-sitter-langs
  :ensure t
  :hook (rustic-mode . tree-sitter-hl-mode))

(use-package tree-sitter-indent
  :ensure t)

(use-package treesit-auto
  :ensure t
  :diminish
  :config
  (setq treesit-auto-install 'prompt
        treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(custom-set-faces '(tree-sitter-hl-face:string ((t (:foreground "#C27D65")))))
;; (custom-set-faces '(tree-sitter-hl-face:string ((t (:foreground "#89CFF8")))))
(custom-set-faces '(tree-sitter-hl-face:function.macro ((t (:foreground "#4E82B8")))))
(custom-set-faces '(tree-sitter-hl-face:property ((t (:foreground "#99D5FC")))))
(custom-set-faces '(tree-sitter-hl-face:method.call ((t (:foreground "#99D5FC")))))
(custom-set-faces '(tree-sitter-hl-face:doc ((t (:foreground "#62884E" :weight bold)))))
(custom-set-faces '(tree-sitter-hl-face:attribute ((t (:foreground "#C5C5C5" :slant italic)))))


(provide 'config-tree-sitter)
;;; config-tree-sitter.el ends here
