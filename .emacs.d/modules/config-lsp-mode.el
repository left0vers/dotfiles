;;; config-lsp-mode.el --- Summary
;;;
;;; Commentary:
;;;
;;; Code:

;; /////////////////////////////////////////////////////////////////////////////
;;
;; `lsp-mode': Language Server Protocol.
;;

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-completion-enable t
        lsp-signature-auto-activate nil
        lsp-eldoc-enable-hover t
        lsp-completion-provider :none
        lsp-diagnostics-provider :flymake
        lsp-enable-snippet t
        lsp-headerline-breadcrumb-enable nil
        lsp-enable-symbol-highlighting nil)

  (defun my/lsp-mode-setup-completion ()
    "Change LSP completion styles."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  (add-hook 'lsp-completion-mode-hook 'my/lsp-mode-setup-completion)
  (add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)

  (define-key lsp-mode-map (kbd "C-c .") 'lsp-execute-code-action)
  
  (pcase modal-mode
    (:evil (general-def 'normal
             "SPC m l" '(:ignore t :which-key "LSP")
             "SPC m l a" 'lsp-execute-code-action
             "SPC m l r" 'lsp-rename))))

(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-hover nil
        lsp-ui-doc-enable nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-use-webkit nil
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-show-with-mouse nil)

  (pcase modal-mode
    (:evil (general-def 'normal
             "g h" 'lsp-ui-doc-toggle
             "g r" 'lsp-find-references
             "g d" 'lsp-find-definition
             "g t" 'lsp-find-type-definition)))
  
  (define-key lsp-ui-mode-map (kbd "M-i") 'lsp-ui-imenu)
  
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package consult-lsp
  :config
  (general-def 'normal
    "SPC l s" 'consult-lsp-symbols
    "SPC d l" 'consult-lsp-diagnostics))


(provide 'config-lsp-mode)
;;; config-lsp-mode.el ends here
