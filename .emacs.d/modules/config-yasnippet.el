;;; config-yasnippet.el --- Summary
;;;
;;; Commentary:
;;;
;;; Code:

(use-package yasnippet
  :ensure t
  :diminish
  :hook (after-init . yas-global-mode)
  :commands (yas-minor-mode-on
             yas-expand
             yas-expand-snippet
             yas-lookup-snippet
             yas-insert-snippet
             yas-new-snippet
             yas-visit-snippet-file
             yas-activate-extra-mode
             yas-deactivate-extra-mode
             yas-maybe-expand-abbrev-key-filter)
  :config
  (advice-add #'yas-snippet-dirs :filter-return #'delete-dups)

  (general-def
    "M-SPC" 'company-yasnippet)

  (eval-after-load 'smartparens
    (advice-add #'yas-expand :before #'sp-remove-active-pair-overlay)))

(use-package yasnippet-snippets
  :ensure t
  :diminish)

(provide 'config-yasnippet)
;;; config-yasnippet.el ends here
