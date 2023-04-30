;;; config-search.el --- Summary:
;;;
;;; Commentary:
;;;
;;; Code:

(use-package ctrlf
  :config
  (setq ctrlf-default-search-style 'fuzzy)
  (ctrlf-mode)

  (if become-evil-p
      (general-def '(normal)
        "?" 'ctrlf-backward-default
        "/" 'ctrlf-forward-default
        "C-f" 'ctrlf-forward-default))

  (general-def 'ctrlf-minibuffer-mode-map
    "C-s" 'ctrlf-forward-default))

(provide 'config-search)
;;; config-search.el ends here
