;;; config-git.el --- Summary:
;;;
;;; Commentary:
;;;
;;; Code:

(require 'general)


(use-package magit
  :config
  (setq magit-diff-refine-hunk 'all)
  (pcase modal-mode
    (:evil (general-def 'normal
             "SPC g" '(:ignore t :which-key "[M]agit")
             "SPC g s" 'magit-status
             "SPC g b" 'magit-blame-echo))))

;;
;; `diff-hl': show in the gutter which parts of a file have been modified.
;;
(use-package diff-hl
  :ensure t
  :hook (after-init . global-diff-hl-mode)
  :config
  (setq diff-hl-draw-borders nil))

;;
;; Forge: how to access the GitHub features through Magit.
;;
(setq auth-sources '("~/.authinfo"))

(use-package forge
  :ensure t
  :after magit)


(provide 'config-git)
;;; config-git.el ends here
