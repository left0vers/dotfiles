;;; config-prog.el --- Summary
;;;
;;; Commentary:
;;;
;;; Code:

(require 'general)
;; (require 'doom-themes)
;; (require 'hydra)


(show-paren-mode    t)
(electric-pair-mode t)

(pcase modal-mode
  (:evil (general-def 'normal
           "g r" 'xref-find-references
           "g d" 'xref-find-definitions)))


;; (require 'flyspell)
;; (setenv "LANG" "en_US")
;; (setq ispell-program-name "hunspell")
;; (setq ispell-local-dictionary "en_US")
;; (setq ispell-hunspell-dict-paths-alist
;;       '(("en_US" "/Users/julien/Library/Dictionaries/en_US.aff")
;;         ("fr"    "/Users/julien/Library/Dictionaries/fr.aff")))
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)


(require 'flymake)

(defhydra flymake/navigate-diagnostics ()
  "Navigate flymake diagnostics with single key strokes."
  ("n" flymake-goto-next-error "next")
  ("p" flymake-goto-prev-error "prev"))

(pcase modal-mode
  (:evil (general-def 'normal
           "M-d"      'flymake/navigate-diagnostics/body 
           "SPC d n"  'flymake/navigate-diagnostics/flymake-goto-next-error
           "SPC d p"  'flymake/navigate-diagnostics/flymake-goto-prev-error
           "SPC d s"  'flymake-start
           "SPC d l"  'flymake-show-project-diagnostics)))


(setq flymake-no-changes-timeout nil
      flymake-start-on-save-buffer nil
      flymake-compilation-prevents-syntax-check nil)

(add-hook 'prog-mode-hook (lambda () (add-hook 'after-save-hook 'flymake-start nil t)))

;; (use-package flymake-diagnostic-at-point
;;   :config
;;   (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

;;;
;;; `flycheck': syntax checker.
;;;
;; (use-package flycheck
;;   :preface

;;   ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
;;   ;;
;;   ;; Show the error / warning / info messages produced by Flycheck in the echo
;;   ;; area alongside the Eldoc documentation.
;;   (defun my/flycheck-eldoc (callback &rest _ignored)
;;     "Print flycheck messages at point by calling CALLBACK."
;;     (when-let ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
;;       (mapc
;;        (lambda (err)
;;          (funcall callback
;;                   (format "%s: %s"
;;                           (let ((level (flycheck-error-level err)))
;;                             (pcase level
;;                               ('info (propertize "I" 'face 'flycheck-error-list-info))
;;                               ('error (propertize "E" 'face 'flycheck-error-list-error))
;;                               ('warning (propertize "W" 'face 'flycheck-error-list-warning))
;;                               (_ level)))
;;                           (flycheck-error-message err))
;;                   :thing (or (flycheck-error-id err)
;;                              (flycheck-error-group err))
;;                   :face 'font-lock-doc-face))
;;        flycheck-errors)))

;;   (defun my/flycheck-prefer-eldoc ()
;;     "Tell Flycheck to use eldoc."
;;     (add-hook 'eldoc-documentation-functions #'my/flycheck-eldoc nil t)
;;     (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly
;;           flycheck-display-errors-function nil
;;           flycheck-help-echo-function nil))
  
;;   :config
;;   (setq flycheck-check-syntax-automatically '(mode-enabled save))
;;   ;; (add-hook 'after-init-hook 'global-flycheck-mode)
;;   (general-def '(normal)
;;     "SPC f n" 'flycheck-next-error
;;     "SPC f p" 'flycheck-previous-error)

;;   :hook ((after-init . global-flycheck-mode)
;;          (flycheck-mode . my/flycheck-prefer-eldoc)))


;;
;; `tree-sitter'
;;
(use-package tree-sitter)
(use-package tree-sitter-langs
  :after tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


;;
;; treemacs
;;
(use-package treemacs
  :config
  (pcase modal-mode
    (:evil (general-def 'normal
             "SPC t" '(:ignore t :which-key "[T]reemacs")
             "SPC t t" 'treemacs-select-window
             "SPC t q" 'treemacs-narrow-to-current-file))))

  ;; (with-eval-after-load 'treemacs
  ;;   (doom-themes-treemacs-config))

  ;; (setq doom-themes-treemacs-theme "doom-colors"
  ;;       treemacs-project-follow-mode t
  ;;       treemacs-indent-guide-mode t))


(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(provide 'config-prog)
;;; config-prog.el ends here
