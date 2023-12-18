;;; config-prog.el --- Summary
;;;
;;; Commentary:
;;;
;;; Code:

(require 'general)
(require 'hydra)
(require 'flymake)

;; (use-package emacs
;;   :config
;;   (setq major-mode-remap-alist '((bash-mode       . bash-ts-mode)
;;                                  (css-mode        . css-ts-mode)
;;                                  (js2-mode        . js-ts-mode)
;;                                  (json-mode       . json-ts-mode)
;;                                  (python-mode     . python-ts-mode)
;;                                  (rust-mode       . rust-ts-mode)
;;                                  (rustic-mode     . rust-ts-mode)
;;                                  (typescript-mode . typescript-ts-mode)
;;                                  (yaml-mode       . yaml-ts-mode))))

(pcase modal-mode
  (:evil (general-def 'normal
           "g r" 'xref-find-references
           "g d" 'xref-find-definitions)))

(use-package flymake
  :diminish
  :hook (prog-mode . flymake-mode)
  :init
  (setq flymake-fringe-indicator-position 'left-fringe)
  :config
  (setq elisp-flymake-byte-compile-load-path
        (append elisp-flymake-byte-compile-load-path load-path))
  (setq flymake-no-changes-timeout 1)

  (defhydra flymake/navigate-diagnostics ()
    "Navigate diagnostics with single key strokes."
    ("C-n" flymake-goto-next-error "next")
    ("C-p" flymake-goto-prev-error "prev"))

  (pcase modal-mode
    (:evil (general-def 'normal
             "M-d"      'flymake/navigate-diagnostics/body
             "SPC d n"  'flymake/navigate-diagnostics/flymake-goto-next-error
             "SPC d p"  'flymake/navigate-diagnostics/flymake-goto-prev-error
             "SPC d s"  'flymake-start
             "SPC d l"  'flymake-show-project-diagnostics))))

;; (use-package flymake-codespell
;;   :ensure t
;;   :hook ((prog-mode . flymake-codespell-setup-backend)))

;; (use-package flycheck
;;   :ensure t
;;   :diminish
;;   :hook (after-init . global-flycheck-mode)
;;   :config

;;   (defhydra flycheck/navigate-diagnostics ()
;;     "Navigate flycheck diagnostics with single key strokes."
;;     ("C-n" flycheck-next-error "next")
;;     ("C-p" flycheck-previous-error "prev"))

;;   (pcase modal-mode
;;     (:evil (general-def 'normal
;;              "M-d"     'flycheck/navigate-diagnostics/body
;;              "SPC d n" 'flycheck/navigate-diagnostics/flycheck-next-error
;;              "SPC d p" 'flycheck/navigate-diagnostics/flycheck-previous-error))))

;; (use-package sideline
;;   :ensure t)

;; (use-package sideline-flymake
;;   :ensure t
;;   :diminish sideline-mode
;;   :hook (flymake-mode . sideline-mode)
;;   :init
;;   (setq sideline-flymake-display-mode 'point
;;         sideline-backends-right '(sideline-flymake)))

;; (use-package sideline-lsp
;;   :ensure t
;;   :init
;;   (add-to-list 'sideline-backends-right 'sideline-lsp))


;; (setq flymake-no-changes-timeout nil
;;       flymake-start-on-save-buffer nil
;;       flymake-compilation-prevents-syntax-check nil)

;; (add-hook 'prog-mode-hook (lambda () (add-hook 'after-save-hook 'flymake-start nil t)))

(use-package show-paren
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(use-package smartparens
  :ensure t
  :diminish
  :hook (after-init . smartparens-global-mode)
  :config
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (setq sp-navigate-skip-match nil
        sp-navigate-consider-sgml-tags nil)
  (setq sp-max-prefix-length 25
        sp-max-pair-length 4)

  ;; -- Stolen from doom emacs:
  ;; Autopair quotes more conservatively; if I'm next to a word/before another
  ;; quote, I don't want to open a new pair or it would unbalance them.
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))

  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET"))
             ;; :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
             ;; Don't autopair opening braces if before a word character or
             ;; other opening brace. The rationale: it interferes with manual
             ;; balancing of braces, and is odd form to have s-exps with no
             ;; whitespace in between, e.g. ()()(). Insert whitespace if
             ;; genuinely want to start a new form in the middle of a word.
             :unless '(sp-point-before-word-p sp-point-before-same-p)))

  (sp-with-modes '(minibuffer-mode)
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "\"" nil :actions nil)
    (sp-local-pair "(" nil :wrap "C-("))

  (sp-with-modes 'emacs-lisp-mode
    ;; disable ', it's the quote character!
    (sp-local-pair "'" nil :actions nil)
    ;; also only use the pseudo-quote inside strings where it
    ;; serves as hyperlink.
    (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))))

(provide 'config-prog)
;;; config-prog.el ends here
