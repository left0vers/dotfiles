;;; config-completion.el --- Summary:
;;;
;;; Commentary:
;;;
;;; Code:

(require 'general)

;; Ignore case when completing.
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;;
;; Orderless: completion style that divides the pattern into space-separated
;; components, and matches candidates that match all of the components in any
;; order.
;;
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))


;;
;; Vertico: minibuffer completion overlay.
;;
(use-package vertico
  :config
  (setq vertico-cycle t)
  (vertico-mode)

  ;; Extension: vertico-directory. Such that when I press "M-DEL" I either go up
  ;; one directory or I remove part of the file name (depending if the last part
  ;; of the path is a subdirectory or a file name).
  ;; (straight-use-package
  ;;  '(vertico-directory :type nil :local-repo "vertico/extensions"))
  (require 'vertico-directory)
  (general-def vertico-map
    "M-DEL" 'vertico-directory-delete-word))


;;
;; Marginalia: helpful colorful annotations placed at the margin of the
;; minibuffer for your completion candidates.
;;
(use-package marginalia
  :config
  (marginalia-mode))


;;
;; Consult: practical commands based on the Emacs completion function
;; completing-read.
;; 
(use-package consult
  :config
  (setq consult-preview-key "M-.")

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (pcase modal-mode
    (:evil (general-def 'normal
             "SPC b b" 'consult-buffer
             "SPC b p" 'consult-project-buffer
             "SPC /"   'consult-ripgrep))
    (:god-mode (progn
                 (global-set-key (kbd "C-x C-b") 'consult-buffer)
                 (global-set-key (kbd "C-x C-/") 'consult-ripgrep)))))


;;
;; Corfu: enhances completion at point with a small completion popup.
;;
(use-package corfu
  :config
  (setq corfu-cycle t
        ;; Only use `corfu' when calling `completion-at-point' or
        ;; `indent-for-tab-command'?
        corfu-auto t
        corfu-min-width 20
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match t
        ;; corfu-separator ?\s
        corfu-preselect 'valid)

  ;; - if I press RET, a newline is inserted,
  ;; - if I press TAB and Corfu is active, complete.
  (setq tab-always-indent 'complete
        ;; Always cycle no matter the number of candidates for completion.
        completion-cycle-threshold t)
  (general-def corfu-map
    "<tab>"      'corfu-complete
    "<return>"   'newline
    "C-<return>" 'corfu-insert)

  (general-def
    "M-TAB" 'completion-at-point)

  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode)))

  (global-corfu-mode)

  ;; (advice-add 'evil-normal-state :after 'corfu-quit)

  ;; Sort the candidates based on the number of times they are used.
  ;; (straight-use-package '(corfu-history :type nil :local-repo "corfu/extensions"))
  (require 'savehist)
  (require 'corfu-history)
  (corfu-history-mode)
  (add-to-list 'savehist-additional-variables 'corfu-history)

  (require 'corfu-popupinfo)
  (add-hook 'corfu-mode-hook #'corfu-popupinfo-mode)
  (setq corfu-popupinfo-delay '(1.0 . 1.0)))


;; `cape': Completion At Point Extensions.
(use-package cape
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)

  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))


;;
;; Enhance shell completion.
;;
(use-package pcmpl-args
  :ensure t)


;; `svg-lib' and `kind-icon' add icons to the completion pop-up.
(use-package svg-lib)

(use-package kind-icon
  :config
  (setq kind-icon-default-face 'corfu-default
        kind-icon-use-icons t
        kind-icon-blend-background nil
        kind-icon-blend-frac 0.08)

  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(provide 'config-completion)
;;; config-completion.el ends here
