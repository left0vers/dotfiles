;;; config-completion.el --- Summary:
;;;
;;; Commentary:
;;;
;;; Code:

(require 'general)
(require 'xref)

;;
;; Consult: search and navigation commands based on the completion function
;; backed in Emacs, `completing-read'.
;;
(use-package consult
  :ensure t
  :bind (("C-x b"  . consult-buffer)
         ;; ("M-i" . consult-imenu) ;; replaced by 'symbols-outline
         ("C-s"    . consult-line)
         ("M-y"    . consult-yank-from-kill-ring))
  :config
  (setq xref-show-xrefs-function       #'consult-xref
        xref-show-definitions-function #'consult-xref
        ;; TODO Investigate the narrowing key.
        ;; consult-narrow-key "«"
        ;; By default, the variable is set to `any' which means that any key
        ;; will trigger the preview (i.e. navigating to the previous/next line
        ;; will preview the file at point).
        ;;
        ;; Setting it to a specific key deactivate this behaviour.
        consult-preview-key "M-."))

;; NOTE Declaring these keybindings inside the `:config' section will not make
;; them active until `consult' is first loaded.
(pcase modal-mode
  (:evil (general-def 'normal
           "SPC b b"  'consult-buffer
           "SPC b p"  'consult-project-buffer
           "SPC / g"  'consult-git-grep
           "SPC / s"  'consult-ripgrep)))

(use-package eshell
  :bind (("C-r" . consult-history)))


;;
;; Vertico
;;
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-cycle t))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

(use-package nerd-icons-completion
  :if (display-graphic-p)
  :after vertico
  :ensure t
  :hook (vertico-mode . nerd-icons-completion-mode))

;;
;; Marginalia
;;
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))


;;
;; Company
;;
(use-package company
  :ensure t
  :diminish
  :commands (company-complete-common
             company-complete-common-or-cycle
             company-manual-begin
             company-grab-line)
  :hook (after-init . global-company-mode)
  :init
  (setq company-tooltip-limit 10
        company-tooltip-minimum 10
        company-tooltip-flip-when-above t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes
        '(not erc-mode
              circe-mode
              message-mode
              help-mode
              gud-mode
              vterm-mode)

        ;; Buffer-local backends will be computed when loading a major mode,
        ;; only global default here.
        company-backends '((company-capf company-dabbrev))

        company-selection-wrap-around t
        company-insertion-on-trigger nil

        ;; Only search the current buffer for dabbrev.
        company-dabbrev-other-buffers nil
        ;; Make dabbrev full case-sensitive.
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)

  (if (display-graphic-p)
      (setq company-frontends
            '(company-pseudo-tooltip-frontend ; always show candidate in overlay
              company-echo-metadata-frontend)) ; show selected candidate doc
    (setq company-frontends '(company-pseudo-tooltip-frontend)))

  :config
  (pcase modal-mode
    (:evil (progn
            ;; (require 'evil-core)
            ;; (add-hook 'company-mode-hook #'evil-normalize-keymaps)
            (general-def 'insert
              "C-SPC" 'company-manual-begin))))

  (general-def 'eshell-mode-map
    "<tab>" 'company-manual-begin)

  (general-def 'company-active-map
    "<tab>"      'company-complete
    "C-<return>" 'newline-and-indent
    "M-s"        'company-filter-candidates
    "M-«"        'company-select-first
    "M-»"        'company-select-last
    "C-w"        'evil-delete-backward-word
    "<return>"   'company-complete-selection))


;;
;; Company-box
;;
(use-package company-box
  :if (display-graphic-p)
  :diminish
  :ensure t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-doc-delay 1
        company-box-icons-alist 'company-box-icons-images))


(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion))))

  ;; We follow a suggestion by company maintainer u/hvis:
  ;; https://www.reddit.com/r/emacs/comments/nichkl/comment/gz1jr3s/
  (defun company-completion-styles (capf-fn &rest args)
    (let ((completion-styles '(basic partial-completion)))
      (apply capf-fn args)))

  (advice-add 'company-capf :around #'company-completion-styles))

(provide 'config-completion)
;;; config-completion.el ends here
