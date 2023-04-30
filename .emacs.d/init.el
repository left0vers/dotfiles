;;; init.el --- Summary:  -*- lexical-binding: t no-byte-compile: t -*-
;;;
;;; Commentary:
;;;
;;; Code:

;; Speed up startup.
(setq auto-mode-case-fold nil)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))

;; Prevent the very annoying buffer *Warnings* from popping out.
(setq native-comp-async-report-warnings-errors 'silent)

(push (expand-file-name "modules" user-emacs-directory) load-path)

;; TODO Investigate why I cannot have gpg ask directly from within Emacs for the
;; passkey.
(setq epg-pinentry-mode 'loopback)

(defvar become-evil-p t
  "Enable Evil mode keybindings if set to a non-nil value.

Setting this variable to a non-nil value will have no effect if
the Evil configuration file is not enabled.")

(require 'config-package)
(require 'config-basic)
(require 'config-bépo)
(require 'config-evil)
;; (require 'config-meow)
(require 'config-appearance)
(require 'config-ledger)
(require 'config-org)
(require 'config-markdown)
(require 'config-search)
(require 'config-completion)
(require 'config-embark)
(require 'config-navigation)
(require 'config-git)
(require 'config-shell)
(require 'config-eldoc)
(require 'config-prog)
(require 'config-lsp)
(require 'config-project)
(require 'config-rust)
(require 'config-common-lisp)
(require 'config-javascript)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(wgrep emacsql-sqlite yasnippet yasnippet-snippets eglot sqlite3 flycheck-popup-tip doom-modeline evil-collection evil-commentary forge general magit projectile olivetti yaml-mode which-key vertico use-package undo-tree treemacs tree-sitter-langs sly rustic pcmpl-args page-break-lines org-modern orderless minions marginalia lsp-ui ledger-mode kind-icon iscroll hl-todo gcmh expand-region exec-path-from-shell evil-surround evil-nerd-commenter embark-consult doom-themes diminish diff-hl default-text-scale ctrlf corfu consult-lsp cape auto-package-update all-the-icons-completion)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
