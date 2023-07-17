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
(setq custom-file "~/.emacs.d/custom.el")

(defvar modal-mode :evil
  "The modal mode that is active.

Possible values are `:evil', `:god-mode', `:meow'.")

(defvar lsp-client :lsp-mode
  "The LSP client implementation to use.

Possible values are `:eglot' and `:lsp-mode'")

(require 'config-package)
(require 'config-basic)
(require 'config-bépo)
(pcase modal-mode
  (:god-mode (require 'config-god-mode))
  (:evil (require 'config-evil))
  (:meow (require 'config-meow)))
(require 'config-appearance)
(require 'config-org)
(require 'config-spelling)
(require 'config-markdown)
(require 'config-search)
(require 'config-completion)
(require 'config-embark)
(require 'config-navigation)
(require 'config-git)
(require 'config-shell)
(require 'config-eldoc)
(require 'config-prog)
(pcase lsp-client
  (:lsp-mode (require 'config-lsp-mode))
  (:eglot (require 'config-eglot)))
(require 'config-project)
(require 'config-rust)
(require 'config-python)
(require 'config-common-lisp)
(require 'config-javascript)

;;; init.el ends here
