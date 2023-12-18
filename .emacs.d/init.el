;;; init.el --- Summary:  -*- lexical-binding: t no-byte-compile: t -*-
;;;
;;; Commentary:
;;;
;;; Code:

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(push (expand-file-name "modules" user-emacs-directory) load-path)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))

(setq custom-file "~/.emacs.d/custom.el")
(setq epg-pinentry-mode 'loopback)

;; I truly appreciate modal editing, even though I think it is not enough and
;; that a combination of it + Emacs keybindings is what's most efficient.
;;
;; This variable allows to switch between modal-modes easily.
(defvar modal-mode :evil
  "The modal mode that is active.

Possible values are `:evil', `:god-mode', `:meow'.")

;; Nowadays, to have a good IDE the use of an LSP implementation becomes
;; mandatory.
;; There are two strong candidates in the Emacs ecosystem: `eglot' and
;; `lsp-mode'.
(defvar lsp-client :eglot
  "The LSP client implementation to use.

Possible values are `:eglot' and `:lsp-mode'")

;; This configuration file tweaks Emacs itself:
;; -
;;
;; It also installs the following packages that I consider "basic":
;; - general
;; - which-key
;; - hydra
(require 'config-basic)

;; Remapping of a few keys (for keybindings) that are easier to reach on a BÉPO
;; layout:
;; - « » to < >
;; - ( ) to { }
(require 'config-bépo)

(pcase modal-mode
  (:god-mode (require 'config-god-mode))
  (:evil     (require 'config-evil))
  (:meow     (require 'config-meow)))

;;
(require 'config-appearance)

;; - consult
;; - vertico
;; - marginalia
;; - company
;; - company-box
;; - cape
;; - prescient
(require 'config-completion)

;; - jinx
(require 'config-spelling)

;; - ace-window
(require 'config-window)

;;
(require 'config-org)

;; - flymake
;; - sideline-flymake
(require 'config-prog)

;; - projectile
(require 'config-project)

;; - magit
;; - forge
;; - diff-hl
(require 'config-git)

;; - tree-sitter-langs
(require 'config-tree-sitter)

(pcase lsp-client
  (:lsp-mode (require 'config-lsp-mode))
  (:eglot (require 'config-eglot)))

;; - yasnippet
;; - yasnippet-snippets
(require 'config-yasnippet)

;; - rustic
(require 'config-rust)
(require 'config-yaml)
;; (require 'config-markdown)
;; (require 'config-search)
;; (require 'config-embark)
;; (require 'config-navigation)

(require 'config-shell)

;; (require 'config-eldoc)
;; (require 'config-python)
;; (require 'config-common-lisp)
;; (require 'config-javascript)

;;; init.el ends here
