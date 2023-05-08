;;; config-rust.el --- Summary:
;;;
;;; Commentary:
;;;
;;; Code:


(defun config/rust ()
  "Set the `fill-column' to a 100 characters."
  (setq-local fill-column 100))


;; Content to put in the `.dir-locals.el' file at the root of a Rust project
;; 
;; ((rustic-mode
;;   .
;;   ((eglot-workspace-configuration
;;     .
;;     (:rust-analyzer
;;      (:highlightRelated (:references (:enable :json-false))
;;       :check (:command "clippy"))
;;      )))))
;;
;; This tells the rust-analyzer LSP to:
;; - deactivate highlighting of references
;; - use clippy to check (instead of `cargo check')


(use-package rustic
  :config
  (setq
   rustic-compile-directory-method #'rustic-buffer-workspace
   rustic-default-clippy-arguments "--tests --all-targets -- -D warnings"
   rustic-default-test-arguments ""
   rustic-format-trigger 'on-save)
   ;; ///// eglot
   ;; (setq rustic-format-on-save-method #'eglot-format-buffer
   ;;       rustic-lsp-client 'eglot)
   ;; (add-hook 'rust-mode-hook 'eglot-ensure)

   ;; ///// lsp-mode
   (setq rustic-format-on-save-method #'lsp-format-buffer
         lsp-rust-analyzer-server-display-inlay-hints t
         lsp-rust-analyzer-inlay-hints-mode t
         lsp-rust-analyzer-cargo-watch-command "clippy")
   (add-hook 'rust-mode-hook #'lsp-deferred)

   (add-hook 'rust-mode-hook #'config/rust)

  (evil-set-initial-state 'rustic-popup-mode       'emacs)
  (evil-set-initial-state 'rustic-compilation-mode 'emacs)

  (unless (executable-find "cargo-outdated")
    (message "WARNING: rust package `cargo-outdated' was not found.")))


(provide 'config-rust)
;;; config-rust.el ends here
