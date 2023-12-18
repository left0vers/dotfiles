;;; config-basic.el --- Summary
;;;
;;; Commentary:
;;;
;;; Code:

;; Increase how much is read from processes in a single chunk. Default is 4kb.
(setq read-process-output-max (* 1024 1024))  ; 1mb --- lsp-mode recommendation


;; @left0vers: to investigate
;; ;; EasyPG
;; (require 'epg)
;; (setq epg-pinentry-mode 'loopback)

;; Mac OS specifics.
(if (eq system-type 'darwin)
    (progn
      (setq mac-right-command-modifier 'none)
      ;; (setq mac-left-command-modifier  'super)
      (setq mac-right-option-modifier  'none)))


;; Encoding defaults to utf-8.
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

(prefer-coding-system 'utf-8)


;; Make sure that we get the shell variables as well.
(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))


(require 'recentf)
(setq recentf-max-saved-items 300
      recentf-exclude
      '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
        "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
        "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
        "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
        (lambda (file) (file-in-directory-p file package-user-dir))))
(add-hook 'after-init-hook 'recentf-mode)
(push (expand-file-name recentf-save-file) recentf-exclude)
(add-to-list 'recentf-filename-handlers #'abbreviate-file-name)

;; Save minibuffer history.
;; This minor mode is also used in conjunction with the `Corfu' package to
;; remember the completions used.
(savehist-mode)


(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))


(setq-default major-mode 'fundamental-mode
              fill-column 80
              require-final-newline t
              ;; The trailing white space should only be shown for specific
              ;; modes so by default, turn it off.
              show-trailing-whitespace nil
              tab-width 4
              ;; Only indent with spaces.
              indent-tabs-mode nil)

(add-hook 'before-save-hook #'delete-trailing-whitespace)


(setq inhibit-compacting-font-caches t
      delete-by-moving-to-trash t
      make-backup-files nil
      auto-save-default nil

      uniquify-buffer-name-style 'post-forward-angle-brackets
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|…\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil
      ;; ?? Also wrap after characters of a certain category.
      ;; What does "certain category" mean?
      word-wrap-by-category t)

;;
;; Keybindings
;;
(global-set-key (kbd "C-x C-b") 'ibuffer)


;;
;; Dired
;;
(require 'dired)
(setq dired-kill-when-opening-new-dired-buffer t)


;;
;; Ediff
;;
;; The below configuration is taken from this video of Prot'
;; https://www.youtube.com/watch?v=pSvsAutseO0
(require 'ediff)
(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)


;;
;; Less minor modes on the modeline.
;;
(use-package diminish
  :ensure t)


;;
;; Which-key: displays the key bindings following your currently entered
;; incomplete command (a prefix) in a popup.
;;
(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode))


;;
;; A `general' is a good leader…!
;;
(use-package general
  :ensure t)


;;
;; Enter a special "hydra" state when using specific, predefined keybindings.
;;
(use-package hydra
  :ensure t)


(provide 'config-basic)
;;; config-basic.el ends here
