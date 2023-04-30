;;; early-init.el --- Summary: -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;
;; -- 2022-05-18:
;; I took a lot of inspiration from centaur Emacs.
;; https://github.com/seagle0128/.emacs.d/
;;

;;; Code:

;; Defer garbage collection further back in the startup process.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; 2022-05-18: I don’t know what the "implicitly" means. Centaur does it so
;; let’s try…
(setq frame-inhibit-implied-resize t)
;; (add-to-list 'default-frame-alist '(undecorated . t))

;; Default coding system.
(set-language-environment "UTF-8")

;; Force evil to respect visual line mode. I tried setting it in the `:init'
;; section of `evil' but it did not work.
(setq evil-respect-visual-line-mode t)

;; Faster to disable here.
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq byte-compile-warnings '(cl-functions))

(provide 'early-init)
;;; early-init.el ends here
