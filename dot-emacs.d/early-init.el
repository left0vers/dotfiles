;;; early-init.el --- Summary: -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; packages and UI initialization.
;;
;; -- 2022-05-18:
;; I took a lot of inspiration from centaur Emacs.
;; https://github.com/seagle0128/.emacs.d/
;;
;; -- 2023-09-22:
;; https://git.sr.ht/~ashton314/emacs-bedrock/tree/main/item/early-init.el
;; Another source of inspiration.
;;

;;; Code:

;; Speed up startup.
(setq auto-mode-case-fold nil
      gc-cons-threshold 10000000
      ;; @left0vers: this was the previous setting.
      ;; byte-compile-warnings '(cl-functions)
      byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp))
      ;; Prevent the very annoying buffer *Warnings* from popping out.
      native-comp-async-report-warnings-errors 'silent
      inhibit-startup-echo-area-message (user-login-name))

;; 2022-05-18: I don't know what the "implicitly" means. Centaur does it so
;; let's try…
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)
;; (add-to-list 'default-frame-alist '(undecorated . t))

;; Default coding system.
(set-language-environment "UTF-8")

;; Faster to disable here.
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)


(provide 'early-init)
;;; early-init.el ends here
