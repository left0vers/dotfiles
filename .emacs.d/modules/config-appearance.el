;;; config-appearance.el --- Summary:
;;;
;;; Commentary:
;;;
;;; Code:

;; Stop the cursor from blinking.
(blink-cursor-mode -1)
;; Highlight the current line, everywhere.
(global-hl-line-mode)
(global-display-fill-column-indicator-mode)

;; TODO Automatically change the size of the font the moment the size of the
;; monitor changes.
;; These could probably be an inspiration:
;; - https://www.reddit.com/r/emacs/comments/dpc2aj/readjusting_fontsize_according_to_monitor/
;; - https://coderwall.com/p/ifgyag/change-font-size-in-emacs-dynamically-based-on-screen-resolution
(defvar my/keep-size t
  "Whether to keep the size of the frames or not.")
(defvar my/apply-to-all-frames t
  "Whether to apply the font to all the frames (futures & current).")
(defvar my/font (font-spec
                 :name "JetBrains Mono NL"
                 :weight 'light
                 :size 16))
(set-frame-font my/font my/keep-size my/apply-to-all-frames)

(when (daemonp)
  (setq default-frame-alist '((font . my/font))))


;; Make the titlebar transparent & dark.
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Optimizations.
(setq idle-update-delay 5.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)


;; Treat all the themes as safe.
(setq custom-safe-themes t)

(load-theme 'modus-vivendi)

;; Instead of showing a triangle in the middle of the screen to indicate that an
;; action was impossible, invert the color of the modeline.
;;
;; https://www.emacswiki.org/emacs/AlarmBell#h5o-3
(setq visible-bell nil
      ring-bell-function 'ignore)

;; (use-package doom-themes
;;   :config
;;   (doom-themes-visual-bell-config)
;;   (load-theme 'doom-one))

(use-package doom-modeline
  :init
  (setq doom-modeline-minor-modes t
        doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-modal-icon nil)
  (unless after-init-time
    (setq-default mode-line-format nil))
  :config
  ;; Apparently there is a performance penalty when using this feature.
  ;; (setq auto-revert-check-vc-info t)
  (add-hook 'after-init-hook 'doom-modeline-mode))


(use-package minions)
(require 'minions)
(add-hook 'doom-modeline-mode-hook 'minions-mode)


(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'yaml-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-width-start t)
(column-number-mode)


;; Suppress some GUI features.
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-scratch-message nil)

(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))


;; Display dividers between windows.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)


;; Easily adjust the font size in all frames.
(use-package default-text-scale)
(require 'default-text-scale)
(add-hook 'after-init-hook 'default-text-scale-mode)

(pcase modal-mode
  (:evil (general-def '(normal visual motion emacs insert)
           "C-+" 'default-text-scale-increase
           "C--" 'default-text-scale-decrease)))


;; Mouse & Smooth scrolling.
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))

(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000
      auto-window-vscroll nil
      scroll-preserve-screen-position t)

(if (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t))


(use-package iscroll)
(require 'iscroll)
(add-hook 'image-mode-hook 'iscroll-mode)


;; Display page breaks as tidy horizontal lines
(use-package page-break-lines)
(require 'page-break-lines)
(add-hook 'after-init-hook 'global-page-break-lines-mode)


;;
;; `hl-todo': highlight TODO / FIXME in the buffer.
;;
(use-package hl-todo)
(require 'hl-todo)
(add-hook 'after-init-hook 'global-hl-todo-mode)


;;
;; Dashboard
;;
(use-package dashboard
  :config
  (setq dashboard-item-shortcuts '((recents . "f")
                                   (bookmarks . "m")
                                   (projects . "P")
                                   (agenda . "a")
                                   (registers . "e")))
  (general-def 'dashboard-mode-map
    "n" 'next-line
    "p" 'previous-line)
  (dashboard-setup-startup-hook))


(provide 'config-appearance)
;;; config-appearance.el ends here
