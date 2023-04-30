;;; config-navigation.el --- Summary:
;;;
;;; Commentary:
;;;
;;; Code:

(require 'general)


;;
;; `avy': quickly navigate to any place in the buffer.
;;
(use-package avy)
(require 'avy)
(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "C-'") 'avy-goto-char-timer)


;;
;; `ace-window': quickly navigate to any window.
;;
(use-package ace-window
  :init
  ;; NOTE: This has to go before the corresponding `require' statement!
  (defvar aw-dispatch-alist
    '((?d aw-delete-window "Delete window")
      (?S aw-swap-window "Swap Windows")
      ;; (?m aw-move-window "Move Window")
      ;; (?C aw-copy-window "Copy Window")
      ;; (?j aw-switch-buffer-in-window "Select Buffer")
      ;; (?n aw-flip-window)
      ;; (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
      ;; (?c aw-split-window-fair "Split Fair Window")
      (?h aw-split-window-vert "Split Window Vertically")
      (?v aw-split-window-horz "Split Window Horizontally")
      (?D delete-other-windows "Delete Other Windows"))
    ;; (?? aw-show-dispatch-help))
    "List of actions for `aw-dispatch-default'.")
  :config
  (setq aw-keys '(?a ?u ?i ?e ?t ?r ?n)
        aw-dispatch-always t
        aw-scope 'global)
  (general-def
    "M-o" 'ace-window))


;;
;; "Smartly" expand the region
;;
(use-package expand-region
  :config
  (if become-evil-p
      (general-def '(visual)
        "+" 'er/expand-region
        "-" 'er/contract-region)
    (global-set-key (kbd "C-=") 'er/expand-region)))

(provide 'config-navigation)
;;; config-navigation.el ends here
