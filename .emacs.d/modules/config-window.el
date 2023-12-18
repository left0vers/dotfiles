;;; config-window.el --- Summary:
;;;
;;; Commentary:
;;;
;;; Code:

(require 'general)

;;
;; `ace-window': quickly navigate to any window.
;;
(use-package ace-window
  :ensure t
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
        aw-background nil
        aw-scope 'frame)

  ;; Make the letters bigger on the screen.
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:foreground "red" :weight bold :height 3.0)))))

  (general-def
    "M-o" 'ace-window))


(provide 'config-window)
;;; config-window.el ends here
