;;; config-org.el --- Summary:
;;;
;;; Commentary:
;;;
;;; Code:

(require 'general)
(require 'org)

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "|" "DONE")
        (sequence "DELEGATED" "TO-CHECK" "|" "VALIDATED")
        (sequence "|" "MEETING")
        (sequence "|" "CANCELLED")))

(setq org-hide-emphasis-markers t
      org-fontify-quote-and-verse-blocks t
      org-fontify-done-headline nil)

;; hide #+TITLE:
(setq org-hidden-keywords '(title))
;; set basic title font
(set-face-attribute 'org-level-8 nil :weight 'bold :inherit 'default)
;; Low levels are unimportant => no scaling
(set-face-attribute 'org-level-7 nil :inherit 'org-level-8)
(set-face-attribute 'org-level-6 nil :inherit 'org-level-8)
(set-face-attribute 'org-level-5 nil :inherit 'org-level-8)
(set-face-attribute 'org-level-4 nil :inherit 'org-level-8)
(set-face-attribute 'org-level-3 nil :inherit 'org-level-8 :height 1.05)
(set-face-attribute 'org-level-2 nil :inherit 'org-level-8 :height 1.1)
;; (set-face-attribute 'org-level-1 nil :inherit 'org-level-8 :height 1.2)
(set-face-attribute 'org-level-1 nil :height 1.44)
;; Only use the first 4 styles and do not cycle.
(setq org-cycle-level-faces nil)
(setq org-n-level-faces 4)
;; Document Title, (\huge)
(set-face-attribute 'org-document-title nil
                    :height 2.074
                    :foreground 'unspecified
                    :inherit 'org-level-8)

(pcase modal-mode
  (:evil (general-def '(normal insert visual) org-mode-map
           "M-n"   'org-metaright
           "M-N"   'org-shiftmetaright
           "M-t"   'org-metaleft
           "M-T"   'org-shiftmetaleft
           "M-s"   'org-metadown
           "M-S"   'org-shiftmetadown
           "M-r"   'org-metaup
           "M-R"   'org-shiftmetaup
           "<tab>" 'org-cycle)))

(defun my-org-hooks ()
  "In Org-Mode I don't want the lines to be hard-wrapped."
  ;; Note that turning on `visual-line-mode' also turns on `word-wrap'.
  (visual-line-mode)
  (setq-local fill-column nil
              left-margin-width 10
              right-margin-width 5
              company-backends '((company-yasnippet company-capf company-files company-dabbrev))))

(add-hook 'org-mode-hook #'my-org-hooks)
(add-hook 'org-mode-hook #'org-indent-mode)

;;
;; `org-appear' makes it so that when the cursor is on a hidden emphasis, the
;; symbols appear.
;;
(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t))

;;
;; `org-modern' brings a modern style for the Org buffers.
;;
(use-package org-modern
  :ensure t
  :init
  (with-eval-after-load 'org (global-org-modern-mode))
  :config
  (setq org-modern-checkbox nil))


(provide 'config-org)
;;; config-org.el ends here
