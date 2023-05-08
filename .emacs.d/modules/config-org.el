;;; config-org.el --- Summary:
;;;
;;; Commentary:
;;;
;;; Code:

(require 'org)

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "|" "DONE")
        (sequence "DELEGATED" "TO-CHECK" "|" "VALIDATED")
        (sequence "|" "MEETING")
        (sequence "|" "CANCELLED")))

(setq org-hide-emphasis-markers t)

(pcase modal-mode
  (:evil (general-def '(normal insert visual) org-mode-map
           "M-n" 'org-metaright
           "M-N" 'org-shiftmetaright
           "M-t" 'org-metaleft
           "M-T" 'org-shiftmetaleft
           "M-s" 'org-metadown
           "M-S" 'org-shiftmetadown
           "M-r" 'org-metaup
           "M-R" 'org-shiftmetaup
           "<tab>" 'org-cycle)))

(defun my-org-hooks ()
  (toggle-truncate-lines)
  (toggle-word-wrap))

(use-package org-modern)
(require 'org-modern)
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-mode-hook #'my-org-hooks)


(require 'org-agenda)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;; (require 'org-indent)
;; (add-hook 'org-mode-hook 'org-indent-mode)

(provide 'config-org)
;;; config-org.el ends here
