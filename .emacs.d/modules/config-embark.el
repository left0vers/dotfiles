;;; config-embark.el --- Summary:
;;;
;;; Commentary:
;;;
;;; Code:

(require 'general)

(use-package embark
  :config

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (general-def 'minibuffer-mode-map
    "C-." 'embark-act))


;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :config
  (require 'consult)
  (add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode))

(provide 'config-embark)
;;; config-embark.el ends here
