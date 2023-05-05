;;; config-spelling.el --- Summary
;;;
;;; Commentary:
;;;
;;; Code:

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind ([remap ispell-word] . jinx-correct))

(provide 'config-spelling)
;;; config-spelling.el ends here
