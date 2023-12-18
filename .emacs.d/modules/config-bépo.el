;;; config-bépo.el --- Summary: -*- lexical-binding: t no-byte-compile: t -*-
;;;
;;; Commentary:
;;;
;;; Code:

(global-set-key (kbd "M-«") (kbd "M-<"))
(global-set-key (kbd "M-»") (kbd "M->"))

(global-set-key (kbd "M-(") (kbd "M-{"))
(global-set-key (kbd "M-)") (kbd "M-}"))

(global-set-key (kbd "C-c b k") 'kill-this-buffer)
(global-set-key (kbd "C-c b g") 'revert-buffer-quick)

(global-set-key (kbd "C-=") 'indent-region)

(provide 'config-bépo)
;;; config-bépo.el ends here
