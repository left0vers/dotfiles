;;; config-package.el
;;;
;;; Commentary:
;;;
;;; Code:

;; (defvar bootstrap-version)
;;   (let ((bootstrap-file
;;         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
;; (package-refresh-contents)


(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package diminish
  :ensure t)
(use-package bind-key
  :ensure t)


(provide 'config-package)
;;; config-package.el ends here
