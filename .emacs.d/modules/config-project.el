;;; config-project.el --- Summary:
;;;
;;; Commentary:
;;;
;;; Code:

(require 'general)

;; /////////////////////////////////////////////////////////////////////////////
;; project.el
;;
;; The following bindings mimic the ones provided by `projectile.el' that I am
;; used to.
(general-def 'normal
  "SPC p p" 'project-switch-project
  "SPC p f" 'project-find-file
  "SPC p d" 'project-find-dir
  "SPC p k" 'project-kill-buffers
  "SPC p e" 'project-eshell)

(provide 'config-project)
;;; config-project.el ends here
