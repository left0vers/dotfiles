;;; config-project.el --- Summary:
;;;
;;; Commentary:
;;;
;;; Code:

(require 'general)

;;
;; `projectile': project management utility.
;;
;; (use-package projectile
;;   :config
;;   (setq projectile-mode-line-prefix ""
;;         projectile-indexing-method 'hybrid
;;         projectile-sort-order 'recentf
;;         projectile-use-git-grep t)
;;   (add-hook 'after-init-hook 'projectile-mode)
;;   ;; Snippet copied from here:
;;   ;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-projectile.el
;;   (when (executable-find "rg")
;;     (setq projectile-generic-command
;;           (let ((rg-cmd ""))
;;             (dolist (dir projectile-globally-ignored-directories)
;;               (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
;;             (concat "rg -0 --files --color=never --hidden" rg-cmd))))

;;   (pcase modal-mode
;;     (:evil (general-def 'normal
;;               "SPC p" 'projectile-command-map)))

;;   (define-key global-map (kbd "C-x p") 'projectile-command-map))

;; (global-define-key (kbd "C-x "))


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
