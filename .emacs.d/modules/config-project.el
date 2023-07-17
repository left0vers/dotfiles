;;; config-project.el --- Summary:
;;;
;;; Commentary:
;;;
;;; Code:

(require 'general)

;; /////////////////////////////////////////////////////////////////////////////
;; 
;; project.el
;;
;; //////////
;;
;; The following bindings mimic the ones provided by `projectile.el' that I am
;; used to.

(defun my/save-modified-buffers-in-current-project ()
  "Save all modified buffers for the current project."
  (interactive)
  (if (project-current nil)
      ;; we are visiting a buffer that belongs to a project, try to see if there
      ;; unmodified buffers
      (let* ((buffers (project-buffers (project-current nil)))
             (modified-buffers (cl-remove-if-not
                                (lambda (buf)
                                  ;; `buffer-file-name' will return `nil' if
                                  ;; `buf' points to a buffer that is not
                                  ;; associated to a real file.
                                  ;;
                                  ;; In short, that lambda filters out buffers
                                  ;; that are not pointing to a file.
                                  (and (buffer-file-name buf)
                                       (buffer-modified-p buf)))
                                buffers)))
        (if (null modified-buffers)
            (message "(No changes need to be saved)")
          (dolist (buf modified-buffers)
            (with-current-buffer buf
              (save-buffer)
              (message "Saved < %s >" buf)))))
    ;; we are visiting a buffer that does not belong to a project
    (message "(No project found)")))

(general-def 'normal
  "SPC p p" 'project-switch-project
  "SPC p f" 'project-find-file
  "SPC p S" 'my/save-modified-buffers-in-current-project
  "SPC p d" 'project-find-dir
  "SPC p g" 'project-find-regexp
  "SPC p k" 'project-kill-buffers
  "SPC p e" 'project-eshell)

(provide 'config-project)
;;; config-project.el ends here
