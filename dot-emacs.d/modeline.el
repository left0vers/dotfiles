;; (kill-local-variable 'mode-line-format)
;; (force-mode-line-update)

(setq-default mode-line-format
	      '("%e"
		my-mode-line/read-only
		(:eval (format " %s " (my-mode-line/input)))
		;; Git
		(:eval (my-mode-line/git-branch))
		;; Project
		(:eval (my-mode-line/project-root))
		(:eval (my-mode-line/modified))
		(:eval (buffer-name))
		;; The modeline can only display strings.
		(:eval (format " %s   " (my-mode-line/major-mode-indicator)))
		(:eval (eglot--mode-line-format))
		flymake-mode-line-counters))

;; -------------------------------------------------------------------
(defun my-mode-line/project-root ()
  "Returns the name of the project, if any."
  (when (buffer-file-name)
    (let* ((project (project-current nil (file-name-directory (buffer-file-name))))
	   (current-project-name (project-name project)))
      (when current-project-name
	(format "  %s %s/ " (nerd-icons-mdicon "nf-md-folder_home") current-project-name)))))


;; -------------------------------------------------------------------
;; (vc-git-branches)

(set-fontset-font t 'unicode (font-spec :family "Symbols Nerd Font Mono") nil 'prepend)

(defun my-mode-line/git-branch ()
  "Returns the name of the current Git branch, if any."
  (when-let (branch-name (car (vc-git-branches)))
    (propertize (format "  %s %s  " (nerd-icons-mdicon "nf-md-git") (car (vc-git-branches)))
		'face 'nerd-icons-red)))


;; -------------------------------------------------------------------
(defun my-mode-line/buffer-read-only()
  "Returns an icon if the current buffer is read-only."
  (when buffer-read-only
    (propertize (format "%s " (nerd-icons-codicon "nf-cod-workspace_untrusted")) 'face 'nerd-icons-dorange)))

(defvar my-mode-line/read-only
  '(:eval (my-mode-line/buffer-read-only))
  "Mode line construct indicating if the currently displayed buffer
   is read-only.")

(put 'my-mode-line/read-only 'risky-local-variable t)

;; -------------------------------------------------------------------
(defun my-mode-line/modified()
  "Returns a floppy disk icon if the file has been modified."
  (unless buffer-read-only
    (when (buffer-modified-p)
      (propertize (format "%s " (nerd-icons-mdicon "nf-md-content_save_alert")) 'face 'nerd-icons-green))))

;; -------------------------------------------------------------------
(evil-state-p)

(defun my-mode-line/input()
  "Returns the tag corresponding to the input method."
  (pcase evil-state
    ('normal	(propertize "  NORMAL  " 'face '(:background "#81a1c1" :foreground "#000000" :weight bold)))
    ('insert	(propertize "  INSERT  " 'face '(:background "#ebcb8b" :foreground "#000000" :weight bold)))
    ('visual	(propertize "  VISUAL  " 'face '(:background "#8fbcbb" :foreground "#000000" :weight bold)))
    ('motion	(propertize "  MOTION  " 'face '(:background "#a3be8c" :foreground "#000000" :weight bold)))
    ('operator	(propertize " OPERATOR " 'face '(:background "#d08770" :foreground "#000000" :weight bold)))
    ('emacs	(propertize "  EMACS   " 'face '(:background "#734ba9" :foreground "#E4DAEE" :weight bold)))))

;; (propertize (buffer-name) 'face 'error)

;; To define the different elements of the modeline I want to have.
;; (defvar my/text-mode
;;   '(:eval
;;     (format …)))

;; (put 'my/text-mode 'risky-local-variable t) ; MANDATORY

(defconst my-mode-line/evil-state-tags
  '((normal   :short "<N>"   :long "NORMAL")
    (insert   :short "<I>"   :long "INSERT")
    (visual   :short "<V>"   :long "VISUAL")
    (motion   :short "<M>"   :long "MOTION")
    (emacs    :short "<E>"   :long "EMACS")
    (operator :short "<O>"   :long "OPERATOR")
    (replace  :short "<R>"   :long "REPLACE"))
  "Short and long tags for Evil states.")

(defun my-mode-line/major-mode-indicator ()
  "Return an indicator for the major mode."
  (cond
   ((derived-mode-p 'text-mode) (nerd-icons-mdicon "nf-md-format_text"))
   ((derived-mode-p 'rust-ts-mode) (nerd-icons-devicon "nf-dev-rust"))
   ((derived-mode-p 'prog-mode) (nerd-icons-devicon "nf-dev-code"))
   ((derived-mode-p 'comint-mode) (nerd-icons-mdicon "nf-md-cog_box"))
   (t (nerd-icons-mdicon "nf-md-progress_question"))))
