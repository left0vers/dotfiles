;;; init.el --- summary -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; -----------------------------------------------------------------------------
;; BETTER DEFAULTS

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Remember the previously used mini-buffer commands between Emacs session.
(savehist-mode 1)

(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      backup-by-copying t
      frame-inhibit-implied-resize t
      use-short-answers t
      ediff-window-setup-function 'ediff-setup-windows-plain)

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

(setq custom-file "~/.emacs.d/custom.el")


;; Mac OS specifics.
(if (eq system-type 'darwin)
    (progn
      (setq mac-right-command-modifier 'none)
      ;; (setq mac-left-command-modifier  'super)
      (setq mac-right-option-modifier  'none)))

;; Stop the cursor from blinking.
(blink-cursor-mode -1)
(setq visible-bell t)

;; Make the title bar transparent & dark.
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq-default indent-tabs-mode nil)
(add-hook 'prog-mode-hook (lambda ()
                            (hl-line-mode)
                            (setq-local show-trailing-whitespace t)
                            (delete-trailing-whitespace)))

;; Scrolling.
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000
      auto-window-vscroll nil
      scroll-preserve-screen-position t)
(pixel-scroll-precision-mode)

;; It bears repeating.
(repeat-mode 1)

;; Always display the fill column indicator.
(global-display-fill-column-indicator-mode)

(setq-default display-line-numbers-width 3
              fill-column 80)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(column-number-mode)

;; FONT
(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :weight 'light
                    :background "#1E1E1E"
                    :height 170)

;; Increase all the fonts by an increment.
(require 'face-remap)
(setq global-text-scale-adjust--increment-factor 15)
(global-set-key (kbd "s--") 'global-text-scale-adjust)
(global-set-key (kbd "s-+") 'global-text-scale-adjust)

;; Treat all themes as safe.
(setq custom-safe-themes t)
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one))

(use-package ef-themes
  :ensure t)

(defun my/switch-to-light-theme ()
  "Disable the `doom-one' theme and load `ef-light'"
  (interactive)
  (disable-theme 'doom-one)
  (load-theme 'ef-light))

(defun my/switch-to-dark-theme ()
  "Disable the `ef-light' theme and load `doom-one'"
  (interactive)
  (disable-theme 'ef-light)
  (load-theme 'doom-one))

(custom-set-faces '(font-lock-string-face ((t (:foreground "#BE896E")))))
(custom-set-faces '(font-lock-comment-face ((t (:foreground "#6A9956")))))
(custom-set-faces '(font-lock-doc-face ((t (:foreground "#688D4C")))))
(custom-set-faces '(font-lock-property-use-face ((t (:foreground "#5B8FCF")))))
(custom-set-faces '(mode-line-active ((t (:background "#1F6BCB"
                                          :foreground "#C5C5C5")))))
(custom-set-faces '(line-number ((t (:foreground "#717171"
                                     :height 0.9
                                     :slant unspecified)))))
(custom-set-faces '(line-number-current-line((t (:inherit 'line-number
                                                 :slant unspecified)))))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(unless (require 'dired+ nil t)
  (progn
    (package-vc-install "https://github.com/emacsmirror/dired-plus" :last-release)
    (require 'dired+)))

;; Make sure that we get the shell variables as well.
(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(use-package minions
  :ensure t
  :hook (after-init . minions-mode))

;; -----------------------------------------------------------------------------
;; WINDOW MANAGEMENT
(defun my/focus-window (window)
  "Focus the provided WINDOW."
  (select-window window))

(setq display-buffer-alist
      `(;; flymake list project diagnostics
        ((or . ((derived-mode . flymake-project-diagnostics-mode)
                (derived-mode . flycheck-error-list-mode)))
         (display-buffer-in-side-window)
         (dedicated     . t)
         (side          . bottom)
         (window-height . 0.3)
         (window-parameters . ((mode-line-format . none)))
         (body-function . my/focus-window))

        ((or . ("\\*eldoc\\*"
                "\\*persistent-doc-at-point*"))
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (dedicated     . t)
         (side          . bottom)
         (window-height . 0.3)
         (body-function . my/focus-window))

        ((derived-mode . help-mode)
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (dedicated     . t)
         (side          . bottom)
         (window-height . 0.3))))

;; -----------------------------------------------------------------------------
;; KEYBINDINGS & EVIL (MODAL EDITING)

;;
;; A `general' is a good leader…!
;;
(use-package general
  :ensure t)

(global-set-key (kbd "M-«") (kbd "M-<"))
(global-set-key (kbd "M-»") (kbd "M->"))

(global-set-key (kbd "M-(") (kbd "M-{"))
(global-set-key (kbd "M-)") (kbd "M-}"))

(global-set-key (kbd "C-c b k") 'kill-this-buffer)
(global-set-key (kbd "C-c b g") 'revert-buffer-quick)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-=") 'indent-region)

(defun tab-new-to-scratch-buffer ()
  "Creates a new tab on the right of the current tab opened on the
*scratch* buffer."
  (interactive)
  (tab-bar-new-tab 1)
  (scratch-buffer))

(defun tab-new-to-vterm ()
  "Creates a new tab on the right of the current tab opened on a *new* vterm."
  (interactive)
  (tab-bar-new-tab 1)
  (setq current-prefix-arg '(4))
  (call-interactively 'vterm))

;; Because my dotfiles are under version control, if I want not to type "yes"
;; every time I open my `init.el', I should set this variable to `t'.
(setq vc-follow-symlinks t)

(defun my/open-emacs-init ()
  "Open .emacs/init.el configuration file."
  (interactive)
  (find-file (f-join user-emacs-directory "init.el")))

(general-def
  "C-<next>"  'tab-next
  "C-<prior>" 'tab-previous
  "C-x t RET" 'tab-new-to
  "C-x t v"   'tab-new-to-vterm
  "C-x t t"   'tab-switch
  "C-x t d"   'tab-close
  "C-x t D"   'tab-close-other

  "C-c e"     '(:ignore t :which-key "[e]macs")
  "C-c e l"   'list-packages
  "C-c e p"   'list-processes
  "C-c e i"   'my/open-emacs-init)

(general-def (special-mode-map help-mode-map)
  "n" 'next-line
  "p" 'previous-line)

;; `which-key': keybindings discovery.
(use-package which-key
  :ensure t
  :config
  (which-key-mode))


;; `evil' just because I cannot live without modal editing.
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t
	evil-disable-insert-state-bindings t
        evil-want-keybinding nil
	evil-default-state 'emacs
	evil-motion-state-modes nil
        ;; evil-undo-system 'undo-tree
        ;; The cursor is allowed to go one character after the end of the line
        ;; just like in Emacs.
        evil-move-beyond-eol t)
  :config
  (evil-mode)

  (general-def '(normal motion visual insert)
    "C-e" 'move-end-of-line
    "C-a" 'move-beginning-of-line
    "C-b" 'backward-char
    "C-f" 'forward-char
    "C-r" 'undo-redo
    "s-u" 'universal-argument
    "s-s" 'save-buffer
    "C-n" 'next-line
    "C-p" 'previous-line
    "M-«" 'beginning-of-buffer
    "M-»" 'end-of-buffer)

  (general-def 'normal
    "SPC SPC" 'execute-extended-command

    ;; Lookup utilities.
    "g r"  'xref-find-references
    "g d"  'xref-find-definitions

    ;; Emacs utilities.
    "SPC e" '(:ignore t :which-key "[e]macs")
    "SPC e l" 'list-packages
    "SPC e p" 'list-processes
    "SPC e i" 'my/open-emacs-init

    ;; Buffer management.
    "SPC b k" 'kill-this-buffer
    "SPC b g" 'revert-buffer-quick

    ;; Tab management.
    "SPC t t" 'tab-switch
    "SPC t n" 'tab-new
    "SPC t p" 'project-other-tab-command
    "SPC t <return>" 'other-tab-prefix
    "SPC t r" 'tab-rename
    "SPC t d" 'tab-close
    "SPC t D" 'tab-close-other)

  (general-def 'visual
    "C-c r" '(:ignore t :which-key "[r]egion")
    :prefix "C-c r"
    "s" 'sort-lines
    "a" 'align-regexp
    "c" 'count-words)

  (general-def 'insert
    "C-w" 'evil-delete-backward-word
    "C-d" nil
    "C-p" nil
    "C-n" nil)

  (general-def '(normal motion operator visual)
    "t" 'backward-char
    "s" 'next-line
    "r" 'previous-line
    "n" 'forward-char
    "j" 'evil-find-char-to
    "k" 'evil-substitute
    "h" 'evil-replace
    "é" 'evil-forward-word-begin
    "É" 'evil-forward-WORD-begin
    "(" 'evil-previous-open-paren
    "{" 'evil-backward-paragraph
    ")" 'evil-next-close-paren
    "}" 'evil-forward-paragraph
    "«" 'evil-shift-left
    "»" 'evil-shift-right)

  (evil-set-initial-state 'fundamental-mode 'normal)
  (evil-set-initial-state 'eshell-mode  'emacs)
  (evil-set-initial-state 'apropos-mode 'emacs)
  (evil-set-initial-state 'text-mode    'normal)
  (evil-set-initial-state 'conf-mode    'normal)
  (evil-set-initial-state 'org-mode     'normal)
  (evil-set-initial-state 'prog-mode    'normal)
  (evil-set-initial-state 'wdired-mode  'normal))


;;
;; `evil-surround': the power of surrounding things.
;;
(use-package evil-surround
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-evil-surround-mode))


;;
;; `evil-nerd-commenter': easy comments.
;;
(use-package evil-nerd-commenter
  :ensure t
  :config
  (general-def '(normal visual)
    "g c" 'evilnc-comment-or-uncomment-lines))

;; -----------------------------------------------------------------------------
;; ESHELL
(use-package eshell
  :bind (("C-r" . consult-history))
  :config
  (add-hook 'eshell-mode-hook (lambda ()
                                (setq fill-column -1))))

;; -----------------------------------------------------------------------------
;; VTERM
(use-package vterm
  :ensure t
  :config
  (general-def
    "C-c s" 'vterm)
  (add-hook 'vterm-mode-hook (lambda () (setq fill-column -1))))


;; -----------------------------------------------------------------------------
;; ORG-MODE

(general-def 'org-mode-map
  "<tab>" 'org-cycle
  "M-t"   'org-metaleft
  "M-T"   'org-shiftmetaleft
  "M-n"   'org-metaright
  "M-N"   'org-shiftmetaright
  "M-s"   'org-metadown
  "M-S"   'org-shiftmetadown
  "M-r"   'org-metaup
  "M-R"   'org-shiftmetaup)

(require 'org)
(set-face-attribute 'org-block nil :inherit 'default)
(setq org-hide-emphasis-markers t)

(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-checkbox nil))

;; `org-appear' makes it so that when the cursor is on a hidden emphasis, the
;; symbols appear.
(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t))

(add-hook 'org-mode-hook (lambda ()
                           (setq-local fill-column nil
                                       left-margin-width 8
                                       right-margin-width 8)))
(add-hook 'org-mode-hook #'visual-line-mode)


;; -----------------------------------------------------------------------------
;; NAVIGATION

;;
;; `ace-window': quickly navigate to any window.
;;
(use-package ace-window
  :ensure t
  :init
  ;; NOTE: This has to go before the corresponding `require' statement!
  (defvar aw-dispatch-alist
    '((?d aw-delete-window "Delete window")
      (?S aw-swap-window "Swap Windows")
      ;; (?m aw-move-window "Move Window")
      ;; (?C aw-copy-window "Copy Window")
      ;; (?j aw-switch-buffer-in-window "Select Buffer")
      ;; (?n aw-flip-window)
      ;; (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
      ;; (?c aw-split-window-fair "Split Fair Window")
      (?h aw-split-window-vert "Split Window Vertically")
      (?v aw-split-window-horz "Split Window Horizontally")
      (?D delete-other-windows "Delete Other Windows"))
    ;; (?? aw-show-dispatch-help))
    "List of actions for `aw-dispatch-default'.")
  :config
  (setq aw-keys '(?a ?u ?i ?e ?t ?r ?n)
        aw-dispatch-always t
        aw-background nil
        aw-scope 'frame)

  ;; Make the letters bigger on the screen.
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:foreground "red" :background unspecified :weight bold :height 3.0)))))

  (general-def
    "s-w" 'ace-window))

(use-package avy
  :ensure t
  :bind (("M-s '" . avy-goto-char-timer)))


;; -----------------------------------------------------------------------------
;; COMPLETION & NARROWING

;;
;; Consult: search and navigation commands based on the completion function
;; backed in Emacs, `completing-read'.
;;
(use-package consult
  :ensure t
  :bind (("C-x b"  . consult-buffer)
         ;; ("M-i" . consult-imenu) ;; replaced by 'symbols-outline
         ("C-s"    . consult-line)
         ("M-y"    . consult-yank-from-kill-ring))
  :config
  (setq xref-show-xrefs-function       #'consult-xref
        xref-show-definitions-function #'consult-xref
        ;; TODO Investigate the narrowing key.
        ;; consult-narrow-key "«"
        ;; By default, the variable is set to `any' which means that any key
        ;; will trigger the preview (i.e. navigating to the previous/next line
        ;; will preview the file at point).
        ;;
        ;; Setting it to a specific key deactivate this behavior.
        consult-preview-key "M-.")

  ;; NOTE Declaring these keybindings inside the `:config' section will not make
  ;; them active until `consult' is first loaded.
  (general-def 'normal
    "SPC b b"  'consult-buffer
    "SPC b p"  'consult-project-buffer
    "SPC / g"  'consult-git-grep
    "SPC / /"  'consult-ripgrep))


;;
;; Vertico
;;
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-cycle t))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

(use-package nerd-icons-completion
  :if (display-graphic-p)
  :after vertico
  :ensure t
  :hook (vertico-mode . nerd-icons-completion-mode))

;;
;; Marginalia
;;
(use-package marginalia
  :ensure t
  :config
  (set-face-attribute 'marginalia-documentation nil :foreground "#72809a" :height 1.0 :inherit nil)
  (marginalia-mode))


;;
;; `orderless' completion style.
;;
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;;
;; Embark
;;
(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act))         ;; pick some comfortable binding
  ;; ("C-;" . embark-dwim)        ;; good alternative: M-.
  ;; ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  )

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;;
;; Corfu: Completion Overlay in Region FUnction
;;
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :hook ((corfu-mode . corfu-popupinfo-mode)
         (evil-normal-state-entry . corfu-quit))
  :config
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-prefix 2
        corfu-preselect 'first)
  (general-def 'insert
    :prefix "C-SPC"
    "SPC" 'completion-at-point))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :ensure t
  :config
  (general-def 'insert
    :prefix "C-SPC"
    "/" 'cape-dabbrev
    "d" 'cape-dict
    ":" 'cape-emoji
    "f" 'cape-file))


;; -----------------------------------------------------------------------------
;; SPELL-CHECKER

(use-package jinx
  :ensure t
  :diminish
  :hook (emacs-startup . global-jinx-mode)
  :bind ([remap ispell-word] . jinx-correct)
  :config
  (setq jinx-languages "en_GB fr_FR")
  (push '(toml-ts-mode font-lock-comment-face font-lock-string-face) jinx-include-faces))



;; -----------------------------------------------------------------------------
;; YASNIPPETS

(use-package yasnippet
  :ensure t
  :diminish
  :hook (after-init . yas-global-mode)
  :commands (yas-minor-mode-on
             yas-expand
             yas-expand-snippet
             yas-lookup-snippet
             yas-insert-snippet
             yas-new-snippet
             yas-visit-snippet-file
             yas-activate-extra-mode
             yas-deactivate-extra-mode
             yas-maybe-expand-abbrev-key-filter)
  :config
  (advice-add #'yas-snippet-dirs :filter-return #'delete-dups)
  (general-def 'insert
    :prefix "C-SPC"
    "y" 'yas-insert-snippet))

(use-package yasnippet-snippets
  :ensure t
  :diminish)


;; -----------------------------------------------------------------------------
;; PROJECT MANAGEMENT

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
            (message "(No change need to be saved)")
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


;; -----------------------------------------------------------------------------
;; PROGRAMMING

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :ensure t
  :hook (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil
        sp-show-pair-from-inside t
        sp-cancel-autoskip-on-backward-movement nil
        ;; By default, if a pair is inserted, the `sp-pair-overlay-keymap' is
        ;; entered which makes `C-g' require two presses in order to quit
        ;; whatever we are doing.
        sp-pair-overlay-keymap (make-sparse-keymap)
        sp-max-prefix-length 25
        sp-max-pair-length 4)

  (defun smartparens-pair-newline-and-indent (id action context)
    (save-excursion
      (newline)
      (indent-according-to-mode))
    (indent-according-to-mode))

  (sp-pair "{" nil :post-handlers
           '(:add (smartparens-pair-newline-and-indent "RET")))
  (sp-pair "[" nil :post-handlers
           '(:add (smartparens-pair-newline-and-indent "RET")))

  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "'" nil :actions nil)
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "`" nil :actions nil)
  (sp-local-pair 'rust-ts-mode "r#\"" "\"#" :wrap "C-#"))

(use-package hl-todo
  :ensure t
  :hook (after-init . global-hl-todo-mode))

(use-package markdown-mode
  :ensure t
  :config
  (custom-set-faces '(markdown-code-face ((t :background "unspecified")))))

(use-package flymake
  :config
  (setq-default next-error-function 'flymake-goto-next-error
                next-error-move-function 'flymake-goto-next-error)

  (general-def 'normal
    "SPC d" '(:ignore t :which-key "[d]iagnostics")
    "SPC d l" 'flymake-show-project-diagnostics
    "SPC d n" 'next-error
    "SPC d p" 'previous-error))

;; (use-package flycheck
;;   :ensure t
;;   :hook (after-init . global-flycheck-mode))

;; (use-package flycheck-eglot
;;   :ensure t
;;   :after (flycheck eglot)
;;   :config
;;   (global-flycheck-eglot-mode 1))


;; -----------------------------------------------------------------------------
;; MAGIT

;; Setting up GPG on macOS.
;;
;; 1. Install pinentry-mac: homebrew install pinentry-mac
;;
;; 2. Modify the ~/.gnupg/gpg-agent.conf to include the following lines:
;;
;; allow-emacs-pinentry
;; allow-loopback-pinentry
;; pinentry-program /opt/homebrew/bin/pinentry-mac
;;
;; 3. Reload your gpg-agent:
;;
;; gpg-connect-agent reloadagent /bye

(use-package magit
  :ensure t
  :config
  ;; Requiring `magit-extras' autoloads the function `magit-project-status'
  ;; which then appears once we open a new version-controlled project via
  ;; `project.el'
  (require 'magit-extras)
  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1)
  (setq magit-bury-buffer-function #'magit-restore-window-configuration)

  (add-hook 'git-commit-mode-hook (lambda () (setq fill-column 72)))

  (general-def 'normal
    "SPC g" '(:ignore t :which-key "[M]agit")
    "SPC g s" 'magit-status))

(use-package diff-hl
  :ensure t
  :hook ((after-init         . global-diff-hl-mode)
         (dired-mode         . diff-hl-dired-mode)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))


;; -----------------------------------------------------------------------------
;; TREE-SITTER

(use-package tree-sitter
  :diminish
  :defer t)

(use-package tree-sitter-langs
  :ensure t)

(use-package tree-sitter-indent
  :ensure t)

(use-package treesit-auto
  :ensure t
  :diminish
  :config
  (setq treesit-auto-install 'prompt
        treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


;; -----------------------------------------------------------------------------
;; LSP CLIENT

(setq read-process-output-max (* 1024 1024))  ; 1mb --- lsp-mode recommendation

(defvar persistent-doc-buffer-name "*persistent-doc-at-point*"
  "The name of the buffer holding the persistent documentation.")

(defun my/persistent-doc-at-point ()
  "Document the thing at point and display it in a frame below the current one.

The content of the frame will not be refreshed if the cursor moves. It will only
be updated on a following call to this function."
  (interactive)
  (let ((symbol-documentation-buffer (eldoc-doc-buffer nil)))
    (with-temp-buffer-window
        persistent-doc-buffer-name
        nil  ; The display of the buffer is controlled by `display-buffer-alist'
        nil
      (with-current-buffer symbol-documentation-buffer
        (copy-to-buffer persistent-doc-buffer-name (point-min) (point-max))))

    (with-current-buffer persistent-doc-buffer-name (help-mode))))

(use-package eglot
  :ensure t
  :commands eglot
  :config
  (custom-set-faces
   '(eglot-inlay-hint-face ((t
                             :height 0.8
                             ;; :foreground "#d8dee9"
                             :foreground "#72809a"
                             ;; :background "#2E3440"
                             :slant italic))))

  ;; Disable the events buffer to give a little boost to Eglot when completing.
  (setq eglot-events-buffer-size 0)
  (general-def 'normal eglot-mode-map
    "SPC l" '(:ignore t :which-key "Language Server")
    "SPC l r" 'eglot-rename
    "SPC l a" 'eglot-code-actions
    "g t"     'eglot-find-typeDefinition
    "g i"     'eglot-find-implementation
    "g h"     'my/persistent-doc-at-point)

  (add-to-list 'eglot-server-programs
               `(rust-ts-mode . ("rust-analyzer" :initializationOptions
                                 (:procMacro (:enable t)
                                  :check (:command "clippy")
                                  :cargo (:buildScripts (:enable t))))))

  (general-def eglot-mode-map
    "C-c ." 'eglot-code-actions))

(use-package consult-eglot
  :ensure t
  :after eglot
  :config
  (general-def 'normal eglot-mode-map
    "SPC l s" 'consult-eglot-symbols))

;; The package `eglot-booster' requires first to install the Rust executable:
;;
;; https://github.com/blahgeek/emacs-lsp-booster
(if (executable-find "emacs-lsp-booster")
    (progn
      (unless (require 'eglot-booster nil t)
          (package-vc-install "https://github.com/jdtsmith/eglot-booster" :last-release))
      (use-package eglot-booster
        :after eglot
        :config (eglot-booster-mode)))
  (message "⚠️ Could not find `emacs-lsp-booster' executable: https://github.com/blahgeek/emacs-lsp-booster"))

(use-package breadcrumb
  :ensure t
  :hook ((rust-ts-mode . breadcrumb-local-mode))
  :config
  (custom-set-faces '(breadcrumb-face ((t :foreground "#72809a")))))

;; -----------------------------------------------------------------------------
;; RUST
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-hook 'rust-ts-mode-hook #'eglot-ensure)
(add-hook 'rust-ts-mode-hook
          (lambda ()
            "Rust specific configurations."
            (setq-local fill-column 120)
            ;; The last argument to `add-hook' makes it local to the mode.
            (add-hook 'before-save-hook #'eglot-format-buffer nil t)))




;; -----------------------------------------------------------------------------
;; YAML
(add-to-list 'auto-mode-alist '("\\.\\(?:yml\\|yaml\\)\\'" . yaml-ts-mode))


;;; init.el ends here
