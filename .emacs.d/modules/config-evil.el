;;; config-evil.el --- Summary:  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:

(require 'general)

;;
;; `undo-tree' as the way to deal with undo history.
;;
(use-package undo-tree
  :ensure t
  :init
  (setq undo-tree-history-directory-alist '(("." . "/tmp")))
  (add-hook 'after-init-hook 'global-undo-tree-mode))

;;
;; `evil' just because I cannot live without modal editing.
(use-package evil
  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil
        ;; The cursor is allowed to go one character after the end of the line
        ;; just like in Emacs.
        evil-undo-system 'undo-tree
        evil-move-beyond-eol t)
  :config
  (evil-mode 1))

;; Custom keybindings.
(general-def '(normal motion visual insert)
  "C-e"   'move-end-of-line
  "C-a"   'move-beginning-of-line
  "C-s"   'save-buffer)

(general-def '(normal visual)
  "SPC u" 'universal-argument)

(general-def '(insert motion visual)
  "C-f" 'forward-char)

(general-def 'insert
  "C-d" 'delete-char)


;;
;; `evil-collection' helps me define my bépo keybindings for different modes
;; that I might us.
;;
(defvar my-intercept-mode-map (make-sparse-keymap)
  "High precedence keymap.")

(define-minor-mode my-intercept-mode
  "Global minor mode for higher precedence evil keybindings."
  :global t)

(my-intercept-mode)

(dolist (state '(normal visual motion))
  (evil-make-intercept-map
   ;; NOTE: This requires an evil version from 2018-03-20 or later
   (evil-get-auxiliary-keymap my-intercept-mode-map state t t)
   state))

(evil-define-key '(normal visual motion) my-intercept-mode-map
  (kbd "t") 'backward-char
  (kbd "n") 'forward-char
  (kbd "s") 'next-line
  (kbd "r") 'previous-line)

(defun config/hjkl-bepo-rotation (modes mode-keymaps &rest _rest)
  "Rotate the default evil keys into bépo for MODES and MODE-KEYMAPS."
  (evil-collection-translate-key modes mode-keymaps
    ;; for me `tsrn' is qwerty `hjkl'
    "t" "h"
    "s" "j"
    "r" "k"
    "n" "l"
    ;; add back `tsrn' functionality
    "j" "t"
    "k" "s"
    "h" "r"
    "l" "n"
    ;; also switch `L' for coherence with `l'
    "L" "N"
    ;; `w' is a bit far on bépo, `é' is better suited
    "é" "w"
    "É" "W"
    ;; `[]' remaped to `()'
    "(" "["
    ")" "]"
    ;; `<>' to `«»'
    "«" "<"
    "»" ">"))

(defun config/rotate-evil-collection (_mode mode-keymaps &rest _rest)
  "Rotate the default evil keys into bépo for all MODE-KEYMAPS."
  (config/hjkl-bepo-rotation '(normal visual motion) mode-keymaps))

(use-package evil-collection
  :ensure t)

(require 'evil-collection)
(add-hook 'evil-collection-setup-hook #'config/rotate-evil-collection)
;; (setq evil-collection-mode-list nil)
(evil-collection-init '(org corfu))

(config/hjkl-bepo-rotation nil '(evil-normal-state-map
                                 evil-visual-state-map
                                 evil-operator-state-map
                                 evil-motion-state-map))


;; NOTE: I feel like evil-collection mode overwrites the default state for some
;; of the major modes. My main issue was with `magit-mode' as I want to be in
;; `'emacs' state.
(evil-set-initial-state 'eshell-mode          'emacs)
(evil-set-initial-state 'magit-mode           'emacs)
(evil-set-initial-state 'compilation-mode     'emacs)
(evil-set-initial-state 'dired-mode           'emacs)
(evil-set-initial-state 'messages-buffer-mode 'emacs)
(evil-set-initial-state 'help-mode            'emacs)
(evil-set-initial-state 'special-mode         'emacs)
(evil-set-initial-state 'info-mode            'emacs)
(evil-set-initial-state 'diff-mode            'emacs)
(evil-set-initial-state 'rg-mode              'emacs)
(evil-set-initial-state 'epresent-mode        'emacs)
(evil-set-initial-state 'term-mode            'emacs)
(evil-set-initial-state 'wdired-mode          'normal)


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


(provide 'config-evil)
;;; config-evil.el ends here
