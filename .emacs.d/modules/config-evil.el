;;; config-evil.el --- Summary:  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:

(require 'general)

;;
;; `evil' just because I cannot live without modal editing.
;;
(use-package evil
  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil
        ;; evil-undo-system 'undo-tree
        ;; The cursor is allowed to go one character after the end of the line
        ;; just like in Emacs.
        evil-move-beyond-eol t)
  :config
  (evil-mode))

;; Custom keybindings.
(defun l0/open-line-no-split (&optional direction)
  "Insert a newline DIRECTION lines above or below the cursor.

This function does not split the current line.

If DIRECTION is less than or equal to 0, insert the newline DIRECTION - 1
line(s) above the cursor.

If DIRECTION is superior to 0 or nil, insert the newline DIRECTION lines below
the cursor."
  (interactive)

  (save-excursion
    (move-end-of-line direction)
    (newline)))

(defun l0/open-line-no-split-above ()
  "Insert a newline above the cursor without splitting the current line."
  (interactive)
  (l0/open-line-no-split 0))

(defun l0/open-line-no-split-below ()
  "Insert a newline below the cursor without splitting the current line."
  (interactive)
  (l0/open-line-no-split))

(general-def '(normal motion visual insert)
  "C-e"        'move-end-of-line
  "C-a"        'move-beginning-of-line
  "C-b"        'backward-char
  "C-r"        'undo-redo
  "s-u"        'universal-argument
  "s-s"        'save-buffer)


(general-def '(normal motion visual insert) 'prog-mode-map
  "M-RET"      'l0/open-line-no-split-above
  "M-<return>" 'l0/open-line-no-split-above
  "C-RET"      'l0/open-line-no-split-below
  "C-<return>" 'l0/open-line-no-split-below)

(general-def '(insert motion visual)
  "C-f" 'forward-char)

(general-def 'insert
  "C-d" 'delete-char
  "C-n" 'next-line
  "C-p" 'previous-line)

(general-def 'normal
  "SPC SPC" 'execute-extended-command
  "SPC b k" 'kill-this-buffer
  "SPC b g" 'revert-buffer-quick)

(general-def 'visual
  "C-c r" '(:ignore t :which-key "[r]egion")
  :prefix "C-c r"
  "s" 'sort-lines
  "a" 'align-regexp
  "c" 'count-words)


;;
;; `evil-collection' helps me define my bépo keybindings for different modes
;; that I might use.
;;
(use-package evil-collection
  :ensure t
  :config
  (add-hook 'evil-collection-setup-hook #'config/rotate-evil-collection))

;; (setq evil-collection-mode-list nil)
;; (evil-collection-init '(org corfu))

(defvar my-intercept-mode-map (make-sparse-keymap)
  "High precedence keymap.")

(define-minor-mode my-intercept-mode
  "Global minor mode for higher precedence evil keybindings."
  :global t)

(my-intercept-mode)

(require 'evil-core)
(require 'evil-collection)

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
