;;; config-meow.el --- Summary:  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:

(defun meow-setup ()
  (meow-leader-define-key
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-motion-overwrite-define-key
   ;; custom keybinding for motion state
   '("<escape>" . ignore))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("'" . meow-inner-of-thing)
   '(")" . meow-bounds-of-thing)
   '("«" . meow-beginning-of-thing)
   '("»" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-line)
   ;; '("E" . meow-goto-line)
   '("f" . meow-find)
   '("G" . meow-grab)
   '("t" . meow-left)
   '("T" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("$" . meow-join)
   '("k" . meow-kill)
   '("j" . meow-till)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("s" . meow-next)
   '("S" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("<escape>" . meow-cancel-selection)
   '("r" . meow-prev)
   '("R" . meow-prev-expand)
   ; '("Q" . meow-quit)
   '("g" . meow-goto-line)
   '("h" . meow-replace)
   '("H" . meow-swap-grab)
   '("/" . meow-search)
   '("n" . meow-right)
   '("N" . meow-right-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-save)
   '("X" . meow-sync-grab)
   '("y" . meow-yank)
   '("z" . meow-pop-selection)
   '("'" . repeat)))
   ;; '("<escape>" . ignore)))

(use-package meow
  :ensure t
  :config
  (meow-setup)
  (meow-global-mode 1))


(provide 'config-meow)
;;; config-meow.el ends here
