;;; config-eglot.el --- Eglot configuration -*- lexical-binding: t -*-

;; Author: Julien Loudet
;; Package-Requires: (general)
;; Homepage: homepage
;; Keywords: keywords

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

;;; Code:

(require 'general)

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
  (pcase modal-mode
    (:evil (general-def 'normal 'eglot-mode-map
             "SPC l" '(:ignore t :which-key "Language Server")
             "SPC l r" 'eglot-rename
             "SPC l a" 'eglot-code-actions
             "g t"     'eglot-find-typeDefinition)))

  (general-def eglot-mode-map
    "C-c ." 'eglot-code-actions))

(use-package consult-eglot
  :ensure t
  :commands consult-eglot-symbols
  :bind (:map eglot-mode-map ([remap xref-find-apropos] . consult-eglot-symbols))
  :init
  (pcase modal-mode
    (:evil (general-def 'normal
             "SPC l s" 'consult-eglot-symbols))))


(use-package symbols-outline
  :ensure t
  :commands symbols-outline-show
  :config
  (global-set-key (kbd "M-i") 'symbols-outline-show)
  (setq symbols-outline-fetch-fn #'symbols-outline-lsp-fetch
        symbols-outline-window-position 'left)
  (symbols-outline-follow-mode))


(use-package eldoc-box
  :ensure t
  :commands eldoc-box-help-at-point
  :init
  (pcase modal-mode
    (:evil (general-def 'normal
             "g h" 'eldoc-box-help-at-point))))

(provide 'config-eglot)
;;; config-eglot.el ends here
