;;; config-shell.el --- Shell configuration -*- lexical-binding: t -*-

;; Author: Julien Loudet
;; Maintainer: Julien Loudet
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/left0vers/dotfiles.git
;; Keywords: shell, eshell


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

(require 'eshell)
(require 'em-term)

(add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show"))
(add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))

(add-hook 'eshell-mode-hook #'(lambda () (setq-local company-idle-delay nil
                                                     show-trailing-whitespace nil)))


(provide 'config-shell)
;;; config-shell.el ends here
