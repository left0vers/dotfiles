;;; Summary: config-shell.el starts here

(require 'eshell)
(require 'em-term)

(add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show"))
(add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))


(provide 'config-shell)
;;; config-shell.el ends here
