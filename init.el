;; emacs22 setup
(if (< emacs-major-version 23)
      (setq load-path (append (append load-path '("~/.emacs.d/nxml/")) '("~/.emacs.d/ruby-mode"))))

;; MuMaMo-Mode for rhtml files
(load "~/.emacs.d/nxhtml/autostart.el")
;;(add-to-list 'load-path "~/.emacs.d/nxhtml/util")
;;(require 'mumamo-fun)
(setq mumamo-chunk-coloring 'submode-colored)
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . eruby-html-mumamo))
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-html-mumamo))

;; load rails-reloaded
(setq load-path (cons (expand-file-name "~/.emacs.d/rails-reloaded") load-path))
(require 'rails-autoload)

;;(rails/defbundle "Lib"
;;  ()
;;
;;  (rails/defresource 'libs "Libs"
;;                     :dir "lib"
;;                     :file-ext  "*")
;;)
