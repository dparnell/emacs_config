;; emacs22 setup
(if (< emacs-major-version 23)
      (setq load-path (append (append load-path '("~/.emacs.d/nxml/")) '("~/.emacs.d/ruby-mode"))))

(setq load-path (append load-path '("~/.emacs.d/ruby-mode")))

;; Use spaces instead of TABs
(setq indent-tabs-mode nil)
(setq c-indent-level 2)

;; Magit
(setq load-path (append load-path '("~/.emacs.d/magit")))
(require 'magit)

;; MuMaMo-Mode for rhtml files
(load "~/.emacs.d/nxhtml/autostart.el")
(add-to-list 'load-path "~/.emacs.d/nxhtml/util")
(require 'mumamo-fun)
(setq mumamo-chunk-coloring 'submode-colored)
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . eruby-html-mumamo))
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-html-mumamo))

;; load rails-reloaded
(setq load-path (cons (expand-file-name "~/.emacs.d/rails-reloaded") load-path))
(require 'rails-autoload)

;; scala-mode
(let ((path "~/.emacs.d/scala-mode"))
  (setq load-path (cons path load-path))
  (load "scala-mode-auto.el"))

(defun scala-turnoff-indent-tabs-mode ()
  (setq indent-tabs-mode nil))

;; scala mode hooks
(add-hook 'scala-mode-hook 'scala-turnoff-indent-tabs-mode)

;; ensime Scala stuff
(require 'scala-mode)
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(add-to-list 'load-path "~/.emacs.d/ensime_2.8.0-0.2.4/elisp/")
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; rspec
(setq load-path (append load-path '("~/.emacs.d/rspec-mode")))
(require 'rspec-mode)

;; Display the current date and time in the status bar
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

;; gforth
(setq load-path (append load-path '("~/.emacs.d/gforth")))
(autoload 'forth-mode "gforth.el")
(autoload 'forth-block-mode "gforth.el")
(add-to-list 'auto-mode-alist '("\\.fs$" . forth-mode))

;; actionscript mode
(autoload 'actionscript-mode "~/.emacs.d/actionscript-mode.el")
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))

;; start the emacs server
(setq server-socket-dir "~/.emacs.d/server")
(server-start)

;; get rid of the message about buffers still having clients when closing a file
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
