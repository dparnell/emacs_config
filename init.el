;; emacs22 setup
(if (< emacs-major-version 23)
      (setq load-path (append (append load-path '("~/.emacs.d/nxml/")) '("~/.emacs.d/ruby-mode"))))

;; solarized colour scheme
(if (< emacs-major-version 24)
      (progn
        (setq load-path (append (append load-path '("~/.emacs.d/color-theme")) '("~/.emacs.d/emacs-color-theme-solarized")))
	(require 'color-theme-solarized)
	(color-theme-solarized-dark))
      (progn
	(setq custom-theme-load-path (append custom-theme-load-path '("~/.emacs.d/custom-theme-load-path")))
	(load-theme 'solarized-dark t)))

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
; (add-to-list 'auto-mode-alist '("\\.rhtml\\'" . eruby-html-mumamo))
; (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-html-mumamo))

(setq
      nxhtml-global-minor-mode t
      mumamo-chunk-coloring 'submode-colored
      nxhtml-skip-welcome t
      indent-region-mode t
      rng-nxml-auto-validate-flag nil
      nxml-degraded t)
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-html-mumamo))
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . eruby-html-mumamo))

;; Special rails file
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))

;; RHTML mode
;;; rhtml mode
; (add-to-list 'load-path "~/.emacs.d/rhtml")
; (require 'rhtml-mode)

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

;; Scala stuff
(require 'scala-mode)
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
;; ensime - scala support
;;
;; Before ensime will work it is necessary to build it from source
;; Go into the ensime directory and enter the following command
;;
;;     sbt stage
;;
;; This will build ensime :)
;;
;; (if (file-accessible-directory-p "~/.emacs.d/ensime/dist/elisp/") (
;;  (add-to-list 'load-path "~/.emacs.d/ensime/dist/elisp/")
;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)))

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
(chmod "~/.emacs.d/server" #o700)
(setq server-socket-dir "~/.emacs.d/server")
(server-start)

;; get rid of the message about buffers still having clients when closing a file
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(global-set-key "\C-cx" 'close-all-buffers)

;; Cucumber mode
(add-to-list 'load-path "~/.emacs.d/cucumber.el")
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; Coffee script mode
(add-to-list 'load-path "~/.emacs.d/coffee-mode")
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

;; SCSS mode
(add-to-list 'load-path (expand-file-name "~/.emacs.d/scss-mode"))
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)

;; Erlang mode
(add-to-list 'load-path (expand-file-name "~/.emacs.d/erlang-mode"))
(require 'erlang-start)

;; Set up mumao they way I want it
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(mumamo-background-chunk-major ((((class color) (min-colors 87) (background light)) nil)))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background light)) (:width normal))))
 '(mumamo-background-chunk-submode2 ((((class color) (min-colors 88) (background light)) (:weight normal))))
 '(mumamo-background-chunk-submode3 ((((class color) (min-colors 88) (background light)) nil)))
 '(mumamo-background-chunk-submode4 ((((class color) (min-colors 88) (background light)) nil)))
)

;; Yaml mode
(setq load-path (append load-path '("~/.emacs.d/yaml-mode")))
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; Textile mode
(autoload 'textile-minor-mode "~/.emacs.d/textile-minor-mode.el")
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-minor-mode))

;; turn on mouse support in iTerm2
(require 'mouse) 
(xterm-mouse-mode t) 
(defun track-mouse (e))

;; PHP support
(setq load-path (append load-path '("~/.emacs.d/php-mode")))
(require 'php-mode)

;; reformat the current file
;; stolen from here: http://emacsblog.org/2007/01/17/indent-whole-buffer/
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
