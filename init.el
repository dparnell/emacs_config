;; Set up the environment path
(if (string-equal "darwin" (symbol-name system-type))
      (progn
        (setenv "PATH" (concat "/usr/local/bin:/usr/local/sbin:" (getenv "PATH")))
        (setq exec-path (append exec-path '("/usr/local/bin" "/usr/local/sbin")))))

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
(setq custom-theme-load-path (append custom-theme-load-path '("~/.emacs.d/emacs-color-theme-solarized")))
(load-theme 'solarized-dark t)))


;; load powerline
(setq load-path (append load-path '("~/.emacs.d/powerline")))
(require 'powerline)

(set-face-attribute 'mode-line nil
	    :foreground "#fdf6e3"
	    :background "#333333"
	    :box nil)
(set-face-attribute 'mode-line-inactive nil
	    :box nil)
(set-face-attribute 'powerline-active1 nil
	    :foreground "#657b83"
	    :background "#111111"
	    :box nil)
(set-face-attribute 'powerline-active2 nil
	    :foreground "#839496"
	    :background "#000000"
	    :box nil)

(if (not window-system)
(setq powerline-default-separator 'utf-8))

(powerline-default-theme)

;; make sure that files are saved without trailing whitespace!
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; load the ruby mode
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
(if (or (< emacs-major-version 24) (and (= emacs-major-version 24) (< emacs-minor-version 2)))
(progn      
(let ((path "~/.emacs.d/scala-mode"))
(setq load-path (cons path load-path))
(load "scala-mode-auto.el")
(require 'scala-mode)))
(progn
(add-to-list 'load-path "~/.emacs.d/scala-mode2/")
(require 'scala-mode2)))

(defun scala-turnoff-indent-tabs-mode ()
(setq indent-tabs-mode nil))

;; scala mode hooks
(add-hook 'scala-mode-hook 'scala-turnoff-indent-tabs-mode)

;; Scala stuff
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

;; ecb
(add-to-list 'load-path "~/.emacs.d/ecb")
(require 'ecb)

(setq load-path (append load-path '("~/.emacs.d/multi-web-mode")))
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
	  (js-mode "<script[^>]*>" "</script>")
	  (css-mode "<style[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;; Django style template support
(add-to-list 'auto-mode-alist '("\\.dtl$" . django-html-mumamo-mode))

;; reformat the current file
;; stolen from here: http://emacsblog.org/2007/01/17/indent-whole-buffer/
(defun iwb ()
"indent whole buffer"
(interactive)
(delete-trailing-whitespace)
(indent-region (point-min) (point-max) nil)
(untabify (point-min) (point-max)))

(autoload 'circe "circe" "Connect to an IRC server" t)

(add-to-list 'load-path "~/.emacs.d/circe/lisp")

;; This defines the password variables below
(when (file-exists-p "~/.emacs.d/private.el")
(progn
(load-file "~/.emacs.d/private.el")
(setq circe-default-realname irc-real-name)))

(setq circe-ignore-list nil
circe-server-coding-system '(latin-1 . undecided)
circe-server-auto-join-channels
'(("" "#dev")))

(setq lui-max-buffer-size 30000)

(eval-after-load "circe"
'(progn
(require 'lui-irc-colors)
(add-to-list 'lui-pre-output-hook 'lui-irc-colors)
(add-to-list 'circe-receive-message-functions
	  'fc-got-something)))

(defun fc-got-something (nick user host command args)
;;	(beep)
)

(defun irc ()
  "Connect to IRC."
  (interactive)
  (circe "IRC"))

(setq whitespace-action '(auto-cleanup)) ;; automatically clean up bad whitespace
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)) ;; only show bad whitespace


;; try to make sure that we don't see any TAB characters introduced into files
(add-hook 'after-change-major-mode-hook 
  '(lambda () 
     (setq-default indent-tabs-mode nil)
     (setq c-basic-indent 2)
     (setq tab-width 2)))


;; auto-complete support

(add-to-list 'load-path "~/.emacs.d/popup-el")

(add-to-list 'load-path "~/.emacs.d/auto-complete/")
(require 'auto-complete-config)
(ac-config-default)

;; quicklisp support
(when (file-exists-p "~/quicklisp/slime-helper.el")
(progn
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")))

;; add flymake support for js
(add-to-list 'load-path "~/.emacs.d/flymake-easy")
(require 'flymake-easy)
(add-to-list 'load-path "~/.emacs.d/flymake-jslint")
(require 'flymake-jslint)
(add-hook 'js-mode-hook 'flymake-jslint-load)

