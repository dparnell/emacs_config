;; Set up the environment path
(if (string-equal "darwin" (symbol-name system-type))
    (progn
      (setenv "PATH" (concat "/usr/local/bin:/usr/local/sbin:" (getenv "PATH")))
      (setq exec-path (append exec-path '("/usr/local/bin" "/usr/local/sbin")))))

;; emacs22 setup
(if (< emacs-major-version 23)
    (setq load-path (append (append load-path '("~/.emacs.d/nxml/")) '("~/.emacs.d/ruby-mode"))))

;; make sure that if we open a frame on a terminal without graphical capabilities we don't set the background colour
(defun on-frame-open (frame)
  (if (not (display-graphic-p frame))
      (set-face-background 'default "unspecified-bg" frame)))
(on-frame-open (selected-frame))
(add-hook 'after-make-frame-functions 'on-frame-open)

;; solarized colour scheme
(message "Loading Solarized theme")
(if (< emacs-major-version 24)
    (progn
      (setq load-path (append (append load-path '("~/.emacs.d/color-theme")) '("~/.emacs.d/emacs-color-theme-solarized")))
      (require 'color-theme-solarized)
      (color-theme-solarized-dark))
  (progn
    (setq custom-theme-load-path (append custom-theme-load-path '("~/.emacs.d/emacs-color-theme-solarized")))
    (load-theme 'solarized-dark t)))

;; load powerline
(message "Loading powerline")
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
(message "Loading ruby mode")
(setq load-path (append load-path '("~/.emacs.d/ruby-mode")))

(message "Loading debug support")
(load-file "~/.emacs.d/cl-lib.el")

(setq rdebug-emacs-path (shell-command-to-string "which ruby > /dev/null && ruby -e \"puts File.join(File.dirname(File.dirname( Gem.bin_path('debugger', 'rdebug'))), 'emacs') rescue ''\""))
(if (not (equal "" rdebug-emacs-path))
    (progn
      (setq load-path (append load-path (list (substring rdebug-emacs-path 0 -1))))
      (require 'rdebug)))

;; Use spaces instead of TABs
(setq indent-tabs-mode nil)
(setq c-indent-level 2)

;; Magit
(message "Loading magit")
(setq load-path (append load-path '("~/.emacs.d/magit")))
(require 'magit)

;; Special rails file
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))

;; Web-mode
(message "Loading web-mode")
(setq load-path (cons (expand-file-name "~/.emacs.d/web-mode") load-path))
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.dtl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.?html?\\'" . web-mode))

;; load rails-reloaded
(message "Loading rails-reloaded")
(setq load-path (cons (expand-file-name "~/.emacs.d/rails-reloaded") load-path))
(require 'rails-autoload)

;; scala-mode
(if (or (< emacs-major-version 24) (and (= emacs-major-version 24) (< emacs-minor-version 2)))
    (progn
      (let ((path "~/.emacs.d/scala-mode"))
        (message "Loading scala-mode")
        (setq load-path (cons path load-path))
        (load "scala-mode-auto.el")
        (require 'scala-mode)))
  (progn
    (message "Loading scala-mode2")
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
(message "Loading rspec")
(setq load-path (append load-path '("~/.emacs.d/rspec-mode")))
(require 'rspec-mode)

;; minimap
(message "Loading minimap")
(setq load-path (append load-path '("~/.emacs.d/emacs-minimap")))
(require 'minimap)

;; Display the current date and time in the status bar
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

;; gforth
(message "Loading gforth")
(setq load-path (append load-path '("~/.emacs.d/gforth")))
(autoload 'forth-mode "gforth.el")
(autoload 'forth-block-mode "gforth.el")
(add-to-list 'auto-mode-alist '("\\.fs$" . forth-mode))

;; actionscript mode
(message "Loading actionscript-mode")
(autoload 'actionscript-mode "~/.emacs.d/actionscript-mode.el")
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))

;; start the emacs server
(message "Starting emacs server")
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
(message "Loading cucumber")
(add-to-list 'load-path "~/.emacs.d/cucumber.el")
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; Coffee script mode - Emacs 24 and higher required
(if (> emacs-major-version 23)
    (progn
	(message "Loading coffee-mode")
        (add-to-list 'load-path "~/.emacs.d/coffee-mode")
        (require 'coffee-mode)
        (custom-set-variables '(coffee-tab-width 2))
        (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
        (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))))

;; SCSS mode
(message "Loading scss-mode")
(add-to-list 'load-path (expand-file-name "~/.emacs.d/scss-mode"))
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)

;; Erlang mode
(message "Loading erlang-mode")
(add-to-list 'load-path (expand-file-name "~/.emacs.d/erlang-mode"))
(require 'erlang-start)

;; Yaml mode
(message "Loading yaml-mode")
(setq load-path (append load-path '("~/.emacs.d/yaml-mode")))
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; Textile mode
(message "Loading textile-mode")
(autoload 'textile-minor-mode "~/.emacs.d/textile-minor-mode.el")
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-minor-mode))

;; turn on mouse support in iTerm2
(message "Setting up iTerm2 mouse support")
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))

;; PHP support
(message "Loading php-mode")
(setq load-path (append load-path '("~/.emacs.d/php-mode")))
(require 'php-mode)

;; ecb
(message "Loading ecb")
(add-to-list 'load-path "~/.emacs.d/ecb")
(require 'ecb)

;; reformat the current file
;; stolen from here: http://emacsblog.org/2007/01/17/indent-whole-buffer/
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; circe
(message "Loading circe")
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
  ;;  (beep)
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
(message "Setting up auto-complete stuff")
(add-to-list 'load-path "~/.emacs.d/popup-el")
(add-to-list 'load-path "~/.emacs.d/auto-complete/")
(require 'auto-complete-config)
(ac-config-default)

;; SLIME
;; setup load-path and autoloads
(add-to-list 'load-path "~/.emacs.d/slime")
(require 'slime-autoloads)

;; Set your lisp system and, optionally, some contribs
(setq inferior-lisp-program "/usr/local/bin/sbcl"
      lisp-indent-function 'common-lisp-indent-function
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      slime-startup-animation t)
(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-banner slime-tramp slime-presentations slime-asdf))
(setq slime-protocol-version 'ignore)

;; quicklisp support
(when (file-exists-p "~/quicklisp/slime-helper.el")
  (progn
    (message "Loading quick-lisp support")
    (load (expand-file-name "~/quicklisp/slime-helper.el"))
    (setq inferior-lisp-program "sbcl")))

;; add flymake support for js
(message "Loading flymake-easy")
(add-to-list 'load-path "~/.emacs.d/flymake-easy")
(require 'flymake-easy)
(add-to-list 'load-path "~/.emacs.d/flymake-jslint")
(require 'flymake-jslint)
(add-hook 'js-mode-hook 'flymake-jslint-load)

;; add flymake suppport for Erlang
(defconst flymake-erlang-err-line-patterns
  '(("^\\(.*\.erl\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3)))

(defun flymake-erlang-command (filename)
  "Construct a command that flymake can use to check erlang source."
  (list (expand-file-name "~/.emacs.d/bin/check-erl") filename))

(defun flymake-erlang-load ()
  "Configure flymake mode to check the current buffer's erlang syntax."
  (interactive)
  (flymake-easy-load 'flymake-erlang-command
                     flymake-erlang-err-line-patterns
                     'inplace
                     "erl"))

(add-hook 'erlang-mode-hook 'flymake-erlang-load)

;; add flymake suppport for Coffeescript
(defconst flymake-coffeescript-err-line-patterns
  '(("^\\(.*\.coffee\\),\\([0-9]+\\),.*,\\(.*\\)$" 1 2 nil 3)))

(defun flymake-coffeescript-command (filename)
  "Construct a command that flymake can use to check coffeescript source."
  (list (expand-file-name "~/.emacs.d/bin/check-coffeescript") filename))

(defun flymake-coffeescript-load ()
  "Configure flymake mode to check the current buffer's coffeescript syntax."
  (interactive)
  (flymake-easy-load 'flymake-coffeescript-command
                     flymake-coffeescript-err-line-patterns
                     'inplace
                     "coffee"))

(add-hook 'coffee-mode-hook 'flymake-coffeescript-load)


(when (file-exists-p "~/.emacs.d/local-settings.el")
    (load-file "~/.emacs.d/local-settings.el"))

;; stop popups from breaking Emacs
(if (string-equal "darwin" (symbol-name system-type))
  (progn
    (defadvice yes-or-no-p (around prevent-dialog activate)
      "Prevent yes-or-no-p from activating a dialog"
      (let ((use-dialog-box nil))
        ad-do-it))
    (defadvice y-or-n-p (around prevent-dialog-yorn activate)
      "Prevent y-or-n-p from activating a dialog"
      (let ((use-dialog-box nil))
        ad-do-it))))
