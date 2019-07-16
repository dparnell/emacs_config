; Set up the environment path

(if (string-equal "darwin" (symbol-name system-type))
    (progn
      (setenv "PATH" (concat "/usr/local/bin:/usr/local/sbin:" (getenv "PATH")))
      (setq exec-path (append exec-path '("/usr/local/bin" "/usr/local/sbin")))))

(if (not (getenv "LANG"))
    (setenv "LANG" "en_AU.UTF-8"))

;; add in some keyboard mappings

(global-set-key (kbd "M-b") nil)
(global-set-key (kbd "M-b M-b") 'buffer-menu)
(global-set-key (kbd "M-b <up>") 'windmove-up)
(global-set-key (kbd "M-b <down>") 'windmove-down)
(global-set-key (kbd "M-b <left>") 'windmove-left)
(global-set-key (kbd "M-b <right>") 'windmove-right)

;; recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
;; save the recect files every now and again
(run-at-time nil (* 5 60) 'recentf-save-list)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(setq auto-package-update-delete-old-versions t)

(message "Loading required packages")
(package-initialize)
(let ((is-emacs-24-4-or-greater (or (> emacs-major-version 24) (and (= emacs-major-version 24) (> emacs-minor-version 3)))))
  (let* ((packages-for-emacs-24-4-or-greater (if is-emacs-24-4-or-greater (list 'auto-package-update 'cider 'magit 'flycheck 'flycheck-clojure 'elixir-mode 'scala-mode 'clojure-mode 'swiper 'lsp-mode 'lsp-ui 'company-lsp 'lsp-elixir) (list)))
         (packages-for-emacs-24-or-greater (if (> emacs-major-version 23) (list 'coffee-mode 'company 'yasnippet 'flymake-easy 'flymake-jslint)
                                             (list)))
         (common-packages (list 'iedit 'wgrep 'web-mode 'scss-mode 'yaml-mode 'json-mode 'js2-mode 'slime 'circe 'dockerfile-mode 'feature-mode 'ecb 'markdown-mode 'php-mode 'typescript-mode))
         (to-install (delq nil (mapcar (lambda (x) (if (package-installed-p x) nil x)) (delq nil (append common-packages packages-for-emacs-24-or-greater packages-for-emacs-24-4-or-greater))))))

    (if to-install
        (progn
          (switch-to-buffer "*Message-Log*")
          (message "There are missing packages: %s" to-install)
          (package-refresh-contents)
          (mapcar (lambda (x) (message "Installing package %s" (symbol-name x)) (package-install x)) to-install))
      (message "All packaged are already installed")))

  ;; emacs22 setup
  (if (< emacs-major-version 23)
      (setq load-path (append (append load-path '("~/.emacs.d/nxml/")) '("~/.emacs.d/ruby-mode"))))

  ;; solarized colour scheme
  (message "Loading Solarized theme")
  (defun load-my-theme ()
    (if (< emacs-major-version 24)
        (progn
          (setq load-path (append (append load-path '("~/.emacs.d/color-theme")) '("~/.emacs.d/emacs-color-theme-solarized")))
          (require 'color-theme-solarized)
          (color-theme-solarized-dark))
      (progn
        (setq custom-theme-load-path (append custom-theme-load-path '("~/.emacs.d/emacs-color-theme-solarized")))
        (load-theme 'solarized-dark t)
        (message "Reloaded theme"))))

  (load-my-theme)

  (defvar reload-theme-on-text-terminals t)
  ;; make sure that if we open a frame on a terminal without graphical capabilities we don't set the background colour
  (defun on-frame-open (frame)
    (if (not (display-graphic-p frame))
        (if reload-theme-on-text-terminals
            (progn
              (message "Reloading theme")
              (run-with-idle-timer 1 nil 'load-my-theme))
          (progn
            (message "Clearing out the background colour")
            (set-face-background 'default "unspecified-bg" frame)))

      (tool-bar-mode -1)))

  (on-frame-open (selected-frame))
  (add-hook 'after-make-frame-functions 'on-frame-open)

  ;; install swiper and ivy
  (if (> emacs-major-version 23)
      (progn
        (global-set-key "\C-s" 'swiper)
        (ivy-mode)))

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

  ;;(if (not (string-equal "windows-nt" (symbol-name system-type)))
  ;;  (progn
  ;;    (setq rdebug-emacs-path (shell-command-to-string "which ruby > /dev/null && ruby -e \"puts File.join(File.dirname(File.dirname( Gem.bin_path('debugger', 'rdebug'))), 'emacs') rescue ''\""))
  ;;    (if (not (equal "" rdebug-emacs-path))
  ;; (progn
  ;;   (setq load-path (append load-path (list (substring rdebug-emacs-path 0 -1))))
  ;;   (require 'rdebug)
  ;;   (require 'rdebug-remote)))))

  ;; Use spaces instead of TABs
  (setq indent-tabs-mode nil)
  (setq c-indent-level 2)

  ;; js2 mode
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

  ;; Special rails file
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))

  ;; Web-mode
  (message "Loading web-mode")
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
  (add-to-list 'auto-mode-alist '("\\.html.eex?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

  ;; load rails-reloaded
  (message "Loading rails-reloaded")
  (setq load-path (cons (expand-file-name "~/.emacs.d/rails-reloaded") load-path))
  (require 'rails-autoload)

  ;; scala-mode
  (if (or (< emacs-major-version 24) (and (= emacs-major-version 24) (< emacs-minor-version 4)))
      (progn
        (let ((path "~/.emacs.d/scala-mode-old"))
          (message "Loading scala-mode")
          (setq load-path (cons path load-path))
          (load "scala-mode-auto.el")
          (require 'scala-mode)))
    (progn
      (message "Loading scala-mode")
      (require 'scala-mode)))

  (defun scala-turnoff-indent-tabs-mode ()
    (setq indent-tabs-mode nil))

  ;; scala mode hooks
  (add-hook 'scala-mode-hook 'scala-turnoff-indent-tabs-mode)

  ;; Scala stuff
  (add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))

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
  (require 'feature-mode)
  (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

  ;; Coffee script mode - Emacs 24 and higher required
  (if (> emacs-major-version 23)
      (progn
        (message "Loading coffee-mode")
        (custom-set-variables '(coffee-tab-width 2))
        (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
        (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))))

  ;; SCSS mode
  (message "Loading scss-mode")
  (autoload 'scss-mode "scss-mode")
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
  (setq scss-compile-at-save nil)

  ;; Erlang mode
  (message "Loading erlang-mode")
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/erlang-mode"))
  (require 'erlang-start)

  ;; Yaml mode
  (message "Loading yaml-mode")
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
  (require 'php-mode)

  ;; ecb
  (message "Loading ecb")
  (require 'ecb)

  ;; reformat the current file
  ;; stolen from here: http://emacsblog.org/2007/01/17/indent-whole-buffer/
  (defun iwb ()
    "indent whole buffer"
    (interactive)
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max)))

  (defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

  ;; circe
  (message "Loading circe")
  (autoload 'circe "circe" "Connect to an IRC server" t)

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
      (add-to-list 'lui-pre-output-hook 'lui-irc-colors)))

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

  ;; SLIME
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
      (load (expand-file-name "~/quicklisp/slime-helper.el"))))

  (if is-emacs-24-4-or-greater
      (progn
        ;; (global-flycheck-mode)

        ;; add in LSP
        (require 'lsp)
        (require 'lsp-clients)
        (add-hook 'js2-mode-hook #'lsp)

        (require 'lsp-mode)
        (require 'lsp-ui)
        (add-hook 'lsp-mode-hook 'lsp-ui-mode)

        ;; (require 'lsp-elixir)
        (add-hook 'elixir-mode-hook #'lsp)

        (setq lsp-ui-sideline-update-mode 'point)

        (when (or (file-exists-p "/usr/local/bin/javascript-typescript-stdio") (file-exists-p "/usr/bin/javascript-typescript-stdio"))
          (require 'lsp-mode)
          (require 'typescript-mode)

          (defun lsp-javascript-typescript--render-string (str)
            (ignore-errors
              (with-temp-buffer
                (typescript-mode)
                (insert str)
                (font-lock-ensure)
                (buffer-string))))

          (defun lsp-javascript-typescript--initialize-client (client)
            (lsp-provide-marked-string-renderer
             client "typescript" 'lsp-javascript-typescript--render-string)
            (lsp-provide-marked-string-renderer
             client "javascript" 'lsp-javascript-typescript--render-string))

          ;; (add-hook 'js-mode-hook #'lsp-javascript-typescript-enable)
          (add-hook 'typescript-mode-hook #'lsp-javascript-typescript-enable))

        (when (file-exists-p "/usr/bin/vls")
          (progn

            (defun lsp-vue--vetur-configuration (features)
              "Get all features configuration."
              (cl-labels ((dotted-p (x) (not (consp (cdr x))))
                          (walklist
                              (l table)
                            (let (
                                  (key (car l))
                                  (value (cdr l))
                                  (localtable (or table (make-hash-table :test 'equal))))

                              (if (listp key)
                                  (dolist (sublist l)
                                    (walklist sublist localtable))

                                (progn
                                  (puthash key (or (gethash key localtable) (make-hash-table :test 'equal)) localtable)
                                  (if (not (dotted-p l))
                                      (puthash key (walklist value (gethash key localtable)) localtable)
                                    (puthash key value localtable))))

                              localtable)))
                (let ((table (make-hash-table :test 'equal)))
                  (dolist (feature features)
                    (walklist
                     (mapcar
                      #'(lambda (form)
                          (let* ((custom (first form))
                                 (path (split-string (symbol-name custom) "\\.")))
                            (append path (symbol-value custom))))
                      (get feature 'custom-group)) table))
                  table)))

            (defun lsp-vue--set-configuration ()
              "Send project config to lsp-server"
              (lsp--set-configuration (lsp-vue--vetur-configuration '(vetur html))))

            (add-hook 'lsp-after-initialize-hook 'lsp-vue--set-configuration)

            ;; (add-hook 'find-file-hook
            ;;           (lambda ()
            ;;             (when (string= (file-name-extension buffer-file-name) "vue")
            ;;               (lsp-vue-enable))))
            ))

        ;; set up magit colours
        (custom-set-faces
         ;; other faces
         '(magit-diff-added ((((type tty)) (:foreground "green"))))
         '(magit-diff-added-highlight ((((type tty)) (:foreground "LimeGreen"))))
         '(magit-diff-context-highlight ((((type tty)) (:foreground "default"))))
         '(magit-diff-file-heading ((((type tty)) nil)))
         '(magit-diff-removed ((((type tty)) (:foreground "red"))))
         '(magit-diff-removed-highlight ((((type tty)) (:foreground "IndianRed"))))
         '(magit-section-highlight ((((type tty)) nil)))))
    (progn
      ;; add flymake support for js
      (message "Loading flymake-easy")
      (require 'flymake-easy)
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
                           "coffee")))

    (add-hook 'coffee-mode-hook 'flymake-coffeescript-load))

  ;; load rainbow delimiters
  (message "Loading rainbow delimiters")
  (load "~/.emacs.d/rainbow-delimiters.el")

  ;; load paredit
  (message "Loading paredit")
  (load "~/.emacs.d/paredit.el")
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

  ;; google protobuf mode
  (message "Loading protobuf-mode")
  (load "~/.emacs.d/protobuf-mode.el")
  (add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

  (defun lets-edit-some-lisp ()
    (enable-paredit-mode)
    (rainbow-delimiters-mode)
    (show-paren-mode))

  ;; now add on the hooks to set up for lisp editing
  (add-hook 'emacs-lisp-mode-hook       #'lets-edit-some-lisp)
  (add-hook 'eval-expression-minibuffer-setup-hook #'lets-edit-some-lisp)
  (add-hook 'ielm-mode-hook             #'lets-edit-some-lisp)
  (add-hook 'lisp-mode-hook             #'lets-edit-some-lisp)
  (add-hook 'lisp-interaction-mode-hook #'lets-edit-some-lisp)
  (add-hook 'scheme-mode-hook           #'lets-edit-some-lisp)

  ;; add dockerfile-mode
  (add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-mode))

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

  ;; add company-mode
  (message "Adding company mode")
  ;; don't downcase company-mode completions
  (setq company-dabbrev-downcase 0)
  (defun setup-company-mode ()
    (global-company-mode)

    (message "Adding company LSP")
    (require 'company-lsp)
    (push 'company-lsp company-backends)

    ;; fix company-mode css regex
    (defconst company-css-property-value-regexp
      "\\_<\\([[:alpha:]-]+\\):\\(?:[[:space:]]+\\)?\\([^{};]*\\_>\\|\\)\\="
      "A regular expression matching CSS tags."))

  (add-hook 'after-init-hook 'setup-company-mode)

  ;; turn on yasnippets
  (yas-global-mode 1)
  (global-set-key (kbd "C-c y") 'company-yasnippet)

  ;; add markdown mode
  (message "Adding markdown mode")
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

  ;; load any local settings
  (when (file-exists-p "~/.emacs.d/local-settings.el")
    (message "Loading local settings")
    (load-file "~/.emacs.d/local-settings.el"))

  ;; finally load any customizations
  (setq custom-file "~/.emacs.d/custom.el")
  (when (file-exists-p "~/.emacs.d/custom.el")
    (message "Loading custom settings")
    (load custom-file)))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
