(defun rdebug-connect ()
  "Start up the rdebug stuff connecting to a local ruby instance waiting for a debugger"
;  (interactive
;   (let ((init (buffer-file-name)))
;     (setq init (and init
;                     (file-name-nondirectory init)))
;     (list (gud-query-cmdline 'rdebug init))))

  (rdebug-debug-enter "rdebug"
    (rdebug-set-window-configuration-state 'debugger t)
    ;; Parse the command line and pick out the script name and whether
    ;; --annotate has been set.
    (let* ((target-name "connect")
           (annotate-p (cadr script-name-annotate-p))
           (cmd-buffer-name (format "rdebug-cmd-%s" target-name))
           (rdebug-cmd-buffer-name (format "*%s*" cmd-buffer-name))
           (rdebug-cmd-buffer (get-buffer rdebug-cmd-buffer-name))
           (gud-chdir-before-run nil))

      ;; `gud-rdebug-massage-args' needs whole `command-line'.
      ;; command-line is refered through dynamic scope.
      (rdebug-common-init cmd-buffer-name rdebug-cmd-buffer target-name
                            "rdebug" "-c"
                              'gud-rdebug-marker-filter
                                'gud-rdebug-find-file)
      (setq comint-process-echoes t)

      (setq rdebug-inferior-status "running")

      (rdebug-command-initialization)

      ;; Setup exit callback so that the original frame configuration
      ;; can be restored.
      (let ((process (get-buffer-process gud-comint-buffer)))
        (when process
          (unless (equal rdebug-line-width 120)
                (gud-call (format "set width %d" rdebug-line-width)))
          (set-process-sentinel process
                                'rdebug-process-sentinel)))

      ;; Add the buffer-displaying commands to the Gud buffer,
      ;; FIXME: combine with code in rdebug-track.el; make common
      ;; command buffer mode map.
      (let ((prefix-map (make-sparse-keymap)))
        (define-key (current-local-map) gud-key-prefix prefix-map)
        (define-key prefix-map "t" 'rdebug-goto-traceback-line)
        (define-key prefix-map "!" 'rdebug-goto-dollarbang-traceback-line)
        (rdebug-populate-secondary-buffer-map-plain prefix-map))

      (rdebug-populate-common-keys (current-local-map))
      (rdebug-populate-debugger-menu (current-local-map))

      (setq comint-prompt-regexp (concat "^" rdebug-input-prompt-regexp))
      (setq paragraph-start comint-prompt-regexp)

      (setcdr (assq 'rdebug-debugger-support-minor-mode minor-mode-map-alist)
              rdebug-debugger-support-minor-mode-map-when-active)
      (when rdebug-many-windows
        (rdebug-setup-windows-initially))

      (run-hooks 'rdebug-mode-hook))))

(provide 'rdebug-remote)

