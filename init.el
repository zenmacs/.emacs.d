(setq vemv/terminal-emacs? (getenv "TERMINAL_EMACS"))

(defvar vemv/input-enabled t)

;; These two reduce `(emacs-init-time)`:
(let ((file-name-handler-alist nil)
      (gc-cons-threshold (* 200 1000 1000)))

  (progn
    "Prevent dialogs asap, in case of error during initialization"

    (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
      "Prevent annoying \"Active processes exist\" query when you quit Emacs."
      (cl-letf (((symbol-function #'process-list) (lambda ())))
        ad-do-it))

    ;; http://www.emacswiki.org/emacs/BackupDirectory
    (setq backup-by-copying t
          delete-old-versions t
          kept-new-versions 6
          kept-old-versions 2
          version-control t))

  (setq vemv/original-debugger #'debug)
  (setq vemv/original-command-error-function #'command-error-default-function)
  (setq vemv/original-minibuffer-message #'minibuffer-message)

  (progn
    "Stuff that needs to be performed immediately, for a visually pleasant startup"

    (setq-default line-spacing 1) ;; NOTE: might mess up the echo area

    (setq-default frame-title-format '("%b"))

    (setq vemv/should-start-in-verbose-mode nil)

    (setq vemv/verbose-mode (not vemv/should-start-in-verbose-mode))

    ;; set globally (this seems the best way - a plain `setq` won't work)
    ;; XXX shouldn't be set on --debug-init
    (if vemv/should-start-in-verbose-mode
        (call-interactively 'toggle-debug-on-error))

    (defun vemv/set-verbosity-to (v)
      "Sets `vemv/verbose-mode', and disables `debug-on-error' unconditionally.
Set `debug-on-error' with M-x toggle-debug-on-error if needed."
      (setq vemv/verbose-mode v)
      (setq inhibit-startup-message (not vemv/verbose-mode))
      (setq inhibit-message (not vemv/verbose-mode)) ;; Silence minibuffer

      (unless vemv/should-start-in-verbose-mode ;; allow debugging issues with `kill -USR2 $emacs_pid`

        ;; NOTE: overly verbose, so it's always set to nil.
        ;; (proof of verbosity: try to undo in a pristine buffer, with this line uncommented)
        (setq debug-on-error nil)
        (setq-default debug-on-error debug-on-error)


        (setq debugger (if vemv/verbose-mode
                           vemv/original-debugger
                         (lambda (&rest args)
                           (->> (backtrace)
                                with-output-to-string
                                s-lines
                                (-drop 23)
                                (s-join "\n")
                                (concat "Found exception: " (pr-str (second args)) "\n")
                                (message)))))
        (setq-default debugger debugger)

        (setq command-error-function (if vemv/verbose-mode ;; Silence "End of buffer" messages
                                         vemv/original-command-error-function
                                       (lambda (&rest _))))
        (setq-default command-error-function command-error-function)

        (defun minibuffer-message (&rest args) ;; Silence "No matching parenthesis found"
          (if vemv/verbose-mode
              (apply vemv/original-minibuffer-message args)
            nil))))

    (defun vemv/toggle-verbosity ()
      (vemv/set-verbosity-to (not vemv/verbose-mode)))

    (vemv/toggle-verbosity)

    (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
    (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
    (if (fboundp 'menu-bar-mode) (menu-bar-mode -1)))

  (let ((default-directory "~/.emacs.d/lib"))
    (normal-top-level-add-subdirs-to-load-path))

  (setq vemv-home (getenv "HOME"))

  (setq vemv/overrides-directory (concat vemv-home "/.emacs.d.overrides/"))
  (setq vemv/overrides-forks-directory (concat vemv/overrides-directory "forks/"))
  (setq vemv/overrides-lib-directory (concat vemv/overrides-directory "lib/"))
  (setq vemv/overrides-file (concat vemv/overrides-lib-directory "emacs.d.overrides.el")) []
  (setq vemv/overrides-project-file (concat vemv/overrides-lib-directory "vemv.project.overrides.el"))
  (setq vemv/emacs-project-file (concat vemv/overrides-lib-directory "vemv.project.emacs.el"))

  (defun vemv/possibly-create-overrides-dir! ()
    (unless (file-exists-p vemv/overrides-directory)
      (make-directory vemv/overrides-directory))
    (unless (file-exists-p vemv/overrides-lib-directory)
      (make-directory vemv/overrides-lib-directory))
    (unless (file-exists-p vemv/overrides-forks-directory)
      (make-directory vemv/overrides-forks-directory))
    (unless (file-exists-p vemv/overrides-file)
      (shell-command-to-string (concat "cp ~/.emacs.d/templates/emacs.overrides.el " vemv/overrides-file)))
    (unless (file-exists-p vemv/overrides-project-file)
      (shell-command-to-string (concat "cp ~/.emacs.d/templates/vemv.project.overrides.el " vemv/overrides-project-file)))
    (unless (file-exists-p vemv/emacs-project-file)
      (shell-command-to-string (concat "cp ~/.emacs.d/templates/vemv.project.emacs.el " vemv/emacs-project-file))))

  (vemv/possibly-create-overrides-dir!)

  ;; the ~/.emacs.d.overrides/forks directory allows one to add custom copies of built-in .el libraries,
  ;; allowing otherwise impossible changes, or ensuring one's patches (defadvice, etc) don't go stale.
  (add-to-list 'load-path vemv/overrides-forks-directory)

  (when (eq system-type 'darwin)
    (global-set-key (kbd "C-q") 'save-buffers-kill-emacs) ;; (redundantly) set C-q, in case of failure during init.el load
    (setq mac-control-modifier 'super)
    (setq mac-option-modifier 'meta)
    (setq mac-command-modifier 'control))

  (setq vemv-font (if (eq system-type 'darwin)
                      "Monaco-12"
                    "DejaVu Sans Mono-13"))

  (when (window-system)
    (set-face-attribute 'default nil :font vemv-font))

  (setenv "SHELL" "/bin/zsh")

  (require 'cl) ;; for assert

  (unless vemv/terminal-emacs?
    (let ((default-directory vemv/overrides-directory))
      (assert (file-exists-p default-directory))
      (normal-top-level-add-subdirs-to-load-path)))

  (defmacro vemv/verbosely (&rest forms)
    `(let* ((old vemv/verbose-mode)
            (_ (vemv/set-verbosity-to t))
            (v (progn
                 ,@forms)))
       (vemv/set-verbosity-to old)
       v))

  (defun vemv/apply-verbosely (f &rest args)
    (vemv/verbosely
     (apply f args)))

  (require 'vemv.lang.core)
  (require 'vemv.packages)
  (require 'dash)
  (require 's)

  (defun vemv-source (filename)
    (assert (file-exists-p filename))
    (->> (concat "diff -u  <(true; export) <(source " filename "; export) | tail -n +4")
         (shell-command-to-string)
         (s-split "\n")
         (-filter (lambda (x)
                    (vemv/starts-with x "+")))
         (mapcar (lambda (x)
                   (let* ((xy (s-split "=" (s-chop-prefix "+" x)))
                          (x (car xy))
                          (y (car (last xy))))
                     (setenv (s-chop-prefix "declare -x " x)
                             (s-replace "\"" "" y)))))))

  (unless vemv/terminal-emacs?
    (require 'emacs.d.overrides)
    (require 'desktop)
    (require 'vemv.desktop))

  (defun zero? (n) (and n (eq 0 n)))
  (defun filter (p c) (-filter p c))

  (defun vemv/sort-list-by-list (target-list criterion-list)
    (-sort (lambda (a b)
             (< (or (-elem-index a criterion-list) 99999999)
                (or (-elem-index b criterion-list) 99999999)))
           target-list))

  (defun vemv/sort-car-by-car (target-list criterion-list)
    (let ((crit (mapcar 'car criterion-list)))
      (-sort (lambda (a b)
               (< (or (-elem-index (car a) crit) 99999999)
                  (or (-elem-index (car b) crit) 99999999)))
             target-list)))

  (when vemv/terminal-emacs?
    (setq vemv/available-workspaces nil))
  (setq vemv/on-the-fly-projects nil)

  (defun vemv/create-missing-project-files! ()
    (interactive)
    (dolist (project (-difference vemv/available-projects vemv/on-the-fly-projects))
      (let* ((prefix "vemv.project.")
             (f (concat vemv-home "/.emacs.d.overrides/lib/" prefix project ".el")))
        (unless (file-exists-p f)
          (with-current-buffer (find-file-noselect f)
            (insert "(provide '" prefix project  ")\n")
            (save-buffer))))))

  (defun vemv/set-available-projects! ()
    (setq vemv/available-projects (-mapcat 'second vemv/available-workspaces))
    (setq vemv/on-the-fly-projects (filter (lambda (x)
                                             (not (member x vemv/available-projects)))
                                           vemv/on-the-fly-projects))
    (vemv/create-missing-project-files!))

  (vemv/set-available-projects!)
  (setq cider-launched nil)
  (setq vemv-cider-connecting nil)
  (setq vemv-cider-connected nil)
  (setq vemv-robe-connecting nil)
  (setq vemv-robe-connected nil)
  (setq vemv/running-project nil)
  (setq vemv/running-project-root-dir nil)
  (setq vemv/running-project-type nil)
  (let ((ordered (unless vemv/terminal-emacs? (vemv.desktop/ordered-workspaces-list))))
    (->> ordered
         (vemv/sort-car-by-car vemv/available-workspaces)
         (mapcar (lambda (w)
                   (let* ((ws (car w))
                          (projs (second w))
                          (this (second (-first (lambda (_ws)
                                                  (equal (car _ws) ws))
                                                ordered))))
                     (list ws (vemv/sort-list-by-list projs this)))))
         (setq vemv/all-workspaces)))
  (setq vemv/current-workspace (car vemv/all-workspaces))
  (setq vemv/current-project (car (second vemv/current-workspace)))

  (require 'vemv.init))

(when vemv/terminal-emacs?
  (menu-bar-mode -1)
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
  (setq mouse-sel-mode t))

(defun display-startup-echo-area-message ())

(message (concat "\n------------------------------ Emacs loaded in: " (emacs-init-time) " ------------------------------\n"))
