;; These two reduce `(emacs-init-time)`:

(setq vemv/terminal-emacs? (getenv "TERMINAL_EMACS"))

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

    (setq vemv/should-start-in-verbose-mode nil)

    (setq vemv/verbose-mode (not vemv/should-start-in-verbose-mode))

    ;; set globally (this seems the best way - a plain `setq` won't work)
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

  (setq vemv-home (getenv "HOME"))

  (setenv "SHELL" "/bin/zsh")
  (setenv "PATH" (concat (getenv "PATH") ":" vemv-home "/bin"))

  (require 'cl) ;; for assert

  (unless vemv/terminal-emacs?
    (let ((default-directory "~/.emacs.d.overrides/"))
      (assert (file-exists-p default-directory)
              nil
              (concat default-directory " not found. Please follow the readme here: https://github.com/vemv/.emacs.d"))
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
  (unless vemv/terminal-emacs?
    (require 'emacs.d.overrides)
    (require 'desktop)
    (require 'vemv.desktop))
  (require 'dash)
  (require 's)

  (defun vemv-source (filename)
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

  (defun vemv/set-available-projects! ()
    (setq vemv/available-projects (-mapcat 'second vemv/available-workspaces))
    (setq vemv/on-the-fly-projects (filter (lambda (x)
                                             (not (member x vemv/available-projects)))
                                           vemv/on-the-fly-projects)))

  (vemv/set-available-projects!)
  (setq cider-launched nil)
  (setq vemv-cider-connecting nil)
  (setq vemv-cider-connected nil)
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
