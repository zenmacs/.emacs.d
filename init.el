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

    (setq vemv/should-start-in-verbose-mode nil)

    (setq vemv/verbose-mode (not vemv/should-start-in-verbose-mode))

    (defun vemv/set-verbosity-to (v)
      (setq vemv/verbose-mode v)
      (setq inhibit-startup-message (not vemv/verbose-mode))
      (setq inhibit-message (not vemv/verbose-mode)) ;; Silence minibuffer

      (unless vemv/should-start-in-verbose-mode ;; allow debugging issues with `kill -USR2 $emacs_pid`

        ;; NOTE: overly verbose, so it's always set to nil. Set with M-x toggle-debug-on-error if needed.
        ;; (proof of verbosity: try to undo in a pristine buffer, with this line uncommented)
        (setq debug-on-error nil)

        (setq debugger (if vemv/verbose-mode ;; Disable annoying *Backtrace* buffer
                           vemv/original-debugger
                           (lambda (&rest _))))
        (setq command-error-function (if vemv/verbose-mode ;; Silence "End of buffer" messages
                                         vemv/original-command-error-function
                                         (lambda (&rest _))))
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

  (let ((default-directory "~/.emacs.d.overrides/"))
    (assert (file-exists-p default-directory)
            nil
            (concat default-directory " not found. Please follow the readme here: https://github.com/vemv/.emacs.d"))
    (normal-top-level-add-subdirs-to-load-path))

  (defmacro vemv/verbosely (&rest forms)
    `(let ((old vemv/verbose-mode))
       (vemv/set-verbosity-to t)
       ,@forms
       (vemv/set-verbosity-to old)))

  (defun vemv/apply-verbosely (f &rest args)
    (vemv/verbosely
     (apply f args)))

  (require 'vemv.lang.core)
  (require 'vemv.packages)
  (require 'emacs.d.overrides)
  (require 'desktop)
  (require 'vemv.desktop)
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
  (let ((ordered (vemv.desktop/ordered-workspaces-list)))
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

(defun display-startup-echo-area-message ())

(message (concat "\n------------------------------ Emacs loaded in: " (emacs-init-time) " ------------------------------\n"))
