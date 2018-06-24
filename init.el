(package-initialize)

(progn "Prevent dialogs asap, in case of error during initialization"

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

(progn "Stuff that needs to be performed immediately, for a visually pleasant startup"

  (setq-default line-spacing 1) ;; NOTE: might mess up the echo area
  
  (setq vemv/should-start-in-verbose-mode nil)
  
  (setq vemv/verbose-mode (not vemv/should-start-in-verbose-mode))

  (defun vemv/set-verbosity-to (v)
    (setq vemv/verbose-mode v)
    (setq inhibit-startup-message (not vemv/verbose-mode))
    (setq inhibit-message (not vemv/verbose-mode)) ;; Silence minibuffer
    
    (setq debug-on-error nil) ;; NOTE: overly verbose, so it's always set to nil. Set manually if needed. (proof of verbosity: try to undo in a pristine buffer, with this line uncommented)
    
    (setq debugger (if vemv/verbose-mode ;; Disable annoying *Backtrace* buffer
                     vemv/original-debugger
                     (lambda (&rest _))))
    (setq command-error-function (if vemv/verbose-mode ;; Silence "End of buffer" messages
                                    vemv/original-command-error-function
                                    (lambda (&rest _))))
    (defun minibuffer-message (&rest args) ;; Silence "No matching parenthesis found"
      (if vemv/verbose-mode
         (apply vemv/original-minibuffer-message args)
         nil)))
  
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

(setq vemv-font (if (eq system-type 'darwin) "Monaco-12" "DejaVu Sans Mono-13"))

(when (window-system)
    (set-face-attribute 'default nil :font vemv-font))

(setq vemv-home (getenv "HOME"))

(defun vemv-source (filename)
  (mapcar
   (lambda (x)
     (let* ((xy (s-split "=" (s-chop-prefix "+" x)))
            (x (car xy))
            (y (car (last xy))))
        (setenv (s-chop-prefix "declare -x " x)
                (s-replace "\"" "" y))))
   (-filter
    (lambda (x) (vemv/starts-with x "+"))
    (s-split
     "\n"
     (shell-command-to-string (concat "diff -u  <(true; export) <(source " filename "; export) | tail -n +4"))))))

(setenv "SHELL" "/bin/zsh")
(setenv "PATH" (concat (getenv "PATH") ":" vemv-home "/bin"))

(when (file-exists-p "~/.emacs.d.overrides/")
  (let ((default-directory "~/.emacs.d.overrides/"))
        (normal-top-level-add-subdirs-to-load-path)))

(require 'emacs.d.overrides)

(setq vemv/on-the-fly-projects nil)
(setq vemv/all-projects vemv/available-projects)

(require 'vemv.init)
