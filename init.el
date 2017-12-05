(package-initialize)

(setq vemv/original-debugger #'debug)
(setq vemv/original-command-error-function #'command-error-default-function)
(setq vemv/original-minibuffer-message #'minibuffer-message)

(progn "Stuff that needs to be performed immediately, for a visually pleasant startup"

  (setq-default line-spacing 1) ;; NOTE: might mess up the echo area
  
  ;; set to the opposite of the initially desired value, since it will be toggled below
  (setq vemv/verbose-mode t)

  (defun vemv/toggle-verbosity ()
    (setq vemv/verbose-mode (not vemv/verbose-mode))
    (setq inhibit-startup-message (not vemv/verbose-mode))
    (setq inhibit-message (not vemv/verbose-mode)) ;; Silence minibuffer
    (setq debug-on-error vemv/verbose-mode)
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

(if (window-system) (set-face-attribute 'default nil :font vemv-font))

(require 'vemv.init)

(when (file-exists-p "~/.emacs.d.overrides/")
  (let ((default-directory "~/.emacs.d.overrides/"))
        (normal-top-level-add-subdirs-to-load-path))
  (require 'emacs.d.overrides))
