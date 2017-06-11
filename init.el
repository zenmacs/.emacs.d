
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(progn "Stuff that needs to be performed immediately, for a visually pleasant startup"

  (setq inhibit-startup-message t)
  ; (setq-default line-spacing 1) ; cute , but messes up the echo area
  ;; Silence minibuffer
  (setq inhibit-message t)
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

(setq gpm-using-nrepl t)
(setq vemv-home (getenv "HOME"))

(require 'vemv.init)
