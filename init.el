(progn "Stuff that needs to be performed immediately, for a visually pleasant startup"

  (setq inhibit-startup-message t)
  ; (setq-default line-spacing 1) ; cute , but messes up the echo area
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1)))

(let ((default-directory "~/.emacs.d/lib"))
      (normal-top-level-add-subdirs-to-load-path))

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'control))

(if (window-system) (set-face-attribute 'default nil :font "DejaVu Sans Mono-13"))

(require 'vemv.init)
