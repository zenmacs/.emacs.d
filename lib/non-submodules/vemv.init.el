;;; -*- lexical-binding: t -*-

(setq lexical-binding t)

(require 'vemv.lang)
(require 'vemv.setqs)
(require 'vemv.window-system)
(require 'vemv.open)
(require 'vemv.buffer-querying)
(require 'vemv.clojure-interaction)
(require 'vemv.paredit)
(require 'vemv.edit)
(require 'vemv.search)
(require 'vemv.mode-line)
(require 'vemv.project-interaction)
(require 'vemv.project)
(require 'vemv.workspace)
(require 'vemv.data)
(unless vemv/terminal-emacs?
  (require 'vemv.data.bindings))
(require 'vemv.theme)
(require 'vemv.hooks)
(require 'vemv.keyboard-init)
(unless vemv/terminal-emacs?
  (require 'vemv.undo)) ;; Shouldn't be needed, but otherwise I can't redo the first undo for some unknown reason
(provide 'vemv.init)

(show-paren-mode 1)
(ido-mode 1)
(blink-cursor-mode -1)
(tooltip-mode -1)
(smex-initialize)
(global-subword-mode)
(savehist-mode 1)

(unless vemv/terminal-emacs?
  (recentf-mode 1)
  (global-company-mode)
  (menu-bar-mode)
  (yas-global-mode 1)
  (shell-command-to-string "touch ~/.emacs.d/custom.el")
  (load custom-file))

(global-auto-revert-mode t) ;; refresh buffers on external changes to the underlying files
(global-hl-line-mode t)
(make-variable-buffer-local 'global-hl-line-mode)
(global-whitespace-mode)
(cua-mode 1) ;; initialized after customizing cua-remap-control-v
(electric-indent-mode -1)

;; Disables mode-line tooltips, making `vemv/echo-clojure-source' more persistent
;; Not in vemv.setqs, something else would reset it later
(setq show-help-function nil)

(when (window-system)
  (delay (argless
          (set-face-attribute 'default nil :font vemv-font))
         1))

(defun undo (&rest args)
  (interactive)
  (require 'undo-tree)
  (apply 'undo-tree-undo args))

(assert (eq (length vemv/available-projects)
            (length (-uniq vemv/available-projects))))

(unless vemv/terminal-emacs?

  (vemv/set-keys-for-scope :global vemv/global-key-bindings)

  (vemv/open-files-from-last-session!)

  (vemv/initial-layout
   (argless
    (vemv/next-file-buffer)
    (vemv/previous-file-buffer)

    ;; every 5 seconds. in practice, not so often b/c `vemv/refreshing-caches` (timestamp lock)
    ;; disabled until PE deemed stable again
    (comm delay (argless (run-with-timer 0 5 (argless
                                              (let ((w (selected-window)))
                                                (vemv/refresh-file-caches (argless (vemv/safe-select-window w)))))))
          60))))
