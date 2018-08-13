;;; -*- lexical-binding: t -*-

(setq lexical-binding t)

(require 'vemv.lang)
(require 'vemv.setqs)
(require 'vemv.project-explorer)
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
(require 'vemv.data.bindings)
(require 'vemv.theme)
(require 'vemv.hooks)
(require 'vemv.keyboard-init)
(require 'vemv.undo) ;; Shouldn't be needed, but otherwise I can't redo the first undo for some unknown reason
(provide 'vemv.init)

(show-paren-mode 1)
(recentf-mode 1)
(ido-mode 1)
(blink-cursor-mode -1)
(tooltip-mode -1)
(smex-initialize)
(global-subword-mode)
(global-company-mode)
(menu-bar-mode)
(global-auto-revert-mode t) ;; refresh buffers on external changes to the underlying files
(global-hl-line-mode t)
(make-variable-buffer-local 'global-hl-line-mode)
(global-whitespace-mode)
(yas-global-mode 1)
(cua-mode 1) ;; initialized after customizing cua-remap-control-v
(electric-indent-mode -1)

(shell-command-to-string "touch ~/.emacs.d/custom.el")
(load custom-file)

(delay (argless (if (window-system)
                    (set-face-attribute 'default nil :font vemv-font)))
       1)

(defun undo (&rest args)
  (interactive)
  (require 'undo-tree)
  (apply 'undo-tree-undo args))

(vemv/set-keys-for-scope :global vemv/global-key-bindings)
(vemv/set-keys-for-scope clojure-mode-map vemv/clojure-key-bindings)

(assert (eq (length vemv/available-projects)
            (length (-uniq vemv/available-projects))))

(vemv/open-files-from-last-session!)

(vemv/initial-layout
 (argless
  (vemv/next-file-buffer)
  (vemv/previous-file-buffer)

  ;; every 5 seconds. in practice, not so often b/c `vemv/refreshing-caches` (timestamp lock)
  ;; disabled until PE deemed stable again
  (comm delay (argless (run-with-timer 0 5 (argless
                                            (let ((w (selected-window)))
                                              (vemv/refresh-file-caches (argless (select-window w)))))))
        60)))
