;;; -*- lexical-binding: t -*-

(setq lexical-binding t)

(require 'vemv.lang)
(require 'vemv.project-explorer)
(require 'vemv.window-system)
(require 'vemv.clojure-interaction)
(require 'vemv.paredit)
(require 'vemv.mode-line)
(require 'vemv.helm)
(require 'vemv.project)
(require 'vemv.workspace)
(require 'vemv.data)
(require 'vemv.data.bindings)
(require 'vemv.theme)
(require 'vemv.setqs)
(require 'vemv.hooks)
(require 'vemv.keyboard-init)
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
(global-whitespace-mode)
(cua-mode 1) ;; initialized after customizing cua-remap-control-v

(vemv/initial-layout)

(shell-command-to-string "touch ~/.emacs.d/custom.el")
(load custom-file)

(delay 'vemv/clojure-init 1)

(delay (argless (if (window-system)
                    (set-face-attribute 'default nil :font vemv-font)))
       1)

(defun undo (&rest args)
  (interactive)
  (apply 'undo-tree-undo args))

(delay (argless (setq vemv/project-explorer-initialized t))
       12)

;; every 5 seconds. in practice, not so often b/c `vemv/refreshing-caches` (timestamp lock)
(delay (argless (run-with-timer 0 5 'vemv/refresh-file-caches))
       60)

(vemv/set-keys-for-scope :global vemv/global-key-bindings)

(vemv/set-keys-for-scope clojure-mode-map vemv/clojure-key-bindings)

(vemv/set-keys-for-scope ruby-mode-map vemv/ruby-key-bindings)

(require 'vemv.desktop)
