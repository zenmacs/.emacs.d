;;; -*- lexical-binding: t -*-

(setq lexical-binding t)

(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))

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
(require 'vemv-theme)
(setq custom-safe-themes '(default
                            ;; shas of vemv-theme.el as automaticaly computed by emacs:
                            "353ffc150b5271aa12c1e43b5367ace3dfe8b3bb7c6050325958a43618f30afc"))
(load-theme 'vemv)
(require 'vemv.hooks)
(require 'vemv.keyboard-init)
(unless vemv/terminal-emacs?
  (require 'vemv.undo)) ;; Shouldn't be needed, but otherwise I can't redo the first undo for some unknown reason
(require 'vemv.git)
(require 'ido-completing-read+)
(provide 'vemv.init)

(show-paren-mode 1)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(blink-cursor-mode -1)
(tooltip-mode (if vemv/use-eldoc-and-tooltips
                  1
                -1))
(smex-initialize)
(global-subword-mode)
(savehist-mode 1)

(unless vemv/terminal-emacs?
  (recentf-mode 1)
  (global-set-key [menu-bar File] nil) ;; b/c recentf-mode
  (global-company-mode)
  (menu-bar-mode)
  (yas-global-mode 1))

(defun prevent-whitespace-mode-for-magit ()
  (not (derived-mode-p 'magit-mode)))

(add-function :before-while whitespace-enable-predicate 'prevent-whitespace-mode-for-magit)

(global-auto-revert-mode t) ;; refresh buffers on external changes to the underlying files
(global-hl-line-mode t)
(make-variable-buffer-local 'global-hl-line-mode)
(vemv/global-whitespace-mode)
(cua-mode 1) ;; initialized after customizing cua-remap-control-v
(electric-indent-mode -1)

;; Disables mode-line tooltips, making `vemv/echo-clojure-source' more persistent
;; Not in vemv.setqs, something else would reset it later
(unless vemv/use-eldoc-and-tooltips
  (setq show-help-function nil))

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
