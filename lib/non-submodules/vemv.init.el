;;; -*- lexical-binding: t -*-

(setq lexical-binding t)

(require 'vemv.lang)
(require 'vemv.project)
(require 'vemv.workspace)
(require 'vemv.data)
(require 'vemv.data.bindings)
(require 'vemv.theme)
(require 'vemv.setqs)
(require 'vemv.hooks)
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

(global-set-key (kbd "M-x") 'smex)

;; Important - remove keybindings before (vemv/initial-layout) so M-x cannot interrupt

(dolist (mode (list paredit-mode-map comint-mode-map undo-tree-map cider-mode-map))
  (mapc (lambda (arg)
          (define-key mode (vemv/keyboard-macro arg) nil))
        vemv/exhaustive-list-of-bindings-to-remove))

(dolist (key vemv/key-bindings-to-remove)
  (global-unset-key key))

(dolist (key vemv/key-bindings-to-dummy)
  (global-set-key key (argless)))

(dolist (binding (vemv/partition 3 vemv/local-key-bindings))
  (define-key
    (car binding)
    (let ((k (second binding)))
      (if (stringp k)
          (read-kbd-macro k)
          k))
    (third binding)))

(vemv/initial-layout)

(shell-command-to-string "touch ~/.emacs.d/custom.el")
(load custom-file)

(delay 'vemv/clojure-init 1)

(delay (argless (if (window-system)
                    (set-face-attribute 'default nil :font vemv-font)))
       1)

(defun vemv/make-frame ()
  (make-frame `((width . ,(frame-width)) (height . ,(frame-height)))))

(defvar vemv/help-frame nil)

(defmacro vemv/get-help-frame ()
  `(if (and vemv/help-frame (terminal-live-p vemv/help-frame))
       vemv/help-frame
       (let ((frame (vemv/make-frame)))
         (select-frame frame)
         (vemv/maximize)
         (setq vemv/help-frame frame))))

(defun vemv/display-completion (buffer)
  (vemv/safe-select-window vemv/main_window)
  (set-window-buffer vemv/main_window buffer))

(defun undo (&rest args)
  (interactive)
  (apply 'undo-tree-undo args))

(global-set-key [kp-delete] 'delete-char) ;; OSX

(delay
 (argless (setq vemv/project-explorer-initialized t))
 12)

(delay
 ;; every 5 seconds. in practice, not so often b/c `vemv/refreshing-caches` (timestamp lock)
 (argless (run-with-timer 0 5 'vemv/refresh-file-caches))
 60)

;; this is a defmacro so `M-x describe-key` doesn't show a giantic hash, freezing emacs
(defmacro vemv/set-keys-for-scope (scope source)
  `(maphash (lambda (key _)
             (let* ((keyboard-macro (vemv/keyboard-macro key)))
               (if (eq ,scope :global)
                   (global-set-key keyboard-macro
                                   (argless (call-interactively (gethash key ,source))))
                   (define-key ,scope
                     keyboard-macro
                     (argless (call-interactively (gethash key ,source)))))))
           ,source))

(vemv/set-keys-for-scope :global vemv/global-key-bindings)

(vemv/set-keys-for-scope clojure-mode-map vemv/clojure-key-bindings)

(vemv/set-keys-for-scope ruby-mode-map vemv/ruby-key-bindings)

(require 'vemv.desktop)
