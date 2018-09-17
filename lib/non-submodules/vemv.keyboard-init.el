;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(provide 'vemv.keyboard-init)

(global-set-key (kbd "M-x") 'smex)

;; Important - remove keybindings before (vemv/initial-layout) so M-x cannot interrupt

(unless vemv/terminal-emacs?
  (dolist (mode (list paredit-mode-map emacs-lisp-mode-map yas-minor-mode-map))
    (mapc (lambda (arg)
            (define-key mode (vemv/keyboard-macro arg) nil))
          vemv/exhaustive-list-of-bindings-to-remove))

  (add-hook 'cider-mode-hook
            (argless
             (mapc (lambda (arg)
                     (define-key cider-mode-map (vemv/keyboard-macro arg) nil))
                   vemv/exhaustive-list-of-bindings-to-remove)))

  (add-hook 'cider-repl-mode-hook
            (define-key cider-repl-mode-map (kbd "<tab>") 'vemv/tab)))

(add-hook 'magit-diff-mode-hook
          (argless (mapc (lambda (arg)
                           (define-key magit-mode-map (vemv/keyboard-macro arg) nil)
                           (define-key magit-diff-mode-map (vemv/keyboard-macro arg) nil))
                         vemv/exhaustive-list-of-bindings-to-remove)
                   (vemv/set-keys-for-scope magit-diff-mode-map vemv/clojure-key-bindings)))

(add-hook 'project-explorer-mode-hook
          (argless
           (es-define-keys project-explorer-mode-map
             (kbd "<mouse-1>") 'pe/left-click)))

(unless vemv/terminal-emacs?
  (dolist (key vemv/key-bindings-to-remove)
    (global-unset-key key)))

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

(global-set-key [kp-delete] 'delete-char) ;; OSX

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
