(provide 'vemv.keyboard-init)

(global-set-key (kbd "M-x") 'smex)

;; Important - remove keybindings before (vemv/initial-layout) so M-x cannot interrupt

(dolist (mode (list paredit-mode-map undo-tree-map cider-mode-map))
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
