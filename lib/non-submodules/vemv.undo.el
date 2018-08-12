(require 'undo-tree)
(provide 'vemv.undo)

(global-undo-tree-mode)

(mapc (lambda (arg)
        (define-key undo-tree-map (vemv/keyboard-macro arg) nil))
      vemv/exhaustive-list-of-bindings-to-remove)
