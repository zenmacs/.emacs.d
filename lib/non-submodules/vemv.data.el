(require 'vemv.project)
(require 'vemv.lang)
(provide 'vemv.data)

(setq vemv/emacs-files '("vemv.init.el" "vemv.lang.el" "vemv-theme.el" "vemv.data.el"))

;; available: C-escape, C-', (C/M f/p), M-`
;; ummodificable: C-m, C-i, C-[
;; ESC acts like alt, but one does not have to hold it pressed.

(setq vemv/key-bindings-to-remove '("\C-a" "\C-b" "\C-e" "\C-f" "\C-s" "\C-w" "\C-j" "\M-x" "\C-\M-f"
                                    "\C-l" "\C-n" "\C-o" "\C-p" "\C-q" "\C-o" "\C-k" "\M-a"
                                    "\C-t" "\C-u" "\C-v" "\C-z" "\C-d" "\C-y" "\C-S-z"
                                    "\C-m" "\C-\\" "\C-h" "\C-r" [f10] "\M-e" "\M-!"
                                    "\M-\"" "\M-|" "\M-$" "\M-y" "\M-f" "\M-T" "\M-t"
                                    [menu-bar help-menu]
                                    [menu-bar file]
                                    [menu-bar File]
                                    [menu-bar tools]
                                    [menu-bar edit]
                                    [menu-bar options]
                                    [menu-bar buffer]))

(setq vemv/key-bindings-to-dummy
      '([mouse-3] [mouse-6] [mouse-7]
        [double-mouse-6] [double-mouse-7]
        [triple-mouse-6] [triple-mouse-7]
        ;; [triple-wheel-up] [triple-wheel-down] ;; don't override these.
        ;; they disable three-finger swipe (as I originally intended), but also two-finger swipe too (ruins scrolling)
        [triple-wheel-right] [triple-wheel-left]))

(setq vemv/local-key-bindings
      (list emacs-lisp-mode-map "<backtab>" (argless
                                             (let ((vemv/max-mini-window-height 0.99))
                                               (or (ignore-errors
                                                     (replying-yes
                                                      (-some-> (symbol-at-point) documentation vemv/echo)))
                                                   (ignore-errors
                                                     (let ((s (symbol-at-point)))
                                                       (-some-> s (documentation-property 'variable-documentation) vemv/echo))))))
            emacs-lisp-mode-map "<tab>" 'vemv/tab
            emacs-lisp-mode-map ";" 'vemv/semicolon
            emacs-lisp-mode-map [menu-bar emacs-lisp] nil
            emacs-lisp-mode-map "RET" 'newline-and-indent
            emacs-lisp-mode-map "C-?" 'vemv/elisp-window-documentation
            minibuffer-local-map [menu-bar minibuf] nil
            *fiplr-keymap* "<S-return>" 'exit-minibuffer ;; makes it equivalent to RET. Sometime I type S-RET due to muscle memory
            ))

;; basics reminder:
;; c-space - set the mark
;; M-: - eval lisp
;; C-g - cancel command
;; M-x - interactive command
;; M-x describe-key - resolve a key binding

(defun vemv/mouse-delete-window (click)
  "Like `mouse-delete-window', but only deletes popup windows.
   See https://emacs.stackexchange.com/questions/33464"
  (interactive "e")
  (unless (one-window-p t)
    (mouse-minibuffer-check click)
    (let* ((window (posn-window (event-start click))))
      (when (not (vemv/good-window-p window))
        (delete-window window)))))

(global-set-key [mode-line mouse-3] 'vemv/mouse-delete-window)
