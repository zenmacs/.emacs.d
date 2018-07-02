(require 'vemv.project)
(require 'vemv.lang)
(provide 'vemv.data)

(setq vemv/emacs-files '("vemv.init.el" "vemv.lang.el" "vemv.theme.el" "vemv.data.el"))

;; available: C-escape, C-', (C/M f/p), M-`
;; ummodificable: C-m, C-i, C-[
;; ESC acts like alt, but one does not have to hold it pressed.

(setq vemv/key-bindings-to-remove '("\C-a" "\C-b" "\C-e" "\C-f" "\C-s" "\C-w" "\C-j" "\M-x"
                                    "\C-l" "\C-n" "\C-o" "\C-p" "\C-q" "\C-o" "\C-k" "\M-a"
                                    "\C-t" "\C-u" "\C-v" "\C-z" "\C-d" "\C-y" "\C-S-z"
                                    "\C-m" "\C-\\" "\C-h" "\C-r" [f10] "\M-e" "\M-!"
                                    "\M-\"" "\M-|" "\M-$" "\M-y" "\M-f" "\M-T" "\M-t"))

(setq vemv/key-bindings-to-dummy
      '([mouse-6] [mouse-7]
        [double-mouse-6] [double-mouse-7]
        [triple-mouse-6] [triple-mouse-7]
        ;; [triple-wheel-up] [triple-wheel-down] ;; don't override these.
        ;; they disable three-finger swipe (as I originally intended), but also two-finger swipe too (ruins scrolling)
        [triple-wheel-right] [triple-wheel-left]))

;; XXX create instead vemv/clojure-mode-key-bindings, vemv/emacs-elisp-mode-key-bindings
(setq vemv/local-key-bindings
      (list clojure-mode-map  ";" 'vemv/semicolon
            clojure-mode-map "<tab>" 'vemv/tab
            emacs-lisp-mode-map "<tab>" 'vemv/tab
            ruby-mode-map "<tab>" 'vemv/tab
            html-mode-map "<tab>" 'vemv/tab
            js-mode-map "<tab>" 'vemv/tab
            haml-mode-map "<tab>" 'vemv/tab
            emacs-lisp-mode-map  ";" 'vemv/semicolon
            helm-map [(shift return)] (argless (interactive) (helm-select-nth-action 1))
            helm-map "C-a" 'vemv/helm-persistent-action-all
            emacs-lisp-mode-map "RET" 'newline-and-indent
            emacs-lisp-mode-map "C-?" 'vemv/elisp-window-documentation
            *fiplr-keymap* "<S-return>" 'exit-minibuffer ;; makes it equivalent to RET. Sometime I type S-RET due to muscle memory
            ))

; basics reminder:
; c-space - set the mark
; M-: - eval lisp
; C-g - cancel command
; M-x - interactive command
; M-x describe-key - resolve a key binding

(global-set-key [mode-line mouse-3] 'nil) ;; prevent close-on-right-click
