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

;; XXX remove everything, and programatically
(setq vemv/local-key-bindings-to-remove
      (list (list paredit-mode-map "\C-d" "\C-k" "\C-j" "\M-\"" [127] (kbd ";")) ;; [127] stands for DEL, [27 127] is M-DEL.
            (list comint-mode-map "\M-p")
            (list undo-tree-map (kbd "C-/") (kbd "C-?"))))

(setq vemv/key-bindings-to-dummy '([mouse-6] [mouse-7]
                                   [double-mouse-6] [double-mouse-7]
                                   [triple-mouse-6] [triple-mouse-7]
                                   ;; [triple-wheel-up] [triple-wheel-down] ;; don't override these. they disable three-finger swipe (as I originally intended), but also two-finger swipe too (ruins scrolling)
                                   [triple-wheel-right] [triple-wheel-left]
                                   ))

;; XXX create instead vemv/clojure-mode-key-bindings, vemv/emacs-elisp-mode-key-bindings
(setq vemv/local-key-bindings
      (list clojure-mode-map  ";" 'vemv/semicolon
            clojure-mode-map "<tab>" 'vemv/tab
            emacs-lisp-mode-map  ";" 'vemv/semicolon
            helm-map [(shift return)] (argless (interactive) (helm-select-nth-action 1))
            helm-map "C-a" 'vemv/helm-persistent-action-all
            emacs-lisp-mode-map "RET" 'newline-and-indent
            emacs-lisp-mode-map "C-?" 'vemv/elisp-window-documentation))

; basics reminder:
; c-space - set the mark
; M-: - eval lisp
; C-g - cancel command
; M-x - interactive command
; C-x 0 - kill window without killing buffer
; M-x describe-key - resolve a key binding

(global-set-key [mode-line mouse-3] 'nil) ;; prevent close-on-right-click
