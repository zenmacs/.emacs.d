(require 'vemv.lang)
(provide 'vemv.data)

(setq vemv/emacs-files '("vemv.init.el" "vemv.lang.el" "vemv.theme.el" "vemv.data.el"))

;; :emacs "~/.emacs.d/packs/user/user-pack"
;; :haskell "~/Development/vemv/src/haskell"

;; ---

;; available: C-escape, C-', (C/M f/p), M-`
;; ummodificable: C-m, C-i, C-[f
;; ESC acts like alt, but one does not have to hold it pressed.

;; XXX remove everything, and programatically
(setq vemv/key-bindings-to-remove '("\C-a" "\C-b" "\C-e" "\C-f" "\C-s" "\C-w" "\C-j"
                                    "\C-l" "\C-n" "\C-o" "\C-p" "\C-q" "\C-o" "\C-k"
                                    "\C-t" "\C-u" "\C-v" "\C-z" "\C-d" "\C-y" "\C-S-z"
                                    "\C-m" "\C-\\" "\C-h" "\C-r" [f10] "\M-e" "\M-!"
                                    "\M-\"" "\M-|" "\M-$" "\M-y" "\M-f" "\M-T" "\M-t"))

;; XXX remove everything, and programatically
(setq vemv/local-key-bindings-to-remove
      (list (list paredit-mode-map "\C-d" "\C-k" "\C-j" "\M-\"" [127] (kbd ";")) ;; [127] stands for DEL, [27 127] is M-DEL.
            (list comint-mode-map "\M-p")
            (list undo-tree-map (kbd "C-/") (kbd "C-?"))))

(setq vemv/key-bindings-to-dummy '([mouse-6] [mouse-7] [double-mouse-6] [double-mouse-7] [triple-mouse-6] [triple-mouse-7]))

(setq vemv/local-key-bindings
      (list clojure-mode-map  ";" 'vemv/semicolon
            emacs-lisp-mode-map  ";" 'vemv/semicolon
            emacs-lisp-mode-map "RET" 'newline-and-indent
            emacs-lisp-mode-map "C-/" 'vemv/elisp-popup-documentation
            emacs-lisp-mode-map "C-?" 'vemv/elisp-window-documentation))

; basics reminder:
; c-space - set the mark
; M-: - eval lisp
; C-g - cancel command
; M-x - interactive command
; C-x 0 - kill window without killing buffer
; M-x describe-key - resolve a key binding

;; XXX M-RET alters the kill-ring.
;; NOTE - don't use C-o / C-O distinction - doesn't work in this emacs build. use C-S-o instead.
;; NOTE ;; `s-` (meta) must be in lowercase
;; This setup was optimized for a UK keyboard with a Colemak layout, with the number/symbol row switched (e.g. "(" is the default, "9" requires shift).
(setq vemv/global-key-bindings
      (vemv/hash-map
       "C-j" (argless
              (if (and (not cider-launched) vemv/using-nrepl)
                (progn
                 (setq cider-launched t)
                 (setq vemv-cider-connecting t)
                 (delay
                  (argless
                   (funcall (vemv/project-initializers))
                   (select-window vemv/main_window)
                   (cider-jack-in-clojurescript))
                  1))
                (if vemv/using-nrepl
                  (if (cider-connected-p)
                    (if (vemv/current-main-buffer-is-cljs)
                      (vemv/send :cljs)
                      (vemv/send :clj)))
                  (vemv/send :shell))))
       "s-`" (argless
              (let* ((old cider-prompt-for-symbol))
                    (setq cider-prompt-for-symbol nil)
                    (cider-find-var)
                    (setq cider-prompt-for-symbol old)
                    (vemv/advice-nrepl)))
       "C-w" 'vemv/close-this
       "C-f" (argless (ignore-errors
                       (call-interactively 'search-forward)
                       (setq vemv-last-search (first minibuffer-history))))
       "M-l" (argless (save-buffer)
                      (kill-buffer (current-buffer)))
       "<backspace>" (argless (if (region-active-p)
                                (progn (call-interactively 'kill-region)
                                       (pop kill-ring))
                                (paredit-backward-delete)))
       "<S-backspace>" (argless (if (region-active-p)
                                  (progn (call-interactively 'kill-region))
                                  (paredit-backward-delete)))
       "M-t" (argless
              (setq vemv/previous-buffer (current-buffer))
              (vemv/fiplr (lambda (filename)
                                  (find-file filename)
                                  (when (not (eq vemv/previous-buffer (current-buffer)))
                                    (kill-buffer vemv/previous-buffer)))))
       "<C-backspace>" 'vemv/delete-this-line
       "<end>" 'vemv/end-of-line-or-code
       "<home>" 'back-to-indentation
       "<tab>" (argless (or (and (or (not (eq (selected-window) vemv/main_window))
                                     (vemv/in-indentation-point-p)
                                     (vemv/non-completable-char-p))
                                 (or (call-interactively 'indent-for-tab-command)
                                     t))
                            (call-interactively 'company-complete)
                            (call-interactively 'company-dabbrev)))
       "C-;" 'toggle-truncate-lines
       "C-'" (argless (vemv/send :ielm))
       "C-`" 'other-frame
       "C-=" 'mark-whole-buffer
       "C-|" (argless (vemv/send :emacs))
       "C-3" 'vemv/indent
       "C-a" (argless (vemv/copy-selection-or-next-sexpr))
       "C-b" 'vemv/duplicate
       "C-h" 'replace-string ;; search and replace
       "C-k" 'vemv/kill
       "C-n" (argless (make-frame `((width . ,(frame-width)) (height . ,(frame-height))))) ;; in order to kill a frame, use the window system's standard exit (e.g. Alt-F4) command. The other frames won't close.
       "C-o" (argless (vemv/open))
       "C-p" (argless (ignore-errors (search-forward vemv-last-search)))
       "C-q" 'save-buffers-kill-terminal
       "C-S-z" 'undo-tree-redo
       "C-s" 'vemv/save
       "C-t" (argless (vemv/fiplr))
       "C-u" (argless (vemv/delete-backward :cut))
       "C-v" 'cua-paste ;; paste
       "C-z" 'undo-tree-undo
       "M-'" (argless (vemv/send :ielm :backward))
       "M-[" 'paredit-backward ;; move one sexpr backward
       "M-]" 'paredit-forward
       "M-<next>" 'previous-buffer
       "M-<prior>" 'next-buffer
       "M-<up>" 'paredit-splice-sexp-killing-backward
       "M-|" (argless (vemv/send :emacs :backward))
       "M-a" (argless (kill-new (vemv/sexpr-content :backward)))
       "M-K" (argless (kill-new (vemv/kill :backward)))
       "M-k" (argless (vemv/kill :backward))
       "RET" 'newline
       "s-<end>" 'end-of-buffer ;; alias of c-end
       "s-<home>" 'beginning-of-buffer ;; alias of c-home
       "s-<mouse-1>" 'mc/add-cursor-on-click
       "s-<return>" 'vemv/load-clojure-buffer
       "s-e" (argless (insert "é"))
       "s-E" (argless (insert "É"))
       "s-j" 'cider-eval-sexp-at-point
       "s-k" (argless (kill-new (vemv/kill))) ;; cut
       "s-o" (argless (vemv/open-project))
       [f10] 'vemv/ensure-layout
       [f11] 'vemv/maximize
       [f4] (argless
             (save-excursion
              (select-window vemv/repl2)
              (cider-repl-clear-buffer)))
       [f6] 'vemv/hide-ns
       [f7] 'vemv/previous-file-buffer
       [f8] 'vemv/after-file-open
       [f9] 'vemv/next-file-buffer))
        
(global-set-key [mode-line mouse-3] 'nil) ;; prevent close-on-right-click
