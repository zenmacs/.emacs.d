(require 'vemv.lang)
(provide 'vemv.data)

;; XXX intro w/o moving the comment

(setq vemv/open_file_buffers ())

(setq vemv/emacs-files '("vemv.init.el" "vemv.lang.el" "vemv.theme.el" "vemv.data.el"))

(setq vemv/tree-dirs (let ((d (-drop 2 (directory-files "~/clj"))))
		       (flatten (mapcar (lambda (a) (list (vemv/string-to-keyword a) (concat "~/clj/" a))) d))))

(mapc (lambda (a) (conj! vemv/tree-dirs a))
      (list
       "~/.emacs.d/lib" :lib
       "~/.emacs.d/lib/non-submodules" :non
       ))

;; :emacs "~/.emacs.d/packs/user/user-pack"
;; :haskell "~/Development/vemv/src/haskell"

;; ---

;; available: C-escape, C-', C-j, C-y, (C/M f/p)
;; ummodificable: C-m, C-i, C-[f
;; ESC acts like alt, but one does not have to hold it pressed.
;; C-click is very handy for switching between arbitrary buffers.

(setq vemv/key-bindings-to-remove '("\C-a" "\C-b" "\C-e" "\C-f" "\C-s" "\C-w" "\C-j"
                                    "\C-l" "\C-n" "\C-o" "\C-p" "\C-q" "\C-o" "\C-k"
                                    "\C-t" "\C-u" "\C-v" "\C-z" "\C-d" "\C-y" "\C-S-z"
                                    "\C-m" "\C-\\" "\C-h" "\C-r" [f10] "\M-e" "\M-!"
                                    "\M-\"" "\M-|" "\M-$" "\M-y" "\M-f"))

(setq vemv/local-key-bindings-to-remove
      (list (list paredit-mode-map "\C-d" "\C-k" "\C-j" "\M-\"" [127] (kbd ";")) ; [127] stands for DEL, [27 127] is M-DEL.
            (list comint-mode-map "\M-p")
	    (list undo-tree-map (kbd "C-/") (kbd "C-?"))))

(setq vemv/key-bindings-to-dummy '([mouse-6] [mouse-7] [double-mouse-6] [double-mouse-7] [triple-mouse-6] [triple-mouse-7]))

(setq vemv/local-key-bindings
      (list ; clojure-mode-map  "C-/" 'nrepl-doc
            ; clojure-mode-map  "C-?" 'nrepl-src
            clojure-mode-map  ";" 'vemv/semicolon
            clojure-mode-map  "C-e" (argless (vemv/send :cider))
            clojure-mode-map  "M-e" (argless (vemv/send :cider :backward))
            clojurescript-mode-map  "C-e" (argless (vemv/send :cljs))
            clojurescript-mode-map  "M-e" (argless (vemv/send :cljs :backward))
            emacs-lisp-mode-map "C-/" 'vemv/elisp-popup-documentation
            emacs-lisp-mode-map "C-?" 'vemv/elisp-window-documentation
				    ; emacs-lisp-mode-map "C-z" 'undo-tree-undo
				    emacs-lisp-mode-map "RET" 'newline-and-indent
            emacs-lisp-mode-map  ";" 'vemv/semicolon
            haskell-mode-map  "C-e" (argless (vemv/send :shell))
            haskell-mode-map  "M-e" (argless (vemv/send :shell :backward))
            haskell-mode-map "RET" (argless (insert "\n"))
            haskell-mode-map "," (argless (insert ", "))
            ruby-mode-map "RET" 'newline-and-indent)) ;; 'ruby-reindent-then-newline-and-indent

(comm setq vemv/local-key-bindings
      (vemv/hash-map
       clojure-mode-map (vemv/hash-map
			 ; "C-/" ;  'nrepl-doc
			 ; "C-?" ; 'nrepl-src
			 ; ";" (argless (paredit-semicolon) (paredit-semicolon) (insert " ")) ;; (when (and (eolp) COLUMN > 0) (insert " "))
			 )
       emacs-lisp-mode-map (vemv/hash-map
			     "C-/" 'vemv/elisp-popup-documentation
			     "C-?" 'vemv/elisp-window-documentation
			     ; "C-z" 'undo-tree-undo
			     "RET" 'newline-and-indent
			     ";" (argless (paredit-semicolon) (paredit-semicolon) (insert " ")))
       haskell-mode-map (vemv/hash-map
			   "C-e" (argless (vemv/send :shell))
			   "M-e" (argless (vemv/send :shell :backward))
			   "RET" (argless (insert "\n"))
			   "," (argless (insert ", ")))
       ruby-mode-map (vemv/hash-map
		       "RET" 'ruby-reindent-then-newline-and-indent)))

; basics reminder:
; c-space - set the mark
; M-: - eval lisp
; C-g - cancel command
; M-x - interactive command
; C-x 0 - kill window without killing buffer
; M-x describe-key - resolve a key binding

;XXX M-RET alters the kill-ring.
(setq vemv/global-key-bindings ; This setup is optimized for a UK keyboard with a Colemak layout, with the number/symbol row switched (e.g. "(" is the default, "9" requires shift).
      (vemv/hash-map
            "S-<next>" 'vemv/previous-window ; on OSX: shift-fn-up
            "S-<prior>" 'vemv/next-window
	    "§" 'hippie-expand
            "C-<next>" 'vemv/previous-file-buffer
            "C-<prior>" 'vemv/next-file-buffer
            "M-<next>" 'previous-buffer
            "M-<prior>" 'next-buffer
	    "M-<begin>" nil
	    "M-<end>" nil
	    ; "<backtab>" 'auto-complete
            "C-a" (argless (if (region-active-p) ; copy selection or next sexpr
                               (call-interactively 'kill-ring-save)
                               (kill-new (vemv/sexpr-content))))
            "M-a" (argless (kill-new (vemv/sexpr-content :backward)))
            "C-s" 'save-buffer ; save
            "C-v" 'cua-paste ; paste
            "C-o" 'vemv/open
            "C-O" (argless (vemv/open-project))

            "C-k" 'vemv/kill
            "C-K" (argless (kill-new (vemv/kill))) ; cut
            "M-k" (argless (vemv/kill :backward))
            "M-<up>" 'paredit-splice-sexp-killing-backward
            "M-K" (argless (kill-new (vemv/kill :backward)))

            "C-b" 'vemv/duplicate
            ; "C-w" 'smex ; M-x
						"C-j" (argless
							(when (and (not cider-launched) gpm-using-nrepl)
								(setq cider-launched t)
								(message "Connecting...")
								; (shell-command-to-string "sudo /usr/local/bin/nginx -s stop") ; nginx from rails projects interfers with jvm servers
								; (shell-command-to-string "setopt nullglob; cd ~/gpm; rm -rf logs/*; lein clean")
								(message "Running make clean...")
								(shell-command-to-string "source ~/.zshrc; cd ~/gpm/src; make clean")
								(shell-command-to-string "source ~/.zshrc; cd ~/gpm/src; make sass")
								(select-window vemv/main_window)
								(cider-jack-in-clojurescript)))
            "C-z" 'undo-tree-undo
            "C-S-z" 'undo-tree-redo
            "C-`" 'other-frame
            "<backspace>" (argless (if (region-active-p)
                                       (progn (call-interactively 'kill-region)
                                              (pop kill-ring))
                                       (paredit-backward-delete)))
            "<C-backspace>" 'vemv/delete-this-line
            "<S-backspace>" (argless (if (region-active-p)
                                       (progn (call-interactively 'kill-region))
                                       (paredit-backward-delete)))
            "<home>" 'back-to-indentation
            "<end>" 'vemv/end-of-line-or-code
            "C-u" (argless (vemv/delete-backward :cut))
            ;"M-RET" (argless) ; TODO comment-aware intro

            ;; "C-!" (argless (vemv/send :cljs))
            "C-'" (argless (vemv/send :ielm))
            "C-|" (argless (vemv/send :emacs))
            "C-$" (argless (if gpm-using-nrepl (vemv/send :cljs) (vemv/send :shell)))
            ;; "M-!" (argless (vemv/send :cljs :backward))
            "M-'" (argless (vemv/send :ielm :backward))
            "M-|" (argless (vemv/send :emacs :backward))
            "M-$" (argless (if gpm-using-nrepl (vemv/send :cljs :backward) (vemv/send :shell :backward)))

            "C-l" (argless (kill-buffer (current-buffer))) ; XXX and switch to the next file buffer
            ;; "C-L" (argless (let (kill-buffer-query-functions) (kill-buffer)))
            "M-l" (argless (save-buffer)
                           (kill-buffer (current-buffer)))
						"C-w" (argless (if (< (length (vemv/current-frame-buffers)) 2)
													   (delete-frame (selected-frame) t) ; close an auxiliary frame
														 (kill-buffer-and-window) (vemv/previous-window))) ; close annoying popup windows
            "C-h" 'replace-string ; search and replace
            "C-f" (argless (ignore-errors (call-interactively 'search-forward)))
            "M-[" 'paredit-backward ; move one sexpr backward
            "M-]" 'paredit-forward
            [f6] 'split-window-vertically
            [f7] 'split-window-horizontally
            "C-n" (argless (make-frame `((width . ,(frame-width)) (height . ,(frame-height))))) ; in order to kill a frame, use the window system's standard exit (e.g. Alt-F4) command. The other frames won't close.
            [f10] 'vemv/ensure-layout
            [f11] 'vemv/maximize
	    "s-e" (argless (insert "é"))
	    "s-E" (argless (insert "É"))
            "C-;" 'toggle-truncate-lines
            "C-*" (argless (vemv/open (concat "~/.emacs.d/lib/non-submodules/"
                                              (ido-completing-read "Open: " vemv/emacs-files))))
	    "C-3" 'vemv/indent
      "C-t" 'fiplr-find-file
      ; "C-T" (argless (switch-to-buffer "*scratch*"))
	    "C-." 'vemv/ns-form
	    ; "C-," 'nrepl-load-current-buffer
	    "RET" 'newline
	    "C-\\" 'sgml-close-tag
	    "C-=" 'mark-whole-buffer
	    "C-q" 'save-buffers-kill-terminal))
            ; eval-minibuffer: M-:
