(require 'vemv.lang)
(provide 'vemv.data)

; TODO DEL (translated from <backspace>) runs the command paredit-backward-delete
; XXX intro w/o moving the comment

(setq vemv/open_file_buffers ())

(setq vemv/emacs-files '("vemv.init.el" "vemv.lang.el" "vemv.theme.el" "vemv.data.el"))

(setq vemv/tree-dirs
      '(:clj "~/clj/src"
        :lib "~/.emacs.d/lib"
        :non "~/.emacs.d/lib/non-submodules"
       ;:cljs "~/clj/src-cljs"
       ;:soundcloud-cljs "~/Development/needleforsoundcloud/frontend/cljs/needle"
       ;:ellipse "~/Development/ellipse/src/ellipse"
       ;:emacs "~/.emacs.d/packs/user/user-pack"
       ;:haskell "~/Development/vemv/src/haskell"
       :loudlist "~/loudlist"))
(setq vemv/headers (vemv/hash-map

:clj

"(ns vemv
  (:require [clojure.pprint :as pp]
            [clojure.string :as string]))\n\n"
:web

"(ns vemv
  (:require
   [ring.adapter.jetty :as jetty]
   [clojure.pprint :as pretty]
   [ring.util.response :as response]
   [net.cgrand.enlive-html :as enlive]
   [clj-http.client :as http])
  (:use (compojure handler [core :only (GET POST defroutes)])))\n\n"))

; available: C-,, C-., C-escape, C-', C-j, C-y, C/M-f/p.
; ummodificable: C-m, C-i, C-[
; ESC acts like alt, but one does not have to hold it pressed.
; C-click is very handy for switching between arbitrary buffers. Most times vemv/previous-file-buffer and vemv/next-file-buffer will do though.

(setq vemv/key-bindings-to-remove '("\C-a" "\C-b" "\C-e" "\C-f" "\C-s" "\C-w" "\C-j"
                                    "\C-l" "\C-n" "\C-o" "\C-p" "\C-q" "\C-o" "\C-k"
                                    "\C-t" "\C-u" "\C-v" "\C-z" "\C-d" "\C-y"
                                    "\C-m" "\C-\\" "\C-h" "\C-r" [f10] "\M-e" "\M-!"
                                    "\M-\"" "\M-|" "\M-$" "\M-y" "\M-f"))

(setq vemv/local-key-bindings-to-remove
      (list (list paredit-mode-map "\C-k" "\C-j" "\M-\"" [127]) ; [127] stands for DEL, [27 127] is M-DEL.
            (list comint-mode-map "\M-p")))

(setq vemv/key-bindings-to-dummy '([mouse-6] [mouse-7] [double-mouse-6] [double-mouse-7] [triple-mouse-6] [triple-mouse-7]))

(setq vemv/local-key-bindings
      (list clojure-mode-map  "C-/" 'nrepl-doc
            clojure-mode-map  "C-?" 'nrepl-src
            clojure-mode-map  "C-e" (argless (vemv/send :slime))
            clojure-mode-map  "M-e" (argless (vemv/send :slime :backward))
            emacs-lisp-mode-map "C-/" 'vemv/elisp-popup-documentation
            emacs-lisp-mode-map "C-?" 'vemv/elisp-window-documentation
	    emacs-lisp-mode-map "C-z" 'undo-tree-undo
            haskell-mode-map  "C-e" (argless (vemv/send :shell))
            haskell-mode-map  "M-e" (argless (vemv/send :shell :backward))
            haskell-mode-map "RET" (argless (insert "\n"))
            emacs-lisp-mode-map "RET" (argless (insert "\n"))
            haskell-mode-map "," (argless (insert ", "))
            ruby-mode-map "RET" 'ruby-reindent-then-newline-and-indent
	    emacs-lisp-mode-map "RET" 'newline-and-indent))

;XXX M-RET alters the kill-ring.
(setq vemv/global-key-bindings ; This setup is optimized for a UK keyboard with a Colemak layout, with the number/symbol row switched (e.g. "(" is the default, "9" requires shift).
      (list "C-<prior>" 'vemv/previous-window
            "C-<next>" 'vemv/next-window
            "M-<prior>" 'vemv/previous-file-buffer
            "M-<next>" 'vemv/next-file-buffer
            "S-<prior>" 'previous-buffer
            "S-<next>" 'next-buffer

            "C-a" (argless (if (region-active-p) ; copy selection or next sexpr
                               (call-interactively 'kill-ring-save)
                               (kill-new (vemv/sexpr-content))))
            "M-a" (argless (kill-new (vemv/sexpr-content :backward)))
            "C-s" 'save-buffer ; save
            "C-v" 'cua-paste ; paste
            "C-o" 'vemv/open

            "C-k" 'vemv/kill
            "C-K" (argless (kill-new (vemv/kill))) ; cut
            "M-k" (argless (vemv/kill :backward))
            "M-K" (argless (kill-new (vemv/kill :backward)))

            "C-b" 'vemv/duplicate
            "C-w" 'smex ; M-x
            "C-z" 'undo-tree-undo
            "C-`" 'undo-tree-redo
            "<backspace>" (argless (if (region-active-p)
                                       (progn (call-interactively 'kill-region)
                                              (pop kill-ring))
                                       (paredit-backward-delete)))
            "<S-backspace>" (argless (if (region-active-p)
                                       (progn (call-interactively 'kill-region))
                                       (paredit-backward-delete)))
            ;"<home>" 'vemv/home
            ;"<end>" 'vemv/end
            "C-u" (argless (vemv/delete-backward :cut))
            ;"M-RET" (argless) ; TODO comment-aware intro

            "C-!" (argless (vemv/send :cljs))
            "C-\"" (argless (vemv/send :ielm))
            "C-|" (argless (vemv/send :emacs))
            "C-$" (argless (vemv/send :shell))
            "M-!" (argless (vemv/send :cljs :backward))
            "M-\"" (argless (vemv/send :ielm :backward))
            "M-|" (argless (vemv/send :emacs :backward))
            "M-$" (argless (vemv/send :shell :backward))

            "C-l" (argless (kill-buffer (current-buffer))) ; XXX and switch to the next file buffer
            "M-L" (argless (kill-buffer (current-buffer))) ; XXX don't ask
            "M-l" (argless (save-buffer)
                           (kill-buffer (current-buffer)))
            "C-n" (argless (kill-buffer-and-window) (vemv/previous-window))
            "C-h" 'replace-string ; search and replace
            "C-f" (argless (ignore-errors (call-interactively 'search-forward)))
            "M-[" 'paredit-backward ; move one sexpr backward
            "M-]" 'paredit-forward
            [f6] 'split-window-vertically
            [f7] 'split-window-horizontally
            [f9] (argless (make-frame `((width . ,(window-width)) (height . ,(frame-height))))) ; in order to kill a frame, use the window system's standard exit (e.g. Alt-F4) command. The other frames won't close.
            [f10] 'vemv/ensure-layout
            [f11] 'vemv/maximize
	    "s-e" (argless (insert "é"))
	    "s-E" (argless (insert "É"))
            "C-;" 'toggle-truncate-lines
            "C-&" (argless (vemv/open (concat "~/.emacs.d/lib/non-submodules/"
                                              (ido-completing-read "Open: " vemv/emacs-files))))
            "C-*" (argless (insert (gethash (vemv/string-to-keyword (ido-completing-read "Insert header: " '("clj" "web")))
                                            vemv/headers)))
            "C-#" 'vemv/indent ; XXXXXXX set to TAB instead
            "C-t" (argless (switch-to-buffer "*scratch*"))))
            ; eval-minibuffer: M-:
