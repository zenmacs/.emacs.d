(show-paren-mode 1)      
(cua-mode 1)
(ido-mode 1)

(require 'nrepl)
(require 'popup)
(require 'auto-complete)
(require 'ac-nrepl)
(require 'smex)
(require 'ruby-mode)
(require 'clojure-mode)
(require 'comint)
(require 'dirtree)
(require 'paredit)
(require 'haskell-mode)
(require 'vemv.lang)
(require 'vemv.data)
(require 'vemv.theme)
(provide 'vemv.init)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(comm add-hook 'clojure-mode-hook 'auto-complete-mode)
(add-hook 'emacs-lisp-mode-hook 'auto-complete-mode)

(comm  
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)

   
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)
)
;; restart
;; load clj namespaces
;; indent, del, intro paredit issues
;; cross-proj conf <<<<<<<<<<<<<<<<<<<<<<<<<<<<
;; tree: refresh on adds
;; javadoc
;; render
;; semantic home/end keys behavior
;; setq: Wrong type argument: wholenump, -201
;; popup doc for defvar
;; goto fn defs

(add-hook 'nrepl-connected-hook (argless (delay (argless ; apparently needed only on the first run!
                                                 (delete-window)
                                                 (let ((w (selected-window)))
                                                   (select-window vemv/repl1)
                                                   (switch-to-buffer "*nrepl*")
                                                   (select-window w))))))

(add-hook 'slime-connected-hook (argless (delay (argless (kill-buffer "*slime-events*"))
                                                5)))

(add-hook 'clojure-mode-hook (argless (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'kill-buffer-hook (argless (let ((killed (buffer-name (current-buffer))))
                                       (setq vemv/open_file_buffers
                                             (filter (lambda (_) (not (equal _ killed)))
                                                     vemv/open_file_buffers)))))

(custom-set-variables
 '(haskell-mode-hook '(turn-on-haskell-indentation)))

; find-file-hook

(if (window-system) (vemv/maximize))

(setq vemv/launched nil)
(delay ; XXX no error reporting
 (argless

  "Create a layout and fill it with the corresponding content."

  (comm if (window-system) (progn (vemv/maximize) (vemv/maximize))) ; workaround minibuffer size bug.

  (comm x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_FULLSCREEN" 0))

  (split-window-vertically) ; Actually means "split in two horizontal halves".
  (enlarge-window 12)
  (vemv/next-window)

  (comm
   (vemv/render-trees vemv/tree-dirs)

   (enlarge-window-horizontally -84) ; Unlike split-window-*, this one does get the naming right.
   (beginning-of-buffer)
   (vemv/next-window) (vemv/next-window))

  (split-window-horizontally)

  (switch-to-buffer "*scratch*")
  (emacs-lisp-mode)
  (ielm)
  (paredit-mode) (auto-complete-mode) (toggle-truncate-lines)
  (setq vemv/repl1 (selected-window))
  (vemv/next-window)

  (shell)
  (setq vemv/repl2 (selected-window))
  (vemv/next-window)

  (message "")
  (setq vemv/launched t)))

; (setq debug-on-error t)

(setq custom-file "~/.emacs.d/custom.el") ; touch on install!
(load custom-file)

(setq visible-bell nil) ; disable flickering
(setq ido-auto-merge-delay-time 99999) ; prevents annoying folder switching. might be handy: (setq ido-max-directory-size 100000)
(setq slime-net-coding-system 'utf-8-unix) ;; as for clojure, the issue is best solved at leiningen level: http://stackoverflow.com/questions/10167829/cant-send-funny-chars-to-slime

;; opens the latest file
(recentf-mode 1)
(delay
 (argless
     (switch-to-buffer "*scratch*") (erase-buffer)
     (if (file-readable-p recentf-save-file)
              (if (pos? (length recentf-list))
                  (let ((head (car recentf-list)))
                    (ignore-errors (vemv/open
                                    (if (vemv/ends-with head ".ido.last")
                                        (second recentf-list)
                                        head)))))))
 2)

; (cd "~/clj/src/")

(if (window-system) (set-face-attribute 'default nil :font "DejaVu Sans Mono-9"))

(put 'if 'lisp-indent-function nil)

(defadvice save-buffers-kill-emacs (around no-y-or-n activate) ; switches the expected input from "yes no" to "y n" on exit-without-save
  (flet ((yes-or-no-p (&rest args) t)
         (y-or-n-p (&rest args) t))
    ad-do-it))

(comm (delay (argless (slime-connect "localhost" 4005)))) ; XXX lazy load on C-E

(dolist (key vemv/local-key-bindings-to-remove)
  (mapc (lambda (arg)
          (define-key (car key) arg nil))
        (cdr key)))

(dolist (key vemv/key-bindings-to-remove)
  (global-unset-key key))

(dolist (key vemv/key-bindings-to-dummy)
  (global-set-key key (argless)))

(dolist (binding (vemv/partition 2 vemv/global-key-bindings))
  (global-set-key (let ((k (car binding)))
                    (if (stringp k)
                        (read-kbd-macro k)
                        k))
                  (second binding)))

(dolist (binding (vemv/partition 3 vemv/local-key-bindings))
  (define-key
    (car binding)
    (let ((k (second binding)))
      (if (stringp k)
          (read-kbd-macro k)
          k))
    (third binding)))

; customize-group tree-widget: tree-widget-image-enable

;(nrepl-jack-in)
