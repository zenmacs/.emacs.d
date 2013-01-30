;;; -*- lexical-binding: t -*-
(setq lexical-binding t)

(show-paren-mode 1)
(recentf-mode 1)
(ido-mode 1)
(cua-mode 1)

(require 'yasnippet)
(require 'saveplace)
(require 'dash)
(require 'nrepl)
(require 'popup)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'ac-nrepl)
(require 'smex)
(require 'ruby-mode)
(require 'clojure-mode)
(require 'comint)
(require 'dirtree)
(require 'paredit)
(require 'haskell-mode)
(require 'undo-tree)
(require 'vemv.lang)
(require 'vemv.data)
(require 'vemv.theme)
(provide 'vemv.init)

(yas-global-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)
(setq initial-scratch-message "")
(eval-after-load "auto-complete" ; XXX don't show doc in nREPLs AC
  '(add-to-list 'ac-modes 'nrepl-mode))

(custom-set-variables
 '(haskell-mode-hook '(turn-on-haskell-indentation))
 '(tree-widget-image-enable nil)
 '(nrepl-popup-stacktraces nil)
 '(ielm-prompt "ielm> "))

(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'undo-tree-mode)
(add-hook 'clojure-mode-hook 'auto-complete-mode)
(add-hook 'clojure-mode-hook (argless (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'clojure-mode-hook (argless (if-let (ns (clojure-find-ns))
					      (progn
						(nrepl-eval-ns-form)
						(with-current-buffer "*nrepl*"
						    (nrepl-set-ns ns)))
					      (when (vemv/contains? (buffer-name) ".clj")
						(let ((name (substring (buffer-name) ; XXX needs prefixing
									     0
									     (- (length (buffer-name)) 4)))) ; removes the .clj
						  (save-excursion ;; XXX nrepl-load-current-buffer
						    (beginning-of-buffer)
						    (insert (concat "(ns "
								    name
								    ")"))
						    (nrepl-eval-ns-form))
						  (with-current-buffer "*nrepl*"
						    (nrepl-set-ns name)))))))

(add-hook 'emacs-lisp-mode-hook 'auto-complete-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
   
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-mode-hook 'auto-complete-mode)
(add-hook 'nrepl-mode-hook 'enable-paredit-mode)
(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-connected-hook (argless (delay (argless ; apparently needed only on the first run! (this comment was placed in the slime era)
                                                 (select-window vemv/main_window)
                                                 (nrepl-eval-ns-form)))))

(add-hook 'kill-buffer-hook (argless (let ((killed (buffer-name (current-buffer))))
                                       (setq vemv/open_file_buffers
                                             (filter (lambda (_) (not (equal _ killed)))
                                                     vemv/open_file_buffers)))))

(set-default 'truncate-lines t)
(setq-default save-place t)

(ac-config-default)
(ac-flyspell-workaround)
;(add-to-list 'ac-dictionary-directories (concat (live-pack-lib-dir) "auto-complete/dict"))

(setq ac-auto-show-menu t)
(setq ac-dwim t)
(setq ac-use-menu-map t)
(setq ac-quick-help-delay 1)
(setq ac-delay 0.8)
;; (setq ac-quick-help-height 60)

(set-default 'ac-sources
             '(ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer))

(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                html-mode nxml-mode sh-mode smarty-mode clojure-mode
                lisp-mode textile-mode markdown-mode tuareg-mode))
  (add-to-list 'ac-modes mode))
  
;; restart
;; tree: refresh on adds
;; javadoc
;; popup doc for defvar
;; goto fn defs

(if (window-system) (vemv/maximize))
(setq vemv/launched nil)

(split-window-vertically)
(enlarge-window 8)

(split-window-horizontally) ;;  two vertical halves actually
(vemv/render-trees vemv/tree-dirs)

(enlarge-window-horizontally -52) ; Unlike split-window-*, this one does get the naming right.

(beginning-of-buffer)
(vemv/next-window)

(setq vemv/main_window (selected-window))
(vemv/next-window)

(split-window-horizontally)

(switch-to-buffer "*scratch*")
(emacs-lisp-mode)
(setq vemv/repl1 (selected-window))
(vemv/next-window)

(ielm)
(paredit-mode) (auto-complete-mode)

(setq vemv/repl2 (selected-window))
(vemv/next-window)
(vemv/next-window)

(message "")
(setq vemv/launched t)

; (setq debug-on-error t)

(setq custom-file "~/.emacs.d/custom.el") ; touch on install!
(load custom-file)

(setq visible-bell nil) ; disable flickering
(setq ido-auto-merge-delay-time 99999) ; prevents annoying folder switching. might be handy: (setq ido-max-directory-size 100000)

(delay (argless
	(select-window vemv/main_window)
	(if (file-readable-p recentf-save-file)
	    (if (pos? (length recentf-list))
		(let ((head (car recentf-list)))
		  (ignore-errors (vemv/open
				  (if (vemv/ends-with head ".ido.last")
				      (second recentf-list)
				      head)))))))
       4)


(cd "~/clj/loudlist/src/loudlist")

(if (window-system) (set-face-attribute 'default nil :font "DejaVu Sans Mono-9"))

(put 'if 'lisp-indent-function nil)

(comment (setq comment-indent-function (argless (save-excursion (forward-line -1) (current-indentation)))))

(defadvice save-buffers-kill-emacs (around no-y-or-n activate) ; switches the expected input from "yes no" to "y n" on exit-without-save
  (flet ((yes-or-no-p (&rest args) t)
         (y-or-n-p (&rest args) t))
    ad-do-it))

(setq back-to-indentation-state nil)

(defadvice back-to-indentation (around back-to-back)
  (if (eq last-command this-command)
      (progn
	(if back-to-indentation-state
	    ad-do-it
	    (beginning-of-line)
	(send! back-to-indentation-state 'not)))
      (progn
	(setq back-to-indentation-state nil)
	ad-do-it)))

(ad-activate 'back-to-indentation)

(dolist (key vemv/local-key-bindings-to-remove)
  (mapc (lambda (arg)
          (define-key (car key) arg nil))
        (cdr key)))

(dolist (key vemv/key-bindings-to-remove)
  (global-unset-key key))

(dolist (key vemv/key-bindings-to-dummy)
  (global-set-key key (argless)))

(maphash (lambda (key _)
	   (let* ((keyboard-macro (if (stringp key)
				      (read-kbd-macro key)
				      key)))
	     (global-set-key 
	      keyboard-macro
	      (argless (call-interactively (gethash key vemv/global-key-bindings))))))
  vemv/global-key-bindings)

(comm maphash (lambda (lang_key _)
	   (maphash (lambda (key _)
		      (let ((keyboard-macro (if (stringp key) (read-kbd-macro key) key)))
			(comm (define-key
				clojure-mode-map
				keyboard-macro
				(argless (call-interactively (gethash key (gethash lang_key vemv/local-key-bindings))))))))
		    vemv/local-key-bindings))
	 vemv/local-key-bindings)

(dolist (binding (vemv/partition 3 vemv/local-key-bindings))
  (define-key
    (car binding)
    (let ((k (second binding)))
      (if (stringp k)
          (read-kbd-macro k)
          k))
    (third binding)))

(condition-case ex (nrepl "localhost" 9119) ('error))

(setq redisplay-dont-pause t
      column-number-mode t
      echo-keystrokes 0.02
      inhibit-startup-message t
      transient-mark-mode t
      shift-select-mode nil
      require-final-newline t
      truncate-partial-width-windows nil
      delete-by-moving-to-trash nil
      confirm-nonexistent-file-or-buffer nil
      x-select-enable-clipboard t)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq vemv/main_frame (selected-frame))

(defun vemv/make-frame ()
  (make-frame `((width . ,(frame-width)) (height . ,(frame-height)))))

(defvar vemv/help-frame nil)

(defmacro vemv/get-help-frame ()
  `(if (and vemv/help-frame (terminal-live-p vemv/help-frame))
       vemv/help-frame
       (let ((frame (vemv/make-frame)))
	 (select-frame frame)
	 (vemv/maximize)
	 (setq vemv/help-frame frame))))

(defun vemv/display-help (buffer)
  (let ((frame (vemv/get-help-frame)))
    (select-frame frame)
;    (clojure-mode)
    ;(switch-to-buffer "*nREPL doc*")
    ;(when clj?)
    ;(set-window-buffer (frame-first-window frame) buffer)
    (delay (argless (select-window (frame-first-window vemv/help-frame))))
    (raise-frame)))

(defun vemv/display-completion (buffer)
  (select-window vemv/main_window)
  (set-window-buffer vemv/main_window buffer))

(comm add-to-list 'special-display-regexps '(".*" vemv/display-help))
(add-to-list 'special-display-buffer-names '("*Help*" vemv/display-completion))
(add-to-list 'special-display-buffer-names '("*nREPL doc*" vemv/display-help))
(add-to-list 'special-display-buffer-names '("*Ido Completions*" vemv/display-completion))
(add-to-list 'special-display-buffer-names '("*nrepl-error*" vemv/display-completion)) ; FIXME yanks the stacktrace to the responsible buffer instead
(add-to-list 'special-display-buffer-names '("*Diff*" vemv/display-completion))
