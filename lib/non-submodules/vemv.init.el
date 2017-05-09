;;; -*- lexical-binding: t -*-

;; NOTE: we don't use ac/auto-complete anymore. company now, since feb 2016

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;(unless (package-installed-p 'cider) ; package temporarily don't work on ubuntu 16 (TLS)
;  (package-refresh-contents)
;  (package-install 'cider))

(unless (package-installed-p 'company)
  (package-refresh-contents)
  (package-install 'company))

(unless (package-installed-p 'queue)
  (package-refresh-contents)
  (package-install 'queue))

(setq lexical-binding t)
(setq-default indent-tabs-mode nil)
(show-paren-mode 1)
(recentf-mode 1)
(ido-mode 1)
(cua-mode 1)
(blink-cursor-mode -1)
;; (setq yas-use-menu nil)
;; XXX detect nrepl's project, open the latest file within that proj.
;; XXX add send hook to ns-eval-form
(require 'yasnippet)
(require 'saveplace)
(require 'dash)
(require 'popup)
(require 'smex)
(require 'ruby-mode)
(require 'ruby-end)
(require 'cider)
(require 'epl)
(require 'pkg-info)
(require 'spinner)
(require 'comint)
(require 'dirtree)
(require 'es-lib)
(require 'es-windows)
(require 'project-explorer)
(require 'paredit)
(require 'haskell-mode)
(require 'undo-tree)
(require 'nyan-mode)
(require 'vemv.lang)
(require 'vemv.data)
(require 'vemv.theme)
(provide 'vemv.init)

(global-company-mode)

(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'css-mode-hook (lambda () (rainbow-mode 1)))

(add-to-list 'exec-path (concat vemv-home "/bin"))

(yas-reload-all)
(menu-bar-mode)
(yas-global-mode 1)
(global-auto-revert-mode t) ;; refresh buffers on external changes to the underlying files

(fset 'yes-or-no-p 'y-or-n-p)
(setq initial-scratch-message "")
(setq yas-use-menu nil)

(setq require-final-newline 't)
(global-hl-line-mode t)

(setq cider-repl-display-help-banner' nil)
(setq ido-show-dot-for-dired t)

(custom-set-variables
 '(cider-connection-message-fn nil)
 '(haskell-mode-hook '(turn-on-haskell-indentation))
 '(tree-widget-image-enable nil)
 '(nrepl-popup-stacktraces nil)
 '(ielm-prompt "ielm> "))

(setq clojure-indent-style ':align-arguments)

(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'undo-tree-mode)
(add-hook 'clojure-mode-hook (argless (local-set-key (kbd "RET") 'newline-and-indent)))

(comm add-hook 'clojure-mode-hook (argless (if-let (ns (clojure-find-ns))
					      (progn
						(nrepl-eval-ns-form)
						(with-current-buffer "*nrepl*"
						    (nrepl-set-ns ns)))
					      (when (vemv/contains? (buffer-name) ".clj")
						(let ((name (substring (buffer-name) ; XXX needs prefixing
									     0
									     (- (length (buffer-name)) 4)))) ; removes the .clj
						  (comm save-excursion ;; XXX nrepl-load-current-buffer
						    (beginning-of-buffer)
						    (insert (concat "(ns "
								    name
								    ")"))
						    (nrepl-eval-ns-form))
						  (with-current-buffer "*nrepl*"
						    (nrepl-set-ns name)))))))

(add-hook 'ruby-mode-hook 'enable-paredit-mode)
(add-hook 'ruby-mode-hook 'electric-pair-mode)

(add-hook 'html-mode-hook 'electric-pair-mode)

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(comm
  (add-hook 'nrepl-mode-hook 'enable-paredit-mode)
  (comm add-hook 'nrepl-connected-hook (argless (delay (argless
  						 ;;(delete-window)
  						 (vemv/next-window)
  						 (switch-to-buffer "*nrepl*")
  						 (vemv/next-window)
  						 (switch-to-buffer "*ielm*")
  						 (select-window vemv/main_window) ;; apparently needed only on the first run! (this comment was placed in the slime era)
                                                   (nrepl-eval-ns-form)) 2)))
)

(setq cider-cljs-lein-repl
  (if gpm-using-nrepl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))"
      ""))

(add-hook 'cider-connected-hook (argless
  (delay (argless
          (select-window vemv/main_window)
          (vemv/next-window)
          (switch-to-buffer "*cider-repl CLJS horizon*")))
          (select-window vemv/main_window)
         2)
  )

(add-hook 'kill-buffer-hook (argless (let ((killed (buffer-name (current-buffer))))
                                       (setq vemv/open_file_buffers
                                             (filter (lambda (_) (not (equal _ killed)))
                                                     vemv/open_file_buffers)))))
(add-hook 'html-mode-hook
          (lambda()
            (setq sgml-basic-offset 2)
            (setq indent-tabs-mode nil)))

(set-default 'truncate-lines t)
(setq-default save-place t)

(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))

(setq mouse-wheel-scroll-amount '(4 ((shift) . 4)))
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1)

(setq nrepl-hide-special-buffers t)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-show-error-buffer nil)
(add-hook 'cider-repl-mode-hook #'paredit-mode)

(setenv "PATH" (concat (getenv "PATH") ":" vemv-home "/bin"))
(setenv "HORIZON_FG_HARD_RELOAD" "true")
(setenv "HORIZON_FIGWHEEL_NO_NREPL" "true")
(setenv "HORIZON_FIGWHEEL_IP" "0.0.0.0")
;; (setenv "USE_YOURKIT_AGENT" "true")

;; restart
;; tree: refresh on adds
;; javadoc
;; popup doc for defvar
;; goto fn defs
(setq vemv/launched nil)

(if (window-system) (vemv/maximize))

(split-window-vertically)
(enlarge-window 8)

(setq default-directory vemv-home)
(let ((default-directory (concat vemv-home "/gpm/src/"))) ;; trailing slash required
  (call-interactively 'project-explorer-open)
  (enlarge-window-horizontally -20))

(vemv/next-window)

(setq vemv/main_window (selected-window))
(vemv/next-window)

(comm
  (split-window-horizontally)

  (switch-to-buffer "*scratch*")

  (ielm)
  (setq vemv/repl1 (selected-window))
  (vemv/next-window))

(let ((default-directory (concat vemv-home "/gpm/src")))
  (sh))
(paredit-mode)
;(auto-complete-mode)

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
				  (if (vemv/ends-with head "ido.last")
				      (second recentf-list)
				      head)))))))
       1)


;; FONT SIZE -> 13 for laptop, 11 for desktop
(delay (argless (if (window-system) (set-face-attribute 'default nil :font vemv-font))) 1)

(put 'if 'lisp-indent-function nil)

(comment (setq comment-indent-function (argless (save-excursion (forward-line -1) (current-indentation)))))

(defadvice save-buffers-kill-emacs (around no-y-or-n activate) ; switches the expected input from "yes no" to "y n" on exit-without-save
  (flet ((yes-or-no-p (&rest args) t)
         (y-or-n-p (&rest args) t))
    ad-do-it))

(comm defadvice eval-ns-form (around nrepl-eval-ns-form)
  (save-excursion
    (when (clojure-find-ns)
      (goto-char (match-beginning 0))
      (vemv/send :slime)))
  ad-do-it)

(comm ad-activate 'eval-ns-form)

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
(comm
(condition-case ex (nrepl "localhost" 9119) ('error))
(condition-case ex (nrepl "localhost" 9120) ('error))
)
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
    ;; (clojure-mode)
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
(comm add-to-list 'special-display-buffer-names '("*nREPL doc*" vemv/display-help))
(add-to-list 'special-display-buffer-names '("*Ido Completions*" vemv/display-completion))
(comm add-to-list 'special-display-buffer-names '("*nrepl-error*" vemv/display-completion)) ; FIXME yanks the stacktrace to the responsible buffer instead
(add-to-list 'special-display-buffer-names '("*Diff*" vemv/display-completion))

(defun undo (&rest args)
  (interactive)
  (apply 'undo-tree-undo args))

(global-set-key [kp-delete] 'delete-char) ;; OS X

(setq ;; http://www.emacswiki.org/emacs/BackupDirectory
   backup-by-copying t      ; don't clobber symlinks
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))

(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
	   (and (not current-prefix-arg)
		(member major-mode '(emacs-lisp-mode lisp-mode
						     clojure-mode    scheme-mode
						     haskell-mode    ruby-mode
						     rspec-mode      python-mode
						     c-mode          c++-mode
						     objc-mode       latex-mode
						     plain-tex-mode))
		(let ((mark-even-if-inactive transient-mark-mode))
		  (indent-region (region-beginning) (region-end) nil))))))

; ensure nrepl opens horizon project
(find-file (concat vemv-home "/gpm/src/horizon/src/horizon/desktop/core.cljs"))
