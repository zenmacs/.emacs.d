;;; -*- lexical-binding: t -*-

;; NOTE: we don't use ac/auto-complete anymore. company now, since feb 2016

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'cider)
  (package-refresh-contents)
  (package-install 'cider))

(unless (package-installed-p 'company)
  (package-refresh-contents)
  (package-install 'company))

(unless (package-installed-p 'queue)
  (package-refresh-contents)
  (package-install 'queue))
  
(unless (package-installed-p 'fiplr)
  (package-refresh-contents)
  (package-install 'fiplr))
  
(unless (package-installed-p 'clojure-mode)
  (package-refresh-contents)
  (package-install 'clojure-mode))

(unless (package-installed-p 'clj-refactor)
  (package-refresh-contents)
  (package-install 'clj-refactor))

(setq lexical-binding t)
(setq-default indent-tabs-mode nil)
(show-paren-mode 1)
(recentf-mode 1)
(ido-mode 1)
(cua-mode 1)
(blink-cursor-mode -1)
;; (setq yas-use-menu nil)
;; XXX detect nrepl's project, open the latest file within that proj.
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
(require 's)
(require 'clj-refactor)
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

;; for clojure-factor c n
(setq whitespace-line-column 240)

(setq pe/mode-line-format
  `(:eval (concat (propertize
                   (concat "  "
                           (file-name-nondirectory
                            (directory-file-name
                             default-directory)))
                   'face 'font-lock-function-name-face
                   'help-echo default-directory)
                  (when (or pe/reverting pe/filter-regex)
                    (format " (%s)"
                            (concat
                             (when pe/filter-regex
                               "Filtered")
                             (and pe/filter-regex
                                  pe/reverting
                                  ", ")
                             (when pe/reverting
                               "Indexing"))))
                  )))

(setq fiplr-ignored-globs
      ;; `directories` is semi-useless. do not alter but also do not bother adding entries
      '((directories (".git" ".svn" ".hg" ".bzr" "tools" "res-vagrant" ".paket" "doc" "src/horizon/resources/public/js" "src/.sass-cache" "src/horizon/node_modules"  "src/horizon/node_modules/*" "src/horizon/node_modules*" "src/horizon/node_modules/**" "src/utils" "src/integration-testing" "src/integration-testing/spec/features" "src/integration-testing/spec" "src/integration\-testing/public"))
        (files (".#*" "*~" "*.so" "*.jpg" "*.png" "*.gif" "*.pdf" "*.gz" "*.zip" "*.js" "*.DS_Store"
                "*.md" "*.gitgnore" "*.scssc" "*.keep" "*.json" "LICENSE" "LICENCE" "license" "*.patch"
                "flask-server" "Makefile" "makefile" "*.txt" "*.yml" "*.html" "*ignore" "*.rb" "*.*rc" "*.map"
                "*.ico" "*.css" "*.erb" "Gemfile" "Rakefile" ".rspec" "*integration-testing*" "*node_modules*"
                "*.workerjs" "*.MIT" "acorn" "AUTHORS" "*.APACHE2" "JSONStream" "babylon" "*.iml" "*.BSD" "*.log" "*.rake" "*.ru"
                "*.ls" "loose-envify" "errno" "*.flow" "*.properties" "*.extract-native-dependencies" "*.targets"
                "*.sh" "*.ps1" "*.arcconfig" "Vagrantfile" "*.template" "*.nuspec" "*.emz" "1" "2" "*.svg"
                 "*.ttf" ".lein-repl-history" "*.scss" "*.cur" "profile" ".figwheel-compile-stamp" "*.woff" "*.eor"
                "*.xml" "*.coffee" "*.lock" "*.markdown" "*.opts" "module-deps"))))

(custom-set-variables
 '(mac-mouse-wheel-smooth-scroll nil)
 '(cider-connection-message-fn nil)
 '(haskell-mode-hook '(turn-on-haskell-indentation))
 '(cider-repl-display-help-banner nil)
 '(pe/inline-folders nil)
 '(tree-widget-image-enable nil)
 '(nrepl-popup-stacktraces nil)
 '(ielm-prompt "ielm> ")
 '(cljr-auto-sort-ns nil)
 '(cljr-magic-require-namespaces
   '(("io"   . "clojure.java.io")
    ("set"  . "clojure.set")
    ("str"  . "clojure.string")
    ("walk" . "clojure.walk")
    ("zip"  . "clojure.zip")
    ("om"  . "om.core")
    ("pprint" . "cljs.pprint")
    ("html" . "sablono.core")
    
    ("common.routing"  . "horizon.common.routing")
    ("s" . "horizon.common.state.core")
    ("config" . "horizon.common.config")
    ("log" . "horizon.common.logging")
    ("c" . "horizon.common.config")
    ("env" . "horizon.common.env")
    ("constants" . "horizon.common.config-constants")
    ("dispatcher" . "horizon.common.dispatcher")
    ("i18n" . "horizon.common.i18n.core")
    ("m" . "horizon.common.messaging.core")
    ("p" . "horizon.common.protocols")
    ("service" . "horizon.common.service.core")
    ("actions" . "horizon.common.state.actions")
    ("tp" . "horizon.common.time-periods")
    ("ptp" . "horizon.common.utils.plant-time-period")
    ("utils.string" . "horizon.common.utils.string")
    
    ("utils.css-transitions-group" . "horizon.controls.utils.css-transitions-group")
    ("utils.reactive" . "horizon.controls.utils.reactive")
    ("utils.time" . "horizon.controls.utils.time-core")
    ("widgets.combobox" . "horizon.controls.widgets.combobox")
    ("widgets.comboboxes.status" . "horizon.controls.widgets.comboboxes.status")
    ("widgets.data-input" . "horizon.controls.widgets.data-input")
    ("widgets.timestamp" . "horizon.controls.widgets.timestamp")

    ("domain.routing" . "horizon.domain.routing")
    ("service-helpers" . "horizon.domain.service-helpers")
    
    ("expectations" . "horizon.test-helpers.expectations")
    ))
  '(cljr-project-clean-prompt nil)
  '(cljr-favor-private-function nil)
  '(cljr-auto-clean-ns nil)
  '(cljr-libspec-whitelist '("^cljsns" "^slingshot.test" "^monger.joda-time" "^monger.json" "^cljsjs" "^horizon.controls.devcards" "^goog" ".*card.*" ".*asDatepicker.*" "horizon.desktop.core" "horizon.controls.bootstrap" "leongersen.*" "horizon.desktop.layout.page" "horizon.common.macros" "horizon.desktop.bootstrap" "rabbit.stomp"))
  '(cljr-warn-on-eval nil)
 )

(defun cider-repl--banner () "")

(setq clojure-indent-style ':align-arguments)

(setq-default mode-line-format (list "  "
                             '(:eval (when (and (buffer-file-name) (buffer-modified-p)) "*"))
                             '(:eval (buffer-name))
                             " "
                             '(:eval (when (buffer-file-name) (propertize "%l:%c" 'face 'font-lock-line-and-column-face)))))

(setq vemv-cider-connecting nil)
(setq vemv-cider-connected nil)

(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(when (not vemv-cleaning-namespaces)
  (add-hook 'clojure-mode-hook 'hs-minor-mode))
(add-hook 'clojure-mode-hook 'undo-tree-mode)
(add-hook 'clojure-mode-hook (argless (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'clojure-mode-hook (argless (clj-refactor-mode 1)
                                      (cljr-add-keybindings-with-prefix "<f5>")
                                      (setq-local mode-line-format
                                        (list
                                          "  "
                                          '(:eval (when (buffer-modified-p) (propertize "*" 'face 'font-lock-function-name-face)))
                                          '(:eval (vemv/message-file-buffers-impl))
                                          '(:eval (propertize " %l:%c" 'face 'font-lock-line-and-column-face))
                                          '(:eval (when (and (not vemv-cider-connecting) (not vemv-cider-connected)) (propertize " Disconnected" 'face 'font-lock-line-and-column-face)))
                                          '(:eval (when vemv-cider-connecting (propertize " Connecting..." 'face 'vemv-cider-connection-face)))
                                          ))))

(add-hook 'ruby-mode-hook 'enable-paredit-mode)
(add-hook 'ruby-mode-hook 'electric-pair-mode)

(add-hook 'html-mode-hook 'electric-pair-mode)

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(add-hook 'ielm-mode-hook 'enable-paredit-mode)

(when (not vemv-cleaning-namespaces)
  (setq cider-cljs-lein-repl
    (if gpm-using-nrepl
        "(do (require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!)
             (figwheel-sidecar.repl-api/cljs-repl))"
        "")))

(add-hook 'cider-connected-hook (argless
  (delay (argless
          (select-window vemv/main_window)
          (vemv/next-window)
          (switch-to-buffer "*cider-repl CLJS horizon*")))
          (select-window vemv/main_window)
          (setq vemv-cider-connecting nil)
          (setq vemv-cider-connected t)
          (comment ;; XXX breaks cljs repl
            when (not vemv-cleaning-namespaces)
            (vemv/advice-nrepl))
         2)
  )

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

(defun vemv-source (filename)
  (mapcar
    (lambda (x)
      (let* ((xy (s-split "=" (s-chop-prefix "+" x)))
             (x (car xy))
             (y (car (last xy))))
             (setenv x y)))
    (-filter
      (lambda (x) (vemv/starts-with x "+"))
      (s-split
        "\n"
        (shell-command-to-string (concat "diff -u  <(true; export) <(source " filename "; export) | tail -n +4"))))))

(vemv-source "/Users/vemv/gpm/src/environment.sh")
(vemv-source "/Users/vemv/gpm/src/custom-environment.sh")
(vemv-source "/Users/vemv/.ldap")

(setenv "PATH" (concat (getenv "PATH") ":" vemv-home "/bin"))
(setenv "GPM_SRC" "/Users/vemv/gpm/src")
(setenv "FIGW_ADDR" "0.0.0.0")
(setenv "EXTEND_IPERSISTENTVECTOR" "true")
(setenv "FIGWHEEL_DESKTOP_NOTIFICATIONS" "true")
(setenv "HORIZON_DISABLE_SPINNERS_ANIMATION" "true")
(setenv "ENABLE_DEVCARDS_IN_DEV" "true")
;; (setenv "HORIZON_FG_HARD_RELOAD" "true")
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
  (enlarge-window-horizontally -20)
  (setq vemv/project-explorer-window (selected-window)))

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

(vemv/next-window)

(setq vemv/repl2 (selected-window))

(delay (argless 
        (select-window vemv/repl2)
        (switch-to-buffer "*shell-1*")
        (enable-paredit-mode)
        (select-window vemv/main_window))
       1)
       
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
              (let* ((head (car recentf-list))
                     (the-file (ignore-errors
                                 (if (vemv/ends-with head "ido.last")
                                     (second recentf-list)
                                     head))))
                     (when the-file
                       (vemv/open
                         (if (vemv/contains? the-file "/gpm/src/horizon") ; ensure nrepl opens horizon project
                           the-file
                           "/Users/vemv/gpm/src/horizon/src/horizon/desktop/core.cljs"))
                       (delay 'vemv/show-current-file-in-project-explorer 3)))))
         
         (advice-add 'pe/show-buffer :after 'vemv/after-file-open)
         (advice-add 'vemv/fiplr :after 'vemv/after-file-open)
         (advice-add 'vemv/open :after 'vemv/after-file-open)
         (advice-add 'vemv/next-file-buffer :after 'vemv/after-file-open)
         (advice-add 'vemv/previous-file-buffer :after 'vemv/after-file-open)
         (advice-add 'vemv/close-this-buffer :after 'vemv/after-file-open)
         1))


;; FONT SIZE -> 13 for laptop, 11 for desktop
(delay (argless (if (window-system) (set-face-attribute 'default nil :font vemv-font))) 1)

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

(delay
  (argless
    (setq vemv/project-explorer-initialized t)
  12))
  
(delay
  (argless
    (run-with-timer 0 5 'vemv/refresh-file-caches) ; every 5 seconds. in practice, not so often b/c `vemv/refreshing-caches` (timestamp lock)
  60))

(setq company-dabbrev-char-regexp "\\sw\\|-")