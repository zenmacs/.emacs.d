;;; -*- lexical-binding: t -*-

;; NOTE: we don't use ac/auto-complete anymore. company now, since feb 2016

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(dolist (package '(cider company queue fiplr clojure-mode clj-refactor company-quickhelp dash simpleclip helm-ag git-timemachine))
        (unless (package-installed-p package)
                (package-refresh-contents)
                (package-install package)))

;; Eases editing locally-modified packages.
;; Example:
;; Fork a package
;; Copy the relevant file to ~/.emacs.d/elpa/the-package
;; rm ~/.emacs.d/elpa/the-package/foo.elc
;; restart emacs.
(add-hook 'compilation-finish-functions (lambda (b _) (kill-buffer b)))
(byte-recompile-directory (expand-file-name "~/.emacs.d/elpa") 0)

(setq lexical-binding t)
(setq vc-follow-symlinks t)
(setq-default indent-tabs-mode nil)
(show-paren-mode 1)
(recentf-mode 1)
(ido-mode 1)
(blink-cursor-mode -1)

;; defonces
(setq cider-launched nil)
(setq vemv-cider-connecting nil)
(setq vemv-cider-connected nil)
;; XXX last from a file
(setq vemv/current-project "gpm")
(setq vemv/running-project nil)
(setq vemv/running-project-root-dir nil)
(setq vemv/running-project-type nil)

(require 'saveplace)
(require 'dash)
(require 'popup)
(require 'smex)
(require 'cider)
(require 'epl)
(require 'pkg-info)
(require 'spinner)
(require 'comint)
(require 'es-lib)
(require 'es-windows)
(require 'project-explorer)
(require 'paredit)
(require 'undo-tree)
(require 's)
(require 'clj-refactor)
(require 'fiplr)
(require 'helm-ag)
(require 'vemv.lang)
(require 'vemv.project)
(require 'vemv.data)
(require 'vemv.data.bindings)
(require 'vemv.theme)
(provide 'vemv.init)

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(global-subword-mode)

(global-company-mode)
(company-quickhelp-mode 1)

(add-to-list 'exec-path (concat vemv-home "/bin"))

(menu-bar-mode)
(global-auto-revert-mode t) ;; refresh buffers on external changes to the underlying files

(fset 'yes-or-no-p 'y-or-n-p)
(setq initial-scratch-message "")

(setq require-final-newline 't)
(global-hl-line-mode t)

(setq cider-repl-display-help-banner' nil)
(setq ido-show-dot-for-dired t)

;; XXX should be per-project.
;; when doing that improvement, keep in mind that whitespace-mode doesn't re-render by a mere `setq`
;; (setq changes are only applied after doing `M-x whitespace-mode` twice)
(setq whitespace-line-column 131)
(setq whitespace-style '(face lines-tail))
(global-whitespace-mode)

;; no .#filenames
(setq create-lockfiles nil)

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
                                   "Indexing")))))))

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
                      "*.xml" "*.coffee" "*.lock" "*.markdown" "*.opts" "module-deps" ".nrepl-port" "repl-port"))))

(setq company-idle-delay nil) ;; no autopopup

(custom-set-variables
 '(cider-connection-message-fn nil)
 '(cider-repl-display-help-banner nil)
 '(cider-font-lock-dynamically '(macro deprecated))
 '(cua-remap-control-v nil)
 '(ielm-prompt "ielm> ")
 '(mac-mouse-wheel-smooth-scroll nil)
 '(nrepl-popup-stacktraces nil)
 '(pe/inline-folders nil)
 '(tree-widget-image-enable nil)
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

     ("expectations" . "horizon.test-helpers.expectations")))
 '(cljr-project-clean-prompt nil)
 '(cljr-favor-private-function nil)
 '(cljr-auto-clean-ns nil)
 '(cljr-libspec-whitelist '("^cljsns" "^slingshot.test" "^monger.joda-time" "^monger.json" "^cljsjs" "^horizon.controls.devcards" "^goog" ".*card.*" ".*asDatepicker.*" "horizon.desktop.core" "horizon.controls.bootstrap" "leongersen.*" "horizon.desktop.layout.page" "horizon.common.macros" "horizon.desktop.bootstrap" "rabbit.stomp"))
 '(cljr-warn-on-eval nil))

(defun cider-repl--banner () "")

(setq clojure-indent-style ':always-align)

(setq-default mode-line-format (list "  "
                                     '(:eval (when (and (buffer-file-name) (buffer-modified-p)) "*"))
                                     '(:eval (buffer-name))
                                     " "
                                     '(:eval (when (buffer-file-name) (propertize "%l:%c" 'face 'font-lock-line-and-column-face)))))

;; initialized after customizing cua-remap-control-v
(cua-mode 1)

(when (not vemv-cleaning-namespaces)
  (add-hook 'clojure-mode-hook 'hs-minor-mode))

(add-hook 'clojure-mode-hook (argless (enable-paredit-mode)
                                      (clj-refactor-mode 1)
                                      (undo-tree-mode)
                                      (cljr-add-keybindings-with-prefix "C-0")
                                      (global-set-key (kbd "C-r") 'vemv/test-this-ns) ;; must be defined there. TODO: define all clojure bindings here
                                      (setq-local mode-line-format
                                                  (list
                                                   "  "
                                                   '(:eval (when (buffer-modified-p) (propertize "*" 'face 'font-lock-function-name-face)))
                                                   '(:eval (vemv/message-file-buffers-impl))
                                                   '(:eval (propertize " %l:%c" 'face 'font-lock-line-and-column-face))
                                                   '(:eval (when (and (not vemv-cider-connecting) (not vemv-cider-connected)) (propertize " Disconnected" 'face 'font-lock-line-and-column-face)))
                                                   '(:eval (when vemv/verbose-mode (propertize " Verbose" 'face 'font-lock-line-and-column-face)))
                                                   '(:eval (when vemv/current-project (propertize (concat " " vemv/current-project) 'face 'font-lock-line-and-column-face)))
                                                   '(:eval (when vemv-cider-connecting (propertize " Connecting..." 'face 'vemv-cider-connection-face)))))))

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(add-hook 'ielm-mode-hook 'enable-paredit-mode)

(when (not vemv-cleaning-namespaces)
  (setq cider-cljs-lein-repl
        (if vemv/using-nrepl
          "(do (require 'figwheel-sidecar.repl-api)
               
               (try
                 (require 'figwheel-sidecar.system)
                 (alter-var-root #'figwheel-sidecar.system/repl-function-docs (constantly \"Results: Stored in vars *1, *2, *3, *e holds last exception object\"))
                 (catch Throwable e))
               (figwheel-sidecar.repl-api/start-figwheel!)
               (figwheel-sidecar.repl-api/cljs-repl))"
          "")))

(add-hook 'cider-connected-hook
          (argless
           (delay (argless
                   (vemv/show-clj-or-cljs-repl)
                   (setq vemv-cider-connecting nil)
                   (setq vemv-cider-connected t)
                   (when (not vemv-cleaning-namespaces)
                     (vemv/advice-nrepl)))
                  2)))

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

;; Important - remove keybindings before (vemv/initial-layout) so M-x cannot interrupt

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

(dolist (binding (vemv/partition 3 vemv/local-key-bindings))
        (define-key
          (car binding)
          (let ((k (second binding)))
            (if (stringp k)
              (read-kbd-macro k)
              k))
          (third binding)))

(setq vemv/launched nil)

(vemv/initial-layout)

(setq custom-file "~/.emacs.d/custom.el") ;; touch on install!
(load custom-file)

(setq visible-bell nil) ;; disable flickering
(setq ido-auto-merge-delay-time 99999) ;; prevents annoying folder switching. might be handy: (setq ido-max-directory-size 100000)

(setq mouse-buffer-menu-maxlen 99999)
(setq mouse-buffer-menu-mode-mult 1)

(mapcar (lambda (f)
          (let ((emacs-path (concat vemv-home "/.emacs.d/lib/non-submodules")))
                (vemv/open (concat emacs-path "/vemv." f ".el"))
                (switch-to-buffer "*scratch*")))
      '("init" "lang" "project" "theme" "shortcuts.global"))

(delay 'vemv/clojure-init 1)

;; FONT SIZE -> 13 for laptop, 11 for desktop
(delay (argless (if (window-system)
                  (set-face-attribute 'default nil :font vemv-font)))
       1)

(put 'if 'lisp-indent-function nil)

;; switches the expected input from "yes no" to "y n" on exit-without-save
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
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

(defun vemv/display-completion (buffer)
  (vemv/safe-select-window vemv/main_window)
  (set-window-buffer vemv/main_window buffer))

;; Prevents annoying popups
(add-to-list 'special-display-buffer-names '("*Help*" vemv/display-completion))
(add-to-list 'special-display-buffer-names '("*Ido Completions*" vemv/display-completion))
(add-to-list 'special-display-buffer-names '("*Diff*" vemv/display-completion))

(defun undo (&rest args)
  (interactive)
  (apply 'undo-tree-undo args))

(global-set-key [kp-delete] 'delete-char) ;; OSX

(setq ;; http://www.emacswiki.org/emacs/BackupDirectory
 backup-by-copying t ;; don't clobber symlinks
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(dolist (command '(yank yank-pop))
        (eval `(defadvice ,command (after indent-region activate)
                 (and (not current-prefix-arg)
                      (member major-mode '(emacs-lisp-mode lisp-mode clojure-mode))
                      (let ((mark-even-if-inactive transient-mark-mode))
                        (indent-region (region-beginning) (region-end) nil))))))

(delay
 (argless (setq vemv/project-explorer-initialized t))
 12)

(delay
 ;; every 5 seconds. in practice, not so often b/c `vemv/refreshing-caches` (timestamp lock)
 (argless (run-with-timer 0 5 'vemv/refresh-file-caches))
 60)

(setq company-dabbrev-char-regexp "\\sw\\|-")

;; monkeypatch for https://github.com/clojure-emacs/cider/issues/2102
(defun cider--format-buffer (formatter)
  "Format the contents of the current buffer.

Uses FORMATTER, a function of one argument, to convert the string contents
of the buffer into a formatted string."
  (let* ((original (substring-no-properties (buffer-string)))
         (formatted (funcall formatter original)))
        (if (or (not formatted) (equal original formatted))
          (when (not formatted) (vemv/echo "Buffer has broken syntax, cannot format"))
          (erase-buffer)
          (insert formatted))))

(advice-add 'helm-ag--edit :after 'vemv/ag-replace)
