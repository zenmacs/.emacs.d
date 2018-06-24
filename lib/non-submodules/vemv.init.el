;;; -*- lexical-binding: t -*-

(require 'vemv.lang)
(require 'vemv.project)
(require 'vemv.workspace)
(require 'vemv.data)
(require 'vemv.data.bindings)
(require 'vemv.theme)
(provide 'vemv.init)

(setq lexical-binding t)
(setq vc-follow-symlinks t)
(setq-default indent-tabs-mode nil)
(show-paren-mode 1)
(recentf-mode 1)
(ido-mode 1)
(blink-cursor-mode -1)
(tooltip-mode -1)

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(global-subword-mode)

(global-company-mode)

(add-to-list 'exec-path (concat vemv-home "/bin"))

(menu-bar-mode)
(global-auto-revert-mode t) ;; refresh buffers on external changes to the underlying files

(fset 'yes-or-no-p 'y-or-n-p)
(setq initial-scratch-message "")

(setq require-final-newline 't)
(global-hl-line-mode t)

(setq cider-repl-display-help-banner' nil)
(setq ido-show-dot-for-dired t)

(setq pe/project-root-function (lambda (&rest _)
                                 (if (vemv/buffer-of-current-project? (current-buffer))
                                  vemv/project-root-dir
                                  default-directory)))

(setq whitespace-style '(face lines-tail))
(global-whitespace-mode)

;; no .#filenames
(setq create-lockfiles nil)

;; https://github.com/clojure-emacs/cider/issues/2327
(setq cljr-warn-on-eval t)

(setq fiplr-ignored-globs
      ;; `directories` entries must be single-segment, i.e `/` doesn't work.
      '((directories (".git" ".svn" ".hg" ".bzr" "tools" "res-vagrant" ".paket" "doc" "bin" "assets" "public" "node_modules" "coverage"))
        (files (".#*" "*~" "*.so" "*.jpg" "*.png" "*.gif" "*.pdf" "*.gz" "*.zip" "*.js" "*.DS_Store"
                "*.md" "*.gitgnore" "*.scssc" "*.keep" "*.json" "LICENSE" "LICENCE" "license" "*.patch"
                "flask-server" "Makefile" "makefile" "*.txt" "*ignore""*.*rc" "*.map" ".last-compilation-digest-development"
                "*.ico" "Gemfile" "Rakefile" ".rspec" "*integration-testing*" "*node_modules*" "webpack" ".editorconfig" "*.pid"
                "*.workerjs" "*.MIT" "acorn" "AUTHORS" "*.APACHE2" "JSONStream" "babylon" "*.iml" "*.BSD" "*.log"
                "*.ru" "*.cache" "*.ts" "*.json5" "atob" "LICENSE-MIT" "public/assets/*" ".*"
                "*.ls" "loose-envify" "errno" "*.flow" "*.properties" "*.extract-native-dependencies" "*.targets"
                "*.sh" "*.ps1" "*.arcconfig" "Vagrantfile" "*.template" "*.nuspec" "*.emz" "1" "2" "*.svg"
                "*.ttf" ".lein-repl-history" "*.cur" "profile" ".figwheel-compile-stamp" "*.woff" "*.eor"
                "*.xml" "*.coffee" "*.lock" "*.markdown" "*.opts" "module-deps" ".nrepl-port" "repl-port"))))

(setq company-idle-delay nil) ;; no autopopup

(setq vemv/cljr-ast-load-counter 0)

(setq js-indent-level 2)
(setq css-indent-offset 2)

(custom-set-variables
 '(xref-prompt-for-identifier nil)
 '(cider-connection-message-fn nil)
 '(cider-use-tooltips nil)
 '(cider-preferred-build-tool "lein")
 '(cider-default-cljs-repl 'figwheel)
 '(cider-repl-display-help-banner nil)
 '(cider-font-lock-dynamically '(macro deprecated))
 '(cua-remap-control-v nil)
 '(cljr-after-warming-ast-cache-hook (lambda (_)
                                       (setq vemv/cljr-ast-load-counter (inc vemv/cljr-ast-load-counter))))
 '(ielm-prompt "ielm> ")
 '(paren-face-regexp "[][(){}#]")
 '(mac-mouse-wheel-smooth-scroll nil)
 '(nrepl-popup-stacktraces nil)
 '(pe/inline-folders nil)
 '(tree-widget-image-enable nil)
 '(cljr-auto-sort-ns nil)
 '(cljr-project-clean-prompt nil)
 '(cljr-favor-private-function nil)
 '(cljr-auto-clean-ns nil)
 '(cljr-libspec-whitelist '("^cljsns" "^slingshot.test" "^monger.joda-time" "^monger.json" "^cljsjs" "leongersen.*"))
 '(cljr-warn-on-eval nil))

(defun cider-repl--banner () "")

(setq clojure-indent-style ':always-align)

(setq-default mode-line-format
              (list "  "
                    '(:eval (when (and (buffer-file-name) (buffer-modified-p)) "*"))
                    '(:eval (buffer-name))
                    " "
                    '(:eval (when (buffer-file-name) (propertize "%l:%c" 'face 'font-lock-line-and-column-face)))))

;; initialized after customizing cua-remap-control-v
(cua-mode 1)

(when (not vemv-cleaning-namespaces)
  (add-hook 'clojure-mode-hook 'hs-minor-mode))

(setq pe/mode-line-format
      `(:eval (vemv/workspace-mode-line-format)))

(setq vemv/pe/mode-line-format
      `(:eval (vemv/pe/mode-line-format*)))

(setq tabbed-line-format
      (list
       '(:eval (concat (propertize "  %l:%c " 'face 'font-lock-line-and-column-face)
                       (when (and (not vemv-cider-connecting) (not vemv-cider-connected))
                         (propertize "Disconnected " 'face 'font-lock-line-and-column-face))
                       (when vemv/verbose-mode (propertize "Verbose " 'face 'font-lock-line-and-column-face))))
       '(:eval (when vemv-cider-connecting
                 (propertize "Connecting... " 'face 'vemv-cider-connection-face)))
       '(:eval (vemv/message-file-buffers-impl))))

(add-to-list 'auto-mode-alist
             '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))

(add-to-list 'auto-mode-alist
             '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

(add-hook 'ruby-mode-hook (argless (smartparens-mode)))

(add-hook 'emacs-lisp-mode-hook
          (argless (setq-local mode-line-format tabbed-line-format)))

(add-hook 'cider-repl-mode-hook
          (argless (setq-local mode-line-format vemv/pe/mode-line-format)))

(add-hook 'ielm-mode-hook
          (argless (setq-local mode-line-format vemv/pe/mode-line-format)))

(add-hook 'shell-mode-hook
          (argless (setq-local mode-line-format vemv/pe/mode-line-format)))

(dolist (mode (list 'emacs-lisp-mode-hook 'ruby-mode-hook 'clojure-mode-hook 'js-mode-hook 'css-mode-hook 'html-mode-hook))
  (add-hook mode (argless (call-interactively 'text-scale-increase))))

(add-hook 'clojure-mode-hook
          (argless (enable-paredit-mode)
                   (clj-refactor-mode 1)
                   (paren-face-mode 1)
                   (undo-tree-mode)
                   (cljr-add-keybindings-with-prefix "C-0")
                   (global-set-key (kbd "C-r") 'vemv/test-this-ns) ;; must be defined there. TODO: define all clojure bindings here
                   (setq-local mode-line-format tabbed-line-format)))

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
                   (setq vemv-cider-connecting nil)
                   (setq vemv-cider-connected t)
                   (vemv/show-clj-or-cljs-repl)
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
(setq mouse-wheel-follow-mouse 't)       ;; scroll window under mouse
(setq scroll-step 1)

(setq nrepl-hide-special-buffers t)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-show-error-buffer nil)
(add-hook 'cider-repl-mode-hook #'paredit-mode)

;; Important - remove keybindings before (vemv/initial-layout) so M-x cannot interrupt

(dolist (mode (list paredit-mode-map comint-mode-map undo-tree-map cider-mode-map))
  (mapc (lambda (arg)
          (define-key mode (vemv/keyboard-macro arg) nil))
        vemv/exhaustive-list-of-bindings-to-remove))

(dolist (key vemv/key-bindings-to-remove)
  (global-unset-key key))

(dolist (key vemv/key-bindings-to-dummy)
  (global-set-key key (argless)))

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

(shell-command-to-string "touch ~/.emacs.d/custom.el")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq visible-bell nil) ;; disable flickering
(setq ido-auto-merge-delay-time 99999) ;; prevents annoying folder switching. might be handy: (setq ido-max-directory-size 100000)

(setq mouse-buffer-menu-maxlen 99999)
(setq mouse-buffer-menu-mode-mult 1)

(delay 'vemv/clojure-init 1)

(delay (argless (if (window-system)
                    (set-face-attribute 'default nil :font vemv-font)))
       1)

(put 'if 'lisp-indent-function nil)

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

;; http://www.emacswiki.org/emacs/BackupDirectory
(setq backup-by-copying t
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

(advice-add 'cider-test-run-test :around 'vemv/apply-tests-verbosely)
(advice-add 'cider-test-run-ns-tests :around 'vemv/apply-tests-verbosely)
(advice-add 'cider-test-run-project-tests :around 'vemv/apply-tests-verbosely)
(advice-add 'cider-test-rerun-failed-tests :around 'vemv/apply-tests-verbosely)
(advice-add 'cider-test-show-report :around 'vemv/apply-tests-verbosely)

;; this is a defmacro so `M-x describe-key` doesn't show a giantic hash, freezing emacs
(defmacro vemv/set-keys-for-scope (scope source)
  `(maphash (lambda (key _)
             (let* ((keyboard-macro (vemv/keyboard-macro key)))
               (if (eq ,scope :global)
                   (global-set-key keyboard-macro
                                   (argless (call-interactively (gethash key ,source))))
                   (define-key ,scope
                     keyboard-macro
                     (argless (call-interactively (gethash key ,source)))))))
           ,source))

(vemv/set-keys-for-scope :global vemv/global-key-bindings)

(vemv/set-keys-for-scope clojure-mode-map vemv/clojure-key-bindings)

(vemv/set-keys-for-scope ruby-mode-map vemv/ruby-key-bindings)
