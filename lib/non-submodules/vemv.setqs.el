(provide 'vemv.setqs)

(setq vc-follow-symlinks t)
(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq initial-scratch-message "")

(setq cider-repl-display-help-banner' nil)
(setq ido-show-dot-for-dired t)

(setq pe/project-root-function (lambda (&rest _)
                                 (if (vemv/buffer-of-current-project? (current-buffer))
                                     vemv/project-root-dir
                                     default-directory)))

(add-to-list 'exec-path (concat vemv-home "/bin"))

(setq whitespace-style '(face lines-tail))

;; no .#filenames
(setq create-lockfiles nil)

(setq fiplr-ignored-globs
      ;; `directories` entries must be single-segment, i.e `/` doesn't work.
      '((directories (".git" "tmp" ".svn" ".hg" ".bzr" "tools" "res-vagrant"
                      ".paket" "doc" "bin" "assets" "public" "node_modules" "coverage"))
        (files (".#*" "*~" "*.so" "*.jpg" "*.png" "*.gif" "*.pdf" "*.gz" "*.zip" "*.DS_Store"
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
(setq sh-basic-offset 2)
(setq sh-indentation 2)
(setq smie-indent-basic 2)

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
 '(cljr-libspec-whitelist '("^cljsns" "^slingshot.test" "^monger.joda-time" "^monger.json" "^cljsjs" "leongersen.*")))

(defun cider-repl--banner () "")

(setq clojure-indent-style ':always-align)

(setq-default mode-line-format
              (list "  "
                    '(:eval (when (and (buffer-file-name) (buffer-modified-p)) "*"))
                    '(:eval (buffer-name))
                    " "
                    '(:eval (when (buffer-file-name) (propertize "%l:%c" 'face 'font-lock-line-and-column-face)))))

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
             '("\\.js.erb$" . js-mode))

(add-to-list 'auto-mode-alist
             '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))

(add-to-list 'auto-mode-alist
             '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

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

(setq vemv/launched nil)

(setq custom-file "~/.emacs.d/custom.el")

(setq visible-bell nil) ;; disable flickering
(setq ido-auto-merge-delay-time 99999) ;; prevents annoying folder switching. might be handy: (setq ido-max-directory-size 100000)

(setq mouse-buffer-menu-maxlen 99999)
(setq mouse-buffer-menu-mode-mult 1)

(put 'if 'lisp-indent-function nil)

(setq back-to-indentation-state nil)

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

;; Prevents annoying popups
(add-to-list 'special-display-buffer-names '("*Help*" vemv/display-completion))
(add-to-list 'special-display-buffer-names '("*Ido Completions*" vemv/display-completion))
(add-to-list 'special-display-buffer-names '("*Diff*" vemv/display-completion))

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

(setq company-dabbrev-char-regexp "\\sw\\|_\\|-\\|!\\|\\?\\|*\\|+")

;; https://github.com/company-mode/company-mode/issues/808
(setq company-backends (-remove (lambda (x)
                                  (and (listp x)
                                       (equal (car x) 'company-dabbrev-code)))
                                company-backends))

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
