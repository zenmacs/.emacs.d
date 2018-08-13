(require 'vemv.theme)
(provide 'vemv.setqs)

(fset 'yes-or-no-p 'y-or-n-p)
(put-clojure-indent 'with 1)

(setq-default truncate-lines t)
(setq-default save-place t)
(setq-default indent-tabs-mode nil)
(setq-default mode-line-format
              (list "  "
                    '(:eval (when (and (buffer-file-name) (buffer-modified-p)) "*"))
                    '(:eval (buffer-name))
                    " "
                    '(:eval (when (buffer-file-name) (propertize "%l:%c" 'face 'font-lock-line-and-column-face)))))

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(add-to-list 'exec-path (concat vemv-home "/bin"))

;; Prevents annoying popups
(add-to-list 'special-display-buffer-names '("*Messages*" vemv/display-completion))
(add-to-list 'special-display-buffer-names '("*xref*" vemv/display-completion))
(add-to-list 'special-display-buffer-names '("*Help*" vemv/display-completion))
(add-to-list 'special-display-buffer-names '("*Ido Completions*" vemv/display-completion))
(add-to-list 'special-display-buffer-names '("*Diff*" vemv/display-completion))
(add-to-list 'sp-no-reindent-after-kill-modes 'haml-mode)

(setq pe/project-root-function (lambda (&rest _)
                                 vemv/project-root-dir))

(setq pe/mode-line-format
      `(:eval (vemv/workspace-mode-line-format)))

(setq vemv/pe/mode-line-format
      `(:eval (vemv/pe/mode-line-format*)))

(setq tabbed-line-format
      (list
       '(:eval (concat (propertize "  %l:%c " 'face 'font-lock-line-and-column-face)
                       (when debug-on-error (propertize "debug-on-error " 'face 'font-lock-warning-face))
                       (when (and (not vemv-cider-connecting) (not vemv-cider-connected))
                         (propertize "Disconnected " 'face 'font-lock-line-and-column-face))
                       (when vemv/verbose-mode (propertize "Verbose " 'face 'font-lock-line-and-column-face))))
       '(:eval (when vemv-cider-connecting
                 (propertize "Connecting... " 'face 'vemv-cider-connection-face)))
       '(:eval (vemv/message-file-buffers-impl))))

;; http://www.emacswiki.org/emacs/BackupDirectory
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; https://github.com/company-mode/company-mode/issues/808
(setq company-backends (-remove (lambda (x)
                                  (and (listp x)
                                       (equal (car x) 'company-dabbrev-code)))
                                company-backends))

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      back-to-indentation-state nil
      backup-directory-alist `((".*" . ,temporary-file-directory))
      backup-directory-alist `((".*" . ,temporary-file-directory))
      cider-repl-display-help-banner' nil
      cider-repl-pop-to-buffer-on-connect nil
      cider-show-error-buffer nil
      clojure-indent-style ':always-align
      column-number-mode t
      company-dabbrev-char-regexp "\\sw\\|_\\|-\\|!\\|\\?\\|*\\|+"
      company-idle-delay nil ;; no autopopup
      confirm-nonexistent-file-or-buffer nil
      create-lockfiles nil ;; no .#filenames
      css-indent-offset 2
      custom-file "~/.emacs.d/custom.el"
      delete-by-moving-to-trash nil
      echo-keystrokes 0.02
      helm-display-header-line nil
      hi-lock-file-patterns-policy 'never
      highlight-indent-guides-character ?·
      highlight-indent-guides-method 'character
      highlight-indent-guides-responsive 'top
      ido-auto-merge-delay-time 99999 ;; prevents annoying folder switching. might be handy: (setq ido-max-directory-size 100000)
      ido-show-dot-for-dired t
      inhibit-startup-message t
      initial-scratch-message ""
      js-indent-level 2
      mouse-buffer-menu-maxlen 99999
      mouse-buffer-menu-mode-mult 1
      mouse-wheel-follow-mouse 't ;; scroll window under mouse
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-scroll-amount '(4 ((shift) . 4))
      nrepl-hide-special-buffers t
      pe/cache-enabled nil
      pe/get-directory-tree-async-delay 0.00000000000000001
      pe/width 21
      redisplay-dont-pause t
      require-final-newline t
      ruby-insert-encoding-magic-comment nil
      scroll-step 1
      sh-basic-offset 2
      sh-indentation 2
      shift-select-mode nil
      smie-indent-basic 2
      transient-mark-mode t
      truncate-partial-width-windows nil
      vc-follow-symlinks t
      vemv/cljr-ast-load-counter 0
      vemv/launched nil
      visible-bell nil ;; disable flickering
      whitespace-style '(face lines-tail)
      x-select-enable-clipboard t)

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

(setq fiplr-ignored-globs
      ;; `directories` entries must be single-segment, i.e `/` doesn't work.
      '((directories (".git" "tmp" ".svn" ".hg" ".bzr" "tools" "res-vagrant" "resources" ".lumo-cache"
                      ".paket" "doc" "bin" "assets" "public" "node_modules" "coverage" "target"))
        (files (".#*" "*~" "*.so" "*.jpg" "*.png" "*.gif" "*.pdf" "*.gz" "*.zip" "*.DS_Store"
                "*.md" "*.gitgnore" "*.scssc" "*.keep" "*.json" "LICENSE" "LICENCE" "license" "*.patch"
                "flask-server" "Makefile" "makefile" "*.txt" "*ignore""*.*rc" "*.map" ".last-compilation-digest-development"
                "*.ico" "Gemfile" "Rakefile" ".rspec" "*integration-testing*" "*node_modules*" "webpack" ".editorconfig" "*.pid"
                "*.workerjs" "*.MIT" "acorn" "AUTHORS" "*.APACHE2" "JSONStream" "babylon" "*.iml" "*.BSD" "*.log"
                "*.ru" "*.cache" "*.ts" "*.json5" "atob" "LICENSE-MIT" "public/assets/*" ".*"
                "*.ls" "loose-envify" "errno" "*.flow" "*.properties" "*.extract-native-dependencies" "*.targets"
                "*.ps1" "*.arcconfig" "Vagrantfile" "*.template" "*.nuspec" "*.emz" "1" "2" "*.svg"
                "*.ttf" ".lein-repl-history" "*.cur" "profile" ".figwheel-compile-stamp" "*.woff" "*.eor"
                "*.xml" "*.coffee" "*.lock" "*.markdown" "*.opts" "module-deps" ".nrepl-port" "repl-port"))))

;; Without this, performance can freeze.
;; `public`: for Rails' `public/assets`
(setq pe/omit-regex (mapconcat 'identity
                               (list "^#" "~$" "^node_modules$" "tmp" ".git$" ".sass-cache"
                                     ".lumo-cache" "target" "auto-save-list" "project-explorer-cache" "public")
                               "\\|"))
