(require 'warnings)
(require 'vemv-theme)
(provide 'vemv.setqs)

(fset 'yes-or-no-p 'y-or-n-p)
(put-clojure-indent 'with 1)

(setq-default truncate-lines t)
(setq-default save-place t)
(setq-default indent-tabs-mode nil)
(when vemv/terminal-emacs?
  (setq-default revert-without-query t))
(setq-default mode-line-format
              (list "  "
                    '(:eval (when (and (buffer-file-name) (buffer-modified-p))
                              "*"))
                    '(:eval (buffer-name))
                    " "
                    '(:eval (when (buffer-file-name)
                              (propertize "%l:%c" 'face 'font-lock-line-and-column-face)))))

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Prevents annoying popups
(add-to-list 'display-buffer-alist (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))
(add-to-list 'special-display-buffer-names '("*Messages*" vemv/display-completion))
(add-to-list 'special-display-buffer-names '("*xref*" vemv/display-completion))
(add-to-list 'special-display-buffer-names '("*Help*" vemv/display-completion))
(add-to-list 'special-display-buffer-names '("*Ido Completions*" vemv/display-completion))
(add-to-list 'special-display-buffer-names '("*Diff*" vemv/display-completion))

(add-to-list 'special-display-buffer-names '("*rails*" vemv/repl-completion))
(add-to-list 'special-display-buffer-names '("*helm-ag*" vemv/repl-completion))

(add-to-list 'special-display-buffer-names '("*cider-test-report*" vemv.completions/split-window-vertically-small))
(add-to-list 'special-display-buffer-names '("*cider-error*" vemv.completions/split-window-vertically-small))
(add-to-list 'special-display-buffer-names '("*rspec-compilation*" vemv.completions/split-window-vertically-big))

(unless vemv/terminal-emacs?
  (add-to-list 'sp-no-reindent-after-kill-modes 'ruby-mode)
  (add-to-list 'sp-no-reindent-after-kill-modes 'haml-mode))

;; disable annoying popup when large values are printed to the repl
(add-to-list 'warning-suppress-types '(undo discard-info))

(setq pe/project-root-function (lambda (&rest _)
                                 vemv/project-root-dir))

(setq pe/mode-line-format
      `(:eval (vemv/workspace-mode-line-format)))

(setq vemv/pe/mode-line-format
      `(:eval (vemv/pe/mode-line-format*)))

(setq tabbed-line-format
      (list
       '(:eval (let* ((clj-disconnected? (and (not vemv-cider-connecting)
                                              (not vemv-cider-connected)
                                              (vemv/in-a-clojure-mode?)
                                              (vemv/clojure-project?)))
                      (ruby-disconnected? (and (not vemv-robe-connecting)
                                               (not vemv-robe-connected)
                                               (eq :ruby vemv/project-type)))
                      (ruby-connecting? (and vemv-robe-connecting (eq :ruby vemv/project-type)))
                      (clj-connecting? (and (not ruby-connecting?) vemv-cider-connecting (vemv/in-a-clojure-mode?))))
                 (concat (propertize "  %l:%c " 'face 'font-lock-line-and-column-face)
                         (when debug-on-error
                           (propertize "debug-on-error " 'face 'vemv-default-foreground-face-very-slightly-darker))
                         (when clj-disconnected?
                           (propertize "Disconnected " 'face 'font-lock-line-and-column-face))
                         (when ruby-disconnected?
                           (propertize "Disconnected " 'face 'font-lock-line-and-column-face))
                         (when-let* ((relevant? (and (not clj-disconnected?) ;; avoid computations for most buffers
                                                     (not clj-connecting?)
                                                     (not ruby-connecting?)))
                                     (bfn (buffer-file-name))
                                     (clj? (vemv/contains? bfn ".clj"))
                                     (test? (vemv/contains? bfn "test"))
                                     (ff? (bound-and-true-p cider-test-fail-fast)))
                           (propertize "ff " 'face 'font-lock-line-and-column-face))
                         (when vemv/verbose-mode
                           (propertize "Verbose " 'face 'font-lock-line-and-column-face))
                         (when clj-connecting?
                           (propertize "Connecting... " 'face 'vemv-cider-connection-face))
                         (when ruby-connecting?
                           (propertize "Connecting... " 'face 'vemv-cider-connection-face))
                         (when (vemv/good-window-p)
                           (vemv/present-one-tab-per-project-file)))))))

(unless vemv/terminal-emacs?
  ;; https://github.com/company-mode/company-mode/issues/808
  (setq company-backends (-remove (lambda (x)
                                    (and (listp x)
                                         (equal (car x) 'company-dabbrev-code)))
                                  company-backends))
  (push 'company-robe company-backends))

(setq-default tab-width 2)

(setq auto-save-default nil
      auto-save-list-file-prefix nil
      back-to-indentation-state nil
      backup-inhibited t
      cider-auto-jump-to-error t
      cider-repl-display-help-banner' nil
      cider-repl-pop-to-buffer-on-connect nil
      cider-show-error-buffer 'always
      cider-stacktrace-navigate-to-other-window nil
      cider-stacktrace-default-positive-filters '(project)
      cider-stacktrace-default-filters '(tooling dup repl)
      cider-stacktrace-fill-column nil
      cider-font-lock-reader-conditionals nil
      cider-repl-history-file (concat vemv-home "/.cider-repl-history")
      column-number-mode t
      company-dabbrev-char-regexp "\\sw\\|_\\|-\\|!\\|\\?\\|*\\|+"
      company-echo-truncate-lines nil
      company-idle-delay nil ;; no autopopup
      company-tooltip-align-annotations t
      confirm-nonexistent-file-or-buffer nil
      create-lockfiles nil ;; no .#filenames
      css-indent-offset 2
      custom-file "~/.emacs.d/custom.el" ;; not to be used anymore - I want everything to be set in `vemv.setqs` instead. This setq avoids the default, so that we can decide to not `load` the custom file at all.
      delete-by-moving-to-trash nil
      echo-keystrokes 0.02
      flycheck-display-errors-function (lambda (errors)
                                         (vemv/verbosely
                                          (flycheck-display-error-messages errors)))
      global-hl-line-sticky-flag t
      helm-display-header-line nil
      helm-truncate-lines t
      helm-ag-base-command "rg --no-heading"
      helm-ag-success-exit-status '(0 2)
      hi-lock-file-patterns-policy 'never
      highlight-indent-guides-character ?·
      highlight-indent-guides-method 'character
      highlight-indent-guides-responsive 'top
      ielm-header ""
      initial-major-mode 'fundamental-mode
      ido-auto-merge-delay-time 99999 ;; prevents annoying folder switching. might be handy: (setq ido-max-directory-size 100000)
      ido-show-dot-for-dired t
      inhibit-startup-message t
      initial-scratch-message ""
      js-indent-level 2
      exec-path-from-shell-check-startup-files nil
      magit-completing-read-function 'magit-ido-completing-read
      magit-turn-on-auto-revert-mode nil
      make-backup-files nil
      max-mini-window-height 0.25
      mouse-buffer-menu-maxlen 99999
      mouse-buffer-menu-mode-mult 1
      mouse-wheel-follow-mouse 't ;; scroll window under mouse
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-scroll-amount '(4 ((shift) . 4))
      nrepl-hide-special-buffers t
      pdf-outline-enable-imenu nil
      pe/cache-enabled nil
      pe/get-directory-tree-async-delay 0.00000000000000001
      pe/goto-current-file-on-open nil
      pe/width 21
      redisplay-dont-pause t
      require-final-newline t
      robe-completing-read-func 'ido-completing-read
      ruby-insert-encoding-magic-comment nil
      scroll-step 1
      sh-basic-offset 2
      sh-indentation 2
      simpleclip-edit-menu nil
      shift-select-mode nil
      smie-indent-basic 2
      transient-mark-mode t
      truncate-partial-width-windows nil
      typescript-indent-level 2
      vc-follow-symlinks t
      vemv/cljr-ast-load-counter 0
      visible-bell nil ;; disable flickering
      whitespace-style '(face lines-tail)
      x-select-enable-clipboard nil
      yas-key-syntaxes '("w_")
      yas-triggers-in-field t
      yas-use-menu nil
      yas-verbosity 1
      *grizzl-read-max-results* 10)

;; must go separetely
(setq vemv/launched nil)

(setq cljr-magic-require-namespaces
      '(("case"   . "camel-snake-kebab.core")
        ("edn" . "clojure.edn")
        ("io"   . "clojure.java.io")
        ("set"  . "clojure.set")
        ("math.c" . "clojure.math.combinatorics")
        ("combinatorics" . "clojure.math.combinatorics")
        ("pprint"  . "clojure.pprint")
        ("shell" . "clojure.java.shell")
        ("speced"  . "nedap.speced.def")
        ("spec"  . "clojure.spec.alpha")
        ("reducers"  . "clojure.core.reducers")
        ("async"  . "clojure.core.async")
        ("string"  . "clojure.string")
        ("walk" . "clojure.walk")
        ("zip"  . "clojure.zip")))

(custom-set-variables
 '(xref-prompt-for-identifier nil)
 '(cider-connection-message-fn nil)
 '(nrepl-log-messages t)
 '(nrepl-message-buffer-max-size (* 15
                                    ;; the default:
                                    1000000))
 `(cider-use-tooltips ,vemv/use-eldoc-and-tooltips)
 '(cider-enrich-classpath t)
 '(cider-clojure-cli-aliases ":dev:test")
 '(cider-preferred-build-tool "lein")
 ;; '(cider-default-cljs-repl 'figwheel)
 '(cider-repl-auto-detect-type nil) ;; prevents repl buffers from magically changing from cljs type to clj type, which doesn't makessense for shadow-cljs repl buffers (they're always cljs)
 '(cider-repl-display-help-banner nil)
 '(cider-font-lock-dynamically '(macro deprecated))

 ;; this ensures stdout is visible. However printng will be slower.
 ;; it could be selectively set to t for the following cases:
 ;; shadow-cljs repls (they need to communicate things like disconnected status)
 ;; (refresh)
 ;; ctrl-j, cmd-e
 '(cider-repl-display-output-before-window-boundaries t)
 '(cua-remap-control-v nil)
 '(cljr-after-warming-ast-cache-hook (lambda (_)
                                       (setq vemv/cljr-ast-load-counter (inc vemv/cljr-ast-load-counter))))
 '(ielm-prompt "ielm> ")
 '(paren-face-regexp "\\([( ]\\.-\\|[( ]\\.+\\|[][(){}#/]\\)")
 '(mac-mouse-wheel-smooth-scroll nil)
 '(nrepl-popup-stacktraces nil)
 '(pe/inline-folders nil)
 '(tree-widget-image-enable nil)
 '(cljr-auto-sort-ns nil)
 '(cljr-slash-uses-suggest-libspec t)
 '(cljr-suppress-middleware-warnings t)
 '(cljr-assume-language-context "clj")
 '(cljr-project-clean-prompt nil)
 '(cljr-ignore-analyzer-errors t)
 '(cljr-clojure-test-declaration "[clojure.test :refer [are deftest is join-fixtures testing use-fixtures]]")
 '(cljr-cljs-clojure-test-declaration "[cljs.test :refer-macros [deftest testing is are] :refer [use-fixtures]]")
 '(cljr-cljc-clojure-test-declaration "#?(:clj [clojure.test :refer [deftest testing are is use-fixtures]]\n        :cljs [cljs.test :refer-macros [deftest testing is are] :refer [use-fixtures]])")
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
        (when (not formatted)
          (vemv/echo "Buffer has broken syntax, cannot format"))
      (erase-buffer)
      (insert formatted))))

;; set something brief, since we already are applying .gitignore.
;; note that fiplr would break with a nil value.
(setq fiplr-ignored-globs
      ;; `directories` entries must be single-segment, i.e `a/b` doesn't work.
      '((directories (".git" "tmp" ".svn" ".hg" ".bzr" ".lumo-cache" "pom.xml" "semantic"
                      "node_modules" "coverage" "target" ".cljs_rhino_repl"))
        (files (".#*" "*~" "*.DS_Store"))))

;; Without this, performance can freeze (update: not so much given we now correctly use `pe/omit-gitignore')
;; `public`: for Rails' `public/assets`
(setq pe/omit-regex (mapconcat 'identity
                               (list "^#" "~$" "^node_modules$" "^tmp" ".git$" ".sass-cache" "^checkouts" ".elc$" "^backups"
                                     "^integration-testing$"
                                     "^unzipped-jdk-source$"
                                     "pom.xml" "^semantic"
                                     "classes" "^.cpcache"
                                     "crux" ".idea" ".shadow-cljs"
                                     "package-lock.json"
                                     "core.async"
                                     ".rebel_readline_history"
                                     ".lumo-cache" "^target" "auto-save-list" "project-explorer-cache" "^public$" ".nrepl-port"
                                     "^dist" "^generated" ".ok$" ".DS_Store" ".lein-*" ".nrepl-* " ".eastwood" ".cljs_rhino_repl"
                                     "^.clj-kondo" "^coverage" "\.*.log$")
                               "\\|"))

;; Taken from ruby-mode
(setq vemv/ruby-keywords '("alias"
                           "and"
                           "begin"
                           "break"
                           "case"
                           "class"
                           "def"
                           "defined?"
                           "do"
                           "elsif"
                           "else"
                           "fail"
                           "ensure"
                           "for"
                           "end"
                           "if"
                           "in"
                           "module"
                           "next"
                           "not"
                           "or"
                           "redo"
                           "rescue"
                           "retry"
                           "return"
                           "self"
                           "super"
                           "then"
                           "unless"
                           "undef"
                           "until"
                           "when"
                           "while"
                           "yield"))
