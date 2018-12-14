(require 'vemv.theme)
(provide 'vemv.setqs)

(fset 'yes-or-no-p 'y-or-n-p)
(put-clojure-indent 'with 1)

(setq-default truncate-lines t)
(setq-default save-place t)
(setq-default indent-tabs-mode nil)
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

(setq pe/project-root-function (lambda (&rest _)
                                 vemv/project-root-dir))

(setq pe/mode-line-format
      `(:eval (vemv/workspace-mode-line-format)))

(setq vemv/pe/mode-line-format
      `(:eval (vemv/pe/mode-line-format*)))

(setq tabbed-line-format
      (list
       '(:eval (concat (propertize "  %l:%c " 'face 'font-lock-line-and-column-face)
                       (when debug-on-error
                         (propertize "debug-on-error " 'face 'vemv-default-foreground-face-very-slightly-darker))
                       (when (and (not vemv-cider-connecting)
                                  (not vemv-cider-connected)
                                  (vemv/in-a-clojure-mode?)
                                  (vemv/clojure-project?))
                         (propertize "Disconnected " 'face 'font-lock-line-and-column-face))
                       (when (and (not vemv-robe-connecting)
                                  (not vemv-robe-connected)
                                  (eq :ruby vemv/project-type))
                         (propertize "Disconnected " 'face 'font-lock-line-and-column-face))
                       (when vemv/verbose-mode
                         (propertize "Verbose " 'face 'font-lock-line-and-column-face))
                       (when (and vemv-cider-connecting (vemv/in-a-clojure-mode?))
                         (propertize "Connecting... " 'face 'vemv-cider-connection-face))
                       (when (and vemv-robe-connecting (eq :ruby vemv/project-type))
                         (propertize "Connecting... " 'face 'vemv-cider-connection-face))
                       (when (vemv/good-window-p)
                         (vemv/present-one-tab-per-project-file))))))

;; http://www.emacswiki.org/emacs/BackupDirectory
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(unless vemv/terminal-emacs?
  ;; https://github.com/company-mode/company-mode/issues/808
  (setq company-backends (-remove (lambda (x)
                                    (and (listp x)
                                         (equal (car x) 'company-dabbrev-code)))
                                  company-backends))
  (push 'company-robe company-backends))

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      back-to-indentation-state nil
      backup-directory-alist `((".*" . ,temporary-file-directory))
      backup-directory-alist `((".*" . ,temporary-file-directory))
      cider-repl-display-help-banner' nil
      cider-repl-pop-to-buffer-on-connect nil
      cider-show-error-buffer nil
      cider-stacktrace-default-positive-filters '(project)
      cider-stacktrace-default-filters '(tooling dup repl)
      cider-stacktrace-fill-column nil
      column-number-mode t
      company-dabbrev-char-regexp "\\sw\\|_\\|-\\|!\\|\\?\\|*\\|+"
      company-echo-truncate-lines nil
      company-idle-delay nil ;; no autopopup
      company-tooltip-align-annotations t
      confirm-nonexistent-file-or-buffer nil
      create-lockfiles nil ;; no .#filenames
      css-indent-offset 2
      custom-file "~/.emacs.d/custom.el"
      delete-by-moving-to-trash nil
      echo-keystrokes 0.02
      flycheck-display-errors-function (lambda (errors)
                                         (vemv/verbosely
                                          (flycheck-display-error-messages errors)))
      global-hl-line-sticky-flag t
      helm-ag-command-option "--hidden"
      helm-display-header-line nil
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
      vemv/launched nil
      visible-bell nil ;; disable flickering
      whitespace-style '(face lines-tail)
      x-select-enable-clipboard nil
      yas-key-syntaxes '("w_")
      yas-use-menu nil
      yas-verbosity 1)

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
 '(paren-face-regexp "\\([( ]\\.-\\|[( ]\\.+\\|[][(){}#/]\\)")
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
        (when (not formatted)
          (vemv/echo "Buffer has broken syntax, cannot format"))
      (erase-buffer)
      (insert formatted))))

;; set something brief, since we already are applying .gitignore.
;; note that fiplr would break with a nil value.
(setq fiplr-ignored-globs
      ;; `directories` entries must be single-segment, i.e `a/b` doesn't work.
      '((directories (".git" "tmp" ".svn" ".hg" ".bzr" ".lumo-cache"
                      "node_modules" "coverage" "target"))
        (files (".#*" "*~" "*.DS_Store"))))

;; Without this, performance can freeze (update: not so much given we know correctly use `pe/omit-gitignore')
;; `public`: for Rails' `public/assets`
;; `checkouts`: important one, can get huge with my `accessible-jars` plugin
(setq pe/omit-regex (mapconcat 'identity
                               (list "^#" "~$" "^node_modules$" "tmp" ".git$" ".sass-cache" "checkouts" ".elc$"
                                     ".lumo-cache" "target" "auto-save-list" "project-explorer-cache" "public"
                                     "dist" "generated")
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
