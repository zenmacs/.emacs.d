;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(require 'vemv.theme)
(require 'vemv.setqs)
(provide 'vemv.hooks)

(when (and (not vemv-cleaning-namespaces)
           (not vemv/terminal-emacs?))
  (add-hook 'clojure-mode-hook 'hs-minor-mode))

(advice-add 'ruby-mode :before (argless
                                ;; sets vemv/blue-face, clojure-global-constant-face
                                (setq ruby-font-lock-keywords
                                      `( ;; Functions.
                                        ("^\\s *def\\s +\\(?:[^( \t\n.]*\\.\\)?\\([^( \t\n]+\\)"
                                         1 font-lock-function-name-face)
                                        ;; Keywords.
                                        (,(concat
                                           ruby-font-lock-keyword-beg-re
                                           (regexp-opt
                                            vemv/ruby-keywords
                                            'symbols))
                                         (1 font-lock-keyword-face))
                                        ;; Core methods that have required arguments.
                                        (,(concat
                                           ruby-font-lock-keyword-beg-re
                                           (regexp-opt
                                            '( ;; built-in methods on Kernel
                                              "at_exit"
                                              "autoload"
                                              "autoload?"
                                              "callcc"
                                              "catch"
                                              "eval"
                                              "exec"
                                              "format"
                                              "lambda"
                                              "load"
                                              "loop"
                                              "open"
                                              "p"
                                              "print"
                                              "printf"
                                              "proc"
                                              "putc"
                                              "puts"
                                              "require"
                                              "require_relative"
                                              "spawn"
                                              "sprintf"
                                              "syscall"
                                              "system"
                                              "throw"
                                              "trace_var"
                                              "trap"
                                              "untrace_var"
                                              "warn"
                                              ;; keyword-like private methods on Module
                                              "alias_method"
                                              "attr"
                                              "attr_accessor"
                                              "attr_reader"
                                              "attr_writer"
                                              "define_method"
                                              "extend"
                                              "include"
                                              "module_function"
                                              "prepend"
                                              "private_class_method"
                                              "private_constant"
                                              "public_class_method"
                                              "public_constant"
                                              "refine"
                                              "using")
                                            'symbols))
                                         (1 (unless (looking-at " *\\(?:[]|,.)}=]\\|$\\)")
                                              font-lock-builtin-face)))
                                        ;; Kernel methods that have no required arguments.
                                        (,(concat
                                           ruby-font-lock-keyword-beg-re
                                           (regexp-opt
                                            '("__callee__"
                                              "__dir__"
                                              "__method__"
                                              "abort"
                                              "binding"
                                              "block_given?"
                                              "caller"
                                              "exit"
                                              "exit!"
                                              "fail"
                                              "fork"
                                              "global_variables"
                                              "local_variables"
                                              "private"
                                              "protected"
                                              "public"
                                              "raise"
                                              "rand"
                                              "readline"
                                              "readlines"
                                              "sleep"
                                              "srand")
                                            'symbols))
                                         (1 font-lock-builtin-face))
                                        ;; Here-doc beginnings.
                                        (,ruby-here-doc-beg-re
                                         (0 (when (ruby-verify-heredoc (match-beginning 0))
                                              'font-lock-string-face)))
                                        ;; Perl-ish keywords.
                                        "\\_<\\(?:BEGIN\\|END\\)\\_>\\|^__END__$"
                                        ;; Singleton objects.
                                        (,(concat ruby-font-lock-keyword-beg-re
                                                  "\\_<\\(nil\\|true\\|false\\)\\_>")
                                         1 clojure-global-constant-face)
                                        ;; Keywords that evaluate to certain values.
                                        ("\\_<__\\(?:LINE\\|ENCODING\\|FILE\\)__\\_>"
                                         (0 font-lock-builtin-face))
                                        ;; Symbols.
                                        ("\\(^\\|[^:]\\)\\(:@\\{0,2\\}\\(?:\\sw\\|\\s_\\)+\\)"
                                         (2 font-lock-constant-face)
                                         (3 (unless (and (eq (char-before (match-end 3)) ?=)
                                                         (eq (char-after (match-end 3)) ?>))
                                              ;; bug#18644
                                              font-lock-constant-face)
                                            nil t))
                                        ;; Special globals.
                                        (,(concat "\\$\\(?:[:\"!@;,/\\._><\\$?~=*&`'+0-9]\\|-[0adFiIlpvw]\\|"
                                                  (regexp-opt '("LOAD_PATH" "LOADED_FEATURES" "PROGRAM_NAME"
                                                                "ERROR_INFO" "ERROR_POSITION"
                                                                "FS" "FIELD_SEPARATOR"
                                                                "OFS" "OUTPUT_FIELD_SEPARATOR"
                                                                "RS" "INPUT_RECORD_SEPARATOR"
                                                                "ORS" "OUTPUT_RECORD_SEPARATOR"
                                                                "NR" "INPUT_LINE_NUMBER"
                                                                "LAST_READ_LINE" "DEFAULT_OUTPUT" "DEFAULT_INPUT"
                                                                "PID" "PROCESS_ID" "CHILD_STATUS"
                                                                "LAST_MATCH_INFO" "IGNORECASE"
                                                                "ARGV" "MATCH" "PREMATCH" "POSTMATCH"
                                                                "LAST_PAREN_MATCH" "stdin" "stdout" "stderr"
                                                                "DEBUG" "FILENAME" "VERBOSE" "SAFE" "CLASSPATH"
                                                                "JRUBY_VERSION" "JRUBY_REVISION" "ENV_JAVA"))
                                                  "\\_>\\)")
                                         0 font-lock-builtin-face)
                                        ("\\(\\$\\|@\\|@@\\)\\(\\w\\|_\\)+"
                                         0 font-lock-variable-name-face)
                                        ;; Constants.
                                        ("\\_<\\([A-Z]+\\(\\w\\|_\\)*\\)"
                                         1 (unless (eq ?\( (char-after)) font-lock-type-face))
                                        ;; Ruby 1.9-style symbol hash keys.
                                        ("\\(?:^\\s *\\|[[{(,]\\s *\\|\\sw\\s +\\)\\(\\(\\sw\\|_\\)+:\\)[^:]"
                                         (1 (progn (forward-char -1) font-lock-constant-face)))
                                        ;; Conversion methods on Kernel.
                                        (,(concat ruby-font-lock-keyword-beg-re
                                                  (regexp-opt '("Array" "Complex" "Float" "Hash"
                                                                "Integer" "Rational" "String") 'symbols))
                                         (1 font-lock-builtin-face))
                                        ;; Expression expansion.
                                        (ruby-match-expression-expansion
                                         2 'vemv/blue-face t)
                                        ;; Negation char.
                                        ("\\(?:^\\|[^[:alnum:]_]\\)\\(!+\\)[^=~]"
                                         1 font-lock-negation-char-face)
                                        ;; Character literals.
                                        ;; FIXME: Support longer escape sequences.
                                        ("\\?\\\\?\\_<.\\_>" 0 font-lock-string-face)
                                        ;; Regexp options.
                                        ("\\(?:\\s|\\|/\\)\\([imxo]+\\)"
                                         1 (when (save-excursion
                                                   (let ((state (syntax-ppss (match-beginning 0))))
                                                     (and (nth 3 state)
                                                          (or (eq (char-after) ?/)
                                                              (progn
                                                                (goto-char (nth 8 state))
                                                                (looking-at "%r"))))))
                                             font-lock-preprocessor-face))))))

(advice-add 'js-mode :before (argless
                              (setq js--font-lock-keywords-2
                                    (append js--font-lock-keywords-1
                                            (list (list js--keyword-re 1 font-lock-keyword-face)
                                                  (list "\\_<for\\_>"
                                                        "\\s-+\\(each\\)\\_>" nil nil
                                                        (list 1 'font-lock-keyword-face))
                                                  (cons js--basic-type-re font-lock-type-face)
                                                  (cons js--constant-re clojure-global-constant-face))))
                              (setq js--font-lock-keywords-3
                                    `(
                                      ;; This goes before keywords-2 so it gets used preferentially
                                      ;; instead of the keywords in keywords-2. Don't use override
                                      ;; because that will override syntactic fontification too, which
                                      ;; will fontify commented-out directives as if they weren't
                                      ;; commented out.
                                      ,@cpp-font-lock-keywords ; from font-lock.el

                                      ,@js--font-lock-keywords-2

                                      ("\\.\\(prototype\\)\\_>"
                                       (1 font-lock-constant-face))

                                      ;; Highlights class being declared, in parts
                                      (js--class-decl-matcher
                                       ,(concat "\\(" js--name-re "\\)\\(?:\\.\\|.*$\\)")
                                       (goto-char (match-beginning 1))
                                       nil
                                       (1 font-lock-type-face))

                                      ;; Highlights parent class, in parts, if available
                                      (js--class-decl-matcher
                                       ,(concat "\\(" js--name-re "\\)\\(?:\\.\\|.*$\\)")
                                       (if (match-beginning 2)
                                           (progn
                                             (setq js--tmp-location (match-end 2))
                                             (goto-char js--tmp-location)
                                             (insert "=")
                                             (goto-char (match-beginning 2)))
                                         (setq js--tmp-location nil)
                                         (goto-char (point-at-eol)))
                                       (when js--tmp-location
                                         (save-excursion
                                           (goto-char js--tmp-location)
                                           (delete-char 1)))
                                       (1 font-lock-type-face))

                                      ;; Highlights parent class
                                      (js--class-decl-matcher
                                       (2 font-lock-type-face nil t))

                                      ;; Dojo needs its own matcher to override the string highlighting
                                      (,(js--make-framework-matcher
                                         'dojo
                                         "^\\s-*dojo\\.declare\\s-*(\""
                                         "\\(" js--dotted-name-re "\\)"
                                         "\\(?:\"\\s-*,\\s-*\\(" js--dotted-name-re "\\)\\)?")
                                       (1 font-lock-type-face t)
                                       (2 font-lock-type-face nil t))

                                      ;; Match Dojo base classes. Of course Mojo has to be different
                                      ;; from everything else under the sun...
                                      (,(js--make-framework-matcher
                                         'dojo
                                         "^\\s-*dojo\\.declare\\s-*(\""
                                         "\\(" js--dotted-name-re "\\)\"\\s-*,\\s-*\\[")
                                       ,(concat "[[,]\\s-*\\(" js--dotted-name-re "\\)\\s-*"
                                                "\\(?:\\].*$\\)?")
                                       (backward-char)
                                       (end-of-line)
                                       (1 font-lock-type-face))

                                      ;; continued Dojo base-class list
                                      (,(js--make-framework-matcher
                                         'dojo
                                         "^\\s-*" js--dotted-name-re "\\s-*[],]")
                                       ,(concat "\\(" js--dotted-name-re "\\)"
                                                "\\s-*\\(?:\\].*$\\)?")
                                       (if (save-excursion (backward-char)
                                                           (js--inside-dojo-class-list-p))
                                           (forward-symbol -1)
                                         (end-of-line))
                                       (end-of-line)
                                       (1 font-lock-type-face))

                                      ;; variable declarations
                                      ,(list
                                        (concat "\\_<\\(const\\|var\\|let\\)\\_>\\|" js--basic-type-re)
                                        (list #'js--variable-decl-matcher nil nil nil))

                                      ;; class instantiation
                                      ,(list
                                        (concat "\\_<new\\_>\\s-+\\(" js--dotted-name-re "\\)")
                                        (list 1 'font-lock-type-face))

                                      ;; instanceof
                                      ,(list
                                        (concat "\\_<instanceof\\_>\\s-+\\(" js--dotted-name-re "\\)")
                                        (list 1 'font-lock-type-face))

                                      ;; formal parameters
                                      ,(list
                                        (concat
                                         "\\_<function\\_>\\(\\s-+" js--name-re "\\)?\\s-*(\\s-*"
                                         js--name-start-re)
                                        (list (concat "\\(" js--name-re "\\)\\(\\s-*).*\\)?")
                                              '(backward-char)
                                              '(end-of-line)
                                              '(1 font-lock-variable-name-face)))

                                      ;; continued formal parameter list
                                      ,(list
                                        (concat
                                         "^\\s-*" js--name-re "\\s-*[,)]")
                                        (list js--name-re
                                              '(if (save-excursion (backward-char)
                                                                   (js--inside-param-list-p))
                                                   (forward-symbol -1)
                                                 (end-of-line))
                                              '(end-of-line)
                                              '(0 font-lock-variable-name-face)))))
                              (setq js--font-lock-keywords
                                    '(js--font-lock-keywords-3 js--font-lock-keywords-1
                                                               js--font-lock-keywords-2
                                                               js--font-lock-keywords-3))))

(add-hook 'ruby-mode-hook (argless (ruby-end-mode)
                                   (robe-mode)
                                   (setq-local paren-face-regexp (concat "\\("
                                                                         (->> vemv/ruby-keywords
                                                                              (mapcar (lambda (x)
                                                                                        (concat "\\_<" x "\\_>")))
                                                                              (s-join "\\|")
                                                                              (concat "::\\|,\\|=\\|<\\|>\\|[][(){}|@]\\|"))
                                                                         "\\)"))
                                   (paren-face-mode 1)
                                   (vemv/set-keys-for-scope ruby-mode-map vemv/ruby-key-bindings)
                                   (define-key ruby-mode-map [tab] 'vemv/tab)))

(defun vemv/start-robe ()
  (if-let (b (get-buffer "*rails*"))
      (with-current-buffer b
        (progn
          (beginning-of-buffer)
          (if (search-forward vemv/pry-prompt nil t)
              (progn
                (robe-start)
                (setq vemv-robe-connecting nil)
                (setq vemv-robe-connected t)
                (comint-clear-buffer)
                (end-of-buffer))
            (delay 'vemv/start-robe 0.75))))
    (delay 'vemv/start-robe 0.75)))

(add-hook 'inf-ruby-mode-hook 'vemv/start-robe)

(add-hook 'emacs-lisp-mode-hook
          (argless (setq-local mode-line-format tabbed-line-format)))

(add-hook 'cider-repl-mode-hook
          (argless (setq-local mode-line-format vemv/pe/mode-line-format)))

(add-hook 'ielm-mode-hook
          (argless (setq-local mode-line-format vemv/pe/mode-line-format)))

(add-hook 'shell-mode-hook
          (argless (setq-local mode-line-format vemv/pe/mode-line-format)))

(add-hook 'inf-ruby-mode-hook
          (argless (setq-local mode-line-format vemv/pe/mode-line-format)))

(unless vemv/terminal-emacs?
  (dolist (mode (list 'emacs-lisp-mode-hook 'ruby-mode-hook 'clojure-mode-hook
                      'js-mode-hook 'css-mode-hook 'html-mode-hook 'haml-mode-hook))
    (add-hook mode (argless (call-interactively 'text-scale-increase)))))

(advice-add 'pe/show-buffer :after 'vemv/after-file-open)
(advice-add 'vemv/fiplr :after 'vemv/after-file-open)
(advice-add 'vemv/open :after 'vemv/after-file-open)
(advice-add 'vemv/next-file-buffer :after 'vemv/after-file-open)
(advice-add 'vemv/previous-file-buffer :after 'vemv/after-file-open)
(advice-add 'vemv/close-this-buffer :after 'vemv/after-file-open)
(advice-add 'helm-ag--action-find-file :after 'vemv/after-file-open)
(advice-add 'cider-new-error-buffer :after (lambda (&rest _)
                                             (cider-interactive-eval "(try (clojure.core/prn clojure.core/*e)
                                                                              (catch java.lang.Throwable e))")
                                             (delay (argless
                                                     (when (vemv/buffer-of-current-running-project?
                                                            (vemv/save-window-excursion
                                                             (vemv/safe-select-window vemv/main_window)
                                                             (current-buffer)))
                                                       (vemv/save-window-excursion
                                                        (vemv/safe-select-window vemv/repl-window)
                                                        (vemv/switch-to-buffer-in-any-frame vemv/clj-repl-name)
                                                        (end-of-buffer)
                                                        (paredit-backward)
                                                        (paredit-backward))))
                                                    0.7)))

(add-hook 'clojure-mode-hook
          (argless (enable-paredit-mode)
                   (paren-face-mode 1)
                   (global-set-key (kbd "C-r") 'vemv/test-this-ns) ;; must be defined there. TODO: define all clojure bindings here
                   (setq-local mode-line-format tabbed-line-format)))

(add-hook 'emacs-lisp-mode-hook (argless
                                 (enable-paredit-mode)
                                 (setq-local paren-face-regexp "[][()/#'`,]")
                                 (paren-face-mode 1)))

(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'ido-at-point-mode)

(add-hook 'haml-mode-hook (argless
                           (require 'highlight-indent-guides)
                           (setq highlight-indent-guides-auto-enabled nil)
                           (set-face-foreground 'highlight-indent-guides-character-face vemv-default-foreground-color-much-darker)
                           (set-face-background 'highlight-indent-guides-top-character-face vemv-colors/purple)
                           (set-face-foreground 'highlight-indent-guides-top-character-face vemv-colors/purple)
                           (setq global-hl-line-mode nil)
                           (highlight-indent-guides-mode)))

(add-hook 'clojure-mode-hook (argless
                              (unless vemv/terminal-emacs?
                                (vemv/set-keys-for-scope clojure-mode-map vemv/clojure-key-bindings))
                              (define-key clojure-mode-map (kbd ";") 'vemv/semicolon)
                              (define-key clojure-mode-map (kbd "<tab>") 'vemv/tab)
                              ;; XXX backtab not handled by gen.rb
                              (define-key clojure-mode-map (kbd "<backtab>") (argless
                                                                              (let ((max-mini-window-height 0.99))
                                                                                (vemv/message-clojure-doc))))))

(add-hook 'haml-mode-hook (argless
                           (vemv/set-keys-for-scope haml-mode-map vemv/ruby-key-bindings)
                           (define-key haml-mode-map [tab] 'vemv/tab)
                           (yas-activate-extra-mode 'ruby-mode)))

(add-hook 'js-mode-hook (argless
                         (vemv/set-keys-for-scope js-mode-map vemv/ruby-key-bindings)
                         (define-key js-mode-map [tab] 'vemv/tab)))

(add-hook 'sh-mode-hook (argless
                         (vemv/set-keys-for-scope sh-mode-map vemv/ruby-key-bindings)
                         (define-key sh-mode-map [tab] 'vemv/tab)))

(add-hook 'html-mode-hook (argless
                           (define-key html-mode-map [tab] 'vemv/tab)))

(add-hook 'cider-connected-hook
          (argless
           (delay (argless
                   (setq vemv-cider-connecting nil)
                   (setq vemv-cider-connected t)
                   (vemv/show-clj-or-cljs-repl)
                   (when (not vemv-cleaning-namespaces)
                     (vemv/advice-nrepl))
                   (define-key clojure-mode-map (kbd "/") 'cljr-slash))
                  2)))

(add-hook 'cider-repl-mode-hook #'paredit-mode)

(defvar vemv/verbosity-before-company)

(add-hook 'company-completion-started-hook
          (argless
           (setq vemv/verbosity-before-company vemv/verbose-mode)
           (vemv/set-verbosity-to t)))

(add-hook 'company-completion-cancelled-hook
          (argless
           (vemv/set-verbosity-to vemv/verbosity-before-company)))

(add-hook 'company-completion-finished-hook
          (argless
           (vemv/set-verbosity-to vemv/verbosity-before-company)))

;; for when one opens a file via the terminal
;; disabled, seems to mess up tabs
;; (add-hook 'buffer-list-update-hook 'vemv/clean-chosen-file-buffer-order)

;; https://stackoverflow.com/questions/14243583/semantic-movement-across-a-line
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

(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode clojure-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

(advice-add 'vemv/jump-to-clojure-definition :after 'vemv/clean-chosen-file-buffer-order)
(advice-add 'xref-pop-marker-stack :after 'vemv/clean-chosen-file-buffer-order)

(setq vemv/clicking-left-click nil)

(defun vemv/pe/left-click (f &rest args)
  (let ((vemv/clicking-left-click t))
    (apply f args)))

(defun vemv/pe/middle-click (x)
  (when (not vemv/clicking-left-click)
    (kill-buffer (window-buffer vemv/main_window)))
  t)

;; Thanks to these, by clicking middle click one closes the current file before opening the chosen one.
;; On macOS: fn + click.
(comm ;; disabled - stopped working for some reason
 (advice-add 'pe/left-click ':around 'vemv/pe/left-click)
 (advice-add 'pe/middle-click ':before 'vemv/pe/middle-click))

;; https://github.com/DarthFennec/highlight-indent-guides/issues/44#issuecomment-411486188
(defadvice highlight-indent-guides--update-line-cache
    (around my-update-line-cache activate)
  (let ((higp 'highlight-indent-guides-prop) pos indent)
    (save-excursion
      (beginning-of-line)
      (while (and (not (eobp))
                  (or (let ((s (syntax-ppss))) (or (nth 3 s) (nth 4 s)))
                      (looking-at "[[:space:]]*$")))
        (forward-line))
      (setq pos (point) indent (current-indentation))
      (forward-line)
      (while (and (not (eobp))
                  (or (let ((s (syntax-ppss))) (or (nth 3 s) (nth 4 s)))
                      (looking-at "[[:space:]]*$")))
        (forward-line))
      (unless (< indent (current-indentation)) (goto-char pos))
      (back-to-indentation)
      (setq ad-return-value
            (unless (bolp) (nth 5 (get-text-property (1- (point)) higp)))))))

(advice-add 'helm-ag--edit :after 'vemv/ag-replace)

(advice-add 'cider-test-run-test :around 'vemv/apply-tests-verbosely)
(advice-add 'cider-test-run-ns-tests :around 'vemv/apply-tests-verbosely)
(advice-add 'cider-test-run-project-tests :around 'vemv/apply-tests-verbosely)
(advice-add 'cider-test-rerun-failed-tests :around 'vemv/apply-tests-verbosely)
(advice-add 'cider-test-show-report :around 'vemv/apply-tests-verbosely)
(advice-add 'cider-test-execute :around 'vemv/apply-tests-verbosely)

(defun vemv/honoring-newline-at-eof-setting (f &rest args)
  (let (require-final-newline (not vemv/no-newline-at-eof))
    (apply f args)))

(advice-add 'save-buffer :around 'vemv/honoring-newline-at-eof-setting)

(defun hack-local-variables-confirm (f &rest args)
  "Disables annoying dialog 'The local variables list in :x contains values that may not be safe"
  t)

(when pe/cache-enabled
  (add-hook 'kill-emacs-hook 'pe/cache-clear))

(defun grizzl-format-match (match-str selected)
  "Adds git-status awareness"
  (let* ((margin (if selected "> "            "  "))
         (face   (if selected 'grizzl-selection-face 'default))
         (dirty (-find (lambda (x)
                         (vemv/ends-with x match-str))
                       vemv.fiplr.cache/all-git-modified-files)))
    (concat (propertize (format "%s%s" margin match-str) 'face face)
            (when dirty
              (propertize "*" 'face vemv-default-foreground-face-very-slightly-darker)))))

(defun fiplr-list-files-shell-command (type path ignored-globs)
  "Adds ability to honor .gitignore"
  (let* ((type-abbrev
          (lambda (assoc-type)
            (cl-case assoc-type
              ('directories "d")
              ('files "f"))))
         (name-matcher
          (lambda (glob)
            (mapconcat 'identity
                       `("-name" ,(shell-quote-argument glob))
                       " ")))
         (grouped-name-matchers
          (lambda (type)
            (mapconcat 'identity
                       `(,(shell-quote-argument "(")
                         ,(mapconcat (lambda (v) (funcall name-matcher v))
                                     (cadr (assoc type ignored-globs))
                                     " -o ")
                         ,(shell-quote-argument ")"))
                       " ")))
         (matcher
          (lambda (assoc-type)
            (mapconcat 'identity
                       `(,(shell-quote-argument "(")
                         "-type"
                         ,(funcall type-abbrev assoc-type)
                         ,(funcall grouped-name-matchers assoc-type)
                         ,(shell-quote-argument ")"))
                       " "))))
    (let* ((find (mapconcat 'identity
                            `("find"
                              "-L"
                              ,(shell-quote-argument (directory-file-name path))
                              ,(funcall matcher 'directories)
                              "-prune"
                              "-o"
                              "-not"
                              ,(funcall matcher 'files)
                              "-type"
                              ,(funcall type-abbrev type)
                              "-print")
                            " ")))
      (if (vemv/in-a-git-repo? vemv/project-fiplr-dir)
          (concat "cd " vemv/project-fiplr-dir "; comm -23 <(sort <(git ls-files --exclude-standard) <(git status --porcelain | grep -v \"^ D\" | sed s/^...//) | uniq | sort) <(git ls-files --deleted | sort) | while read line; do echo \"$PWD/$line\"; done"
                  "| ruby -e 'puts STDIN.read.split(\"\\n\").sort_by{|line| -(%w("
                  (if (eq vemv/project-type :ruby)
                      "app/controllers spec"
                    (if (eq vemv/project-type :elisp)
                        "vemv. .el"
                      "src test"))
                  ").find_index{|pattern| line.include? pattern } || 9999) }'")
        find))))

(defun vemv/company-calculate-candidates (prefix)
  "https://github.com/company-mode/company-mode/issues/205#issuecomment-317918803"
  (let ((candidates (cdr (assoc prefix company-candidates-cache)))
        (ignore-case (company-call-backend 'ignore-case)))
    (or candidates
        (when company-candidates-cache
          (let ((len (length prefix))
                (completion-ignore-case ignore-case)
                prev)
            (cl-dotimes (i (1+ len))
              (when (setq prev (cdr (assoc (substring prefix 0 (- len i))
                                           company-candidates-cache)))
                (setq candidates (all-completions prefix prev))
                (cl-return t)))))
        (progn
          ;; No cache match, call the backend.
          (setq candidates (company--preprocess-candidates
                            (company--fetch-candidates prefix)))
          ;; Save in cache.
          (push (cons prefix candidates) company-candidates-cache)))
    ;; Only now apply the predicate and transformers.
    (setq candidates (company--postprocess-candidates candidates))
    (when candidates
      (if (or (cdr candidates)
              (get-text-property 0 'yas-template (car candidates))
              (not (eq t (compare-strings (car candidates) nil nil
                                          prefix nil nil ignore-case))))
          candidates
        t))))

(advice-add 'company-calculate-candidates
            :override 'vemv/company-calculate-candidates)

(add-hook 'eval-expression-minibuffer-setup-hook (argless
                                                  (eldoc-mode -1)))
