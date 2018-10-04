;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(provide 'vemv.clojure-interaction)

(setq vemv/apply-tests-verbosely-counter 0)

(defun vemv/ciderable-p ()
  (and
   (vemv/in-clojure-mode?)
   (cider-connected-p)
   vemv-cider-connected))

(defun vemv/apply-tests-verbosely (f &rest args)
  (let* ((old vemv/verbose-mode)
         (counter vemv/apply-tests-verbosely-counter)
         (setter (lambda (&rest _)
                   (when (eq counter vemv/apply-tests-verbosely-counter)
                     (vemv/set-verbosity-to old)
                     (setq vemv/apply-tests-verbosely-counter (+ 1 vemv/apply-tests-verbosely-counter))))))
    (vemv/set-verbosity-to t)
    (advice-add 'cider-test-echo-summary :after setter)
    (apply f args)))

(defun vemv/current-ns (&optional which-buffer)
  (with-current-buffer (buffer-name which-buffer)
    (cider-current-ns)))

(defun vemv/advice-nrepl* (&optional after)
  (interactive)
  (delay (argless (unless (or (not (buffer-file-name))
                              (not (vemv/buffer-of-current-running-project-or-children? (current-buffer)))
                              (and (eq vemv/running-project-type :clj) (vemv/current-main-buffer-is-cljs)))
                    (when (and (vemv/ciderable-p)
                               (not (string-equal (vemv/current-ns)
                                                  (vemv/current-ns (window-buffer vemv/repl-window)))))
                      (-some-> (vemv/current-ns) cider-repl-set-ns))
                    (-some-> after funcall)))
         1))

(setq vemv/debounced-advice-nrepl (vemv/debounce 'vemv/advice-nrepl* 0.4))

(defun vemv/advice-nrepl (&optional x)
  (funcall vemv/debounced-advice-nrepl x))

(defun vemv/toggle-ns-hiding (&optional after-file-open)
  (interactive)
  (when (not vemv-cleaning-namespaces)
    (let ((curr-buff-name (buffer-name (current-buffer))))
      (setq-local vemv/ns-shown (if after-file-open
                                    (if vemv/ns-shown
                                        vemv/ns-shown
                                      nil)
                                  (if vemv/ns-shown
                                      nil
                                    curr-buff-name)))
      (if vemv/ns-shown
          (hs-show-all)
        (let* ((hs-block-start-regexp "(ns")
               (hs-block-end-regexp ")")
               (hs-hide-comments-when-hiding-all nil)
               (hs-adjust-block-beginning (lambda (initial)
                                            (save-excursion
                                              (point)))))
          (apply #'hs-hide-all ()))))))

(defun vemv/show-clj-or-cljs-repl ()
  (when (vemv/ciderable-p)
    (vemv/safe-select-window vemv/main_window)
    (setq was (vemv/current-main-buffer-is-cljs))
    (vemv/safe-select-window vemv/repl-window)
    (if was
        (switch-to-buffer vemv/cljs-repl-name)
      (switch-to-buffer vemv/clj-repl-name))
    (vemv/safe-select-window vemv/main_window)))

(defun vemv/ensure-repl-visible ()
  (when (and (cider-connected-p)
             (string-equal cider-launched vemv/current-project))
    (vemv/show-clj-or-cljs-repl))
  (when (eq major-mode 'emacs-lisp-mode)
    (with-selected-window vemv/repl-window
      (switch-to-buffer "*ielm*")))
  (when (and (eq vemv/project-type :ruby)
             (get-buffer "*rails*"))
    (with-selected-window vemv/repl-window
      (switch-to-buffer "*rails*"))))

(setq vemv/ns-shown nil)

(defun cljr--maybe-wrap-form ()) ;; void it

;; we can use this in horizon when ns's properly use initialization patterns
(defun vemv/clean-project-namespaces ()
  (if (not vemv-cleaning-namespaces)
      (vemv/echo "vemv-cleaning-namespaces set to false!")
    (let* ((files (filter (lambda (x)
                            (vemv/ends-with x ".cljs"))
                          (directory-files-recursively "/Users/vemv/gpm/src/horizon/src/" ".cljs"))))
      (vemv/safe-select-window vemv/repl-window)
      (switch-to-buffer "*Messages*")
      (vemv/safe-select-window vemv/main_window)
      (vemv/open "/Users/vemv/gpm/src/horizon/project.clj")
      (seq-doseq (filename files)
        (vemv/safe-select-window vemv/main_window)
        (vemv/open filename)
        (setq lexical-binding t)
        (setq whitespace-line-column 240)
        (cljr-clean-ns)
        (beginning-of-buffer)
        (while (re-search-forward "(:require[^\-]" nil t)
          (replace-match "(:require\n"))
        (beginning-of-buffer)
        (while (re-search-forward "(:require\-macros[^\n]" nil t)
          (replace-match "(:require\-macros\n"))
        (beginning-of-buffer)
        (while (re-search-forward "(:import[^\-]" nil t)
          (replace-match "(:import\n"))
        (beginning-of-buffer)
        (while (re-search-forward "(:use\-macros[^\n]" nil t)
          (replace-match "(:use\-macros\n"))
        (vemv/save)
        (vemv/save)
        (vemv/close-this-buffer)))
    (vemv/echo "clean-project-namespaces done!")
    (vemv/echo "Remember: goog* libspec can be spuriously removed.")))

(setq vemv/cljr-building-ast-cache? nil) ;; XXX implement on new clj-refactor.el release

(defun vemv/load-clojure-buffer (&optional callback)
  (interactive)
  (if (vemv/ciderable-p)
      (if (vemv/current-main-buffer-is-cljs)
          (vemv/send :cljs nil "(.reload js/location true)")
        (progn
          ;; code has been potentially unloaded, so the underlying `vemv/check-unused-requires' will fail. Prevent that:
          (vemv/save-all-buffers-for-this-project :skip-check-unused-requires :skip-formatting)
          (vemv/advice-nrepl)
          (vemv/clear-cider-repl-buffer nil
                                        (argless
                                         (replying-yes
                                          (if vemv/using-component-reloaded-workflow
                                              (if vemv/cljr-building-ast-cache?
                                                  (message "Currently building AST cache. Wait a few seconds and try again.")
                                                (cider-interactive-eval (or vemv/clojure-reload-command
                                                                            "(with-out-str
                                                                               (com.stuartsierra.component.user-helpers/reset))")
                                                                        callback))
                                            (cider-load-buffer)
                                            (cider-load-all-project-ns)
                                            (-some-> callback funcall)
                                            ;; format code, which was previously skipped
                                            (vemv/save-all-buffers-for-this-project)))))))
    (if (vemv/in-a-lisp-mode?)
        (progn
          (vemv/save)
          (call-interactively 'eval-buffer)
          (-some-> callback funcall)))))

(defun vemv/is-cljs-project? ()
  (or (eq vemv/project-type :cljs)
      (vemv/current-main-buffer-is-cljs)
      (if-let ((p (vemv/project-dot-clj-file)))
          (let* ((was-open (get-file-buffer p))
                 (_ (unless was-open
                      (find-file-noselect p)))
                 (ret (with-current-buffer (get-file-buffer p)
                        (vemv/contains? (buffer-string) "org.clojure/clojurescript"))))
            (unless was-open
              (kill-buffer (get-file-buffer p)))
            ret))))

(defun cider-repl-set-type (&optional type)
  "Backported from https://github.com/clojure-emacs/cider/issues/1976"
  (interactive)
  (let ((type (or type (completing-read
                        (format "Set REPL type (currently `%s') to: "
                                cider-repl-type)
                        '("clj" "cljs")))))
    (setq cider-repl-type type)))

(defun cider-connect-clojurescript (port)
  "Forked from https://github.com/clojure-emacs/cider/issues/1976"
  (interactive)
  (let ((cider-repl-type "cljs"))
    (when-let* ((conn (cider-connect "127.0.0.1" port vemv/project-root-dir)))
      (let ((b (get-buffer "*cider-repl 127.0.0.1*")))
        (with-current-buffer b
          (setq cider-repl-type "cljs")
          (cider-create-sibling-cljs-repl b))))))

(defun vemv/clojure-init-or-send-sexpr ()
  (interactive)
  (when (vemv/in-clojure-mode?)
    (if (and (not cider-launched) vemv/using-nrepl)
        (progn
          (require 'cider)
          (require 'clj-refactor)
          (electric-indent-mode -1)
          (clj-refactor-mode 1)
          (cljr-add-keybindings-with-prefix "C-0")
          (setq cider-launched vemv/current-project)
          (setq vemv-cider-connecting t)
          (setq vemv/running-project vemv/current-project)
          (setq vemv/running-project-root-dir vemv/project-root-dir)
          (setq vemv/running-project-type vemv/project-type)
          (delay (argless (funcall vemv/project-initializers)
                          (vemv/safe-select-window vemv/main_window)
                          (if (vemv/is-cljs-project?)
                              (progn
                                (-some-> vemv/before-figwheel-fn funcall)
                                (if vemv/cider-port
                                    (cider-connect-clojurescript vemv/cider-port)
                                  (cider-jack-in-clojurescript)))
                            (if vemv/cider-port
                                (cider-connect "127.0.0.1" vemv/cider-port vemv/project-root-dir)
                              (cider-jack-in))))
                 1))
      (if vemv/using-nrepl
          (if (cider-connected-p)
              (if (vemv/current-main-buffer-is-cljs)
                  (vemv/send :cljs)
                (vemv/send :clj)))
        (vemv/send :shell)))))

(defun vemv/jump-to-clojure-definition ()
  (interactive)
  (if (not (vemv/in-clojure-mode?))
      (if (eq major-mode 'ruby-mode)
          (call-interactively 'robe-jump)
        (call-interactively 'xref-find-definitions))
    (let* ((curr-token (cider-symbol-at-point 'look-back))
           (curr-token-is-qualified-kw (vemv/starts-with curr-token "::"))
           (cider-prompt-for-symbol nil))
      (if curr-token-is-qualified-kw
          (call-interactively 'cider-find-keyword)
        (cider-find-var)))))

(defun vemv/docstring-of-var (var)
  (let ((cider-prompt-for-symbol nil)
        (h (ignore-errors
             (cider-var-info var))))
    (when h
      (let* ((a (nrepl-dict-get h "arglists-str"))
             (d (-some->> (nrepl-dict-get h "doc")
                          (s-split "\n\n")
                          (mapcar (lambda (x)
                                    (->> x
                                         (s-split "\n")
                                         (mapcar 's-trim)
                                         (s-join "\n"))))
                          (s-join "\n\n"))))
        (concat a
                (if (and a d)
                    "\n\n")
                d)))))

(defun vemv/message-clojure-doc ()
  (interactive)
  (if (vemv/ciderable-p)
      (let* ((docstring (vemv/docstring-of-var (cider-symbol-at-point 'look-back))))
        (if docstring
            (if (> (length (s-lines docstring)) 40)
                (vemv/echo "Docstring too long, jump to it instead.")
              (vemv/echo docstring))
          (vemv/echo "No docs found.")))
    (vemv/echo "Not connected.")))

(defun cider-company-docsig (thing)
  "Adds the docstring"
  (let* ((eldoc-info (cider-eldoc-info thing))
         (ns (lax-plist-get eldoc-info "ns"))
         (symbol (lax-plist-get eldoc-info "symbol"))
         (arglists (lax-plist-get eldoc-info "arglists"))
         (docstring (lax-plist-get eldoc-info "docstring")))
    (when eldoc-info
      (comm ;; some padding code that can be eventually useful to prevent blinking
       (let* ((v ...)
              (lines (s-lines v))
              (padded (-pad "\n" lines nil (-repeat 10 nil))))
         (s-join "\n" (car padded))))
      (format "%s: %s %s"
              (cider-eldoc-format-thing ns symbol thing
                                        (cider-eldoc-thing-type eldoc-info))
              (cider-eldoc-format-arglist arglists 0)
              (when docstring
                (concat "\n\n" docstring))))))

(defun vemv/clear-cider-repl-buffer (&optional recurring callback)
  (interactive)
  (when (cider-connected-p)
    (vemv/save-window-excursion
     (vemv/safe-select-window vemv/repl-window)
     (end-of-buffer)
     (let* ((should-recur (and (not recurring)
                               (or (> (point-max) 5000) ;; b/c I think the code below is slow, can hang emacs
                                   (vemv/contains? (prin1-to-string (buffer-string))
                                                   "cider-repl-stdout-face")))))
       (when should-recur
         (cider-repl-return) ;; un-hijack the prompt. XXX: only do if there's no pending input
         (cider-repl-clear-buffer)
         (delay (argless (vemv/clear-cider-repl-buffer :recurring callback))
                0.75))
       (cider-repl-clear-buffer)
       (end-of-buffer)
       (when (or recurring (not should-recur))
         (-some-> callback funcall))))))

(setq vemv/latest-clojure-test-ran nil)
(setq vemv/latest-cljs-test-ran nil)

;; XXX better impl: is ns inside :source-paths?
(defun vemv/is-testing-ns (&optional n inferred)
  (let* ((n (or n (cider-current-ns t))))
    (or (string-equal n (or inferred (funcall cider-test-infer-test-ns n)))
        (vemv/starts-with n "unit."))))

;; We close this buffer because otherwise it gets buried, leaving a useless extra window.
;; Customizing `cider-ancillary-buffers' didn't work.
(defun vemv/close-cider-error ()
  (when-let (w (get-buffer-window "*cider-error*"))
    (with-selected-window w
      (vemv/close-this))))

(defmacro vemv/on-nrepl-success (&rest body)
  `(lambda (&rest __args)
     (when (ignore-errors
             (-some-> __args car (nrepl-dict-get "status") (car) (string-equal "done")))
       ,@body)))

(defun vemv/test-this-ns ()
  "Runs the tests for the current namespace, or if not applicable, for the latest applicable ns."
  (interactive)
  (vemv/close-cider-error)
  (when (vemv/in-clojure-mode?)
    (vemv/load-clojure-buffer
     (vemv/on-nrepl-success
      (vemv/advice-nrepl
       (argless
        (let* ((cljs (vemv/current-main-buffer-is-cljs))
               (ns (vemv/current-ns))
               (inferred (funcall cider-test-infer-test-ns ns))
               (chosen (if (vemv/is-testing-ns ns inferred)
                           ns
                         (if cljs
                             vemv/latest-cljs-test-ran
                           vemv/latest-clojure-test-ran))))
          (when chosen
            (setq vemv/latest-clojure-test-ran chosen)
            (if cljs
                (vemv/send :cljs
                           nil
                           (concat "(cljs.test/run-tests '"
                                   chosen
                                   ")"))
              (cider-test-execute chosen nil nil))))))))))

(defun vemv/run-this-deftest-cljs ()
  "Assuming `point` is at a deftest name, it runs it"
  (interactive)
  (vemv/advice-nrepl (argless
                      (let* ((ns (vemv/current-ns))
                             (chosen (cider-symbol-at-point)))
                        (when chosen
                          (vemv/send :cljs
                                     nil
                                     (concat  "(.reload js/location true) "
                                              "(cljs.test/run-block ["
                                              chosen
                                              "])")))))))
(defun vemv/dumb-cljs-test ()
  "Needs M-x cider-load-buffer first"
  (interactive)
  (vemv/send :cljs nil (concat "(cljs.test/run-tests '" (vemv/current-ns) ")")))

(defun vemv.clojure-interaction/sync-eval-to-string (s)
  (-> s cider-nrepl-sync-request:eval (nrepl-dict-get "value")))

(defun vemv/run-this-deftest ()
  "Evaluates and runs the test definition form at point. It can be `deftest',
or something custom that returns a var, which must have :name and :test metadata."
  (when (and (vemv/ciderable-p)
             (s-ends-with? ".clj" (buffer-file-name)))
    (vemv/close-cider-error)
    (vemv/load-clojure-buffer
     (vemv/on-nrepl-success
      (with-selected-window vemv/main_window
        (save-excursion
          (unless (and (zero? (current-column))
                       (looking-at-p "("))
            (end-of-line)
            (beginning-of-defun))
          (let* ((ns (vemv/current-ns))
                 (sym (-> (concat "(-> "
                                  (vemv.clojure-interaction/sync-eval-to-string (vemv/sexpr-content))
                                  " meta :name)")
                          vemv.clojure-interaction/sync-eval-to-string
                          list)))
            (vemv/echo ns sym)
            (cider-test-update-last-test ns sym)
            (cider-test-execute ns sym))))))))

(defun vemv/echo-clojure-source ()
  "Shows the Clojure source of the symbol at point."
  (interactive)
  (when (vemv/ciderable-p)
    (if-let* ((info (cider-var-info (cider-symbol-at-point 'look-back))))
        (let* ((vemv/max-mini-window-height 0.99)
               (buffer-count (length (vemv/all-buffers)))
               (line (nrepl-dict-get info "line"))
               (buffer (cider-find-file (nrepl-dict-get info "file")))
               (v (with-current-buffer buffer
                    (goto-line line)
                    (beginning-of-line)
                    (font-lock-ensure)
                    (vemv/sexpr-content nil :with-properties))))
          (when (< buffer-count (length (vemv/all-buffers)))
            (kill-buffer buffer))
          (vemv/echo v))
      (user-error "Not found."))))

(defun vemv/parse-requires (x)
  (->> x
       (-find (lambda (x)
                (and (listp x)
                     (equal :require (car x)))))
       (cdr)
       (mapcar (lambda (x)
                 (if (symbolp x)
                     (vector x)
                   x)))))

(defun vemv/check-unused-requires ()
  (interactive)
  (when (vemv/ciderable-p)
    (when-let ((clean (cljr--call-middleware-sync
                       (cljr--create-msg "clean-ns"
                                         "path" (cljr--project-relative-path (buffer-file-name))
                                         "libspec-whitelist" cljr-libspec-whitelist
                                         "prune-ns-form" "true")
                       "ns")))
      (let* ((ideal (->> clean read vemv/parse-requires))
             (actual (->> (cider-ns-form) read vemv/parse-requires))
             (diff (-difference actual ideal))
             (message (-some->> diff
                                (mapcar 'pr-str)
                                (mapcar (lambda (x)
                                          (s-replace "\\" "" x)))
                                (s-join "\n")
                                (concat (propertize (vemv/current-ns)
                                                    'face 'vemv-cider-connection-face)
                                        (propertize " - There are unused requires:\n"
                                                    'face 'vemv-warning-face)
                                        "\n"))))
        (when message
          (vemv/echo message)
          (shell-command-to-string (concat "terminal-notifier -message '" message "'")))))))

(defun vemv/fix-defn-oneliners ()
  "Places a newline, if needed, between defn names and their arglists."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-regexp "defn \\(\\_<.*?\\_>\\) \\["
                    "defn \\1\n  [")))

(define-minor-mode docsolver-mode
  "Highlights certain tokens as dangerous."
  :lighter ""
  (font-lock-add-keywords nil `(("\\b\\(when\\|=\\|if\\|if-not\\|when-not\\|->>\\|->\\)\\b" 0 'vemv-reverse-warning-face)))
  (vemv/fontify))

(defun cider-stacktrace--should-hide-p (neg-filters pos-filters flags)
  "I customized the logic here, didn't like the original one (`show' means for them: show unconditionally, disregarding negative filters)."
  (let ((neg (seq-intersection neg-filters flags))
        (pos (seq-intersection pos-filters flags)))
    (cond (neg t)
          ((and pos-filters (not pos)) t)
          (t nil))))
