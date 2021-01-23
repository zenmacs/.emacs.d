;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(require 'vemv.buffer-querying)
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
                              ;; disabled for now. Don't know the original intent; now I find it desirable to have a repl in a 3rd party ns.
                              ;; (not (vemv/buffer-of-current-running-project-or-children? (current-buffer)))
                              (and (eq vemv/running-project-type :clj) (vemv/current-buffer-is-cljs)))
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
    (let* ((was (with-selected-window vemv/main_window
                  (vemv/current-buffer-is-cljs)))
           (should (get-buffer vemv/cljs-repl-name)))
      (with-selected-window vemv/repl-window
        (if (and was should)
            (vemv/safe-switch-to-buffer vemv/cljs-repl-name)
          (vemv/safe-switch-to-buffer vemv/clj-repl-name))))))

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

(setq vemv/cljr-building-ast-cache? nil) ;; XXX implement on new clj-refactor.el release

(defun vemv/load-clojure-buffer (&optional callback reload-command)
  (interactive)
  (if (vemv/ciderable-p)
      (if (vemv/current-buffer-is-cljs)
          (vemv/send :cljs nil "(.reload js/location true)")
        (progn
          ;; code has been potentially unloaded, so the underlying `vemv/check-unused-requires' will fail. Prevent that:
          (ignore-errors ;; ignore-errors important, else the whole `vemv/load-clojure-buffer` operation can fail silently
            (vemv/save-all-buffers-for-this-project :skip-check-unused-requires :skip-formatting))
          (vemv/advice-nrepl)
          (vemv/clear-cider-repl-buffer nil
                                        (argless
                                         (replying-yes
                                          (if vemv/using-component-reloaded-workflow
                                              (if vemv/cljr-building-ast-cache?
                                                  (message "Currently building AST cache. Wait a few seconds and try again.")
                                                ;; NOTE: has to be `cider-interactive-eval', so there's visual feedback + error reporting
                                                (cider-interactive-eval (or reload-command
                                                                            vemv/clojure-reload-command
                                                                            "(with-out-str
                                                                               (com.stuartsierra.component.user-helpers/reset))")
                                                                        callback))
                                            (with-current-buffer (window-buffer vemv/main_window)
                                              (cider-load-buffer)
                                              (cider-load-all-project-ns)
                                              (-some-> callback funcall))))))))
    (if (vemv/in-a-lisp-mode?)
        (progn
          (vemv/save)
          (call-interactively 'eval-buffer)
          (-some-> callback funcall)))))

(defun vemv/is-cljs-project? ()
  (and (not (eq vemv/project-type :clj))
       (or (eq vemv/project-type :cljs)
           (vemv/current-buffer-is-cljs)
           (if-let ((p (vemv/project-dot-clj-file)))
               (let* ((was-open (get-file-buffer p))
                      (_ (unless was-open
                           (find-file-noselect p)))
                      (ret (with-current-buffer (get-file-buffer p)
                             (vemv/contains? (buffer-string) "org.clojure/clojurescript"))))
                 (unless was-open
                   (kill-buffer (get-file-buffer p)))
                 ret)))))

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


(defun vemv/lein-deps-command ()
  (concat "source ~/.zshrc; " ;; XXX don't assume zsh
          vemv.project/cd-command
          vemv/project-clojure-dir
          "; export ZENMACS_BRANCH=$(git symbolic-ref refs/remotes/origin/HEAD 2> /dev/null | ruby -e \"puts STDIN.read.split('/').last || 'master'\")"
          "; if git diff $ZENMACS_BRANCH --exit-code -- project.clj > /dev/null; then; else"
          "; lein with-profile "
          (s-join "," vemv.project.default-lein-profiles)
          " do clean, deps; fi"))

(defun vemv/clojure-init-or-send-sexpr ()
  (interactive)
  (when (vemv/in-clojure-mode?)
    (if (and (not cider-launched))
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
                          (unless vemv/cider-port
                            (let* ((s (vemv/lein-deps-command)))
                              (shell-command-to-string s)))
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
      (if (cider-connected-p)
          (if (vemv/current-buffer-is-cljs)
              (vemv/send :cljs)
            (vemv/send :clj))))))

(defun vemv/maybe-classify-token (token)
  "Extracts the class out of TOKEN if possible."
  (let* ((maybe-class (->> token (s-split "/") car)))
    (if (equal maybe-class (s-downcase maybe-class))
        token
      (replace-regexp-in-string "\\.$" "" maybe-class))))

(defun vemv/jump-to-clojure-definition ()
  (interactive)
  (if (not (vemv/in-clojure-mode?))
      (if (eq major-mode 'ruby-mode)
          (call-interactively 'robe-jump)
        (if (eq major-mode 'typescript-mode)
            (tide-jump-to-definition)
          (call-interactively 'xref-find-definitions)))
    (let* ((cider-prompt-for-symbol nil)
           (curr-token (cider-symbol-at-point 'look-back))
           (curr-token-is-qualified-kw (vemv/starts-with curr-token "::")))
      (if curr-token-is-qualified-kw
          (call-interactively 'cider-find-keyword)
        (let* ((original-token curr-token)
               (curr-token (vemv/maybe-classify-token curr-token))
               (command (concat "
(clojure.core/let [x '" curr-token "
                   y (try (clojure.core/-> x clojure.core/pr-str (clojure.string/split #\"/\") clojure.core/first clojure.core/read-string clojure.core/eval) (catch java.lang.Exception _))]
  (clojure.core/if-not (clojure.core/class? y)
     nil
     (clojure.core/-> y clojure.core/pr-str clojure.core/munge (clojure.string/replace \".\" \"/\") (clojure.core/str \".java\") (clojure.java.io/resource) clojure.core/str)))"))
               (maybe-jar-ref (read (vemv.clojure-interaction/sync-eval-to-string
                                     command))))
          (if (s-blank? maybe-jar-ref)
              (cider-find-var)
            (when-let* ((x (cider-find-file maybe-jar-ref)))
              ;; for some reason beginning-of-buffer doesn't appear to work if the buffer was open already.
              ;; maybe some lib it's interfering? (there's one that remembers/restores POINT)
              (kill-buffer x)
              (let* ((x (cider-find-file maybe-jar-ref)))
                (xref-push-marker-stack)
                (with-current-buffer x
                  (end-of-buffer)
                  (beginning-of-defun)
                  (when (s-contains? "/" original-token)
                    (condition-case nil
                        (progn ;; search for an method:
                          (re-search-forward (concat "\s" (->> original-token (s-split "/") last car) "\s*\("))
                          (left-char))
                      (error ;; search for an enum value:
                       (progn
                         (re-search-forward (concat "\s" (->> original-token (s-split "/") last car) "\s*"))
                         (left-char)))))
                  (when (s-ends-with? "." original-token)
                    (re-search-forward (concat "\s" curr-token "\s*\("))
                    (left-char)))
                (switch-to-buffer x)))))))))

(defun vemv/docstring-of-ctor (x)
  (read (vemv.clojure-interaction/sync-eval-to-string
         (concat "(let [c (-> \"" x "\" (clojure.string/replace #\"\\.$\" \"\") read-string eval)
                   cn (.getName c)
                   imports (into {}
                                 (map (fn [[k v]]
                                          [(-> v .getName symbol)
                                           k]))
                                 (ns-imports *ns*))
                   ctors (->> c
                              clojure.reflect/reflect
                              :members
                              (filter (partial instance? clojure.reflect.Constructor))
                              (map :parameter-types)
                              (map (fn [syms]
                                       (->> syms
                                            (mapv (fn [sym]
                                                      (get imports sym sym))))))
                              (sort-by count)
                              (clojure.string/join \\newline))]
             ctors)"))))

(defun vemv/propertize-class (s)
  (propertize s 'face 'vemv-warning-face))

(defun vemv/propertize-interface (s)
  (propertize s 'face 'clojure-type-metadata-face))

(defun vemv/docstring-of-var (var)
  (let ((cider-prompt-for-symbol nil)
        (h (ignore-errors
             (cider-var-info var))))
    (when h
      (let* ((c (nrepl-dict-get h "class"))
             (a (nrepl-dict-get h "arglists-str"))
             (d (-some->> (nrepl-dict-get h "doc")
                  (s-split "\n\n")
                  (mapcar (lambda (x)
                            (->> x
                                 (s-split "\n")
                                 (mapcar 's-trim)
                                 (s-join "\n"))))
                  (s-join "\n\n")))
             (d (if (and d
                         (> (length (s-lines d)) 40))
                    "..."
                  d))
             (name (nrepl-dict-get h "name"))
             (ns (nrepl-dict-get h "ns")))
        (if c
            (if (and (s-ends-with? "." var)
                     (vemv/current-buffer-is-jvm-clj))
                (concat (vemv/propertize-class c)
                        "\n\n"
                        (vemv/docstring-of-ctor var))
              (if (s-contains? "/" var)
                  (concat (vemv/propertize-class var)
                          "\n\n" (nrepl-dict-get h "arglists-str"))
                (let* ((i (nrepl-dict-get h "interfaces"))
                       (i-info (when (first i)
                                 (concat "\n\nimplements " (->> i
                                                                (mapcar 'vemv/propertize-interface)
                                                                (s-join ", ")))))
                       (super (nrepl-dict-get h "super"))
                       (super-info (when (and super
                                              (not (string-equal super "java.lang.Object")))
                                     (concat " extends " (vemv/propertize-interface super)))))
                  "super" "java.lang.Object"
                  (concat (vemv/propertize-class c)
                          super-info
                          i-info))))
          (concat (if (and name ns)
                      (vemv/propertize-class (concat ns "/" name))
                    name)
                  (when a
                    (concat "\n\n" a))
                  (when d
                    (concat "\n\n" d))))))))

(defun vemv/message-clojure-doc ()
  (interactive)
  (if (vemv/ciderable-p)
      (let* ((docstring (vemv/docstring-of-var (cider-symbol-at-point 'look-back))))
        (if docstring
            (vemv/echo docstring)
          (vemv/echo "No docs found.")))
    (vemv/echo "Not connected.")))

(defun vemv.message-clojure-doc/of-current-invocation ()
  "Like `vemv/message-clojure-doc', but instead of looking up thing-at-point,
it looks up the thing currently being invoked, i.e. the first element of the first list that wraps thing-at-point."
  (interactive)
  (save-excursion
    (while (and (not (string-equal (vemv/current-char-at-point) "("))
                (not (eq 1 (point))))
      (paredit-backward-up))
    (when (string-equal (vemv/current-char-at-point) "(")
      (forward-char)
      (paredit-forward)
      (vemv/message-clojure-doc))))

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
    (vemv/show-clj-or-cljs-repl)
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

(defun vemv/is-testing-ns (&optional n inferred)
  (let* ((n (or n (cider-current-ns t))))
    (or (string-equal n (or inferred (funcall cider-test-infer-test-ns n)))
        (vemv/starts-with n "acceptance.")
        (vemv/starts-with n "unit.")
        (vemv/starts-with n "functional.")
        (vemv/starts-with n "integration.")
        (vemv/starts-with n "generative.")

        (vemv/contains? n ".t-")
        (vemv/contains? n ".acceptance.")
        (vemv/contains? n ".unit.")
        (vemv/contains? n ".functional.")
        (vemv/contains? n ".integration.")
        (vemv/contains? n ".generative.")

        (vemv/ends-with n "-spec")
        (vemv/ends-with n "-test"))))

;; We close this buffer because otherwise it gets buried, leaving a useless extra window.
;; Customizing `cider-ancillary-buffers' didn't work.
(defun vemv/close-cider-error ()
  (when-let* ((w (get-buffer-window "*cider-error*")))
    (with-selected-window w
      (vemv/close-this))))

(defmacro vemv/on-nrepl-success (&rest body)
  "Creates a callback apt for async and sync scenarios.
When `vemv/using-component-reloaded-workflow', the callback will be repeatedly invoked, and we regard only the last one.
When not, the callback will be invoked just once, so the code can be inconditionally run."
  `(let* ((__errors nil))
     (lambda (&rest __args)
       (if vemv/using-component-reloaded-workflow
           (when-let* ((dict (-some-> __args car)))
             (let* ((e (nrepl-dict-get dict "err")))
               (when e
                 (push e __errors)
                 (vemv/echo (s-trim-right e))))
             (when (ignore-errors
                     (and (-some-> dict (nrepl-dict-get "status") (car) (string-equal "done"))
                          (not __errors)))
               ,@body))
         ,@body))))

(defun vemv/remove-log-files! ()
  (vemv.clojure-interaction/sync-eval-to-string
   "(->> [\"dev.log\" \"test.log\"]
       (mapv (fn [logfile]
                 (let [f (-> \"user.dir\" System/getProperty (clojure.java.io/file \"log\" logfile))]
                   (when (-> f .exists)
                     [logfile :deleted (-> f .delete)]
                     (-> f .createNewFile))))))"))

(defun vemv/test-this-ns ()
  "Runs the tests for the current namespace, or if not applicable, for the latest applicable ns."
  (interactive)
  (vemv/close-cider-error)
  (when (vemv/in-clojure-mode?)
    (vemv/load-clojure-buffer (vemv/on-nrepl-success (vemv/advice-nrepl
                                                      (argless
                                                       (let* ((cljs (vemv/current-buffer-is-cljs))
                                                              (ns (vemv/current-ns))
                                                              (inferred (funcall cider-test-infer-test-ns ns))
                                                              (chosen (if (vemv/is-testing-ns ns inferred)
                                                                          ns
                                                                        (if cljs
                                                                            vemv/latest-cljs-test-ran
                                                                          vemv/latest-clojure-test-ran))))
                                                         (when (not chosen)
                                                           (vemv/echo "Not detected as a testing ns: " ns))
                                                         (when chosen
                                                           (setq vemv/latest-clojure-test-ran chosen)
                                                           (if cljs
                                                               (vemv/send :cljs
                                                                          nil
                                                                          (concat "(cljs.test/run-tests '"
                                                                                  chosen
                                                                                  ")"))
                                                             (progn
                                                               (vemv/remove-log-files!)
                                                               (cider-test-execute chosen nil nil))))))))
                              vemv/clojure-test-refresh-command)))

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
  (let* ((x (concat "(do (clojure.core/in-ns '" (vemv/current-ns) ") "  s  ")"))
         (dict (cider-nrepl-sync-request:eval x))
         (e (nrepl-dict-get dict "err"))
         (v (nrepl-dict-get dict "value")))
    (if e
        (user-error (pr-str e))
      v)))

(defun vemv/run-this-deftest ()
  "Evaluates and runs the test definition form at point. It can be `deftest',
or something custom that returns a var, which must have :name and :test metadata."
  (when (and (vemv/ciderable-p)
             (or (s-ends-with? ".clj" (buffer-file-name))
                 (s-ends-with? ".cljc" (buffer-file-name))))
    (vemv/close-cider-error)
    (vemv/remove-log-files!)
    (vemv/load-clojure-buffer (vemv/on-nrepl-success (with-selected-window vemv/main_window
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
                                                           (assert (and ns (car sym)))
                                                           (cider-test-update-last-test ns sym)
                                                           (cider-test-execute ns sym)))))
                              vemv/clojure-test-refresh-command)))

(defun vemv/cider-find-keyword-silently (&optional arg)
  "Silent version of cider-find-keyword. Just returns the line/file.
Also removes `noerror' from `search-forward-regexp' for accuracy.

Adds kw-to-find-fallback."
  (interactive "P")
  (cider-ensure-connected)
  (let* ((kw (let ((kw-at-point (cider-symbol-at-point 'look-back)))
               (if (or cider-prompt-for-symbol arg)
                   (read-string
                    (format "Keyword (default %s): " kw-at-point)
                    nil nil kw-at-point)
                 kw-at-point)))
         (ns-qualifier (and
                        (string-match "^:+\\(.+\\)/.+$" kw)
                        (match-string 1 kw)))
         (kw-ns (if ns-qualifier
                    (cider-resolve-alias (cider-current-ns) ns-qualifier)
                  (cider-current-ns)))
         (kw-to-find (concat "::" (replace-regexp-in-string "^:+\\(.+/\\)?" "" kw) "")) ;; " " should be here but it causes occasional false positives. actually with " " one also can get false positives. only fix is to update cider.
         (kw-to-find-fallback (concat "::" (replace-regexp-in-string "^:+\\(.+/\\)?" "" kw) ""))) ;; no " "

    (when (and ns-qualifier (string= kw-ns (cider-current-ns)))
      (error "Could not resolve alias `%s' in `%s'" ns-qualifier (cider-current-ns)))
    (if-let* ((path (cider-sync-request:ns-path kw-ns)))
        (let* ((buffer (cider-find-file path)))
          (with-current-buffer buffer
            (save-excursion
              (beginning-of-buffer)
              (or (ignore-errors
                    (search-forward-regexp kw-to-find nil)
                    t)
                  (progn
                    (beginning-of-buffer)
                    (search-forward-regexp kw-to-find-fallback nil)))
              (list (vemv/current-line-number)
                    (cider-sync-request:ns-path (cider-current-ns))))))
      (user-error "Can't find namespace `%s'" ns))))

(defun vemv/echo-clojure-source ()
  "Shows the Clojure source of the symbol at point."
  (interactive)
  (when (vemv/ciderable-p)
    (let* ((info (cider-var-info (cider-symbol-at-point 'look-back)))
           (line-and-file (if info
                              (list (nrepl-dict-get info "line")
                                    (nrepl-dict-get info "file"))
                            (let* ((curr-token (cider-symbol-at-point 'look-back))
                                   (curr-token-is-qualified-kw (vemv/starts-with curr-token "::"))
                                   (cider-prompt-for-symbol nil))
                              (when curr-token-is-qualified-kw
                                (call-interactively 'vemv/cider-find-keyword-silently))))))
      (if line-and-file
          (let* ((line (first line-and-file))
                 (file (second line-and-file)))
            (if (and line file)
                (let* ((vemv/max-mini-window-height 0.99)
                       (buffer-count (length (vemv/all-buffers)))
                       (buffer (cider-find-file file))
                       (v (if buffer
                              (with-current-buffer buffer
                                (save-excursion
                                  (goto-line line)
                                  (beginning-of-line)
                                  (font-lock-ensure)
                                  (vemv/sexpr-content nil :with-properties)))
                            (user-error "Couldn't open buffer."))))
                  (when (< buffer-count (length (vemv/all-buffers)))
                    (kill-buffer buffer))
                  (vemv/echo v))
              (user-error "Not found.")))
        (user-error "Not found.")))))

(defun vemv/parse-requires (x)
  (->> x
       (-find (lambda (x)
                (and (listp x)
                     (equal :require (car x)))))
       (cdr)
       (mapcar (lambda (x)
                 (-> (if (symbolp x)
                         (vector x)
                       (if (listp x)
                           (vconcat x)
                         x))
                     (aref 0))))))

(defun vemv/fix-defn-oneliners ()
  "Places a newline, if needed, between defn names and their arglists."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-regexp "defn \\(\\_<.*?\\_>\\) \\["
                    "defn \\1\n  [")))

(define-minor-mode vemv.clojure/warn-mode
  "Highlights certain tokens as dangerous."
  :lighter ""
  (font-lock-add-keywords nil `(("\\b\\(when\\|=\\|and\\|or\\|if\\|if-not\\|when-not\\|->>\\|->\\)\\b" 0 'vemv-reverse-warning-face)))
  (vemv/fontify))

(defun cider-stacktrace--should-hide-p (neg-filters pos-filters flags)
  "I customized the logic here, didn't like the original one (`show' means for them: show unconditionally, disregarding negative filters)."
  (let ((neg (seq-intersection neg-filters flags))
        (pos (seq-intersection pos-filters flags)))
    (cond (neg t)
          ((and pos-filters (not pos)) t)
          (t nil))))

(defun zenmacs.clojure-interaction/show-java-decompilation-of-top-level-sexpr ()
  "Jumps to the Java decompilation of the current top-level sexpr.

`clj-java-decompiler.core' must be already `require'd into the running JVM process."
  (interactive)
  (when (and (vemv/ciderable-p)
             (not (vemv/current-buffer-is-cljs)))
    (end-of-defun)
    (beginning-of-defun)
    (let* ((bounds (save-excursion
                     (goto-char (cadr (cider-sexp-at-point 'bounds)))
                     (cider-last-sexp 'bounds)))
           (content (vemv/sexpr-content))
           (s (vemv.clojure-interaction/sync-eval-to-string (concat "(clojure.core/with-out-str (clj-java-decompiler.core/decompile "
                                                                    content
                                                                    "))")))
           ;; eval again for preserving metadata:
           (_ (cider-interactive-eval (concat "(eval '" content ")")
                                      nil
                                      ;; specify line/column info (which also ensures :file correctness):
                                      (list (-> bounds car)
                                            (-> bounds car))))
           (s (->> s
                   (s-chop-prefix "\"")
                   (s-chop-suffix "\"")
                   (vemv/unescape)
                   (s-chop-prefix "\n")))
           (b (get-buffer-create "*zenmacs-java-decompilation*")))
      (with-current-buffer b
        (erase-buffer)
        (insert s)
        (java-mode)
        (font-lock-ensure))
      (xref-push-marker-stack)
      (switch-to-buffer b))))
