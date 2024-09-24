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

(defun vemv/active-port (filename)
  (when (and (file-exists-p filename)
             (not (equal ""
                         (shell-command-to-string (concat "lsof -i:"
                                                          (vemv/slurp filename))))))
    (read (vemv/slurp filename))))

(defun vemv/remove-leftmost-directory (path)
  "Remove the leftmost directory from PATH."
  (let ((dirs (split-string path "/" t)))
    (if (< (length dirs) 2)
        path
      (mapconcat 'identity (cdr dirs) "/"))))

(defun vemv/calc-clj-repl-name ()
  (let* ((id (s-replace "/Users/vemv/" "~/" vemv/repl-identifier))
         (id (s-replace-regexp "/$" "" id))
         (id-alt (s-replace "~/" "" id))
         (id-alt-alt (vemv/remove-leftmost-directory id-alt))
         (port? vemv/cider-port)
         (port (or vemv/cider-port
                   (vemv/active-port ".nrepl-port")
                   ;; the jvm repl started by shadow:
                   (vemv/active-port ".shadow-cljs/nrepl.port"))))
    (when (not port?)
      (setq vemv/cider-port port)) ;; cache this computation
    (->> (list (concat "*cider-repl "
                       id
                       ":127.0.0.1:"
                       (when port
                         (prin1-to-string port))
                       "(clj)*")
               (concat "*cider-repl "
                       id
                       ":localhost:"
                       (when port
                         (prin1-to-string port))
                       "(clj)*")
               (concat "*cider-repl "
                       id-alt
                       ":127.0.0.1:"
                       (when port
                         (prin1-to-string port))
                       "(clj)*")
               (concat "*cider-repl "
                       id-alt
                       ":localhost:"
                       (when port
                         (prin1-to-string port))
                       "(clj)*")
               (concat "*cider-repl "
                       id-alt-alt
                       ":127.0.0.1:"
                       (when port
                         (prin1-to-string port))
                       "(clj)*")
               (concat "*cider-repl "
                       id-alt-alt
                       ":localhost:"
                       (when port
                         (prin1-to-string port))
                       "(clj)*"))
         (filter (lambda (s)
                   (get-buffer s)))
         (car))))

(defun vemv/safe-clj-repl-name (destination-buffer)
  (let* ((n (if (boundp 'cider-clojure-cli-command)
                (vemv/calc-clj-repl-name)
              "*cider-repl 127.0.0.1*")))
    (if (and (or (not destination-buffer)
                 (not (get-buffer destination-buffer)))
             n
             (get-buffer n))
        (buffer-name (get-buffer n))
      destination-buffer)))

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
    (cider-current-ns :no-default)))

(defun vemv/advice-nrepl* (&optional after)
  (interactive)
  (delay (argless (unless (or (not (buffer-file-name))
                              ;; disabled for now. Don't know the original intent; now I find it desirable to have a repl in a 3rd party ns.
                              ;; (not (vemv/buffer-of-current-running-project-or-children? (current-buffer)))
                              (and (eq vemv/running-project-type :clj) (vemv/current-buffer-is-cljs)))
                    (let* ((c (vemv/ciderable-p)))
                      (when c
                        (setq xref-backend-functions '(cider--xref-backend)))
                      (when (and c
                                 (not (string-equal (vemv/current-ns)
                                                    (vemv/current-ns (window-buffer vemv/repl-window)))))
                        (when-let* ((n (vemv/current-ns)))
                          (when (seq-some (lambda (session)
                                            (cider--sesman-friendly-session-p session))
                                          (sesman--all-system-sessions 'CIDER))
                            ;; only set the ns if cider--sesman-friendly-session-p.
                            ;; this way, we avoid an internal `in-ns`, which would cause cider--sesman-friendly-session-p
                            ;; to always be t.
                            ;; ...nowadays I'm trying for Clojure buffers to not always be evalable.
                            ;; (at some point I'll want to run multiple projects concurrently)
                            (cider-repl-set-ns n)))))
                    (-some-> after funcall)))
         1))

(setq vemv/debounced-advice-nrepl (vemv/debounce 'vemv/advice-nrepl* 0.4))

(defun vemv/advice-nrepl (&optional x)
  (funcall vemv/debounced-advice-nrepl x))

(defun vemv/do-toggle-ns-hiding ()
  (if vemv/ns-shown
      (hs-show-all)
    (let* ((hs-block-start-regexp "(ns")
           (hs-block-end-regexp ")")
           (hs-hide-comments-when-hiding-all nil)
           (hs-adjust-block-beginning (lambda (initial)
                                        (save-excursion
                                          (point)))))
      (apply #'hs-hide-all ()))))

(defvar vemv/java-toggled nil)

(defun vemv/java-hide-all ()
  "Hides all Java methods and doc comments.
Does not hide the top level itself.

This defun is as copy of `hs-hide-all' except for the ALL-CAPS comments."
  (interactive)
  (hs-life-goes-on
   (save-excursion
     (unless hs-allow-nesting
       (hs-discard-overlays (point-min) (point-max)))
     (end-of-buffer) ;; ADDED
     (beginning-of-defun) ;; ADDED
     (re-search-forward "{") ;; ADDED
     (next-line) ;; ADDED
     ;; (goto-char (point-min)) ;; COMMENTED OUT
     (syntax-propertize (point-max))
     (let ((spew (make-progress-reporter "Hiding all blocks..."
                                         (point-min) (point-max)))
           (re (concat "\\("
                       hs-block-start-regexp
                       "\\)"
                       (if hs-hide-comments-when-hiding-all
                           (concat "\\|\\("
                                   hs-c-start-regexp
                                   "\\)")
                         ""))))
       (while (progn
                (unless hs-hide-comments-when-hiding-all
                  (forward-comment (point-max)))
                (re-search-forward re (point-max) t))
         (if (match-beginning 1)
             ;; We have found a block beginning.
             (progn
               (goto-char (match-beginning 1))
	             (unless (if hs-hide-all-non-comment-function
			                     (funcall hs-hide-all-non-comment-function)
			                   (hs-hide-block-at-point t))
		             ;; Go to end of matched data to prevent from getting stuck
		             ;; with an endless loop.
                 (when (looking-at hs-block-start-regexp)
		               (goto-char (match-end 0)))))
           ;; found a comment, probably
           (let ((c-reg (hs-inside-comment-p)))
             (when (and c-reg (car c-reg))
               (if (> (count-lines (car c-reg) (nth 1 c-reg)) 1)
                   (hs-hide-block-at-point t c-reg)
                 (goto-char (nth 1 c-reg))))))
         (progress-reporter-update spew (point)))
       (progress-reporter-done spew)))
   (beginning-of-line)
   (run-hooks 'hs-hide-hook)))

(defun vemv/toggle-ns-hiding (&optional after-file-open)
  (interactive)
  (if (equal major-mode 'java-mode)
      (progn
        (setq vemv/java-toggled (not vemv/java-toggled))
        (if vemv/java-toggled
            (vemv/java-hide-all)
          (hs-show-all)))
    (when (not vemv-cleaning-namespaces)
      (let ((curr-buff-name (buffer-name (current-buffer))))
        (setq-local vemv/ns-shown (if after-file-open
                                      (if vemv/ns-shown
                                          vemv/ns-shown
                                        nil)
                                    (if vemv/ns-shown
                                        nil
                                      curr-buff-name)))
        (vemv/do-toggle-ns-hiding)))))

(defvar vemv/toggle-all--hidden nil)

(defun vemv/toggle-all ()
  (interactive)
  (if vemv/toggle-all--hidden
      (progn
        (call-interactively 'text-scale-decrease)
        (setq-local line-spacing 1)
        (call-interactively 'hs-show-all)
        (when (vemv/in-a-clojure-mode?)
          (when (not vemv/ns-shown)
            (vemv/do-toggle-ns-hiding)))
        (when (not (eq (selected-window) vemv/main_window))
          (vemv/close-this)))
    (progn
      (call-interactively 'text-scale-increase)
      (setq-local line-spacing 20)
      (call-interactively 'hs-hide-all)
      (vemv/new-frame)))
  (setq-local vemv/toggle-all--hidden (not vemv/toggle-all--hidden)))

(defvar vemv/focus--focused nil)

(defun vemv/focus ()
  (interactive)
  (if vemv/focus--focused
      (progn
        (call-interactively 'text-scale-decrease)
        (when (not (eq (selected-window) vemv/main_window))
          (vemv/close-this)))
    (progn
      (call-interactively 'text-scale-increase)
      (vemv/new-frame)))
  (setq-local vemv/focus--focused (not vemv/focus--focused)))

(defun vemv/show-clj-or-cljs-repl ()
  (let* ((b (cider-current-repl 'any nil)))
    (when (or b (vemv/ciderable-p))
      (let* ((was (with-selected-window vemv/main_window
                    (vemv/current-buffer-is-cljs)))
             (should (when vemv/cljs-repl-name
                       (get-buffer vemv/cljs-repl-name))))
        (with-selected-window vemv/repl-window
          (if (and was should)
              (vemv/safe-switch-to-buffer vemv/cljs-repl-name)
            (vemv/safe-switch-to-buffer (or (vemv/safe-clj-repl-name vemv/clj-repl-name)
                                            (when b
                                              (buffer-name b))))))))))

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

(defun vemv/before-refresh (clear-cider-log?)
  (vemv/set-verbosity-to t) ;; let cider-ns report progress
  (progn
    (ignore-errors ;; ignore-errors important, else the whole `vemv/load-clojure-buffer` operation can fail silently
      (vemv/save-all-buffers-for-this-project :skip-check-unused-requires :skip-formatting))
    (vemv/advice-nrepl)
    (vemv/clear-cider-repl-buffer nil
                                  (argless
                                   (replying-yes
                                    ;; Clear the fringes that are rendered after as successful `(refresh)`.
                                    ;; This way, one can be sure of when a new `(refresh)` has actually completed:
                                    ;; (possibly does not apply anymore after adopting cid-erns)
                                    (with-selected-window vemv/repl-window
                                      (dolist (o (overlays-in (window-start) (window-end)))
                                        (when (overlay-get o 'cider-temporary)
                                          (delete-overlay o))))))
                                  clear-cider-log?)))

(defun vemv/load-clojure-buffer (&optional callback reload-command)
  (interactive)
  (if (vemv/ciderable-p)
      (if (vemv/current-buffer-is-cljs)
          (-some-> callback funcall)
        (progn
          (ignore-errors ;; ignore-errors important, else the whole `vemv/load-clojure-buffer` operation can fail silently
            (vemv/save-all-buffers-for-this-project :skip-check-unused-requires :skip-formatting))
          (vemv/advice-nrepl)
          (vemv/clear-cider-repl-buffer nil
                                        (argless
                                         (replying-yes
                                          (if vemv/using-component-reloaded-workflow
                                              (if vemv/cljr-building-ast-cache?
                                                  (message "Currently building AST cache. Wait a few seconds and try again.")
                                                ;; Clear the fringes that are rendered after as successful `(refresh)`.
                                                ;; This way, one can be sure of when a new `(refresh)` has actually completed:
                                                (with-selected-window vemv/repl-window
                                                  (dolist (o (overlays-in (window-start) (window-end)))
                                                    (when (overlay-get o 'cider-temporary)
                                                      (delete-overlay o))))
                                                ;; NOTE: has to be `cider-interactive-eval', so there's visual feedback + error reporting
                                                (cider-interactive-eval (or reload-command
                                                                            vemv/clojure-reload-command
                                                                            "(with-out-str
                                                                               (com.stuartsierra.component.user-helpers/reset))")
                                                                        callback))
                                            (with-current-buffer (window-buffer vemv/main_window)
                                              (cider-load-buffer)
                                              (cider-load-all-project-ns)
                                              (-some-> callback funcall)))))
                                        (if reload-command
                                            (not (equal reload-command vemv/clojure-lightweight-reload-command))
                                          t))))
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

(defun legacy/cider-connect-clojurescript (port)
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
    (if (not cider-launched)
        (progn
          (require 'cider)
          (require 'cider-find)
          (require 'cider-xref)
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
                          (when (not vemv/cider-port)
                            (setq vemv/cider-port
                                  (or (vemv/active-port ".nrepl-port")
                                      (when (eq vemv/project-type :cljs)
                                        (vemv/active-port ".shadow-cljs/nrepl.port"))
                                      (when (eq vemv/project-type :cljs)
                                        (vemv/active-port "target/repl-port")))))
                          (unless vemv/cider-port
                            (comm
                             (let* ((s (vemv/lein-deps-command)))
                               (shell-command-to-string s))))
                          (vemv/safe-select-window vemv/main_window)
                          (if (vemv/is-cljs-project?)
                              (progn
                                (-some-> vemv/before-figwheel-fn funcall)
                                (if vemv/cider-port
                                    (if (boundp 'cider-clojure-cli-command)
                                        (cider-connect-clj&cljs `(:host "127.0.0.1"
                                                                        :cljs-repl-type shadow
                                                                        :port ,vemv/cider-port
                                                                        :project-dir ,vemv/project-root-dir))
                                      (legacy/cider-connect-clojurescript vemv/cider-port))
                                  (cider-jack-in-cljs ())))
                            (if vemv/cider-port
                                (if (boundp 'cider-clojure-cli-command)
                                    (cider-connect `(:host "127.0.0.1"
                                                           :port ,vemv/cider-port
                                                           :project-dir ,vemv/project-root-dir))
                                  (cider-connect "127.0.0.1" vemv/cider-port vemv/project-root-dir))
                              (cider-jack-in-clj ()))))
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
      (let* ((v (replace-regexp-in-string "\\.$" "" maybe-class)))
        (if (equal major-mode 'java-mode)
            (let* ((pkg-name (->> (s-match-strings-all "package .*" (substring-no-properties (buffer-string)))
                                  first
                                  first
                                  (s-replace "package " "")
                                  (s-replace ";" "")))
                   (candidates (->> token
                                    cljr--call-middleware-to-resolve-missing
                                    (seq-map (lambda (c)
                                               (symbol-name (gethash :name c))))))
                   (only-candidate (if (equal 1 (length candidates))
                                       (car candidates))))
              (or only-candidate
                  (->> candidates
                       (filter (lambda (s)
                                 (s-starts-with? pkg-name s)))
                       car)
                  v))
          v)))))

(defun vemv/aws-source (var)
  "jar + method name"
  (if-let* ((info (cider-var-info var)))
      (let* ((ns (nrepl-dict-get info "ns"))
             (name (nrepl-dict-get info "name")))
        (when (and (not (s-blank? ns))
                   (not (s-blank? name)))
          (let* ((v (read (vemv.clojure-interaction/sync-eval-to-string (concat "(clojure.core/binding [clojure.core/*print-namespace-maps* false]"
                                                                                " (clojure.core/some-> (clojure.core/resolve '"
                                                                                ns
                                                                                "/"
                                                                                name
                                                                                ") "
                                                                                "clojure.core/meta "
                                                                                "((clojure.core/juxt :amazonica/source :amazonica/method-name))"
                                                                                "clojure.core/seq))")))))
            (when (and v
                       (first v)
                       (-> v last first))
              v))))))

(defun vemv/find-protocol-method (x)
  "Finds a protocol method from a bare symbol. Only needed when using frameworks like Trapperkeeper."
  (when (s-contains? "/iroh" default-directory)
    (when-let* ((data (read (vemv.clojure-interaction/sync-eval-to-string (concat "(let [found (->> (user.linters.util/project-namespaces)
                                                                                                    (keep find-ns)
                                                                                                    (map ns-publics)
                                                                                                    (mapcat vals)
                                                                                                    (filter var?)
                                                                                                    (map deref)
                                                                                                    (filter map?)
                                                                                                    (filter :method-builders)
                                                                                                    (map :method-builders)
                                                                                                    (mapcat keys)
                                                                                                    (filter (comp #{'"
                                                                                  x
                                                                                  "                       } :name meta))
                                                                                                    (map meta)
                                                                                                    (group-by :name) ;; this group-by is a bit superfluous - the keys won't be used
                                                                                                    (map (fn [[k v]]
                                                                                                            (let [extract (fn [s]
                                                                                                                            (clojure.string/split s #\"\\.\"))
                                                                                                                  n-e (-> *ns* str extract)
                                                                                                                  ranked (->> v
                                                                                                                              (map (fn [{:keys [protocol] :as thing}]
                                                                                                                                     [thing (->> protocol
                                                                                                                                                 symbol
                                                                                                                                                 str
                                                                                                                                                 extract
                                                                                                                                                 (map = n-e)
                                                                                                                                                 (take-while true?)
                                                                                                                                                 count)])))
                                                                                                                  same? (->> ranked (map second) (apply =))]
                                                                                                              [k (if same?
                                                                                                                   v
                                                                                                                   (->> ranked
                                                                                                                       (sort-by second)
                                                                                                                       reverse
                                                                                                                       ffirst
                                                                                                                       vector))])))
                                                                                                    (into {})
                                                                                                    (vals)
                                                                                                    (apply concat))]
                                                                                     (when (= 1 (count found))
                                                                                       (let [[{:keys [protocol file line column name doc ns arglists]}] found
                                                                                             f (or file
                                                                                                   (-> ns meta :file)
                                                                                                   (-> ns str symbol user.linters.util/ns-sym->filename))]
                                                                                         (list (or (some-> f clojure.java.io/resource str (clojure.string/replace \"file:\" \"\"))
                                                                                                   f)
                                                                                               (or line (-> protocol meta :line))
                                                                                               (or column (-> protocol meta :column))
                                                                                               (str ns)
                                                                                               doc
                                                                                               (cond-> arglists (= (count arglists) 1) first, true pr-str)
                                                                                               (str name)))))")))))
      (let* ((file     (-> data car))
             (line     (-> data rest car))
             (column   (-> data rest rest car))
             (ns       (-> data rest rest rest car))
             (doc      (-> data rest rest rest rest car))
             (arglists (-> data rest rest rest rest rest car))
             (name     (-> data rest rest rest rest rest rest car)))
        (when (-some 'identity data)
          (nrepl-dict "arglists-str" arglists
                      "doc" doc
                      "name" name
                      "ns" ns
                      "line" line
                      "file" file
                      "column" column))))))

(defun vemv/jump-to-clojure-definition ()
  (interactive)
  (if (not (or (vemv/in-clojure-mode?)
               (equal major-mode 'java-mode)))
      (if (eq major-mode 'ruby-mode)
          (call-interactively 'robe-jump)
        (if (eq major-mode 'typescript-mode)
            (tide-jump-to-definition)
          (call-interactively 'xref-find-definitions)))
    (let* ((cider-prompt-for-symbol nil)
           (curr-token (if (and (equal major-mode 'java-mode)
                                (s-match "import" (vemv/current-line-contents))
                                (not (s-match "\*" (vemv/current-line-contents))))
                           (->> (s-match-strings-all "import.*" (vemv/current-line-contents))
                                first
                                first
                                (s-replace "import " "")
                                (s-replace ";" ""))
                         (cider-symbol-at-point 'look-back)))
           (curr-token-is-qualified-kw (vemv/starts-with curr-token ":")))
      (if curr-token-is-qualified-kw
          (call-interactively 'cider-find-keyword)
        (let* ((original-token curr-token)
               (curr-token (vemv/maybe-classify-token curr-token))
               (command (concat "
(clojure.core/binding [clojure.core/*warn-on-reflection* false]
(clojure.core/let [x '" curr-token "
                     y (try (clojure.core/-> x clojure.core/pr-str (clojure.string/split #\"/\") clojure.core/first clojure.core/read-string clojure.core/eval) (catch java.lang.Exception _))
                     module-qualified-resource #(do (clojure.java.io/resource (clojure.core/str %2 \"/\" %1)))]
                  (clojure.core/if-not (clojure.core/class? y)
                                       nil
                                       (clojure.core/or (clojure.core/some-> y clojure.core/pr-str clojure.core/munge (clojure.string/replace \".\" \"/\") (clojure.core/str \".java\") (clojure.java.io/resource) clojure.core/str)
                                                        (clojure.core/when-let [module-name (try (clojure.core/some-> y .getModule .getName) (catch Exception e (.printStackTrace e) nil))]
                                                                               (clojure.core/some-> y clojure.core/pr-str clojure.core/munge (clojure.string/replace \".\" \"/\") (clojure.core/str \".java\") (module-qualified-resource module-name)  clojure.core/str)))
                                       )))"))
               (maybe-jar-ref (read (vemv.clojure-interaction/sync-eval-to-string
                                     command)))
               (found-jar-ref? maybe-jar-ref)
               (aws (when (not found-jar-ref?)
                      (vemv/aws-source curr-token)))
               (maybe-jar-ref (or maybe-jar-ref
                                  (and aws (car aws))))
               (aws-method (and aws (-> aws last car)))
               (protocol-method (when (and (not found-jar-ref?)
                                           (not aws))
                                  (vemv/find-protocol-method original-token))))
          (if (and (s-blank? maybe-jar-ref) (not protocol-method))
              (cider-find-var)
            (if protocol-method
                (when (nrepl-dict-get protocol-method "file")
                  (let* ((file (nrepl-dict-get protocol-method "file"))
                         (line (nrepl-dict-get protocol-method "line"))
                         (name (nrepl-dict-get protocol-method "name"))
                         (buffer (cider-find-file file)))
                    (if buffer
                        (cider-jump-to buffer
                                       (if line
                                           (cons line nil)
                                         name)
                                       nil)
                      (error "Failed when trying to jump to a detected protocol definition."))))
              (when-let* ((x (cider-find-file maybe-jar-ref)))
                ;; for some reason beginning-of-buffer doesn't appear to work if the buffer was open already.
                ;; maybe some lib it's interfering? (there's one that remembers/restores POINT)
                (kill-buffer x)
                (let* ((x (cider-find-file maybe-jar-ref)))
                  (xref-push-marker-stack)
                  (with-current-buffer x
                    (end-of-buffer)
                    (beginning-of-defun)
                    (when (or aws-method (s-contains? "/" original-token))
                      (condition-case nil
                          (progn ;; search for an method:
                            (re-search-forward (concat "\s"
                                                       (or aws-method
                                                           (->> original-token (s-split "/") last car))
                                                       "\s*\("))
                            (left-char))
                        (error ;; search for an enum value:
                         (condition-case nil
                             (progn
                               (re-search-forward (concat "\s" (->> original-token (s-split "/") last car) "\s*"))
                               (left-char))
                           (error nil)))))
                    (when (s-ends-with? "." original-token)
                      (re-search-forward (concat "\s" curr-token "\s*\("))
                      (left-char)))
                  (switch-to-buffer x))))))))))

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

(defun vemv/replace-backtick-words (s)
  "Replace each backtick-delimited word in S with the same word, propertized with font-lock-comment-face."
  (let* ((input-string (propertize s 'face 'font-lock-string-face))
         (start 0))
    ;; `+?` - non-greedy version
    (while (string-match "`\\([^`]+?\\)`" input-string start)
      (let* ((match-string (match-string 1 input-string))
             (propertized-match-string (propertize match-string 'face 'clojure-keyword-face)))
        (setq input-string (replace-match propertized-match-string t t input-string))
        (setq start (- (match-end 0)
                       ;; 2, because we removed two backticks:
                       2))))
    input-string))

(defun vemv/replace-triple-backticks (s)
  (let* ((input-string (propertize s 'face 'font-lock-string-face))
         (start 0))
    (while (string-match "```\\([^`]+?\\)```" input-string start)
      (let* ((match-string (match-string 1 input-string))
             (propertized-match-string (propertize match-string 'face 'clojure-keyword-face)))
        (setq input-string (replace-match propertized-match-string t t input-string))
        (setq start (- (match-end 0)
                       6))))
    input-string))

(comm
 (vemv/replace-triple-backticks "```
a
```"))

(defun vemv/replace-brackets (s)
  (let* ((input-string (propertize s 'face 'font-lock-string-face))
         (start 0))
    (while (string-match "\\[\\[\\([^`]+?\\)\\]\\]" input-string start)
      (let* ((match-string (match-string 1 input-string))
             (propertized-match-string (propertize match-string 'face 'clojure-keyword-face)))
        (setq input-string (replace-match propertized-match-string t t input-string))
        (setq start (- (match-end 0)
                       4))))
    input-string))

(defun vemv/remove-irrelevant-docstring-suffix (s)
  (let* ((vemv/remove-irrelevant-docstring-suffix--found nil))
    (cl-reduce (lambda (new-s crit)
                 (if vemv/remove-irrelevant-docstring-suffix--found
                     vemv/remove-irrelevant-docstring-suffix--found
                   (let* ((split (s-split crit new-s))
                          (head (car split))
                          (garbage (when (> (length split)
                                            1)
                                     (-> split last car))))
                     (when (and garbage
                                (not (equal garbage "")))
                       (setq vemv/remove-irrelevant-docstring-suffix--found head))
                     head)))
               '("Example usage"
                 "Example use"
                 "Examples:"
                 "\\*\\*Example"
                 "#### Example Use:"
                 "Usage:"
                 "To use, "
                 "An example of use"
                 "To obtain the current value from the Signal"
                 "`reg-sub` arguments are:"
                 "You can explore"
                 "Your event handlers will look like this:"
                 "This facility is useful"
                 "If a dispatch looked like this:"
                 "Warning:  calling `clojure.data/diff` on large")
               :initial-value s)))

(defun vemv/docstring-of-var (var)
  (let* ((cider-prompt-for-symbol nil)
         (h (ignore-errors
              (cider-var-info var)))
         (h (or h
                (vemv/find-protocol-method var))))
    (when h
      (let* ((c (nrepl-dict-get h "class"))
             (a (or (nrepl-dict-get h "annotated-arglists")
                    (unless c
                      (nrepl-dict-get h "arglists-str"))))
             (javadoc (when (nrepl-dict-contains h "doc-fragments")
                        (cider--render-docstring (list "doc-fragments" (nrepl-dict-get h "doc-fragments")
                                                       "doc-block-tags-fragments" (nrepl-dict-get h "doc-block-tags-fragments")
                                                       "doc-first-sentence-fragments" (nrepl-dict-get h "doc-first-sentence-fragments")))
                        ;; (vemv/render-docstring var (cdr h))
                        ))
             (d (or javadoc
                    (-some->> (nrepl-dict-get h "doc")
                      (s-split "\n\n")
                      (mapcar (lambda (x)
                                (->> x
                                     (s-split "\n")
                                     (mapcar 's-trim)
                                     (s-join "\n"))))
                      (s-join "\n\n"))
                    (nrepl-dict-get h "doc")))
             (d (or javadoc
                    (when d (s-trim (condition-case nil
                                        (vemv/replace-brackets
                                         (vemv/replace-backtick-words
                                          (vemv/replace-triple-backticks
                                           (vemv/remove-irrelevant-docstring-suffix d))))
                                      (error d))))))
             (d (or javadoc
                    (if (and d
                             (> (length (s-lines d))
                                40))
                        "..."
                      d)))
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
                          "\n\n"
                          (nrepl-dict-get h "arglists-str")
                          (when javadoc
                            "\n\n")
                          javadoc)
                (let* ((i (nrepl-dict-get h "interfaces"))
                       (i-info (when (first i)
                                 (concat "\n\nimplements " (->> i
                                                                (mapcar 'vemv/propertize-interface)
                                                                (s-join ", ")))))
                       (super (nrepl-dict-get h "super"))
                       (super-info (when (and super
                                              (not (string-equal super "java.lang.Object")))
                                     (concat " extends " (vemv/propertize-interface super))))
                       (unprefixed (string-remove-prefix "." var))
                       (the-count (length (concat c "/" unprefixed " ")))
                       (arglists-info (concat (vemv/propertize-class c)
                                              "/"
                                              unprefixed
                                              " "
                                              (when (car a)
                                                (vemv/propertize-interface (car a)))
                                              (when (rest a)
                                                (->> a
                                                     rest
                                                     (mapcar (lambda (s)
                                                               (concat "\n"
                                                                       (make-string the-count ?\s)
                                                                       (vemv/propertize-interface s))))
                                                     (s-join ""))))))
                  (if arglists-info
                      (concat arglists-info
                              (when javadoc
                                "\n\n")
                              javadoc)
                    (concat (vemv/propertize-class c)
                            arglists-info
                            super-info
                            i-info
                            (when javadoc
                              "\n\n")
                            javadoc)))))
          (let* ((v (concat (if (and name ns)
                                (vemv/propertize-class (concat ns "/" name))
                              name)
                            (when a
                              (concat "\n\n" a))
                            (when d
                              (concat "\n\n" d)))))
            v))))))

(defun vemv/message-clojure-doc ()
  (interactive)
  (if (vemv/ciderable-p)
      (let* ((docstring (vemv/docstring-of-var (->> (cider-symbol-at-point 'look-back)
                                                    (string-remove-prefix "#'")
                                                    (string-remove-prefix "'")))))
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

(defun vemv/clear-cider-repl-buffer (&optional recurring callback clear-cider-log?)
  (interactive)
  (when-let* ((b (cider-current-repl 'any nil)))
    (with-current-buffer b
      (vemv/close-cider-error)
      (when-let* ((w (get-buffer-window "*cider-test-report*")))
        (with-selected-window w
          (vemv/close-this)))
      (when clear-cider-log?
        (when-let* ((b (get-buffer "*cider-log*")))
          (cider-log-clear-event-buffer b)))
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
           (delay (argless (vemv/clear-cider-repl-buffer :recurring callback clear-cider-log?))
                  0.75))
         (cider-repl-clear-buffer)
         (end-of-buffer)
         (when (or recurring (not should-recur))
           (-some-> callback funcall)))))))

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

        (vemv/contains? n ".test.")

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
             (when (nrepl-dict-p dict)
               (let* ((e (nrepl-dict-get dict "err")))
                 (when e
                   (push e __errors)
                   (vemv/echo (s-trim-right e))))
               (when (ignore-errors
                       (and (-some-> dict (nrepl-dict-get "status") (car) (string-equal "done"))
                            (not __errors)
                            (not (member "changed-namespaces" (nrepl-dict-keys dict)))))
                 ,@body)))
         ,@body))))

(defun vemv/remove-log-files! ()
  (comm ;; the Tailer I use lately does not support log rotation
   (vemv.clojure-interaction/sync-eval-to-string
    "(clojure.core/->> [\"dev.log\" \"test.log\"]
       (clojure.core/mapv (clojure.core/fn [logfile]
                            (clojure.core/let [f (clojure.core/-> \"user.dir\" System/getProperty (clojure.java.io/file \"log\" logfile))]
                              (clojure.core/when (clojure.core/-> f .exists)
                                [logfile :deleted (clojure.core/-> f .delete)]
                                (clojure.core/-> f .createNewFile))))))")))

(defun vemv/test-vars-for-this-ns (chosen)
  (read
   (vemv.clojure-interaction/sync-eval-to-string
    (concat "(clojure.core/->> \"" chosen "\" clojure.core/symbol clojure.core/find-ns clojure.core/ns-publics clojure.core/vals (clojure.core/filter (clojure.core/comp :test clojure.core/meta)) (clojure.core/remove (clojure.core/comp (clojure.core/some-fn :disabled :sleepy :generative :integration :acceptance :functional :benchmark) clojure.core/meta)) (clojure.core/map (clojure.core/comp clojure.core/str clojure.core/name clojure.core/symbol)))"))))

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
                                                               (cider-test-execute chosen
                                                                                   (when (not cljs)
                                                                                     (vemv/test-vars-for-this-ns chosen))
                                                                                   nil))))))))
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
                                     (concat "(cljs.test/run-block ["
                                             chosen
                                             "])")))))))
(defun vemv/dumb-cljs-test ()
  "Needs M-x cider-load-buffer first"
  (interactive)
  (vemv/send :cljs nil (concat "(cljs.test/run-tests '" (vemv/current-ns) ")")))

(defun vemv.clojure-interaction/sync-eval-to-string (s)
  (let* ((x (concat "(do (clojure.core/in-ns '" (vemv/current-ns) ") "  s  ")"))
         (dict (cider-sync-tooling-eval x)) ;; used to be cider-nrepl-sync-request:eval, but this one is better for cljs (this way, we avoid running jvm code in a cljs repl, which would fail)
         (e (when (nrepl-dict-p dict)
              (nrepl-dict-get dict "err")))
         (v (when (nrepl-dict-p dict)
              (nrepl-dict-get dict "value"))))
    (if e
        (user-error (pr-str e))
      v)))

(defun vemv/echo-clojure-source* (&optional which-var)
  (let* ((var (or which-var (->> (cider-symbol-at-point 'look-back)
                                 (string-remove-prefix "#'")
                                 (string-remove-prefix "'"))))
         (info (cider-var-info var))
         (info (or info
                   (if which-var
                       nil
                     (vemv/find-protocol-method var))))
         (line-and-file (if info
                            (list (nrepl-dict-get info "line")
                                  (nrepl-dict-get info "file"))
                          (when (not which-var)
                            (let* ((curr-token (->> (cider-symbol-at-point 'look-back)
                                                    (string-remove-prefix "#'")
                                                    (string-remove-prefix "'")))
                                   (curr-token-is-qualified-kw (vemv/starts-with curr-token ":"))
                                   (cider-prompt-for-symbol nil))
                              (when curr-token-is-qualified-kw
                                (nrepl-dbind-response (cider--find-keyword-loc (cider-symbol-at-point 'look-back))
                                    (url dest dest-point)
                                  (when url
                                    (list (with-current-buffer dest
                                            (goto-char dest-point)
                                            (vemv/current-line-number))
                                          url)))))))))
    (when line-and-file
      (let* ((line (first line-and-file))
             (file (second line-and-file)))
        (if (and line file)
            (let* ((buffer-count (length (vemv/all-buffers)))
                   (buffer (cider-find-file file))
                   (v (if buffer
                          (with-current-buffer buffer
                            (save-excursion
                              (goto-line line)
                              (beginning-of-line)
                              (font-lock-ensure)
                              (vemv/sexpr-content nil :with-properties)))
                        (if which-var
                            nil
                          (user-error "Couldn't open buffer.")))))
              (when (< buffer-count (length (vemv/all-buffers)))
                (kill-buffer buffer))
              v)

          (if which-var
              nil
            (user-error "Not found.")))))))

(defun vemv/echo-clojure-source (&optional which-var)
  "Shows the Clojure source of the symbol at point."
  (interactive)
  (when (vemv/ciderable-p)
    (let* ((s1 (vemv/echo-clojure-source*))
           (s2 (when s1
                 (let* ((v (vemv/echo-clojure-source* (concat (->> (cider-symbol-at-point 'look-back)
                                                                   (string-remove-prefix "#'")
                                                                   (string-remove-prefix "'"))
                                                              "--fdef-source"))))
                   (when v
                     (concat v "\n\n"))))))
      (if s1
          (let* ((vemv/max-mini-window-height 0.99))
            (vemv/echo (concat s2 s1)))
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

(defun vemv/set-clj-repl-name ()
  (setq vemv/clj-repl-name (vemv/calc-clj-repl-name)))

(defun vemv/set-cljs-repl-name ()
  (let* ((id (s-replace "/Users/vemv/" "~/" vemv/repl-identifier))
         (id (s-replace-regexp "/$" "" id))
         (port (or (condition-case nil
                       (nrepl--port-from-file (expand-file-name ".shadow-cljs/nrepl.port"
                                                                vemv/project-root-dir))
                     (error nil))
                   (condition-case nil
                       (nrepl--port-from-file (expand-file-name "target/repl-port" ;; figwheel
                                                                vemv/project-root-dir))
                     (error nil)))))
    (setq vemv/cljs-repl-name (->> (list (concat "*cider-repl "
                                                 id
                                                 ":localhost:"
                                                 port
                                                 "(cljs:shadow)*")
                                         (concat "*cider-repl "
                                                 id
                                                 ":localhost:"
                                                 port
                                                 "(cljs:figwheel-main)*")
                                         (concat "*cider-repl "
                                                 id
                                                 ":localhost:"
                                                 port
                                                 "(cljs)*")
                                         (concat "*cider-repl "
                                                 id
                                                 ":localhost:"
                                                 port
                                                 "(clj)*")
                                         (concat "*cider-repl "
                                                 id
                                                 ":127.0.0.1:"
                                                 port
                                                 "(cljs:shadow)*")
                                         (concat "*cider-repl "
                                                 id
                                                 ":127.0.0.1:"
                                                 port
                                                 "(cljs:figwheel-main)*")
                                         (concat "*cider-repl "
                                                 id
                                                 ":127.0.0.1:"
                                                 port
                                                 "(cljs)*")
                                         (concat "*cider-repl "
                                                 id
                                                 ":127.0.0.1:"
                                                 port
                                                 "(clj)*"))
                                   (filter (lambda (s)
                                             (get-buffer s)))
                                   (car)))))

(defun vemv/inspect-tap ()
  (interactive)
  (select-frame-set-input-focus vemv/inspector_frame)
  (cider-inspect-expr "(vemv.tap/view!)" (cider-current-ns)))

(defun vemv/inspect-region ()
  (interactive)
  (cider-inspect-expr (vemv/selected-region) (cider-current-ns)))

(defun vemv/clear-tap ()
  (interactive)
  (cider-sync-tooling-eval "(vemv.tap/reset-queue!)"))

(defun vemv/read-lines (prompt initial some-keyseq)
  (let ((keymap (copy-keymap minibuffer-local-map)))
    (define-key keymap (kbd "RET") 'newline)
    (define-key keymap some-keyseq 'exit-minibuffer)
    (read-from-minibuffer prompt initial keymap)))

(defun vemv/maybe-close-magit-after-commit ()
  (let ((default-directory vemv/project-root-dir))
    (when (equal ""
                 (shell-command-to-string "git status --porcelain"))
      (when (frame-live-p vemv/magit_frame)
        (with-selected-frame vemv/magit_frame
          (vemv/close-this-frame)
          t)))))

(defun vemv/git-commit ()
  (interactive)
  (let* ((message (vemv/read-lines "Commit (C-s to confirm): " nil (kbd "C-s"))))
    (when (= 0 (magit-call-git "commit" "-m" message))
      (or (vemv/maybe-close-magit-after-commit)
          (magit-refresh)))))

(defun vemv/git-commit-amend ()
  (interactive)
  (let* ((message (vemv/read-lines "Amend (C-s to confirm): "
                                   (replace-regexp-in-string "\r" "" (s-trim (shell-command-to-string "git log -1 --pretty=%B")))
                                   (kbd "C-s"))))
    (when (= 0 (magit-call-git "commit" "--amend" "-m" message))
      (or (vemv/maybe-close-magit-after-commit)
          (magit-refresh)))))

(defun vemv/rama-edit-macro ()
  (interactive)
  (if (use-region-p)
      (let ((region-start (region-beginning))
            (region-end (region-end)))
        (save-excursion
          (goto-char region-start)
          (while (re-search-forward "\\(\\*\\w+\\(?:-\\w+\\)*\\)" region-end t)
            (replace-match (concat "'" (match-string 0) "#") nil nil))))
    (message "No region selected")))
