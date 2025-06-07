(require 'vemv.lang)
(require 'vemv.clojure-interaction)
(require 'vemv.project-interaction)
(require 'vemv.open)
(unless vemv/terminal-emacs?
  (require 'cider)) ;; Ideally would not be there as it slightly slows down things.
(provide 'vemv.project)

;; on a terminal, or on cider latest
(when (not (fboundp 'cider-project-name))
  (defun cider-project-name (x)
    x))

(when vemv/terminal-emacs?
  (defun put-clojure-indent (&rest _)))

(defvar vemv/clj-repl-name nil)

(setq vemv/default-cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)

          (try
           (require 'figwheel-sidecar.system)
           (alter-var-root #'figwheel-sidecar.system/repl-function-docs
                           (constantly \"Results: Stored in vars *1, *2, *3, *e holds last exception object\"))
           (catch java.lang.Throwable e))
        (figwheel-sidecar.repl-api/start-figwheel!)
        (figwheel-sidecar.repl-api/cljs-repl))")

(setq vemv/initial-cider-lein-parameters (unless vemv/terminal-emacs?
                                           cider-lein-parameters))

;; - make `setq`s defuns
;; - infer project from currently open file
;; - use inferred value as implicit argument to these defuns

(defun vemv/root-marker ()
  "A string that proves that a project is a full directory, rather than a project id (name - like \"myproject\")"
  "/Users")

(condition-case nil
    (load (concat "vemv.project." vemv/current-project))
  (error nil))

;; intentionally outside vemv.project/reset* so it can be refreshed in a single file
(defvar vemv.project.default-lein-profiles nil)

(defmacro vemv.project/reset* ()
  `(progn
     (set-variable 'cider-lein-parameters vemv/initial-cider-lein-parameters)
     (setq vemv/project-type nil) ;; :clj, :cljs (these two have an effect), or any other keyword (with the only purpose of preventing :clj/:cljs effects)
     (setq vemv/using-component-reloaded-workflow nil) ;; whether the project uses com.stuartsierra.component.user-helpers
     (setq vemv/clojure-reload-command nil) ;; The command, as a string, to reload code if `vemv/using-component-reloaded-workflow`.
     (setq vemv/clojure-lightweight-reload-command nil) ;; like `vemv/clojure-reload-command', but with whatever 'lightweight' semantics you wish (e.g. reload the code but don't reset any components)
     (setq vemv/clojure-test-refresh-command nil)
     (setq vemv/cider-port nil)
     (setq vemv/project-initializers nil) ;; an argless lambda which can run arbitrary code before connecting to CIDER.
     (setq vemv/project-root-dir nil) ;; Where a project is located
     (setq vemv/project-clojure-dir nil) ;; Within a project, a directory that contains the Leiningen project. Leave unset if it equals `vemv/project-root-dir`
     (setq vemv/project-fiplr-dir nil) ;; The dir within which fiplr completions will be performed. Normally unnecessary.
     (setq vemv/project-ns-prefix nil) ;; The prefix that every namespace in this project shares. Will be omitted from each tab representing a file/ns.
     (setq vemv/repl-identifier nil) ;; The name that CIDER assigns to the project in its repls. Normally well-inferred; use this setting as a workaround.
     (setq vemv/default-clojure-file nil) ;; The file that will be open with the project, if no files were open for this project last time you quit Emacs.
     (setq vemv/parent-project-root-dirs nil) ;; The root dirs (as in `vemv/project-root-dir`) of parent projects of this project. Set vemv.project/chilren-root-dirs too if setting this.
     (setq vemv.project/chilren-root-dirs nil) ;; The root dirs (as in `vemv/project-root-dir`) of children projects of this project. Set vemv/parent-project-root-dirs too if setting this.
     (setq vemv-cleaning-namespaces nil)
     (setq vemv/modifiers/primary nil)
     (setq vemv/modifiers/secondary nil)
     (setq vemv/modifiers/tertiary nil)
     (setq cljr-warn-on-eval nil) ;; https://github.com/clojure-emacs/cider/issues/2327
     (setq cljr-eagerly-build-asts-on-startup nil)

     ;; Reason for commenting out:
     ;; let the first connection be sticky across projects. handy for monorepos
     ;;   (update some months later: maybe not needed now given the new `vemv/safe-clj-repl-name` fn)
     ;; (setq vemv/clj-repl-name nil)
     (setq vemv/cljs-repl-name nil)

     (setq vemv/no-newline-at-eof nil)
     (setq vemv/comment-indent-function 'comment-indent-default)
     (setq comment-indent-function vemv/comment-indent-function)
     (setq-default comment-indent-function vemv/comment-indent-function)
     (setq clojure-indent-style :always-align)
     (put-clojure-indent 'as-> 2)
     (setq clojure-align-forms-automatically nil)
     (setq whitespace-line-column nil)
     (setq vemv/before-figwheel-fn nil)
     (setq vemv.project/default-git-branch "master")
     (setq vemv.project/skip-formatting nil)
     (setq vemv.project/cd-command "cd ") ;; the command that will be used in *shell-1* to change directories as you change projects.
     (setq vemv/pry-prompt "pry") ;; The pry prompt as per your `~/.pryrc' or such. Will affect robe-mode initialization process, so make sure to reflect any customized value here.
     (setq vemv.project.reasonable-file-count?/threshold 3000)
     (when (not vemv-cleaning-namespaces)
       (comm (setq cider-cljs-lein-repl vemv/default-cider-cljs-lein-repl)))))

(defun vemv.project/reset ()
  (vemv.project/reset*))

(vemv.project/reset)

(defun vemv/project-dot-clj-file ()
  (or (let ((f (concat vemv/project-clojure-dir "project.clj")))
        (when (file-exists-p f)
          f))
      (let ((f (concat vemv/project-clojure-dir "deps.edn")))
        (when (file-exists-p f)
          f))
      (let ((f (concat vemv/project-clojure-dir "profiles.clj")))
        (when (file-exists-p f)
          f))))

(defun vemv/on-the-fly-project? (which)
  "An on-the-fly project is one that was opened via a command.
At opening time, it was ensured that that project didn't belong to vemv/available-projects."
  (member which vemv/on-the-fly-projects))

(defun vemv/all-project-names (&optional no-prettify)
  (mapcar (lambda (x)
            (if (and (not no-prettify) (vemv/on-the-fly-project? x))
                (cider-project-name x)
              x))
          (vemv/projects-for-workspace)))

(defun vemv/refresh-current-project (which &optional switch-p)
  (let ((old-project-type vemv/project-type)
        (on-the-fly-project (vemv/on-the-fly-project? which)))
    (vemv.project/reset)

    (when which
      (setq vemv/current-project (if on-the-fly-project
                                     (cider-project-name which)
                                   which)))

    ;; XXX the case statements are hardcoded. parameterize
    (setq vemv/project-root-dir
          (or vemv/project-root-dir (if on-the-fly-project
                                        which
                                      (vemv/dir-for-project vemv/current-project))))

    (when (and which (not on-the-fly-project))
      (condition-case nil
          (load (concat "vemv.project." which))
        (error nil)))

    (setq comment-indent-function vemv/comment-indent-function)
    (setq-default comment-indent-function vemv/comment-indent-function)

    ;; Ensures correct `vemv/safe-show-current-file-in-project-explorer` functioning
    (setq vemv/project-root-dir (concat vemv/project-root-dir (if (s-ends-with? "/" vemv/project-root-dir)
                                                                  ""
                                                                "/")))

    (setq default-directory vemv/project-root-dir)
    (setq-default default-directory vemv/project-root-dir)
    (setq-local default-directory vemv/project-root-dir)

    (unless (file-exists-p vemv/project-root-dir)
      (vemv/echo (concat "vemv/project-root-dir doesn't exist: " vemv/project-root-dir)))

    ;; note: `pe/omit-gitignore' is poorly named, should be called `pe/honor-gitignore'
    ;; disabled due to https://github.com/vemv/.emacs.d/issues/206
    ;; (setq pe/omit-gitignore (file-exists-p (concat vemv/project-root-dir ".gitignore")))

    (setq vemv/project-clojure-dir (or vemv/project-clojure-dir vemv/project-root-dir))

    (setq vemv/project-clojure-dir (concat vemv/project-clojure-dir (if (s-ends-with? "/" vemv/project-clojure-dir)
                                                                        ""
                                                                      "/")))

    (let* ((chosen-project-type vemv/project-type))
      (setq vemv/project-type (or vemv/project-type
                                  (if (file-exists-p (concat vemv/project-root-dir "Gemfile"))
                                      :ruby
                                    ;; (`:cljs` project detection disabled for now / not particularly reliable or needed)
                                    :clj)))


      (when (and (not chosen-project-type)
                 (eq vemv/project-type :clj)
                 (fboundp 'vemv/default-clj-setup))
        ;; `vemv/default-clj-setup` is a personal function - cannot be hardcoded
        (vemv/default-clj-setup)))

    (setq vemv/project-initializers (or vemv/project-initializers (lambda ())))

    (setq vemv/project-fiplr-dir
          (or vemv/project-fiplr-dir
              (if on-the-fly-project
                  which
                (concat vemv/project-clojure-dir))))

    (setq vemv/project-fiplr-dir
          (if (file-exists-p vemv/project-fiplr-dir)
              vemv/project-fiplr-dir
            vemv/project-root-dir))

    ;; the bit to remove in tabs (mode-line). also identifies repls (important)
    (setq vemv/project-ns-prefix (or vemv/project-ns-prefix vemv/current-project))

    (setq vemv/repl-identifier (or vemv/repl-identifier (cider-project-name vemv/project-root-dir)))

    ;; let the first connection be sticky across projects. handy for monorepos
    (when (not (and vemv/clj-repl-name
                    (cider-connected-p)))

      (if vemv/cider-port
          (if (boundp 'cider-clojure-cli-command)
              (vemv/set-clj-repl-name)
            (setq vemv/clj-repl-name (concat "*cider-repl 127.0.0.1*")))
        (if (boundp 'cider-clojure-cli-command)
            (vemv/set-clj-repl-name)
          (setq vemv/clj-repl-name (concat "*cider-repl " vemv/repl-identifier "*")))))

    (if (boundp 'cider-clojure-cli-command)
        ;; recent:
        (vemv/set-cljs-repl-name)

      ;; forked:
      (setq vemv/cljs-repl-name (concat "*cider-repl CLJS " vemv/repl-identifier "*")))

    (mapcar (lambda (ext)
              (let* ((f (concat vemv/project-clojure-dir
                                "src/"
                                (->> vemv/project-ns-prefix
                                     (s-replace "." "/")
                                     (s-replace "-" "_"))
                                "/core.clj"
                                (if (eq vemv/project-type :cljs)
                                    "s"
                                  ""))))
                f))
            '(".clj" ".cljs" ".cljc"))

    (setq vemv/default-clojure-file
          (or
           vemv/default-clojure-file
           (->> '(".clj" ".cljs" ".cljc")
                (mapcar (lambda (ext)
                          (let* ((f (concat vemv/project-clojure-dir
                                            "src/"
                                            (->> vemv/project-ns-prefix
                                                 (s-replace "." "/")
                                                 (s-replace "-" "_"))
                                            "/core.clj"
                                            (if (eq vemv/project-type :cljs)
                                                "s"
                                              ""))))
                            (when (file-exists-p f)
                              f))))
                (filter 'identity)
                car)
           (vemv/project-dot-clj-file)))

    (setq vemv-cleaning-namespaces nil)
    (setq vemv/modifiers/primary "C")
    (setq vemv/modifiers/secondary "M")
    (setq vemv/modifiers/tertiary "s")

    (when whitespace-line-column
      (->> vemv/chosen-file-buffer-order
           (gethash vemv/current-project)
           (mapcar (lambda (_b)
                     (when-let* ((b (get-file-buffer _b)))
                       (with-current-buffer b
                         (dotimes (_ 2)
                           (call-interactively 'whitespace-mode))))))))

    (when (not (equal vemv/project-type old-project-type))
      (load "vemv-theme"))

    (when switch-p
      (vemv/safe-select-window vemv/repl-window) ;; ensures the currently-selected project is visible
      (funcall vemv/maybe-change-project-graphically))))

(unless vemv/terminal-emacs?
  (vemv/refresh-current-project vemv/current-project))

(defun vemv/copy-relative-path-impl ()
  (file-relative-name (buffer-file-name) (locate-dominating-file default-directory ".git")))

(defun vemv/copy-relative-path ()
  (interactive)
  (let* ((s (vemv/copy-relative-path-impl)))
    (simpleclip-set-contents s)
    (vemv/echo (concat "Copied: " s))))

(defun vemv/copy-relative-path-with-at-prefix ()
  (interactive)
  (let* ((s (concat "@" (vemv/copy-relative-path-impl) " ")))
    (simpleclip-set-contents s)
    (vemv/echo (concat "Copied: " s))))

;; inspired by cljr--find-source-ns-of-test-ns
(defun vemv/find-test-namespaces-of-source-ns (ns file)
  (let* ((ns-chunks (split-string ns "[.]" t))
         (name (car (last ns-chunks)))
         (other-files (mapcar (lambda (prefix)
                                (concat vemv/project-root-dir
                                        "test/"
                                        prefix
                                        "/"
                                        (s-replace vemv/project-root-dir
                                                   ""
                                                   (s-replace "src/"
                                                              ""
                                                              (buffer-file-name)))))
                              '("unit"
                                "integration"
                                "functional"
                                "acceptance"
                                "generative")))
         (oother-files (mapcar (lambda (pair)
                                 (let* ((from (car pair))
                                        (to (car (last pair)))
                                        (x
                                         (concat vemv/project-root-dir
                                                 "test/"
                                                 (s-replace vemv/project-root-dir
                                                            ""
                                                            (s-replace "src/"
                                                                       ""
                                                                       (replace-regexp-in-string from
                                                                                                 to
                                                                                                 (buffer-file-name)))))))
                                   x))
                               '(("web_service/routes\\.clj" "web_service_test.clj"))))
         (other-files (filter 'file-exists-p (nconc other-files oother-files)))
         (candidates (seq-map (lambda (file-name)
                                (replace-regexp-in-string "^test/"
                                                          ""
                                                          (s-replace vemv/project-root-dir
                                                                     ""
                                                                     (replace-regexp-in-string "_"
                                                                                               "-"
                                                                                               (file-name-sans-extension file-name)))))

                              (seq-remove (lambda (x)
                                            (or (not x)
                                                (equal x ".")
                                                (not (file-exists-p x))
                                                (file-directory-p x)))
                                          (nconc (ignore-errors
                                                   (directory-files (replace-regexp-in-string "src/" "test/" (file-name-directory file) t t)
                                                                    t))
                                                 other-files))))

         (test-nss (seq-filter (lambda (it)
                                 (let* ((clean (replace-regexp-in-string "-test$" "" it))
                                        (clean (s-replace "/" "." clean))
                                        (clean (if other-files
                                                   (replace-regexp-in-string "^\\." "" (replace-regexp-in-string "^unit\\\|integration\\\|functional\\\|acceptance\\\|generative" "" clean))
                                                 clean)))
                                   (or (equal clean ns)
                                       (equal clean
                                              (replace-regexp-in-string "web-service\\.routes" "web-service" ns)))))
                               candidates)))
    (mapcar (lambda (test-ns)
              (replace-regexp-in-string "/" "." test-ns))
            test-nss)))

(defun vemv/in-clojure-implementation? ()
  (condition-case nil
      (let* ((n (clojure-find-ns))
             (f (buffer-file-name)))
        (or (string-match-p "\\.core$" n)
            (string-match-p "\\.impl$" n)
            (string-match-p "\\.routes$" n)
            (string-match-p "\\.test-helpers$" n)
            (string-match-p "\\.core-test$" n)
            (string-match-p "\\.impl-test$" n)
            (string-match-p "\\.routes-test$" n)))
    (error nil)))

(defun vemv/in-web-ns? ()
  (condition-case nil
      (let* ((n (clojure-find-ns))
             (f (buffer-file-name)))
        (or (string-match-p "web-service$" n)
            (string-match-p "web-service-test$" n)
            (string-match-p "web-service\\." n)))
    (error nil)))

(defun vemv/find-related-files (ns file substitutions)
  (let* ((ns-chunks (split-string ns "[.]" t))
         (name (car (last ns-chunks)))
         (candidates (seq-map (lambda (file-name)
                                (s-replace "src/"
                                           ""
                                           (s-replace "test/"
                                                      ""
                                                      (s-replace vemv/project-root-dir
                                                                 ""
                                                                 (replace-regexp-in-string "_"
                                                                                           "-"
                                                                                           (file-name-sans-extension file-name))))))

                              (seq-remove (lambda (x)
                                            (or (equal x file)
                                                (not (file-exists-p x))))
                                          (mapcar (lambda (pair)
                                                    (let* ((from (car pair))
                                                           (to (car (last pair))))
                                                      (replace-regexp-in-string from to file)))
                                                  substitutions)))))
    (mapcar (lambda (test-ns)
              (replace-regexp-in-string "/" "." test-ns))
            candidates)))

(defun vemv/find-impl-namespaces-of-api-ns (ns file)
  (vemv/find-related-files ns file '(("\\.clj" "/impl.clj")
                                     ("\\.clj" "/core.clj")
                                     ("\\.clj" "/routes.clj")
                                     ("_test\\.clj" "/impl_test.clj")
                                     ("_test\\.clj" "/core_test.clj")
                                     ("_test\\.clj" "/routes_test.clj")
                                     ("_test\\.clj" "/test_helpers.clj"))))

(defun vemv/find-api-namespaces-of-impl-ns (ns file)
  (vemv/find-related-files ns file '(("/impl\\.clj" ".clj")
                                     ("/core\\.clj" ".clj")
                                     ("/routes\\.clj" ".clj")
                                     ("/impl_test\\.clj" "_test.clj")
                                     ("/core_test\\.clj" "_test.clj")
                                     ("/routes_test\\.clj" "_test.clj")
                                     ("/test_helpers\\.clj" "_test.clj"))))

(defun vemv/find-web-namespaces-of-ns (ns file)
  (vemv/find-related-files ns file '(("service\\.clj" "web_service.clj")
                                     ("service_test\\.clj" "web_service_test.clj")
                                     ("service/core\\.clj" "web_service/routes.clj")
                                     ("service/core_test\\.clj" "web_service/routes_test.clj")
                                     ("service/core_test\\.clj" "web_service_test.clj")
                                     ("service/test\\.clj" "web_service_test.clj")
                                     ("service/test_helpers\\.clj" "web_service/test_helpers.clj"))))

(defun vemv/find-non-web-namespaces-of-ns (ns file)
  (vemv/find-related-files ns file '(("web_service\\.clj" "service.clj")
                                     ("web_service/routes\\.clj" "service/core.clj")
                                     ("web_service/routes_test\\.clj" "service/core_test.clj")
                                     ("web_service/routes_test\\.clj" "service_test.clj")
                                     ("web_service_test\\.clj" "service_test.clj")
                                     ("web_service/test_helpers\\.clj" "service/test_helpers.clj"))))

(defun vemv/toggle-related-files (file-name pred choice-1 choice-2)
  (unless (and file-name
               (file-exists-p file-name))
    (error "The current buffer is not visiting a file"))
  (let* ((f (if (funcall pred)
                choice-1
              choice-2))
         (namespaces-as-strings (let* ((x (funcall f
                                                   (clojure-find-ns)
                                                   file-name))
                                       (y (if x
                                              (if (listp x)
                                                  x
                                                (list x))
                                            (error "No counterpart found"))))
                                  (mapcar #'substring-no-properties
                                          y))))
    (when namespaces-as-strings
      (xref-push-marker-stack)
      (read (vemv.clojure-interaction/sync-eval-to-string
             (concat "(clojure.core/->> '" (pr-str namespaces-as-strings) " (clojure.core/map (clojure.core/comp vemv/ns-sym->filename clojure.core/symbol)))"))))))

(defun vemv/find-implementation-or-test (file-name)
  (vemv/toggle-related-files file-name 'cljr--in-tests-p 'cljr--find-source-ns-of-test-ns 'vemv/find-test-namespaces-of-source-ns))

(defun vemv/find-api-or-implementation (file-name)
  (vemv/toggle-related-files file-name 'vemv/in-clojure-implementation? 'vemv/find-api-namespaces-of-impl-ns 'vemv/find-impl-namespaces-of-api-ns))

(defun vemv/find-domain-or-web (file-name)
  (vemv/toggle-related-files file-name 'vemv/in-web-ns? 'vemv/find-non-web-namespaces-of-ns 'vemv/find-web-namespaces-of-ns))

(defun vemv/open-by-toggle-fn (f)
  (when (vemv/in-a-clojure-mode?)
    (mapcar 'vemv/open
            (funcall f (buffer-file-name)))))

(defun vemv/toggle-between-implementation-and-test ()
  "Toggle between a Clojure implementation file and its test file."
  (interactive)
  (vemv/open-by-toggle-fn 'vemv/find-implementation-or-test))

(defun vemv/toggle-between-api-and-implementation ()
  "Toggle between a Clojure API and implementation file."
  (interactive)
  (vemv/open-by-toggle-fn 'vemv/find-api-or-implementation))

(defun vemv/toggle-between-domain-and-web ()
  "Toggle between a web service and its domain layer."
  (interactive)
  (vemv/open-by-toggle-fn 'vemv/find-domain-or-web))
