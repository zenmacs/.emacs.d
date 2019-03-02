(require 'vemv.lang)
(require 'vemv.clojure-interaction)
(require 'vemv.project-interaction)
(require 'vemv.open)
(unless vemv/terminal-emacs?
  (require 'cider)) ;; Ideally would not be there as it slightly slows down things.
(provide 'vemv.project)

(when vemv/terminal-emacs?
  (defun cider-project-name (x)
    x)
  (defun put-clojure-indent (&rest _)))

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
     (setq vemv/project-ns-prefix nil) ;; The prefix that every namespace in this project shares. Will be ommitted from each tab reprsenting a file/ns.
     (setq vemv/repl-identifier nil) ;; The name that CIDER assigns to the project in its repls. Normally well-inferred; use this setting as a workaround.
     (setq vemv/default-clojure-file nil) ;; The file that will be open with the project, if no files were open for this project last time you quit Emacs.
     (setq vemv/parent-project-root-dirs nil) ;; The root dirs (as in `vemv/project-root-dir`) of parent projects of this project. Set vemv.project/chilren-root-dirs too if setting this.
     (setq vemv.project/chilren-root-dirs nil) ;; The root dirs (as in `vemv/project-root-dir`) of children projects of this project. Set vemv/parent-project-root-dirs too if setting this.
     (setq vemv-cleaning-namespaces nil)
     (setq vemv/modifiers/primary nil)
     (setq vemv/modifiers/secondary nil)
     (setq vemv/modifiers/tertiary nil)
     (setq cljr-warn-on-eval t) ;; https://github.com/clojure-emacs/cider/issues/2327
     (setq vemv/clj-repl-name nil)
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
     (when (not vemv-cleaning-namespaces)
       (setq cider-cljs-lein-repl vemv/default-cider-cljs-lein-repl))))

(defun vemv.project/reset ()
  (vemv.project/reset*))

(vemv.project/reset)

(defun vemv/project-dot-clj-file ()
  (let ((f (concat vemv/project-clojure-dir "project.clj")))
    (when (file-exists-p f)
      f)))

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

    (setq vemv/project-type (or vemv/project-type
                                (if (file-exists-p (concat vemv/project-root-dir "Gemfile"))
                                    :ruby
                                  (if (vemv/is-cljs-project?)
                                      :cljs
                                    :clj))))

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

    (if vemv/cider-port
        (setq vemv/clj-repl-name (concat "*cider-repl 127.0.0.1*"))
      (setq vemv/clj-repl-name (concat "*cider-repl " vemv/repl-identifier "*")))

    (if "using cider 0.16"
        (setq vemv/cljs-repl-name (concat "*cider-repl CLJS " vemv/repl-identifier "*"))
      (setq vemv/cljs-repl-name (concat "*cider-repl " vemv/repl-identifier "(cljs)*")))

    (setq vemv/default-clojure-file
          (or
           vemv/default-clojure-file
           ;; XXX should be the first existing file: .clj, .cljs or .cljc
           (concat vemv/project-clojure-dir
                   "src/"
                   vemv/project-ns-prefix
                   "/core.clj"
                   (if (eq vemv/project-type :cljs)
                       "s"
                     ""))))

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
      (load "vemv.theme"))

    (when switch-p
      (vemv/safe-select-window vemv/repl-window) ;; ensures the currently-selected project is visible
      (funcall vemv/maybe-change-project-graphically))))

(unless vemv/terminal-emacs?
  (vemv/refresh-current-project vemv/current-project))
