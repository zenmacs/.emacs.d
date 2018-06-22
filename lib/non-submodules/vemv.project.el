(require 'vemv.lang)
(provide 'vemv.project)

(setq vemv/using-nrepl t)

(setq vemv/initial-cider-lein-parameters cider-lein-parameters)

;; - make `setq`s defuns
;; - infer project from currently open file
;; - use inferred value as implicit argument to these defuns

;; ~/.emacs.d.overrides/lib/emacs.d.overrides.el must have a value such as:
;; `(setq vemv/available-projects '("gpm" "ventas" "jumbo" "assign" "overrides" "emacs"))`,
;; where each identifier corresponds with a .el file (example: `vemv.project.gpm.el`)

(defun vemv/root-marker ()
  "A string that proves that a project is a full directory, rather than a project id (name - like 'gpm')"
  "/Users")

(condition-case nil
    (load (concat "vemv.project." vemv/current-project))
  (error nil))

;; XXX document the meaning of each of these?
(defmacro vemv.project/reset ()
  `(progn
     (set-variable 'cider-lein-parameters vemv/initial-cider-lein-parameters)
     (setq vemv/project-type nil)
     (setq vemv/using-component-reloaded-workflow nil) ;; whether the project uses com.stuartsierra.component.user-helpers
     (setq vemv/cider-port nil)
     (setq vemv/project-initializers nil)
     (setq vemv/project-root-dir nil)
     (setq vemv/project-clojure-dir nil)
     (setq vemv/project-fiplr-dir nil)
     (setq vemv/project-ns-prefix  nil)
     (setq vemv/repl-identifier nil)
     (setq vemv/default-clojure-file nil)
     (setq vemv-cleaning-namespaces nil)
     (setq vemv/modifiers/primary nil)
     (setq vemv/modifiers/secondary nil)
     (setq vemv/modifiers/tertiary nil)
     (setq vemv/clj-repl-name nil)
     (setq vemv/cljs-repl-name nil)
     (setq clojure-indent-style :always-align)
     (setq clojure-align-forms-automatically nil)
     (setq whitespace-line-column 131)
     (setq vemv/clojure-reload-command nil)
     ;; avoids expensive computation on mode-line
     (setq vemv/cached-projects-with-initialization-files (vemv/projects-with-initialization-files))))

(vemv.project/reset)

(defun vemv/on-the-fly-project? (which &optional candidates)
  (not (member which (or candidates (vemv/projects-with-initialization-files)))))

(defun vemv/all-project-names ()
  (let ((candidates vemv/cached-projects-with-initialization-files))
    (mapcar (lambda (x)
              (if (vemv/on-the-fly-project? x candidates)
                  (cider-project-name x)
                  x))
            vemv/all-projects)))

(defun vemv/refresh-current-project (which &optional switch-p)
  (let ((on-the-fly-project (vemv/on-the-fly-project? which vemv/cached-projects-with-initialization-files)))
    (vemv.project/reset)

    (when which
      (setq vemv/current-project (if on-the-fly-project
                                     (cider-project-name which)
                                     which)))
    
    ;; XXX assert default value exists in filesystem
    ;; XXX the case statements are hardcoded. parameterize
    (setq vemv/project-root-dir
          (or vemv/project-root-dir (vemv/dir-for-project vemv/current-project)))

    (setq vemv/project-root-dir (concat vemv/project-root-dir (if (s-ends-with? "/" vemv/project-root-dir)
                                                                  ""
                                                                  "/")))
    
    (when (and which (not on-the-fly-project))
      (condition-case nil
          (load (concat "vemv.project." which))
        (error nil)))

    (setq vemv/project-clojure-dir (or vemv/project-clojure-dir vemv/project-root-dir))

    (setq vemv/project-clojure-dir (concat vemv/project-clojure-dir (if (s-ends-with? "/" vemv/project-clojure-dir)
                                                                        ""
                                                                        "/")))
    
    (setq vemv/project-type (or vemv/project-type (if (file-exists-p (concat vemv/project-root-dir "Gemfile")) :ruby :clj)))
    
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
    (setq vemv/cljs-repl-name (concat "*cider-repl " vemv/repl-identifier "(cljs)*"))

    (setq vemv/default-clojure-file
          (or
           vemv/default-clojure-file
           ;; XXX should be the first existing file: .clj, .cljs or .cljc
           (concat vemv/project-clojure-dir "src/" vemv/project-ns-prefix "/core.clj" (if (eq vemv/project-type :cljs) "s" ""))))

    (setq vemv-cleaning-namespaces nil)
    (setq vemv/modifiers/primary "C")
    (setq vemv/modifiers/secondary "M")
    (setq vemv/modifiers/tertiary "s")
    
    (call-interactively 'whitespace-mode)
    (call-interactively 'whitespace-mode)
    
    (when switch-p
      (select-window vemv/repl2) ;; ensures the currently-selected project is visible
      (funcall vemv/maybe-change-project-graphically))

    (when (not (gethash vemv/current-project vemv/chosen-file-buffer-order))
        (vemv/open-recent-file-for-this-project!))))

(vemv/refresh-current-project vemv/current-project)
    
