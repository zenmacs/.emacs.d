(require 'vemv.lang)
(provide 'vemv.project)

(setq vemv/using-nrepl t)

(setq vemv/initial-cider-lein-parameters cider-lein-parameters)

;; - make `setq`s defuns
;; - infer project from currently open file
;; - use inferred value as implicit argument to these defuns

;; ~/.emacs.d.overrides/lib/emacs.d.overrides.el must have a value such as:
;; `(setq vemv/all-projects '("gpm" "ventas" "jumbo" "assign" "overrides" "emacs"))`,
;; where each identifier corresponds with a .el file (example: `vemv.project.gpm.el`)

(defun vemv/root-marker ()
  "A string that proves that a project is a full directory, rather than a project id (name - like 'gpm')"
  "/Users")

(when (not (vemv/starts-with vemv/current-project (vemv/root-marker)))
  (load (concat "vemv.project." vemv/current-project)))

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
     (setq vemv/cljs-repl-name nil)))

(vemv.project/reset)

(defun vemv/refresh-current-project (which &optional switch-p)
  (let ((on-the-fly-project (vemv/starts-with which (vemv/root-marker))))
    (vemv.project/reset)

    (when which
      (setq vemv/current-project (if on-the-fly-project
                                     (cider-project-name which)
                                     which)))
    
    ;;gpm defines its own,
    ;; trailing slash required
    ;; XXX assert default value exists in filesystem
    ;; XXX the case statements are hardcoded. paremeterize
    (setq vemv/project-root-dir
          (or vemv/project-root-dir
              (if on-the-fly-project
                  which
                  (concat vemv-home "/" vemv/current-project))))

    ;; must set before (load), but if we do so, gpm loses its customization
    ;; should equal to vemv/project-root-dir unless project.clj is non-root
    (setq vemv/project-clojure-dir (or vemv/project-clojure-dir vemv/project-root-dir))

    (when (and which (not on-the-fly-project))
      (load (concat "vemv.project." which)))

    (setq vemv/project-type (or vemv/project-type :clj))
    
    (setq vemv/project-initializers (or vemv/project-initializers (lambda ())))

    (setq vemv/project-fiplr-dir
          (or vemv/project-fiplr-dir
              (if on-the-fly-project
                  which
                  (concat vemv/project-clojure-dir "/src"))))

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
           (concat vemv/project-clojure-dir "/src/" vemv/project-ns-prefix "/core.clj" (if (eq vemv/project-type :cljs) "s" ""))))

    (setq vemv-cleaning-namespaces nil)
    (setq vemv/modifiers/primary "C")
    (setq vemv/modifiers/secondary "M")
    (setq vemv/modifiers/tertiary "s")
    
    (when switch-p
      (vemv/next-file-buffer)
      (vemv/previous-file-buffer)
      (select-window vemv/project-explorer-window)
      (let ((default-directory vemv/project-root-dir))
        (call-interactively 'project-explorer-open))
      (select-window vemv/repl2)
      (unless (or cider-launched vemv-cider-connected (cider-connected-p))
        (vemv/send :shell nil vemv/project-root-dir)
        (delay (argless
                (comint-clear-buffer)
                (select-window vemv/main_window))
               0.3)))

    (when (not (gethash vemv/current-project vemv/chosen-file-buffer-order))
        (vemv/open-recent-file-for-this-project!))))

(vemv/refresh-current-project vemv/current-project)
    
