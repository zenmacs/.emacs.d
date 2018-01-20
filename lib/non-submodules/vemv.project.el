(require 'vemv.lang)
(provide 'vemv.project)

(setq vemv/using-nrepl t)
(setq vemv-home (getenv "HOME"))

;; - make `setq`s defuns
;; - infer project from currently open file
;; - use inferred value as implicit argument to these defuns

(setq vemv/all-projects '("gpm" "jumbo" "assign"))

(load (concat "vemv.project." vemv/current-project))

(defmacro vemv.project/reset ()
  `(progn
     (setq vemv/project-type nil)
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
  
  (vemv.project/reset)

  (when which
    (setq vemv/current-project which))
  
  ;;gpm defines its own,
  ;; trailing slash required
  ;; XXX assert default value exists in filesystem
  ;; XXX the case statements are hardcoded. paremeterize
  (setq vemv/project-root-dir
        (or vemv/project-root-dir
            (concat vemv-home "/" vemv/current-project)))

  ;; must set before (load), but if we do so, gpm loses its customization
  ;; should equal to vemv/project-root-dir unless project.clj is non-root
  (setq vemv/project-clojure-dir (or vemv/project-clojure-dir vemv/project-root-dir))

  (when which
    (load (concat "vemv.project." which)))

  (setq vemv/project-type (or vemv/project-type :clj))
  
  (setq vemv/project-initializers (or vemv/project-initializers (lambda ())))

  (setq vemv/project-fiplr-dir
        (or vemv/project-fiplr-dir
            (concat vemv/project-clojure-dir "/src")))
  
  ;; the bit to remove in tabs (mode-line). also identifies repls (important)
  (setq vemv/project-ns-prefix (or vemv/project-ns-prefix vemv/current-project))

  ;; XXX automatically infer instead
  (setq vemv/repl-identifier (or vemv/repl-identifier vemv/project-ns-prefix))

  (setq vemv/clj-repl-name (concat "*cider-repl " vemv/repl-identifier "*"))
  (setq vemv/cljs-repl-name (concat "*cider-repl CLJS " vemv/repl-identifier "*"))

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
    (vemv/save-window-excursion
     (select-window vemv/main_window)
     (vemv/close-this-buffer)
     (vemv/open vemv/default-clojure-file)
     (select-window vemv/project-explorer-window)
     (let ((default-directory vemv/project-root-dir))
       (call-interactively 'project-explorer-open)))
    (select-window vemv/repl2)
    (unless (or cider-launched vemv-cider-connected (cider-connected-p))
      (vemv/send :shell nil vemv/project-root-dir)
      (delay (argless (vemv/save-window-excursion (comint-clear-buffer))) 0.3))))

(vemv/refresh-current-project vemv/current-project)

(defun vemv-source (filename)
  (mapcar
   (lambda (x)
     (let* ((xy (s-split "=" (s-chop-prefix "+" x)))
            (x (car xy))
            (y (car (last xy))))
       (setenv x y)))
   (-filter
    (lambda (x) (vemv/starts-with x "+"))
    (s-split
     "\n"
     (shell-command-to-string (concat "diff -u  <(true; export) <(source " filename "; export) | tail -n +4"))))))

(vemv-source "/Users/vemv/gpm/src/environment.sh")
(vemv-source "/Users/vemv/gpm/src/custom-environment.sh")
(vemv-source "/Users/vemv/.ldap")

(setenv "PATH" (concat (getenv "PATH") ":" vemv-home "/bin"))
(setenv "HORIZON_IS_DEVELOPMENT_SERVER" "true")
