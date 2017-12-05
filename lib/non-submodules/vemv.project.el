(provide 'vemv.project)

(setq vemv/using-nrepl t)
(setq vemv-home (getenv "HOME"))

;; - make `setq`s defuns
;; - infer project from currently open file
;; - use inferred value as implicit argument to these defuns
(setq vemv/current-project (or
                               "gpm"
                              ; "assign"
                              "jumbo"
                              ))

(setq vemv/project-type
  (or (pcase vemv/current-project ("gpm" :cljs)
                                  ("assign" :cljs)
                                  ("jumbo" :clj))
      :clj))

(defun vemv/project-initializers ()
  (or
   (pcase vemv/current-project
          ("gpm" (lambda ()
                   (shell-command-to-string "source ~/.zshrc; cd ~/gpm/src; make clean")))
          ("assign" (lambda ()
                      (shell-command-to-string "source ~/.zshrc; cd ~/assign; lein clean"))))
   (lambda ())))

;; XXX assert default value exists in filesystem
;; XXX the case statements are hardcoded. paremeterize
(setq vemv/project-root-dir ;; trailing slash required
      (or
       (pcase vemv/current-project
              ("gpm" (concat vemv-home "/gpm/src/")))
       (concat vemv-home "/" vemv/current-project)))

;; should equal to vemv/project-root-dir unless project.clj is non-root
(setq vemv/project-clojure-dir
      (or
       (pcase vemv/current-project
              ("gpm" (concat vemv/project-root-dir "horizon/")))
       vemv/project-root-dir))

(setq vemv/project-fiplr-dir
      (or
       (pcase vemv/current-project
              ("gpm" (concat vemv-home "/fiplr-gpm"))) ;; this is a directory with symlinks to src and test. avoids displaying compilation artifacts, unrelated files etc
       (concat vemv/project-clojure-dir "/src")))

(setq vemv/project-ns-prefix ;; the bit to remove in tabs (mode-line). also identifies repls (important)
      (or
       (pcase vemv/current-project
              ("gpm" "horizon")
              ("jumbo" "vemv"))
       vemv/current-project))

;; XXX automatically infer instead
(setq vemv/repl-identifier
  (or (pcase vemv/current-project
        ("jumbo" "jumbo"))
      vemv/project-ns-prefix))

(setq vemv/default-clojure-file
      (or
       (pcase vemv/current-project
              ("gpm" (concat vemv/project-clojure-dir "src/horizon/desktop/core.cljs"))
              ("assign" (concat vemv/project-clojure-dir "/src/" vemv/project-ns-prefix "/core.cljs"))
              ("jumbo" (concat vemv/project-clojure-dir "/clojure/" vemv/project-ns-prefix "/jumbo.clj")))
       ;; XXX should be the first existing file: .clj, .cljs or .cljc
       (concat vemv/project-clojure-dir "/src/" vemv/project-ns-prefix "/core.clj" (if (eq vemv/project-type :cljs "s" "")))))

(setq vemv-cleaning-namespaces nil)

(setq vemv/modifiers/primary "C")

(setq vemv/modifiers/secondary "M")

(setq vemv/modifiers/tertiary "s")
