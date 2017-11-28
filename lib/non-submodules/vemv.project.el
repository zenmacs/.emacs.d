(provide 'vemv.project)

(setq vemv/using-nrepl t)
(setq vemv-home (getenv "HOME"))

;; - make `setq`s defuns
;; - infer project from currently open file
;; - use inferred value as implicit argument to these defuns
(setq vemv/current-project (if t "gpm" "assign"))

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
              ("gpm" (concat vemv-home "/fiplr-gpm")) ;; this is a directory with symlinks to src and test. avoids displaying compilation artifacts, unrelated files etc
              ("assign" (concat vemv/project-clojure-dir "/src")))
       vemv/project-clojure-dir))

(setq vemv/project-ns-prefix ;; the bit to remove in tabs (mode-line). also identifies repls (important)
      (or
       (pcase vemv/current-project
              ("gpm" "horizon"))
       vemv/current-project))

(setq vemv/default-clojure-file
      (or
       (pcase vemv/current-project
              ("gpm" (concat vemv/project-clojure-dir "src/horizon/desktop/core.cljs")))
    ;; XXX should be the first existing file: .clj, .cljs or .cljc
       (concat vemv/project-clojure-dir "/src/" vemv/project-ns-prefix "/core.cljs")))

(setq vemv-cleaning-namespaces nil)

(setq vemv/modifiers/primary "C")

(setq vemv/modifiers/secondary "M")

(setq vemv/modifiers/tertiary "s")
