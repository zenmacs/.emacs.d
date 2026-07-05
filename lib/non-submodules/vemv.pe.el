(provide 'vemv.pe)

(defvar vemv/project-root-dir nil
  "Where a project is located.")

(defun vemv/pe-omit-regex (&optional dir-arg)
  (let* ((xs (list "^#" "~$" "^node_modules$"
                   "^tmp"
                   "^build"
                   "^.gradle"
                   "^.intellijPlatform"
                   "^.kotlin"
                   "^.run"
                   "^out$"
                   "^cs-ide"
                   "^storage"
                   "^codescene-vs"
                   "^opencode"
                   "^cs-darwin"
                   ".claude$"
                   ".git$" ".sass-cache" "^checkouts" ".elc$" "^backups"
                   ".next$"
                   "^integration-testing$"
                   "emacs-source"
                   "^.make"
                   "test-runner"
                   "^.java"
                   "^unzipped-jdk-source$"
                   "pom.xml" "^semantic"
                   "classes" "^.cpcache"
                   "crux" ".idea" ".shadow-cljs"
                   "package-lock.json"
                   "core.async"
                   ".vscode-test"
                   ".rebel_readline_history"
                   ".lumo-cache" "^target" "auto-save-list" "project-explorer-cache"
                   "^public$" ;; XXX ideally, only do this if the project type is ruby
                   ".nrepl-port"
                   "^dist" "^generated" ".ok$" ".DS_Store" ".lein-*" ".nrepl-* " ".eastwood" ".cljs_rhino_repl"
                   "^migrations$" "^feedscripts$" "^importers$" "^components$" "^services$" "^resources$"
                   "^.clj-kondo" "^coverage" "\.*.log$"
                   ".csv$" ".json$" ".class$" ;; codescene
                   "csv" "json" "class"       ;; codescene
                   ;; Python virtual environments;; codescene
                   "^\.venv$"))
         (xs (if (or (vemv/contains? vemv/project-root-dir "codescene/analysis")
                     (vemv/contains? default-directory "codescene/analysis")
                     (vemv/contains? dir-arg "codescene/analysis"))
                 (cons "test" (cons "src" (cons "resources" xs))) ;; XXX move to overrides
               xs)))
    (mapconcat 'identity
               xs
               "\\|")))

(setq pe/omit-regex (vemv/pe-omit-regex))
