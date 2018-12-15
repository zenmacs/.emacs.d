(provide 'emacs.d.overrides)

(defvar vemv.overrides/path
  (when nil
    ;; any code for setting further env-vars goes here.
    )
  "This is a defvar for avoiding acculating huge PATH values, as one reloads this file, breaking `shell-command-to-string'.")

(when nil
  ;; As one changes projects, `cd` is used. Do you wish to use something else? Place it here.
  ;; Make sure it ends with a space.
  (advice-add 'vemv.project/reset :after (argless
                                          (setq vemv.project/cd-command "my-cd "))))

;; This maps workspace names to project names.
;; Add more workspaces/projects here!
;; As you save this file, corresponding files will be automatically created in `~/.emacs.d.overrides/lib/<project-id>.el`.
;; NOTE: project names must be unique globally (i.e. you cannot have two projects called `backend`, belonging to different workspaces).
(setq vemv/available-workspaces `(("emacs" ("emacs"
                                            "overrides"))
                                  ;; Uncomment and tweak as needed:
                                  ;; ("another-workspace" ("backend" "frontend"))
                                  ;; ("cool-workspace" ("microservice-1" "microservice-2"))
                                  ))
