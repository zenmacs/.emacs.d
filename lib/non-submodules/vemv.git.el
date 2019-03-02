(require 'vemv.project)
(provide 'vemv.git)

(defun vemv/git-add-A ()
  (interactive)
  (shell-command-to-string (concat " " vemv.project/cd-command vemv/project-root-dir "; git add -A")))
