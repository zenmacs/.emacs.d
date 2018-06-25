(provide 'vemv.helm)

;; `-u foo`: case-sensitive
;; `-i foo`: case-insensitive
;; Pasting must be done with tertiary-v
;; C-S: mark occurrence
;; RET: for choosing where to search
;; TAB: generally never use.
;; RET: edit marked ocurrences (or all, by default)
;; S-RET: goto file
(defun vemv/ag-replace (&rest _)
  (interactive)
  (with-current-buffer "*helm-ag-edit*"
    (let* ((search (replace-regexp-in-string "^\-[u|i] " "" helm-ag--last-query))
           (replacement (read-from-minibuffer (concat "Replacement for `" search "`: "))))
      (when (and replacement (not (string-equal replacement "")))
        (vemv/replace-regexp-entire-buffer search replacement)
        (call-interactively 'helm-ag--edit-commit)
        (vemv/echo "Replaced!")))))

(defun vemv/abort-ag ()
  (interactive)
  (vemv/stop-using-minibuffer 'helm-ag--exit-from-edit-mode))

;; redefine for having just one action

(setq helm-ag--actions
      (helm-make-actions
       "Edit search results" #'helm-ag--edit
       "Open file"           #'helm-ag--action-find-file))

;; redefine for using the redefined helm-ag--actions

(setq helm-ag-source
      (helm-build-in-buffer-source "The Silver Searcher"
        :init 'helm-ag--init
        :real-to-display 'helm-ag--candidate-transformer
        :persistent-action 'helm-ag--persistent-action
        :fuzzy-match helm-ag-fuzzy-match
        :action helm-ag--actions
        :candidate-number-limit 9999
        :keymap helm-ag-map
        :follow (and helm-follow-mode-persistent 1)))

;; same

(setq helm-source-do-ag
      (helm-build-async-source "The Silver Searcher"
        :init 'helm-ag--do-ag-set-command
        :candidates-process 'helm-ag--do-ag-candidate-process
        :persistent-action  'helm-ag--persistent-action
        :action helm-ag--actions
        :nohighlight t
        :requires-pattern 3
        :candidate-number-limit 9999
        :keymap helm-do-ag-map
        :follow (and helm-follow-mode-persistent 1)))

(defun vemv/helm-persistent-action-all ()
  (interactive)
  (call-interactively 'helm-beginning-of-buffer)
  (setq-local vemv-current-line (vemv/current-line-number))
  (setq-local vemv-last-line (save-excursion (call-interactively 'helm-end-of-buffer) (vemv/current-line-number)))
  (call-interactively 'helm-beginning-of-buffer)
  (dotimes (i 200) ; while (not (eq vemv-current-line vemv-last-line))
    (setq-local vemv-current-line (vemv/current-line-number))
    (call-interactively 'helm-execute-persistent-action)
    (call-interactively 'helm-next-line))
  
  ;; Jump to the last file, so one ensures that:
  ;;   helm is closed. Less steps
  ;;   the open files are shown. Helm likes to jump back to *scratch* after completion 
  (helm-select-nth-action 1))
