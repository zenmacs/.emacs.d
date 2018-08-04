;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(provide 'vemv.helm)

(defun vemv/ag-replace (&rest _)
  (interactive)
  (with-current-buffer "*helm-ag-edit*"
    (let* ((search (replace-regexp-in-string "^\-[s|i|Q]+ " "" helm-ag--last-query))
           (replacement (vemv/read-from-minibuffer (concat "Replacement for `" search "`: "))))
      (if (not replacement)
          (helm-ag--exit-from-edit-mode)
          (vemv/replace-regexp-entire-buffer search replacement)
          (call-interactively 'helm-ag--edit-commit)
          (vemv/echo "Replaced!")))))

(comm

 ;; these break two things:
 ;; - when no buffers are open and I C-a, I won't see the opened buffers until I vemv/next-file-buffer
 ;; - S-RET.
 ;; The original intent was that on C-A, the currently-open buffer is still the current one.

 (defun helm-ag--persistent-action (candidate)
   "For find-file-noselect"
   (let ((find-func (if helm-ag-use-temp-buffer
                        #'helm-ag--open-file-with-temp-buffer
                        #'find-file-noselect)))
     (helm-ag--find-file-action candidate find-func (helm-ag--search-this-file-p) t)
          (helm-highlight-current-line)))

 (defun helm-ag--action-find-file (candidate)
   "For find-file-noselect"
   (helm-ag--find-file-action candidate 'find-file-noselect (helm-ag--search-this-file-p))))

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
  (call-interactively 'helm-end-of-buffer)
  (let* ((currently-open (window-buffer vemv/main_window))
         (vemv-last-line (helm-candidate-number-at-point)))

    (call-interactively 'helm-beginning-of-buffer)
    (dotimes (i (max 0 (- vemv-last-line 1)))
      (call-interactively 'helm-execute-persistent-action)
      (call-interactively 'helm-next-line))

    ;; Jump to the last file, so one ensures that:
    ;;   helm is closed. Less steps
    ;;   the open files are shown. Helm likes to jump back to *scratch* after completion
    (helm-select-nth-action 1)))

(setq helm-mode-line-string "")

(defun helm-next-page (&rest v)
  "Rebinds C-v"
  (interactive)
  (vemv/paste-from-clipboard))

(defun helm-ag--previous-file ()
  "Rebinds <left>"
  (interactive)
  (left-char))

(defun helm-ag--next-file ()
  "Rebinds <right>"
  (interactive)
  (right-char))

(defun helm-do-ag--helm ()
  "Adds a :prompt option"
  (let ((search-dir (if (not (helm-ag--windows-p))
                        helm-ag--default-directory
                        (if (helm-do-ag--target-one-directory-p helm-ag--default-target)
                            (car helm-ag--default-target)
                            helm-ag--default-directory))))
    (helm-attrset 'name (helm-ag--helm-header search-dir)
                  helm-source-do-ag)
    (helm :sources '(helm-source-do-ag) :buffer "*helm-ag*" :keymap helm-do-ag-map
          :input (or (helm-ag--marked-input t)
                     (helm-ag--insert-thing-at-point helm-ag-insert-at-point))
          :history 'helm-ag--helm-history
          :prompt "(`-s ` prefix: case-sensitive; `-i `: case-insensitive; `-Q `: literal search) Pattern: ")))

(defun helm-ag--edit (_candidate)
  "Customizes header-line-format"
  (let* ((helm-buf-dir (or helm-ag--default-directory
                           helm-ag--last-default-directory
                           default-directory))
         (default-directory helm-buf-dir))
    (with-current-buffer (get-buffer-create "*helm-ag-edit*")
      (erase-buffer)
      (setq-local helm-ag--default-directory helm-buf-dir)
      (unless (helm-ag--vimgrep-option)
        (setq-local helm-ag--search-this-file-p
                    (assoc-default 'search-this-file (helm-get-current-source))))
      (let (buf-content)
        (with-current-buffer (get-buffer "*helm-ag*")
          (goto-char (point-min))
          (forward-line 1)
          (let* ((body-start (point))
                 (marked-lines (cl-loop for ov in (overlays-in body-start (point-max))
                                        when (eq 'helm-visible-mark (overlay-get ov 'face))
                                        return (helm-marked-candidates))))
            (if (not marked-lines)
                (setq buf-content (buffer-substring-no-properties
                                   body-start (point-max)))
                (setq buf-content (concat (string-join marked-lines "\n") "\n")))))
        (insert buf-content)
        (add-text-properties (point-min) (point-max)
                             '(read-only t rear-nonsticky t front-sticky t))
        (let ((inhibit-read-only t)
              (regexp (helm-ag--match-line-regexp)))
          (setq header-line-format
                " RET (if prompted) or C-c C-c (otherwise): perform replacement.")
          (goto-char (point-min))
          (while (re-search-forward regexp nil t)
            (let ((file-line-begin (match-beginning 4))
                  (file-line-end (match-end 4))
                  (body-begin (match-beginning 3))
                  (body-end (match-end 3)))
              (add-text-properties file-line-begin file-line-end
                                   '(face font-lock-function-name-face
                                          intangible t))
              (remove-text-properties body-begin body-end '(read-only t))
              (set-text-properties body-end (1+ body-end)
                                   '(read-only t rear-nonsticky t))))))))
  (other-window 1)
  (switch-to-buffer (get-buffer "*helm-ag-edit*"))
  (goto-char (point-min))
  (setq next-error-function 'compilation-next-error-function)
  (setq-local compilation-locs (make-hash-table :test 'equal :weakness 'value))
  (use-local-map helm-ag-edit-map))

(defsubst helm-ag--helm-header (dir)
  "Places C-a tip"
  (if helm-ag--buffer-search
      "Search Buffers"
      "S-RET: open file. C-a: open all files. C-SPC: mark occurrence. RET: edit marked (or all) ocurrences for replacement"))
