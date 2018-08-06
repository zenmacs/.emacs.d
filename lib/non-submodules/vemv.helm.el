;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(provide 'vemv.helm)

(defun vemv/helm-search-and-replace (&optional follow)
  (vemv/set-helm-ag-source follow)
  (vemv/safe-select-window vemv/main_window)
  (helm-attrset 'follow (when follow 1) helm-ag-source)
  (helm-attrset 'follow (when follow 1) helm-source-do-ag)
  (setq helm-follow-mode-persistent follow)
  (let* ((helm-ag-use-temp-buffer follow)
         (default-directory vemv/project-clojure-dir)
         (require-final-newline (not vemv/no-newline-at-eof))
         (where (ido-read-directory-name "Where: ")))
    (assert (file-exists-p where))
    (helm-do-ag where)))

(defun vemv/helm-search-and-replace-with-previews ()
  "Performs a vemv/helm-search-and-replace, but with helm 'follow' mode, namely there's a preview of each ocurrence
(as a temporary buffer), so one can see a larger context."
  (vemv/helm-search-and-replace t))

(setq helm-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "<tab>")      (argless))
    (define-key map [(shift return)]   (argless
                                        (interactive)
                                        (helm-select-nth-action 1)))
    (define-key map (kbd "C-a")        'vemv/helm-persistent-action-all)
    (define-key map (kbd "<down>")     'helm-next-line)
    (define-key map (kbd "<up>")       'helm-previous-line)
    (define-key map (kbd "<prior>")    'helm-previous-page)
    (define-key map (kbd "<next>")     'helm-next-page)
    (define-key map (kbd "<C-v>")      'vemv/paste-from-clipboard)
    (define-key map (kbd "C-g")        'helm-keyboard-quit)
    (define-key map (kbd "<right>")    'right-char)
    (define-key map (kbd "<left>")     'left-char)
    (define-key map (kbd "<RET>")      'helm-maybe-exit-minibuffer)
    (define-key map (kbd "C-j")        'helm-execute-persistent-action)
    (define-key map (kbd "C-SPC")      'helm-toggle-visible-mark)
    (define-key map (kbd "M-[")        nil)
    (define-key map (kbd "C-k")        'helm-delete-minibuffer-contents)
    (define-key map (kbd "C-x C-f")    'helm-quit-and-find-file)
    (define-key map (kbd "C-s")        'undefined)
    (define-key map (kbd "M-s")        'undefined)
    (define-key map (kbd "C-;")        'helm-toggle-truncate-line)
    ;; Allow to eval keymap without errors.
    (define-key map [f1] nil)
    (define-key map (kbd "C-h C-h")    'undefined)
    (define-key map (kbd "C-h h")      'undefined)
    (helm-define-key-with-subkeys map
      (kbd "C-w") ?\C-w 'helm-yank-text-at-point
      '((?\C-_ . helm-undo-yank-text-at-point)))
    ;; Use `describe-mode' key in `global-map'.
    (cl-dolist (k (where-is-internal 'describe-mode global-map))
      (define-key map k 'helm-help))
    (define-key map (kbd "C-c ?")    'helm-help)
    map))

(setq helm-ag-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    map))

(setq helm-do-ag-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map helm-ag-map)
        map))

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

(defun vemv/set-helm-ag-source (follow)
  "Parameterizes :follow and customizes helm-ag--actions"
  (setq helm-ag-source
        (helm-build-in-buffer-source "The Silver Searcher"
          :init 'helm-ag--init
          :real-to-display 'helm-ag--candidate-transformer
          :persistent-action 'helm-ag--persistent-action
          :fuzzy-match helm-ag-fuzzy-match
          :action helm-ag--actions
          :candidate-number-limit 9999
          :keymap helm-ag-map
          :follow follow))

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
          :follow follow)))

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
