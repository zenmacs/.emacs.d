(provide 'vemv.paredit)

(setq vemv/kill-list-bound 10)

;; The 10 last elements copied to the clipboard.
;; I don't use kill-ring, since third-parties (e.g. paredit) can mess with it
(setq vemv/kill-list (-repeat vemv/kill-list-bound nil))

(setq vemv/line-before-formatting nil)
(setq vemv/token-before-formatting nil)

(defun vemv/save-position-before-formatting ()
  (setq vemv/line-before-formatting (max 0 (- (vemv/current-line-number) 3)))
  (setq vemv/token-before-formatting (vemv/sexpr-content)))

;; XXX unused
;; XXX should be per-buffer (see vemv/save-all-buffers-for-this-project)
(defun vemv/restore-position-before-formatting ()
  (beginning-of-buffer)
  (dotimes (i vemv/line-before-formatting)
    (next-line))
  (ignore-errors (search-forward vemv/token-before-formatting))
  (paredit-backward)
  (back-to-indentation))

(defun vemv/save (&optional b)
  (interactive)
  (let ((b (or b (current-buffer))))
    (with-current-buffer b
      (unless (or (eq clojure-indent-style :align-arguments) clojure-align-forms-automatically)
        (when (vemv/ciderable-p)
          (vemv/save-position-before-formatting)
          (let ((old (substring-no-properties (buffer-string))))
            (save-excursion
              (condition-case nil (cider-format-buffer)
                (error
                 (erase-buffer)
                 (insert old)))))))
      (save-buffer))))

(defun vemv/tab ()
  (interactive)
  (or (and (or
            (vemv/in-indentation-point-p)
            (vemv/non-completable-char-p))
           (or (call-interactively 'indent-for-tab-command)
               t))
      (call-interactively 'company-complete)
      (call-interactively 'company-dabbrev)))

(defun vemv/cut ()
  (interactive)
  (vemv/bounded-list/insert-at-head! (vemv/kill nil nil)
                                     vemv/kill-list
                                     vemv/kill-list-bound))

(defun vemv/copy-inserting-at-kill-list ()
  (interactive)
  (vemv/bounded-list/insert-at-head! (vemv/copy-selection-or-next-sexpr)
                                     vemv/kill-list
                                     vemv/kill-list-bound))

(defun vemv/maybe-indent-on-paste (content)
  (when (and (vemv/in-a-lisp-mode?)
             (s-match "^\s*[\(|[|{]" content))
    (paredit-backward)
    (vemv/indent)))

(defun vemv/paste-from-clipboard ()
  (let ((content (substring-no-properties (simpleclip-get-contents))))
    (insert content)
    (vemv/maybe-indent-on-paste content)))

(defun vemv/paste-from-kill-list ()
  (let ((content (car vemv/kill-list)))
    (insert content)
    (vemv/maybe-indent-on-paste content)))
