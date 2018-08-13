;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(provide 'vemv.edit)

(defun vemv/replace-regexp-entire-buffer (pattern replacement)
  "Perform regular-expression replacement throughout buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (replace-match replacement))))

(defun vemv/backspace ()
  (interactive)
  (if (region-active-p)
      (progn (call-interactively 'kill-region)
             (pop kill-ring))
    (paredit-backward-delete)))

(defun vemv/force-backspace ()
  "Performs a deletion, overriding paredit safeguards"
  (interactive)
  (if (region-active-p)
      (progn (call-interactively 'kill-region))
    (delete-region (dec (point)) (point))))

(defun vemv/end-of-line-code* (skip-last-step)
  (let* ((bolpos (progn (beginning-of-line) (point)))
         (eolpos (progn (end-of-line) (point))))
    (if (comment-search-backward bolpos t)
        (search-backward-regexp comment-start-skip bolpos 'noerror))
    (skip-syntax-backward " " bolpos)
    (unless skip-last-step
      (while (member (vemv/char-at-left)
                     `(" " ";" "/" "#"))
        (left-char)))))

(defun vemv/end-of-line-code (skip-last-step)
  (interactive "^")
  (save-match-data
    (vemv/end-of-line-code* skip-last-step)))

(defun vemv/end-of-line-or-code ()
  (interactive "^")
  (ignore-errors (let ((here (point))
                       (region-active (region-active-p))
                       (in-comment (and (vemv/in-a-lisp-mode?)
                                        (paredit-in-comment-p))))
                   (vemv/end-of-line-code region-active)
                   (if (or (= here (point))
                           (bolp)
                           in-comment)
                       (end-of-line))
                   (if (and (= here (point))
                            in-comment
                            (eolp))
                       (vemv/end-of-line-code region-active)))))

(defun vemv/delete-only-this-line ()
  (end-of-line)
  (cua-set-mark)
  (previous-line)
  (end-of-line)
  (call-interactively 'kill-region))

(defun vemv/delete-this-line ()
  "Deletes the entire current line regardless of its contents, and any preceding empty lines."
  (interactive)
  (vemv/delete-only-this-line)
  (let ((line (vemv/current-line)))
    (if (and (vemv/line-empty? line)
             (vemv/line-empty? (vemv/previous-line)))
        (vemv/delete-this-line)
      (progn
        (next-line)
        (back-to-indentation)))))
