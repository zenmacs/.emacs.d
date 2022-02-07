;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(require 'string-inflection)
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
    (when comment-start-skip ;; avoid error in haml-mode
      (if (comment-search-backward bolpos t)
          (search-backward-regexp comment-start-skip bolpos 'noerror)))
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

(defun vemv/indent-region (&optional n)
  "Indents the current region, or the current line"
  (interactive)
  (let* ((was-selected (region-active-p))
         (n (or n 2))
         (beg (if was-selected
                  (region-beginning)
                (save-excursion
                  (beginning-of-line-text)
                  (point))))
         (end (if was-selected
                  (region-end)
                (save-excursion
                  (end-of-line)
                  (point))))
         (beg (save-excursion
                (goto-char beg)
                (beginning-of-line)
                (point)))
         (end (save-excursion
                (goto-char end)
                (end-of-line)
                (point))))
    (indent-rigidly beg end n)
    (when was-selected
      (select-region beg end))))

(defun vemv/unindent-region ()
  "Unindents the current region, or the current line"
  (interactive)
  (vemv/indent-region -2))

(defun vemv/delete-file-and-buffer ()
  "Kills the current buffer and deletes the file it is visiting."
  (interactive)
  (replying-yes
   (when-let* ((filename (buffer-file-name)))
     (->> vemv/chosen-file-buffer-order
          (gethash vemv/current-project)
          (-clone)
          (mapcar (lambda (_b)
                    (if (string-equal _b filename)
                        (when-let* ((b (get-file-buffer _b)))
                          (with-current-buffer b
                            (vemv/close-this-buffer :noswitch)))))))
     (delete-file filename)
     (vemv/refresh-file-caches nil :force))))

(defun vemv/sudo ()
  (interactive)
  (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))

(defun vemv/rename-this-file ()
  (interactive)
  (when-let* ((f (buffer-file-name)))
    (let* ((n (read-string (concat "Rename " f " to: ") f)))
      (when (not (string-equal f n))
        (make-directory (file-name-directory n) t)
        (let* ((_ (rename-file f n))
               (b (-some-> f get-file-buffer)))
          (when (and b
                     (not (buffer-modified-p b)))
            (kill-buffer b)
            (vemv/open n)))))))

(defun vemv/downcase-region ()
  (interactive)
  (downcase-region (mark) (point)))

(defun vemv/upcase-region ()
  (interactive)
  (upcase-region (mark) (point)))

(defun vemv/capitalize-region ()
  (interactive)
  (capitalize-region (mark) (point)))

(defalias 'kebab-region 'string-inflection-kebab-case)

(defalias 'underscore-region 'string-inflection-underscore)

(defalias 'camel-region 'string-inflection-camelcase)

(defalias 'lower-camel-region 'string-inflection-lower-camelcase)
