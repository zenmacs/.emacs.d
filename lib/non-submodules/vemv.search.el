;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(provide 'vemv.search)

(defun vemv/search-in-this-buffer ()
  (ignore-errors
    (call-interactively 'search-forward)
    (setq vemv-last-search (first minibuffer-history))))

(defun vemv/repeat-last-search-in-this-buffer ()
  (interactive)
  (ignore-errors (search-forward vemv-last-search)))
