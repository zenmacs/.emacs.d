;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(provide 'vemv.buffer-querying)

(defun vemv/in-a-clojure-mode? (&optional m)
  (let ((mode (or m major-mode)))
    (or (eq mode 'clojure-mode)
        (eq mode 'clojurec-mode)
        (eq mode 'clojurescript-mode)
        (eq mode 'cider-repl-mode))))

(defun vemv/in-a-lisp-mode? (&optional m)
  (let ((mode (or m major-mode)))
    (or (vemv/in-a-clojure-mode? mode)
        (eq mode 'emacs-lisp-mode)
        (eq mode 'inferior-emacs-lisp-mode))))

(defun vemv/selected-region ()
  "Returns the selected region as a string. Side effects free."
  (kill-ring-save (mark) (point))
  (let ((result (substring-no-properties (car kill-ring))))
    (pop kill-ring)
    result))

(defun vemv/current-frame-buffers ()
  (mapcar #'buffer-name (mapcar #'window-buffer (window-list))))

(defun vemv/all-buffers ()
  (buffer-list))

(defun vemv/all-buffer-names ()
  (mapcar #'buffer-name (vemv/all-buffers)))

(defun vemv/active-modes ()
  "Returns a list of the minor modes that are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil)))
          minor-mode-list)
    active-modes))

(defun vemv/current-line-contents ()
  "Returns the content of the line at which the point is currently located. Side effects free."
  (interactive)
  (let ((result (buffer-substring-no-properties (line-beginning-position 1) (line-beginning-position 2))))
    (if (equal result "") ;; abstact away EOFs
        "\n"
        result)))

(defun vemv/previous-line ()
  (save-excursion
    (call-interactively 'previous-line)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun vemv/current-line ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun vemv/current-line-number ()
  (save-excursion
    (beginning-of-line)
    (1+ (count-lines 1 (point)))))

(defun vemv/current-char-at-point (&optional offset)
  "Returns the character -as a string- hovered by the point, or a contiguous one, if an integer offset is specified."
  (interactive)
  (let ((s (buffer-size)))
    (if (eq s 0)
        ""
        (let* ((o (or offset 0))
               (beg (+ (point) o))
               (end (+ beg 1))
               (end (if (> end s) s end))
               (_ (kill-ring-save beg end))
               (result (substring-no-properties (car kill-ring))))
          (pop kill-ring)
          result))))

(defun vemv/in-clojure-mode? ()
  ;; better: derived-mode-p
  (vemv/contains? (pr-str major-mode) "clojure"))

(defun vemv/current-main-buffer-is-cljs ()
  (or (vemv/contains? (buffer-name) ".cljs")
      (and (vemv/contains? (buffer-name) ".cljc")
           (eq vemv/project-type :cljs))))

(defun vemv/line-empty? (line)
  (or (= 0 (length line))
      (every (lambda (char) (= char 32)) line)))

(defun vemv/at-beginning-of-line-p ()
  (eq (point) (save-excursion (beginning-of-line) (point))))

(defun vemv/at-end-of-line-p ()
  (eq (point) (save-excursion (end-of-line) (point))))

(defun vemv/char-at-left ()
  (save-excursion
    (push-mark)
    (left-char)
    (vemv/selected-region)))

(defun vemv/chars-at-left ()
  (save-excursion
    (push-mark)
    (move-beginning-of-line 1)
    (vemv/selected-region)))

(defun vemv/char-at-right ()
  (save-excursion
    (push-mark)
    (right-char)
    (vemv/selected-region)))

(defun vemv/chars-at-right ()
  (save-excursion
    (push-mark)
    (move-end-of-line 1)
    (vemv/selected-region)))

(defun vemv/in-indentation-point-p ()
  "Whether the cursor is in a point apt for triggering an indentation command."

  (or (vemv/at-beginning-of-line-p)
      (every (lambda (x) (= x 32))
             (vemv/chars-at-left))))

(defun vemv/non-completable-char-p ()
  "Whether the cursor is in a point predictably impossible to autocomplete"
  (let ((current-char (vemv/current-char-at-point)))
    (-any?
     (lambda (x)
       (string-equal x current-char))
     (list ";" "(" "[" "{" "#" "\""))))
