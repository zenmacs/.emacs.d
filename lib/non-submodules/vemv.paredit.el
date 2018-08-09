;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(provide 'vemv.paredit)

(setq vemv/kill-list-bound 10)

;; The 10 last elements copied to the clipboard.
;; I don't use kill-ring, since third-parties (e.g. paredit) can mess with it
(setq vemv/kill-list (-repeat vemv/kill-list-bound nil))

(defun vemv/save (&optional b)
  (interactive)
  (let* ((line (vemv/current-line-number))
         ;; for `indent-for-tab-command`:
         (last-command nil)
         (b (or b (current-buffer)))
         (dc (string-equal "dc" (car vemv/current-workspace)))
         ;; for `save-buffer`:
         (require-final-newline (and (not vemv/no-newline-at-eof)
                                     (not dc))))
    (with-current-buffer b
      (unless dc
        (delete-trailing-whitespace))
      (call-interactively 'mark-whole-buffer)
      (call-interactively 'indent-for-tab-command)
      (goto-line line)
      (vemv/end-of-line-code* nil)
      (when vemv/no-newline-at-eof
        (save-excursion
          (end-of-buffer)
          (while (string-equal (vemv/current-char-at-point) "\n")
            (delete-backward-char 1))))
      (save-buffer))))

(defun vemv/tab ()
  (interactive)
  (or (and (or
            (vemv/in-indentation-point-p)
            (vemv/non-completable-char-p))
           (or (let ((last-command nil))
                 (call-interactively 'indent-for-tab-command))
               t))
      (call-interactively 'company-complete)
      (call-interactively 'company-dabbrev)))

(defun vemv/dumb-cut ()
  "Cuts the current selection, regardless of paredit boundaries"
  (when (region-active-p)
    (let ((content (vemv/selected-region)))
      (call-interactively 'kill-region)
      (vemv/bounded-list/insert-at-head! content
                                         vemv/kill-list
                                         vemv/kill-list-bound)
      (simpleclip-set-contents content))))

(defun vemv/cut ()
  "Cuts via vemv/kill, i.e. taking into account paredit boundaries"
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

(defun vemv/pull-next-sexpr ()
  "Brings the sexpr located in the next line at the current one."
  (interactive)
  (just-one-space -1)
  (backward-up-list)
  (vemv/indent)
  (beginning-of-line-text))

(defmacro vemv/paredit-safely (&rest body)
  "* Paredit commands over non-lisps can cause Emacs freezes.
   * comment-indent-function has a strange behavior, it's hard to dynamically set.
   * Indentation can go wild due to `last-command'"
  `(when (vemv/in-a-lisp-mode?)
     (let* ((comment-indent-function vemv/comment-indent-function)
            (last-command nil)
            (v (progn
                 ,@body)))
       v)))

(defun vemv/safe-paredit-command (command)
  (argless
   (vemv/paredit-safely
    (call-interactively command))))

(defun vemv/sexpr-content (&optional backward?)
  "Returns the content of the next (or previous, on non-nil values of BACKWARD?) sexpr, as a string.

Unlike paredit-copy-as-kill, this function will only grab one sexpr (and no more even -
if they are contigous), and is side-effect free."
  (interactive)
  (save-excursion
    (push-mark)
    (if backward? (paredit-backward) (paredit-forward))

    (let ((result (vemv/selected-region)))
      (pop-mark)
      (if backward? (paredit-forward) (paredit-backward))
      result)))

(defun vemv/duplicate (&optional backward?)
  "Copies the current line (or sexpr, if point is at the beggining of one, or selection, if the region is active),
inserting it at a new line."
  (interactive)

  (if (region-active-p)

      (let ((content (vemv/selected-region)))
        (dotimes (i (- (region-end) (point)))
          (forward-char))
        (insert content))

      (back-to-indentation)

      (if (some (lambda (char)
                  (equal char (vemv/current-char-at-point)))
                '("(" "[" "{" "<" "\""))
          (progn
            (let ((content (vemv/sexpr-content))
                  (at-b (vemv/at-beginning-of-line-p)))
              (paredit-forward)
              (insert (concat (if at-b "\n\n" "\n") content))
              (paredit-backward)
              (beginning-of-line)
              (indent-for-tab-command)))

          (progn
            (move-beginning-of-line 1)
            (kill-line)
            (yank)
            (open-line 1)
            (next-line 1)
            (yank)
            (pop kill-ring)))))

(defun vemv/ensure-no-double-blank-newlines ()
  (while (and (string-equal "\n" (vemv/current-line-contents))
              (string-equal "\n" (vemv/current-char-at-point -1))
              (string-equal "\n" (vemv/current-char-at-point -2)))
    (delete-backward-char 1))
  (while (and (string-equal "\n" (vemv/current-line-contents))
              (string-equal "\n" (vemv/current-char-at-point 1)))
    (delete-forward-char 1)))

(defun vemv/kill (&optional backward? skip-save-to-clipboard? skip-kill-whitespace?)
  "Deletes the next sexpr (or previous, if BACKWARD?).

   Unlike paredit-kill, this function will only grab one sexpr (and no more, if they are contigous),
   and it doesn't alter the kill-ring.
  (interactive)"
  (unless skip-kill-whitespace?
    (while (and (or (equal " " (vemv/current-char-at-point))
                    (equal "\n" (vemv/current-char-at-point))
                    (if backward?
                        (or (equal " " (vemv/current-char-at-point -1))
                            (equal "\n" (vemv/current-char-at-point -1)))
                        nil))
                (if backward?
                    (or (equal " " (vemv/current-char-at-point -1))
                        (equal "\n" (vemv/current-char-at-point -1)))
                    t))
      (if backward?
          (delete-backward-char 1)
          (delete-forward-char 1))))
  (when (eq (point)
            (save-excursion
              (if backward?
                  (progn
                    (paredit-backward)
                    (paredit-forward))
                  (progn
                    (paredit-forward)
                    (paredit-backward)))
              (point)))
    (ignore-errors
      (push-mark)
      (if backward? (paredit-backward) (paredit-forward))
      (let ((result (vemv/selected-region)))
        (delete-region (mark) (point))
        (unless skip-kill-whitespace?
          (while (and
                  (equal " " (vemv/current-char-at-point))
                  (not (equal "\n" (vemv/current-char-at-point))))
            (paredit-forward-delete)))
        (when (not skip-save-to-clipboard?)
          (simpleclip-set-contents result))
        (unless skip-kill-whitespace?
          (ignore-errors
            (vemv/ensure-no-double-blank-newlines)))
        result))))

(defun vemv/delete-backward (&optional cut?)
  "Performs a paredit-backward-delete unless the region is active, in which case the selection gets unconditionally removed.

   The removed value will be pushed to the kill-ring only on non-nil values of CUT?.

   Unconditionally removing code may yield semantically wrong results, i.e. leaving sexprs unmatched.
   I personally like this tradeoff - use with caution!"
  (interactive)

  (funcall (if cut?
               'vemv/bounded-list/insert-at-head!
               'vemv/bounded-list/insert-at-second-position!)
           (if (region-active-p)
               (progn (call-interactively 'kill-region)
                      (if (not cut?) (pop kill-ring)))

               (paredit-backward-delete))
           vemv/kill-list
           vemv/kill-list-bound))

(defun vemv/backward-up-list ()
  "Like backward-up-list, but doesn't complain if invoked at ( itself"
  (while (not (some (lambda (char)
                      (equal char (vemv/current-char-at-point)))
                    '("(" "[" "{")))
    (beginning-of-sexp)))

(defun vemv/dumb-indent ()
  (interactive)
  (vemv/paredit-safely
   (vemv/backward-up-list)
   (paredit-wrap-sexp)
   (paredit-splice-sexp-killing-backward))
  (when (eq vemv/comment-indent-function 'vemv/normal-indentation)
    (let* ((initial-line (vemv/current-line-number))
           (final-line (save-excursion
                         (vemv/paredit-safely
                          (paredit-forward)
                          (vemv/current-line-number)))))
      (goto-line initial-line)
      (while (not (eq (vemv/current-line-number)
                      final-line))
        (progn
          (beginning-of-line)
          (call-interactively 'indent-for-tab-command)
          (next-line)))
      (goto-line initial-line)
      (beginning-of-line-text))))

(defun vemv/indent ()
  "Indents the next sexpr."
  (interactive)
  (vemv/dumb-indent))

(defun vemv/home ()
  "Moves the point to leftmost non-empty character in the current line."
  (interactive)
  (move-beginning-of-line 1)
  (if (not (equal last-command 'vemv/home))
      (while (some (lambda (char)
                     (equal char (vemv/current-char-at-point)))
                   '(" " "\t"))
        (forward-char))))

(defun vemv/end () ;; XXX doesn't honor region
  "Moves the point to rightmost non-empty character in the current line.

   Comments get ignored, this is, point will only move as long as its position still belongs to the code -
   unless this command has been fired for the second time."
  (interactive)
  (if (equal last-command 'vemv/end)
      (call-interactively 'move-end-of-line)
      (let* ((line (vemv/current-line-contents))
             (rev (vemv/reverse line))
             (line_length (length line))
             (movement (recur-let ((result 0))
                                  (if (some (lambda (char)
                                              (equal char (substring line result (inc result))))
                                            '(";" "\n"))
                                      result
                                      (recur (inc result))))))
        (move-beginning-of-line 1)
        (forward-char movement)
        ;; there may exist empty space between code and comment:
        (if (pos? movement)
            (while (not (some (lambda (char)
                                (equal char (vemv/current-char-at-point)))
                              '(" ")))
              (backward-char)))
        (comm backward-char (recur-let ((result 0))
                                       (if (or
                                            (equal result line_length)
                                            (equal " " (substring rev result (inc result))))
                                           result
                                           (recur (inc result))))))))

(defun vemv/copy-selection-or-next-sexpr ()
  (let ((content (if (region-active-p)
                     (vemv/selected-region)
                     (vemv/sexpr-content))))
    (when (region-active-p)
      (call-interactively 'cua-set-mark))
    (vemv/bounded-list/insert-at-head! content vemv/kill-list vemv/kill-list-bound)
    (simpleclip-set-contents content)))

(defun vemv/copy-sexpr-content-backward ()
  (interactive)
  (kill-new (vemv/sexpr-content :backward)))

(defun vemv/kill-backward ()
  (interactive)
  (vemv/kill :backward))

(defun vemv/kill-backward-copying-content ()
  (interactive)
  (kill-new (vemv/kill :backward)))

(defun vemv/onelineize ()
  "Turns the current sexpr into a oneliner"
  (interactive)
  (let ((replacement (replace-regexp-in-string "[\s|\n]+" " " (vemv/sexpr-content))))
    (vemv/kill nil t t)
    (insert (concat replacement " "))
    (when (string-equal " " (vemv/char-at-left))
      (paredit-backward-delete))
    (call-interactively 'paredit-backward)))

(defun vemv/normal-indentation ()
  "https://stackoverflow.com/a/14196835/569050"
  (let ((c (current-buffer))
        (m major-mode)
        (p (point)))
    (if (and (looking-at "\\s<\\s<\\(\\s<\\)?")
             (or (match-end 1) (/= (current-column) (current-indentation))))
        0
        (let ((curr (with-temp-buffer
                      (insert-buffer-substring c)
                      (when (vemv/in-a-clojure-mode? m)
                        (clojure-mode))
                      (goto-char p)
                      (end-of-line)
                      (insert "\n")
                      (call-interactively 'indent-for-tab-command)
                      (current-indentation))))
          (when (or (/= (current-column) curr)
                    (and (> comment-add 0) (looking-at "\\s<\\(\\S<\\|\\'\\)")))
            curr)))))

(defun vemv/semicolon ()
  (interactive)
  (vemv/paredit-safely
   (if (or (equal (vemv/current-char-at-point) ";")
           (paredit-in-string-p)
           (paredit-in-comment-p))
       (insert ";")
       (let* ((what (if (eq vemv/comment-indent-function 'vemv/normal-indentation)
                        ";"
                        ";;"))
              (_ (when (s-blank-str? (vemv/current-line-contents))
                   (call-interactively 'indent-for-tab-command)))
              (whitespace-chars (string-to-list " \n"))
              (maybe-newline (when (-any? (lambda (x)
                                            (not (member x whitespace-chars)))
                                          (string-to-list (vemv/chars-at-right)))
                               "\n"))
              (maybe-prefix (if (or (vemv/at-beginning-of-line-p)
                                    (string-equal " " (vemv/char-at-left)))
                                ""
                                " ")))
         (insert (concat maybe-prefix what " " maybe-newline))
         (when maybe-newline
           (beginning-of-line)
           (call-interactively 'indent-for-tab-command)
           (previous-line)
           (beginning-of-line)
           (call-interactively 'indent-for-tab-command)
           (end-of-line))))))

(setq vemv/thread-message
      "Press 1 to thread-first,\n      2 to fully thread-first,\n      3 to thread-last, or\n      4 to fully thread-last:\n\n")

(defun vemv/thread ()
  (interactive)
  (let* ((choices (string-to-list "1234"))
         (choice (read-char-choice vemv/thread-message choices)))
    (case choice
      (?1 (clojure-thread-first-all t))
      (?2 (clojure-thread-first-all nil))
      (?3 (clojure-thread-last-all t))
      (?4 (clojure-thread-last-all nil)))
    (unless (eq (read-char-choice "Press any of (1, 2, 3, 4) to `vemv/onelineize` the result, or RET for leaving it as-is:"
                                  (cons 13 choices))
                13)
      (vemv/onelineize))))
