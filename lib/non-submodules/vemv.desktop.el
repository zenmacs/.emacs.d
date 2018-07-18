;; "https://www.emacswiki.org/emacs?action=browse;oldid=DeskTop;id=Desktop#toc5"
(provide 'vemv.desktop)

(defun vemv/files-from-previous-session ()
  (ignore-errors
    (mapcar (lambda (x)
              (list (nth 2 x) (nth 6 x)))
            (filter (lambda (x)
                      (and (listp x)
                           (eq 'desktop-create-buffer (car x))
                           (not (vemv/contains? (third x) ".gz"))))
                    (read (concat "(list "
                                  (with-temp-buffer
                                            (insert-file-contents (desktop-full-file-name))
                                            (buffer-string))
                                  ")"))))))

(setq desktop-path '("~/.emacs.d/"))

(setq desktop-dirname "~/.emacs.d/")

(setq desktop-base-file-name "emacs-desktop")

(setq desktop-globals-to-save nil)

(add-hook 'desktop-after-read-hook
          '(lambda ()
             (setq desktop-dirname-tmp desktop-dirname)
             (desktop-remove)
             (setq desktop-dirname desktop-dirname-tmp)))

(defun vemv/desktop-save ()
  (interactive)
  (replying-yes
   (desktop-save desktop-dirname)))

(add-hook 'auto-save-hook 'vemv/desktop-save)

(add-hook 'kill-emacs-hook 'vemv/desktop-save)

(defun vemv/open-files-from-last-session! ()
  "Open every file that was open the last time Emacs was closed."
  (mapcar (lambda (e)
            (let ((x (first e))
                  (n (second e)))
              (ignore-errors
                (when (file-exists-p x)
                  (find-file-noselect x)
                  (with-current-buffer (get-file-buffer x)
                    (goto-char n))))))
          (vemv/files-from-previous-session)))
