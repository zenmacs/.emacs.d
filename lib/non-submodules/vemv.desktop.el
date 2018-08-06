;; "https://www.emacswiki.org/emacs?action=browse;oldid=DeskTop;id=Desktop#toc5"
(provide 'vemv.desktop)

(defun vemv.desktop/desktop-file-contents ()
  (read (concat "(list "
                (with-temp-buffer
                  (insert-file-contents (concat vemv-home "/.emacs.d/emacs-desktop"))
                  (buffer-string))
                ")")))

(defun vemv.desktop/ordered-workspaces-list ()
  (ignore-errors
    (eval
     (third
      (car
       (-filter (lambda (x)
                  (and (listp x)
                       (equal `(setq vemv/all-workspaces) (-take 2 x))))
                (vemv.desktop/desktop-file-contents)))))))

(defun vemv.desktop/ordered-file-buffers-list ()
  (ignore-errors
    (-flatten (mapcar 'second (eval
                               (third
                                (-find (lambda (x)
                                         (and (listp x)
                                              (equal `(setq vemv/chosen-file-buffer-order-as-list) (-take 2 x))))
                                       (vemv.desktop/desktop-file-contents))))))))

(defun vemv/files-from-previous-session ()
  (ignore-errors
    (mapcar (lambda (x)
              (list (nth 2 x) (nth 6 x)))
            (-filter (lambda (x)
                       (and (listp x)
                            (eq 'desktop-create-buffer (car x))
                            (not (vemv/contains? (third x) ".gz"))))
                     (vemv.desktop/desktop-file-contents)))))

(setq desktop-path '("~/.emacs.d/"))

(setq desktop-dirname "~/.emacs.d/")

(setq desktop-base-file-name "emacs-desktop")

(setq desktop-globals-to-save `(vemv/all-workspaces vemv/chosen-file-buffer-order-as-list))

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
                    (goto-char n)
                    (vemv/clean-chosen-file-buffer-order))))))
          (vemv/sort-car-by-car (vemv/files-from-previous-session)
                                (mapcar 'list (vemv.desktop/ordered-file-buffers-list)))))
