;;; -*- lexical-binding: t -*-

(require 'package)

;; Eases going back to Emacs 26
(setq package-check-signature nil)

;; Not necessary - melpa has everything elpa has. Elpa has downtime more frequently.
;; (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-refresh-contents)

(when vemv/terminal-emacs?
  (setq package-load-list '(all
                            (unwanted-package magit))))

(package-initialize)

(add-to-list 'same-window-buffer-names "*Compile-Log*")

(setq byte-compile-verbose nil)
(setq byte-compile-warnings nil)
(setq byte-compile-log-warning-function (argless))

(setq vemv/packages-refreshed nil)

(unless vemv/terminal-emacs?
  (dolist (package '(async
                     benchmark-init
                     buttercup
                     dash
                     dockerfile-mode
                     ert
                     exec-path-from-shell
                     fiplr
                     git-timemachine
                     haml-mode
                     highlight-indent-guides
                     hydra
                     ido-at-point
                     ido-completing-read+
                     inf-ruby
                     inflections
                     logview
                     magit
                     multiple-cursors
                     paren-face
                     parseedn
                     ;; pdf-tools
                     queue
                     ;; robe
                     rspec-mode
                     rubocop
                     ruby-end
                     simpleclip
                     smartparens
                     swift-mode
                     string-inflection
                     tide
                     typescript-mode
                     yasnippet))
    (let ((refreshed nil))
      (unless (package-installed-p package)
        (vemv/verbosely
         (unless vemv/packages-refreshed
           (unless refreshed
             (package-refresh-contents)
             (setq refreshed t))
           (setq vemv/packages-refreshed t))
         (condition-case nil
             (package-install package)
           (error
            (unless refreshed
              (package-refresh-contents)
              (setq refreshed t))
            (condition-case nil
                (package-install package)
              (error
               (unless refreshed
                 (package-refresh-contents)
                 (setq refreshed t))
               (package-install package))))))))))

(require 'dash)
(defun vemv/maybe-omit-message (f m &rest args)
  ;; Important to take extra caution here - any error will leave Emacs unable to receive keyboard input
  (if (or (not m)
          (-find (lambda (x)
                   (ignore-errors
                     (string-match x m)))
                 `("^Checking .*\\.\\.\\."
                   "^Loading .*\\.\\.\\."
                   "You appear to be setting environment variables"
                   "Hiding all blocks"
                   "Cleaning up the recentf"
                   "loading of snippets successfully"
                   "Saving file"
                   "Truncate long lines"
                   "Mark activated"
                   "uncompressing"
                   "^Wrote"
                   "Done (Total of"
                   "Mark set"
                   "Mark cleared"
                   "Auto-saving"
                   "Undo branch point"
                   "Indenting region")))
      nil
    (apply f m args)))

;; some output comes from `make-progress-reporter', which is harder to disable
(advice-add 'message ':around 'vemv/maybe-omit-message)

;; M-x benchmark-init/show-durations-tabulated / M-x benchmark-init/show-durations-tree
(require 'benchmark-init)
(add-hook 'after-init-hook 'benchmark-init/deactivate)

(when (member window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(unless vemv/terminal-emacs?
  (require 'saveplace))
(require 'popup)
(require 'smex)
(unless vemv/terminal-emacs?
  (require 'company))
(require 'epl)
(unless vemv/terminal-emacs?
  (require 'spinner)
  (require 'comint))
(require 'paredit)
(require 's)
(require 'grizzl)
(require 'fiplr)
(unless vemv/terminal-emacs?
  (require 'desktop)
  ;; (require 'smartparens-config)
  (require 'yasnippet)
  (custom-set-faces
   '(yas-field-highlight-face ((t (:inherit nil))))))

(autoload 'ido-at-point-mode "ido-at-point")

(when vemv/terminal-emacs?
  (autoload 'clojure-mode "clojure-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljc$" . clojure-mode)))

(autoload 'haml-mode "haml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

(autoload 'js "js-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.js.erb$" . js-mode))

(add-to-list 'auto-mode-alist
             '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))

(add-to-list 'auto-mode-alist
             '("\\(?:Brewfile\\|Capfile\\|Gemfile$\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))


(add-to-list 'auto-mode-alist '("\\.org$" . fundamental-mode))

(add-to-list 'auto-mode-alist '("\\.md$" . fundamental-mode))

(add-to-list 'auto-mode-alist '("\\.fern$" . clojure-mode))

(add-to-list 'auto-mode-alist '("bootstrap.cfg" . clojure-mode))

;; Eases editing locally-modified packages.
;; Also makes things faster.
;; Example:
;; Fork a package, place the git repo outside ~/.emacs.d
;; `ln -s` the relevant file to ~/.emacs.d/elpa/the-package/foo.el
;; rm ~/.emacs.d/elpa/the-package/foo.elc
;; restart emacs.

(unless vemv/terminal-emacs?
  (byte-recompile-directory vemv/overrides-forks-directory)
  (let* ((lib-dir (expand-file-name "~/.emacs.d/lib/"))
         (dirs (->> lib-dir
                    directory-files
                    (-remove (lambda (x)
                               (member x `("." ".." "non-submodules"))))
                    (mapcar (lambda (x)
                              (concat lib-dir x)))
                    (cons (expand-file-name "~/.emacs.d/elpa")))))
    (dolist (dir dirs)
      (byte-recompile-directory dir 0))))

(provide 'vemv.packages)
