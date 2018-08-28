;;; -*- lexical-binding: t -*-

(require 'package)

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(add-to-list 'same-window-buffer-names "*Compile-Log*")

(setq byte-compile-verbose nil)
(setq byte-compile-warnings nil)
(setq byte-compile-log-warning-function (argless))

(setq vemv/packages-refreshed nil)

(unless vemv/terminal-emacs?
  (dolist (package '(benchmark-init
                     company
                     dash
                     edn
                     exec-path-from-shell
                     fiplr
                     git-timemachine
                     haml-mode
                     helm-ag
                     highlight-indent-guides
                     hydra
                     inflections
                     multiple-cursors
                     paren-face
                     queue
                     robe
                     ruby-end
                     simpleclip
                     smartparens
                     yasnippet))
    (unless (package-installed-p package)
      (vemv/verbosely
       (unless vemv/packages-refreshed
         (package-refresh-contents)
         (setq vemv/packages-refreshed t))
       (condition-case nil
           (package-install package)
         (error
          (package-refresh-contents)
          (condition-case nil
              (package-install package)
            (error
             (package-refresh-contents)
             (package-install package)))))))))

;; M-x benchmark-init/show-durations-tabulated / M-x benchmark-init/show-durations-tree
(require 'benchmark-init)
(add-hook 'after-init-hook 'benchmark-init/deactivate)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(unless vemv/terminal-emacs?
  (require 'saveplace))
(require 'dash)
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
  (require 'smartparens-config)
  (require 'yasnippet))

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
             '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

;; Eases editing locally-modified packages.
;; Also makes things faster.
;; Example:
;; Fork a package, place the git repo outside ~/.emacs.d
;; `ln -s` the relevant file to ~/.emacs.d/elpa/the-package/foo.el
;; rm ~/.emacs.d/elpa/the-package/foo.elc
;; restart emacs.

(unless vemv/terminal-emacs?
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
