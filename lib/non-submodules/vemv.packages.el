;;; -*- lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(setq vemv/packages-refreshed nil)

(dolist (package '(cider company queue fiplr clojure-mode clj-refactor smartparens
                         dash simpleclip helm-ag git-timemachine paren-face haml-mode))
  (unless (package-installed-p package)
    (unless vemv/packages-refreshed
      (package-refresh-contents)
      (setq vemv/packages-refreshed t))
    (package-install package)))

;; Eases editing locally-modified packages.
;; Example:
;; Fork a package
;; `ln -s` the relevant file to ~/.emacs.d/elpa/the-package
;; rm ~/.emacs.d/elpa/the-package/foo.elc
;; restart emacs.
(add-hook 'compilation-finish-functions (lambda (b _) (kill-buffer b)))
(byte-recompile-directory (expand-file-name "~/.emacs.d/elpa") 0)

(require 'saveplace)
(require 'dash)
(require 'popup)
(require 'smex)
(require 'cider)
(require 'epl)
(require 'pkg-info)
(require 'spinner)
(require 'ruby-mode)
(require 'comint)
(require 'es-lib)
(require 'es-windows)
(require 'project-explorer)
(require 'paredit)
(require 'undo-tree)
(require 's)
(require 'clj-refactor)
(require 'fiplr)
(require 'helm-ag)
(require 'desktop)
(provide 'vemv.packages)
