;;; -*- lexical-binding: t -*-

(require 'package)

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(setq vemv/packages-refreshed nil)

(dolist (package '(edn inflections hydra company queue fiplr smartparens yasnippet multiple-cursors
                   dash simpleclip helm-ag git-timemachine paren-face haml-mode ruby-end))
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
(require 'company)
(require 'cider)
(require 'epl)
(require 'pkg-info)
(require 'spinner)
(require 'ruby-mode)
(require 'haml-mode)
(require 'js)
(require 'comint)
(require 'es-lib)
(require 'es-windows)
(require 'project-explorer)
(require 'paredit)
(require 'undo-tree)
(require 's)
(require 'cider)
(require 'clj-refactor)
(require 'fiplr)
(require 'helm-ag)
(require 'desktop)
(provide 'vemv.packages)
