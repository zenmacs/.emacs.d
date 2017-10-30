
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(progn "Stuff that needs to be performed immediately, for a visually pleasant startup"

  (setq inhibit-startup-message t)
  (setq-default line-spacing 1) ; NOTE: might mess up the echo area
  ;; Silence minibuffer
  (setq inhibit-message t)
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1)))

(let ((default-directory "~/.emacs.d/lib"))
      (normal-top-level-add-subdirs-to-load-path))

(when (eq system-type 'darwin)
  (setq mac-control-modifier 'super)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'control))

(setq vemv-font (if (eq system-type 'darwin) "Monaco-12" "DejaVu Sans Mono-13"))

(if (window-system) (set-face-attribute 'default nil :font vemv-font))

(setq vemv/using-nrepl t)
(setq vemv-home (getenv "HOME"))

;; - make `setq`s defuns
;; - infer project from currently open file
;; - use inferred value as implicit argument to these defuns
(setq vemv/current-project (if t "gpm" "assign"))

(defun vemv/project-initializers ()
  (or
    (pcase vemv/current-project
      ("gpm" (lambda ()
                    (shell-command-to-string "source ~/.zshrc; cd ~/gpm/src; make clean")
                    (shell-command-to-string "source ~/.zshrc; cd ~/gpm/src; make sass")))
      ("assign" (lambda ())))
    (lambda ())))

;; XXX assert default value exists in filesystem
;; XXX the case statements are hardcoded. paremeterize
(setq vemv/project-root-dir ;; trailing slash required
  (or
    (pcase vemv/current-project
      ("gpm" (concat vemv-home "/gpm/src/")))
    (concat vemv-home "/" vemv/current-project)))

;; should equal to vemv/project-root-dir unless project.clj is non-root
(setq vemv/project-clojure-dir
  (or
    (pcase vemv/current-project
      ("gpm" (concat vemv/project-root-dir "horizon/")))
  vemv/project-root-dir))

(setq vemv/project-fiplr-dir
  (or
    (pcase vemv/current-project
      ("assign" (concat vemv/project-clojure-dir "/src")))
  vemv/clojure-dir))

(setq vemv/project-ns-prefix ;; the bit to remove in tabs (mode-line). also identifies repls (important)
  (or
    (pcase vemv/current-project
      ("gpm" "horizon"))
    vemv/current-project))

(setq vemv/default-clojure-file
  (or
    (pcase vemv/current-project
      ("gpm" (concat vemv/project-clojure-dir "src/horizon/desktop/core.cljs")))
    ;; XXX should be the first existing file: .clj, .cljs or .cljc
    (concat vemv/project-clojure-dir "/src/" vemv/project-ns-prefix "/core.cljs")))

(setq vemv-cleaning-namespaces nil)

(require 'vemv.init)
