(require 'vemv.lang)
(require 'vemv.shortcuts.ruby.base)
(provide 'vemv.shortcuts.ruby)

(defun vemv/minitest-project-p ()
  "Return t if current project uses minitest (has test/ directory)."
  (file-exists-p (expand-file-name "test/" vemv/project-root-dir)))

(setq vemv/shortcuts/ruby/secondary-up               (argless
                                                      (call-interactively 'sp-splice-sexp-killing-backward))
      vemv/shortcuts/ruby/primary-backspace          'vemv/delete-this-line
      vemv/shortcuts/ruby/primary-r                  (argless
                                                      (vemv/save)
                                                      (if (vemv/minitest-project-p)
                                                          (minitest-verify)
                                                        (rspec-verify)))
      vemv/shortcuts/ruby/primary-S-r                (argless
                                                      (vemv/save-all-buffers-for-this-project)
                                                      (if (vemv/minitest-project-p)
                                                          (minitest-verify-all)
                                                        (rspec-verify-all)))
      vemv/shortcuts/ruby/tertiary-r                 (argless
                                                      (vemv/save)
                                                      (if (vemv/minitest-project-p)
                                                          (minitest-verify-single)
                                                        (rspec-verify-single)))
      vemv/shortcuts/ruby/secondary-k                (argless
                                                      (call-interactively 'sp-backward-kill-sexp))
      vemv/shortcuts/ruby/secondary-backspace        (argless
                                                      (call-interactively 'sp-backward-kill-word))
      vemv/shortcuts/ruby/secondary-left-bracket     (argless
                                                      (call-interactively (if (fboundp 'sp-backward-sexp)
                                                                              'sp-backward-sexp
                                                                            'backward-sexp)))
      vemv/shortcuts/ruby/secondary-left-parens      (argless
                                                      (call-interactively 'sp-wrap-round))
      vemv/shortcuts/ruby/secondary-right-bracket    (argless
                                                      (call-interactively (if (fboundp 'sp-forward-sexp)
                                                                              'sp-forward-sexp
                                                                            'forward-sexp))))
