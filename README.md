#### On scrolling

Scrolling might be broken in fresh installations. I don't know what fixed it, but this seemed to work:

* Install https://github.com/overtone/emacs-live on .emacs.d
* run it once, opening a large file, scrolling through it
* `mv .emacs.d .emacs.d.other`
* clone this repo, pull submodules, run normally.