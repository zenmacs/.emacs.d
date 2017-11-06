# .emacs.d

vemv's emacs abstraction thingy.

One could say it follows a key principle explained in _Design, Composition and Performance_: **limitations = creativity**. At the cost of introducing a few
limitations, I was able to implement a simple yet powerful workflow, including a variety of mechanisms that you'll rarely find elsewhere (such as tabs).

**Pros**:

* Keyboard shortcuts are ergonomic (free of typical emacs-isms), and are defined in a [centralized](https://github.com/vemv/.emacs.d/blob/master/lib/non-submodules/vemv.shortcuts.global.el), OS-independent, discoverable, reloadable manner.
* It has tabs (representing each Clojure file). Buffer (file) navigation is great in general, hiding from you the typical Emacs annoying pseudo-files (buffers).
* Editor, directory tree and repl are always in sync between them.
* It is optimized for Clojure(Script), instead of intending to solve infinite problems
* It's not overarchitected, prefering library reuse over greenfield solutions.
* Key things (shortcuts, default projects, etc) can be overriden by design, in a separate repo that you can cleanly keep version-controlled.
* It removes Emacs from its many quirks, noisy feedback and esoteric ways of approaching mundane tasks. Philosophy is to make Emacs behave like a normal editor, similarly to when you open Atom and expect everything to 'just work'.
* You can switch between Clojure(Script) projects in the same Emacs session, and the key tools (editor, directory tree, repl) will react accordingly.
* Settings/shortcuts get auto-reloaded as you edit them (even from an external editor)

**Cons**:

* Any language other than Clojure(Script) is not intendedly supported. I use Atom/vim/IDEA for non-Clojure stuff (aka right tool for the job).
* There's the key assumption that one works with one project, one window, one directory tree, one editor and one repl at a time. I don't want to maintain an intrincate graphical/project system, tracking and syncing stuff. These limitations turn out to be liberating (and one still can switch between projects in a same Emacs session - as long as the same window is used)
* `vemv/` is the fragment used to prevent clashes with other functions/vars. Sorry for all the vemvs!

## Getting started

* Watch the video so you know what to expect.
* Get Emacs 25.
* Backup and remove `~/.emacs.d` if that directory existed already
* Clone the repo into `~`, so Emacs will pick up `~/.emacs.d` on startup
* cd into it, make sure that each git submodule is pulled. `./lib` should have many non-empty directories.
* Take a look at `vemv.project.el`, override it with at least one Clojure project, and get familiar with the concept of `vemv/modifiers/primary` (and secondary, tertiary)
* Take a look at `vemv.shortcuts.global.el` and `vemv.shortcuts.clojure.el`. Learn those shortcuts.
* Execute the shortcut corresponding to `vemv/shortcuts/global/primary-j` (typically: `C-j`) so CIDER starts and you can get a Clojure(Script) REPL accordingly.
* Play around with some paredit or evaluation shortcuts!

## Customizing keyboard shortcuts (et al)

* Create a `~/.emacs.d.overrides` directory with the following structure:

```
.
└── lib
    └── emacs.d.overrides.el
```

Now in the .el file you can:

```
(provide 'emacs.d.overrides)

(setq vemv/shortcuts/global/primary-b nil) ;; nullify an undesired binding
(setq vemv/shortcuts/global/primary-r 'vemv/duplicate) ;; customize a binding
```

Just by creating said directory/structure, .emacs.d will detect it, load it at startup, and reload it as you change it.