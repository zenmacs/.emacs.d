# Zenmacs - an .emacs.d

## Goal

Zenmacs has the goal of providing a lightweight IDE experience (similar to Atom, dissimilar to IntelliJ), with intuitive tools that work perfectly, a holy-grail window layout, clean support for handling/displaying arbitrarily many workspaces/projects/files, and a persistent desire for un-sucking Emacs, disregarding complexity-inducing features and consistently embracing better approaches.

Often I've gone as far as creating custom systems that subvert the Emacs way, and forking/maintaining projects as needed.

The feature set aims to have just the right size: one big enough to provide a satisfactory experience, but small enough to be adoptable (less chances for bugs, lack of documentation or discrepancies in opinions).

Worth noting, this is not "some guy's emacs". You won't find here oddities specific to my work, locale, OS environment, opinions, etc. All such stuff is in a separate repo: `.emacs.d.overrides`, which contents will vary per-user.

Similarly, this isn't some kind of boilerplate project with an opinionated automagical setup for every programming language under the face of Earth. For genericity, only Clojure, Elisp, and [lsp-mode](https://github.com/emacs-lsp/lsp-mode) are core to this project. I set up other major modes like Ruby, JavaScript etc in the mentioned `.emacs.d.overrides` directory/repo: as one gets more specific, there's an increased chance for doing something you disagree with (or don't care about).

Importantly, I've developed a lot of Clojure-specific features, which make Zenmacs unique and powerful.

You are encouraged to customize anything and everything. If Zenmacs makes it difficult for you - it's a bug!

**Key features**:

* Keyboard shortcuts are ergonomic (free of typical emacs-isms), and are defined in a [centralized](https://github.com/zenmacs/.emacs.d/blob/master/lib/non-submodules/vemv.shortcuts.global.el), OS-independent, discoverable, reloadable manner.
* A fixed window layout: there's always a main-window, a repl-window and a project-explorer-window. You cannot break this layout, which gives a Zenmacs a structured yet lightweight IDE feel. You are still free to split windows, or create new frames. **Note**: Emacs has special meanings for the terms _window_ and _frame_.  
* It has rich tabs (representing files, projects, workspaces). This is original work. Buffer (file) navigation is great in general, hiding from you the typical Emacs annoying pseudo-files (buffers).
* Editor, directory tree and repl are always in sync between them as you switch projects/workspaces.
* Key things (shortcuts, editor configuration, project setup, etc) can be overriden by design (`.emacs.d.overrides` repo).
* It removes Emacs from its many quirks, noisy feedback and esoteric ways of approaching mundane tasks. Philosophy is to make Emacs behave like a normal editor, similarly to when you open Atom and expect everything to 'just work'.

**Caveats**:

* The Clojure tooling (MELPA packages) is version-frozen using my own forks. Forking is a good way to ensure stability, since those are moving targets. I update once a year or so.

**"But, monolith"**

This setup is somewhat similar to others in that it's a monolithic conglomerate of functionality - you couldn't cherry-pick just what you wanted.

This has the drawback of being all-or-nothing: either you use all the provided features, or you are left bare-bones and have to build it all yourself (maybe copying some bits).

I am aware of this weakness. Be assured, dear reader, that if this project takes off, I will split this setup into many independent packages. Meanwhile such effort would be premature optimization.

## Getting started

* Note that you need an Unix-y OS.
* Watch the video so you know what to expect.
* Get Emacs (26 or 25). For macOS I recommend `brew install railwaycat/emacsmacport/emacs-mac`.
* Possibly, disable unnecessary OS system shortcuts (example: `Control + left` in macOS) which may take precedence over Emacs.
* Backup and remove `~/.emacs.d` if that directory existed already
* Clone the repo into `~`, so Emacs will pick up `~/.emacs.d` on startup
* cd into it, make sure that each git submodule is pulled, typically with `git submodule update --init --recursive`. `./lib` should have many non-empty directories, and no errors in the fetching process.
* Install the_silver_searcher (for helm-ag), ruby (for fiplr), tree (for project-explorer).
* If you want Ruby code autoformatting to work, make sure the `rubocop` gem is installed in each Ruby project. Also a global install will be useful for spare files. If rubocop is not present, nothing bad will happen.
* Launch Emacs. On the first run, the screen will be frozen for a couple minutes since packages are being installed.
* Take a look at `vemv.shortcuts.global.el`. Learn those shortcuts (which you can override later).
* Open a Leiningen-based Clojure project: `vemv/shortcuts/global/tertiary-o` (typically: `Control-o`).
* Execute the shortcut corresponding to `vemv/shortcuts/global/primary-j` (typically: `C-j`) so CIDER starts and you can get a Clojure(Script) REPL accordingly.
* Play around with some paredit or evaluation shortcuts!

## Customizing keyboard shortcuts, creating project-specific configuration, etc.

* If it didn't exist already, Zenmacs creates `~/.emacs.d.overrides`, which you should keep version-controlled. No code is ever auto-dumped there - it's up to you to add project-specific configuration, if desired.

Its structure is as follows:

```
.
└── lib
    └── emacs.d.overrides.el       # Global user-specific configuration
    └── zenmacs.project.foobar.el  # Configuration specific to the project with id `foobar`
    └── zenmacs.project.bazquux.el # Configuration specific to the project with id `bazquux`
```

In emacs.d.overrides.el you can:

```
(provide 'emacs.d.overrides)

(setq vemv/shortcuts/global/primary-b nil)                ;; nullify an undesired binding
(setq vemv/shortcuts/global/primary-r 'vemv/duplicate)    ;; customize a binding
(setq vemv.project.reasonable-file-count?/threshold 1000) ;; customize a global (non-project-specific) config value
```

And more. You can also define user-specific functions, for arbitrary purposes (creating new functionalities), or also for creating helper functions for DRYing out project configuration (e.g. a set of projects must have the same configuration options).

As for the project-specific configuration files: these are great places for the stuff that varies between projects. Different formatting rules, expected directory structures, and so on.

Project files are created automatically, based on the value of `vemv/available-workspaces`. Creation happens both on Emacs startup, and every time you save `emacs.d.overrides.el`. This reduces the friction of creating config files.

All Elisp code under `emacs.d.overrides/lib` are loaded automatically every time you switch to a different project.
