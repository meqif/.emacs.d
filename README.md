# My emacs settings

A hodge-podge of stuff, much of it copied as needed from [magnar's setup](https://github.com/magnars/.emacs.d).

As a former VIM user, I'm impressed by Emacs's flexibility. Evil-mode makes me feel mostly at home, so the adaptation required was smaller than I expected.

Enjoy and copy freely to your configuration files!

## Getting started on a fresh machine

```bash
git clone $REPO_URL
cd ~/.emacs.d
# Ensure borg is present
git submodule update --init --remote --rebase lib/borg
make bootstrap
```

You may need to manually update some packages with `git submodule update --remote --rebase lib/$package` in case you get a fatal error about invalid objects.
Some people rewrite the git history and break the process.

## Misc

[bbatsov's style guide](https://github.com/bbatsov/emacs-lisp-style-guide)
