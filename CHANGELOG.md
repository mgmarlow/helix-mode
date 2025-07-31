# Changelog

## 0.8.0 (2025-07-31)

Now available on MELPA! Helix Mode is now split into several smaller
packages, behaves more appropriately towards advice, and bytecompiles
without warnings.

New features:

- Character navigation (`t`, `T`, `f`, `F`).
- Repeat last motion (applying to character navigation).
- Move word/WORD end (`e`, `E`).
- Undo-redo (`U`).
- Change thing at point (`c`).
- Mark whole buffer (`%`).
- LSP rename (using Eglot, `SPC r`).
- LSP show buffer diagnostics (using Flymake, `SPC d`).

Contributor kudos:

- @SoraTenshi for adding a bunch of functionality.
- @roy-corentin for helping address bugs in navigation.

## 0.7.0 (2025-06-19)

Navigation overhaul! Navigating via word/long-word (w/b/W/B) now
behaves more similarly to Helix.

We also now have a
[roadmap](https://github.com/mgmarlow/helix-mode?tab=readme-ov-file#roadmap)
outlining the focus of Helix Mode development going forward.

New features:

- Long word navigation ("W" and "B").
- Overhaul of normal word navigation ("w" and "b").

Contributor kudos:

- @roy-corentin for implementing long-word navigation and adding a
  test suite!

## 0.6.1 (2025-06-07)

New features:

- Extension functions for adding keys or typable commands. See
  `helix-define-key` and `helix-define-typable-command` for more info.
- Simple indentation keybindings on `<` and `>` that indent a single
  space.

Fix:

- When yanking or killing text, clear Helix selection. This resolves
  issues where killing/yanking text would prevent visual mode from
  creating a new mark.

## 0.6.0 (2025-06-01)

New features:

- Experimental multiple-cursors support. Requires the
  [multiple-cursors](https://github.com/magnars/multiple-cursors.el)
  package.
- New Goto command: `g-s` go to first non-whitespace.

Contributor kudos:

- @dpassen for implementing goto-non-whitespace.

## 0.5.1 (2025-05-23)

New features:

- Add `helix-jj-setup` as an easier means to configure jj as an exit
  combination.

Fix:

- When the jj exit combination is active, any non-j keypress following
  an initial j keypress will immediately abort the timeout. This makes
  the experience of using jj way smoother when typing words that
  include j (like "project").

  This fix requires using the new jj setup function `helix-jj-setup`,
  since that function registers a `pre-command-hook` on the user's
  behalf.

  In a future minor release, `helix-jj-timeout` might be renamed to
  `helix--jj-timeout` so folks know to use the new setup function.

## 0.5.0 (2025-05-17)

Note: Minimum Emacs version increased to 29.1.

New features:

- New Goto command: `g-r` find references.
- New Goto command: `g-d` find definitions.
- New colon command: `:reload-all`, which reloads all buffers.
- New autoloaded function: `helix-mode-all`, which activates Helix
  Mode in all buffers.
- Add "jj" support for exiting Insert Mode.

You can activate "jj" support by defining the following custom
variable:

```lisp
(setq helix-jj-timeout 0.2)
```

Fix:

- `o`/`O` now properly resume indentation.
- `x` expands current selection if the current selection doesn't
  contain the entire line (e.g. if I press `x` after a search).
- `:q`/`:wq` now close the current window if a split is active, rather
  than closing Emacs.

## 0.4.0 (2025-03-17)

Note: Minimum Emacs version increased to 27.1.

New features:

- Add a bunch more colon commands (`:new`, `:open`, `:config-open`, etc.).
- Add Space mode (accessed by hitting `space`). Use this mode to jump
  around via `project.el`.
- `G` lets you jump to a particular line.
- `C-b` and `C-f` scroll the buffer up and down respectively.
- `C-c` comments the current line or region.

Fix:

- Ensure `C-g` clears out intermediate data in the same way as `ESC`.
- Backspace no longer deletes characters in normal mode.

## 0.3.2 (2025-03-15)

Fix:

- Vastly improve word selection during
  navigation. `helix-forward-word` and `helix-backward-word` will now
  reliably select the word navigated as a "thing at point", ignoring
  special characters. In short, the behavior is improved for
  programming language modes.

## 0.3.1 (2025-03-15)

Fix:

- Add `helix-normal-mode` to autoloads.

## 0.3.0 (2025-03-15)

New features:

- Replacements, accessed via `r` or `R`.
- More insert-mode state switchers (`a`, `A`, `I`).
- Colon commands (currently only `:w` and `:write` supported).

## 0.2.0 (2025-03-10)

Added a custom searching mode that replaces the previous usage of
`isearch`. Use `/` to initiate the search and `n`/`N` to navigate
matches.

Added `O` (newline insert above) and fixed a couple bugs.

## 0.1.0 (2025-03-09)

Initial release! Only supporting a minimal subset of my most-used
Helix keybindings. Mostly the basics.
