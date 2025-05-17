# Changelog

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
