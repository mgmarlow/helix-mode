# Helix Mode

[![MELPA](https://melpa.org/packages/helix-badge.svg)](https://melpa.org/#/helix)

Postmodern keybindings for a premodern
editor: [Helix](https://helix-editor.com/) keybindings in Emacs.

Helix is a modal text editor with keybindings similar to vi, but with
some [noteworthy
differences](https://docs.helix-editor.com/from-vim.html). Helix Mode
supports a small subset of Helix functionality with the goal of
recreating the editor navigation/selection experience in Helix while
leaving the hard problems (like directory navigation and searching) to
Emacs.  The result is a small keybinding layer that supports Vim-like
hjkl motion commands, while retaining Helix's selection-first model.

Helix Mode integrates nicely with `project.el`, `eglot`, and `xref`.

## Installation

Emacs >= 29.1 required.

Helix Mode is available on [MELPA](https://melpa.org/#/getting-started):

```lisp
(use-package helix
  :ensure t
  :config
  (helix-mode))
```

Or you can clone this repository and modify your load path:

```lisp
(add-to-list 'load-path "/path/to/helix-mode")
(require 'helix)
(helix-mode)
```

### Global vs. local mode

You can enable Helix mode globally (e.g. for all buffers besides
minibuffers) via

```lisp
(helix-mode)
```

Alternatively, you can manually toggle Helix mode on/off in local
buffers by invoking `helix-normal-mode`:

```lisp
(helix-normal-mode 1)
```

## Configuration

### Multiple cursors

Helix Mode provides extensions for the
[multiple-cursors.el](https://github.com/magnars/multiple-cursors.el)
package available on NonGNU ELPA. If you have `multiple-cursors`
installed, Helix Mode automatically detects it and sets up the
integration when loaded. No additional configuration is needed.

The integration adds selection manipulation keybindings that spawn
multiple cursors (e.g. `helix-multiple-cursors-select-regex`).

### jj as escape

Helix Mode supports remapping "jj" as escape for the purpose of
exiting Insert Mode. Invoke `helix-jj-setup` to activate jj-mode.

This is useful in terminal mode, since escape [currently cannot be
bound](https://github.com/mgmarlow/helix-mode/issues/24).

```lisp
(helix-jj-setup 0.2)
```

By default, `helix-jj-setup` configures a 0.2 second timeout that
waits for a second "j" keypress before canceling. You can configure
different timeouts by passing your desired timeout as an argument to
`helix-jj-setup`.

### Relative line number mode

You can replicate Helix's `line-number: relative` in Helix Mode by
changing the `display-line-numbers` variable in Emacs based on the
current minor mode:

```lisp
(use-package helix
  :hook ((helix-normal-mode . (lambda () (setq display-line-numbers 'relative)))
         (helix-insert-mode . (lambda () (setq display-line-numbers t))))
  :config
  (helix-mode))
```

## Extension

### Keys

You can add new keys to the Helix keymaps via `helix-define-key`:

Example:

```lisp
(helix-define-key 'space "w" #'do-something-cool)
```

The first argument to `helix-define-key` is a Helix state. The valid
options are: insert, normal, space, view, goto, and window.

#### Major-mode-specific keys

You can override Helix keybindings for specific major modes by passing
a mode symbol as the fourth argument to `helix-define-key`. These
bindings only take effect when that mode is active:

```lisp
(with-eval-after-load 'dired
  (helix-define-key 'normal "j" #'dired-next-line 'dired-mode)
  (helix-define-key 'normal "k" #'dired-previous-line 'dired-mode))
```

In this example, `j` and `k` call `dired-next-line` and
`dired-previous-line` when in a Dired buffer, but retain their default
Helix behavior everywhere else. This works by storing the bindings in
`minor-mode-overriding-map-alist`, which takes precedence over the
Helix state keymaps in `minor-mode-map-alist`.

### Typable commands

You can create new typable commands (invoked via ":command-name") with
`helix-define-typable-command`.

Example:

```lisp
(helix-define-typable-command "format" #'format-all-buffer)
```

## Supported keybindings

Normal mode is the default mode. You can return to it by pressing
`ESC`.

### Movement

| Key | Description                     | Command                        |
|:----|:--------------------------------|:-------------------------------|
| h   | Move left                       | `helix-backward-char`          |
| l   | Move right                      | `helix-forward-char`           |
| j   | Move down                       | `helix-next-line`              |
| k   | Move up                         | `helix-previous-line`          |
| w   | Move next word start            | `helix-forward-word-start`     |
| W   | Move next WORD                  | `helix-forward-long-word-start`|
| e   | Move word end                   | `helix-forward-word-end`       |
| E   | Move WORD end                   | `helix-forward-long-word-end`  |
| b   | Move previous word              | `helix-backward-word`          |
| B   | Move previous WORD              | `helix-backward-long-word`     |
| t   | Find 'till next char            | `helix-find-till-char`         |
| T   | Find 'till prev char            | `helix-find-prev-till-char`    |
| f   | Find next char                  | `helix-find-next-char`         |
| F   | Find prev char                  | `helix-find-prev-char`         |
| M-. | Repeat last motion (f, t, F, T) | `helix-find-repeat`            |
| G   | Go to line                      | N/A                            |
| C-b | Move page up                    | N/A                            |
| C-f | Move page down                  | N/A                            |

### Changes

| Key | Description              | Command                       |
|:----|:-------------------------|:------------------------------|
| d   | Delete selection         | `helix-kill-thing-at-point`   |
| y   | Yank selection           | `helix-kill-ring-save`        |
| p   | Paste                    | N/A                           |
| v   | Begin selection          | `helix-begin-selection`       |
| u   | Undo                     | N/A                           |
| o   | Insert newline           | `helix-insert-newline`        |
| O   | Insert line above        | `helis-insert-prevline`       |
| i   | Insert mode              | `helix-insert`                |
| I   | Insert beginning of line | `helix-insert-beginning-line` |
| a   | Insert after             | `helix-insert-after`          |
| A   | Insert end of line       | `helix-insert-after-end-line` |
| r   | Replace with a character | `helix-replace`               |
| R   | Replace with yanked text | `helix-replace-yanked`        |
| C-c | Comment line             | N/A                           |

### Selection

| Key | Description         | Command             |
|:----|:--------------------|:--------------------|
| x   | Select current line | `helix-select-line` |

### Search

| Key | Description               | Command                 |
|:----|:--------------------------|:------------------------|
| /   | Search                    | `helix-search`          |
| n   | Continue search forwards  | `helix-search-forward`  |
| N   | Continue search backwards | `helix-search-backward` |

### Command mode

Accessed by typing `:` in normal mode. Accepts typable commands like
`:write`, `:quit`, and so on.

### Goto mode

Accessed by typing `g` in normal mode.

| Key | Description                          | Command                        |
|:----|:-------------------------------------|:-------------------------------|
| g   | Go to beginning of file              | `helix-go-beginning-buffer`    |
| e   | Go to end of file                    | `helix-go-end-buffer`          |
| l   | Go to end of line                    | `helix-go-end-line`            |
| h   | Go to beginning of line              | `helix-go-beginning-line`      |
| s   | Go to first non-whitespace character | `helix-go-first-nonwhitespace` |
| r   | Find references                      | N/A                            |
| d   | Find definitions                     | N/A                            |

### Window mode

Accessed by typing `C-w` in normal mode.

| Key | Description              | Command |
|:----|:-------------------------|:--------|
| w   | Switch to next window    | N/A     |
| v   | Vertical right split     | N/A     |
| s   | Horizontal bottom split  | N/A     |
| h   | Move to left split       | N/A     |
| j   | Move to split below      | N/A     |
| k   | Move to split above      | N/A     |
| l   | Move to right split      | N/A     |
| q   | Close current window     | N/A     |
| o   | Only keep current window | N/A     |

### Space mode

Accessed by typing `space` in normal mode.

| Key | Description               | Command |
|:----|:--------------------------|:--------|
| f   | Find file at project root | N/A     |
| b   | Switch to project buffer  | N/A     |
| j   | Switch project            | N/A     |
| /   | Search within project     | N/A     |

## Roadmap

Helix Mode isn't designed to completely re-implement Helix in Emacs,
but rather serve as a compatibility layer that connects many of the
Helix keybindings to Emacs functions. I expect most users to still
rely on Emacs fundamentals like isearch, Eglot, consult, or vertico.

Goals:

- Core editing, navigation, and selection behaviors.
- A framework for surrounding contexts (word, paragraph, etc.).
- Tree-sitter navigation.
- Simple multiple cursors (via
  [multiple-cursors](https://github.com/magnars/multiple-cursors.el)
  integration).
- LSP (via [Eglot](https://github.com/joaotavora/eglot) integration).
- Extensibility for custom keybindings/typable commands.

Non-goals:

- Search (Helix Mode provides a simple search, but I think most folks
  are better off using [consult](https://github.com/minad/consult)).
- Pickers.
- Extensive configuration options like the Helix config/languages TOML
  files.
- Completion (I use
  [completion-preview](https://github.com/emacs-mirror/emacs/blob/master/lisp/completion-preview.el)
  and it works perfectly with Helix Mode).
- Advanced multiple-cursors + selection behaviors. I'll do my best to
  support keybindings for multiple-cursors, but I'm unsure how deep I
  want to dive into overhauling the Emacs selection framework.

## Contributing

See [CONTRIBUTING](./CONTRIBUTING.md).

## License

Licensed under GPLv3.
