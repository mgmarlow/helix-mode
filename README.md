# Helix Mode

Helix keybindings in Emacs.

[Helix](https://helix-editor.com/) is a modal text editor that uses a
keybinding scheme very similar to
[Kakoune](https://kakoune.org/why-kakoune/why-kakoune.html). That is,
keybindings that are reminiscent of vi but flip around the verb-object
model. Helix Mode implements a subset of Helix keybindings and
modes.

## Installation

Clone Helix Mode:

```
git clone https://github.com/mgmarlow/helix-mode /path/to/helix-mode
```

Add Helix Mode to your load path and enable it globally:

```lisp
(add-to-list 'load-path "/path/to/helix-mode")
(require 'helix)
(helix-mode)
```

Or (>= Emacs 30.1) point `use-package` to this repository with the vc
keyword:

```lisp
(use-package helix
  :vc (:url "https://github.com/mgmarlow/helix-mode")
  :config
  (helix-mode))
```

### Global vs. local mode

You can enable Helix mode globally (e.g. for all buffers besides
minibuffers) via

```lisp
(helix-mode)
```

> Warning: Helix mode is still a work in progress, so some special
> buffer modes like dired will not work perfectly. You can still use
> Helix globally but note that you may need to manually invoke some
> commands in special buffers.

Alternatively, you can manually toggle Helix mode on/off in local
buffers by invoking `helix-normal-mode`:

```lisp
(helix-normal-mode 1)
```

## Supported keybindings

Normal mode is the default mode. You can return to it by pressing
`ESC`.

### Movement

| Key | Description        | Command               |
|:----|:-------------------|:----------------------|
| h   | Move left          | `helix-backward-char` |
| l   | Move right         | `helix-forward-char`  |
| j   | Move down          | `helix-next-line`     |
| k   | Move up            | `helix-previous-line` |
| w   | Move next word     | `helix-forward-word`  |
| b   | Move previous word | `helix-backward-word` |
| G   | Go to line         | N/A                   |
| C-b | Move page up       | N/A                   |
| C-f | Move page down     | N/A                   |

### Changes

| Key | Description              | Command                       |
|:----|:-------------------------|:------------------------------|
| d   | Delete selection         | `helix-kill-thing-at-point`   |
| y   | Yank selection           | N/A                           |
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

Accessed by typing `:` in normal mode.

Currently only supports basic save/quit commands, e.g. `write`,
`write-quit`.

### Goto mode

Accessed by typing `g` in normal mode.

| Key | Description             | Command                     |
|:----|:------------------------|:----------------------------|
| g   | Go to beginning of file | `helix-go-beginning-buffer` |
| e   | Go to end of file       | `helix-go-end-buffer`       |
| l   | Go to end of line       | `helix-go-end-line`         |
| h   | Go to beginning of line | `helix-go-beginning-line`   |

## License

Licensed under GPLv3.
