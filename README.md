# Helix Mode

Helix keybindings in Emacs.

## Installation

Point `use-package` to this repository:

```lisp
(use-package helix
  :vc (:url "https://github.com/mgmarlow/helix-mode")
  :config
  ;; Activate Helix globally
  (helix-mode))
```

### Global vs. local mode

You can enable Helix mode globally (e.g. for all buffers besides
minibuffers) via

```lisp
(helix-mode)
```

> Warning: Helix mode is still a work in progress, so some special
> buffer modes like dired will not work nicely with Helix. You can
> still use Helix globally but note that you may need to manually
> invoke some commands in these special buffers, like
> `dired-do-delete`.

Alternatively, you can manually toggle turn Helix mode on/off in local
buffers by invoking `helix-normal-mode`:

```lisp
(helix-normal-mode 1)
```

## Supported keybindings

Only a subset of [Helix
keybindings](https://docs.helix-editor.com/keymap.html) are currently
supported.

### Movement, selection, and editing

| Key | Description                  | Command                            |
|:----|:-----------------------------|:-----------------------------------|
| h   | Move left                    | `helix-backward-char`              |
| l   | Move right                   | `helix-forward-char`               |
| j   | Move down                    | `helix-next-line`                  |
| k   | Move up                      | `helix-previous-line`              |
| w   | Move next word               | `helix-forward-word`               |
| b   | Move previous word           | `helix-backward-word`              |
| x   | Select current line          | `helix-select-line`                |
| d   | Delete selection             | `helix-kill-thing-at-point`        |
| y   | Yank selection               | N/A                                |
| p   | Paste                        | N/A                                |
| v   | Begin selection              | `helix-begin-selection`            |
| u   | Undo                         | N/A                                |
| /   | Search                       | `helix-search`                     |
| n   | Continue search forwards     | `helix-search-forward`             |
| N   | Continue search backwards    | `helix-search-backward`            |
| o   | Insert newline               | `helix-insert-newline`             |
| O   | Insert line above            | `helis-insert-prevline`            |
| i   | Insert mode                  | `helix-insert`                     |
| ESC | Cancel/switch to normal mode | `helix-cancel`/`helix-insert-exit` |

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
