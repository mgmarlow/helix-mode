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
| o   | Insert newline               | `helix-insert-newline`             |
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
