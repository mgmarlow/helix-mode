;;; helix.el --- A minor mode emulating Helix keybindings  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Graham Marlow

;; Author: Graham Marlow
;; Keywords: convenience
;; Version: 0.3.1
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/mgmarlow/helix-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Helix keybindings in Emacs.

;;; Code:

(defgroup helix nil
  "Custom group for Helix."
  :group 'helix)

(defvar helix--current-state 'normal
  "Current modal state, one of normal or insert.")

(defvar helix-state-mode-alist
  `((insert . helix-insert-mode)
    (normal . helix-normal-mode))
  "Alist of symbol state name to minor mode.")

(defvar helix--current-selection nil
  "Beginning point of current visual selection.")

(defvar helix-global-mode nil
  "Enable Helix mode in all buffers.")

(defvar helix-current-search nil
  "Current search string, initiated via `helix-search'.

Nil if no search has taken place while helix-mode is active.")

(defun helix--update-cursor ()
  "Update cursor appearance based on modal state."
  (setq cursor-type
        (cond
          ((eq helix--current-state 'insert) 'bar)
          ((eq helix--current-state 'normal) 'box))))

(defun helix--unload-current-state ()
  (let ((mode (alist-get helix--current-state helix-state-mode-alist)))
    (funcall mode -1)))

(defun helix--switch-state (state)
  "Switch to STATE."
  (unless (eq state helix--current-state)
    (helix--unload-current-state)
    (helix--clear-data)
    (setq helix--current-state state)
    (let ((mode (alist-get state helix-state-mode-alist)))
      (funcall mode 1))
    (helix--update-cursor)))

(defun helix--clear-data ()
  "Clear any intermediate data, e.g. selections/marks."
  (setq helix--current-selection nil)
  (deactivate-mark))

(defun helix-insert ()
  "Switch to insert state."
  (interactive)
  (helix--switch-state 'insert))

(defun helix-insert-exit ()
  "Switch to normal state."
  (interactive)
  (helix--switch-state 'normal))

(defun helix--clear-highlights ()
  "Clear any active highlights, unless `helix--current-state' is non-nil."
  (unless helix--current-selection
    (deactivate-mark)))

(defun helix-backward-char ()
  "Move left."
  (interactive)
  (helix--clear-highlights)
  (backward-char))

(defun helix-forward-char ()
  "Move right."
  (interactive)
  (helix--clear-highlights)
  (forward-char))

(defun helix-next-line ()
  "Move down."
  (interactive)
  (helix--clear-highlights)
  (next-line))

(defun helix-previous-line ()
  "Move up."
  (interactive)
  (helix--clear-highlights)
  (previous-line))

(defun helix--select-thing-at-point (dir &optional thing)
  "Create a region around the THING at point, relative to DIR.

If THING is nil, the syntactic entity defaults to 'word."
  (let ((bounds (bounds-of-thing-at-point (or thing 'word))))
    (when bounds
      (push-mark
       (if (eq dir 'backward) (cdr bounds) (car bounds))
       t 'activate))))

(defun helix-forward-word ()
  "Move to next word.

If `helix--current-selection' is nil, create a region around the word at
point.  Otherwise, continue the existing region."
  (interactive)
  (helix--clear-highlights)
  (forward-word)
  (unless (use-region-p)
    (helix--select-thing-at-point 'forward)))

(defun helix-backward-word ()
  "Move to previous word.

If `helix--current-selection' is nil, create a region around the word at
point. Otherwise, continue the existing region."
  (interactive)
  (helix--clear-highlights)
  (backward-word)
  (unless (use-region-p)
    (helix--select-thing-at-point 'backward)))

(defun helix-go-beginning-line ()
  "Go to beginning of line"
  (interactive)
  (helix--clear-highlights)
  (beginning-of-line))

(defun helix-go-end-line ()
  "Go to end of line"
  (interactive)
  (helix--clear-highlights)
  (end-of-line))

(defun helix-go-beginning-buffer ()
  "Go to beginning of buffer."
  (interactive)
  (helix--clear-highlights)
  (beginning-of-buffer))

(defun helix-go-end-buffer ()
  "Go to end of buffer."
  (interactive)
  (helix--clear-highlights)
  (end-of-buffer))

(defun helix-select-line ()
  "Select the current line, moving the cursor to the end."
  (interactive)
  (if (region-active-p)
      (progn
        (next-line)
        (end-of-line))
    (beginning-of-line)
    (set-mark-command nil)
    (end-of-line)))

(defun helix-kill-thing-at-point ()
  "Kill current region or current point."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (delete-char 1)))

(defun helix-begin-selection ()
  "Begin selection at existing region or current point."
  (interactive)
  (unless helix--current-selection
    (if (use-region-p)
        (setq helix--current-selection (region-beginning))
      (set-mark-command nil)
      (setq helix--current-selection (point)))))

(defun helix-cancel ()
  "Clear any selections, reset data, and cancel commands."
  (interactive)
  (helix--clear-data)
  (keyboard-quit))

(defun helix--end-of-line-p ()
  "Returns non-nil if current point is at the end of the current line."
  (save-excursion
    (let ((cur (point))
          eol)
      (end-of-line)
      (setq eol (point))
      (= cur eol))))

(defun helix-insert-after ()
  "Swap to insert mode one character beyond current point."
  (interactive)
  (unless (helix--end-of-line-p)
    (forward-char))
  (helix-insert))

(defun helix-insert-beginning-line ()
  "Move current point to the beginning of line and enter insert mode."
  (interactive)
  (beginning-of-line)
  (helix-insert))

(defun helix-insert-after-end-line ()
  "Move current point to the end of line and enter insert mode."
  (interactive)
  (end-of-line)
  (helix-insert))

;; TODO: better handling of indentation based on lang mode.
(defun helix-insert-newline ()
  "Insert newline and change `helix--current-state' to INSERT mode."
  (interactive)
  (helix--clear-data)
  (end-of-line)
  (newline)
  (helix-insert))

(defun helix-insert-prevline ()
  "Insert line above and change `helix--current-state' to INSERT mode."
  (interactive)
  (helix--clear-data)
  (beginning-of-line)
  (newline)
  (previous-line)
  (helix-insert))

(defun helix-search (input)
  "Begin a search for INPUT."
  (interactive "ssearch:")
  (setq helix-current-search input)
  (helix-search-forward))

(defun helix--select-region (start end)
  "Create a region between START and END, leaving the current point at END."
  (deactivate-mark)
  (goto-char start)
  (set-mark-command nil)
  (goto-char end))

(defun helix-search-forward ()
  "When `helix-current-search' is non-nil, search forward."
  (interactive)
  (when helix-current-search
    (search-forward helix-current-search)
    (helix--select-region (match-beginning 0) (match-end 0))))

(defun helix-search-backward ()
  "When `helix-current-search' is non-nil, search backward.

Note that the current point is shifted a single character
backwards before a search takes place so that repeated calls to
`helix-search-backward' work as expected.  Helix places the
current point at the end of the matching word in both forward and
backward searches, while Emacs places the cursor at the beginning
of the matching word in backward searches."
  (interactive)
  (when helix-current-search
    (backward-char)
    (search-backward helix-current-search)
    (helix--select-region (match-beginning 0) (match-end 0))))

(defun helix--replace-region (start end text)
  "Replace region from START to END in-place with TEXT."
  (delete-region start end)
  (insert text)
  (helix--clear-data))

(defun helix-replace (char)
  "Replace selection with CHAR.

If `helix--current-selection' is nil, replace character at point."
  (interactive "c")
  (if helix--current-selection
      (helix--replace-region helix--current-selection (point)
                             (make-string (abs (- (point) helix--current-selection)) char))
    (helix--replace-region (point) (1+ (point)) char)))

(defun helix-replace-yanked ()
  "Replace selection with the last stretch of killed text.

If `helix--current-selection' is nil, replace character at point."
  (interactive)
  (if (= 0 (length kill-ring))
      (message "nothing to yank")
    (if helix--current-selection
        (delete-region helix--current-selection (point))
      (delete-char 1))
    (yank)
    (helix--clear-data)))

(defun helix-execute-command (command)
  "Execute COMMAND."
  (interactive "s:")
  (cond
   ((string= command "w") (call-interactively #'save-buffer))
   ((string= command "write") (call-interactively #'save-buffer))
   (t (message "no such command: \'%s\'" command))))

(defvar helix-normal-state-keymap
  (let ((keymap (make-keymap)))
    (define-prefix-command 'helix-goto-map)
    (suppress-keymap keymap t)

    ;; Movement keys
    (define-key keymap "h" #'helix-backward-char)
    (define-key keymap "l" #'helix-forward-char)
    (define-key keymap "j" #'helix-next-line)
    (define-key keymap "k" #'helix-previous-line)
    (define-key keymap "w" #'helix-forward-word)
    (define-key keymap "b" #'helix-backward-word)

    ;; Go-to menu
    (define-key keymap "g" 'helix-goto-map)
    (define-key helix-goto-map "l" #'helix-go-end-line)
    (define-key helix-goto-map "h" #'helix-go-beginning-line)
    (define-key helix-goto-map "g" #'helix-go-beginning-buffer)
    (define-key helix-goto-map "e" #'helix-go-end-buffer)
    (define-key helix-goto-map "j" #'helix-next-line)
    (define-key helix-goto-map "k" #'helix-previous-line)
    
    ;; Editing commands
    (define-key keymap "x" #'helix-select-line)
    (define-key keymap "d" #'helix-kill-thing-at-point)
    (define-key keymap "y" #'kill-ring-save)
    (define-key keymap "p" #'yank)
    (define-key keymap "v" #'helix-begin-selection)
    (define-key keymap "u" #'undo)
    (define-key keymap "o" #'helix-insert-newline)
    (define-key keymap "O" #'helix-insert-prevline)
    (define-key keymap "/" #'helix-search)
    (define-key keymap "n" #'helix-search-forward)
    (define-key keymap "N" #'helix-search-backward)
    (define-key keymap "r" #'helix-replace)
    (define-key keymap "R" #'helix-replace-yanked)

    ;; State switching
    (define-key keymap "i" #'helix-insert)
    (define-key keymap "I" #'helix-insert-beginning-line)
    (define-key keymap "a" #'helix-insert-after)
    (define-key keymap "A" #'helix-insert-after-end-line)
    (define-key keymap ":" #'helix-execute-command)
    (define-key keymap [escape] #'helix-cancel)
    keymap)
  "Keymap for Helix normal state.")

(defvar helix-insert-state-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap [escape] #'helix-insert-exit)
    keymap)
  "Keymap for Helix insert state.")

(define-minor-mode helix-insert-mode
  "Helix INSERT state minor mode."
  :lighter " helix[I]"
  :init-value nil
  :interactive nil
  :global nil
  :keymap helix-insert-state-keymap)

;;;###autoload
(define-minor-mode helix-normal-mode
  "Helix NORMAL state minor mode."
  :lighter " helix[N]"
  :init-value nil
  :interactive t
  :global nil
  :keymap helix-normal-state-keymap)

(add-hook 'after-change-major-mode-hook #'helix-mode-maybe-activate)

(defun helix-mode-maybe-activate ()
  "Activate `helix-normal-mode' when appropriate."
  (when (and (not (minibufferp)) helix-global-mode)
    (helix-normal-mode 1)))

;;;###autoload
(defun helix-mode ()
  "Toggle global Helix mode"
  (interactive)
  (setq helix-global-mode (not helix-global-mode))
  (if helix-global-mode
      (helix-normal-mode 1)
    (cond
     (helix-normal-mode (helix-normal-mode -1))
     ;; TODO: need to clean up any changes to cursor, etc.
     (helix-insert-mode (helix-insert-mode -1)))))

(provide 'helix)
;;; helix.el ends here
