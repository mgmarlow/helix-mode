;;; helix-core.el --- Base modes and keybindings  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Graham Marlow

;; Author: Graham Marlow
;; Keywords: convenience
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

;; Base modes and keybindings.

;;; Code:

(require 'flymake)
(require 'eglot)

(defgroup helix nil
  "Custom group for Helix."
  :group 'helix)

(defvar-local helix--current-state 'normal
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

Nil if no search has taken place while `helix-mode' is active.")

(defvar helix-current-find nil
  "Current find (method . character), initiated via different helix find methods.

Stores a cons cell where the car is the find method function and the cdr is
the target character.  Methods include `helix-find-next-char',
`helix-find-till-char', `helix-find-prev-char', and `helix-find-prev-till-char'.

Nil if no find has taken place while `helix-mode' is active.")

;; These Helix Minor keymap modes are assigned keymaps during
;; `helix-normal-mode' initialization.
(defvar helix-goto-map nil "Keymap for Goto mode.")
(defvar helix-view-map nil "Keymap for View mode.")
(defvar helix-space-map nil "Keymap for Space mode.")
(defvar helix-window-map nil "Keymap for Window mode.")

(defun helix--unload-current-state ()
  "Deactivate the minor mode described by `helix--current-state'."
  (let ((mode (alist-get helix--current-state helix-state-mode-alist)))
    (funcall mode -1)))

(defun helix--switch-state (state)
  "Switch to STATE."
  (unless (eq state helix--current-state)
    (helix--unload-current-state)
    (helix--clear-data)
    (setq-local helix--current-state state)
    (let ((mode (alist-get state helix-state-mode-alist)))
      (funcall mode 1))))

(defun helix--clear-data ()
  "Clear any intermediate data, e.g. selections/mark."
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
  "Clear any active highlight, unless `helix--current-state' is non-nil."
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
  (call-interactively #'next-line))

(defun helix-previous-line ()
  "Move up."
  (interactive)
  (helix--clear-highlights)
  (call-interactively #'previous-line))

;; TODO: for use in mark mode
(defun helix-surround-thing-at-point (&optional thing)
  "Construct a region around THING at point.

Argument THING must be one of the things identified by the package
thingatpt.  Defaults to \\='word."
  (let ((bounds (bounds-of-thing-at-point (or thing 'word))))
    (when bounds
      (set-mark (car bounds))
      (goto-char (cdr bounds))
      (activate-mark))))

(defmacro helix--with-movement-surround (&rest body)
  "Create a region around movement defined in BODY.

If a region is already active, no new region is created."
  `(progn
     (helix--clear-highlights)
     (let ((current (point)))
       ,@body
       (unless (use-region-p)
         (push-mark current t 'activate)))))

(defun helix-forward-word ()
  "Move to next word."
  (interactive)
  (helix--with-movement-surround
   (re-search-forward "[[:alnum:]]+[ ]*\\|[[:punct:]]+[ ]*\\|\n" nil 'move)))

(defun helix-backward-word ()
  "Move to previous word."
  (interactive)
  (helix--with-movement-surround
   (when (re-search-backward "\\([[:alnum:]]+[ ]*\\)\\|\\([[:punct:]]+[ ]*\\)\\|\n" nil 'move)
     (or (eq (char-after (match-beginning 0)) ?\n)
         (if (match-string 1)
             (skip-syntax-backward "w")
           (skip-syntax-backward ".()"))))))

(defun helix-forward-long-word ()
  "Move to next long word.
If the point is at the end of a line, it first searches for the
non-empty line before moving to the next long word."
  (interactive)
  (unless (eobp)
    (when (looking-at-p "\\s-\\S-") (forward-char))
    (while (looking-at-p ".?$") (forward-line))
    (helix--with-movement-surround
     (when (re-search-forward "[ \t]+\\S-" (- (pos-eol) 1) 'move)
       (backward-char 2)))))

(defun helix-backward-long-word ()
  "Move to previous long word.
If the point is at the beginning of a line, it first searches for the
previous character before moving to the previous long word."
  (interactive)
  (unless (bobp)
    (when (and (bolp) (re-search-backward "[^\n]"))
      (forward-char))
    (helix--with-movement-surround
     (when (re-search-backward "[ \t]+\\S-" (pos-bol) 'move)
       (forward-char)))))

(defun helix-go-beginning-line ()
  "Go to beginning of line."
  (interactive)
  (helix--clear-highlights)
  (beginning-of-line))

(defun helix-go-end-line ()
  "Go to end of line."
  (interactive)
  (helix--clear-highlights)
  (end-of-line))

(defun helix-go-first-nonwhitespace ()
  "Go to first non-whitespace character in line."
  (interactive)
  (helix--clear-highlights)
  (back-to-indentation))

(defun helix-go-beginning-buffer ()
  "Go to beginning of buffer."
  (interactive)
  (helix--clear-highlights)
  (call-interactively #'beginning-of-buffer))

(defun helix-go-end-buffer ()
  "Go to end of buffer."
  (interactive)
  (helix--clear-highlights)
  (call-interactively #'end-of-buffer))

(defun helix-select-line ()
  "Select the current line, moving the cursor to the end."
  (interactive)
  (if (and (region-active-p) (eolp))
      (progn
        (call-interactively #'next-line)
        (end-of-line))
    (beginning-of-line)
    (set-mark-command nil)
    (end-of-line)))

(defun helix-kill-thing-at-point ()
  "Kill current region or current point."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (delete-char 1))
  (helix--clear-data))

(defun helix-begin-selection ()
  "Begin selection at existing region or current point."
  (interactive)
  (unless helix--current-selection
    (if (use-region-p)
        (setq helix--current-selection (region-beginning))
      (set-mark-command nil)
      (setq helix--current-selection (point)))))

(defun helix--end-of-line-p ()
  "Return non-nil if current point is at the end of the current line."
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

(defun helix-insert-newline ()
  "Insert newline and change `helix--current-state' to INSERT mode."
  (interactive)
  (helix--clear-data)
  (end-of-line)
  (newline-and-indent)
  (helix-insert))

(defun helix-insert-prevline ()
  "Insert line above and change `helix--current-state' to INSERT mode."
  (interactive)
  (helix--clear-data)
  (beginning-of-line)
  (let ((electric-indent-mode nil))
    (newline nil t)
    (call-interactively #'previous-line)
    (indent-according-to-mode))
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

(defun helix-find-next-char (char)
  "Goto next CHAR."
  (interactive "c")
  (setq helix-current-find (cons #'helix-find-next-char char))
  (helix--clear-highlights)
  (helix--find-next-char char))

(defun helix-find-prev-char (char)
  "Goto next CHAR."
  (interactive "c")
  (setq helix-current-find (cons #'helix-find-prev-char char))
  (helix--clear-highlights)
  (helix--find-prev-char char))

(defun helix-find-till-char (char)
  "Goto till CHAR."
  (interactive "c")
  (setq helix-current-find (cons #'helix-find-till-char char))
  (helix--clear-highlights)
  (helix--find-next-char char t))

(defun helix-find-prev-till-char (char)
  "Goto till CHAR."
  (interactive "c")
  (setq helix-current-find (cons #'helix-find-prev-till-char char))
  (helix--clear-highlights)
  (helix--find-prev-char char t))

(defun helix-find-repeat ()
  "Repeat the last helix find method."
  (interactive)
  (when helix-current-find
    (funcall (car helix-current-find) (cdr helix-current-find))))

(defun helix--find-next-char (char &optional till)
  "Goto next CHAR.
Place cursor on character found if TILL set to t."
  (let ((current (point))
        (char (make-string 1 char)))
    (when (looking-at-p char) (forward-char))
    (when (and (search-forward char) till)
      (backward-char))
    (helix--select-region current (point))))

(defun helix--find-prev-char (char &optional till)
  "Goto prev CHAR.
Place cursor on character found if TILL set to t."
  (let ((current (point))
        (char (make-string 1 char)))
    (when (char-before (point)) (backward-char))
    (when (and (search-backward char) till)
      (forward-char))
    (helix--select-region current (point))))

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

(defun helix-kill-ring-save ()
  "Save region to `kill-ring' and clear Helix selection data."
  (interactive)
  (call-interactively #'kill-ring-save)
  (helix--clear-data))

(defun helix-indent-left ()
  "Indent region leftward and clear Helix selection data."
  (interactive)
  (call-interactively #'indent-rigidly-left)
  (helix--clear-data))

(defun helix-indent-right ()
  "Indent region rightward and clear Helix selection data."
  (interactive)
  (call-interactively #'indent-rigidly-right)
  (helix--clear-data))

(defun helix-quit (&optional force)
  "Kill Emacs if there's only one window active, otherwise quit the current window.

If FORCE is non-nil, don't prompt for save when killing Emacs."
  (if (one-window-p)
      (if force
          (kill-emacs)
        (call-interactively #'save-buffers-kill-terminal))
    (delete-window)))

(defun helix-revert-all-buffers-quick ()
  "Execute `revert-buffer-quick' on all file-associated buffers."
  (let ((target-buffers (seq-filter
                         (lambda (buf)
                           (and
                            (buffer-file-name buf)
                            (file-readable-p (buffer-file-name buf))))
                         (buffer-list))))
    (mapc (lambda (buf)
            (with-current-buffer buf
              (revert-buffer-quick)))
          target-buffers)
    (message "Reverted %s buffers" (length target-buffers))))

(defvar helix--command-alist
  '((("w" "write") . (lambda () (call-interactively #'save-buffer)))
    (("q" "quit") . helix-quit)
    (("q!" "quit!") . (lambda () (helix-quit t)))
    (("wq" "write-quit") . (lambda ()
                             (save-buffer)
                             (helix-quit)))
    (("o" "open" "e" "edit") . (lambda () (call-interactively #'find-file)))
    (("n" "new") . scratch-buffer)
    (("rl" "reload") . revert-buffer-quick)
    (("reload-all") . helix-revert-all-buffers-quick)
    (("pwd" "show-directory") . pwd)
    (("vs" "vsplit") . split-window-right)
    (("hs" "hsplit") . split-window-below)
    (("config-open") . (lambda () (find-file user-init-file))))
  "Alist of commands executed by `helix-execute-command'.")

(defun helix-define-typable-command (command callback)
  "Add COMMAND to `helix--command-alist' that can be invoked via ':<command>'.

Argument CALLBACK is a lambda or function quote defining the behavior
for the typable command.

Example that defines the typable command ':format':
\(helix-define-typable-command \"format\" #\\='format-all-buffer)"
  (add-to-list 'helix--command-alist
               (cons (if (listp command) command (list command)) callback)))

(defun helix-execute-command (input)
  "Look for INPUT in `helix--command-alist' and execute it, if present."
  (interactive "s:")
  (let ((command (string-trim input)))
    (funcall (alist-get command
                        helix--command-alist
                        (lambda ()
                          (message "no such command \'%s\'" command))
                        nil
                        #'seq-contains-p))))

(defvar helix-normal-state-keymap
  (let ((keymap (make-keymap)))
    (define-prefix-command 'helix-goto-map)
    (define-prefix-command 'helix-view-map)
    (define-prefix-command 'helix-space-map)
    (define-prefix-command 'helix-window-map)
    (suppress-keymap keymap t)

    ;; Movement keys
    (define-key keymap "h" #'helix-backward-char)
    (define-key keymap "l" #'helix-forward-char)
    (define-key keymap "j" #'helix-next-line)
    (define-key keymap "k" #'helix-previous-line)
    (define-key keymap "w" #'helix-forward-word)
    (define-key keymap "W" #'helix-forward-long-word)
    (define-key keymap "b" #'helix-backward-word)
    (define-key keymap "B" #'helix-backward-long-word)
    (define-key keymap "G" #'goto-line)
    (define-key keymap "f" #'helix-find-next-char)
    (define-key keymap "t" #'helix-find-till-char)
    (define-key keymap "F" #'helix-find-prev-char)
    (define-key keymap "T" #'helix-find-prev-till-char)
    (define-key keymap "M-." #'helix-find-repeat)
    (define-key keymap (kbd "C-f") #'scroll-up-command)
    (define-key keymap (kbd "C-b") #'scroll-down-command)

    ;; Goto mode
    (define-key keymap "g" 'helix-goto-map)
    (define-key helix-goto-map "l" #'helix-go-end-line)
    (define-key helix-goto-map "h" #'helix-go-beginning-line)
    (define-key helix-goto-map "s" #'helix-go-first-nonwhitespace)
    (define-key helix-goto-map "g" #'helix-go-beginning-buffer)
    (define-key helix-goto-map "e" #'helix-go-end-buffer)
    (define-key helix-goto-map "j" #'helix-next-line)
    (define-key helix-goto-map "k" #'helix-previous-line)
    (define-key helix-goto-map "r" #'xref-find-references)
    (define-key helix-goto-map "d" #'xref-find-definitions)
    (define-key helix-goto-map "y" #'eglot-find-typeDefinition)
    (define-key helix-goto-map "i" #'eglot-find-implementation)

    ;; View mode
    (define-key keymap "z" 'helix-view-map)
    (define-key helix-view-map "z" #'recenter-top-bottom)

    ;; Space mode
    (define-key keymap (kbd "SPC") 'helix-space-map)
    (define-key helix-space-map "f" #'project-find-file)
    (define-key helix-space-map "b" #'project-switch-to-buffer)
    (define-key helix-space-map "j" #'project-switch-project)
    (define-key helix-space-map "/" #'project-find-regexp)
    (define-key helix-space-map "a" #'eglot-code-action-quickfix)

    ;; Window mode
    (define-key keymap (kbd "C-w") 'helix-window-map)
    (define-key helix-window-map "h" #'windmove-left)
    (define-key helix-window-map "l" #'windmove-right)
    (define-key helix-window-map "j" #'windmove-down)
    (define-key helix-window-map "k" #'windmove-up)
    (define-key helix-window-map "w" #'other-window)
    (define-key helix-window-map "v" #'split-window-right)
    (define-key helix-window-map "s" #'split-window-below)
    (define-key helix-window-map "q" #'delete-window)
    (define-key helix-window-map "o" #'delete-other-windows)

    ;; Editing commands
    (define-key keymap "x" #'helix-select-line)
    (define-key keymap "d" #'helix-kill-thing-at-point)
    (define-key keymap "y" #'helix-kill-ring-save)
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
    (define-key keymap "<" #'helix-indent-left)
    (define-key keymap ">" #'helix-indent-right)
    (define-key keymap (kbd "C-c") #'comment-line)

    ;; Unimpared
    (define-key keymap (kbd "]d") #'flymake-goto-next-error)
    (define-key keymap (kbd "[d") #'flymake-goto-prev-error)

    ;; State switching
    (define-key keymap "i" #'helix-insert)
    (define-key keymap "I" #'helix-insert-beginning-line)
    (define-key keymap "a" #'helix-insert-after)
    (define-key keymap "A" #'helix-insert-after-end-line)
    (define-key keymap ":" #'helix-execute-command)
    ;; ESC is defined as the meta-prefix-key, so we can't simply
    ;; rebind "ESC".  Instead, rebind [escape].  More info:
    ;; https://emacs.stackexchange.com/questions/14755/how-to-remove-bindings-to-the-esc-prefix-key
    (define-key keymap [escape] #'keyboard-quit)
    (define-key keymap (kbd "DEL") (lambda () (interactive)))
    keymap)
  "Keymap for Helix normal state.")

(defvar helix-insert-state-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap [escape] #'helix-insert-exit)
    keymap)
  "Keymap for Helix insert state.")

(defvar helix--state-to-keymap-alist
  `((insert . ,helix-insert-state-keymap)
    (normal . ,helix-normal-state-keymap)
    (view . ,helix-view-map)
    (goto . ,helix-goto-map)
    (window . ,helix-window-map)
    (space . ,helix-space-map))
  "Alist mapping a state symbol to a Helix keymap.")

(defun helix-define-key (state key def)
  "Define a new Helix command mapping KEY to the keymap associated with STATE.

Argument STATE must be one of:

- insert
- normal
- view
- goto
- window
- space

Argument DEF should be an interactive function, matching the usage
pattern of `define-key'."
  (if-let (keymap (alist-get state helix--state-to-keymap-alist))
      (define-key keymap key def)
    (error "Invalid state %s" state)))

(define-minor-mode helix-insert-mode
  "Helix INSERT state minor mode."
  :lighter " helix[I]"
  :init-value nil
  :interactive nil
  :global nil
  :keymap helix-insert-state-keymap
  (if helix-insert-mode
      (progn
        (setq-local helix--current-state 'insert)
        (setq cursor-type 'bar))
    (setq-local helix--current-state 'normal)))

;;;###autoload
(define-minor-mode helix-normal-mode
  "Helix NORMAL state minor mode."
  :lighter " helix[N]"
  :init-value nil
  :interactive t
  :global nil
  :keymap helix-normal-state-keymap
  (if helix-normal-mode
      (progn
        (setq-local helix--current-state 'normal)
        (setq cursor-type 'box))))

(defun helix-mode-maybe-activate (&optional status)
  "Activate `helix-normal-mode' if `helix-global-mode' is non-nil.

A non-nil value of STATUS can be passed into `helix-normal-mode' for
disabling."
  (when (and (not (minibufferp)) helix-global-mode)
    (helix-normal-mode (if status status 1))))

;;;###autoload
(defun helix-mode-all (&optional status)
  "Activate `helix-normal-mode' in all buffers.

Argument STATUS is passed through to `helix-mode-maybe-activate'."
  (interactive)
  ;; Set global mode to t before iterating over the buffers so that we
  ;; send the status directly to `helix-normal-mode' (which checks for
  ;; a non-nil value of `helix-global-mode'.
  (setq helix-global-mode t)
  (mapc (lambda (buf)
          (with-current-buffer buf
            (helix-mode-maybe-activate status)))
        (buffer-list))
  (setq helix-global-mode (if status status 1)))

;;;###autoload
(defun helix-mode ()
  "Toggle global Helix mode."
  (interactive)
  (setq helix-global-mode (not helix-global-mode))
  (if helix-global-mode
      (progn
        ;; Ensure `keyboard-quit' clears out intermediate Helix state.
        (advice-add #'keyboard-quit :before #'helix--clear-data)
        (add-hook 'after-change-major-mode-hook #'helix-mode-maybe-activate)
        (helix-normal-mode 1))
    (cond
     (helix-normal-mode (helix-normal-mode -1))
     (helix-insert-mode (helix-insert-mode -1)))
    (advice-remove #'keyboard-quit #'helix--clear-data)
    (remove-hook 'after-change-major-mode-hook #'helix-mode-maybe-activate)))

(provide 'helix-core)
;;; helix-core.el ends here
