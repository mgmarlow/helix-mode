;;; helix.el --- A minor mode emulating Helix keybindings  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Graham Marlow

;; Author: Graham Marlow
;; Keywords: convenience

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

;; Eventually define a defcustom group:
; (defgroup helix nil
;   "Custom group for Helix."
;   :group 'helix)

(defvar helix--current-state 'normal)

(defvar helix-state-mode-alist
  `((insert . helix-insert-mode)
    (normal . helix-normal-mode))
  "Alist of symbol state name to minor mode.")

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
    (setq helix--current-state state)
    (let ((mode (alist-get state helix-state-mode-alist)))
      (funcall mode 1))
    (helix--update-cursor)))

(defun helix-insert ()
  "Switch to insert state."
  (interactive)
  (helix--switch-state 'insert))

(defun helix-insert-exit ()
  "Switch to normal state."
  (interactive)
  (helix--switch-state 'normal))

;; TODO persist highlights when "v" is active.
(defun helix--clear-highlights ()
  "Clear any active highlights, if present."
  (deactivate-mark))

(defun helix--backward-char ()
  (interactive)
  (helix--clear-highlights)
  (backward-char))

(defun helix--forward-char ()
  (interactive)
  (helix--clear-highlights)
  (forward-char))

(defun helix--next-line ()
  (interactive)
  (helix--clear-highlights)
  (next-line))

(defun helix--previous-line ()
  (interactive)
  (helix--clear-highlights)
  (previous-line))

;; TODO handle line breaks more effectively
(defun helix--forward-word ()
  (interactive)
  (helix--clear-highlights)
  (unless (use-region-p)
    (set-mark-command nil))
  (forward-word))

(defun helix--backward-word ()
  (interactive)
  (helix--clear-highlights)
  (unless (use-region-p)
    (set-mark-command nil))
  (backward-word))

(defun helix--select-line ()
  (interactive)
  (if (use-region-p)
      (next-line)
    (beginning-of-line)
    (set-mark-command nil)
    (end-of-line)))

(defun helix--kill-thing-at-point ()
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (delete-char 1)))

(defvar helix-normal-state-keymap
  (let ((keymap (make-keymap)))
    (suppress-keymap keymap t)

    ;; Movement keys
    (define-key keymap "h" 'helix--backward-char)
    (define-key keymap "l" 'helix--forward-char)
    (define-key keymap "j" 'helix--next-line)
    (define-key keymap "k" 'helix--previous-line)
    (define-key keymap "w" 'helix--forward-word)
    (define-key keymap "b" 'helix--backward-word)
    
    ;; Editing commands
    (define-key keymap "x" 'helix--select-line)
    (define-key keymap "d" 'helix--kill-thing-at-point)
    (define-key keymap "y" 'kill-ring-save)
    (define-key keymap "p" 'yank)

    ;; State switching
    (define-key keymap "i" 'helix-insert)
    keymap)
  "Keymap for Helix normal state.")

(defvar helix-insert-state-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap [escape] 'helix-insert-exit)
    keymap)
  "Keymap for Helix insert state.")

(define-minor-mode helix-insert-mode
  "Helix INSERT state minor mode."
  :lighter " helix[I]"
  :keymap helix-insert-state-keymap
  (when helix-insert-mode
    (message "in insert mode")))

(define-minor-mode helix-normal-mode
  "Helix NORMAL state minor mode."
  :lighter " helix[N]"
  :keymap helix-normal-state-keymap
  (when helix-normal-mode
    (message "in normal mode")))

(provide 'helix)
;;; helix.el ends here
