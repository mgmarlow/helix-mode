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

(defun helix--switch-state (state)
  "Switch to STATE."
  (unless (eq state helix--current-state)
    (let ((mode (alist-get helix--current-state helix-state-mode-alist)))
      (funcall mode -1))
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

(defvar helix-normal-state-keymap
  (let ((keymap (make-keymap)))
    (suppress-keymap keymap t)

    ;; Movement keys
    (define-key keymap "h" 'backward-char)
    (define-key keymap "j" 'next-line)
    (define-key keymap "k" 'previous-line)
    (define-key keymap "l" 'forward-char)
    (define-key keymap "w" 'forward-word)
    (define-key keymap "b" 'backward-word)
    
    ;; Editing commands
    (define-key keymap "x" 'delete-char)
    (define-key keymap "d" 'kill-line)
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
  :lighter "h[insert]"
  :keymap helix-insert-state-keymap
  (when helix-insert-mode
    (message "in insert mode")))

(define-minor-mode helix-normal-mode
  "Helix NORMAL state minor mode."
  :lighter "h[normal]"
  :keymap helix-normal-state-keymap
  (when helix-normal-mode
    (message "in normal mode")))

(defun helix--update-cursor ()
  "Update cursor appearance based on modal state."
  (setq cursor-type
        (cond
          ((eq helix--current-state 'insert) 'bar)
          (t 'box))))

(provide 'helix)
;;; helix.el ends here
