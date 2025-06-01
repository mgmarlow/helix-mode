;;; helix-multiple-cursors.el --- Bridge between helix-mode and multiple-cursors  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Graham Marlow

;; Author: Graham Marlow
;; Keywords: convenience
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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

;; Extensions for multiple-cursors:
;; https://github.com/magnars/multiple-cursors.el/tree/master

;;; Code:

(require 'multiple-cursors nil t)

(defvar helix-multiple-cursors-run-for-all-commands
  '(helix-forward-char
    helix-forward-word
    helix-backward-char
    helix-backward-word
    helix-next-line
    helix-previous-line
    helix-go-beginning-line
    helix-go-end-line
    helix-kill-thing-at-point
    helix-insert-exit
    helix-insert
    helix-insert-beginning-line
    helix-insert-after
    helix-insert-after-end-line)
  "Helix Mode commands that are run for all cursors.")

(defun helix-multiple-cursors-select-regex ()
  "Mark every match of selection within region."
  (interactive)
  (call-interactively #'mc/mark-all-in-region-regexp))

;;;###autoload
(defun helix-multiple-cursors-setup ()
  "Set up Helix Mode keybindings for multiple-cursors."
  (unless (featurep 'multiple-cursors)
    (error "requires the multiple-cursors package"))
  (dolist (cmd helix-multiple-cursors-run-for-all-commands)
    (add-to-list 'mc/cmds-to-run-for-all cmd))
  (advice-add #'mc/keyboard-quit :before #'helix--clear-data)
  (define-key helix-normal-state-keymap "s" #'helix-multiple-cursors-select-regex)
  (define-key helix-normal-state-keymap "," #'mc/keyboard-quit)
  (define-key helix-normal-state-keymap [escape] #'mc/keyboard-quit))

(provide 'helix-multiple-cursors)
;;; helix-multiple-cursors.el ends here
