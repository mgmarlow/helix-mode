;;; helix-jj.el --- Alternative escape sequences  -*- lexical-binding: t; -*-

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

;; Alternative escape sequences for exiting to `helix-normal-mode'.

;;; Code:

(defcustom helix-jj-timeout nil
  "Timeout in seconds for the 'jj' key sequence to exit insert mode.

Defaults to nil, which disables 'jj' exit functionality.  A short value
like 0.2 is recommended."
  :group 'helix)

(defvar-local helix-jj--timer nil
  "Timer for detecting 'jj' key sequence in insert mode.")

;; TODO: eventually support key combinations other than "jj".
(defun helix-jj--maybe-key-combo-exit ()
  "When `helix-jj-timeout' is non-nil, allow existing `helix-insert-mode' via 'jj'.

The timeout set by `helix-jj-timeout' determines how long `helix-mode' waits
before inserting a 'j' character and giving up on existing `helix-insert-mode'."
  (interactive)
  (if helix-jj-timeout
        (if helix-jj--timer
            (progn
              (cancel-timer helix-jj--timer)
              (setq helix-jj--timer nil)
              (helix-insert-exit))
          (setq helix-jj--timer
                (run-with-timer helix-jj-timeout nil
                                (lambda ()
                                  (setq helix-jj--timer nil)
                                  (self-insert-command 1)))))
    (self-insert-command 1)))

(defun helix-jj--maybe-abort-key-combo-exit ()
  "Used in a `pre-command-hook' to escape early from a 'jj' sequence.

If a non-j character is typed, immediately escape from a 'jj' sequence
and remain in `helix-insert-mode'."
  (when (and helix-jj-timeout
             (eq this-command 'self-insert-command)
             (characterp last-command-event)
             (not (eq last-command-event ?j))
             helix-jj--timer)
    (insert "j")
    (cancel-timer helix-jj--timer)
    (setq helix-jj--timer nil)))

;;;###autoload
(defun helix-jj-setup (&optional timeout)
  "Set up 'jj' as an alternative way to exit `helix-insert-mode'.

TIMEOUT is passed `helix-jj-timeout', defaulting to 0.2.  The timeout
controls how many seconds `helix-insert-mode' waits for a second j
keypress to escape to normal mode."
  (setq helix-jj-timeout (or timeout 0.2))
  (add-hook 'pre-command-hook #'helix-jj--maybe-abort-key-combo-exit)
  (define-key helix-insert-state-keymap "j" #'helix-jj--maybe-key-combo-exit))

(provide 'helix-jj)
;;; helix-jj.el ends here
