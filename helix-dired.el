;;; helix-dired.el --- Dired extensions for Helix Mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Graham Marlow

;; Author: Graham Marlow <gmarlow@LYD02P9G4G-Graham-Marlow>
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

;; Dired extensions for Helix Mode.

;;; Code:

(require 'helix-core)
(require 'dired)

(defun helix-extension-define-key (state keymap-symbol &rest bindings)
  "Define key bindings for extension modes that work with Helix states.
Argument STATE must be one of the helix states (normal, insert, etc.).
Argument KEYMAP-SYMBOL is the symbol of the keymap to bind keys in.
BINDINGS is a list of alternating keys and commands.
Example:
  (helix-extension-define-key 'normal 'dired-mode-map
                              \"q\" #'quit-window
                              \"j\" #'dired-next-line)"
  ;; state currently unused.
  (unless (alist-get state helix--state-to-keymap-alist)
    (error "Invalid state %s" state))

  (let ((keymap (symbol-value keymap-symbol)))
    (unless (keymapp keymap)
      (error "%s is not a keymap" keymap-symbol))

    ;; Process bindings in pairs
    (while bindings
      (let ((key (pop bindings))
            (def (pop bindings)))
        (unless def
          (error "Odd number of arguments in key bindings"))
        (message "defining key!")
        (define-key keymap key def)))))

;;;###autoload
(defun helix-dired-setup ()
  "Set up Helix bindings for `dired'."
  ;; TODO https://www.gnu.org/software/emacs/manual/html_node/elisp/Controlling-Active-Maps.html
  (helix-extension-define-key 'normal 'dired-mode-map
                              "q" #'quit-window
                              "j" #'dired-next-line
                              "k" #'dired-previous-line
                              [mouse-2] #'dired-mouse-find-file-other-window
                              "D" #'dired-do-delete
                              "B" #'dired-do-byte-compile
                              "i" #'dired-toggle-read-only
                              "r" #'dired-do-redisplay
                              "m" #'dired-mark
                              "u" #'dired-unmark))

(provide 'helix-dired)
;;; helix-dired.el ends here
