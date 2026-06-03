;;; helix-avy.el --- Helix goto-mode extensions with Avy -*- lexical-binding: t; -*-

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

;; Extensions for goto-mode backed by Avy:
;; https://github.com/abo-abo/avy

;;; Code:

(require 'helix-core)
(require 'avy nil t)

;; Declare optional external dependencies to satisfy the byte compiler
(declare-function avy-goto-word-0 "avy")

(defun helix-avy-goto-word ()
  "Call `avy-goto-word-0' and navigate to the end of the desired word."
  (interactive)
  (call-interactively #'avy-goto-word-0)
  (call-interactively #'helix-forward-word-end))

;;;###autoload
(defun helix-avy-setup ()
  "Set up Helix Mode keybindings for avy."
  (unless (featurep 'avy)
    (error "Requires the avy package"))
  (helix-define-key 'goto "w" #'helix-avy-goto-word))

(provide 'helix-avy)
;;; helix-avy.el ends here
