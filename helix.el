;;; helix.el --- A minor mode emulating Helix keybindings  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Graham Marlow

;; Author: Graham Marlow
;; Keywords: convenience
;; Version: 0.8.0
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

;; Helix keybindings in Emacs.
;;
;; Helix Mode is a minor mode that emulates Helix editor keybindings
;; in Emacs.  Helix is a modal text editor with keybindings similar to
;; vi, but with some noteworthy differences. Helix Mode supports a
;; small subset of Helix functionality with the goal of recreating the
;; editor navigation/selection experience in Helix while leaving the
;; hard problems (like directory navigation and searching) to Emacs.
;;
;; Usage:
;;
;; Enable Helix mode globally (for all buffers besides minibuffers):
;;
;;   (helix-mode)
;;
;; Or enable it locally in specific buffers:
;;
;;   (helix-normal-mode 1)
;;
;; Normal mode is the default mode. You can return to it by pressing
;; ESC.  Enter insert mode by pressing 'i' (insert before cursor) or
;; 'a' (insert after cursor).
;;
;; Key Features:
;;
;; - Modal editing with normal and insert modes
;; - Helix-style movement commands (hjkl, w/e/b, f/t, etc.)
;; - Selection-first model for editing
;; - Goto mode (g prefix) for quick navigation
;; - Space mode (SPC prefix) for project commands
;; - Window mode (C-w prefix) for window management
;; - Typable commands (invoked with :)
;;
;; Extending Helix Mode:
;;
;; Add custom keybindings:
;;
;;   (helix-define-key 'space "w" #'my-custom-function)
;;
;; Valid states: insert, normal, space, view, goto, window
;;
;; Define custom typable commands:
;;
;;   (helix-define-typable-command "format" #'format-all-buffer)

;;; Code:

(require 'helix-core)
(when (locate-library "multiple-cursors")
  (require 'helix-multiple-cursors)
  (helix-multiple-cursors-setup))
(require 'helix-jj)

(provide 'helix)
;;; helix.el ends here
