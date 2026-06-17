;;; helix-treesit.el --- Tree-sitter extensions for helix-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Graham Marlow

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

;; Tree-sitter extensions.

;;; Code:

(require 'helix-core)
(require 'treesit)

(defvar-local helix-treesit--selection-stack nil
  "Stack of tree-sitter selection bounds.

The car is the current tree-sitter selection, and the cdr is the
shrink history.")

(defun helix-treesit--clear-selection-state ()
  "Clear tree-sitter selection stack for the current buffer."
  (setq helix-treesit--selection-stack nil))

(defun helix-treesit--active-region-bounds ()
  "Return active region bounds as a cons cell, or nil."
  (when (and mark-active (mark t) (/= (point) (mark t)))
    (cons (region-beginning) (region-end))))

(defun helix-treesit--select-node-bounds (bounds)
  "Select BOUNDS."
  (helix--select-region (car bounds) (cdr bounds)))

(defun helix-treesit--node-bounds (node)
  "Return NODE bounds as a cons cell."
  (when node
    (cons (treesit-node-start node) (treesit-node-end node))))

(defun helix-treesit--node-or-bounds (node-or-bounds)
  "Return NODE-OR-BOUNDS as a bounds cons cell."
  (if (consp node-or-bounds)
      node-or-bounds
    (helix-treesit--node-bounds node-or-bounds)))

(defun helix-treesit--node-enclosed-fallback-p (smaller larger &optional strict)
  "Return non-nil when SMALLER bounds are enclosed by LARGER bounds.

SMALLER and LARGER can be either tree-sitter nodes or cons-cell
bounds.  Argument STRICT follows the same meaning as in
`treesit-node-enclosed-p'."
  (let ((smaller-bounds (helix-treesit--node-or-bounds smaller))
        (larger-bounds (helix-treesit--node-or-bounds larger)))
    (and smaller-bounds
         larger-bounds
         (<= (car larger-bounds) (car smaller-bounds))
         (>= (cdr larger-bounds) (cdr smaller-bounds))
         (or (not strict)
             (if (eq strict 'partial)
                 (or (< (car larger-bounds) (car smaller-bounds))
                     (> (cdr larger-bounds) (cdr smaller-bounds)))
               (and (< (car larger-bounds) (car smaller-bounds))
                    (> (cdr larger-bounds) (cdr smaller-bounds))))))))

;; For Emacs 29 compatibility
(defun helix-treesit--node-enclosed-p (smaller larger &optional strict)
  "Compatibility wrapper for `treesit-node-enclosed-p'."
  (if (fboundp 'treesit-node-enclosed-p)
      (condition-case nil
          (treesit-node-enclosed-p smaller larger strict)
        ((wrong-number-of-arguments wrong-type-argument)
         (helix-treesit--node-enclosed-fallback-p smaller larger strict)))
    (helix-treesit--node-enclosed-fallback-p smaller larger strict)))

(defun helix-treesit--node-strictly-contains-region-p (node start end)
  "Return non-nil if NODE strictly contains the region from START to END."
  (and node
       (treesit-node-check node 'named)
       (helix-treesit--node-enclosed-p (cons start end) node 'partial)))

;; Emacs 30 added optional BUFFER/LANGUAGE/TAG arguments.
(defun helix-treesit--parser-list ()
  "Return tree-sitter parsers in the current buffer."
  (condition-case nil
      (treesit-parser-list nil nil t)
    (wrong-number-of-arguments
     (treesit-parser-list))))

(defun helix-treesit--expansion-node (node start end)
  "Return the nearest named NODE ancestor expanding START to END."
  (treesit-parent-until
   node
   (lambda (candidate)
     (helix-treesit--node-strictly-contains-region-p candidate start end))
   t))

(defun helix-treesit--node-at-point ()
  "Return the named tree-sitter node at point, or nil."
  (when (helix-treesit--parser-list)
    (treesit-node-at (point) nil t)))

(defun helix-treesit--node-at-selection-or-point ()
  "Return the named tree-sitter node at the active selection or point."
  (when (helix-treesit--parser-list)
    (let ((bounds (helix-treesit--active-region-bounds)))
      (if bounds
          (treesit-node-descendant-for-range
           (treesit-buffer-root-node) (car bounds) (cdr bounds) t)
        (treesit-node-at (point) nil t)))))

(defun helix-treesit--named-parent (node)
  "Return the nearest named parent of NODE."
  (and node
       (treesit-parent-until
        node
        (lambda (candidate)
          (treesit-node-check candidate 'named)))))

(defun helix-treesit--go-parent-boundary (boundary)
  "Move point to BOUNDARY of the parent tree-sitter node."
  (let ((parent (helix-treesit--named-parent
                 (helix-treesit--node-at-selection-or-point))))
    (if parent
        (progn
          (helix-treesit--clear-selection-state)
          (helix--clear-highlights)
          (goto-char (funcall boundary parent)))
      (message "No parent tree-sitter node"))))

;;;###autoload
(defun helix-expand-selection ()
  "Expand selection to the next enclosing named tree-sitter node.

If no region is active, select the smallest named node containing
point.  Repeated invocations select increasingly larger ancestor
nodes."
  (interactive)
  (if (helix-treesit--parser-list)
      (let* ((bounds (helix-treesit--active-region-bounds))
             (start (or (car bounds) (point)))
             (end (or (cdr bounds) (point)))
             (node (helix-treesit--node-at-point))
             (target (helix-treesit--expansion-node node start end))
             (target-bounds (helix-treesit--node-bounds target)))
        (if target
            (progn
              (cond
               ((not bounds)
                (helix-treesit--clear-selection-state))
               ((not (equal bounds (car helix-treesit--selection-stack)))
                (setq helix-treesit--selection-stack (list bounds))))
              (unless (equal (car helix-treesit--selection-stack)
                             target-bounds)
                (push target-bounds helix-treesit--selection-stack))
              (helix-treesit--select-node-bounds target-bounds))
          (message "No larger tree-sitter node")))
    (message "Not in a tree-sitter buffer")))

;;;###autoload
(defun helix-shrink-selection ()
  "Shrink selection to the previous tree-sitter selection.

This reverses prior `helix-expand-selection' invocations in the
current buffer.  If no tree-sitter selection history exists,
select the current named node."
  (interactive)
  (if (helix-treesit--parser-list)
      (let* ((bounds (helix-treesit--active-region-bounds))
             (node-bounds (helix-treesit--node-bounds
                           (helix-treesit--node-at-selection-or-point))))
        (cond
         ((and bounds
               (equal bounds (car helix-treesit--selection-stack))
               (cdr helix-treesit--selection-stack))
          (pop helix-treesit--selection-stack)
          (helix-treesit--select-node-bounds
           (car helix-treesit--selection-stack)))
         (node-bounds
          (setq helix-treesit--selection-stack (list node-bounds))
          (helix-treesit--select-node-bounds node-bounds))
         (t
          (helix-treesit--clear-selection-state)
          (message "No smaller tree-sitter node"))))
    (message "Not in a tree-sitter buffer")))

;;;###autoload
(defun helix-treesit-go-parent-start ()
  "Move to the start of the parent node in the syntax tree."
  (interactive)
  (if (helix-treesit--parser-list)
      (helix-treesit--go-parent-boundary #'treesit-node-start)
    (message "Not in a tree-sitter buffer")))

;;;###autoload
(defun helix-treesit-go-parent-end ()
  "Move to the end of the parent node in the syntax tree."
  (interactive)
  (if (helix-treesit--parser-list)
      (helix-treesit--go-parent-boundary #'treesit-node-end)
    (message "Not in a tree-sitter buffer")))

;;;###autoload
(defun helix-treesit-setup ()
  "Set up Helix Mode keybindings for tree-sitter."
  (helix-define-key 'normal (kbd "M-o") #'helix-expand-selection)
  (helix-define-key 'normal (kbd "M-i") #'helix-shrink-selection)
  (helix-define-key 'normal (kbd "M-b") #'helix-treesit-go-parent-start)
  (helix-define-key 'normal (kbd "M-e") #'helix-treesit-go-parent-end))

(provide 'helix-treesit)
;;; helix-treesit.el ends here
