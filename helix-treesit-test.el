;;; helix-treesit-test.el --- Tests for Helix tree-sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Graham Marlow

;; Author: Graham Marlow
;; Keywords: tests
;; Version: 0
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

;; Helix tree-sitter selection tests.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'helix)

(cl-defstruct helix-test--treesit-node
  start
  end
  parent
  (named t))

(cl-defun helix-test--treesit-node (start end &key parent (named t))
  "Return a fake tree-sitter node from START to END."
  (make-helix-test--treesit-node
   :start start :end end :parent parent :named named))

(defun helix-test--should-region (start end &optional point)
  "Assert the active region spans START to END.

When POINT is non-nil, also assert point is there."
  (should (= (region-beginning) start))
  (should (= (region-end) end))
  (when point
    (should (= (point) point))))

(defmacro helix-test--with-mocked-treesit-node (node &rest body)
  "Run BODY with tree-sitter node functions mocked around NODE."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'treesit-node-at)
              (lambda (&rest _args) ,node))
             ((symbol-function 'treesit-parser-list)
              (lambda (&rest _args) '(helix-test-parser)))
             ((symbol-function 'treesit-buffer-root-node)
              (lambda (&rest _args) ,node))
             ((symbol-function 'treesit-node-descendant-for-range)
              (lambda (&rest _args) ,node))
             ((symbol-function 'treesit-parent-until)
              (lambda (node pred &optional include-node)
                (unless include-node
                  (setq node (helix-test--treesit-node-parent node)))
                (while (and node (not (funcall pred node)))
                  (setq node (helix-test--treesit-node-parent node)))
                node))
             ((symbol-function 'treesit-node-enclosed-p)
              (lambda (smaller larger &optional strict)
                (let ((small-start (if (consp smaller)
                                       (car smaller)
                                     (helix-test--treesit-node-start smaller)))
                      (small-end (if (consp smaller)
                                     (cdr smaller)
                                   (helix-test--treesit-node-end smaller)))
                      (large-start (if (consp larger)
                                       (car larger)
                                     (helix-test--treesit-node-start larger)))
                      (large-end (if (consp larger)
                                     (cdr larger)
                                   (helix-test--treesit-node-end larger))))
                  (and (<= large-start small-start)
                       (>= large-end small-end)
                       (or (not strict)
                           (< large-start small-start)
                           (> large-end small-end))))))
             ((symbol-function 'treesit-node-start)
              #'helix-test--treesit-node-start)
             ((symbol-function 'treesit-node-end)
              #'helix-test--treesit-node-end)
             ((symbol-function 'treesit-node-parent)
              #'helix-test--treesit-node-parent)
             ((symbol-function 'treesit-node-check)
              (lambda (node property)
                (and node
                     (eq property 'named)
                     (helix-test--treesit-node-named node)))))
     ,@body))

(defmacro helix-test--with-standard-treesit-buffer (&rest body)
  "Run BODY in a buffer with a standard mocked tree-sitter node."
  (declare (indent 0))
  `(with-temp-buffer
     (insert "(foo bar)")
     (goto-char 3)
     (let* ((root (helix-test--treesit-node 1 10))
            (symbol (helix-test--treesit-node 2 5 :parent root)))
       (helix-test--with-mocked-treesit-node symbol
         ,@body))))

(defmacro helix-test--with-no-treesit-parser (&rest body)
  "Run BODY in a buffer without a mocked tree-sitter parser."
  (declare (indent 0))
  `(with-temp-buffer
     (insert "(foo bar)")
     (goto-char 3)
     (let (message-text)
       (cl-letf (((symbol-function 'treesit-parser-list)
                  (lambda (&rest _args) nil))
                 ((symbol-function 'treesit-node-at)
                  (lambda (&rest _args)
                    (error "treesit-node-at should not be called")))
                 ((symbol-function 'message)
                  (lambda (format-string &rest args)
                    (setq message-text (apply #'format format-string args)))))
         ,@body))))

;;; Tree-sitter selection tests

(ert-deftest helix-test-treesit-parser-list-supports-emacs-29-arity ()
  "Test parser detection supports Emacs 29 style `treesit-parser-list'."
  (cl-letf (((symbol-function 'treesit-parser-list)
             (lambda (&rest args)
               (if args
                   (signal 'wrong-number-of-arguments
                           (list 'treesit-parser-list (length args)))
                 '(helix-test-parser)))))
    (should (equal (helix-treesit--parser-list) '(helix-test-parser)))))

(ert-deftest helix-test-treesit-node-enclosed-fallback ()
  "Test node containment works without `treesit-node-enclosed-p'."
  (let ((node (helix-test--treesit-node 1 10))
        (original-fboundp (symbol-function 'fboundp)))
    (cl-letf (((symbol-function 'treesit-node-start)
               #'helix-test--treesit-node-start)
              ((symbol-function 'treesit-node-end)
               #'helix-test--treesit-node-end)
              ((symbol-function 'fboundp)
               (lambda (symbol)
                 (and (not (eq symbol 'treesit-node-enclosed-p))
                      (funcall original-fboundp symbol)))))
      (should (helix-treesit--node-enclosed-p (cons 2 5) node 'partial))
      (should-not (helix-treesit--node-enclosed-p (cons 1 10) node 'partial))
      (should (helix-treesit--node-enclosed-p (cons 2 5) node t))
      (should-not (helix-treesit--node-enclosed-p (cons 1 5) node t)))))

(ert-deftest helix-test-treesit-node-enclosed-supports-emacs-29-signature ()
  "Test node containment falls back with Emacs 29 `treesit-node-enclosed-p'."
  (let ((node (helix-test--treesit-node 1 10)))
    (cl-letf (((symbol-function 'treesit-node-start)
               #'helix-test--treesit-node-start)
              ((symbol-function 'treesit-node-end)
               #'helix-test--treesit-node-end)
              ((symbol-function 'treesit-node-enclosed-p)
               (lambda (node start end)
                 (unless (helix-test--treesit-node-p node)
                   (signal 'wrong-type-argument
                           (list 'treesit-node-p node)))
                 (and (<= (helix-test--treesit-node-start node) start)
                      (>= (helix-test--treesit-node-end node) end)))))
      (should (helix-treesit--node-enclosed-p (cons 2 5) node 'partial))
      (should-not (helix-treesit--node-enclosed-p (cons 1 10) node 'partial)))))

(ert-deftest helix-test-expand-selection-selects-node-at-point ()
  "Test expanding selection selects the smallest named node at point."
  (helix-test--with-standard-treesit-buffer
    (helix-expand-selection)
    (should mark-active)
    (helix-test--should-region 2 5 5)))

(ert-deftest helix-test-expand-selection-expands-active-region ()
  "Test expanding an active node selection selects the parent node."
  (helix-test--with-standard-treesit-buffer
    (helix-expand-selection)
    (helix-expand-selection)
    (helix-test--should-region 1 10 10)))

(ert-deftest helix-test-expand-selection-skips-unnamed-ancestors ()
  "Test expanding selection skips unnamed tree-sitter ancestors."
  (with-temp-buffer
    (insert "(foo bar)")
    (goto-char 3)
    (let* ((root (helix-test--treesit-node 1 10))
           (list-node (helix-test--treesit-node 1 10
                                                :parent root :named nil))
           (symbol (helix-test--treesit-node 2 5 :parent list-node)))
      (helix-test--with-mocked-treesit-node symbol
        (helix-expand-selection)
        (helix-expand-selection)
        (helix-test--should-region 1 10 10)))))

(ert-deftest helix-test-expand-selection-preserves-largest-node ()
  "Test expanding the largest available node leaves selection unchanged."
  (with-temp-buffer
    (insert "(foo bar)")
    (let ((root (helix-test--treesit-node 1 10)))
      (helix--select-region 1 10)
      (helix-test--with-mocked-treesit-node root
        (helix-expand-selection)
        (helix-test--should-region 1 10 10)))))

(ert-deftest helix-test-expand-selection-without-parser-does-not-error ()
  "Test expanding selection without a tree-sitter parser does not error."
  (helix-test--with-no-treesit-parser
    (helix-expand-selection)
    (should (= (point) 3))
    (should-not mark-active)
    (should (equal message-text "Not in a tree-sitter buffer"))))

(ert-deftest helix-test-shrink-selection-restores-previous-expansion ()
  "Test shrinking selection restores the previous tree-sitter selection."
  (helix-test--with-standard-treesit-buffer
    (helix-expand-selection)
    (helix-expand-selection)
    (helix-shrink-selection)
    (helix-test--should-region 2 5 5)))

(ert-deftest helix-test-shrink-selection-selects-node-without-history ()
  "Test shrinking without history selects the current tree-sitter node."
  (helix-test--with-standard-treesit-buffer
    (helix-shrink-selection)
    (helix-test--should-region 2 5 5)))

(ert-deftest helix-test-shrink-selection-stops-at-smallest-expansion ()
  "Test shrinking twice does not duplicate the smallest tree-sitter selection."
  (helix-test--with-standard-treesit-buffer
    (helix-expand-selection)
    (helix-expand-selection)
    (helix-shrink-selection)
    (helix-shrink-selection)
    (helix-test--should-region 2 5 5)))

(ert-deftest helix-test-shrink-selection-without-parser-preserves-selection ()
  "Test shrinking without a tree-sitter parser does not move point."
  (helix-test--with-no-treesit-parser
    (helix--select-region 2 5)
    (helix-shrink-selection)
    (helix-test--should-region 2 5 5)
    (should (equal message-text "Not in a tree-sitter buffer"))))

(ert-deftest helix-test-treesit-go-parent-start ()
  "Test moving to the parent tree-sitter node start."
  (helix-test--with-standard-treesit-buffer
    (helix-treesit-go-parent-start)
    (should (= (point) 1))))

(ert-deftest helix-test-treesit-go-parent-end-from-selection ()
  "Test moving to the parent tree-sitter node end from a selection."
  (helix-test--with-standard-treesit-buffer
    (helix--select-region 2 5)
    (helix-treesit-go-parent-end)
    (should (= (point) 10))))

(ert-deftest helix-test-treesit-go-parent-skips-unnamed-parents ()
  "Test moving to a parent tree-sitter node skips unnamed parents."
  (with-temp-buffer
    (insert "(foo bar)")
    (goto-char 3)
    (let* ((root (helix-test--treesit-node 1 10))
           (unnamed (helix-test--treesit-node 2 5
                                              :parent root :named nil))
           (symbol (helix-test--treesit-node 3 4 :parent unnamed)))
      (helix-test--with-mocked-treesit-node symbol
        (helix-treesit-go-parent-start)
        (should (= (point) 1))))))

(ert-deftest helix-test-treesit-go-parent-without-parser-does-not-error ()
  "Test moving to parent node without a tree-sitter parser does not error."
  (helix-test--with-no-treesit-parser
    (helix-treesit-go-parent-start)
    (should (= (point) 3))
    (should-not mark-active)
    (should (equal message-text "Not in a tree-sitter buffer"))))

(ert-deftest helix-test-treesit-setup-binds-selection-commands ()
  "Test tree-sitter setup binds tree-sitter commands in normal mode."
  (let ((original-m-i (lookup-key helix-normal-state-keymap (kbd "M-i")))
        (original-m-b (lookup-key helix-normal-state-keymap (kbd "M-b")))
        (original-m-e (lookup-key helix-normal-state-keymap (kbd "M-e"))))
    (unwind-protect
        (progn
          (helix-treesit-setup)
          (should (eq (lookup-key helix-normal-state-keymap (kbd "M-i"))
                      #'helix-shrink-selection))
          (should (eq (lookup-key helix-normal-state-keymap (kbd "M-b"))
                      #'helix-treesit-go-parent-start))
          (should (eq (lookup-key helix-normal-state-keymap (kbd "M-e"))
                      #'helix-treesit-go-parent-end)))
      (define-key helix-normal-state-keymap (kbd "M-i") original-m-i)
      (define-key helix-normal-state-keymap (kbd "M-b") original-m-b)
      (define-key helix-normal-state-keymap (kbd "M-e") original-m-e))))

(provide 'helix-treesit-test)
;;; helix-treesit-test.el ends here
