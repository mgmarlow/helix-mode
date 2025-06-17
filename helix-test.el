;;; helix.el --- A minor mode emulating Helix keybindings  -*- lexical-binding: t; -*-

;;(C) 2025  Graham Marlow

;; Author: Graham Marlow
;; Keywords: convenience
;; Version: 0.6.1
;; Package-Requires: ((emacs "28.1"))
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

;; Helix tests.

;;; Code:

(require 'ert)
(require 'helix)

(ert-deftest helix-test-forward-long-word ()
  "Test `helix-forward-long-word' functionality with various scenarios."
  ;; Word case
  (with-temp-buffer
    (insert "This is a test-string to verify helix functionality.")
    (goto-char (point-min))
    (helix-forward-long-word)
    (should (equal (point) (string-match "is a" (buffer-string)))))

  ;; Long word case
  (with-temp-buffer
    (insert "This-is a test-string to verify helix functionality.")
    (goto-char (point-min))
    (helix-forward-long-word)
    (should (equal (point) (string-match "a " (buffer-string)))))

  ;; On whitespace skip it
  (with-temp-buffer
    (insert "This is a test-string to verify helix functionality.")
    (goto-char 5)
    (helix-forward-long-word)
    (should (equal (- (region-end) (region-beginning)) 2))
    (should (equal (point) (string-match "a " (buffer-string)))))

  ;; Case at end of line
  (with-temp-buffer
    (insert "This is a test sentence.\n\nNext sentence follows.")
    (goto-char 11)
    (helix-forward-long-word)
    (should (equal (point) (string-match "sentence" (buffer-string))))))

(ert-deftest helix-test-forward-long-word-before-eol ()
  "Test `helix-forward-long-word' behavior before end of line."
  (with-temp-buffer
    (insert "This is a test sentence.\n\nNext sentence follows.")
    (goto-char 16)
    (helix-forward-long-word)
    (should (equal (point) (string-match "\n" (buffer-string))))))


(ert-deftest helix-test-forward-long-word-at-eob ()
  "Test `helix-forward-long-word' behavior at the end of the buffer."
  (with-temp-buffer
    (insert "This is a test string.")
    (goto-char (point-max))
    (let ((initial-point (point)))
      (helix-forward-long-word)
      (should (equal (point) initial-point)))))

(ert-deftest helix-test-forward-long-word-whitespaces-between-lines ()
  "Test `helix-forward-long-word' behavior with line full of whitespaces.
It should select only the whitespaces."
  (with-temp-buffer
    (insert "This is a test sentence.\n   \nNext sentence follows.")
    (goto-char 24)
    (helix-forward-long-word)
    (should (equal (- (region-end) (region-beginning)) 2))
    (should (equal (point) (string-match "\nNext" (buffer-string))))))

(ert-deftest helix-test-backward-long-word ()
  "Test `helix-backward-long-word' functionality with various scenarios."
  ;; Word case
  (with-temp-buffer
    (insert "This is a test string to verify helix functionality.")
    (goto-char (point-max))
    (helix-backward-long-word)
    ;; Check if point moved correctly
    (should (equal (point) (string-match "unctionality." (buffer-string)))))

  ;; Long word case
  (with-temp-buffer
    (insert "This is a test string to verify helix_functionality.")
    (goto-char (point-max))
    (helix-backward-long-word)
    ;; Check if point moved correctly
    (should (equal (point) (string-match "elix_functionality." (buffer-string)))))

  ;; On whitespace keep it
  (with-temp-buffer
    (insert "This is a test-string to verify helix functionality.")
    (goto-char 8)
    (helix-backward-long-word)
    (should (equal (- (region-end) (region-beginning)) 3))
    (should (equal (point) (string-match "s a" (buffer-string)))))

  ;; Case at beginning of line
  (with-temp-buffer
    (insert "First line.\n\nSecond line follows.")
    (goto-char 13)
    (helix-backward-long-word)
    (should (equal (point) (string-match "ine." (buffer-string))))))

(ert-deftest helix-test-backward-long-word-before-bol ()
  "Test `helix-backward-long-word' behavior before beginning of line."
  (with-temp-buffer
    (insert "This is a test sentence.\n\nNext sentence follows.")
    (goto-char 28)
    (helix-backward-long-word)
    (should (equal (point) (string-match "ext" (buffer-string))))))

(ert-deftest helix-test-backward-long-word-at-bob ()
  "Test `helix-backward-long-word' behavior at the beginning of the buffer."
  (with-temp-buffer
    (insert "This is a test string.")
    (goto-char (point-min))
    (let ((initial-point (point)))
      (helix-backward-long-word)
      (should (equal (point) initial-point)))))

(ert-deftest helix-test-backward-long-word-whitespaces-between-lines ()
  "Test `helix-backward-long-word' behavior with line full of whitespaces.
It should select only the whitespaces."
  (with-temp-buffer
    (insert "This is a test sentence.\n   \nNext sentence follows.")
    (goto-char 29)
    (helix-backward-long-word)
    (should (equal (- (region-end) (region-beginning)) 2))
    (should (equal (point) (string-match "\n" (buffer-string))))))

;; Run all tests
(ert-run-tests-interactively "helix")
;;; helix-test.el ends here
