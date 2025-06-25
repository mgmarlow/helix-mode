;;; helix.el --- Tests for Helix minor mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Graham Marlow

;; Author: Graham Marlow
;;         Corentin Roy
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

;; Helix tests.

;;; Code:

(require 'ert)
(require 'helix)

;;; Forward long word tests

(ert-deftest helix-test-forward-long-word-basic-movement ()
  "Test basic forward movement between words."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char (point-min))
    (helix-forward-long-word)
    (should (= (point) 6)) ; before "world"
    (helix-forward-long-word)
    (should (= (point) 12)) ; before "test"
    (helix-forward-long-word)
    (should (= (point) (- (point-max) 1))))) ; before end of line

(ert-deftest helix-test-forward-long-word-hyphenated-words ()
  "Test forward movement with hyphenated words (long words)."
  (with-temp-buffer
    (insert "this test-string-example works")
    (goto-char (point-min))
    (helix-forward-long-word)
    (should (= (point) 5)) ; before "test-string-example"
    (helix-forward-long-word)
    (should (= (point) 25)))) ; before "works"

(ert-deftest helix-test-forward-long-word-on-whitespace ()
  "Test that forward movement skips over whitespace."
  (with-temp-buffer
    (insert "word next")
    (goto-char 5) ; on the first whitespace
    (helix-forward-long-word)
    (should (= (point) (- (point-max) 1))))) ; before end of line

(ert-deftest helix-test-forward-long-word-on-whitespaces ()
  "Test that forward movement skips over whitespace."
  (with-temp-buffer
    (insert "word   \t  next")
    (goto-char 5) ; on the first whitespace
    (helix-forward-long-word)
    (should (= (point) 10)))) ; start of "next"

(ert-deftest helix-test-forward-long-word-multiple-lines ()
  "Test forward movement across multiple lines."
  (with-temp-buffer
    (insert "first line\nsecond line\nthird")
    (goto-char 1) ; start of buffer
    (helix-forward-long-word)
    (should (= (point) 6)) ; before "line"
    (helix-forward-long-word)
    (should (= (point) 10)) ; before end of first line
    (helix-forward-long-word)
    (should (= (point) 18)))) ; before "line" of second line

(ert-deftest helix-test-forward-long-word-empty-lines ()
  "Test forward movement with empty lines."
  (with-temp-buffer
    (insert "first\n\n\nsecond")
    (goto-char 5) ; before end of first line
    (helix-forward-long-word)
    (should (= (point) (- (point-max) 1))))) ; before end of second line

(ert-deftest helix-test-forward-long-word-at-end-of-buffer ()
  "Test that forward movement at end of buffer doesn't move."
  (with-temp-buffer
    (insert "test word")
    (goto-char (point-max))
    (let ((initial-point (point)))
      (helix-forward-long-word)
      (should (= (point) initial-point)))))

(ert-deftest helix-test-forward-long-word-mixed-separators ()
  "Test forward movement with mixed word separators."
  (with-temp-buffer
    (insert "word1_part2-part3.part4 next")
    (goto-char (point-min))
    (helix-forward-long-word)
    (should (= (point) 24)))) ; before "next"

(ert-deftest helix-test-forward-long-word-punctuation ()
  "Test forward movement with punctuation."
  (with-temp-buffer
    (insert "Hello, world! How are you?")
    (goto-char (point-min))
    (helix-forward-long-word)
    (should (= (point) 7)) ; before "world!"
    (helix-forward-long-word)
    (should (= (point) 14)))) ; before "How"

;;; Backward long word tests

(ert-deftest helix-test-backward-long-word-basic-movement ()
  "Test basic backward movement between words."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 13)) ; start of "test"
    (helix-backward-long-word)
    (should (= (point) 7)) ; start of "world"
    (helix-backward-long-word)
    (should (= (point) 1)))) ; start of "hello"

(ert-deftest helix-test-backward-long-word-hyphenated-words ()
  "Test backward movement with hyphenated words (long words)."
  (with-temp-buffer
    (insert "this test-string-example works")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 26)) ; start of "works"
    (helix-backward-long-word)
    (should (= (point) 6)))) ; start of "test-string-example"

(ert-deftest helix-test-backward-long-word-on-whitespace ()
  "Test that backward movement skips over whitespace."
  (with-temp-buffer
    (insert "word next")
    (goto-char 5) ; on whitespace
    (helix-backward-long-word)
    (should (= (point) 1)))) ; start of "word"

(ert-deftest helix-test-backward-long-word-on-whitespaces ()
  "Test that backward movement skips over whitespace."
  (with-temp-buffer
    (insert "word   \t  next")
    (goto-char 10) ; on the last whitespace
    (helix-backward-long-word)
    (should (= (point) 1)))) ; start of "word"

(ert-deftest helix-test-backward-long-word-multiple-lines ()
  "Test backward movement across multiple lines."
  (with-temp-buffer
    (insert "first line\nsecond line\nthird")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 24)) ; start of "third"
    (helix-backward-long-word)
    (should (= (point) 19)) ; start of "line"
    (helix-backward-long-word)
    (should (= (point) 12)))) ; start of "second"

(ert-deftest helix-test-backward-long-word-empty-lines ()
  "Test backward movement with empty lines."
  (with-temp-buffer
    (insert "first\n\n\nsecond")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 9)) ; start of "second"
    (helix-backward-long-word)
    (should (= (point) 1)))) ; start of "first"

(ert-deftest helix-test-backward-long-word-at-beginning-of-buffer ()
  "Test that backward movement at beginning of buffer doesn't move."
  (with-temp-buffer
    (insert "test word")
    (goto-char (point-min))
    (let ((initial-point (point)))
      (helix-backward-long-word)
      (should (= (point) initial-point)))))

(ert-deftest helix-test-backward-long-word-mixed-separators ()
  "Test backward movement with mixed word separators."
  (with-temp-buffer
    (insert "word1_part2-part3.part4 next")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 25)) ; start of "next"
    (helix-backward-long-word)
    (should (= (point) 1)))) ; start of "word1_part2-part3.part4"

(ert-deftest helix-test-backward-long-word-punctuation ()
  "Test backward movement with punctuation."
  (with-temp-buffer
    (insert "Hello, world! How are you?")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 23)) ; start of "you?"
    (helix-backward-long-word)
    (should (= (point) 19)))) ; start of "are"

;;; Edge case tests

(ert-deftest helix-test-forward-long-word-empty-buffer ()
  "Test forward movement in empty buffer."
  (with-temp-buffer
    (let ((initial-point (point)))
      (helix-forward-long-word)
      (should (= (point) initial-point)))))

(ert-deftest helix-test-backward-long-word-empty-buffer ()
  "Test backward movement in empty buffer."
  (with-temp-buffer
    (let ((initial-point (point)))
      (helix-backward-long-word)
      (should (= (point) initial-point)))))

(ert-deftest helix-test-forward-long-word-only-whitespace ()
  "Test forward movement in buffer with only whitespace."
  (with-temp-buffer
    (insert "   \t\n  ")
    (goto-char 1)
    (helix-forward-long-word)
    (should (= (point) 4)))) ; before end of first line

(ert-deftest helix-test-backward-long-word-only-whitespace ()
  "Test backward movement in buffer with only whitespace."
  (with-temp-buffer
    (insert "   \t\n  ")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 6)))) ; start of second line

(ert-deftest helix-test-forward-long-word-single-character ()
  "Test forward movement with single character words."
  (with-temp-buffer
    (insert "a b c d")
    (goto-char (point-min))
    (helix-forward-long-word)
    (should (= (point) 2)) ; before "b"
    (helix-forward-long-word)
    (should (= (point) 4)))) ; before "c"

(ert-deftest helix-test-backward-long-word-single-character ()
  "Test backward movement with single character words."
  (with-temp-buffer
    (insert "a b c d")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 7)) ; start of "d"
    (helix-backward-long-word)
    (should (= (point) 5)))) ; start of "c"

;;; Region/selection tests

(ert-deftest helix-test-forward-long-word-with-region ()
  "Test forward movement surrounding behavior."
  (with-temp-buffer
    (insert "first second third")
    (goto-char 1)
    (helix-forward-long-word)
    (should (eql (- (region-end) (region-beginning)) 5))
    (helix-forward-long-word)
    (should (eql (- (region-end) (region-beginning)) 6))))

(ert-deftest helix-test-forward-long-word-with-region-at-eol ()
  "Test forward movement surrounding behavior."
  (with-temp-buffer
    (insert "first\nsecond third")
    (goto-char 5) ; before end of line
    (helix-forward-long-word)
    (should (eql (- (region-end) (region-beginning)) 6))))

(ert-deftest helix-test-forward-long-word-on-whitespace-with-region ()
  "Test forward movement surrounding behavior."
  (with-temp-buffer
    (insert "first second third")
    (goto-char 6) ; on whitespace
    (helix-forward-long-word)
    (should (eql (- (region-end) (region-beginning)) 6))))

(ert-deftest helix-test-forward-long-word-on-whitespaces-with-region ()
  "Test forward movement surrounding behavior."
  (with-temp-buffer
    (insert "first   second third") ; three whitespaces
    (goto-char 6) ; on first whitespace
    (helix-forward-long-word)
    (should (eql (- (region-end) (region-beginning)) 2))))

(ert-deftest helix-test-backward-long-word-with-region ()
  "Test backward movement surrounding behavior."
  (with-temp-buffer
    (insert "first second third")
    (goto-char 18)
    (helix-backward-long-word)
    (should (eql (- (region-end) (region-beginning)) 4))
    (helix-backward-long-word)
    (should (eql (- (region-end) (region-beginning)) 7))))

(ert-deftest helix-test-backward-long-word-with-region-at-bol ()
  "Test backward movement surrounding behavior."
  (with-temp-buffer
    (insert "first second\nthird")
    (goto-char 13) ; start of second line
    (helix-backward-long-word)
    (should (eql (- (region-end) (region-beginning)) 6))))

(ert-deftest helix-test-backward-long-word-on-whitespace-with-region ()
  "Test backward movement surrounding behavior."
  (with-temp-buffer
    (insert "first second third")
    (goto-char 6) ; on whitespace
    (helix-backward-long-word)
    (should (eql (- (region-end) (region-beginning)) 5))))

(ert-deftest helix-test-backward-long-word-on-whitespaces-with-region ()
  "Test backward movement surrounding behavior."
  (with-temp-buffer
    (insert "first   second third") ; three whitespaces
    (goto-char 8) ; on last whitespace
    (helix-backward-long-word)
    (should (eql (- (region-end) (region-beginning)) 7))))

;; Find char

(ert-deftest helix-test-find-next-char ()
  "Test find char go to char and select to it."
  (with-temp-buffer
    (insert "first second third")
    (goto-char 1)
    (helix-find-next-char ?d)
    (should (eql (point) 13))))

(ert-deftest helix-test-find-next-char-two-line ()
  "Test find on multi lines char go to char and select to it."
  (with-temp-buffer
    (insert "first\nsecond\nthird")
    (goto-char 1)
    (helix-find-next-char ?d)
    (should (eql (point) 13))))

(ert-deftest helix-test-find-till-char ()
  "Test find char go to char and select to it."
  (with-temp-buffer
    (insert "first second third")
    (goto-char 1)
    (helix-find-till-char ?d)
    (should (eql (point) 12))))

(ert-deftest helix-test-find-till-char-two-line ()
  "Test find on multi lines char go to char and select to it."
  (with-temp-buffer
    (insert "first\nsecond\nthird")
    (goto-char 1)
    (helix-find-till-char ?d)
    (should (eql (point) 12))))

(ert-deftest helix-test-find-till-char-repeat ()
  "Test find char go to char and select to it."
  (with-temp-buffer
    (insert "first second third")
    (goto-char 1)
    (helix-find-till-char ?d)
    (helix-find-repeat)
    (should (eql (point) 18))))

(ert-deftest helix-test-find-next-char-repeat ()
  "Test find char go to char and select to it."
  (with-temp-buffer
    (insert "first second third")
    (goto-char 1)
    (helix-find-next-char ?d)
    (helix-find-repeat)
    (should (eql (point) 19))))

(ert-deftest helix-test-empty-find-repeat ()
  "Test find repeat when nothing to repeat."
  (with-temp-buffer
    (insert "first second third")
    (goto-char 1)
    (helix-find-repeat)
    (should (eql (point) 1))))

(provide 'helix-test)
;;; helix-test.el ends here
