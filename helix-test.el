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

(ert-deftest helix-test-forward-long-word-start-basic-movement ()
  "Test basic forward movement between words."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char 1)
    (helix-forward-long-word-start)
    (should (= (point) 6)) ; before "world"
    (should (= (- (region-end) (region-beginning)) 5))
    (helix-forward-long-word-start)
    (should (= (point) 12)) ; before "test"
    (should (= (- (region-end) (region-beginning)) 5))
    (helix-forward-long-word-start)
    (should (= (point) (- (point-max) 1))) ; before end of line
    (should (= (- (region-end) (region-beginning)) 3))))

(ert-deftest helix-test-forward-long-word-start-hyphenated-words ()
  "Test forward movement with hyphenated words (long words)."
  (with-temp-buffer
    (insert "this test-string-example works")
    (goto-char 1)
    (helix-forward-long-word-start)
    (should (= (point) 5)) ; before "test-string-example"
    (should (= (- (region-end) (region-beginning)) 4))
    (helix-forward-long-word-start)
    (should (= (point) 25)) ; before "works"
    (should (= (- (region-end) (region-beginning)) 19))))

(ert-deftest helix-test-forward-long-word-start-on-whitespace ()
  "Test that forward movement skips over whitespace."
  (with-temp-buffer
    (insert "word next")
    (goto-char 5) ; on the first whitespace
    (helix-forward-long-word-start)
    (should (= (point) (- (point-max) 1))) ; before end of line
    (should (= (- (region-end) (region-beginning)) 3))))

(ert-deftest helix-test-forward-long-word-start-on-whitespaces ()
  "Test that forward movement skips over whitespace."
  (with-temp-buffer
    (insert "word   \t  next")
    (goto-char 5) ; on the first whitespace
    (helix-forward-long-word-start)
    (should (= (point) 10)) ; start of "next"
    (should (= (- (region-end) (region-beginning)) 5))))

(ert-deftest helix-test-forward-long-word-start-multiple-lines ()
  "Test forward movement across multiple lines."
  (with-temp-buffer
    (insert "first line\nsecond line\nthird")
    (goto-char 1) ; start of buffer
    (helix-forward-long-word-start)
    (should (= (point) 6)) ; before "line"
    (should (= (- (region-end) (region-beginning)) 5))
    (helix-forward-long-word-start)
    (should (= (point) 10)) ; before end of first line
    (should (= (- (region-end) (region-beginning)) 3))
    (helix-forward-long-word-start)
    (should (= (point) 18)) ; before "line" of second line
    (should (= (- (region-end) (region-beginning)) 6))))

(ert-deftest helix-test-forward-long-word-start-empty-lines ()
  "Test forward movement with empty lines."
  (with-temp-buffer
    (insert "first\n\n\nsecond")
    (goto-char 5) ; before end of first line
    (helix-forward-long-word-start)
    (should (= (point) (- (point-max) 1))) ; before end of second line
    (should (= (- (region-end) (region-beginning)) 5))))

(ert-deftest helix-test-forward-long-word-start-at-end-of-buffer ()
  "Test that forward movement at end of buffer doesn't move."
  (with-temp-buffer
    (insert "test word")
    (goto-char (point-max))
    (let ((initial-point (point)))
      (helix-forward-long-word-start)
      (should (= (point) initial-point))
      (should (not (use-region-p))))))

(ert-deftest helix-test-forward-long-word-start-mixed-separators ()
  "Test forward movement with mixed word separators."
  (with-temp-buffer
    (insert "word1_part2-part3.part4 next")
    (goto-char 1)
    (helix-forward-long-word-start)
    (should (= (point) 24)) ; before "next"
    (should (= (- (region-end) (region-beginning)) 23))))

(ert-deftest helix-test-forward-long-word-start-punctuation ()
  "Test forward movement with punctuation."
  (with-temp-buffer
    (insert "Hello, world! How are you?")
    (goto-char 1)
    (helix-forward-long-word-start)
    (should (= (point) 7)) ; before "world!"
    (should (= (- (region-end) (region-beginning)) 6))
    (helix-forward-long-word-start)
    (should (= (point) 14)) ; before "How"
    (should (= (- (region-end) (region-beginning)) 6))))

;;; Forward long word end tests

(ert-deftest helix-test-forward-long-word-end-basic-movement ()
  "Test basic forward movement to word ends."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char 1)
    (helix-forward-long-word-end)
    (should (= (point) 5)) ; end of "hello"
    (should (= (- (region-end) (region-beginning)) 4))
    (helix-forward-long-word-end)
    (should (= (point) 11)) ; end of "world"
    (should (= (- (region-end) (region-beginning)) 5))
    (helix-forward-long-word-end)
    (should (= (point) 16)) ; end of "test"
    (should (= (- (region-end) (region-beginning)) 4))))

(ert-deftest helix-test-forward-long-word-end-hyphenated-words ()
  "Test forward movement to ends of hyphenated words (long words)."
  (with-temp-buffer
    (insert "this test-string-example works")
    (goto-char 1)
    (helix-forward-long-word-end)
    (should (= (point) 4)) ; end of "this"
    (should (= (- (region-end) (region-beginning)) 3))
    (helix-forward-long-word-end)
    (should (= (point) 24)) ; end of "test-string-example"
    (should (= (- (region-end) (region-beginning)) 19))
    (helix-forward-long-word-end)
    (should (= (point) 30)) ; end of "works"
    (should (= (- (region-end) (region-beginning)) 5))))

(ert-deftest helix-test-forward-long-word-end-on-whitespace ()
  "Test that forward movement to word ends skips over whitespace."
  (with-temp-buffer
    (insert "word next")
    (goto-char 5) ; on the first whitespace
    (helix-forward-long-word-end)
    (should (= (point) 9)) ; end of "next"
    (should (= (- (region-end) (region-beginning)) 4))))

(ert-deftest helix-test-forward-long-word-end-on-whitespaces ()
  "Test that forward movement to word ends skips over multiple whitespaces."
  (with-temp-buffer
    (insert "word   \t  next")
    (goto-char 5) ; on the first whitespace
    (helix-forward-long-word-end)
    (should (= (point) 14)) ; end of "next"
    (should (= (- (region-end) (region-beginning)) 9))))

(ert-deftest helix-test-forward-long-word-end-multiple-lines ()
  "Test forward movement to word ends across multiple lines."
  (with-temp-buffer
    (insert "first line\nsecond line\nthird")
    (goto-char 1) ; start of buffer
    (helix-forward-long-word-end)
    (should (= (point) 5)) ; end of "first"
    (should (= (- (region-end) (region-beginning)) 4))
    (helix-forward-long-word-end)
    (should (= (point) 10)) ; end of "line"
    (should (= (- (region-end) (region-beginning)) 4))
    (helix-forward-long-word-end)
    (should (= (point) 17)) ; end of "second"
    (should (= (- (region-end) (region-beginning)) 5))))

(ert-deftest helix-test-forward-long-word-end-empty-lines ()
  "Test forward movement to word ends with empty lines."
  (with-temp-buffer
    (insert "first\n\n\nsecond")
    (goto-char 1)
    (helix-forward-long-word-end)
    (should (= (point) 5)) ; end of "first"
    (should (= (- (region-end) (region-beginning)) 4))
    (helix-forward-long-word-end)
    (should (= (point) 14)) ; end of "second"
    (should (= (- (region-end) (region-beginning)) 5))))

(ert-deftest helix-test-forward-long-word-end-at-end-of-buffer ()
  "Test that forward movement to word end at end of buffer doesn't move."
  (with-temp-buffer
    (insert "test word")
    (goto-char (point-max))
    (let ((initial-point (point)))
      (helix-forward-long-word-end)
      (should (= (point) initial-point))
      (should (not (use-region-p))))))

(ert-deftest helix-test-forward-long-word-end-mixed-separators ()
  "Test forward movement to word ends with mixed word separators."
  (with-temp-buffer
    (insert "word1_part2-part3.part4 next")
    (goto-char 1)
    (helix-forward-long-word-end)
    (should (= (point) 23)) ; end of "word1_part2-part3.part4"
    (should (= (- (region-end) (region-beginning)) 22))
    (helix-forward-long-word-end)
    (should (= (point) 28)) ; end of "next"
    (should (= (- (region-end) (region-beginning)) 4))))

(ert-deftest helix-test-forward-long-word-end-punctuation ()
  "Test forward movement to word ends with punctuation."
  (with-temp-buffer
    (insert "Hello, world! How are you?")
    (goto-char 1)
    (helix-forward-long-word-end)
    (should (= (point) 6)) ; end of "Hello,"
    (should (= (- (region-end) (region-beginning)) 5))
    (helix-forward-long-word-end)
    (should (= (point) 13)) ; end of "world!"
    (should (= (- (region-end) (region-beginning)) 6))
    (helix-forward-long-word-end)
    (should (= (point) 17)) ; end of "How"
    (should (= (- (region-end) (region-beginning)) 3))))

(ert-deftest helix-test-forward-long-word-end-empty-buffer ()
  "Test forward movement to word end in empty buffer."
  (with-temp-buffer
    (let ((initial-point (point)))
      (helix-forward-long-word-end)
      (should (= (point) initial-point))
      (should (not (use-region-p))))))

;;; Backward long word tests

(ert-deftest helix-test-backward-long-word-basic-movement ()
  "Test basic backward movement between words."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 13)) ; start of "test"
    (should (= (- (region-end) (region-beginning)) 4))
    (helix-backward-long-word)
    (should (= (point) 7)) ; start of "world"
    (should (= (- (region-end) (region-beginning)) 6))
    (helix-backward-long-word)
    (should (= (point) 1)) ; start of "hello"
    (should (= (- (region-end) (region-beginning)) 6))))

(ert-deftest helix-test-backward-long-word-hyphenated-words ()
  "Test backward movement with hyphenated words (long words)."
  (with-temp-buffer
    (insert "this test-string-example works")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 26)) ; start of "works"
    (should (= (- (region-end) (region-beginning)) 5))
    (helix-backward-long-word)
    (should (= (point) 6)) ; start of "test-string-example"
    (should (= (- (region-end) (region-beginning)) 20))))

(ert-deftest helix-test-backward-long-word-on-whitespace ()
  "Test that backward movement skips over whitespace."
  (with-temp-buffer
    (insert "word next")
    (goto-char 5) ; on whitespace
    (helix-backward-long-word)
    (should (= (point) 1)) ; start of "word"
    (should (= (- (region-end) (region-beginning)) 4))))

(ert-deftest helix-test-backward-long-word-on-whitespaces ()
  "Test that backward movement skips over whitespace."
  (with-temp-buffer
    (insert "word   \t  next")
    (goto-char 10) ; on the last whitespace
    (helix-backward-long-word)
    (should (= (point) 1)) ; start of "word"
    (should (= (- (region-end) (region-beginning)) 9))))

(ert-deftest helix-test-backward-long-word-multiple-lines ()
  "Test backward movement across multiple lines."
  (with-temp-buffer
    (insert "first line\nsecond line\nthird")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 24)) ; start of "third"
    (should (= (- (region-end) (region-beginning)) 5))
    (helix-backward-long-word)
    (should (= (point) 19)) ; start of "line"
    (should (= (- (region-end) (region-beginning)) 4))
    (helix-backward-long-word)
    (should (= (point) 12)))) ; start of "second"

(ert-deftest helix-test-backward-long-word-empty-lines ()
  "Test backward movement with empty lines."
  (with-temp-buffer
    (insert "first\n\n\nsecond")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 9)) ; start of "second"
    (should (= (- (region-end) (region-beginning)) 6))
    (helix-backward-long-word)
    (should (= (point) 1)) ; start of "first"
    (should (= (- (region-end) (region-beginning)) 5))))

(ert-deftest helix-test-backward-long-word-at-beginning-of-buffer ()
  "Test that backward movement at beginning of buffer doesn't move."
  (with-temp-buffer
    (insert "test word")
    (goto-char 1)
    (let ((initial-point (point)))
      (helix-backward-long-word)
      (should (= (point) initial-point))
      (should (not (use-region-p))))))

(ert-deftest helix-test-backward-long-word-mixed-separators ()
  "Test backward movement with mixed word separators."
  (with-temp-buffer
    (insert "word1_part2-part3.part4 next")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 25)) ; start of "next"
    (should (= (- (region-end) (region-beginning)) 4))
    (helix-backward-long-word)
    (should (= (point) 1)) ; start of "word1_part2-part3.part4"
    (should (= (- (region-end) (region-beginning)) 24))))

(ert-deftest helix-test-backward-long-word-punctuation ()
  "Test backward movement with punctuation."
  (with-temp-buffer
    (insert "Hello, world! How are you?")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 23)) ; start of "you?"
    (should (= (- (region-end) (region-beginning)) 4))
    (helix-backward-long-word)
    (should (= (point) 19)) ; start of "are"
    (should (= (- (region-end) (region-beginning)) 4))))

;;; Edge case tests

(ert-deftest helix-test-forward-long-word-empty-buffer ()
  "Test forward movement in empty buffer."
  (with-temp-buffer
    (let ((initial-point (point)))
      (helix-forward-long-word-start)
      (should (= (point) initial-point))
      (should (not (use-region-p))))))

(ert-deftest helix-test-backward-long-word-empty-buffer ()
  "Test backward movement in empty buffer."
  (with-temp-buffer
    (let ((initial-point (point)))
      (helix-backward-long-word)
      (should (= (point) initial-point))
      (should (not (use-region-p))))))

(ert-deftest helix-test-forward-long-word-start-only-whitespace ()
  "Test forward movement in buffer with only whitespace."
  (with-temp-buffer
    (insert "   \t\n  ")
    (goto-char 1)
    (helix-forward-long-word-start)
    (should (= (point) 4)) ; before end of first line
    (should (= (- (region-end) (region-beginning)) 3))))

(ert-deftest helix-test-forward-long-word-end-only-whitespace ()
  "Test forward movement to word end in buffer with only whitespace."
  (with-temp-buffer
    (insert "   \t\n  ")
    (goto-char 1)
    (helix-forward-long-word-end)
    (should (= (point) 4)) ; end of first line whitespace
    (should (= (- (region-end) (region-beginning)) 3))
    (helix-forward-long-word-end)
    (should (= (point) 7)) ; end of buffer
    (should (= (- (region-end) (region-beginning)) 1))))

(ert-deftest helix-test-backward-long-word-only-whitespace ()
  "Test backward movement in buffer with only whitespace."
  (with-temp-buffer
    (insert "   \t\n  ")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 6)) ; start of second line
    (should (= (- (region-end) (region-beginning) 2)))))

(ert-deftest helix-test-forward-long-word-start-single-character ()
  "Test forward movement with single character words."
  (with-temp-buffer
    (insert "a b c d")
    (goto-char 1)
    (helix-forward-long-word-start)
    (should (= (point) 2)) ; before "b"
    (should (= (- (region-end) (region-beginning)) 1))
    (helix-forward-long-word-start)
    (should (= (point) 4)) ; before "c"
    (should (= (- (region-end) (region-beginning)) 1))))

(ert-deftest helix-test-forward-long-word-end-single-character ()
  "Test forward movement to word ends with single character words."
  (with-temp-buffer
    (insert "a b c d")
    (goto-char 1)
    (helix-forward-long-word-end)
    (should (= (point) 3)) ; end of "b"
    (should (= (- (region-end) (region-beginning)) 1))
    (helix-forward-long-word-end)
    (should (= (point) 5)) ; end of "c"
    (should (= (- (region-end) (region-beginning)) 1))
    (helix-forward-long-word-end)
    (should (= (point) 7)) ; end of "d"
    (should (= (- (region-end) (region-beginning)) 1))))

(ert-deftest helix-test-backward-long-word-single-character ()
  "Test backward movement with single character words."
  (with-temp-buffer
    (insert "a b c d")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 7)) ; start of "d"
    (should (= (- (region-end) (region-beginning)) 1))
    (helix-backward-long-word)
    (should (= (point) 5)) ; start of "c"
    (should (= (- (region-end) (region-beginning)) 2))))

;; Find char

(ert-deftest helix-test-find-next-char ()
  "Test finding next character and selecting from current position to it."
  (with-temp-buffer
    (insert "first second third")
    (goto-char 1)
    (helix-find-next-char ?d)
    (should (eql (point) 13))
    (should (eql (- (region-end) (region-beginning)) 12))))

(ert-deftest helix-test-find-next-char-two-line ()
  "Test finding next character across multiple lines and selecting to it."
  (with-temp-buffer
    (insert "first\nsecond\nthird")
    (goto-char 1)
    (helix-find-next-char ?d)
    (should (eql (point) 13))
    (should (eql (- (region-end) (region-beginning)) 12))))

(ert-deftest helix-test-find-till-char ()
  "Test finding till character and selecting from current position to before it."
  (with-temp-buffer
    (insert "first second third")
    (goto-char 1)
    (helix-find-till-char ?d)
    (should (eql (point) 12))
    (should (eql (- (region-end) (region-beginning)) 11))))

(ert-deftest helix-test-find-till-char-two-line ()
  "Test finding till character across multiple lines and selecting to before it."
  (with-temp-buffer
    (insert "first\nsecond\nthird")
    (goto-char 1)
    (helix-find-till-char ?d)
    (should (eql (point) 12))
    (should (eql (- (region-end) (region-beginning)) 11))))

(ert-deftest helix-test-find-till-char-repeat ()
  "Test repeating find till character operation to next occurrence."
  (with-temp-buffer
    (insert "first second third")
    (goto-char 1)
    (helix-find-till-char ?d)
    (helix-find-repeat)
    (should (eql (point) 18))
    (should (eql (- (region-end) (region-beginning)) 5))))

(ert-deftest helix-test-find-next-char-repeat ()
  "Test repeating find next character operation to next occurrence."
  (with-temp-buffer
    (insert "first second third")
    (goto-char 1)
    (helix-find-next-char ?d)
    (helix-find-repeat)
    (should (eql (point) 19))
    (should (eql (- (region-end) (region-beginning)) 6))))

(ert-deftest helix-test-find-prev-char ()
  "Test finding previous character and selecting to it."
  (with-temp-buffer
    (insert "first second third")
    (goto-char (point-max))
    (helix-find-prev-char ?s)
    (should (eql (point) 7))
    (should (eql (- (region-end) (region-beginning)) 12))))

(ert-deftest helix-test-find-prev-char-multiple-lines ()
  "Test finding previous character across multiple lines and selecting to it."
  (with-temp-buffer
    (insert "first\nsecond\nthird")
    (goto-char (point-max))
    (helix-find-prev-char ?s)
    (should (eql (point) 7))
    (should (eql (- (region-end) (region-beginning)) 12))))

(ert-deftest helix-test-find-prev-till-char ()
  "Test finding previous character and selecting till (after) it."
  (with-temp-buffer
    (insert "first second third")
    (goto-char (point-max))
    (helix-find-prev-till-char ?s)
    (should (eql (point) 8))
    (should (eql (- (region-end) (region-beginning)) 11))))

(ert-deftest helix-test-find-prev-till-char-multiple-lines ()
  "Test finding previous character across multiple lines and selecting till (after) it."
  (with-temp-buffer
    (insert "first\nsecond\nthird")
    (goto-char (point-max))
    (helix-find-prev-till-char ?s)
    (should (eql (point) 8))
    (should (eql (- (region-end) (region-beginning)) 11))))

(ert-deftest helix-test-find-prev-char-repeat ()
  "Test repeating previous character find operation and extending selection."
  (with-temp-buffer
    (insert "first second third")
    (goto-char (point-max))
    (helix-find-prev-char ?s)
    (helix-find-repeat)
    (should (eql (point) 4))
    (should (eql (- (region-end) (region-beginning)) 3))))

(ert-deftest helix-test-find-prev-till-char-repeat ()
  "Test repeating previous till character find operation and extending selection."
  (with-temp-buffer
    (insert "first second third")
    (goto-char (point-max))
    (helix-find-prev-till-char ?s)
    (helix-find-repeat)
    (should (eql (point) 5))
    (should (eql (- (region-end) (region-beginning)) 2))))

(ert-deftest helix-test-empty-find-repeat ()
  "Test find repeat when nothing to repeat."
  (with-temp-buffer
    (insert "first second third")
    (goto-char 1)
    (helix-find-repeat)
    (should (eql (point) 1))))

;;; helix-define-key tests

(ert-deftest helix-test-define-key-standard ()
  "Test standard helix-define-key without optional keymap."
  (let ((original-binding (lookup-key helix-space-map "t")))
    (unwind-protect
        (progn
          (helix-define-key 'space "t" #'ignore)
          (should (eq (lookup-key helix-space-map "t") #'ignore)))
      (define-key helix-space-map "t" original-binding))))

(ert-deftest helix-test-define-key-with-mode ()
  "Test helix-define-key with MODE stores binding in helix--mode-keybindings."
  (let ((helix--mode-keybindings nil))
    (helix-define-key 'normal "j" #'next-line 'dired-mode)
    (let ((entry (assoc (cons 'dired-mode 'normal) helix--mode-keybindings)))
      (should entry)
      (should (eq (lookup-key (cdr entry) "j") #'next-line)))))

(ert-deftest helix-test-define-key-with-mode-multiple-bindings ()
  "Test that multiple bindings for the same mode and state accumulate."
  (let ((helix--mode-keybindings nil))
    (helix-define-key 'normal "j" #'next-line 'dired-mode)
    (helix-define-key 'normal "k" #'previous-line 'dired-mode)
    (let ((entry (assoc (cons 'dired-mode 'normal) helix--mode-keybindings)))
      (should (eq (lookup-key (cdr entry) "j") #'next-line))
      (should (eq (lookup-key (cdr entry) "k") #'previous-line)))))

(ert-deftest helix-test-define-key-with-mode-different-states ()
  "Test that different states get different sparse keymaps."
  (let ((helix--mode-keybindings nil))
    (helix-define-key 'normal "j" #'next-line 'dired-mode)
    (helix-define-key 'insert "j" #'self-insert-command 'dired-mode)
    (let ((normal-entry (assoc (cons 'dired-mode 'normal) helix--mode-keybindings))
          (insert-entry (assoc (cons 'dired-mode 'insert) helix--mode-keybindings)))
      (should normal-entry)
      (should insert-entry)
      (should-not (eq (cdr normal-entry) (cdr insert-entry)))
      (should (eq (lookup-key (cdr normal-entry) "j") #'next-line))
      (should (eq (lookup-key (cdr insert-entry) "j") #'self-insert-command)))))

(ert-deftest helix-test-define-key-invalid-state ()
  "Test that invalid state signals an error."
  (should-error (helix-define-key 'invalid-state "t" #'ignore)))

(ert-deftest helix-test-define-key-invalid-state-with-mode ()
  "Test that invalid state signals error even with explicit mode."
  (should-error (helix-define-key 'invalid-state "t" #'ignore 'dired-mode)))

;;; helix--refresh-overriding-maps tests

(ert-deftest helix-test-refresh-overriding-maps-with-major-mode-bindings ()
  "Test that refresh builds correct minor-mode-overriding-map-alist."
  (let ((helix--mode-keybindings nil))
    (with-temp-buffer
      ;; Simulate a major mode
      (setq major-mode 'helix-test-mode)
      (setq-local helix--current-state 'normal)
      ;; Register a binding for this mode
      (helix-define-key 'normal "j" #'next-line 'helix-test-mode)
      (helix--refresh-overriding-maps)
      ;; Should have an entry in minor-mode-overriding-map-alist
      (let ((entry (assq 'helix-normal-mode minor-mode-overriding-map-alist)))
        (should entry)
        (should (eq (lookup-key (cdr entry) "j") #'next-line))))))

(ert-deftest helix-test-refresh-overriding-maps-clears-when-no-bindings ()
  "Test that refresh clears overriding alist when no bindings apply."
  (let ((helix--mode-keybindings nil))
    (with-temp-buffer
      (setq-local helix--current-state 'normal)
      ;; Pre-populate with a stale entry
      (setq minor-mode-overriding-map-alist
            (list (cons 'helix-normal-mode (make-sparse-keymap))))
      (helix--refresh-overriding-maps)
      ;; Should have cleared the entry
      (should-not (assq 'helix-normal-mode minor-mode-overriding-map-alist)))))

(ert-deftest helix-test-refresh-overriding-maps-no-cross-mode-leak ()
  "Test that bindings for one major mode don't leak into another."
  (let ((helix--mode-keybindings nil))
    (with-temp-buffer
      ;; Register binding for dired-mode
      (helix-define-key 'normal "j" #'next-line 'dired-mode)
      ;; But current buffer is a different major mode
      (setq major-mode 'fundamental-mode)
      (setq-local helix--current-state 'normal)
      (helix--refresh-overriding-maps)
      ;; Should have no overriding entry
      (should-not (assq 'helix-normal-mode minor-mode-overriding-map-alist)))))

(ert-deftest helix-test-refresh-overriding-maps-fallback-to-base ()
  "Test that non-overridden keys fall back to base helix keymap."
  (let ((helix--mode-keybindings nil))
    (with-temp-buffer
      (setq major-mode 'helix-test-mode)
      (setq-local helix--current-state 'normal)
      ;; Override only "j"
      (helix-define-key 'normal "j" #'next-line 'helix-test-mode)
      (helix--refresh-overriding-maps)
      (let ((entry (assq 'helix-normal-mode minor-mode-overriding-map-alist)))
        (should entry)
        ;; Overridden key works
        (should (eq (lookup-key (cdr entry) "j") #'next-line))
        ;; Non-overridden key falls back to helix binding
        (should (eq (lookup-key (cdr entry) "k") #'helix-previous-line))))))

(provide 'helix-test)
;;; helix-test.el ends here
