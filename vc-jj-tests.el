;;; vc-jj-tests.el --- tests for vc-jj.el            -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Rudolf Schlatte

;; Author: Rudolf Schlatte <rudi@constantly.at>

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

;;

;;; Code:

(require 'ert-x)
(require 'vc)
(require 'vc-dir)
(require 'vc-jj)
(require 'iso8601)
(require 'cl-lib)
(require 'thingatpt)

(defun vc-jj-test-environment (seq)
  "Create a list suitable for prepending to `process-environment'.
The purpose is to make tests reproducible by fixing timestamps,
change ids, author information etc. SEQ is an integer that
modifies the JJ_RANDOMNESS_SEED, JJ_TIMESTAMP and JJ_OP_TIMESTAMP
environment variables. Increasing values for SEQ will result in
increasing timestamps.

Note that it not necessary to use this function, except when
stably increasing timestamps and stable change ids across test
runs are necessary."
  ;; For other potentially relevant variables, see
  ;; https://github.com/jj-vcs/jj/blob/d79c7a0dd5b8f9d3d6f9436452dcf0e1600b0b14/cli/tests/common/test_environment.rs#L115
  (let* ((startdate (iso8601-parse "2001-02-03T04:05:06+07:00"))
         (timezone (cl-ninth startdate))
         (offset (time-add (encode-time startdate) seq))
         (timestring (format-time-string "%FT%T%:z" offset timezone)))
    (list "JJ_EMAIL=john@example.com"
          "JJ_USER=john"
          (format "JJ_RANDOMNESS_SEED=%i" (+ 12345 seq))
          (format "JJ_TIMESTAMP=%s" timestring)
          (format "JJ_OP_TIMESTAMP=%s" timestring))))

(defmacro vc-jj-test-with-repo (name &rest body)
  "Initialize a repository in a temporary directory and evaluate BODY.
The current directory will be set to the top of that repository;
NAME will be bound to that directory's file name.  Once BODY
exits, the directory will be deleted.

jj commands are executed with a fixed username and email; augment
`process-environment' with `vc-jj-test-environment' if control
over timestamps and random number seed (and thereby change ids)
is needed."
  (declare (indent 1))
  `(ert-with-temp-directory ,name
     (let ((default-directory ,name)
           (process-environment
            (append (list "JJ_EMAIL=john@example.com" "JJ_USER=john")
                    process-environment)))
       (let ((process-environment
              (append (vc-jj-test-environment 0) process-environment)))
         (vc-create-repo 'jj))
       ,@body)))

(ert-deftest vc-jj-test-add-file ()
  (vc-jj-test-with-repo repo
    (write-region "New file" nil "README")
    (should (vc-jj--file-tracked "README"))
    (should (vc-jj--file-added "README"))
    (should (not (vc-jj--file-modified "README")))
    (should (not (vc-jj--file-conflicted "README")))
    (should (eq (vc-state "README" 'jj) 'added))))

(ert-deftest vc-jj-test-added-tracked ()
  (vc-jj-test-with-repo repo
    (write-region "In first commit" nil "first-file")
    (vc-jj-checkin '("first-file") "First commit")
    (write-region "In second commit" nil "second-file")
    (should (eq (vc-jj-state "second-file") 'added))
    (should (eq (vc-jj-state "first-file") 'up-to-date))))

(ert-deftest vc-jj-test-conflict ()
  (vc-jj-test-with-repo repo
    (let (branch-1 branch-2 branch-merged)
      ;; the root change id is always zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
      (shell-command "jj new zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz")
      (setq branch-1 (vc-jj-working-revision "."))
      (write-region "Unconflicted" nil "unconflicted.txt")
      (write-region "Branch 1" nil "conflicted.txt")
      (make-directory "subdir")
      (write-region "Branch 1" nil "subdir/conflicted.txt")
      (shell-command "jj new zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz")
      (setq branch-2 (vc-jj-working-revision "."))
      (write-region "Unconflicted" nil "unconflicted.txt")
      (write-region "Branch 2" nil "conflicted.txt")
      (make-directory "subdir")
      (write-region "Branch 2" nil "subdir/conflicted.txt")
      (shell-command (concat "jj new " branch-1 " " branch-2))
      (should (eq (vc-jj-state "unconflicted.txt") 'up-to-date))
      (should (eq (vc-jj-state "conflicted.txt") 'conflict))
      (should (eq (vc-jj-state "subdir/conflicted.txt") 'conflict)))))

(ert-deftest vc-jj-test-annotate ()
  (vc-jj-test-with-repo repo
    (let ( change-1 change-2
           readme-buffer annotation-buffer)
      ;; Create two changes, make sure that the change ids in the
      ;; annotation buffer match.  This test is supposed to detect
      ;; changes in the output format of `jj annotate'.
      (write-region "Line 1\n" nil "README")
      (setq change-1 (vc-jj-working-revision "README"))
      (shell-command "jj commit -m 'First change'")
      (write-region "Line 2\n" nil "README" t)
      (shell-command "jj describe -m 'Second change'")
      (setq change-2 (vc-jj-working-revision "README"))
      (find-file "README")
      (setq readme-buffer (current-buffer))
      (vc-annotate "README" change-2)
      (setq annotation-buffer (current-buffer))
      (goto-char (point-min))
      (should (string-prefix-p (thing-at-point 'word) change-1))
      (forward-line)
      (should (string-prefix-p (thing-at-point 'word) change-2))
      (kill-buffer readme-buffer)
      (kill-buffer annotation-buffer))))

(provide 'vc-jj-tests)
;;; vc-jj-tests.el ends here
