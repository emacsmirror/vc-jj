;;; vc-jj-tests.el --- Tests for vc-jj.el            -*- lexical-binding: t; -*-

;; Copyright (C) 2025, 2026  Free Software Foundation, Inc.

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

;; Test coverage for vc-jj.el.

;;; Code:

(require 'ert-x)
(require 'vc)
(require 'vc-dir)
(require 'vc-jj)
(require 'iso8601)
(require 'cl-lib)
(require 'thingatpt)

;;;; Testing suite helpers

(defconst vc-jj-test--environment-email "JJ_EMAIL=john@example.com"
  "Value for JJ_EMAIL environment variable.
This is meant to be prepended to `process-environment'.  See
`vc-jj-test--environment' and `vc-jj-test--with-repo'.")

(defconst vc-jj-test--environment-user "JJ_USER=john"
  "Value for JJ_USER environment variable.
This is meant to be prepended to `process-environment'.  See
`vc-jj-test--environment' and `vc-jj-test--with-repo'.")

(defvar vc-jj-test--repo-colocate nil
  "Whether to colocate jj test repository.
When this variable is non-nil, jj repositories initialized in
`vc-jj-test--with-repo' are colocated.  When nil, they are not
colocated.

This variable is meant to be let-bound in tests that need a colocated
repository.  By default, repositories are not colocated.")

(defun vc-jj-test--environment (seq)
  "Create a list suitable for prepending to `process-environment'.
The purpose is to make tests reproducible by fixing timestamps, change
IDs, author information, etc.

Note that it not necessary to use this function, except when stably
increasing timestamps and stable change ids across test runs are
necessary.

SEQ is an integer that modifies the JJ_RANDOMNESS_SEED, JJ_TIMESTAMP and
JJ_OP_TIMESTAMP environment variables.  Increasing values for SEQ will
result in increasing timestamps."
  ;; For other potentially relevant variables, see
  ;; https://github.com/jj-vcs/jj/blob/d79c7a0dd5b8f9d3d6f9436452dcf0e1600b0b14/cli/tests/common/test_environment.rs#L115
  (let* ((startdate (iso8601-parse "2001-02-03T04:05:06+07:00"))
         (timezone (cl-ninth startdate))
         (offset (time-add (encode-time startdate) seq))
         (timestring (format-time-string "%FT%T%:z" offset timezone)))
    (list vc-jj-test--environment-email
          vc-jj-test--environment-user
          (format "JJ_RANDOMNESS_SEED=%i" (+ 12345 seq))
          (format "JJ_TIMESTAMP=%s" timestring)
          (format "JJ_OP_TIMESTAMP=%s" timestring))))

(defmacro vc-jj-test--with-repo (name &rest body)
  "Initialize a repository in a temporary directory and evaluate BODY.
The current directory will be set to the root of that repository.  NAME
is a symbol that will be bound to that repository\\='s path.  Once BODY
exits, the directory will be deleted.

Jujutsu commands are executed with a fixed username and email; augment
`process-environment' with `vc-jj-test--environment' if control over
timestamps and random number seed (and thereby change ids) is needed."
  (declare (indent 1) (debug (symbolp body)))
  `(ert-with-temp-directory ,name
     (skip-unless (executable-find vc-jj-program))
     (let ((default-directory ,name)
           (process-environment
            (append (list vc-jj-test--environment-email
                          vc-jj-test--environment-user)
                    process-environment)))
       ;; Append the `vc-jj-test--environment' to
       ;; `process-environment' only when creating the repository, not
       ;; when evaluating BODY
       (let ((process-environment
              (append (vc-jj-test--environment 0) process-environment)))
         (shell-command (format "jj git init %s"
                                (if vc-jj-test--repo-colocate
                                    "--colocate"
                                  "--no-colocate"))))
       (let ((default-directory ,name)
             ;; On macOS, the generated filename "/var/folders/..."
             ;; was in reality "/private/var/folders/...", which got
             ;; unfolded by `vc-jj-root' within some tests -- do this
             ;; here already
             (,name (file-truename (vc-jj-root ,name))))
         ,@body))))

;;;; Check if jujutsu's version number is parseable

(ert-deftest vc-jj-test-jj-version ()
  "Test that we can parse jj's version info."
  (skip-unless (executable-find vc-jj-program))
  (version< "0.0.1" (vc-jj--program-version)))

;;;; State-querying tests

(defun vc-jj-test--dir-status-files-update-function (files-and-states-and-extra _partial-results-p)
  "Simple callback function for `vc-jj-dir-status-files'.
Returns FILES-AND-STATES-AND-EXTRA."
  files-and-states-and-extra)

(ert-deftest vc-jj-test-state-ignored ()
  "Test the \"ignored\" VC state.
Check the correctness of \"ignored\" file state reported by
`vc-jj-state' and `vc-jj-dir-status-files'."
  (vc-jj-test--with-repo repo
    (write-region "file1" nil ".gitignore")
    (write-region "" nil "file1")
    (should (eq (vc-jj-state "file1") 'ignored))
    (should (seq-set-equal-p
             (vc-jj-dir-status-files repo '("file1") #'vc-jj-test--dir-status-files-update-function)
             '(("file1" ignored nil))))))

(ert-deftest vc-jj-test-state-added ()
  "Test the \"added\" VC state.
Check the correctness of \"added\" file state reported by
`vc-jj-state' and `vc-jj-dir-status-files'."
  (vc-jj-test--with-repo repo
    (write-region "" nil "file1")
    (should (eq (vc-jj-state "file1") 'added))
    (should (seq-set-equal-p
             (vc-jj-dir-status-files repo '("file1") #'vc-jj-test--dir-status-files-update-function)
             '(("file1" added nil))))))

(ert-deftest vc-jj-test-state-edited ()
  "Test the \"edited\" VC state.
Check the correctness of \"edited\" file state reported by
`vc-jj-state' and `vc-jj-dir-status-files'."
  (vc-jj-test--with-repo repo
    (write-region "" nil "file1")
    (should (eq (vc-jj-state "file1") 'added))
    (should (seq-set-equal-p
             (vc-jj-dir-status-files repo '("file1") #'vc-jj-test--dir-status-files-update-function)
             '(("file1" added nil))))))

(ert-deftest vc-jj-test-state-up-to-date ()
  "Test the \"up-to-date\" VC state.
Check the correctness of \"up-to-date\" file state reported by
`vc-jj-state' and `vc-jj-dir-status-files'."
  (vc-jj-test--with-repo repo
    (write-region "" nil "file1")
    (should (eq (vc-jj-state "file1") 'added))
    (shell-command "jj new")
    (should (eq (vc-jj-state "file1") 'up-to-date))
    (should (seq-set-equal-p
             (vc-jj-dir-status-files repo '("file1") #'vc-jj-test--dir-status-files-update-function)
             '(("file1" up-to-date nil))))))

(ert-deftest vc-jj-test-state-removed ()
  "Test \"removed\" VC state.
Check the correctness of \"removed\" file state reported by
`vc-jj-state' and `vc-jj-dir-status-files'."
  (vc-jj-test--with-repo repo
    (write-region "" nil "file1")
    (should (eq (vc-jj-state "file1") 'added))
    (shell-command "jj new")
    (shell-command "rm file1")
    (should (eq (vc-jj-state "file1") 'removed))
    (should (seq-set-equal-p
             (vc-jj-dir-status-files repo '("file1") #'vc-jj-test--dir-status-files-update-function)
             '(("file1" removed nil))))))

(ert-deftest vc-jj-test-state-conflict ()
  "Test \"conflict\" VC state.
Check the correctness of \"conflict\" file state reported by
`vc-jj-state' and `vc-jj-dir-status-files'."
  (vc-jj-test--with-repo repo
    (write-region "some content" nil "file1")
    (should (eq (vc-jj-state "file1") 'added))
    (shell-command "jj new @-")
    (write-region "conflicting content" nil "file1")
    (should (eq (vc-jj-state "file1") 'added))
    (shell-command "jj new 'children(@-)'")
    (should (eq (vc-jj-state "file1") 'conflict))
    (should (seq-set-equal-p
             (vc-jj-dir-status-files repo '("file1") #'vc-jj-test--dir-status-files-update-function)
             '(("file1" conflict nil))))))

(ert-deftest vc-jj-test-state-later-ignored ()
  "Test when a tracked file is later ignored.
In the revision where a file that was previously tracked is ignored and
untracked, its state should be removed.  In the revisions after, its
state should be ignored."
  (vc-jj-test--with-repo repo
    (write-region "" nil "file1")
    (should (eq (vc-jj-state "file1") 'added))
    (shell-command "jj new")
    (shell-command "echo \"file1\" >> .gitignore")
    (shell-command "jj file untrack \"file1\"")
    (should (eq (vc-jj-state "file1") 'removed))
    (shell-command "jj new")
    (should (eq (vc-jj-state "file1") 'ignored))))

(ert-deftest vc-jj-test-state-mixed ()
  "Test `vc-jj-dir-status-files' against a variety of VC states."
  (vc-jj-test--with-repo repo
    (let (branch-1 branch-2)
      ;; Create branch 1
      (shell-command "jj new 'root()'")
      (write-region "" nil "unconflicted.txt")
      (write-region "Branch 1" nil "conflicted1.txt")
      (make-directory "subdir")
      (write-region "Branch 1" nil "subdir/conflicted2.txt")
      (setq branch-1 (vc-jj-working-revision "unconflicted.txt"))
      (should (seq-set-equal-p
               (vc-jj-dir-status-files repo
                                       '("unconflicted.txt"
                                         "conflicted1.txt"
                                         "subdir/conflicted2.txt")
                                       #'vc-jj-test--dir-status-files-update-function)
               '(("conflicted1.txt" added nil)
                 ("unconflicted.txt" added nil)
                 ("subdir/conflicted2.txt" added nil))))
      
      ;; Create second branch
      (shell-command "jj new 'root()'")
      (write-region "" nil "unconflicted.txt")
      (write-region "Branch 2" nil "conflicted1.txt")
      (make-directory "subdir")
      (write-region "Branch 2" nil "subdir/conflicted2.txt")
      (setq branch-2 (vc-jj-working-revision "unconflicted.txt"))
      (should (seq-set-equal-p
               (vc-jj-dir-status-files repo
                                       '("unconflicted.txt"
                                         "conflicted1.txt"
                                         "subdir/conflicted2.txt")
                                       #'vc-jj-test--dir-status-files-update-function)
               '(("conflicted1.txt" added nil)
                 ("unconflicted.txt" added nil)
                 ("subdir/conflicted2.txt" added nil))))
      
      ;; Create a merge commit between branch 1 and 2
      (shell-command (format "jj new %s %s" branch-1 branch-2))
      (write-region "" nil "added.txt")
      (should (seq-set-equal-p
               (vc-jj-dir-status-files repo
                                       '("conflicted1.txt"
                                         "subdir/conflicted2.txt"
                                         "added.txt"
                                         "unconflicted.txt")
                                       #'vc-jj-test--dir-status-files-update-function)
               '(("conflicted1.txt" conflict nil)
                 ("subdir/conflicted2.txt" conflict nil)
                 ("added.txt" added nil)
                 ("unconflicted.txt" up-to-date nil))))
      
      ;; Create a new commit, delete all files, then ignore a newly
      ;; added file
      (shell-command "jj new")
      (shell-command "rm conflicted1.txt subdir/conflicted2.txt added.txt unconflicted.txt")
      (shell-command "echo \"ignored.txt\" >> .gitignore")
      (write-region "" nil "ignored.txt")
      (should (seq-set-equal-p
               (vc-jj-dir-status-files repo
                                       '("conflicted1.txt"
                                         "subdir/conflicted2.txt"
                                         "added.txt"
                                         "unconflicted.txt"
                                         "ignored.txt")
                                       #'vc-jj-test--dir-status-files-update-function)
               '(("conflicted1.txt" removed nil)
                 ("subdir/conflicted2.txt" removed nil)
                 ("added.txt" removed nil)
                 ("unconflicted.txt" removed nil)
                 ("ignored.txt" ignored nil)))))))

(ert-deftest vc-jj-test-state-renamed ()
  "Test reported state for files and subdirectories before and after a rename.
Test what `vc-jj-state' and `vc-jj-dir-status-files' report for renamed
files.

When a file is renamed, its old file path should have a VC state of
\"removed\".  The new path should have a VC state of \"added\".

When a directory is renamed, the files contained within it are reported
along the same lines."
  ;; Rename root-level file
  (vc-jj-test--with-repo repo
    ;; 2026-04-26: It seems like Jujutsu's rename detection requires
    ;; file content to identify a single-file rename: without content,
    ;; jj reports the operation as a separate deletion and addition
    ;; rather than a rename.
    (write-region "foo bar" nil "before rename.txt")
    (shell-command "jj new")
    (shell-command "mv 'before rename.txt' 'after rename.txt'")
    
    (should (eq (vc-jj-state "before rename.txt") 'removed))
    (should (eq (vc-jj-state "after rename.txt") 'added))
    (should (seq-set-equal-p
             (vc-jj-dir-status-files repo nil
                                     #'vc-jj-test--dir-status-files-update-function)
             '(("before rename.txt" removed ( :rename-state removed
                                              :rename-other-filename "after rename.txt"))
               ("after rename.txt" added ( :rename-state added
                                           :rename-other-filename "before rename.txt"))))))

  ;; Rename file in subdirectory
  (vc-jj-test--with-repo repo
    (make-directory "subdir")
    (write-region "foo bar" nil "subdir/before rename.txt")
    (shell-command "jj new")
    (shell-command "mv 'subdir/before rename.txt' 'subdir/after rename.txt'")
    
    (should (eq (vc-jj-state "subdir/before rename.txt") 'removed))
    (should (eq (vc-jj-state "subdir/after rename.txt") 'added))
    (should (seq-set-equal-p
             (vc-jj-dir-status-files repo nil
                                     #'vc-jj-test--dir-status-files-update-function)
             '(("subdir/before rename.txt" removed
                ( :rename-state removed
                  :rename-other-filename "subdir/after rename.txt"))
               ("subdir/after rename.txt" added
                ( :rename-state added
                  :rename-other-filename "subdir/before rename.txt"))))))

  ;; Rename entire subdirectory
  (vc-jj-test--with-repo repo
    (make-directory "dir before rename/subdir" t)
    ;; 2026-04-26: It seems that, unlike single-file renames,
    ;; directory renames are detected even when the files within have
    ;; no content.
    (write-region "" nil "dir before rename/subdir/file1.txt")
    (write-region "" nil "dir before rename/subdir/file2.txt")
    (shell-command "jj new")
    (shell-command "mv 'dir before rename' 'dir after rename'")

    (should (eq (vc-jj-state "dir before rename/subdir/file1.txt") 'removed))
    (should (eq (vc-jj-state "dir before rename/subdir/file2.txt") 'removed))
    (should (eq (vc-jj-state "dir after rename/subdir/file1.txt") 'added))
    (should (eq (vc-jj-state "dir after rename/subdir/file2.txt") 'added))
    (should (seq-set-equal-p
             (vc-jj-dir-status-files repo nil
                                     #'vc-jj-test--dir-status-files-update-function)
             '(("dir before rename/subdir/file1.txt" removed
                ( :rename-state removed
                  :rename-other-filename "dir after rename/subdir/file1.txt"))
               ("dir before rename/subdir/file2.txt" removed
                ( :rename-state removed
                  :rename-other-filename "dir after rename/subdir/file2.txt"))
               ("dir after rename/subdir/file1.txt" added
                ( :rename-state added
                  :rename-other-filename "dir before rename/subdir/file1.txt"))
               ("dir after rename/subdir/file2.txt" added
                ( :rename-state added
                  :rename-other-filename "dir before rename/subdir/file2.txt")))))))

(ert-deftest vc-jj-test-state-funky-filename ()
  "Test compatibility with unusual characters in file names.
Test the presence of apostrophes, double quotes, and the equal sign in
file names, which are allowed in Linux.  See
https://codeberg.org/emacs-jj-vc/vc-jj.el/issues/38."
  (vc-jj-test--with-repo repo
    (write-region "" nil "TEST=TEST.txt")
    (should (eq (vc-jj-state "TEST=TEST.txt") 'added))
    (write-region "" nil "with'apostrophe.txt")
    (should (eq (vc-jj-state "with'apostrophe.txt") 'added))
    (write-region "" nil "with\"quotation.txt")
    (should (eq (vc-jj-state "with\"quotation.txt") 'added))

    (should (seq-set-equal-p
             (vc-jj-dir-status-files repo '("TEST=TEST.txt"
                                            "with'apostrophe.txt"
                                            "with\"quotation.txt")
                                     #'vc-jj-test--dir-status-files-update-function)
             '(("TEST=TEST.txt" added nil)
               ("with'apostrophe.txt" added nil)
               ("with\"quotation.txt" added nil))))))

(ert-deftest vc-jj-test-state-subdir ()
  "Test `vc-jj-dir-status-files' with a subdirectory.
Test when a repository subdirectory rather than repository root is
passed to `vc-jj-dir-status-files'."
  (vc-jj-test--with-repo repo
    (shell-command "jj new 'root()'")
    (write-region "" nil "root file.txt")
    (shell-command "jj new")
    (make-directory "subdir")
    (write-region "" nil "subdir/subdir file.txt")
    (should (seq-set-equal-p
             (vc-jj-dir-status-files repo
                                     '("root file.txt"
                                       "subdir/subdir file.txt")
                                     #'vc-jj-test--dir-status-files-update-function)
             '(("root file.txt" up-to-date nil)
               ("subdir/subdir file.txt" added nil))))
    (should (seq-set-equal-p
             (vc-jj-dir-status-files (expand-file-name "subdir" repo)
                                     '("subdir file.txt")
                                     #'vc-jj-test--dir-status-files-update-function)
             '(("subdir file.txt" added nil))))))

(ert-deftest vc-jj-test-state-unspecified-files ()
  "Test `vc-jj-dir-status-files' without specifying a list of files.
Test when `vc-jj-dir-status-files' is not passed a list of files.  In
such cases, only files not in the up-to-date or ignored states will be
reported."
  (vc-jj-test--with-repo repo
    (shell-command "jj new 'root()'")
    (write-region "" nil "file1.txt")
    (shell-command "jj new")
    (make-directory "subdir")
    (write-region "" nil "subdir/file2.txt")
    (shell-command "echo \"ignored.txt\" >> .gitignore")
    (write-region "" nil "ignored.txt")
    (should (seq-set-equal-p
             (vc-jj-dir-status-files repo nil
                                     #'vc-jj-test--dir-status-files-update-function)
             '(("subdir/file2.txt" added nil)
               (".gitignore" added nil))))
    (should (seq-set-equal-p
             (vc-jj-dir-status-files repo nil
                                     #'vc-jj-test--dir-status-files-update-function)
             '(("subdir/file2.txt" added nil)
               (".gitignore" added nil))))))

(ert-deftest vc-jj-test-state-.git-deletion ()
  "Test `vc-jj-dir-status-files' after deleting .git in a colocated repository.
`vc-jj-dir-status-files' should tolerate the deletion of the .git
directory, reporting on no files.  See bug#63."
  (let ((vc-jj-test--repo-colocate t))
    (vc-jj-test--with-repo repo
      (write-region "Hello!" nil "README")
      (shell-command "rm -r .git")
      (should (eq (vc-jj-dir-status-files repo nil #'vc-jj-test--dir-status-files-update-function)
                  nil)))))

(ert-deftest vc-jj-state-very-large-file ()
  "Test very large files.
Jujutsu usually prints to stderr when there is too large of a file
registered.  Such files should be reported as ignored, not interrupting
the user.  See bug#52."
  (vc-jj-test--with-repo repo
    ;; Reduce maximum file size of tracked files
    (shell-command "jj config set --repo snapshot.max-new-file-size 12")
    ;; Acceptable file size
    (write-region "1234567890" nil "numbers.txt")
    (should (eq (vc-jj-state "numbers.txt") 'added))
    ;; Too large of a file
    (write-region "abcdefghijklmnopqrstuvwxyz" nil "alphabet.txt")
    (should (eq (vc-jj-state "alphabet.txt") 'ignored))))

;;;; State-changing commands tests

(ert-deftest vc-jj-test-ignore ()
  "Test `vc-jj-ignore'."
  (vc-jj-test--with-repo repo
    (let (gitignore-buffer)
      (unwind-protect
          (progn
            ;; Create then register two files, one in the root and
            ;; another in a subdirectory
            (setq gitignore-buffer (find-file ".gitignore"))
            (write-region "xxx" nil "root-ignored.txt")
            (make-directory "subdir")
            (write-region "xxx" nil "subdir/subdir-ignored.txt")
            (should (eq (vc-jj-state "root-ignored.txt") 'added))
            (should (eq (vc-jj-state "subdir/subdir-ignored.txt") 'added))
            ;; Ignore both files
            (vc-jj-ignore "root-ignored.txt")
            (vc-jj-ignore "subdir/subdir-ignored.txt")
            (should (eq (vc-jj-state "root-ignored.txt") 'ignored))
            (should (eq (vc-jj-state "subdir/subdir-ignored.txt") 'ignored))
            ;; Un-ignore both files
            (vc-jj-ignore "root-ignored.txt" nil t)
            (vc-jj-ignore "subdir/subdir-ignored.txt" nil t)
            (should (eq (vc-jj-state "root-ignored.txt") 'added))
            (should (eq (vc-jj-state "subdir/subdir-ignored.txt") 'added)))
        (when (buffer-live-p gitignore-buffer)
          (kill-buffer gitignore-buffer))))))

(ert-deftest vc-jj-test-delete-file ()
  "Test `vc-jj-delete-file'."
  (vc-jj-test--with-repo repo
    (write-region "" nil "file1")
    (should (eq (vc-jj-state "file1") 'added))
    (shell-command "jj new")
    (vc-jj-delete-file "file1")
    (should (eq (vc-jj-state "file1") 'removed))))

;;;; Checkin tests

(ert-deftest vc-jj-test-checkin-partial-files ()
  "Test `vc-jj-checkin' with only a subset of edited files."
  (vc-jj-test--with-repo repo
    (write-region "" nil "file1.txt")
    (write-region "" nil "file2.txt")
    (shell-command "jj new")
    (write-region "edited" nil "file1.txt")
    (write-region "edited" nil "file2.txt")
    (write-region "" nil "file3.txt")
    (should (eq (vc-jj-state "file1.txt") 'edited))
    (should (eq (vc-jj-state "file2.txt") 'edited))
    (should (eq (vc-jj-state "file3.txt") 'added))

    (vc-jj-checkin (list "file1.txt") "Commit only some files")
    (should (eq (vc-jj-state "file1.txt") 'up-to-date))
    (should (eq (vc-jj-state "file2.txt") 'edited))
    (should (eq (vc-jj-state "file3.txt") 'added))
    (should (seq-set-equal-p
             (vc-jj-dir-status-files repo
                                     '("file1.txt"
                                       "file2.txt"
                                       "file3.txt")
                                     #'vc-jj-test--dir-status-files-update-function)
             '(("file1.txt" up-to-date nil)
               ("file2.txt" edited nil)
               ("file3.txt" added nil))))))

(ert-deftest vc-jj-test-checkin-sync-and-async ()
  "Test the synchronous and asynchronous versions of `vc-jj-checkin'.
Basic test that to check that `vc-jj-checkin' works both synchronously
and asynchronously."
  (vc-jj-test--with-repo repo
    ;; Synchronous
    (write-region "" nil "sync-file.txt")
    (should (eq (vc-jj-state "sync-file.txt") 'added))
    (let ((vc-async-checkin nil))
      (let ((result (vc-jj-checkin (list "sync-file.txt") "Sync commit")))
        (should (eq result 0))))
    (should (eq (vc-jj-state "sync-file.txt") 'up-to-date))

    ;; Async (when `vc-async-checkin' is non-nil; available only in
    ;; Emacs 31+)
    (when (boundp 'vc-async-checkin)
      (write-region "" nil "async-file.txt")
      (should (eq (vc-jj-state "async-file.txt") 'added))
      (let ((vc-async-checkin t))
        (let ((result (vc-jj-checkin (list "async-file.txt") "Async commit")))
          ;; RESULT must be a list of the form (async PROCESS), where
          ;; process is the process object
          (should (eq (car result) 'async))
          (should (processp (cadr result)))
          ;; Wait for the process to finish
          (let ((proc (cadr result)))
            (while (process-live-p proc)
              (accept-process-output proc 0.1)))))
      (should (eq (vc-jj-state "async-file.txt") 'up-to-date)))))

(ert-deftest vc-jj-test-checkin-directory ()
  "Test `vc-jj-checkin' for an entire directory.
Test when a subdirectory path is passed to `vc-jj-checkin', rather than
a path to a regular file.  See bug#62."
  (vc-jj-test--with-repo repo
    (make-directory "subdir")
    (write-region "foo" nil "subdir/file1.txt")
    (write-region "bar" nil "subdir/file2.txt")
    (make-directory "subdir/deeper_subdir")
    (write-region "baz" nil "subdir/deeper_subdir/file3.txt")
    (should (eq (vc-jj-state "subdir/file1.txt") 'added))
    (should (eq (vc-jj-state "subdir/file2.txt") 'added))
    (should (eq (vc-jj-state "subdir/deeper_subdir/file3.txt") 'added))
    (vc-jj-checkin (list "subdir/") "Sample commit message")
    (should (seq-set-equal-p
             (vc-jj-dir-status-files repo '("subdir/file1.txt"
                                            "subdir/file2.txt"
                                            "subdir/deeper_subdir/file3.txt")
                                     #'vc-jj-test--dir-status-files-update-function)
             '(("subdir/file1.txt" up-to-date nil)
               ("subdir/file2.txt" up-to-date nil)
               ("subdir/deeper_subdir/file3.txt" up-to-date nil))))))

;;;; Diff tests

(ert-deftest vc-jj-test-diff-merge ()
  "Test `vc-jj-diff' when for merges (changes with multiple parents).
A revision with multiple parent revisions is a merge commit.  Test that
`vc-jj-diff' outputs a diff between the working copy and the appropriate
parent when (1) no revisions have been specified to it (the REV1 and
REV2 arguments) and (2) when only REV1 has been specified to it."
  ;; The final revision log of this test looks like this (the output
  ;; of "jj log -p"):
  ;;
  ;; @    lxqzqwul john@example.com 2025-12-07 04:28:07 2cb61b72
  ;; ├─╮  (empty) merge
  ;; │ ○  xvqrvwts john@example.com 2025-12-07 04:28:06 0a819418
  ;; │ │  3
  ;; │ │  Modified regular file first file:
  ;; │ │     1    1: foo
  ;; │ │          2: baz
  ;; │ │  Added regular file second file:
  ;; │ │          1: bar
  ;; ○ │  rrwwokxl john@example.com 2025-12-07 04:28:04 352abbc6
  ;; ├─╯  (empty) 2
  ;; ○  vroztsyt john@example.com 2025-12-07 04:28:04 4bf3ab70
  ;; │  1
  ;; │  Added regular file first file:
  ;; │          1: foo
  ;; ┴  zzzzzzzz root() 00000000

  ;; Kludge: the output of 'jj diff --git' changed slightly after this
  ;; test was originally written; make sure we run at least the
  ;; known-good version.  If this test fails, fix the string constants
  ;; below and bump the version.
  (skip-unless (version<= "0.47.1" (vc-jj--program-version)))
  (vc-jj-test--with-repo repo
    (let ((vc-jj-diff-switches '("--git"))
          parent-rev-1 parent-rev-2
          merge-rev merge-rev-parent)
      ;; Create the first commit and first file
      (shell-command "jj new -m \"1\" root()")
      (write-region "foo\n" nil "first file")
      ;; Create first branch (first parent commit) with no new files
      (shell-command (concat "jj new -m \"2\" @"))
      (setq parent-rev-1 (shell-command-to-string "jj show --no-patch -r @ -T change_id"))
      ;; Create second branch (second parent commit) with a new file
      ;; and changes to the "first file" file
      (shell-command (concat "jj new -m \"3\" @-"))
      (write-region "bar\n" nil "second file")
      (write-region "baz\n" nil "first file" t)
      (setq parent-rev-2 (shell-command-to-string "jj show --no-patch -r @ -T change_id"))
      ;; Create working copy that is a merge commit (revision with two
      ;; parents)
      (shell-command (concat "jj new -m \"merge\" " parent-rev-1 " " parent-rev-2))
      (setq merge-rev (shell-command-to-string "jj show --no-patch -r @ -T change_id")
            merge-rev-parent (vc-jj-previous-revision nil merge-rev))

      ;; Helpful for debugging
      (message (shell-command-to-string "jj log -p"))

      ;; No revisions and no file (show entire diff)
      (with-temp-buffer
        (vc-jj-diff nil nil nil (current-buffer))
        ;; The parent of the working copy (MERGE_REV_PARENT) is
        ;; PARENT-REV-1 (the revision chronologically created first)
        (should (string= merge-rev-parent parent-rev-1))
        (should (string= (buffer-substring-no-properties (point-min) (point-max))
                         (shell-command-to-string
                          ;; We do not do "jj diff --git -r @" because
                          ;; denotes the changes in @, whereas VC
                          ;; expect the changes between @ and its
                          ;; first parent earlier revision
                          (format "jj diff --git -f 'first_parent(@)' -t @"))))
        (should (string= (buffer-substring-no-properties (point-min) (point-max))
                         ;; The diff should be the changes added in
                         ;; PARENT-REV-2 ("second file" and "baz\n" to
                         ;; the end of "first file"), since those were
                         ;; the only changes in between PARENT-REV-1
                         ;; and MERGE-REV-PARENT
                         (string-join '("diff --git a/first file b/first file"
                                        "index 257cc5642c..0c071e1d07 100644"
                                        "--- a/first file"
                                        "+++ b/first file"
                                        "@@ -1,1 +1,2 @@ foo"
                                        " foo"
                                        "+baz"
                                        "diff --git a/second file b/second file"
                                        "new file mode 100644"
                                        "index 0000000000..5716ca5987"
                                        "--- /dev/null"
                                        "+++ b/second file"
                                        "@@ -0,0 +1,1 @@ bar"
                                        "+bar\n")
                                      "\n"))))

      ;; No revisions and specify a file present only in the second,
      ;; older parent (show diff only for that file)
      (with-temp-buffer
        ;; Show the diff between the working copy (MERGE_REV_PARENT)
        ;; and its first parent, but ONLY for the "second file" file
        (vc-jj-diff '("second file") nil nil (current-buffer))
        (should (string= merge-rev-parent parent-rev-1))
        (should (string= (buffer-substring-no-properties (point-min) (point-max))
                         (shell-command-to-string
                          (format "jj diff --git -f 'first_parent(@)' -t @ -- 'second file'"))))
        (should (string= (buffer-substring-no-properties (point-min) (point-max))
                         ;; The diff should only be the addition of
                         ;; the "second file" file (contra the above
                         ;; case)
                         (string-join '("diff --git a/second file b/second file"
                                        "new file mode 100644"
                                        "index 0000000000..5716ca5987"
                                        "--- /dev/null"
                                        "+++ b/second file"
                                        "@@ -0,0 +1,1 @@ bar"
                                        "+bar\n")
                                      "\n")))))))

;;;; Revision log tests

(ert-deftest vc-jj-test-previous-revision-merge ()
  "Test vc-previous-revision for a change with multiple parents.
We expect this function to return the first parent specified."
  (vc-jj-test--with-repo repo
    (let ( branch-1 branch-2 branch-merged
           branch-parent branch-parent-with-file)
      ;; - construct a diamond graph
      ;; - add a file to the second parent of the merge commit
      ;; - check that we find the first parent
      ;; - check that we find the second parent when looking for a
      ;;   parent with the new file
      (shell-command "jj new 'root()'")
      (write-region "Hello!" nil "README")
      (setq branch-1 (vc-jj-working-revision "README"))
      (shell-command "jj new 'root()'")
      (write-region "Hello!" nil "README")
      (write-region "Hello!" nil "README-onlyhere")
      (setq branch-2 (vc-jj-working-revision "README"))
      (shell-command (concat "jj new " branch-1 " " branch-2))
      (setq branch-merged (vc-jj-working-revision "README"))
      (setq branch-parent (vc-jj-previous-revision nil branch-merged))
      (setq branch-parent-with-file (vc-jj-previous-revision "README-onlyhere" branch-merged))
      (should (string= branch-parent branch-1))
      (should (string= branch-parent-with-file branch-2)))))

(ert-deftest vc-jj-test-next-revision-merge ()
  "Test vc-next-revision for a change with multiple children.
We check that we get the revision where a given file was added."
  (vc-jj-test--with-repo repo
    (let ( branch-root branch-1 branch-2 branch-child branch-child-1 branch-child-2)
      (write-region "Hello!" nil "README")
      (setq branch-root (vc-jj-working-revision "README"))
      (shell-command (concat "jj new " branch-root))
      (write-region "Hello!" nil "README-onlyhere")
      (setq branch-1 (vc-jj-working-revision "README-onlyhere"))
      (shell-command (concat "jj new " branch-root))
      (write-region "Persisting branch 2" nil "README-branch2")
      (setq branch-2 (vc-jj-working-revision "README-branch2"))
      (setq branch-child (vc-jj-next-revision nil branch-root))
      ;; Slight pun: these files only exist in the child branches
      (setq branch-child-1 (vc-jj-next-revision "README-onlyhere" branch-root))
      (setq branch-child-2 (vc-jj-next-revision "README-branch2" branch-root))
      ;; We don't want to rely on the exact order in which the
      ;; children are printed
      (should (or (string= branch-child branch-1) (string= branch-child branch-2)))
      (should (string= branch-child-1 branch-1))
      (should (string= branch-child-2 branch-2)))))

;;;; Annotation tests

(ert-deftest vc-jj-test-annotate-command ()
  "Test the output of `vc-jj-annotate-command'."
  (vc-jj-test--with-repo repo
    (let (change-1 change-2 annotation-buffer)
      ;; Create two changes, make sure that the change ids in the
      ;; annotation buffer match.  This test is supposed to detect
      ;; changes in the output format of "jj annotate"
      (unwind-protect
          (progn
            (write-region "Line 1\n" nil "README")
            (setq change-1 (vc-jj-working-revision "README"))
            (shell-command "jj commit -m 'First change'")
            (write-region "Line 2\n" nil "README" t)
            (shell-command "jj describe -m 'Second change'")
            (setq change-2 (vc-jj-working-revision "README"))
            ;; Use a plain buffer and call `vc-jj-annotate-command'
            ;; directly rather than going through `vc-annotate', which
            ;; has display machinery that doesn't work in Emacs
            ;; without an interactive display (e.g., "emacs --batch"
            ;; and eldev)
            (setq annotation-buffer (generate-new-buffer " *vc-jj-test-annotate-command*"))
            (vc-jj-annotate-command "README" annotation-buffer change-2)
            (let ((annotation-process (get-buffer-process annotation-buffer)))
              (while (process-live-p annotation-process)
                (accept-process-output annotation-process)))
            (with-current-buffer annotation-buffer
              (goto-char (point-min))
              (should (string-prefix-p (thing-at-point 'word) change-1))
              (forward-line)
              (should (string-prefix-p (thing-at-point 'word) change-2))))
        (when (buffer-live-p annotation-buffer) (kill-buffer annotation-buffer))))))

(provide 'vc-jj-tests)
;;; vc-jj-tests.el ends here
