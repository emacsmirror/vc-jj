;;; vc-jj.el --- VC backend for the Jujutsu version control system -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Free Software Foundation, Inc.

;; Author: Wojciech Siewierski
;;         Rudolf Schlatte <rudi@constantly.at>
;;         Kristoffer Balintona <krisbalintona@gmail.com>
;; URL: https://codeberg.org/emacs-jj-vc/vc-jj.el
;; Version: 0.6
;; Package-Requires: ((emacs "28.1") (compat "30.1") (project "0.11.2"))
;; Keywords: vc tools

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

;; This package provides a backend for vc.el to handle Jujutsu (jj)
;; repositories.  Jujutsu uses a change-centric model that differs in
;; important ways from Git’s commit-centric model.  Vc-jj adapts
;; vc.el’s abstractions to match jj terminology and behavior.
;;
;; Users may see and customize all options by pressing
;;
;;  'M-x customize-group vc-jj RET'
;;
;; and using the Customize menu.
;;
;; Additional information about Jujutsu and vc-jj.el may be found in
;; the README.org file.  The NEWS.org file contains user-relevant
;; information regarding bug fixes, additions, and changes organized
;; by version.
;;
;; The vc-jj.el project repository can be found online at
;; https://codeberg.org/emacs-jj-vc/vc-jj.el.

;; STATUS AND LIMITATIONS
;;
;; Vc-jj implements most commonly used vc.el commands (such as
;; `vc-dir', `vc-diff', `vc-print-log', and file state queries), but
;; some operations are not yet supported or behave differently from
;; vc-git due to jj’s model.

;; FILE STRUCTURE
;;
;; After the "Customization" and "Internal Utilities" sections, the
;; organization of vc-jj.el file matches the "BACKEND PROPERTIES"
;; section of the preamble of the vc.el file: each outline heading
;; corresponds to a vc backend method and the contents of each heading
;; relate to implementing that backend method.

;; FEEDBACK AND CONTRIBUTIONS
;;
;; Bug reports and contributions are welcome, especially for
;; unimplemented vc.el backend methods.  The home of this project is
;; found here: https://codeberg.org/emacs-jj-vc/vc-jj.el.  Users may
;; file bug reports in the "Issues" tab and create pull requests in
;; the "Pull requests" tab.
;;
;; Vc-jj prefers Jujutsu's terminology and attempts to adhere to it,
;; so we ask contributors to try their best to use the terminology
;; specific to jujutsu as opposed to other version control systems,
;; such as Git.  A brief description of these differences can be found
;; in the README.
;;
;; Additionally, contributors should adhere to the following
;; capitalization and terminology conventions:
;; - "Jujutsu" refers to the Jujutsu version control system.
;; - "jj" refers to the Jujutsu command-line tool or its commands.
;; - "Jj" is used when "jj" would otherwise begin a sentence.
;; - "vc-jj" may be used to refer to this Emacs package.
;; - "Vc-jj" is used when "vc-jj" would otherwise begin a sentence.

;;; Code:

(require 'compat)
(require 'cl-lib)
(require 'seq)
(require 'vc)
(require 'vc-dir)
(require 'vc-git)
(require 'log-view)
(require 'log-edit)
(require 'ansi-color)
(require 'iso8601)
(require 'time-date)
(require 'project)
(declare-function vc-annotate-convert-time "vc-annotate" (&optional time))

;;; Customization

(defgroup vc-jj nil
  "VC Jujutsu backend."
  :group 'vc)

(defcustom vc-jj-program "jj"
  "Name of the jj executable (excluding any arguments)."
  :type 'string
  :risky t)

(defcustom vc-jj-root-log-format
  (list
   ;; Log format (passed as the template for "jj log")
   "
if(root,
  format_root_commit(self),
  label(if(current_working_copy, 'working_copy'),
    concat(
      separate(' ',
        change_id ++ '​' ++ change_id.shortest(8).prefix() ++ '​' ++ change_id.shortest(8).rest(),
        if(author.name(), author.name(), if(author.email(), author.email().local(), email_placeholder)),
        commit_timestamp(self).format('%Y-%m-%d'),
        bookmarks,
        tags,
        working_copies,
        if(self.contained_in('first_parent(@)'), label('git_head', 'git_head()')),
        format_short_commit_id(commit_id),
        if(conflict, label('conflict', 'conflict')),
        if(config('ui.show-cryptographic-signatures').as_boolean(),
          format_short_cryptographic_signature(signature)),
        if(empty, label('empty', '(empty)')),
        if(description,
          description.first_line(),
          label(if(empty, 'empty'), description_placeholder),
        ),
      ) ++ '\n',
    ),
  )
)
"
   ;; Log entry regexp
   (rx
    line-start
    ;; Graph
    (+? nonl) " "
    ;; Full change ID
    (group (+ (any "K-Zk-z")))
    space
    ;; Visible change ID
    (group (+ (any "K-Zk-z")))
    space
    (group (+ (any "K-Zk-z")))
    " "
    ;; Author
    (group (* nonl)) " "
    ;; Time
    (group (= 4 (any num)) "-" (= 2 (any num)) "-" (= 2 (any num)))
    ;; Tags and  bookmarks
    (group (*? nonl)) " "
    ;; Commit ID
    (group (+ (any hex))) " "
    ;; Special states
    (group (opt "conflict "))
    (group (opt "(empty) "))
    (group (opt "(no description set)"))
    ;; Description
    (* nonl) line-end)
   ;; Font lock keywords
   '((1 '(face nil invisible t))        ; Full change ID
     (2 'log-view-message)              ; Short change ID
     (3 'change-log-list)               ; Rest of Change ID
     (4 'change-log-name)               ; Author name
     (5 'change-log-date)               ; Date
     (6 'vc-jj-log-view-bookmark)       ; Bookmark names
     (7 'vc-jj-log-view-commit)         ; Commit ID
     (8 'vc-conflict-state)             ; Conflict marker
     (9 'change-log-function)           ; No description marker
     (10 'change-log-function)))        ; Revision description
  "JJ log format for `vc-print-root-log'.
This option determines the format and fontification of the JJ Log View
buffer created from `vc-print-root-log'.

It should be a list of the form (FORMAT REGEXP KEYWORDS), where FORMAT
is a format string (a JJ template passed to \"jj log\"), REGEXP is a
regular expression matching a single entry in the \"jj log\" output, and
KEYWORDS is a list of font lock keywords (see
`font-lock-keywords'and `(elisp) Search-based Fontification') for
highlighting the Log View buffer.

REGEXP may define capture groups that KEYWORDS can use to fontify
various regions of the Log View buffer."
  :type '(list string regexp (repeat sexp)))

(defcustom vc-jj-global-switches '("--no-pager" "--color" "never")
  "Global switches to pass to any jj command."
  :type '(choice (const :tag "None" nil)
         (string :tag "Argument String")
         (repeat :tag "Argument List" :value ("") string)))

(defcustom vc-jj-annotate-switches nil
  "String or list of strings specifying switches for \"jj file annotate\".
If nil, use the value of `vc-annotate-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
                 (const :tag "None" t)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string)))

(defcustom vc-jj-checkin-switches nil
  "String or list of strings specifying switches for \"jj commit\".
If nil, use the value of `vc-checkin-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
                 (const :tag "None" t)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string)))

(defcustom vc-jj-diff-switches '("--git")
  "String or list of strings specifying switches for \"jj diff\".
If nil, use the value of `vc-diff-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
                 (const :tag "None" t)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string)))

(defface vc-jj-log-view-commit
  '((t :weight light :inherit (shadow italic)))
  "Face for commit IDs in `vc-jj-log-view-mode' buffers.")

(defface vc-jj-log-view-bookmark
  '((t :weight bold :inherit log-view-message))
  "Face for bookmark names in `vc-jj-log-view-mode' buffers.")

;;; Internal Utilities

;; Note that 'JJ' should come before 'Git' in `vc-handled-backends',
;; since by default a jj repository contains both '.jj' and '.git'
;; directories.

;;;###autoload
(add-to-list 'vc-handled-backends 'JJ)

(defun vc-jj--filename-to-fileset (filename)
  "Convert FILENAME to a Jujutsu fileset expression.
Most Jujutsu commands accept filesets rather than file paths.  This
function returns a fileset expression of the form \"root:PATH\", where
PATH is FILENAME expressed relative to the workspace root and quoted as
a string literal.

The caller is responsible for setting `default-directory' to the
repository root (e.g., via `let') before calling this function.
\(FILENAME is relativized to the workspace root using
`default-directory'.)"
  (format "root:%S" (file-relative-name filename default-directory)))

(defun vc-jj--call (infile buffer &rest args)
  "Run jj with ARGS, sending the result to BUFFER.
INFILE and BUFFER are as described in `process-file'.  Return the
process exit status.

The value of `vc-jj-global-switches' is prepended to ARGS.

The process runs in `default-directory'.  The caller must set it to the
repository root before calling this function if filesets may be passed
to the jj command (see the docstring of `vc-jj--filename-to-fileset').
When `default-directory' is a remote path, `process-file' will invoke
the appropriate file name handler (e.g., TRAMP), so this function works
correctly with remote repositories.

This function is the subroutine underlying all non-user-facing vc-jj
process functions, that is, all functions that do not involve calling
`vc-do-command'."
  ;; This function is based on `vc-git--call'
  (let (;; Enable `inhibit-null-byte-detection', otherwise Tramp's EOL
        ;; conversion might get confused
        (inhibit-null-byte-detection t)
        (coding-system-for-read (or coding-system-for-read 'utf-8))
        (coding-system-for-write (or coding-system-for-write 'utf-8)))
    ;; `process-file' works with remote paths (via TRAMP file
    ;; handlers), whereas `call-process' is not
    (apply #'process-file vc-jj-program infile buffer nil
           (append vc-jj-global-switches args))))

(defun vc-jj--process-lines (file-or-list &rest args)
  "Run jj with FILE-OR-LIST and ARGS, returning stdout as a list of strings.
Return the process's stdout as a list of strings, one string for every
line in stdout, with ANSI escape sequences removed from each string.  An
error is signaled if jj exits with a non-zero status.

All stderr is discarded (since jj prints warnings to stderr even when
run with \"--quiet\" option).  As a result, the returned strings are
safe for parsing.

FILE-OR-LIST may be nil or non-nil.  When non-nil, it should be a file
or a list of files.  The process is run in the repository root the first
file belongs in.  These file names are converted to jj fileset
expressions and appended to ARGS.  If the caller would like to pass a
raw file or list of files not converted to a jj fileset, they should be
included in ARGS."
  (let* ((files (ensure-list file-or-list))
         (root (vc-jj-root (or (car files) default-directory)))
         (default-directory root)
         (filesets (mapcar #'vc-jj--filename-to-fileset files))
         status lines)
    (with-temp-buffer
      (setq status (apply #'vc-jj--call nil '(t nil)
                          (append args filesets)))
      (unless (zerop status)
        (error "Vc-jj: 'jj' exited with status %s" status))
      ;; REVIEW 2026-04-06: Does the comment below still hold?
      ;;
      ;; Strip ANSI escape sequences: even with the "--color never" jj
      ;; option, TRAMP connections have been observed to introduce
      ;; spurious sequences.
      (ansi-color-filter-region (point-min) (point-max))
      (goto-char (point-min))
      (while (not (eobp))
        (push (buffer-substring-no-properties (pos-bol) (pos-eol))
              lines)
        (forward-line 1)))
    (nreverse lines)))

(defun vc-jj--command-parseable (file-or-list &rest args)
  "Run jj with FILE-OR-LIST and ARGS, returning stdout as a string.
Return the process's stdout with ANSI escape sequences removed.  An
error is signaled if jj exits with a non-zero status.

All stderr is discarded (since jj prints warnings to stderr even when
run with the \"--quiet\" option).  As a result, the returned string is
safe for parsing.

FILE-OR-LIST may be nil or non-nil.  When non-nil, it should be a file
or a list of files.  The process is run in the repository root the first
file belongs in.  These file names are converted to jj fileset
expressions and appended to ARGS.  If the caller would like to pass a
raw file or list of files not converted to a jj fileset, they should be
included in ARGS."
  (let* ((files (ensure-list file-or-list))
         (root (vc-jj-root (or (car files) default-directory)))
         (default-directory root)
         (filesets (mapcar #'vc-jj--filename-to-fileset files))
         status output)
    (with-temp-buffer
      (setq status (apply #'vc-jj--call nil '(t nil)
                          (append args filesets)))
      (unless (zerop status)
        (error "Vc-jj: 'jj' exited with status %s" status))
      (ansi-color-filter-region (point-min) (point-max))
      (setq output (buffer-substring-no-properties (point-min) (point-max))))
    output))

(defun vc-jj--command-dispatched (buffer okstatus file-or-list &rest args)
  "A wrapper around `vc-do-command' for use in vc-jj.
Run jj with ARGS and FILE-OR-LIST, with stdout and stderr both sent to
BUFFER.  The meaning of BUFFER, OKSTATUS, and ARGS is the same as in
`vc-do-command'; see its docstring for details.

Return the process object for async calls and the exit status for
synchronous calls.

This function is used by user-facing commands such as `vc-push', hooking
into VC machinery by means of `vc-do-command'.  If it is necessary to
parse or interpret jj's output programmatically, use
`vc-jj--command-parseable' or `vc-jj--process-lines' instead.  Those
functions distinguish between stderr and stdout, unlike this one, making
them more suitable for programmatic parsing.

FILE-OR-LIST may be nil or non-nil.  When non-nil, it should be a file
or a list of files.  The process is run in the repository root the first
file belongs in.  These file names are converted to jj fileset
expressions and appended to ARGS.  If the caller would like to pass a
raw file or list of files not converted to a jj fileset, they should be
included in ARGS."
  (let* ((coding-system-for-read (or coding-system-for-read 'utf-8))
         (coding-system-for-write (or coding-system-for-write 'utf-8))
         (files (ensure-list file-or-list))
         (root (vc-jj-root (or (car files) default-directory)))
         (default-directory root)
         (filesets (mapcar #'vc-jj--filename-to-fileset files))
         (global-switches (ensure-list vc-jj-global-switches)))
    ;; We pass our prepared fileset to jj directly rather than to
    ;; `vc-do-command', which would pass raw file names to jj
    (apply #'vc-do-command (or buffer "*vc*") okstatus vc-jj-program nil
           (append global-switches args filesets))))

(defun vc-jj--program-version ()
  "Return the version of the installed jj executable.
The return value is usable as input to `version<' and related functions."
  (when-let* ((raw-version (car (vc-jj--process-lines nil "--version")))
              (version (string-trim raw-version "jj " nil)))
    version))

;;; BACKEND PROPERTIES

;;;; revision-granularity
(defun vc-jj-revision-granularity ()
  "Jj implementation of vc property `revision-granularity'."
  'repository)

;;;; update-on-retrieve-tag
(defun vc-jj-update-on-retrieve-tag ()
  "JJ-specific implementation of `update-on-retrieve-tag' property."
  nil)

;;;; async-checkins
;; Emacs 31 method

(defalias 'vc-jj-async-checkins #'always)

;;;; working-revision-symbol
;; Emacs 31 method

;; Use `defalias' and `cl-constantly' like
;; `vc-git-working-revision-symbol' does
(defalias 'vc-jj-working-revision-symbol (cl-constantly "@"))

;;; STATE-QUERYING FUNCTIONS

;;;; registered

;;;###autoload (defun vc-jj-registered (file)
;;;###autoload   "Return non-nil if FILE is registered with jj."
;;;###autoload   (if (and (vc-find-root file ".jj")   ; Short cut.
;;;###autoload            (executable-find "jj"))
;;;###autoload       (progn
;;;###autoload         (load "vc-jj" nil t)
;;;###autoload         (vc-jj-registered file))))
(defun vc-jj-registered (file)
  "Check whether FILE is registered with jj.
Return non-nil when FILE is file tracked by JJ and nil when not."
  (when-let* ((default-directory (vc-jj-root file)))
    (vc-jj--process-lines file "file" "list")))

;;;; state

(defconst vc-jj--conflict-aware-file-list-template
  "if(self.conflict(), 'C', 'T') ++ ' ' ++ self.path() ++ '\n'"
  "Template for outputting a file list with conflict information.
This template is intended as the value of the \"-T\" argument passed to
\"jj file list\".  It is meant to be used in conjunction with
`vc-jj--parse-conflict-aware-file-table'.")

(defun vc-jj--parse-conflict-aware-file-table (lines)
  "Return a hash table of tracked files with information on conflicted state.
The returned table maps FILE to CONFLICTEDP, where FILE is a tracked
file and CONFLICTEDP is t if that file is in a conflicted state.  This
table helps deduce whether a given file is tracked and whether it is
conflicted.

LINES is a list of strings, where each string is a line of the output of
\"jj file list\" with a template of
`vc-jj--conflict-aware-file-list-template', which shows a list of all
tracked files, with each path prepended with \"C\" or \"T\" depending on
its conflict state."
  (let ((table (make-hash-table :test #'equal)))
    (mapc (lambda (line)
            (puthash (substring line 2) (eq ?C (aref line 0)) table))
          lines)
    table))

(defun vc-jj--parse-diff-types-file-table (lines &optional extra-table)
  "Return a hash table of changed files with their diff types.
The returned table maps FILE to TYPES, where FILE is a changed file and
TYPES is a two-character string indicating the before and after types of
that file.  See `vc-jj--deduce-state-from-diff-types' for which VC
states can be deduced from this two-character string.

If EXTRA-TABLE is provided, it is a hash table mapping FILE to a plist
containing extra information that this function will populate.  This
extra information may be displayed in the *vc-dir* buffer (see
`vc-jj-dir-status-files' and `vc-jj-dir-printer').

LINES is a list of strings, where each string is a line of the output of
\"jj diff --types\", which shows a list of all changed files, with each
path prepended with a two-character type string indicating the before
and after types of the file."
  (let ((table (make-hash-table :test #'equal)))
    (mapc (lambda (line)
            (if (string-match (rx (group-n 1 (* anychar))
                                  "{"
                                  (group-n 2 (1+ (not (any "}"))))
                                  " => "
                                  (group-n 3 (1+ (not (any "}"))))
                                  "}"
                                  (group-n 4 (* anychar)))
                              line)
                ;; For renamed files, create separate entries for the
                ;; before-rename and after-rename files.  The rename
                ;; block {before => after} may appear mid-path, so we
                ;; reconstruct full paths using the surrounding
                ;; context.
                (let* ((prefix (substring (match-string 1 line) 3))
                       (before-part (match-string 2 line))
                       (after-part (match-string 3 line))
                       (suffix (match-string 4 line))
                       (before (concat prefix before-part suffix))
                       (after (concat prefix after-part suffix)))
                  (puthash before "F-" table) ; Removed state
                  (puthash after "-F" table)  ; Added state
                  (when extra-table
                    ;; Populate EXTRA-TABLE with rename information
                    (puthash before (list :rename-state 'removed :rename-other-filename after)
                             extra-table)
                    (puthash after (list :rename-state 'added :rename-other-filename before)
                             extra-table)))
              (puthash (substring line 3) (substring line 0 2) table)))
          lines)
    table))

(defun vc-jj--deduce-state-from-diff-types (diff-types)
  "Return the file state deduced from DIFF-TYPES.
DIFF-TYPES is either nil or a two-character string at the beginning of
each line in of \"jj diff --types\".  See \"jj diff --help\" for a list
of all possible characters and their meanings.

When DIFF-TYPES is nil, return nil.  When it is a two-character string,
return a symbol representing the VC state deduced from DIFF-TYPES.  The
possible state symbols returned are:
- \\='conflict
- \\='added
- \\='removed
- \\='edited
See `vc-jj-state' for a list of all VC states relevant to Jujutsu.

Please note that the conflict state is only partially deduced by this
function: \"jj diff\" reports a conflict only when the conflict
originates in the working copy, not if the conflict originates in an
earlier revision.  To definitively determine the conflict state of a
file, something like \"jj file list -T \\='self.conflict()\\='\" should
be used (see `vc-jj--parse-conflict-aware-file-table'.)"
  (when diff-types
    (let ((before (aref diff-types 0))
          (after (aref diff-types 1)))
      (cond ((eq ?C after) 'conflict)
            ((eq ?- before) 'added)
            ((eq ?- after) 'removed)
            ((and (memq before '(?F ?L ?G)) (memq after  '(?F ?L ?G)))
             'edited)
            (t (error "vc-jj: Unexpected diff types string: %s" diff-types))))))

(defun vc-jj--deduce-state (file file-diff-types-table file-conflict-table)
  "Deduce the VC state of FILE from already parsed jj output.
Return a symbol corresponding to the VC state of FILE.  See
`vc-jj-state' for the possible VC state symbols.

FILE is a path relative to the repository root.  FILE-DIFF-TYPES-TABLE
is a hash table produced by `vc-jj--parse-diff-types-file-table'.
FILE-CONFLICT-TABLE is a hash table produced by
`vc-jj--parse-conflict-aware-file-table'.  Together,
FILE-DIFF-TYPES-TABLE and FILE-CONFLICT-TABLE suffice to deduce the VC
state of FILE.

Note that since FILE-DIFF-TYPES-TABLE should be produced by
`vc-jj--parse-diff-types-file-table', FILE-DIFF-TYPES-TABLE will only
contain file entries for files with changes in the working copy.  On the
other hand, since FILE-CONFLICT-TABLE should be produced by
`vc-jj--parse-conflict-aware-file-table', FILE-CONFLICT-TABLE will only
contain file entries for tracked files.

To avoid unnecessary computation, calls to
`vc-jj--parse-diff-types-file-table' and
`vc-jj--parse-conflict-aware-file-table' can be passed jj output
specific to FILE.  As an example for
`vc-jj--parse-diff-types-file-table',

  (vc-jj--process-lines FILE \"diff\" \"--types\")"
  ;; We use hash tables rather than e.g. alists for the sake of
  ;; performance: table lookups are O(1) whereas list lookups are O(n)
  (let* ((diff-types (gethash file file-diff-types-table))
         (diff-types-state (vc-jj--deduce-state-from-diff-types diff-types)))
    (or
     ;; DIFF-TYPES-STATE definitely deduces the edited, removed, and
     ;; added states, as well as the conflict state when the conflict
     ;; originates in the working copy
     diff-types-state
     ;; When DIFF-TYPES-STATE is nil (i.e., FILE has no changes in the
     ;; working copy), use `file-conflict-table' to distinguish
     ;; between the ignored, up-to-date, and conflict states.  (The
     ;; conflict state can only be caught by DIFF-TYPES-STATE when the
     ;; conflict originates in the working copy; `file-conflict-table'
     ;; catches when a conflict originates in an earlier revision.)
     (let ((conflictp (gethash file file-conflict-table 'not-tracked)))
       (cond ((eq conflictp 'not-tracked) 'ignored)
             (conflictp 'conflict)
             (t 'up-to-date))))))

(defun vc-jj-state (file)
  "Return VC state symbol of FILE.
There are several file states recognized by VC (see the docstring of
`vc-state' for the full list).  Only several of these are relevant to
Jujutsu.  They are:
- \\='added (new file)
- \\='removed (deleted file)
- \\='edited (modified file)
- \\='conflict (merge conflict)
- \\='up-to-date (unmodified file)
- \\='ignored (ignored by repository)
Other VC backends would also recognize the \"unregistered\" state, but
there is no such state in Jujutsu since Jujutsu automatically registers
new files."
  ;; TODO 2026-03-22: Ideally, we would only call one jj command, or
  ;; only call a second one when necessary (instead of calling two
  ;; commands always).  If, in the future, jj provides templates that
  ;; make this possible, then this function, its helpers, and
  ;; `vc-jj-dir-status-files' should be refactored
  (let* ((root (vc-jj-root file))
         (file (file-relative-name file root))
         (default-directory root)
         (file-diff-types-table
          (vc-jj--parse-diff-types-file-table
           (vc-jj--process-lines file "diff" "--types")))
         (file-conflict-table
          (vc-jj--parse-conflict-aware-file-table
           (vc-jj--process-lines file "file" "list"
                                 "-T" vc-jj--conflict-aware-file-list-template))))
    (vc-jj--deduce-state file file-diff-types-table file-conflict-table)))

;;;; dir-status-file

(defun vc-jj-dir-status-files (root-or-subdir files update-function)
  "Call UPDATE-FUNCTION on a computed list of entries for ROOT-OR-SUBDIR.
Compute a list of file entries for ROOT-OR-SUBDIR whose elements are of
the form (FILE STATE EXTRA), where FILE is an absolute path or one
relative to ROOT-OR-SUBDIR, STATE is a VC state symbol, and EXTRA is a
plist containing information that may be used later by
`vc-jj-dir-printer', which see.

Return the result of calling UPDATE-FUNCTION with that list as an
argument.

FILES is either nil or a list of files relative to ROOT-OR-SUBDIR.  If
FILES is nil, return the state of all files that don't have the
\\='up-to-date or \\='ignored states (i.e., only files in the \\='added,
\\='removed, \\='edited, or \\='conflict states).  If FILES is non-nil,
return the state of all FILES, regardless of their state.

ROOT-OR-SUBDIR is the repository root or a subdirectory of the
repository.

For a description of the states relevant to Jujutsu, see the docstring
of `vc-jj-state'."
  (condition-case err
      ;; A big consideration of this function is performance in large
      ;; repositories.  This is why we use hash tables rather than
      ;; lists: minimize the number of operations and loops over
      ;; lists, which are expensive compared to hash table operations
      (let* ((root (vc-jj-root root-or-subdir))
             (default-directory root)
             (extras-table (make-hash-table :test #'equal))
             (file-diff-types-table
              (vc-jj--parse-diff-types-file-table
               (vc-jj--process-lines root-or-subdir "diff" "--types")
               extras-table))
             (file-conflict-table
              (vc-jj--parse-conflict-aware-file-table
               (vc-jj--process-lines root-or-subdir "file" "list"
                                     "-T" vc-jj--conflict-aware-file-list-template)))
             (files-to-report
              (if files
                  ;; When FILES is non-nil, report on all FILES
                  ;; regardless of state.
                  (mapcar (lambda (f)
                            ;; Make all file paths relative to ROOT
                            ;; since the paths stored in
                            ;; `vc-jj--deduce-state' (see below) are
                            ;; relative to the project root
                            (file-relative-name (file-name-concat root-or-subdir f) root))
                          files)
                ;; When FILES is nil, report only on files that are in
                ;; the edited, added, removed, or conflict state
                ;; (i.e., not the up-to-date or ignored states).
                ;;
                ;; Our strategy: we get all files in
                ;; FILE-DIFF-TYPES-TABLE (files not in that table are
                ;; ignored or up-to-date) plus files with conflicts
                ;; that originate in an earlier revision (keys in
                ;; FILE-CONFLICT-TABLE with a non-nil value).
                (let (result)
                  (maphash (lambda (k _) (push k result)) file-diff-types-table)
                  (maphash (lambda (k v)
                             (when (and v
                                        ;; Don't push duplicates of
                                        ;; conflicted files to RESULT.
                                        ;; Doing it this way avoids
                                        ;; having to de-duplicate
                                        ;; RESULTS later by looping
                                        ;; the list
                                        (not (gethash k file-diff-types-table)))
                               (push k result)))
                           file-conflict-table)
                  result)))
             (result
              (mapcar (lambda (root-rel-file)
                        (let ((display-path
                               ;; The files reported should be
                               ;; relative to ROOT-OR-SUBDIR
                               (file-relative-name root-rel-file root-or-subdir))
                              (state (vc-jj--deduce-state root-rel-file
                                                          file-diff-types-table
                                                          file-conflict-table))
                              ;; EXTRAS-TABLE is keyed by
                              ;; repository-relative file names (which
                              ;; ROOT-REL-FILE is)
                              (extra (gethash root-rel-file extras-table)))
                          (list display-path state extra)))
                      files-to-report)))
        (funcall update-function result nil))
    ;; FIXME 2026-03-24(Kris B): Is there a cleaner way to deal with
    ;; repository corruption errors?  This solution seems a bit
    ;; fragile and ad hoc...
    ;;
    ;; For errors related to repository corruption (jj emits an exit
    ;; code of 255), report on no files and warn the user about a
    ;; potential problem.  (See bug#63.) Signal other errors normally.
    (error (if (string-match-p "exited with status 255" (error-message-string err))
               (progn
                 (warn "Vc-jj: jj failed, likely due to a corrupted repository (%s)"
                       (vc-jj-root root-or-subdir))
                 (funcall update-function nil nil))
             (signal (car err) (cdr err))))))

;;;; dir-extra-headers

(defun vc-jj-dir-extra-headers (dir)
  "Return extra headers for `vc-dir' when executed inside DIR.

Always add headers for the first line of the description, the change ID,
and the git commit ID of the current change.  If the current change is
named by one or more bookmarks, also add a Bookmarks header.  If the
current change is conflicted, divergent, hidden or immutable, also add a
Status header.  (We do not check for emptiness of the current change
since the user can see that via the list of files below the headers
anyway.)"
  (cl-destructuring-bind
      ;; We have some fixed lines for the current changeset, then at
      ;; the end 5 lines for each parent
      ( change-id change-id-short commit-id commit-id-short
        description bookmarks conflict divergent hidden immutable
        &rest parent-info)
      (let ((default-directory (file-name-as-directory dir)))
        (vc-jj--process-lines nil "log" "--no-graph" "-r" "@" "-T"
                              "concat(
self.change_id().short(8), '\n',
self.change_id().shortest(), '\n',
self.commit_id().short(8), '\n',
self.commit_id().shortest(), '\n',
description.first_line(), '\n',
bookmarks.join(','), '\n',
self.conflict(), '\n',
self.divergent(), '\n',
self.hidden(), '\n',
self.immutable(), '\n',
parents.map(|c| concat(
  c.change_id().short(8), '\n',
  c.change_id().shortest(), '\n',
  c.commit_id().short(8), '\n',
  c.commit_id().shortest(), '\n',
  c.description().first_line(), '\n'
)))"))
    (cl-labels
        ((str (string &optional face prefix)
           ;; format a string
           (cond ((not face) (propertize string 'face 'vc-dir-header-value))
                 ((not prefix) (propertize string 'face face))
                 (t (concat (propertize prefix 'face 'vc-dir-header-value)
                            (propertize string 'face face)))))
         (info (key description change-id change-id-prefix commit-id commit-id-prefix)
           ;; format a changeset info line
           (let ((change-id-suffix (substring change-id (length change-id-prefix)))
                 (commit-id-suffix (substring commit-id (length commit-id-prefix))))
             (concat
              (str (format "% -11s: " key) 'vc-dir-header)
              ;; There's no vc-dir-header-value-emphasis or similar
              ;; face, so we re-use vc-dir-status-up-to-date to render
              ;; the unique prefix
              " "
              (str change-id-suffix 'vc-dir-status-ignored change-id-prefix)
              " "
              (str commit-id-suffix 'vc-dir-status-ignored commit-id-prefix)
              " "
              (if (string-empty-p description)
                  (str "(no description set)")
                (str description))))))
      (let ((status (concat
                     (and (string= conflict "true") "(conflict)")
                     (and (string= divergent "true") "(divergent)")
                     (and (string= hidden "true") "(hidden)")
                     (and (string= immutable "true") "(immutable)")))
            (parent-keys (cl-loop
                          for (change-id change-id-short commit-id commit-id-short description)
                          in (seq-partition parent-info 5)
                          collect (info "Parent" description
                                        change-id change-id-short
                                        commit-id commit-id-short))))
        (string-join
         (seq-remove
          ;; Remove NIL entries because we get empty lines otherwise
          #'null
          (cl-list*
           (info "Changeset" description change-id change-id-short commit-id commit-id-short)
           (unless (string= bookmarks "")
             (concat (str "Bookmarks  : " 'vc-dir-header) (str bookmarks)))
           (unless (string= status "")
             (concat (str "Status     : " 'vc-dir-header) (str status 'vc-dir-status-warning)))
           parent-keys))
         "\n")))))

;;;; dir-printer

(defvar vc-dir-status-mouse-map)
(defvar vc-dir-filename-mouse-map)

(defun vc-jj-dir-printer (fileentry)
  "Insert a file entry in the current buffer.
Like `vc-default-dir-printer' but also shows information for renamed
files.  Used by `vc-dir-mode' to insert file entries into the *vc-dir*
buffer.

FILEENTRY contains information about a given repository file."
  ;; Modified from `vc-default-dir-printer' then modify it, similar to
  ;; `vc-git-dir-printer'
  (let* ((isdir (vc-dir-fileinfo->directory fileentry))
         (display-state (cond (isdir "")
                              ;; the `display-state' slot only exists in Emacs >= 31
                              ((and (functionp 'vc-dir-fileinfo->display-state) (vc-dir-fileinfo->display-state fileentry)))
                              ((vc-dir-fileinfo->state fileentry))))
         (filename (vc-dir-fileinfo->name fileentry))
         (extra (vc-dir-fileinfo->extra fileentry))
         (rename-info (when extra
                        (cons (plist-get extra :rename-state)
                              (plist-get extra :rename-other-filename)))))
    (insert
     (propertize (format "%c" (if (vc-dir-fileinfo->marked fileentry) ?* ?\s))
                 'face 'vc-dir-mark-indicator)
     "   "
     (propertize (format "%-20s" display-state)
                 'face (cond
                        ((eq display-state 'up-to-date) 'vc-dir-status-up-to-date)
                        ((member display-state
                                 '(missing conflict needs-update unlocked-changes
                                           "committing"))
                         'vc-dir-status-warning)
                        ((eq display-state 'ignored) 'vc-dir-status-ignored)
                        (t 'vc-dir-status-edited))
                 'mouse-face 'highlight
                 'keymap vc-dir-status-mouse-map)
     " "
     (propertize (format "%s" filename)
                 'face
                 (if isdir 'vc-dir-directory 'vc-dir-file)
                 'help-echo
                 (if isdir
                     "Directory\nVC operations can be applied to it\nmouse-3: Pop-up menu"
                   "File\nmouse-3: Pop-up menu")
                 'mouse-face 'highlight
                 'keymap vc-dir-filename-mouse-map)
     (if rename-info
         (propertize
          (let ((state (car rename-info))
                (other-filename (cdr rename-info)))
            ;; For the post-rename file entry, should produce output
            ;; like:
            ;;
            ;;   (renamed from BEFORE-FILE-NAME)
            ;;
            ;; For the pre-rename file entry, should produce output
            ;; like:
            ;;
            ;; (renamed to AFTER-FILE-NAME)
            (concat "   ("
                    (if (eq state 'added)
                        "renamed from "
                      "renamed to ")
                    (format "%s" other-filename)
                    ")"))
          'face 'font-lock-comment-face)
       ""))))

;;;; status-fileinfo-extra

;;;; working-revision

(defun vc-jj-working-revision (file)
  "Return the current change id of the repository containing FILE."
  (when-let* ((default-directory (vc-jj-root file)))
    ;; 'jj log' might print a warning at the start of its output,
    ;; e.g., "Warning: Refused to snapshot some files."  The output we
    ;; want will be printed afterwards.
    (car (last (vc-jj--process-lines nil "log" "--no-graph"
                                     "-r" "@"
                                     "-T" (if (bound-and-true-p vc-use-short-revision)
                                              "self.change_id().shortest() ++ '\n'"
                                            "self.change_id() ++ '\n'"))))))

;;;; checkout-model

(defun vc-jj-checkout-model (_files)
  "JJ-specific implementation of `vc-checkout-model'."
  'implicit)

;;;; mode-line-string

(defun vc-jj-mode-line-string (file)
  "Return a mode line string and tooltip for FILE."
  (pcase-let* ((long-rev (vc-jj-working-revision file))
               (`(,short-rev ,description)
                (vc-jj--process-lines nil "log" "--no-graph" "-r" long-rev
                                      "-T" "self.change_id().shortest() ++ '\n' ++ description.first_line() ++ '\n'"))
               (def-ml (vc-default-mode-line-string 'JJ file))
               (help-echo (get-text-property 0 'help-echo def-ml))
               (face   (get-text-property 0 'face def-ml)))
    ;; See docstring of `vc-default-mode-line-string' for a
    ;; description of the string prefix we extract here
    (propertize (concat (substring def-ml 0 3) short-rev)
                'face face
                'help-echo (concat help-echo
                                   "\nCurrent change: " long-rev
                                   " (" description ")"))))

;;; STATE-CHANGING FUNCTIONS

;;;; create-repo

(defun vc-jj-create-repo ()
  "Create an empty jj repository in the current directory."
  (if current-prefix-arg
      (process-file vc-jj-program nil nil nil "git" "init" "--colocate")
    (process-file vc-jj-program nil nil nil "git" "init")))

;;;; register

(defun vc-jj-register (files &optional _comment)
  "Register FILES into the jj version-control system."
  ;; This is usually a no-op since jj auto-registers all files, so we
  ;; just need to run some jj command so new files are picked up.  We
  ;; run "jj file track" for the case where some of FILES are excluded
  ;; via the "snapshot.auto-track" setting or via git's mechanisms
  ;; such as the .gitignore file.
  (vc-jj--command-dispatched nil 0 files "file" "track"))

;;;; responsible-p

(defalias 'vc-jj-responsible-p #'vc-jj-root)

;;;; receive-file

;;;; unregister

;;;; checkin
;; See also the related 'async-checkins' and 'checkin-patch' methods

(defun vc-jj-checkin (files comment &optional _rev)
  "Create a new change from FILES.
Run \"jj commit\" on FILES with a change description provided in
COMMENT.

FILES is a list of repository files or subdirectories to check in.
COMMENT is a change comment (i.e., the content of a log edit buffer,
which contains all log edit headers).

Also see the `vc-async-checkin' user option (available in Emacs 31),
which, when non-nil, makes the jj command of this function run
asynchronously."
  (let* ((description
          ;; 2026-04-05 TODO: We do not use any comment headers aside
          ;; from the assumed "Summary" header.  If in the future
          ;; vc-jj uses additional headers, then we will need to
          ;; extract those headers from COMMENT.
          (car (log-edit-extract-headers nil comment)))
         (args (append (vc-switches 'jj 'checkin)
                       (list "commit" "-m" description))))
    (if (and (bound-and-true-p vc-async-checkin) ; Emacs 31 option
             (vc-jj-async-checkins))
        (let* ((root (vc-jj-root (or (car-safe files) default-directory)))
               (default-directory root)
               (buffer (format "*vc-jj : %s*" (expand-file-name root)))
               (proc (apply #'vc-do-async-command ; Returns process object
                            buffer root vc-jj-program
                            (append (ensure-list vc-jj-global-switches)
                                    args
                                    (mapcar #'vc-jj--filename-to-fileset files)))))
          ;; The following lines are in `vc-git--checkin', so we do
          ;; the same
          (set-process-query-on-exit-flag proc t)
          (vc-wait-for-process-before-save proc "Finishing checking in files...")
          (with-current-buffer buffer
            (vc-run-delayed (vc-compilation-mode 'jj)))
          (vc-set-async-update buffer)
          ;; When `vc-async-checkin' is non-nil, `vc-checkin' expects
          ;; this function to return a list whose car is 'async' and
          ;; whose cdr is the jj process object
          (list 'async proc))
      (apply #'vc-jj--command-dispatched nil 0 files args))))

;;;; checkin-patch

;;;; find-revision

(defun vc-jj-find-revision (file rev buffer)
  "Read revision REV of FILE into BUFFER and return the buffer."
  (let ((revision (vc-jj--command-parseable file "file" "show" "-r" rev)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert revision)))
  buffer)

;;;; checkout

(defun vc-jj-checkout (file &optional rev)
  "Restore the contents of FILE to be the same as in change REV.
If REV is not specified, revert the file as with `vc-jj-revert'."
  ;; TODO: check that this does the right thing: if REV is not
  ;; specified, should we revert or leave FILE unchanged?
  (let ((args (append (and rev (list "--from" rev)))))
    (apply #'vc-jj--command-dispatched nil 0 file "restore" args)))

;;;; revert

(defun vc-jj-revert (file &optional _contents-done)
  "Restore FILE to the state from its parent(s), via \"jj restore\"."
  (vc-jj--command-dispatched nil 0 file "restore"))

;;;; merge-file

;;;; merge-branch

;;;; merge-news

;;;; pull

(defvar vc-jj-pull-history nil
  "History variable for `vc-jj-pull'.")

(defun vc-jj-pull (prompt)
  "Fetch changes from the default repository remote.
Run \"jj git fetch\", possibly with additional command flags.

The default repository remote is the one specified by the \"git.fetch\"
setting.  If that is not configured and there are multiple remotes, the
remote named \"origin\" is fetched.

PROMPT is the prefix argument.  If it is non-nil, prompt the user for
the specific jj command to run."
  (vc-jj--pushpull "fetch" prompt 'vc-jj-pull-history))

;;;; push

(defvar vc-jj-push-history nil
  "History variable for `vc-jj-push'.")

;; We have the work of `vc-jj-push' and `vc-jj-pull' factored out into
;; `vc-jj--pushpull' because both do the exact same thing but with
;; different "jj git" subcommands and history variables
(defun vc-jj--pushpull (subcommand prompt history-var)
  "Subroutine for `vc-jj-push' and `vc-jj-pull'.
SUBCOMMAND is the \"jj git\" subcommand to run (e.g., \"push\").  PROMPT
is the prefix argument; when non-nil, prompt the user to edit the shell
command before it runs.  HISTORY-VAR is the variable name for the
minibuffer history variable associated with SUBCOMMAND."
  ;; Implementation modified from `vc-git--pushpull'
  (let* ((root (vc-jj-root default-directory))
         (default-directory root)
         (buffer (format "*vc-jj : %s*" (expand-file-name root)))
         (jj-program vc-jj-program)
         (command-args (append (ensure-list vc-jj-global-switches)
                               (list "git" subcommand)))
         proc)
    
    ;; Do the command
    (if (boundp 'vc-filter-command-function)
        (let ((vc-filter-command-function
               (if prompt
                   (lambda (&rest args)
                     (let ((vc-user-edit-command-history history-var))
                       ;; As directed by the docstring of
                       ;; `vc-filter-command-function', see `vc-do-command'
                       ;; for what PROGRAM, _FILE-OR-LIST, and FLAGS are
                       (cl-destructuring-bind (&whole args program _file-or-list flags)
                           (apply #'vc-user-edit-command args)
                         ;; Update relevant variables according to the
                         ;; edits the user made from the call to
                         ;; `vc-user-edit-command' above
                         (setq jj-program program
                               command-args flags)
                         args)))
                 vc-filter-command-function)))
          (setq proc (apply #'vc-do-async-command buffer root jj-program command-args)))
      ;; Emacs 28 didn't yet have `vc-filter-command-function' or
      ;; `vc-user-edit-command', so we use the simple
      ;; `read-shell-command' to edit the command when PROMPT is
      ;; non-nil
      (let* ((full-command (cons vc-jj-program command-args))
             (command-list
              (if prompt
                  (split-string-shell-command
                   (read-shell-command "Edit VC command: "
                                       (string-join full-command " ")
                                       history-var))
                full-command)))
        (setq jj-program (car command-list)
              command-args (cdr command-list))
        ;; `vc-do-async-command' returned BUFFER in Emacs 28
        (apply #'vc-do-async-command buffer root jj-program command-args)
        (setq proc (get-buffer-process (get-buffer buffer)))))
    (set-process-query-on-exit-flag proc t)
    
    ;; Set up compilation buffer
    (with-current-buffer buffer
      (vc-run-delayed
        (vc-compilation-mode 'jj)
        (setq-local compile-command (string-join (cons jj-program command-args) " "))
        (setq-local compilation-directory root)
        ;; Either set `compilation-buffer-name-function' locally to
        ;; nil or use `compilation-arguments' to set `name-function'.
        ;; See `compilation-buffer-name'.
        (setq-local compilation-arguments
                    (list compile-command nil
                          (lambda (_name-of-mode) buffer)
                          nil))
        (ansi-color-filter-region (point-min) (point-max))))
    (vc-set-async-update buffer)))

(defun vc-jj-push (prompt)
  "Push changes to the remote of the default tracking bookmark.
Run \"jj git push\", possibly with additional command flags.

PROMPT is the prefix argument.  If it is non-nil, prompt the user for
the specific jj command to run."
  (vc-jj--pushpull "push" prompt 'vc-jj-push-history))

;;;; steal-lock

;;;; get-change-comment

(defun vc-jj-get-change-comment (_files rev)
  "Return the description of REV.
_FILES currently has no effect on this function."
  (vc-jj--command-parseable nil "show" "--no-patch"
                            rev "-T" "description"))

;;;; modify-change-comment

;; TODO: protect immutable changes
(defun vc-jj-modify-change-comment (_files rev comment)
  "Set the change comment of revision REV to COMMENT."
  (let ((comment (car (log-edit-extract-headers () comment))))
    (vc-jj--command-dispatched nil 0 nil "desc" rev "-m" comment "--quiet")))

;;;; mark-resolved

;;;; find-admin-dir

;;;; add-working-tree
;; Emacs 31 method

;;;; delete-working-tree
;; Emacs 31 method

;;;; move-working-tree
;; Emacs 31 method

;;;; delete-revision
;; Emacs 31 method

(defun vc-jj-delete-revision (rev)
  "Abandon REV."
  (vc-jj--command-dispatched nil 0 nil "abandon" rev))

;;;; delete-revisions-from-end
;; Emacs 31 method

;;;; uncommit-revisions-from-end
;; Emacs 31 method

;;; HISTORY FUNCTIONS

;;;; print-log

(defun vc-jj-print-log (files buffer &optional shortlog start-revision limit)
  "Print commit log associated with FILES into specified BUFFER.
If SHORTLOG is non-nil, use a short log format similar to
`vc-jj-root-log-format'.  If START-REVISION is non-nil, it is a string
of the newest revision in the log to show.  If LIMIT is a number, show
no more than this many entries.  If LIMIT is a non-empty string, use it
as a base revision."
  (vc-setup-buffer buffer)
  (let ((inhibit-read-only t)
        (files
         ;; There is a special case when FILES has one element: the
         ;; root of the project (e.g., when calling
         ;; `vc-print-root-log').  In this case, we do not pass a list
         ;; of files to jj because doing do would cause the log to
         ;; only show ancestors of START-REVISION (even if the fileset
         ;; is "all()").  But this behavior is undesirable in
         ;; `vc-print-root-log' (in a Jujutsu context of bookmarks),
         ;; since users expect to see descendants as well.
         (unless (file-equal-p (vc-jj-root (car files)) (car files))
           files))
        (args (append (pcase limit
                        ;; When LIMIT is a number, only show up to
                        ;; that many revisions
                        ((pred numberp)
                         (list "-n" (number-to-string limit)
                               "-r" (concat "::" start-revision)))
                        ;; When LIMIT is a string, it is a revision.
                        ;; In that case, show the revisions between
                        ;; LIMIT and START-REVISION, not including the
                        ;; revision LIMIT.
                        ((pred stringp)
                         (list "-r" (format "%s::%s & ~%s" limit start-revision limit))))
                      (if shortlog
                          (list "-T" (car vc-jj-root-log-format))
                        (list "--no-graph" "-T" "builtin_log_detailed")))))
    (with-current-buffer buffer
      (apply #'vc-jj--command-dispatched buffer 'async files "log" args))))

;;;; log-outgoing

;; (defun vc-jj-log-outgoing (buffer remote-location)
;;   ;; TODO
;;   )

;;;; log-incoming

;; (defun vc-jj-log-incoming (buffer remote-location)
;;   ;; TODO
;;   )

;;;; log-search

(defun vc-jj-log-search (buffer pattern)
  "Display the log of all revisions whose description matches PATTERN.
PATTERN is a regular expression.  The log is in long format and
outputted into BUFFER."
  (let ((args (list "log" "--no-graph"
                    "-r" (format "description(\"%s\")" (or pattern ""))
                    "-T" "builtin_log_detailed")))
    (vc-setup-buffer buffer)
    (apply #'vc-jj--command-dispatched buffer 'async nil args)))

;;;; log-view-mode

(defun vc-jj-log-view-restore-position ()
  "Restore the position of point prior to reverting.
When this function is added to `revert-buffer-restore-functions' in a
`vc-jj-log-view-mode' buffer, after reverting the buffer, restore the
position of the point to the revision the point was on prior to
reverting.  If that revision no longer exists, do not move the point."
  (when-let* ((rev (log-view-current-tag)))
    (lambda ()
      (when (re-search-forward rev nil t)
        (goto-char (match-beginning 0))
        (beginning-of-line)
        ;; Report to the user that we've restored the point, otherwise
        ;; they might think the buffer was not reverted or that
        ;; something has gone wrong (because that would likely be the
        ;; case with the default behavior, without this function)
        (message "Buffer reverted and point restored to revision %s"
                 (propertize (vc-jj--command-parseable nil "show" "--no-patch" rev
                                                       "-T" "change_id.shortest()")
                             'face 'log-view-message))))))

(defun vc-jj--expanded-log-entry (revision)
  "Return a string of the commit details of REVISION.
Called by `log-view-toggle-entry-display' in a JJ Log View buffer."
  (vc-jj--command-parseable
   nil "log" "--no-graph"
   ;; REVISION may be divergent (i.e., several revisions with the same
   ;; change ID).  In those cases, we opt to avoid jj erroring via "-r
   ;; change_id(REVISION)" and show only all the divergent commits.
   ;; This is preferable to confusing or misinforming the user by
   ;; showing only some of the divergent commits.
   "-r" (format "change_id(%s)" revision)
   "-T" "builtin_log_detailed"))

(defvar auto-revert-mode)

;; FIXME 2025-12-07: This only reverts the parent buffer, but there
;; more often than not multiple files in a project that could be
;; reverted.  Do we revert those too?  (Note: there is
;; `vc-auto-revert-mode' in Emacs 31.1.)
(defun vc-jj--reload-log-buffers ()
  "Revert the log view buffer and its parent buffer.
Revert the parent buffer of the current log view buffer then revert the
log view buffer.

The \"parent buffer\" of the log view buffer is the buffer referenced by
the variable `vc-parent-buffer'; it is the buffer in which the log view
buffer was created."
  (when (derived-mode-p 'vc-jj-log-view-mode)
    (when vc-parent-buffer
      (with-current-buffer vc-parent-buffer
        ;; It's only necessary to ask when `auto-revert-mode' isn't
        ;; already enabled in the `vc-parent-buffer'.  If it is, then
        ;; it is safe to assume the user wants that buffer reverted
        ;; automatically
        (revert-buffer nil auto-revert-mode)))
    ;; Revert the log view buffer
    (revert-buffer)))

(defun vc-jj-log-view-edit-change ()
  "Edit the jj revision at point.
Call \"jj edit\" on the revision at point."
  (interactive nil vc-jj-log-view-mode)
  (let ((rev (log-view-current-tag)))
    (vc-jj-retrieve-tag nil rev nil)
    (vc-jj--reload-log-buffers)))

(defun vc-jj-log-view-abandon-change ()
  "Abandon the jj revision at point.
Call \"jj abandon\" on the revision at point."
  (interactive nil vc-jj-log-view-mode)
  (let ((rev (log-view-current-tag)))
    (when (y-or-n-p (format "Abandon revision %s?"
                            (propertize
                             (vc-jj--command-parseable nil "show" "--no-patch" rev
                                                       "-T" "change_id.shortest()")
                             'face 'log-view-message)))
      (vc-jj-delete-revision rev)
      (vc-jj--reload-log-buffers))))

(defun vc-jj-log-view-new-change ()
  "Create a new, empty change on the revision at point.
Call \"jj new\", creating a new revision whose parent is the revision at
point."
  (interactive nil vc-jj-log-view-mode)
  (let ((rev (log-view-current-tag)))
    (vc-jj--command-dispatched nil 0 nil "new" rev "--quiet")
    (vc-jj--reload-log-buffers)))

(defun vc-jj-log-view-bookmark-set ()
  "Set the bookmark of revision at point.
When called in a `vc-jj-log-view-mode' buffer, prompt for a bookmark to
set at the revision at point.  If the bookmark already exists and would
be moved backwards or sideways in the revision history, confirm with the
user first."
  (interactive nil vc-jj-log-view-mode)
  (when (derived-mode-p 'vc-jj-log-view-mode)
    (let* ((target-rev (log-view-current-tag))
           (bookmarks (vc-jj--process-lines nil "bookmark" "list" "-T" "self.name() ++ '\n'"))
           (bookmark (completing-read "Move or create bookmark: " bookmarks))
           (new-bookmark-p (not (member bookmark bookmarks)))
           ;; If the bookmark already exists and target-rev is not a
           ;; descendant of the revision that the bookmark is
           ;; currently on, this means that the bookmark will be moved
           ;; sideways or backwards
           (backwards-move-p
            (unless new-bookmark-p
              ;; If `vc-jj--process-lines' returns nil, then that
              ;; means the jj command returned no revisions.  This
              ;; REVSET is the intersection between BOOKMARK, its
              ;; descendants, and TARGET-REV.  That intersection is
              ;; non-empty only when TARGET-REV is BOOKMARK or a
              ;; descendant or BOOKMARK
              (not (vc-jj--process-lines nil "log" "-r" (format "%s & %s::" target-rev bookmark))))))
      (when backwards-move-p
        (unless (yes-or-no-p
                 (format-prompt "Moving bookmark %s to revision %s would move it either backwards or sideways. Is this okay?"
                                nil bookmark target-rev bookmark))
          (user-error "Aborted moving bookmark %s to revision %s" bookmark target-rev)))
      (vc-jj--command-dispatched nil 0 nil "bookmark" "set" bookmark "-r" target-rev
                                 "--allow-backwards" "--quiet")
      (revert-buffer))))

(defun vc-jj-log-view-bookmark-rename ()
  "Rename a bookmark pointing to the revision at point.
When called in a `vc-jj-log-view-mode' buffer, rename the bookmark
pointing to the revision at point.  If there are multiple bookmarks
pointing to the revision, prompt the user to one of these bookmarks to
rename."
  (interactive nil vc-jj-log-view-mode)
  (when (derived-mode-p 'vc-jj-log-view-mode)
    (let* ((target-rev (log-view-current-tag))
           (bookmarks-at-rev
            (or (vc-jj--process-lines nil "bookmark" "list" "-r" target-rev
                                      "-T" "if(!self.remote(), self.name() ++ '\n')")
                (user-error "No bookmarks at %s"
                            (propertize
                             (vc-jj--command-parseable nil "show" "--no-patch" target-rev
                                                       "-T" "change_id.shortest()")
                             'face 'log-view-message))))
           (bookmark-old
            (if (= 1 (length bookmarks-at-rev))
                (car bookmarks-at-rev)
              (completing-read "Which bookmark to rename? " bookmarks-at-rev)))
           (bookmark-new
            (read-string (format-prompt "Rename %s to" nil bookmark-old)
                         nil t bookmark-old)))
      (vc-jj--command-dispatched nil 0 nil "bookmark" "rename" bookmark-old bookmark-new
                                 "--quiet")
      (revert-buffer))))

(defun vc-jj-log-view-bookmark-delete ()
  "Delete bookmark of the revision at point.
When called in a `vc-jj-log-view-mode' buffer, delete the bookmark of
the revision at point.  If there are multiple bookmarks attached to the
revision, prompt the user to choose one or more of these bookmarks to
delete."
  (interactive nil vc-jj-log-view-mode)
  (when (derived-mode-p 'vc-jj-log-view-mode)
    (let* ((rev (log-view-current-tag))
           (revision-bookmarks
            (string-split
             (vc-jj--command-parseable
              nil "show" "--no-patch" rev
              "-T" "self.local_bookmarks().map(|b| b.name()) ++ '\n'")
             " " t "\n"))
           (bookmarks
            (if (< 1 (length revision-bookmarks))
                (completing-read-multiple "Delete bookmarks: " revision-bookmarks nil t)
              revision-bookmarks)))
      (apply #'vc-jj--command-dispatched nil 0 nil "--quiet" "bookmark" "delete" bookmarks)
      (revert-buffer))))

(defvar vc-jj-log-view-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "r" #'vc-jj-log-view-edit-change)
    (keymap-set map "x" #'vc-jj-log-view-abandon-change)
    (keymap-set map "i" #'vc-jj-log-view-new-change)
    (keymap-set map "b s" #'vc-jj-log-view-bookmark-set)
    (keymap-set map "b r" #'vc-jj-log-view-bookmark-rename)
    (keymap-set map "b D" #'vc-jj-log-view-bookmark-delete)
    map)
  "Keymap for `vc-jj-log-view-mode'.")

(define-derived-mode vc-jj-log-view-mode log-view-mode "JJ-Log-View"
  "Log View mode specific for JJ."
  :keymap vc-jj-log-view-mode-map

  (require 'add-log) ;; We need the faces add-log.
  ;; Don't have file markers, so use impossible regexp.
  (setq-local log-view-file-re regexp-unmatchable)
  (setq-local log-view-per-file-logs nil)
  ;; The `log-view-message-re' is a regexp matching a revision.  Its
  ;; first match group must match the revision number itself
  (setq-local log-view-message-re
              (if (not (memq vc-log-view-type '(long log-search with-diff)))
                  (cadr vc-jj-root-log-format)
                (rx bol "Commit ID: " (1+ (any alnum)) "\n"
                    "Change ID: " (group (1+ (any alpha))))))
  ;; Allow expanding short log entries.
  (when (memq vc-log-view-type '(short log-outgoing log-incoming mergebase))
    (setq truncate-lines t)
    (setq-local log-view-expanded-log-entry-function 'vc-jj--expanded-log-entry))
  ;; Fontify according to regexp capture groups (for "short" format
  ;; log views, the relevant regexp is found in
  ;; `vc-jj-root-log-format')
  (setq-local log-view-font-lock-keywords
              (if (not (memq vc-log-view-type '(long log-search with-diff)))
                  (list (cons (nth 1 vc-jj-root-log-format)
                              (nth 2 vc-jj-root-log-format)))
                `((,log-view-message-re
                   (1 'change-log-acknowledgment))
                  (,(rx bol "Commit ID" (0+ (any space)) ": " (group (1+ (any alnum))) "\n")
                   (1 'vc-jj-log-view-commit))
                  (,(rx bol "Bookmarks" (0+ (any space)) ": " (group (1+ not-newline)) "\n")
                   (1 'vc-jj-log-view-bookmark))
                  (,(rx bol (or "Author" "Committer") (0+ (any space)) ": "
                        ;; Name
                        (group (+? anything))
                        ;; Email (based on `goto-address-mail-regexp')
                        (group " <"
                               (+ (or alnum (any "-=._+")))
                               "@"
                               (+ (seq (+ (or alnum (any "-_"))) "."))
                               (+ alnum)
                               "> ")
                        ;; Date and time
                        "(" (group (+ (or digit space (any "-:")))) ")\n")
                   (1 'change-log-name)
                   (2 'change-log-email)
                   (3 'change-log-date)))))

  (when (boundp 'revert-buffer-restore-functions) ; Emacs 30.1
    (add-hook 'revert-buffer-restore-functions #'vc-jj-log-view-restore-position nil t)))

;;;; show-log-entry

(defun vc-jj-show-log-entry (revision)
  "Move to the log entry for REVISION."
  ;; TODO: check that this works for all forms of log output, or at
  ;; least the predefined ones
  (goto-char (point-min))
  (when (search-forward-regexp
         ;; TODO: reformulate using `rx'
         (concat "^[^|]\\s-+\\(" (regexp-quote revision) "\\)\\s-+")
         nil t)
    (goto-char (match-beginning 1))))

;;;; comment-history

;;;; update-changelog

;;;; diff

(defun vc-jj-diff (files &optional rev1 rev2 buffer async)
  "Display a diff for FILES between revisions REV1 and REV2.
FILES is a list of file paths.  REV1 and REV2 are the full change IDs of
two revisions.  REV1 is the earlier revision and REV2 is the later
revision.

When BUFFER is non-nil, it is the buffer object or name to insert the
diff into.  Otherwise, when nil, insert the diff into the *vc-diff*
buffer.  If ASYNC is non-nil, run the jj command of this function
asynchronously."
  (setq buffer (or buffer "*vc-diff*"))
  (cond
   ((not (or rev1 rev2))
    ;; Use `vc-jj-previous-revision' instead of "@-" because the
    ;; former handles edge cases like e.g. multiple parents
    (setq rev1 (vc-jj-previous-revision nil "@")))
   ((null rev1)
    (setq rev1 "root()")))
  (setq rev2 (or rev2 "@"))
  (let* ((inhibit-read-only t)
         (root (and files (vc-jj-root (car files))))
         (default-directory (or root default-directory))
         (fileset (mapcar #'vc-jj--filename-to-fileset files))
         ;; When REV1 and REV2 are the same revision, "-f REV1 -t
         ;; REV2" (erroneously) returns an empty diff.  So we check
         ;; for that case and use "-r REV1" instead, which returns the
         ;; correct diff
         (args (append (vc-switches 'jj 'diff)
                       (if (string= rev1 rev2)
                           (list "-r" rev1)
                         (list "-f" rev1 "-t" rev2))
                       fileset)))
    ;; Mimic `vc-git-diff' by returning the value of
    ;; `vc-jj--command-dispatched'.
    ;;
    ;; Also ensure that (vc-jj--command-dispatched BUFFER ...) is
    ;; called when BUFFER is not the current buffer.  Otherwise,
    ;; BUFFER may accumulate content from previous invocations of
    ;; `vc-jj-diff', because `vc-do-command' (called internally by
    ;; `vc-jj--command-dispatched') only erases BUFFER when BUFFER is
    ;; not the current buffer.  See bug#152 for more information.
    (prog1
        (apply #'vc-jj--command-dispatched buffer (if async 'async 0) nil "diff" args)
      (with-current-buffer buffer
        (vc-run-delayed
          (ansi-color-filter-region (point-min) (point-max)))))))

;;;; revision-completion-table

(defun vc-jj--change-annotation-function (cand change-desc-table)
  "Return a propertized change description for CAND.
CAND is a change ID as a string, possibly with a change offset, if it is
a divergent change.  CHANGE-DESC-TABLE is a hash table mapping change
IDs (optionally with offsets) to the first line of each change's
description."
  (let ((description (gethash cand change-desc-table)))
    (format " %s" (propertize description 'face 'completions-annotations))))

(defun vc-jj-revision-completion-table (files)
  "Return a completion table for revisions that changed any file in FILES.
FILES is a list of repository files."
  ;; Considering performance is crucial for this function since it
  ;; will often call for information on a significant portion of the
  ;; repository history.
  (let* (;; Hash table from change ID (with a change offset if it is
         ;; divergent) to first line of its description
         (change-desc-table (make-hash-table :test #'equal))
         ;; Populate CHANGE-DESC-TABLE and pass it to
         ;; `vc-jj--change-annotation-function' so that it already has
         ;; all the information it needs.  This way, we do not need to
         ;; call jj in that function every time we need to retrieve
         ;; e.g. the change description
         (annotation-fn
          (lambda (cand)
            (vc-jj--change-annotation-function cand change-desc-table))))
    
    ;; Implementation note: we choose to output the jj result into a
    ;; buffer and directly operate in that buffer, as opposed to
    ;; retrieving the text of the buffer and manipulating strings.
    ;; The reason is performance: manipulation and operations in and
    ;; on buffers is much faster and induces significantly less GC
    ;; pressure compared to converting that buffer's text into a
    ;; string or a lists of strings (i.e., a bunch of Lisp objects)
    ;; then operating on them.
    (with-temp-buffer
      (apply #'vc-jj--call nil t "log" "--no-graph"
             "-T" "concat(
  self.change_id(), '\t',
  self.change_offset(), '\t',
  self.divergent(), '\t',
  self.description().first_line(),
  '\n'
)"
             (mapcar #'vc-jj--filename-to-fileset files))
      (goto-char (point-min))
      (while (not (eobp))
        (re-search-forward (rx bol
                               (group (+ (any lower-case))) "\t"
                               (group (+ digit)) "\t"
                               (group (or "true" "false")) "\t"
                               (group (*? anything)) eol)
                           nil t)
        ;; Starting jj v0.37.0, change IDs of divergent changes are
        ;; made unique by a numeric suffix, a change offset.  The 0
        ;; offset points to the most recent commit of a divergent
        ;; change ID.  We need to pass the change ID with its offset
        ;; to jj for divergent commits, otherwise jj will error.
        (let* ((change-id (match-string 1))
               (change-offset (match-string 2))
               (divergentp (string-equal "true" (match-string 3)))
               (change-id-with-offset
                (concat change-id (when divergentp (format "/%s" change-offset))))
               (desc-first-line (match-string 4)))
          (puthash change-id-with-offset desc-first-line change-desc-table))
        (forward-line 1)))
    
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata . ((display-sort-function . ,#'identity) ; Retain chronological order
                        (annotation-function . ,annotation-fn)))
        (complete-with-action action change-desc-table string pred)))))

;;;; annotate-command

(defun vc-jj-annotate-command (file buf &optional rev)
  "Fill BUF with per-line change history of FILE at REV."
  (let ((rev (or rev "@")))
    ;; Contrary to most other jj commands, 'jj file annotate' takes a
    ;; path instead of a fileset expression, so we append FILE to the
    ;; unprocessed argument list here.
    (apply #'vc-jj--command-dispatched buf 'async nil "file" "annotate"
           (append (vc-switches 'jj 'annotate)
                   (list "-r" rev
                         ;; "jj file annotate" expects a file path,
                         ;; not a fileset expression
                         (file-relative-name file))))))

;;;; annotate-time

(defconst vc-jj--annotation-line-prefix-re
  (rx bol
      (group (+ (any "a-z")))           ; change id
      " "
      (group (+? anychar))              ; author username
      (+ " ")
      (group                            ; iso 8601-ish datetime
       (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) " "
       (= 2 digit) ":" (= 2 digit) ":" (= 2 digit))
      (+ " ")
      (group (+ digit))                 ; line number
      ": ")
  ;; TODO: find out if the output changes when the file got renamed
  ;; somewhere in its history
  "Regex for the output of \"jj file annotate\".
The regex matches each line's change information and captures
four groups: change id, author, datetime, line number.")

(defun vc-jj-annotate-time ()
  "Return the time for the annotated line."
  (and-let*
      (((re-search-forward vc-jj--annotation-line-prefix-re nil t))
       (dt (match-string 3))
       (dt (and dt (string-replace " " "T" dt)))
       (decoded (ignore-errors (iso8601-parse dt))))
    (vc-annotate-convert-time
     (encode-time (decoded-time-set-defaults decoded)))))

;;;; annotate-current-time

;;;; annotate-extract-revision-at-line

(defun vc-jj-annotate-extract-revision-at-line ()
  "Return the revision (change id) for the annotated line."
  (save-excursion
    (beginning-of-line)
    (when (looking-at vc-jj--annotation-line-prefix-re)
      (match-string-no-properties 1))))

;;;; region-history

(defun vc-jj-region-history (file buffer lfrom lto)
  "JJ-specific version of `vc-region-history'.
Insert into BUFFER the history (log comments and diffs) of the content
of FILE between lines LFROM and LTO."
  ;; Support for vc-region-history via the git version which works
  ;; fine at least when co-located with git.
  (vc-git-region-history file buffer lfrom lto))

;;;; region-history-mode

(define-derived-mode vc-jj-region-history-mode vc-git-region-history-mode
  "JJ/Git-Region-History")

;;;; mergebase

;;;; last-change

;;;; revision-published-p
;; Emacs 31 method

(defun vc-jj-revision-published-p (rev)
  "Whether REV is pushed or not.
Return t when REV has been pushed to a remote repository, and nil
otherwise."
  (member rev
          (vc-jj--process-lines nil "log" "--no-graph"
                                "-r" "mutable()"
                                "-T" "self.change_id() ++ '\n'")))

;;; TAG/BRANCH SYSTEM

;;;; create-tag

(defun vc-jj-create-tag (_dir name branchp)
  "Attach tag named NAME to the current revision.
When BRANCHP is non-nil, a bookmark named NAME is created at the current
revision.

Since jujutsu does not support tagging revisions, a nil value of BRANCHP
has no effect."
  (if branchp
      (vc-jj--command-dispatched nil 0 nil "bookmark" "create" name "--quiet")
    (user-error "Setting tags is not supported by jujutsu")))

;;;; retrieve-tag

(defun vc-jj-retrieve-tag (_dir rev _update)
  "Call jj edit on REV inside DIR.
REV is the change ID of a jj revision.

_DIR and _UPDATE are as described in the vc.el specification."
  (vc-jj--command-dispatched nil 0 nil "edit" rev "--quiet"))

;;; MISCELLANEOUS

;;;; make-version-backups-p

;;;; root

(defun vc-jj-root (file)
  "Return the root of the repository containing FILE.
Return NIL if FILE is not in a jj repository."
  (vc-find-root file ".jj"))

;;;; ignore

(defun vc-jj-ignore (file &optional directory remove)
  "Ignore FILE under DIRECTORY.

FILE is a wildcard specification relative to DIRECTORY.
DIRECTORY defaults to `default-directory'.

If REMOVE is non-nil, remove FILE from ignored files instead.

For jj, modify `.gitignore' and call `jj untrack' or `jj track'."
  (vc-default-ignore 'Git file directory remove)
  (let ((default-directory
         (if directory (file-name-as-directory directory)
           default-directory)))
    (vc-jj--command-dispatched nil 0 file "file" (if remove "track" "untrack"))))

;;;; ignore-completion-table

;;;; find-ignore-file

(defun vc-jj-find-ignore-file (file)
  "Return the jj ignore file that controls FILE."
  ;; Currently, Jujutsu uses .gitignore files, even for non-colocated
  ;; repos.  See
  ;; https://docs.jj-vcs.dev/latest/working-copy/#ignored-files
  (expand-file-name ".gitignore" (vc-jj-root file)))

;;;; previous-revision

(defun vc-jj-previous-revision (file rev)
  "JJ-specific version of `vc-previous-revision'.
Return the revision number that precedes REV for FILE, or nil if no such
revision exists."
  (let ((template (if (bound-and-true-p vc-use-short-revision)
                      "self.change_id().shortest()"
                    "self.change_id()")))
    (if file
        (vc-jj--command-parseable file "log" "--no-graph" "--limit" "1"
                                  "-r" (format "ancestors(%s) & ~%s" rev rev)
                                  "-T" template)
      ;; The jj manual states that "for merges, [first_parent] only
      ;; returns the first parent instead of returning all parents";
      ;; given the choice, we do want to return the first parent of a
      ;; merge change.
      (vc-jj--command-parseable nil "log" "--no-graph"
                                "-r" (concat "first_parent(" rev ")")
                                "-T" template))))

;;;; file-name-changes

;;;; next-revision

(defun vc-jj-next-revision (file rev)
  "JJ-specific version of `vc-next-revision'.
Return the revision that follows REV for FILE, or nil if no such
revision exists."
  (let ((template (if (bound-and-true-p vc-use-short-revision)
                      "self.change_id().shortest()"
                    "self.change_id()")))
    (if file
        (vc-jj--command-parseable file "log" "--no-graph" "--limit" "1"
                                  "-r" (concat "descendants(" rev ")")
                                  "-T" template)
      ;; Note: experimentally, jj (as of 0.35.0) prints children in
      ;; LIFO order (newest child first), but we should not rely on
      ;; that behavior and since none of the children of a change are
      ;; special, we return an arbitrary one.
      (vc-jj--command-parseable nil "log" "--no-graph" "--limit" "1"
                                "-r" (concat "children(" rev ")")
                                "-T" template))))

;;;; log-edit-mode

;; Set up `log-edit-mode' to handle jj description files.

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jjdescription\\'" . log-edit-mode))
;;;###autoload
(add-hook 'vc-log-mode-hook #'vc-jj-ensure-log-edit-callback)
;;;###autoload
(add-hook 'log-edit-mode-hook #'vc-jj-ensure-log-edit-callback)
;;;###autoload
(defun vc-jj-ensure-log-edit-callback ()
  "Set up `log-edit-callback' when editing jj change descriptions."
  (unless (bound-and-true-p log-edit-callback)
    (setq-local log-edit-callback (lambda ()
                                    (interactive)
                                    (save-buffer)
                                    (kill-buffer)))))

;;;; check-headers

;;;; delete-file

(defun vc-jj-delete-file (file)
  "Delete FILE and make sure jj registers the change."
  (when (file-exists-p file)
    (delete-file file)
    (vc-jj--command-dispatched nil 0 nil "status")))

;;;; rename-file

(defun vc-jj-rename-file (old new)
  "Rename file OLD to NEW and make sure jj registers the change."
  (rename-file old new)
  (vc-jj--command-dispatched nil 0 nil "status"))

;;;; find-file-hook

;;;; extra-menu

;;;; extra-dir-menu

;;;; conflicted-files

;;;; repository-url

;;;; prepare-patch

;;;; clone

(defun vc-jj-clone (remote directory rev)
  "Attempt to clone REMOTE repository into DIRECTORY at revision REV.
On failure, return nil.  Upon success, return DIRECTORY."
  (let ((successp (ignore-errors
                    (vc-jj--command-dispatched nil 0 nil "git" "clone" "--colocate" remote directory))))
    (when (and successp rev)
      (let ((default-directory directory))
        (vc-jj--command-dispatched nil 0 nil "new" rev "--quiet")))
    (when successp directory)))

;;; project.el support

(declare-function vc-default-project-list-files "project" (backend dir extra-ignores))

(defun vc-jj-project-list-files (dir extra-ignores)
  "List all files in directory DIR.
Do not include files matching glob patterns in EXTRA-IGNORES in the
result.

This function is called by `project-files' as of project.el 0.11.2.  The
value of EXTRA-IGNORES comes from the variable `project-vc-ignores'.

If EXTRA-IGNORES is non-nil, this implementation falls back to a generic
implementation."
  (if extra-ignores
      ;; Same fallback as in project.el for git <= 2.13.  Note:
      ;; currently includes the contents of the `.jj' directory so we
      ;; don't want to merge this as-is.
      ;; TODO: handle extra-ignores parameter better -- rewrite each
      ;; element e to be `~root-glob:"e"' and use `&' to combine them?
      (vc-default-project-list-files 'JJ dir extra-ignores)
    (let* ((default-directory (expand-file-name (vc-jj-root dir)))
           (files (vc-jj--process-lines (file-relative-name dir)
                                        "file" "list")))
      (mapcar #'expand-file-name files))))

;;;###autoload
(with-eval-after-load 'project
  ;; Note: since jj repositories are co-located by default and
  ;; therefore have both '.jj' and '.git' directories, this entry
  ;; needs to come before the one for Git.  Using
  ;; `with-eval-after-load' because of auto-loading, see pr#195.
  (add-to-list 'project-vc-backend-markers-alist '(JJ . ".jj")))

(provide 'vc-jj)
;;; vc-jj.el ends here
