;;; vc-jj.el --- VC backend for the Jujutsu version control system -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Wojciech Siewierski

;; Author: Wojciech Siewierski
;;         Rudolf Schlatte <rudi@constantly.at>
;; URL: https://codeberg.org/emacs-jj-vc/vc-jj.el
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (compat "29.4"))
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

;; A backend for vc.el to handle Jujutsu repositories.

;;; Code:

(require 'seq)

(autoload 'vc-switches "vc")
(autoload 'ansi-color-apply-on-region "ansi-color")
(autoload 'iso8601-parse "iso8601")
(autoload 'decoded-time-set-defaults "time-date")

(add-to-list 'vc-handled-backends 'JJ)

(defun vc-jj-revision-granularity () 'repository)
(defun vc-jj-checkout-model (_files) 'implicit)
(defun vc-jj-update-on-retrieve-tag () nil)

(defgroup vc-jj nil
  "VC Jujutsu backend."
  :group 'vc)

(defcustom vc-jj-colorize-log t
  "Control whether to have jj colorize the log."
  :type 'boolean)

(defcustom vc-jj-log-template "builtin_log_compact"
  "The template to use for `vc-print-log'."
  :type '(radio (const "builtin_log_oneline")
                (const "builtin_log_compact")
                (const "builtin_log_comfortable")
                (const "builtin_log_compact_full_description")
                (const "builtin_log_detailed")
                (string :tag "Custom template")))

(defun vc-jj--file-tracked (file)
  (with-temp-buffer
    (and (= 0 (call-process "jj" nil t nil "file" "list" "--" file))
         (not (= (point-min) (point-max))))))

(defun vc-jj--file-modified (file)
  (with-temp-buffer
    (and (= 0 (call-process "jj" nil t nil "diff" "--summary" "--" file))
         (not (= (point-min) (point-max)))
         (progn (goto-char (point-min)) (looking-at "M ")))))

(defun vc-jj--file-added (file)
  (with-temp-buffer
    (and (= 0 (call-process "jj" nil t nil "diff" "--summary" "--" file))
         (not (= (point-min) (point-max)))
         (progn (goto-char (point-min)) (looking-at "A ")))))

(defun vc-jj--file-conflicted (file)
  (with-temp-buffer
    (and (= 0 (call-process "jj" nil t nil "resolve" "--list" "--" file))
         (not (= (point-min) (point-max)))
         (progn (goto-char (point-min)) (looking-at file)))))

;;;###autoload (defun vc-jj-registered (file)
;;;###autoload   "Return non-nil if FILE is registered with jj."
;;;###autoload   (if (and (vc-find-root file ".jj")   ; Short cut.
;;;###autoload            (executable-find "jj"))
;;;###autoload       (progn
;;;###autoload         (load "vc-jj" nil t)
;;;###autoload         (vc-jj-registered file))))

(defun vc-jj-registered (file)
  (when (executable-find "jj")
    (unless (not (file-exists-p default-directory))
      (with-demoted-errors "Error: %S"
        (when-let ((root (vc-jj-root file)))
          (let* ((default-directory root)
                 (relative (file-relative-name file)))
            (vc-jj--file-tracked relative)))))))

(defun vc-jj-state (file)
  "JJ implementation of `vc-state' for FILE."
  (when-let ((root (vc-jj-root file)))
    (let* ((default-directory root)
           (relative (file-relative-name file)))
      (cond
       ((vc-jj--file-conflicted relative)
        'conflict)
       ((vc-jj--file-modified relative)
        'edited)
       ((vc-jj--file-added relative)
        'added)
       ((vc-jj--file-tracked relative)
        'up-to-date)
       (t nil)))))

(defun vc-jj-dir-status-files (dir _files update-function)
  "Calculate a list of (FILE STATE EXTRA) entries for DIR.
The list is passed to UPDATE-FUNCTION."
  ;; TODO: could be async!
  (let* ((dir (expand-file-name dir))
         (files (process-lines "jj" "file" "list" "--" dir))
         (changed-files (process-lines "jj" "diff" "--summary" "--" dir))
         (added (mapcar (lambda (entry) (substring entry 2))
                        (seq-filter (lambda (file) (string-prefix-p "A " file))
                                    changed-files)))
         (modified (mapcar (lambda (entry) (substring entry 2))
                        (seq-filter (lambda (file) (string-prefix-p "M " file))
                                    changed-files)))
         ;; The output of `jj resolve --list' is a list of file names
         ;; plus a conflict description per line -- rather than trying
         ;; to be fancy and parsing each line (and getting bugs with
         ;; file names with spaces), use `string-prefix-p' later.
         ;; Also, the command errors when there are no conflicts.
         (conflicted (process-lines-ignore-status "jj" "resolve" "--list")))
    (let ((result
           (mapcar
            (lambda (file)
              (let ((vc-state
                     (cond ((seq-find (lambda (e) (string-prefix-p file e)) conflicted) 'conflict)
                           ((member file added) 'added)
                           ((member file modified) 'edited)
                           (t 'up-to-date))))
                (list file vc-state)))
            files)))
      (funcall update-function result nil))))

(defun vc-jj-dir-extra-headers (dir)
  "Return extra headers for DIR.
Always add the first line of the description, the change ID, and
the git commit ID of the current change.  If the current change
is named by one or more bookmarks, also add a Bookmarks header.
If the current change is conflicted, divergent or hidden, also
add a Status header.  (We do not check for emptiness of the
current change since the user can see that via the list of files
below the headers anyway.)"
  (pcase-let* ((default-directory dir)
               (`( ,change-id ,change-id-short ,commit-id ,commit-id-short
                   ,description ,bookmarks ,conflict ,divergent ,hidden)
                (process-lines "jj" "log" "--no-graph" "-r" "@" "-T"
                               "concat(
self.change_id().short(), \"\\n\",
self.change_id().shortest(), \"\\n\",
self.commit_id().short(), \"\\n\",
self.commit_id().shortest(), \"\\n\",
description.first_line(), \"\\n\",
bookmarks.join(\",\"), \"\\n\",
self.conflict(), \"\\n\",
self.divergent(), \"\\n\",
self.hidden(), \"\\n\"
)"))
               (status (concat
                        (when (string= conflict "true") "(conflict)")
                        (when (string= divergent "true") "(divergent)")
                        (when (string= hidden "true") "(hidden)")))
               (change-id-suffix (substring change-id (length change-id-short)))
               (commit-id-suffix (substring commit-id (length commit-id-short))))
    (cl-flet ((fmt (key value &optional prefix)
                  (concat
                   (propertize (format "% -11s: " key) 'face 'vc-dir-header)
                   ;; there is no header value emphasis face, so we
                   ;; use vc-dir-status-up-to-date for the prefix.
                   (when prefix (propertize prefix 'face 'vc-dir-status-up-to-date))
                   (propertize value 'face 'vc-dir-header-value))))
      (string-join (seq-remove
                        #'null
                        (list
                         (fmt "Description" (if (string= description "") "(no description set)" description))
                         (fmt "Change ID" change-id-suffix change-id-short)
                         (fmt "Commit" commit-id-suffix commit-id-short)
                         (unless (string= bookmarks "") (fmt "Bookmarks" bookmarks))
                         (unless (string= status "")
                           ;; open-code this line instead of adding a
                           ;; `face' parameter to `fmt'
                           (concat
                            (propertize (format "% -11s: " "Status") 'face 'vc-dir-header)
                            (propertize status 'face 'vc-dir-status-warning)))))
                       "\n"))))

(defun vc-jj-working-revision (file)
  (when-let ((default-directory (vc-jj-root file)))
    (car (process-lines "jj" "log" "--no-graph"
                        "-r" "@"
                        "-T" "self.change_id().short() ++ \"\\n\""))))

(defun vc-jj-mode-line-string (file)
  "Return a mode line string and tooltip for FILE."
  (pcase-let* ((long-rev (vc-jj-working-revision file))
               (`(,short-rev ,description)
                (process-lines "jj" "log" "--no-graph" "-r" long-rev
                               "-T" "self.change_id().shortest() ++ \"\\n\" ++ description.first_line() ++ \"\\n\""))
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

(defun vc-jj-create-repo ()
  (if current-prefix-arg
      (call-process "jj" nil nil nil "git" "init" "--colocate")
    (call-process "jj" nil nil nil "git" "init")))

(defun vc-jj-register (_files &optional _comment)
  ;; No action needed.
  )

(defun vc-jj-delete-file (file)
  (when (file-exists-p file)
    (delete-file file)))

(defun vc-jj-rename-file (old new)
  (rename-file old new))

(defun vc-jj-checkin (files comment &optional _rev)
  (setq comment (replace-regexp-in-string "\\`Summary: " "" comment))
  (let ((args (append (vc-switches 'jj 'checkin) (list "--") files)))
    (apply #'call-process "jj" nil nil nil "commit" "-m" comment args)))

(defun vc-jj-find-revision (file rev buffer)
  (call-process "jj" nil buffer nil "file" "show" "-r" rev "--" file))

(defun vc-jj-checkout (file &optional rev)
  (let ((args (if rev
                  (list "--from" rev "--" file)
                (list "--" file))))
    (call-process "jj" nil nil nil "restore" args)))

(defun vc-jj-revert (file &optional _contents-done)
  (call-process "jj" nil nil nil "restore" "--" file))

(defun vc-jj-print-log (files buffer &optional _shortlog start-revision limit)
  "Print commit log associated with FILES into specified BUFFER."
  ;; FIXME: limit can be a revision string, in which case we should
  ;; print revisions between start-revision and limit
  (let ((inhibit-read-only t)
        (args (append
               (when limit
                 (list "-n" (number-to-string limit)))
               (when start-revision
                 (list "-r" (concat ".." start-revision)))
               (when vc-jj-colorize-log (list "--color" "always"))
               (list "-T" vc-jj-log-template "--")
               files)))
    (with-current-buffer buffer (erase-buffer))
    (apply #'call-process "jj" nil buffer nil "log" args)
    (when vc-jj-colorize-log
      (with-current-buffer buffer
        (ansi-color-apply-on-region (point-min) (point-max)))))
  (goto-char (point-min)))

(defun vc-jj-show-log-entry (revision)
  (goto-char (point-min))
  (when (search-forward-regexp
         (concat "^[^|]\\s-+\\(" (regexp-quote revision) "\\)\\s-+")
         nil t)
    (goto-char (match-beginning 1))))

;; (defun vc-jj-log-outgoing (buffer remote-location)
;;   ;; TODO
;;   )
;; (defun vc-jj-log-incoming (buffer remote-location)
;;   ;; TODO
;;   )

(defun vc-jj-root (_file)
  (with-temp-buffer
    (when (= 0 (call-process "jj" nil (list t nil) nil "root"))
      (buffer-substring (point-min) (1- (point-max))))))

(defalias 'vc-jj-responsible-p #'vc-jj-root)

(defun vc-jj-find-ignore-file (file)
  "Return the .gitignore file that controls FILE."
  (let ((root (vc-jj-root file))
        (ignore (expand-file-name
                 (locate-dominating-file default-directory
                                         ".gitignore"))))
    (expand-file-name
     ".gitignore"
     (if (string-prefix-p (file-name-as-directory root)
                          (file-name-as-directory ignore))
         ignore
       root))))

(defun vc-jj-ignore (file &optional directory remove)
  "Ignore FILE under DIRECTORY.

FILE is a wildcard specification relative to DIRECTORY.
DIRECTORY defaults to `default-directory'.

If REMOVE is non-nil, remove FILE from ignored files instead.

For jj, modify `.gitignore' and call `jj untrack' or `jj track'."
  (let ((ignore (expand-file-name ".gitignore" directory)))
    (cond
     (remove
      (vc--remove-regexp (concat "^" (regexp-quote file) "\\(\n\\|$\\)") ignore)
      (let ((default-directory directory))
        (call-process "jj" nil (list t nil) nil "file" "track" file)))
     (t
      (vc--add-line file ignore)
      (let ((default-directory directory))
        (call-process "jj" nil (list t nil) nil "file" "untrack" file))))))

(defvar vc-jj-diff-switches '("--git"))

(defun vc-jj-diff (files &optional rev1 rev2 buffer _async)
  ;; TODO: handle async
  (setq buffer (get-buffer-create (or buffer "*vc-diff*")))
  (cond
   ((and (null rev1)
         (null rev2))
    (setq rev1 "@-"))
   ((null rev1)
    (setq rev1 "root()")))
  (setq rev2 (or rev2 "@"))
  (let ((inhibit-read-only t)
        (args (append (vc-switches 'jj 'diff) (list "--") files)))
    (with-current-buffer buffer
      (erase-buffer))
    (apply #'call-process "jj" nil buffer nil "diff" "--from" rev1 "--to" rev2 args)
    (if (seq-some #'vc-jj--file-modified files)
        1
      0)))

(defun vc-jj-annotate-command (file buf &optional rev)
  (with-current-buffer buf
    (let ((rev (or rev "@")))
      (call-process "jj" nil t nil "file" "annotate" "-r" rev file))))

(defconst vc-jj--annotation-line-prefix-re
  (rx (: bol
         (group (+ (any "a-z")))        ; change id
         " "
         (group (+ (any alnum)))        ; author
         (+ " ")
         (group                         ; iso 8601-ish datetime
          (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) " "
          (= 2 digit) ":" (= 2 digit) ":" (= 2 digit))
         (+ " ")
         (group (+ (any "0-9")))        ; line number
         ": "))
  ;; TODO: find out if the output changes when the file got renamed
  ;; somewhere in its history
  "Regexp for the per-line prefix of the output of 'jj file annotate'.
The regex captures four groups: change id, author, datetime, line number.")

(defun vc-jj-annotate-time ()
  (and (re-search-forward vc-jj--annotation-line-prefix-re nil t)
       (let* ((dt (match-string 3))
              (dt (and dt (string-replace " " "T" dt)))
              (decoded (ignore-errors (iso8601-parse dt))))
         (and decoded
              (vc-annotate-convert-time
               (encode-time (decoded-time-set-defaults decoded)))))))

(defun vc-jj-annotate-extract-revision-at-line ()
  (save-excursion
    (beginning-of-line)
    (when (looking-at vc-jj--annotation-line-prefix-re)
      (match-string-no-properties 1))))

(defun vc-jj-revision-completion-table (files)
  (let ((revisions
         (apply #'process-lines
                "jj" "log" "--no-graph"
                "-T" "self.change_id() ++ \"\\n\"" "--" files)))
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata . ((display-sort-function . ,#'identity)))
        (complete-with-action action revisions string pred)))))


(provide 'vc-jj)
;;; vc-jj.el ends here
