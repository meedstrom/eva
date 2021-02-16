;;; secretary.el --- Help the user achieve goals -*- lexical-binding: t; -*-
;; Author: Martin Edström <meedstrom@teknik.io>
;; URL: https://github.com/meedstrom/secretary
;; Version: 0.1
;; Created: 2020-12-03
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1"))

;; Copyright (C) 2020-2021 Martin Edström

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; See website.

;; user setup

;; (require 'org-id)
;; (org-id-update-id-locations '("/home/kept/Emacs/secretary/test.org"))

;;; Code:

;; builtins
(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'find-func)

;; external
(require 'ts)
(require 'f)
(require 's)
(require 'dash)
(require 'named-timer)

(autoload #'secretary-log-buffer "org-id")
(autoload #'org-clock-in "org-clock")
(autoload #'org-id-uuid "org-id")
(autoload #'org-id-goto "org-id")
(autoload #'notifications-notify "notifications")
(autoload #'calendar-check-holidays "holidays")

(defvar secretary-debug-p t)

(defgroup secretary nil "The Emacs in-house secretary."
  :prefix "secretary-"
  :group 'convenience)

(defcustom secretary-location-diary-discrete "/home/kept/Diary/"
  "The directory containing your discrete diary files.
We assume these files have names such as \"201228.org\".  Used by
`secretary-present-diary'."
  :group 'secretary
  :type 'string)

;; TODO: rename to main-datetree
(defcustom secretary-location-diary-datetree "/home/kept/Journal/diary2.org"
  "The file name of your main datetree, if you have one.
Only relevant if you have one you use as a big archive file, see
Info node `(org) Moving subtrees', or you write/capture
everything directly into.  Used by `secretary-present-diary'."
  :group 'secretary
  :type 'string)

(defcustom secretary-ai-name "Lex"
  "Your secretary's name."
  :group 'secretary
  :type 'string)

(defcustom secretary-user-birthday nil
  "Your birthday."
  :group 'secretary
  :type 'string)

(defcustom secretary-user-name (if (s-equals? user-full-name "")
				   "Mr. Bond"
				 (-first-item (s-split " " user-full-name)))
  "Your name, that you prefer to be addressed by."
  :group 'secretary
  :type 'string)

(defcustom secretary-user-short-title "sir"
  "A short title for you that works on its own, in lowercase."
  :group 'secretary
  :type 'string)

;; TODO: deprecate either this or the -file-name vars
(defcustom secretary-dir
  (expand-file-name "secretary" user-emacs-directory)
  "Directory under which files should sit."
  :group 'secretary
  :type 'string)

(defcustom secretary-idle-beginning-file-name
  (expand-file-name "idle-beginning" secretary-dir)
  nil
  :group 'secretary
  :type 'string)

(defcustom secretary-mood-alist-file-name
  (expand-file-name "secretary-mood-alist" secretary-dir)
  nil
  :group 'secretary
  :type 'string)

(defcustom secretary-sit-long 1
  "A duration in seconds to pause for effect after certain kinds
of messages. See also `secretary-sit-medium' and
`secretary-sit-short'."
  :group 'secretary
  :type 'number)

(defcustom secretary-sit-medium .8
  "A duration in seconds to pause for effect after certain kinds
of messages. See also `secretary-sit-long' and
`secretary-sit-short'."
  :group 'secretary
  :type 'number)

(defcustom secretary-sit-short .5
  "A duration in seconds to pause for effect after certain kinds
of messages. See also `secretary-sit-long' and
`secretary-sit-medium'."
  :group 'secretary
  :type 'number)

(defcustom secretary-chat-log-file
  (expand-file-name "chat-log.txt" secretary-dir)
  "Where to save chat log across sessions. Can be nil."
  :group 'secretary
  :type 'string)

;; REVIEW: see that there's no problem if you delete secretary-dir
(defvar secretary-mood-alist nil
  "For suggesting a score in the `secretary-log-mood' prompt.
Merely a convenience for auto-completion.

The variable populates itself through use, and syncs with a file
at `secretary-mood-alist-file-name'.")

;; (setq secretary-mood-alist '(("bemused" . "3")
;;     ("amused" . "5")
;;     ("great"  . "5")
;;     ("annoyed" . "3")
;;     ("depressed" . "1")))

(defvar secretary-aphorisms)

(defvar secretary--date (ts-now)
  "Date to which to apply the current query.
Can be set anytime during a welcome to override the date to which
some queries apply, for example to log something for yesterday.
This may not apply, check the source for the welcomer you are
using.")


;;; Activity structs
;; Q: What's cl-defstruct?  A: https://nullprogram.com/blog/2018/02/14/

(cl-defstruct (secretary-activity (:constructor secretary-activity-create)
				  (:copier nil))
  name
  id
  cost-false-pos
  cost-false-neg
  query)

(defvar secretary-activities)

(defun secretary-activity-by-name (name)
  (--find (equal name (secretary-activity-name it)) secretary-activities))

(defun secretary-activities-names ()
  (-map #'secretary-activity-name secretary-activities))


;;; Querier structs
;; Q: What's cl-defstruct?  A: https://nullprogram.com/blog/2018/02/14/

;; TODO: come up with a better name for "querier"
;; TODO: Refactor away the weird "use-posted" key
(cl-defstruct (secretary-querier (:constructor secretary-querier-create)
				 (:copier nil))
  fn
  log-file
  max-entries-per-day
  (min-hours-wait 3)
  last-called
  (dismissals 0)
  use-posted)

(defvar secretary-queriers)

(defun secretary--querier-by-fn (fn)
  "Get the querier struct associated with the query function FN."
  (--find (equal fn (secretary-querier-fn it)) secretary-queriers))

(defun secretary--querier-by-log-file (path)
  "Get the querier struct associated with the log file at PATH."
  (--find (equal path (secretary-querier-log-file it)) secretary-queriers))

;; probably unneccessary, but instructive
;; unused
(defun secretary--all-querier-log-files ()
  (-map #'secretary-querier-log-file secretary-queriers))

;; DEPRECATED: Old big function
(defun secretary-do-query-maybe (fn &optional ts ignore-wait)
  "Call the query function FN if appropriate.
Do nothing if it's been recently logged, reached its
max-entries-per-day, etc."
  (let* ((q (secretary--querier-by-fn fn))
	 (last-called (secretary-querier-last-called q))
	 (file (secretary-querier-log-file q))
	 (max-entries (secretary-querier-max-entries-per-day q))
	 (use-posted (secretary-querier-use-posted q))
	 (dismissals (secretary-querier-dismissals q))
	 (min-hrs (secretary-querier-min-hours-wait q))
	 (min-secs (* 60 60 min-hrs))
	 (called-today (when last-called
			 (and (= (ts-day last-called) (ts-day (ts-now)))
			      (> (ts-H last-called) 4))))
	 (recently-logged
	  (when (file-exists-p file)
	    (> min-secs
               (if use-posted
		   (- (ts-unix (ts-now))
		      (string-to-number (car (secretary--last-in-tsv file))))
		 (ts-diff (ts-now)
			  (ts-parse (secretary-last-timestamp-in-tsv file))))))))
    (unless (and recently-logged
		 (null ts))
      (when (or (not called-today)
		(null max-entries)
		(not (file-exists-p file))
		(> max-entries (length (secretary--get-entries-in-tsv file ts))))
	(when (or ignore-wait
		  (null last-called)
		  (< min-hrs (/ (ts-diff (ts-now) last-called) 60 60)))
	  (unless (and (>= dismissals 3)
		       (when (secretary-prompt
			      "You have been dismissing "
			      (symbol-name fn)
			      ", shall I stop tracking it for now?")
			 (secretary-disable-query fn)
			 t))
	    (setf (secretary-querier-last-called q) (ts-now))
	    (setf (secretary-querier-dismissals q) 0)
	    (funcall fn ts)))))))

(defun secretary-do-query-check-dismissals (fn)
  "Call a query, but react specially if it's been dismissed many times.
Also update :last-called so that `secretary--pending-p' will
work correctly next time."
  (let* ((q (secretary--querier-by-fn fn))
	 (dismissals (secretary-querier-dismissals q)))
    (unless (and (>= dismissals 3)
		 (when (secretary-prompt
			"You have been dismissing "
			(symbol-name fn)
			", shall I stop tracking it for now?")
		   (secretary-disable-query fn)
		   t))
      (setf (secretary-querier-last-called q) (ts-now))
      (setf (secretary-querier-dismissals q) 0)
      (funcall fn secretary--date))))

(defun secretary--pending-p (query-fn)
  (let* ((q (secretary--querier-by-fn query-fn))
	 (last-called (secretary-querier-last-called q))
	 (log-file (secretary-querier-log-file q))
	 (max-entries (secretary-querier-max-entries-per-day q))
	 (use-posted (secretary-querier-use-posted q))
	 (dismissals (secretary-querier-dismissals q))
	 (min-hrs (secretary-querier-min-hours-wait q))
	 (min-secs (* 60 60 min-hrs))
	 (called-today (when last-called
			 (and (= (ts-day last-called) (ts-day (ts-now)))
			      (> (ts-H last-called) 4))))
	 (recently-logged
	  (when (file-exists-p log-file)
	    (> min-secs
               (if use-posted
		   (- (ts-unix (ts-now))
		      (string-to-number (car (secretary--last-in-tsv log-file))))
		 (ts-diff (ts-now)
			  (ts-parse (secretary-last-timestamp-in-tsv log-file)))))))
	 ;; Even if we didn't log yet, we don't quite want to be that persistent
	 (recently-called (> (ts-diff (ts-now) last-called)
			     ;; num of hours multiplied by n dismissals
			     (* dismissals 60 60))))
    (unless recently-logged
      (when (or (not called-today)
		(not (file-exists-p log-file))
		(null max-entries)
		(> max-entries (length (secretary--get-entries-in-tsv log-file))))
	(unless recently-called
	  t)))))

;;(secretary--pending-p #'secretary-query-weight)
;;(secretary--pending-p #'secretary-query-sleep)
;;(secretary--pending-p #'secretary-query-mood)

(defun secretary-disable-query (fn)
  (f-write (prin1-to-string (remove fn (secretary--active-queries)))
	   'utf-8
	   secretary-inactive-queries-file))

;; (defun test ()
;;   (interactive)
;;   (secretary-do-query-maybe #'secretary-query-mood nil t))
;; (secretary-querier-dismissals (secretary--querier-by-fn #'secretary-query-weight))

(defcustom secretary-inactive-queries-file
  "/home/kept/Emacs/secretary/pseudo-userdir/inactive-queries.el"
  nil
  :group 'secretary
  :type 'string)

;; TODO: Move to read/save on disk functions? This is atypical.
(defun secretary--active-queries ()
  (let ((all (-map #'secretary-querier-fn secretary-queriers))
	(inactive (read (f-read secretary-inactive-queries-file))))
    (-difference all inactive)))


;;; Library

(defvar secretary--last-msg "")
(defvar secretary--last-msg-2 "")

(defvar secretary--k nil)
(defun secretary--y-or-n-p-insert-k ()
  "Mostly like `y-or-n-p-insert-y'."
  (interactive)
  (delete-minibuffer-contents)
  (insert "y")
  (setq secretary--k t)
  (exit-minibuffer))

(defun secretary-prompt (&rest strings)
  (let* (;; (default-y-or-n-p-map y-or-n-p-map)
         ;; (default-cmd (lookup-key y-or-n-p-map (kbd "k")))
	 (info (concat "Applying to date: " (ts-format "%Y-%B-%d" secretary--date) "\n"
		       secretary--last-msg-2 "\n"
		       secretary--last-msg "\n"))
	 (prompt (string-join strings)))
    (unwind-protect
        (progn
	  (secretary-print-new-date-maybe)
          (switch-to-buffer (secretary-buffer-chat))
          (unless (< 20 (car (window-fringes)))
            (set-window-fringes nil 20 20))
          (goto-char (point-max))
          (secretary-emit prompt)
          (define-key y-or-n-p-map (kbd "o") #'secretary-special-handle-current-query)
          (define-key y-or-n-p-map (kbd "i") #'secretary-special-handle-current-query)
          (define-key y-or-n-p-map (kbd "<SPC>") #'secretary-special-handle-current-query)
          (define-key y-or-n-p-map (kbd "k") #'secretary--y-or-n-p-insert-k)
	  (let ((result (y-or-n-p (concat info prompt))))
	    (if secretary--k
		(progn
		  (setq secretary--k nil)
		  (insert " Okay..."))
	      (if result
		  (insert " Yes.")
		(insert " No.")))
	    (setq secretary--last-msg (buffer-substring (line-beginning-position)
							(line-end-position)))
	    result))
      (dolist (x '("o" "i" "k" "<SPC>"))
        (define-key y-or-n-p-map (kbd x) #'y-or-n-p-insert-other)))))
;; (secretary-prompt "Test")
;; (y-or-n-p "Test")

(defun secretary-read (prompt &optional collection default)
  (let* ((background-info (concat "[Current date: "
				  (ts-format "%Y-%b-%d" secretary--date) "]\n"
				  secretary--last-msg-2 "\n"
				  secretary--last-msg "\n"
				  ))
	 (extra-collection '("Skip to presentations"
			     "Don't disturb me"))
	 (result (completing-read
		  (concat background-info
			  (ts-format "[%H:%M] ")
			  prompt
			  (when (stringp default)
			    " (default " default "): "))
		  (append collection extra-collection)
		  nil nil nil nil
		  (when (stringp default)
		    default))))
    (secretary-emit prompt)
    (if (string-match-p "skip" result)
	(progn
	  ;; (minibuffer-keyboard-quit)
	  (secretary-present-diary)
	  (keyboard-quit))
      result)))

(defcustom secretary-chime-audio-file
  (expand-file-name
   ;; From https://freesound.org/people/josepharaoh99/sounds/380482/
   "assets/Chime Notification-380482.wav"
   ;; From https://bigsoundbank.com/detail-0319-knock-on-a-glass-door-1.html
   ;; "assets/DOORKnck_Knock on a glass door 1 (ID 0319)_BSB.wav"
   (f-dirname (find-library-name "secretary")))
  "Sound to play when a welcomer is triggered unannounced."
  :group 'secretary
  :type 'string)

(defun secretary--chime-aural ()
  (and (executable-find "aplay")
       (file-exists-p secretary-chime-audio-file)
       (start-process "aplay" nil "aplay" secretary-chime-audio-file)))

(defun secretary--chime-visual ()
  (let ((colors '((.1 . "green")
		  (.2 . "#aca")
		  (.3 . "#7a7")
		  (.4 . "#696")
		  (.5 . "#363"))))
    (let ((orig (face-background 'fringe)))
      (dolist (x colors)
	(run-with-timer (car x) nil #'set-face-background 'fringe (cdr x)))
      (run-with-timer .6 nil #'set-face-background 'fringe orig))

    (when (facep 'solaire-fringe-face)
      (let ((orig (face-background 'solaire-fringe-face)))
	(dolist (x colors)
	  (run-with-timer (car x) nil #'set-face-background 'solaire-fringe-face (cdr x)))
	(run-with-timer .6 nil #'set-face-background 'solaire-fringe-face orig)))
    nil))

(defun secretary-emit (&rest strings)
  (secretary-print-new-date-maybe)
  (prog1 (message (string-join strings))
    (let ((msg (concat "\n[" (ts-format "%H:%M") "] " (string-join strings))))
      (setq secretary--last-msg-2 secretary--last-msg)
      (setq secretary--last-msg msg)
      (with-current-buffer (secretary-buffer-chat)
	(goto-char (point-max))
	(insert msg))
      ;; TODO: Just keep open a file-visiting buffer instead of syncing a virtual buffer.
      (when secretary-chat-log-file
	(ignore-errors
	  (secretary-append secretary-chat-log-file msg))))))

(defvar secretary--last-edited (ts-now)
  "Timestamp updated whenever the chat is written to.")

(defun secretary-print-new-date-maybe ()
  (when (/= (ts-day (ts-now))
            (ts-day secretary--last-edited))
    (with-current-buffer (secretary-buffer-chat)
      (goto-char (point-max))
      (insert "\n\n" (ts-format "%Y, %B %d") (secretary--holiday-maybe) "\n"))
    (setq secretary--last-edited (ts-now))))
;; (secretary-print-new-date-maybe)

(defun secretary--holiday-maybe ()
  (require 'calendar)
  (require 'holidays)
  (if-let (foo (calendar-check-holidays (calendar-current-date)))
      (concat " -- " (s-join " " foo))
    ""))

(defun secretary-reschedule ()
  (run-with-timer 3600 nil #'secretary-call-from-reschedule))

(defun secretary--random-p (&rest _args)
  "Return t or nil, at random."
  (> 0 (random)))

(defun secretary--transact-buffer-onto-file (buffer path)
  (when-let ((visiting (get-file-buffer path)))
    (with-current-buffer visiting
      ;; (undo-only 999)
      (save-buffer))
    (kill-buffer visiting)
    (message "Killed buffer to prevent edit war. %s"
	     "To edit, disable `secretary-mode' first."))
  (with-current-buffer buffer
    (whitespace-cleanup)
    (f-append-text (buffer-string) 'utf-8 path)
    (delete-region (point-min) (point-max))))

(defmacro secretary--process-output-to-string (program &rest args)
  "Similar to `shell-command-to-string', but skips the shell
intermediary so you don't need a /bin/sh. PROGRAM and ARGS are
passed on to `call-process'."
  (declare (debug (&rest form)))
  `(with-temp-buffer
     (call-process ,program nil (current-buffer) nil ,@args)
     (buffer-string)))

(defmacro secretary--process-output-to-number (program &rest args)
  "Pipe `secretary--process-output-to-string' through `string-to-number'."
  (declare (debug (&rest form)))
  `(string-to-number (secretary--process-output-to-string ,program ,@args)))

(defun secretary--idle-seconds ()
  "Stub to be redefined."
  (warn "Code ended up in an impossible place."))

(defun secretary-idle-p ()
  (> (secretary--idle-seconds) secretary-idle-threshold))

(defvar secretary-x11idle-program-name "x11idle")

(defun secretary--x11-idle-seconds ()
  "Like `org-x11-idle-seconds' without need for /bin/sh or org."
  (/ (secretary--process-output-to-number secretary-x11idle-program-name) 1000))

(defun secretary--buffer-r ()
  (get-buffer-create (concat "*" secretary-ai-name ": R*")))

;; - Needs to be a function because something might kill the buffer.
;; - Can't use get-buffer-create because I want to turn on `visual-line-mode'.
(defun secretary-buffer-chat ()
  (if-let* ((name (concat "*" secretary-ai-name ": Chat log*"))
            (b (get-buffer name)))
      b
    (with-current-buffer (generate-new-buffer name)
      (visual-line-mode)
      (current-buffer))))

(defvar secretary--frame nil)

(defmacro secretary-raise-frame-and-do (&rest body)
  "Kill any previous secretary frame, raise new one, focus it."
  `(progn
     (and (frame-live-p secretary--frame)
	  (not (= 1 (length (frames-on-display-list))))
	  (delete-frame secretary--frame t))
     (setq secretary--frame (make-frame '((name . "Secretary")
					  (buffer-predicate . nil))))
     (select-frame-set-input-focus secretary--frame t)
     ,@body))

;; We choose tab-separated values.
;; The IANA standard disallows tabs within fields, simplifying sanity checks.
;; https://www.iana.org/assignments/media-types/text/tab-separated-values
(defun secretary-append-tsv (path &rest fields)
  "Append a line to the file located at PATH.
Create the file and its parent directories if it doesn't exist,
and make sure the line begins on a newline.  Treat each argument
in FIELDS... as a separate data field, inserting a tab character
in between, and warn if a field contains a tab character."
  (declare (indent defun))
  (unless (file-exists-p path)
    (make-empty-file path t))
  (unless (-all-p #'stringp fields)
    (warn "[%s] `secretary-append-tsv' was passed nil" (ts-format "%H:%M")))
  (let* ((fields (-replace nil "" fields))
	 (newline-maybe (if (s-ends-with-p "\n" (f-read-bytes path))
                            ""
                          "\n"))
	 (errors-path (concat path "_errors"))
	 (posted (s-pad-right 18 "0" (number-to-string (ts-unix (ts-now)))))
	 (text (string-join fields "\t"))
	 (new-text (concat newline-maybe posted "\t" text)))
    (cond ((--any-p (s-contains-p "\t" it) fields)
	   (warn "Log entry had tabs inside fields, wrote to %s" errors-path)
	   (f-append new-text 'utf-8 errors-path))
	  ((s-contains-p "\n" text)
	   (warn "Log entry had newlines, wrote to %s" errors-path)
	   (f-append new-text 'utf-8 errors-path))
	  (t
	   (f-append new-text 'utf-8 path)))))
;; (secretary-append-tsv "/home/kept/Self_data/idle.tsv"  "sdfsdf" "33")

(defun secretary-append (path &rest text)
  "Append TEXT to the file located at PATH, creating it and its
parent directories if it doesn't exist, and making sure the text
begins on a newline."
  (declare (indent defun))
  (unless (file-exists-p path)
    (make-empty-file path t))
  (let ((newline-maybe (if (s-ends-with-p "\n" (f-read-bytes path))
                           ""
                         "\n")))
    (f-append (concat newline-maybe (string-join text)) 'utf-8 path)))

;; REVIEW
(defun secretary-change-date (&optional ts)
  (require 'org)
  (let ((target (if ts
		    ts
		  (ts-parse (org-read-date)))))
    (setq secretary--date target)))

(defun secretary-change-date-yesterday ()
  (message "Applying this query AND subsequent queries for now to yesterday.")
  (setq secretary--date (ts-dec 'day 1 (ts-now))))


;;; Log-file look-uppers

(defun secretary-last-datestamp-in-file (path)
  "Get the last match of YYYY-MM-DD in PATH.
Beware that if PATH has instances of such where you don't expect
it (in additional columns), you might not get the datestamp you
meant to get."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (goto-char (point-max))
    (re-search-backward (rx (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)))
    (buffer-substring (point) (+ 10 (point)))))

(defun secretary-last-timestamp-in-tsv (path)
  "In file at PATH, get the second field of last row."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-max))
    (when (looking-back "^" nil) ;; if trailing newline
      (forward-line -1))
    (goto-char (line-beginning-position))
    (search-forward "\t")
    (buffer-substring (point) (- (search-forward "\t") 1))))

(defalias 'secretary-tsv-all-this-date #'secretary--get-entries-in-tsv)
(defalias 'secretary-get-all-today-in-tsv #'secretary--get-entries-in-tsv)
(defun secretary--get-entries-in-tsv (path &optional ts)
  (with-temp-buffer
    (insert-file-contents-literally path)
    (let (x)
      (while (search-forward (ts-format "%F" ts) nil t)
        (push (split-string (buffer-substring (line-beginning-position)
					      (line-end-position))
			    "\t")
              x)
	(goto-char (line-end-position)))
      x)))

(defun secretary--last-in-tsv (path)
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-max))
    (when (looking-back "^" nil) ;; if empty line
      (forward-line -1))
    (split-string (buffer-substring (line-beginning-position)
				    (line-end-position))
		  "\t")))

(defun secretary-get-first-today-line-in-file (path &optional ts)
  (with-temp-buffer
    (insert-file-contents path)
    (search-forward (ts-format "%F" ts))
    (buffer-substring (line-beginning-position) (line-end-position))))
;; (secretary-get-first-today-line-in-file "/home/kept/Self_data/ingredients.csv")

(defun secretary-last-value-in-tsv (path)
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents-literally path)
      (goto-char (point-max))
      (search-backward "\t")
      (forward-char)
      (buffer-substring (point) (line-end-position)))))

;; WONTFIX: check for recent activity (if user awake thru the night)
(defun secretary-logged-today (file)
  (when (file-exists-p file)
    ;; don't act like it's a new day if the time is <5am.
    (let ((day (if (> 5 (ts-hour (ts-now)))
                   (ts-dec 'day 1 (ts-now))
                 (ts-now))))
      (with-temp-buffer
        (insert-file-contents-literally file)
        (ignore-errors (search-forward (ts-format "%F" day)))))))
;; (secretary-logged-today "/home/kept/Self_data/weight.tsv")
;; (secretary-logged-today "/home/kept/Self_data/buffers.tsv")


;;;; Greetings

(defvar secretary-greetings '("Welcome back, Master."
			      (concat "Nice to see you again, " secretary-user-name ".")
			      (concat "Greetings, " secretary-user-name "."))
  "Greeting phrases which can initiate a conversation.")

(defun secretary-greeting-curt ()
  "Return a greeting appropriate in the midst of a workday.
Because if you've already exchanged good mornings, it's weird to
do so again."
  (seq-random-elt `("Hello" "Hi" "Hey")))

(defun secretary-greeting ()
  "Return a greeting string."
  (let ((bday (ts-parse secretary-user-birthday)))
    (cond ((ts-in bday bday (ts-now))
	   (concat "Happy birthday, " secretary-user-name "."))
	  ;; If it's morning, always use a variant of "good morning"
	  ((> 10 (ts-hour (ts-now)) 5)
	   (eval (seq-random-elt (secretary--daytime-appropriate-greetings))
		 t))
	  (t
	   (eval (seq-random-elt (append secretary-greetings
					 (-list (secretary--daytime-appropriate-greetings))))
		 t)))))

;; NOTE: I considered making external variables for morning, day and evening
;;       lists, but users might also want to change the daytime boundaries or
;;       even add new boundaries. Too many possibilities, this is a case where
;;       it's ok to make the user override the defun as a primary means of
;;       customization.
(defun secretary--daytime-appropriate-greetings ()
  (cond ((> 5 (ts-hour (ts-now)))
         (list "You're up late, Master."
               "Burning the midnight oil?"))
        ((> 10 (ts-hour (ts-now)))
         (list (concat "Good morning, " secretary-user-name ".")
               "Good morning!"
               "The stars shone upon us last night."))
        ((> 16 (ts-hour (ts-now)))
         (list "Good day!"))
        (t
         (list "Good evening!"
               "Pleasant evening to you!"))))

(defun secretary-greeting-standalone ()
  "Return a greeting that expects to be followed by nothing.
No prompts, no debug message, no info. Suitable for
`notifications-notify' or `startup-echo-area-message'. A superset
of `secretary-greeting'. Mutually exclusive with
`secretary-greeting-curt'."
  (eval (seq-random-elt
         (append secretary-greetings
                 (-list (secretary--daytime-appropriate-greetings))
                 '("How may I help?")))))


;;;; Loggers

(defun secretary-log-idle ()
  (secretary-append "/home/kept/Self_data/idle.tsv"
    (ts-format)
    "\t" (number-to-string (/ (round secretary-length-of-last-idle) 60))))

(defvar secretary-last-buffer nil)

(defvar secretary-known-buffers nil)

;; TODO: recreate buffer upon disabling and reenabling secretary-mode
(defvar secretary-buffer-focus-log-buffer
  (get-buffer-create
   (concat (unless secretary-debug-p " ")
	   "*" secretary-ai-name ": Buffer focus log*")))

(defvar secretary-buffer-existence-log-buffer
  (get-buffer-create
   (concat (unless secretary-debug-p " ")
	   "*" secretary-ai-name ": Buffer existence log*")))

(defun secretary--save-buffer-logs-to-disk ()
  (secretary--transact-buffer-onto-file secretary-buffer-focus-log-buffer
					"/home/kept/Self_data/buffer-focus.tsv")
  (secretary--transact-buffer-onto-file secretary-buffer-existence-log-buffer
					"/home/kept/Self_data/buffer-existence.tsv"))

;; TODO: When buffer major mode changes, count it as a new buffer. Note that
;; (assoc buf secretary-known-buffers) will still work.
;; TODO: When eww url changes, count it as a new buffer
;; TODO: Optimize
(defun secretary-log-buffer (&optional _arg)
  "Log the buffer just switched to.
Put this on `window-buffer-change-functions' and
`window-selection-change-functions'."
  (unless (minibufferp)
    (let* ((buf (current-buffer))
           (mode (symbol-name (buffer-local-value 'major-mode buf)))
           (known (assoc buf secretary-known-buffers))
           (timestamp (s-pad-right 18 "0" (number-to-string (ts-unix (ts-now)))))
	   ;; TODO: use this
	   (eww-url (when (eq buf "eww-mode")
		      (eww-current-url)))
           (exist-record (unless (and known
                                      (string= mode (nth 4 known))) ;; doesnt do it
                           (list buf
                                 (org-id-uuid)
                                 (buffer-name buf)
                                 (buffer-file-name buf)
                                 mode
                                 timestamp ;; time the buffer was first opened
                                 )))
           (focus-record (list timestamp ;; time the buffer was switched to
                               (if known (cadr known) (cadr exist-record)) ;; uuid
                               )))
      (unless (eq secretary-last-buffer buf) ;; you only entered and left minibuffer
        (setq secretary-last-buffer buf)
        (unless known
          (push exist-record secretary-known-buffers)
	  (with-current-buffer secretary-buffer-existence-log-buffer
	    (goto-char (point-max))
	    (insert "\n" (string-join (cdr exist-record) "\t"))))
	(with-current-buffer secretary-buffer-focus-log-buffer
	  (goto-char (point-max))
	  (insert "\n" (string-join focus-record "\t")))))))


;;; Queries

(defun secretary-midquery-keyboard-quit ()
  "Quit, and record which query was quit."
  (interactive)
  (if (or (> (recursion-depth) 1)
	  (and (minibufferp) delete-selection-mode (region-active-p)))
      (minibuffer-keyboard-quit)
    (transient--pop-keymap 'secretary-query-keymap) ;; just in case
    ;; the main reason for this wrapper
    (cl-incf (secretary-querier-dismissals
	      (secretary--querier-by-fn secretary--current-fn)))
    (setq secretary--current-fn nil)
    (if (minibufferp)
	(abort-recursive-edit)
      (keyboard-quit))))

(defvar secretary--current-fn nil
  "Information used by `secretary-midquery-keyboard-quit'.")

(defvar secretary-query-keymap (make-sparse-keymap))
(define-key secretary-query-keymap [remap keyboard-quit] #'secretary-midquery-keyboard-quit)
(define-key secretary-query-keymap [remap minibuffer-keyboard-quit] #'secretary-midquery-keyboard-quit)
(define-key secretary-query-keymap [remap abort-recursive-edit] #'secretary-midquery-keyboard-quit)
(define-key secretary-query-keymap [remap doom/escape] #'secretary-midquery-keyboard-quit)

(defun secretary-check-yesterday-sleep ()
  (let* ((today-rows (secretary-get-all-today-in-tsv
		      "/home/kept/Self_data/sleep.tsv" (ts-dec 'day 1 (ts-now))))
         (total-yesterday (-sum (--map (string-to-number (nth 2 it)) today-rows))))
    (if (> (* 60 4) total-yesterday)
        (if (secretary-prompt "Yesterday, you slept "
			      (number-to-string (round (/ total-yesterday 60.0)))
			      " hours, is this about right?")
            nil
          (secretary-emit "You may edit the history at "
			  "/home/kept/Self_data/sleep.tsv"
			  ". For now, let's talk about today.")
          (sit-for secretary-sit-short)))))

;; TODO: make the dataset append-only
;;;###autoload
(defun secretary-query-ingredients (&optional ts)
  (interactive)
  (setq secretary--current-fn #'secretary-query-ingredients)
  (set-transient-map 'secretary-query-keymap #'active-minibuffer-window)
  (let* ((response (read-string "Comma-separated list of ingredients: "))
         (formatted-response (->> response
                                  (s-split (rx (+ (any "," blank))))
                                  (s-join ", ")
                                  (s-replace "\"" "'")))
         (path "/home/kept/Self_data/ingredients.tsv"))
    (secretary-append-tsv path
      (ts-format ts)
      "\"" formatted-response "\"")
    (secretary-emit "Recorded so far today: "
		    (s-replace "^.*?," ""
			       (secretary-get-first-today-line-in-file path)))))

;; (defalias #'secretary-query-food #'secretary-query-ingredients)

;; This fn is half boilerplate, lol.
;;;###autoload
(defun secretary-query-activity (&optional _ts interactive)
  (interactive "i\np")
  (setq secretary--current-fn #'secretary-query-activity)
  (set-transient-map 'secretary-query-keymap #'active-minibuffer-window)
  (when interactive
    (setq secretary--date (ts-now)))
  (let* ((name (secretary-read "What are you up to? " (secretary-activities-names))))
    (secretary-append-tsv (secretary-querier-log-file
			   (secretary--querier-by-fn secretary--current-fn))
      (ts-format secretary--date) ;; time the datapoint concerns
      name
      (secretary-activity-id (secretary-activity-by-name name)))))
;; (secretary-query-activity)

;;;###autoload
(defun secretary-query-mood (&optional ts prompt)
  (interactive)
  (setq secretary--current-fn #'secretary-query-mood)
  (set-transient-map 'secretary-query-keymap #'active-minibuffer-window)
  (when (called-interactively-p 'any)
    (setq secretary--date (ts-now)))
  (let* ((mood-desc (secretary-read
		     (or prompt "Your mood: ")
		     (cl-sort (mapcar #'car secretary-mood-alist)
			      #'secretary--random-p)))
         (old-score (cdr (assoc mood-desc secretary-mood-alist)))
         (prompt-for-score
          (concat "Score from 1 to 5"
                  (when old-score " (default " old-score ")")
                  ": "))
         (score (read-string prompt-for-score nil nil old-score))
         (score-num (string-to-number score))
         (now (or ts (ts-now))))
    (secretary-append-tsv "/home/kept/Self_data/mood.tsv"
      (ts-format now)
      (s-replace "," "." score)
      mood-desc)
    ;; Update secretary-mood-alist.
    (if (assoc mood-desc secretary-mood-alist)
        (setq secretary-mood-alist
              (--replace-where (string= (car it) mood-desc)
                               (cons (car it) score)
                               secretary-mood-alist))
      (push (cons mood-desc score) secretary-mood-alist))
    ;; Return the mood score, which can be useful for the caller of this
    ;; function. If the input was not a number, like "idk" or an empty string,
    ;; return 3 to be neutral.
    (if (= 0 score-num)
        3
      score-num)))

;;;###autoload
(defun secretary-query-weight (&optional ts)
  (interactive)
  (setq secretary--current-fn #'secretary-query-weight)
  (set-transient-map secretary-query-keymap #'active-minibuffer-window)
  (let* ((now (or ts (ts-now)))
	 (path "/home/kept/Self_data/weight.tsv")
         (last-wt (secretary-last-value-in-tsv path))
         (wt (secretary-read "What do you weigh today? "
			     `(,last-wt
			       "I don't know")
			     last-wt)))
    (if (= 0 (string-to-number wt)) ;; user typed a string without any number
        (secretary-emit "Ok, I'll ask you again later.")
      (secretary-append-tsv path
	(ts-format now)
	(s-replace "," "." wt))
      (secretary-emit "Weight today: " (secretary-last-value-in-tsv path) " kg")
      (setf (secretary-querier-dismissals
	     (secretary--querier-by-fn secretary--current-fn))
	    0))
    (sit-for secretary-sit-short)))


;; TODO: (Feature) Let user say "since 5" instead of quantity
;; TODO: Fix the case where someone wakes up at 23:00 but replies to the query
;;       at 01:00. Notice the unusual hour change and ask if user meant 23
;;       yesterday.
;;;###autoload
(defun secretary-query-sleep (&optional ts)
  "Query you for wake-up time and sleep quantity for one sleep block today.
You are free to decline either query, but you should not later
register sleep quantity from this same block in order to \"get
the totals correct\" -- the program will interpret it as a
different sleep block and continue to count the original one as
having an unknown nonzero quantity of sleep on top of what you
add."
  (interactive)
  ;; FIXME: the keyboard quit doesnt increment dismissals
  (setq secretary--current-fn #'secretary-query-sleep)
  (set-transient-map 'secretary-query-keymap #'active-minibuffer-window)
  (secretary-check-yesterday-sleep)
  (let* ((now (or ts (ts-now)))
         (wakeup-time
          (if (secretary-prompt "Did you wake around now?")
              (ts-dec 'minute 10 now)
            (let ((reply (secretary-read "When did you wake? "
                                         `("I don't know"
					   "Now"
					   ,(ts-format "%H:%M")))))
              (when (-non-nil (parse-time-string reply))
                (ts-parse reply)))))
         (sleep-minutes
	  (secretary-parse-time-amount
	   (secretary-read "How long did you sleep? "
                           `("I don't know"
			     ,(number-to-string
			       (/ secretary-length-of-last-idle 60 60)))))))

    (secretary-emit (when wakeup-time
		      (concat "You woke at " (ts-format "%H:%M" wakeup-time) ". "))
		    (when sleep-minutes
		      (concat "You slept " (number-to-string sleep-minutes)
			      " minutes (" (number-to-string (/ sleep-minutes 60.0))
			      " hours).")))
    (secretary-append-tsv "/home/kept/Self_data/sleep.tsv"
      (ts-format "%F" secretary--date) ;; date
      (when wakeup-time (ts-format "%T" wakeup-time)) ;; time (optional)
      (when sleep-minutes (number-to-string sleep-minutes)))))

(defun secretary-query-meditation (&optional ts)
  (interactive)
  (setq secretary--current-fn #'secretary-query-meditation)
  (set-transient-map 'secretary-query-keymap #'active-minibuffer-window)
  (when (secretary-prompt "Did you meditate today?")
    (let* ((mins (read-string "Do you know how long (in minutes)? "))
	   (cleaned-mins (number-to-string (string-to-number mins)))) ;; ugly, I know
      (secretary-append-tsv "/home/kept/Self_data/meditation.tsv"
	(ts-format ts)
	"TRUE"
	(unless (string= "0" cleaned-mins) cleaned-mins)))))

(defun secretary-query-cold-shower (&optional ts)
  (interactive)
  (setq secretary--current-fn #'secretary-query-cold-shower)
  (set-transient-map 'secretary-query-keymap #'active-minibuffer-window)
  (let ((rating (read-string "Cold rating? "))
	(path "/home/kept/Self_data/cold.tsv"))
    (secretary-append-tsv path
      (ts-format ts)
      rating)))


;;;; Parsers

;; TODO: Catch typos like 03 meaning 30 minutes, not 3 hours.
(defun secretary-parse-time-amount (input)
  "Translate the input from hours or minutes into minutes.
If the input contains no \"h\" or \"m\", assume numbers above 20
are minutes and numbers below are hours."
  (let ((numeric-part (string-to-number input)))
    (cond ((= 0 numeric-part) ;; strings without any number result in 0
           nil) ;; save as a NA observation
          ((and (string-match-p "h.*m" input) (> numeric-part 0))
           (warn "I'm not sophisticated enough to parse that"))
          ((string-match-p "h" input)
           (* 60 numeric-part))
          ((string-match-p "m" input)
           numeric-part)
          ((-> numeric-part (>= 20))
           numeric-part)
          (t
           (* 60 numeric-part)))))
;; (secretary-parse-time-amount "30")

;; (define-key minibuffer-local-completion-map (kbd "C-o") #'secretary-special-handle-current-query)


;;;; Welcomers

;; FIXME: why doesn't the minibuffer prompt show fully? is it my font? does it work if i don't raise new frame?
(defun secretary--call-timidly ()
  "Run pending queries if any."
  (setq secretary--date (ts-now))
  (when-let ((fns (-filter #'secretary--pending-p (secretary--active-queries))))
    (secretary--chime-aural)
    (secretary--chime-visual)
    (run-with-timer 1 nil `(lambda ()
			     (secretary-raise-frame-and-do
			      (dolist (f ,fns)
				(secretary-do-query-check-dismissals f)))))))
;; (secretary--call-timidly)
;; (named-timer-run :secretary-attempt (* 60 60) (* 60 60) #'secretary--call-timidly)

;; TODO: Also call presenters &c.
;; Call all queries, new version.
(defun secretary--call2 ()
  (interactive)
  (setq secretary--date (ts-now))
  (secretary-raise-frame-and-do
   (dolist (x (secretary--active-queries))
     (secretary-do-query-maybe x secretary--date (called-interactively-p 'any)))))
;; (secretary--call2)

(defun secretary-call (&optional arg)
  "Call your secretary. Useful when you're unsure what to do."
  (interactive "P")
  (secretary-emit "Hello!")
  (sit-for secretary-sit-short)
  (secretary-check-neglect)
  (secretary-welcome arg))

(defun secretary-call-from-idle ()
  "Called by idleness-related hooks, doesn't do much if idle was not long."
  (if (< secretary-length-of-last-idle secretary-long-idle-threshold)
      (setq secretary-length-of-last-idle 0)
    (unless (frame-focus-state)
      (notifications-notify :title secretary-ai-name :body (secretary-greeting)))
    (secretary--chime-aural)
    (secretary--chime-visual)
    (run-with-timer
     1
     nil
     (lambda ()
       (unwind-protect
	   (when (secretary-prompt (secretary-greeting)
				   " Do you have time for some questions?")
	     ;; check neglect overlaps with the stuff asked in the following prompt.
	     (secretary-check-neglect)
	     (secretary-welcome t))
	 (setq secretary-length-of-last-idle 0))))))
;; (run-with-timer 3 nil (lambda ()  (print (frame-focus-state))))
;; (secretary-call-from-idle)

(defun secretary-call-from-reschedule ()
  (secretary--chime-aural)
  (secretary--chime-visual)
  (secretary-emit "Hello, " secretary-user-short-title ". ")
  (run-with-timer
   secretary-sit-long nil
   (lambda ()
     (when (secretary-prompt "1 hour ago, you asked to be reminded. Is now a good time?")
       (secretary-check-neglect)
       (secretary-welcome)))))
;;(secretary-call-from-reschedule)

;; TODO: Don't re-ask the same questions the neglect-check asked.
;; TODO: Show the results of changing date via `secretary-special-handle-current-query'.
(defun secretary-welcome (&optional just-idled-p)
  (setq secretary--date (ts-now))
  (secretary-check-neglect)
  (and just-idled-p
       (secretary-prompt "Have you slept?")
       (secretary-query-sleep))
  (unless (secretary-logged-today "/home/kept/Self_data/weight.tsv")
    (secretary-query-weight))
  ;; (and (secretary-prompt "Up to reflect?")
  ;;      (secretary-prompt "Have you learned something?")
  ;;      (org-capture nil "s"))
  ;; (if (secretary-prompt (concat "How about some flashcards?"))
  ;;     (org-drill))
  ;; (if (secretary-prompt "Have you stretched today?")
  ;;     nil
  ;;   (if (secretary-prompt "Do you want reminders for why?")
  ;;       nil
  ;;     nil))
  ;; (if (secretary-prompt "Did you photographe your face today?")
  ;;     nil)
  ;; ;; (unless (secretary-just-slept)
  ;; (unless (secretary-logged-today "/home/kept/Self_data/meditation.csv")
  ;;   (secretary-query-meditation secretary--date))
  ;; (unless (secretary-logged-today "/home/kept/Self_data/cold.csv")
  ;;   (when (secretary-prompt "Have you had a cold shower yet?")
  ;;     (secretary-query-cold-shower secretary--date)))
  ;; (if (secretary-prompt "Have you paid for anything since yesterday?")
  ;;     (org-capture nil "ln"))
  ;; (if (secretary-prompt "Shall I remind you of your life goals? Don't be shy.")
  ;;     (view-file "/home/kept/Journal/gtd2.org"))
  (and (>= 1 (secretary-query-mood "How are you? "))
       (secretary-prompt "Do you need to talk?")
       (secretary-prompt "I can direct you to my colleague Eliza, though "
			 "she's not too bright. Will that do?")
       (doctor))
  (secretary-present-plots)
  ;; and (secretary-prompt "Would you like me to suggest an activity?")
  (secretary-present-diary (ts-now))
  (and (-all-p #'not
               (-map #'secretary-logged-today
		     (-map #'secretary-querier-log-file secretary-queriers)))
       (secretary-prompt "Shall I come back in an hour?")
       (run-with-timer 3600 nil #'secretary-call-from-idle)))

(defun secretary-check-neglect ()
  (interactive)
  (dolist (q secretary-queriers)
    (let ((path (secretary-querier-log-file q)))
      (when (file-exists-p path)
	(let* ((d (ts-parse (secretary-last-datestamp-in-file path)))
               (diff-days (round (/ (ts-diff (ts-now) d) 60 60 24))))
	  (and (< 3 diff-days)
               ;; FIXME: Increment the dismissals upon a "no".
               (secretary-prompt "It's been " (number-to-string diff-days)
				 " days since you logged " (file-name-base path)
				 ". Do you want to log it now?")
               (call-interactively (secretary-querier-fn q))))))))


;;;; Plot presenter

(defvar secretary-plot-hook nil
  "Hook called to print plots. A convenient place to add your
custom plots.")

(defmacro secretary--plot-ascii (gnuplot-script message &rest after-body)
  "Make an ascii plot.
Emit MESSAGE and run GNUPLOT-SCRIPT. After the plotting is done,
run any forms in AFTER-BODY."
  `(let* ((default-directory "/tmp/secretary")
          (r-script (expand-file-name "R/make_data_for_plots.R"
				      (f-dirname (find-library-name "secretary")))))
     (mkdir "/tmp/secretary" t)
     ;; TODO: reuse secretary's R process to minimize package load time, so
     ;; we can use use call-process for easier reasoning.
     (set-process-sentinel
      (start-process secretary-ai-name nil "Rscript" r-script)
      (lambda (_process _event)
	(secretary-emit ,message)
	(switch-to-buffer (secretary-buffer-chat))
	(goto-char (point-max))
	(call-process "gnuplot" ,gnuplot-script (secretary-buffer-chat))
	;; Strip formfeed inserted by gnuplot.
	(search-backward "\f")
	(replace-match "")
	(goto-char (point-max))
	,@after-body))))

(defun secretary-plot-mood ()
  (interactive)
  (let* ((default-directory "/tmp/secretary")
	 (pkg-dir (f-dirname (find-library-name "secretary")))
         (script (expand-file-name "R/sc_daily_plot.R" pkg-dir))
         (plot (expand-file-name "sc_mood.png" default-directory)))
    (secretary-emit "Plotting mood...")
    (mkdir "/tmp/secretary" t)
    (set-process-sentinel
     (start-process secretary-ai-name nil "Rscript" script "14" "-0.1")
     `(lambda (_process _event)
        (secretary-emit "And that's your mood." "\n")
        (switch-to-buffer (secretary-buffer-chat))
        (insert-image-file ,plot)
        (goto-char (point-max))
        (insert "\n")))))

;;;###autoload
(defun secretary-plot-mood-ascii ()
  (interactive)
  (secretary--plot-ascii
   (expand-file-name "mood.gnuplot"
		     (f-dirname (find-library-name "secretary")))
   "Plotting mood..."
   (search-backward "Plotting mood...")
   (forward-line 1)
   (goto-char (line-end-position))
   (just-one-space)
   (insert-rectangle (last
		      (split-string (f-read "/tmp/secretary/mood_desc.txt"))
		      16))
   (goto-char (point-max))))

(defun secretary-plot-weight ()
  (interactive)
  (let* ((default-directory "/tmp/secretary")
         (script (expand-file-name "R/sc_daily_plot.R"
				   (f-dirname (find-library-name "secretary"))))
         (plot (expand-file-name "sc_plot1.png" "/tmp/secretary")))
    (secretary-emit "Plotting weight...")
    (mkdir "/tmp/secretary" t)
    (set-process-sentinel
     (start-process secretary-ai-name nil "Rscript" script "14" "-0.1")
     `(lambda (_process _event)
        (secretary-emit "And here's your weight, boss." "\n")
        (switch-to-buffer (secretary-buffer-chat))
        (insert-image-file ,plot)
        ;; (delete-file ,plot)
        (goto-char (point-max))
        (insert "\n")))))

;;;###autoload
(defun secretary-plot-weight-ascii ()
  (interactive)
  (let* ((default-directory "/tmp/secretary")
         (r-script (expand-file-name "R/make_data_for_plots.R"
				   (f-dirname (find-library-name "secretary"))))
	 (gnuplot-script (expand-file-name "weight.gnuplot"
					   (f-dirname (find-library-name "secretary")))))
    (mkdir "/tmp/secretary" t)
    ;; TODO: reuse secretary's R process to minimize package load time.
    (set-process-sentinel
     ;; TODO: pass start-date (today minus 3mo) and projection incline, letting
     ;; user change the incline (persist for future plots too)
     (start-process secretary-ai-name nil "Rscript" r-script)
     `(lambda (_process _event)
	(secretary-emit "Plotting weight...")
	(switch-to-buffer (secretary-buffer-chat))
	(goto-char (point-max))
	(call-process "gnuplot" ,gnuplot-script (secretary-buffer-chat))
	;; Strip formfeed inserted by gnuplot.
	(search-backward "\f")
	(replace-match "")
	(goto-char (point-max))))))

;;;###autoload
(defun secretary-present-plots ()
  (interactive)
  (unless (null secretary-plot-hook)
    (secretary-emit (seq-random-elt '("I have plotted the latest intel, boss."
				      "Here are some projections!"
				      "Data is beautiful, don't you think?")))
    ;; HACK: because of async; want sync.
    (dolist (hook secretary-plot-hook)
      (funcall hook)
      (sit-for .5))))


;;;; Other presenters

;;;###autoload
(defun secretary-present-mood ()
  (interactive)
  (view-file "/home/kept/Self_data/mood.tsv")
  (auto-revert-mode))

;; WIP
(defun secretary-present-ledger ()
  (interactive)
  (when (fboundp #'ledger-report)
    (let ((file "/home/kept/Journal/Finances/clean_start.ledger"))
      (with-current-buffer (get-buffer-create file)
	(ledger-report (car ledger-reports) nil)))))

;; WIP
(defun secretary-make-ods ()
  "Make an ODS spreadsheet of variables for you to play with."
  (interactive))


;;;; Diary presenter

(defvar secretary-past-sample-function #'secretary-past-sample-default)

(defun secretary-past-sample-default (&optional ts)
  "Return a list of ts objects referring to yesterday, this
weekday the last 4 weeks, this day of the month the last 12
months, and this date the past 100 years."
  (let ((now (or ts (ts-now))))
    (-uniq (append
            (--iterate (ts-dec 'day 1 it) now 1)
            (--iterate (ts-dec 'woy 1 it) now 4)
            (--iterate (ts-dec 'month 1 it) now 12)
            (--iterate (ts-dec 'year 1 it) now 100)))))

(defun secretary-make-indirect-datetree (buffer dates)
  (require 'org)
  (let ((dates (-sort 'ts<= dates))
        (counter 0))
    (switch-to-buffer buffer)
    (org-mode)
    (delete-region (point-min) (point-max))
    (with-temp-buffer
      (insert-file-contents  "/home/kept/Journal/diary2.org")
      (org-with-wide-buffer
       (goto-char (point-min))
       (dolist (date dates)
         (when (search-forward (concat "* " (ts-format "%F" date)) nil t)
           (setq counter (1+ counter)) ;; for summary in prompt
           (goto-char (line-beginning-position)) ;;(beginning-of-line)
           (let ((beg (point)))
             (org-next-visible-heading 1)
             (while (< 3 (org-reduced-level (org-outline-level)))
               (org-next-visible-heading 1))
             (append-to-buffer (get-buffer buffer) beg (point)))))))
    (if (> counter 0)
        (dotimes (_ 2)
          (org-map-region #'org-promote (point-min) (point-max)))
      (kill-buffer buffer))
    counter))
;; (secretary-make-indirect-datetree (get-buffer-create "test")

(defun secretary-existing-diary (&optional date dir file-format)
  "Return the first file in DIR matching FILE-FORMAT.
FILE-FORMAT is handled by `parse-time-string'. The value returned
is a full filesystem path or nil.

When DATE is nil, use today.  Should be a ts object.
When DIR is nil, use `org-journal-dir'.
When FILE-FORMAT is nil, use `org-journal-file-format'.

Note that org-journal is not needed."
  (let* ((dir (or dir (bound-and-true-p org-journal-dir)))
	 (file-format (or file-format (and (boundp 'org-journal-file-type)
					   (eq org-journal-file-type 'daily)
					   org-journal-file-format)))
	 (file (--find (string-match-p (ts-format file-format date)
				       it)
                       (directory-files dir))))
    (unless (null file)
      (expand-file-name file dir))))
;; (secretary-existing-diary (ts-dec 'day 1 (ts-now)))

;; (defun secretary-existing-diary (dir date)
;;   (declare (side-effect-free t))
;;   (let ((foo (car (--filter (string-match-p
;;                              (concat (ts-format "%y%m%d" date) ".*org$")
;;                              it)
;;                             (directory-files dir)))))
;;     (unless (null foo)
;;       (expand-file-name foo dir))))
;; ;; (secretary-existing-diary "/home/kept/Diary" (ts-dec 'month 1 (ts-now)))

;; TODO: allow it to check a different date
;; TODO: allow either discrete or datetree to be nil
;; TODO: allow a list of datetrees
;; TODO: make separate buffers for each datetree entry (use rename-buffer)
;; TODO: make the datetree buffer(s) the next in line when you pop the last
;;       discrete view
;; TODO: try creating a sparse tree, so user can edit in-place
;; TODO: show also the agenda log for each date if not empty
;;;###autoload
(defun secretary-present-diary (&optional date)
  (interactive)
  (let* ((buffer (get-buffer-create (concat "*" secretary-ai-name ": Selected diary entries*")))
         (dates-to-check (funcall secretary-past-sample-function date))
         (discrete-files-found (--keep (secretary-existing-diary it "/home/kept/Diary") dates-to-check))
         (datetree-found-count (secretary-make-indirect-datetree buffer dates-to-check))
         (total-found-count (+ (length discrete-files-found) datetree-found-count)))
    (if (secretary-prompt "Found " (int-to-string total-found-count) " past diary "
			  (if (= 1 total-found-count) "entry" "entries")
			  " relevant to this date. Want me to open "
			  (if (= 1 total-found-count) "it" "them")
			  "?")
        (progn
          (switch-to-buffer buffer)
          (view-mode)
          (if (-non-nil discrete-files-found)
              (dolist (x discrete-files-found)
                (view-file x))))
      (kill-buffer buffer))))
;; (secretary-present-diary (ts-now))


;;;; Handle idle & reboots & crashes

(defvar secretary--timer nil)

(defvar secretary--idle-beginning (ts-now))

(defvar secretary-length-of-last-idle 0
  "Length of the last idle period, in seconds.")

(defcustom secretary-idle-threshold (* 10 60)
  "Duration in seconds, above which the user is considered idle."
  :group 'secretary
  :type 'number)

(defcustom secretary-long-idle-threshold (* 90 60)
  "Be idle at least this many seconds to be greeted upon return."
  :group 'secretary
  :type 'number)

(defcustom secretary-return-from-idle-hook nil
  "Hook run when user returns from a period of idleness.
Note: An Emacs startup also counts as a return from idleness.
You'll probably want your hook to be conditional on some value of
`secretary-length-of-last-idle', which at startup is calculated from
the last Emacs shutdown or crash (technically, last time
`secretary-mode' was running)."
  :group 'secretary
  :type '(repeat function))

(defcustom secretary-periodic-not-idle-hook nil
  "Hook run every minute when the user is not idle."
  :group 'secretary
  :type '(repeat function))

(defun secretary--start-next-timer (&optional assume-idle)
  "Start one or the other timer depending on idleness. If
ASSUME-IDLE is non-nil, skip the idle check and associated
overhead: useful if the caller has already checked it."
  (if (or assume-idle (secretary-idle-p))
      (named-timer-run :secretary 2 nil #'secretary--user-is-idle t)
    (named-timer-run :secretary 60 nil #'secretary--user-is-active)))

(defun secretary--user-is-active ()
  "Do stuff assuming the user is active (not idle).
This function is called by `secretary--start-next-timer'
repeatedly for as long as the user is active (not idle).

Refresh some variables and sync all variables to disk."
  ;; Guard the case where the user puts the computer to sleep manually, which
  ;; means this function will still be queued to run when the computer wakes.  If
  ;; the time difference is suddenly big, hand off to the other function.
  (if (> (ts-diff (ts-now) secretary--idle-beginning) secretary-idle-threshold)
      (secretary--user-is-idle)
    (setq secretary--idle-beginning (ts-now))
    (secretary--start-next-timer)
    ;; Run hooks last, in case they have bugs.
    (run-hooks 'secretary-periodic-not-idle-hook)))

(defun secretary--user-is-idle (&optional decrement)
  "Do stuff assuming the user is idle.
This function is called by `secretary--start-next-timer'
repeatedly for as long as the user is idle.

When the user comes back, this function will be called one last
time, at which point the idleness condition will fail and it sets
`secretary-length-of-last-idle' and runs
`secretary-return-from-idle-hook'.  That it has to run exactly
once with a failing condition that normally succeeds, as opposed
to running never or forever, is the reason it has to be a
separate function from `secretary--user-is-active'."
  (if (secretary-idle-p)
      (secretary--start-next-timer 'assume-idle)
    ;; Take the idle threshold into account and correct the idle begin point.
    (when decrement
      (ts-decf (ts-sec secretary--idle-beginning) secretary-idle-threshold))
    (setq secretary-length-of-last-idle (ts-diff (ts-now)
						 secretary--idle-beginning))
    (unwind-protect
	(run-hooks 'secretary-return-from-idle-hook)
      (setq secretary--idle-beginning (ts-now))
      (secretary--start-next-timer))))



(defun secretary--keepalive ()
  (unless (member (named-timer-get :secretary) timer-list)
    (message "[%s] secretary timer found dead, reviving it."
	     (format-time-string "%H:%M"))
    (secretary--start-next-timer)))

(defun secretary--another-secretary-running-p ()
  "Return t if another Emacs instance has secretary-mode on.
Return nil if only the current Emacs or none has it on.
Behavior indeterminate you've forced it on in several Emacsen."
  (when (file-exists-p "/tmp/secretary/pid")
    (let ((pid (string-to-number (f-read-bytes "/tmp/secretary/pid"))))
      (and (/= pid (emacs-pid))
	   (member pid (list-system-processes))))))
;; (secretary--another-secretary-running-p)

(defun secretary--restore-variables-from-disk ()
  (when (f-exists-p secretary-idle-beginning-file-name)
    (setq secretary--idle-beginning
          (ts-parse (f-read secretary-idle-beginning-file-name))))
  (when (f-exists-p secretary-mood-alist-file-name)
    (setq secretary-mood-alist
          (read (f-read secretary-mood-alist-file-name)))))

(defun secretary--save-variables-to-disk ()
  (make-directory secretary-dir t)
  (f-write (ts-format secretary--idle-beginning) 'utf-8 secretary-idle-beginning-file-name)
  (f-write (prin1-to-string secretary-mood-alist) 'utf-8 secretary-mood-alist-file-name))

;; REVIEW: Is this necessary? See Info node: (elisp)Unloading
(defun secretary-unload-function ()
  "Unload the Secretary library."
  (secretary-mode 0)
  (with-demoted-errors nil
    (unload-feature 'secretary-tests)
    (unload-feature 'secretary-config))
  ;; Continue standard unloading.
  nil)

;;;###autoload
(define-minor-mode secretary-mode
  nil
  :global t
  (if secretary-mode
      (when (and
	     (cond ((eq system-type 'darwin)
		    (autoload #'org-mac-idle-seconds "org-clock")
		    (fset 'secretary--idle-seconds #'org-mac-idle-seconds)
		    t)
                   ((and (eq window-system 'x)
                         (executable-find secretary-x11idle-program-name))
		    (fset 'secretary--idle-seconds #'secretary--x11-idle-seconds)
		    t)
                   (t
		    (secretary-mode 0)
		    (message secretary-ai-name ": Not able to detect idleness, "
			     "I'll be useless. Disabling secretary-mode.")
		    nil))
	     (if (secretary--another-secretary-running-p)
		 (progn
		   (message "Another secretary active.")
		   (secretary-mode 0)
		   nil)
               t)
	     (if (--all-p (and (boundp it)
			       (not (null it)))
			  '(secretary-aphorisms
			    secretary-queriers))
		 t
               (message "Needed variables not set, read manual or do %s."
			"M-x load-library secretary-config")
               (secretary-mode 0)
               nil))
	(mkdir "/tmp/secretary/" t)
	(f-write (number-to-string (emacs-pid))
		 'utf-8 "/tmp/secretary/pid")
        (add-hook 'secretary-return-from-idle-hook #'secretary-log-idle -90)
        (add-hook 'secretary-return-from-idle-hook #'secretary-call-from-idle 90)
	(add-hook 'secretary-periodic-not-idle-hook #'secretary--save-variables-to-disk)
	(add-hook 'secretary-periodic-not-idle-hook #'secretary--save-buffer-logs-to-disk)
        (add-hook 'window-buffer-change-functions #'secretary-log-buffer)
        (add-hook 'window-selection-change-functions #'secretary-log-buffer)
	(add-hook 'after-init-hook #'secretary--restore-variables-from-disk -90)
        (add-hook 'after-init-hook #'secretary--start-next-timer 90)
	(named-timer-run :secretary-keepalive 300 300 #'secretary--keepalive)
        ;; (add-function :after #'after-focus-change-function #'secretary-log-buffer)
        (when after-init-time
          (progn
            (when (-any #'null '(secretary-idle-beginning
				 secretary-mood-alist))
              (secretary--restore-variables-from-disk))
            (secretary--start-next-timer))))
    (f-delete "/tmp/secretary/pid")
    (remove-hook 'secretary-return-from-idle-hook #'secretary-log-idle)
    (remove-hook 'secretary-return-from-idle-hook #'secretary-call-from-idle)
    (remove-hook 'secretary-periodic-not-idle-hook #'secretary--save-variables-to-disk)
    (remove-hook 'window-buffer-change-functions #'secretary-log-buffer)
    (remove-hook 'window-selection-change-functions #'secretary-log-buffer)
    (remove-hook 'after-init-hook #'secretary--restore-variables-from-disk)
    (remove-hook 'after-init-hook #'secretary--start-next-timer)
    (named-timer-cancel :secretary)
    (named-timer-cancel :secretary-keepalive)))

(provide 'secretary)

;;; secretary.el ends here
