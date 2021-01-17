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

(setc secretary-activities-alist
      '(;; id            cost of misprediction (either false positive or false negative)
        ("7441f4e2-7251-47d8-ab06-1c2c205d1ae0"    0)  ;; unknown
        ("c1b1fdb8-ea87-4d5a-b01d-4214266c4f4b"    1)  ;; unknown (afk)
        ("24553859-2214-4fb0-bdc9-84e7f3d04b2b"    8)  ;; studying
        ("a0fdbb69-9cdb-418f-b5e8-37601af43c0d"    6)  ;; coding
        ("784d67a5-c15b-4c09-8c74-97b5767d70e6"    2)  ;; downtime
        ("7aaf9105-d58d-4e83-9d34-045a3c922ac5"   20)  ;; meditating
        ("ac93c132-ab74-455f-a456-71d7b5ee88a6"    3)  ;; sleep
        ))

(setc secretary-activities-alist
      '(;; id            cost of misprediction (either false positive or false negative)
        ("unknown" 0)
        ("unknown afk"     1)  ;; unknown (afk)
        ("studying"    8)  ;; studying
        ("coding"    6)  ;; coding
        ("downtime"    2)  ;; downtime
        ("meditating"   20)  ;; meditating
        ("sleep"    3)  ;; sleep
        ))

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
(require 'parse-csv)
(require 'named-timer)

(autoload #'secretary-log-buffer "org-id")
(autoload #'org-mac-idle-seconds "org-clock")
(autoload #'org-x11-idle-seconds "org-clock")
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
Info node `(org) Moving subtrees', or just write/capture
everything directly into it.  Used by
`secretary-present-diary'."
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

;; TODO: deprecate
(defcustom secretary-activities-alist
  '(;; id            cost of misprediction (either false positive or false negative)
    ("some-org-id"    8)  ;; study
    ("some-org-id"    6)  ;; coding
    ("some-org-id"    2)  ;; downtime
    ("some-org-id"   20)  ;; meditating
    ("some-org-id"    3)  ;; sleep
    ("some-org-id"    1)  ;; unknown (afk)
    ("some-org-id"    0)  ;; unknown (must be 0 as the default guess)
    )
  nil
  :group 'secretary
  :type '(list string number))

;; REVIEW: see that there's no problem if you delete secretary-dir
(defvar secretary-mood-alist nil
  "For suggesting a score in the `secretary-log-mood' prompt.
Merely a convenience for auto-completion; the scores are not
forced.

The variable populates itself through use, and syncs with a file
at `secretary-mood-alist-file-name'.")

;; (setq secretary-mood-alist '(("bemused" . "3")
;; 			 ("amused" . "5")
;; 			 ("great"  . "5")
;; 			 ("annoyed" . "3")
;; 			 ("depressed" . "1")))

(defvar secretary-tsv-alist
  '(("/home/kept/Self_data/weight.tsv" secretary-query-weight)
    ("/home/kept/Self_data/mood.tsv" secretary-query-mood)
    ("/home/kept/Self_data/ingredients.tsv" secretary-query-ingredients)
    ("/home/kept/Self_data/sleep.tsv" secretary-query-sleep)
    ("/home/kept/Self_data/meditation.tsv" secretary-query-meditation)
    ("/home/kept/Self_data/cold.tsv" secretary-query-cold)))

(defvar secretary-log-alist
  '(("/home/kept/Self_data/buffers.tsv" secretary-log-buffer)
    ("/home/kept/Self_data/buffer_focus.tsv" secretary-log-buffer)
    ("/home/kept/Self_data/idle.tsv" secretary-log-idle)))

(defvar secretary--date (ts-now)
  "Date to which to apply the current query.
Can be set anytime during a welcome to override the date to which
some queries apply, for example to log something for yesterday.
This may not apply, check the source for the welcomer you are
using.")

(defvar secretary-chime-sound-file
  (expand-file-name
   ;; From https://freesound.org/people/josepharaoh99/sounds/380482/
   "assets/Chime Notification-380482.wav"
   ;; From https://bigsoundbank.com/detail-0319-knock-on-a-glass-door-1.html
   ;; "assets/DOORKnck_Knock on a glass door 1 (ID 0319)_BSB.wav"
   (f-dirname (find-library-name "secretary")))
  "Sound to play when a welcomer is triggered unannounced.")

(defvar secretary-greetings '("Welcome back, Master."
			      (concat "Nice to see you again, " secretary-user-name ".")
			      (concat "Greetings, " secretary-user-name "."))
  "Greeting phrases which can initiate a conversation.")

;; Q: What's cl-defstruct? A: https://nullprogram.com/blog/2018/02/14/
(cl-defstruct (secretary-activity (:constructor secretary-activity-create)
				  (:copier nil))
  name
  id
  cost-false-pos
  cost-false-neg
  query-struct)

(defun secretary-activity-by-name (name)
  (declare (side-effect-free t))
  (--find (equal name (secretary-activity-name it)) secretary-activities))

;; TODO: There's no need to use a name, the query-fn symbol can be its identifier.
(cl-defstruct (secretary-querier (:constructor secretary-querier-create)
			(:copier nil))
  fn
  log-file
  max-entries-per-day
  (min-hours-wait 3)
  last-called)

(defun secretary--get-associated-struct (fn)
  (--find (equal fn (secretary-querier-fn it)) secretary-queriers))

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

(defun secretary-do-query-maybe (fn &optional ts ignore-wait)
  "Call the query function FN if appropriate.
Do nothing if recently logged, reached max-entries-per-day, etc."
  (let* ((q (secretary--get-associated-struct fn))
	 (last-called (secretary-querier-last-called q))
	 (min-hrs (secretary-querier-min-hours-wait q))
	 (min-secs (* 60 60 min-hrs))
	 (file (secretary-querier-log-file q))
	 (max-entries (secretary-querier-max-entries-per-day q))
	 (recently-logged
	  (when (file-exists-p file)
	    (> min-secs
	       (ts-diff (ts-now)
			(ts-parse (car (secretary--last-in-tsv file))))))))
    (unless (and recently-logged (null ts))
      (when (or (null max-entries)
		(not (file-exists-p file))
		(> max-entries (length (secretary--get-entries-in-tsv file ts))))
	(when (or ignore-wait
		  (null last-called)
		  (< min-hrs (/ (ts-diff (ts-now) last-called) 60 60)))
	  (setf (secretary-querier-last-called q) (ts-now))
	  (funcall fn ts))))))

;; (defun test ()
;;   (interactive)
;;   (secretary-do-query-maybe #'secretary-query-sleep nil t))

(defvar secretary-queriers
  "To be customized by user."
  (list (secretary-querier-create :fn #'secretary-query-sleep
				  :log-file "/home/kept/Self_data/sleep.tsv"
				  :min-hours-wait 5)
	(secretary-querier-create :fn #'secretary-query-weight
				  :log-file "/home/kept/Self_data/weight.tsv"
				  :max-entries-per-day 1)
	(secretary-querier-create :fn #'secretary-query-mood
				  :log-file "/home/kept/Self_data/mood.tsv")
	(secretary-querier-create :fn #'secretary-query-ingredients
				  :log-file "/home/kept/Self_data/ingredients.tsv"
				  :min-hours-wait 5)
	(secretary-querier-create :fn #'secretary-query-cold-shower
				  :log-file "/home/kept/Self_data/cold.tsv"
				  :max-entries-per-day 1)))

(defvar secretary--inactive-queries-file
  "/home/kept/Emacs/secretary/pseudo-userdir/inactive-queries.el")

;; TODO: move to read/save on disk functions
(defvar secretary--active-queries
  (let* ((all (-map #'secretary-querier-fn secretary-queriers))
	 (inactive (read (f-read secretary--inactive-queries-file))))
    (-difference all inactive)))



(defun secretary-play-chime ()
  (and (executable-find "aplay")
       (file-exists-p secretary-chime-sound-file)
       (start-process "aplay" nil "aplay" secretary-chime-sound-file)))

;; TODO: deprecate
(defun secretary-activities-names ()
  (->> secretary-activities-alist
       (-map (lambda (x) (save-window-excursion
                      (org-id-goto (car x))
                      (-last-item (org--get-outline-path-1)))))))

(defvar secretary--last-msg (ts-format "[%H:%M] Recorded blah"))
(defun secretary-prompt (&rest strings)
  (let* (;; (default-y-or-n-p-map y-or-n-p-map)
         ;; (default-cmd (lookup-key y-or-n-p-map (kbd "k")))
	 (info (concat "Applying to date: " (ts-format "%Y-%B-%d" secretary--date) "\n"
		       secretary--last-msg "\n"))
	 (prompt (string-join strings)))
    (unwind-protect
        (progn
          (switch-to-buffer (secretary-buffer-chat))
          (unless (< 20 (car (window-fringes)))
            (set-window-fringes nil 20 20))
          (goto-char (point-max))
          (secretary-emit prompt)
          (define-key y-or-n-p-map (kbd "o") #'secretary-special-handle-current-query)
          (define-key y-or-n-p-map (kbd "i") #'secretary-special-handle-current-query)
          (define-key y-or-n-p-map (kbd "k") #'secretary--y-or-n-p-insert-k)
	  (let ((result (y-or-n-p (concat info prompt))))
	    (if secretary--k
		(progn
		  (setq secretary--k nil)
		  (insert " Okay")
		  t)
	      (if result
		  (progn
		    (insert " Yes")
		    t)
		(insert " No")
		nil)))
	  (setq secretary--last-msg (buffer-substring (line-beginning-position)
						      (line-end-position)))
	  (insert "\n"))
      (dolist (x '("o" "i" "k"))
        (define-key y-or-n-p-map (kbd x) #'y-or-n-p-insert-other)))))
;; (secretary-prompt "Test")
;; (y-or-n-p "Test")

(defvar secretary--k nil)
(defun secretary--y-or-n-p-insert-k ()
  "Mostly like `y-or-n-p-insert-y'."
  (interactive)
  (delete-minibuffer-contents)
  (insert "y")
  (setq secretary--k t)
  (exit-minibuffer))

(defun secretary--idle-seconds ()
  "Stub to be redefined."
  (warn "Code ended up in an impossible place."))

(defvar secretary--last-edited (ts-now)
  "Timestamp updated whenever `secretary-emit' runs.")

(defun secretary-emit (&rest strings)
  (secretary-print-new-date-maybe)
  (setq secretary--last-edited (ts-now))
  (prog1 (message (string-join strings))
    (with-current-buffer (secretary-buffer-chat)
      (goto-char (point-max))
      (insert "\n<" (ts-format "%H:%M") "> " (string-join strings)))))

(defun secretary-print-new-date-maybe ()
  (when (/= (ts-day (ts-now))
            (ts-day secretary--last-edited))
    (with-current-buffer (secretary-buffer-chat)
      (goto-char (point-max))
      (insert "\n\n" (ts-format "%Y, %B %d") (secretary--holiday-maybe)))))
;; (secretary-print-new-date-maybe)

(defun secretary--holiday-maybe ()
  (require 'calendar)
  (require 'holidays)
  (if-let (foo (calendar-check-holidays (calendar-current-date)))
      (concat " -- " foo)
    ""))

(defun secretary--buffer-r ()
  (get-buffer-create (concat "*" secretary-ai-name ": R*")))

(defun secretary-reschedule ()
  (run-with-timer 3600 nil #'secretary-call-from-reschedule))

;; (defmacro secretary-with-file (path &rest body)
;;   (declare (pure t) (indent defun))
;;   `(with-temp-buffer
;;      (insert-file-contents-literally ,path)
;;      ,@body))

;; REVIEW
(defun secretary-last-date-string-in-date-indexed-csv (path)
  (declare (side-effect-free t))
  (with-temp-buffer
    (insert-file-contents-literally path)
    (goto-char (point-max))
    (re-search-backward (rx bol (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)))
    (buffer-substring (point) (+ 10 (point)))))
;; (secretary-last-date-string-in-date-indexed-csv "/home/kept/Self_data/weight.tsv")
;; (secretary-last-date-string-in-date-indexed-csv "/home/kept/Self_data/mood.tsv")
;; (secretary-last-date-string-in-file "/home/kept/Self_data/mood.tsv")

(defun secretary-last-date-string-in-file (path)
  (declare (side-effect-free t))
  (with-temp-buffer
    (insert-file-contents-literally path)
    (goto-char (point-max))
    (re-search-backward
     (rx (or (group (= 4 digit) "-" (= 3 wordchar) "-" (= 2 digit))
             (group (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)))))
    (buffer-substring (point) (+ 11 (point)))))

;; (parse-csv-string-rows
;;  (f-read "/home/kept/Self_data/weight.csv") (string-to-char ",") (string-to-char " ") "\n")

;; REVIEW
(defun secretary-get-all-today-in-date-indexed-csv (path &optional ts)
  (with-temp-buffer
    (insert-file-contents-literally path)
    (let (x)
      (while (search-forward (ts-format "%F" ts) nil t)
        (push (parse-csv->list (buffer-substring (line-beginning-position)
                                                 (line-end-position)))
              x))
      x)))
;; (secretary-get-all-today-in-date-indexed-csv "/home/kept/Self_data/sleep.csv" (ts-dec 'day 1 (ts-now)))

;; (defun secretary-update-or-append-in-date-indexed-csv (path &optional ts)
;;   (secretary-get-first-today-in-date-indexed-csv path ts))

(defun secretary--last-in-tsv (path)
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-max))
    (when (looking-back "^") ;; if empty line
      (forward-line -1))
    (split-string (buffer-substring (line-beginning-position)
				    (line-end-position))
		  "\t")))

;; REVIEW
(defun secretary-get-first-today-in-date-indexed-csv (path &optional ts)
  (with-temp-buffer
    (insert-file-contents path)
    (search-forward (ts-format "%F" ts))
    (buffer-substring (line-beginning-position) (line-end-position))))
;; (secretary-get-first-today-in-date-indexed-csv "/home/kept/Self_data/ingredients.csv")

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
  (declare (side-effect-free t))
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

(defun secretary-buffer-mode (buffer-or-name)
  "Retrieve the `major-mode' of BUFFER-OR-NAME."
  (declare (side-effect-free t))
  (buffer-local-value 'major-mode (get-buffer buffer-or-name)))

(defmacro secretary--process-output-to-string (program &rest args)
  "Similar to `shell-command-to-string', but skips the shell
intermediary so you don't need a /bin/sh. PROGRAM and ARGS are
passed on to `call-process'."
  (declare (debug (&rest form))
	   (indent nil)
	   (side-effect-free t))
  `(with-temp-buffer
     (call-process ,program nil (current-buffer) nil ,@args)
     (buffer-string)))

(defmacro secretary--process-output-to-number (program &rest args)
  "Pipe `secretary--process-output-to-string' through `string-to-number'."
  (declare (debug (&rest form))
	   (indent nil)
	   (side-effect-free t))
  `(string-to-number (secretary--process-output-to-string ,program ,@args)))

(defun secretary-idle-p ()
  (declare (side-effect-free t))
  (> (secretary--idle-seconds) secretary-idle-threshold))

(defvar secretary--x11idle-program-name
  (or org-clock-x11idle-program-name "xprintidle"))

(defun secretary--x11-idle-seconds ()
  "Like `org-x11-idle-seconds' but doesn't need a /bin/sh, nor to
load org."
  (declare (side-effect-free t))
  (/ (secretary--process-output-to-number secretary--x11idle-program-name) 1000))

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
(defmacro secretary--raise-frame-and-do (&rest body)
  `(progn
     (and (frame-live-p secretary--frame)
	  (not (= 1 (length (frames-on-display-list))))
	  (delete-frame secretary--frame t))
     (setq secretary--frame (make-frame '((name . "Secretary")
					 (buffer-predicate . nil))))
     (select-frame-set-input-focus secretary--frame t)
     ,@body))

;; The IANA standard disallows tabs within fields, simplifying sanity checks.
;; https://www.iana.org/assignments/media-types/text/tab-separated-values
(defun secretary-append-tsv (path &rest fields)
  "Append a line to the file located at PATH, creating it and its
parent directories if it doesn't exist, and making sure it begins
on a newline. Treat each argument FIELDS as a separate data
field, inserting a tab character in between."
  (declare (indent defun))
  (unless (file-exists-p path)
    (make-empty-file path t))
  (unless (-all-p #'stringp fields)
    (warn "[%s] `secretary-append-tsv' was passed nil" (ts-format "%H:%M")))
  (let* ((fields (-replace nil "" fields))
	 (newline-maybe (if (s-ends-with-p "\n" (f-read-bytes path))
                            ""
                          "\n"))
	 ;;(cols-count ) ;; TODO sane number of cols
	 (errors-path (concat path "_errors"))
	 (new-text (concat newline-maybe (string-join fields "\t"))))
    (cond ((--any-p (s-contains-p "\t" it) fields)
	   (warn "Log entry had tabs inside fields, wrote to %s" errors-path)
	   (f-append new-text 'utf-8 errors-path))
	  ((s-contains-p "\n" new-text)
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

(defvar secretary-aphorisms
  '("The affairs of the world will go on forever. Do not delay the practice of meditation." ;; not koanish
    "It takes all the running you can do, to keep in the same place."
    "You can't hate yourself into someone that loves who they are."
    "Don't show up to prove, show up to improve."
    "If you are completely okay with ‘never doing’ and all the consequences which follow from never taking action, it’s easier to step into ‘always doing’."
    ;; https://mxplx.com/schema/58/
    "Life feeds on negentropy, sucking orderliness from its environment."
    "The cosmos is just right intellectually, where things are knowable but endlessly challenging."
    "Atoms are so small that if you poured a glass of water into the ocean and gave them time to distribute evenly, when you later took a glass from the ocean you would find a hundred of the original molecules within it."
    "Big whorls have little whorls / Which feed on their velocity / And little whorls have lesser whorls, / And so on to viscosity."
    "We live in a Universe where gravity pulls galaxies towards one another, but universal expansion pulls them apart."
    "All life is made of lifeless sub-components below the level of the cell."
    "An egg is a chemical process with the power to become a lifeless omelet or a living chicken."
    "Everything is vibrations, light and other forms of radiation are roaming vibrations and matter is bottled up vibrations."
    "The total energy of the Universe is Zero."
    "The Cartesian Duality, the idea that our mind is separate from our matter, is an illusion."
    "Light is both spread out and localized."
    "Everything society builds and every social interaction we have with others is a plexus of motor habits."
    "All matter is mostly empty space."
    "The second law of thermodynamics as a profound distinction between past and future."
    "The origin of the universe is ultimately unknowable."
    ;; https://en.wikipedia.org/wiki/Koan
    "If you meet the Buddha, kill the Buddha."
    "One day as Manjusri stood outside the gate, the Buddha called to him, “Manjusri, Manjusri, why do you not enter?” Manjusri replied, “I do not see myself as outside. Why enter?”"
    "Koan after koan explores the theme of nonduality. Hakuin's well-known koan, \"Two hands clap and there is a sound, what is the sound of one hand?\" is clearly about two and one. The koan asks, you know what duality is, now what is nonduality? In \"What is your original face before your mother and father were born?\" the phrase \"father and mother\" alludes to duality. This is obvious to someone versed in the Chinese tradition, where so much philosophical thought is presented in the imagery of paired opposites. The phrase \"your original face\" alludes to the original nonduality."
    "Subject and object - this is two hands clapping. When the monk realizes that the koan is not merely an object of consciousness but is also he himself as the activity of seeking an answer to the koan, then subject and object are no longer separate and distinct [...] This is one hand clapping."
    ;; https://www.greaterwrong.com/posts/3GZmttQJ4CRdMLsmJ/aphorisms-on-motivation
    "All motivation ultimately stems from belief. If you don’t know why you want something, it stems from an alief that is hidden."
    "Most of your hidden motivations will be hidden because they’re not socially desirable, and/or because they’re not consistent with your identity. I am not a paragon of virtue. I am just scared to own up to my vices, and it’s keeping me from integrating myself. The same is true for you and for everyone that isn’t completely enlightened."
    "Your System 1 won’t stop complaining about that marshmallow as long as it’s in front of you. Eat it or throw it away, but don’t let it sit there slowly deconstructing your internal consistency."
    "If you believe something is the right thing to do, but you can’t bring yourself to do it, then not all of you believes it."
    "Aliefs will be ten times less reasonable if you shut them out. If you embrace them, only the most nuanced part of it remains."
    "“Ego depletion” only makes sense if you don’t self-identify with your System 1. If you do, you will simply experience it as a preference."
    "People are a bag of subagents. If you interact with them, your bags mix. If they offend you, that’s often because they’ve expressed a subagent that you repress in yourself. Your reaction is how you would react to this part of you internally."
    ;; https://www.flightfromperfection.com/aphorisms-for-meditation.html
    "Am I putting labels on feelings & thoughts? Am I spinning a story about these feelings & thoughts?"
    "Am I imagining that this person actually is what I’m imagining them to be?"
    "Am I thinking that whatever I’m thinking about is actually what I think it is?"
    ;; https://www.greaterwrong.com/posts/n4ctAtDBNKnv78kxS/some-rationalistic-aphorisms
    "If you never have akrasia, you’re spending too much time on trivial challenges."
    "One of my goals lately has been to write a thousand words a day. I’ve noticed that the first three hundred are difficult but the last three thousand come naturally."
    "I wish people would just update on the expectation of being Dutch booked due to inconsistent preferences and give me money."
    "The feeling of knowing that one knows more than others is addicting and almost always wrong."
    "Analyze verbs timelessly and nouns timefully. That way you look at the part of the conceptualization that isn’t already explicit in its construction."
    "You can learn smart things from stupid people."
    "Nothing is art, but anything can be treated as art."
    "Adaptive is not optimal." ;; definitely koanish
    "We all have the strength to refuse what we are not offered."
    "You are what you fear to appear to be."
    "Whatever you have done, you are the sort of person who would do that."
    "Misspellers learn from speech, mispronouncers from books."
    "The people are flattered more obsequiously than the monarch ever was."
    "The incorruptible politician merely prefers power to money."
    "Why do aphorisms and cynicism go together? A good single sentence saying can’t require background evidencing or further explanation. It must be instantly recognizable as true. It also needs to be news to the listener. Most single sentences that people can immediately verify as true they already believe. What’s left? Things that people don’t believe or think about much for lack of wanting to, despite evidence. Drawing attention to these is called cynicism."
    ;; Atomic Habits
    "Missing once is an accident, missing twice is the start of a new habit."
    "If it happens once it's a mistake. If it happens twice, it's a choice."))


;;;; Greetings

(defun secretary-greeting-curt ()
  "Return a greeting appropriate in the midst of a workday.
Because if you've already exchanged good mornings, it's weird to
do so again."
  (seq-random-elt `("Hello" "Hi" "Hey")))

(defun secretary-greeting ()
  "Return a greeting string."
  ;; If it's morning, always use a variant of "good morning"
  (if (> 10 (ts-hour (ts-now)) 5)
      (seq-random-elt (secretary--daytime-appropriate-greetings))
    (eval (seq-random-elt (append secretary-greetings
                                  (-list (secretary--daytime-appropriate-greetings)))))))
;; (secretary-greeting)

;; NOTE: I considered making external variables for morning, day and evening
;; lists, but users might also want to change the daytime boundaries or even add
;; new boundaries. Too many possibilities, this is a case where it's ok to make
;; the user override the defun as a primary means of customization.
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


;;;; Data collection

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

(defun secretary-log-idle ()
  (secretary-append "/home/kept/Self_data/idle.tsv"
    (ts-format)
    "\t" (number-to-string (/ (round secretary-length-of-last-idle) 60))))

(defun secretary--save-buffer-logs-to-disk ()
  (secretary--transact-buffer-onto-file secretary-buffer-focus-log-buffer
					"/home/kept/Self_data/buffer-focus.tsv")
  (secretary--transact-buffer-onto-file secretary-buffer-existence-log-buffer
					"/home/kept/Self_data/buffer-existence.tsv"))

(defun secretary--transact-buffer-onto-file (buffer path)
  (with-current-buffer buffer
    (whitespace-cleanup)
    (f-append-text (buffer-string) 'utf-8 path)
    ;; TODO: delete region only if appending was successful
    (delete-region (point-min) (point-max))))

;; TODO: When buffer mode changes, count it as a new buffer. Note that (assoc
;; buf secretary-known-buffers) will still work.
(defun secretary-log-buffer (&optional _arg)
  (unless (minibufferp)
    (let* ((buf (current-buffer))
           (mode (symbol-name (secretary-buffer-mode buf)))
           (known (assoc buf secretary-known-buffers))
           (timestamp (s-pad-right 18 "0" (number-to-string (ts-unix (ts-now)))))
           (buffer-record (unless (and known
                                       (string= mode (nth 4 known))) ;; doesnt do it
                            (list buf
                                  (org-id-uuid)
                                  (buffer-name buf)
                                  (buffer-file-name buf)
                                  mode
                                  timestamp ;; time the buffer was first opened
                                  )))
           (focus-record (list timestamp ;; time the buffer was switched to
                               (if known (cadr known) (cadr buffer-record)) ;; uuid
                               )))
      (unless (eq secretary-last-buffer buf) ;; happens if you only entered and left minibuffer
        (setq secretary-last-buffer buf)
        (unless known
          (push buffer-record secretary-known-buffers)
	  (with-current-buffer secretary-buffer-existence-log-buffer
	    (goto-char (point-max))
	    (insert "\n" (string-join (cdr buffer-record) "\t"))))
	(with-current-buffer secretary-buffer-focus-log-buffer
	  (goto-char (point-max))
	  (insert "\n" (string-join focus-record "\t")))))))

;; TODO: make the dataset append-only
;;;###autoload
(defun secretary-query-ingredients (&optional ts)
  (interactive)
  (let* ((response (read-string "Comma-separated list of ingredients: "))
         (formatted-response (->> response
                                  (s-split (rx (+ (any "," blank))))
                                  (s-join ", ")
                                  (s-replace "\"" "'")))
         (path "/home/kept/Self_data/ingredients.tsv"))
    (secretary-append path
		      (ts-format ts)
		      "\t" "\"" formatted-response "\"")
    (secretary-emit "Recorded so far today: "
		    (s-replace "^.*?," ""
			       (secretary-get-first-today-in-date-indexed-csv path)))))

;; (defalias #'secretary-query-food #'secretary-query-ingredients)

(defun secretary--sanity-check-csv ()
  ;; look for overlapping time intervals
  (pcsv-parse-file "/home/kept/Self_data/idle.csv")

  (pcsv-parse-file "/home/kept/Self_data/idle.csv"))

;;;###autoload
(defun secretary-query-activity ()
  (interactive)
  (let* ((name (completing-read "What are you up to? " (secretary-activities-names)))
	 (now (ts-now)))
    (secretary-append "/home/kept/Self_data/activity.tsv"
		      (ts-format now)
		      "\t" name
		      "\t" (secretary-activity-id (secretary-activity-by-name name)))))
;; (secretary-query-activity)

(setq secretary-activities
      (list (secretary-activity-create
	     :name "sleep"
	     :id "ac93c132-ab74-455f-a456-71d7b5ee88a6"
	     :cost-false-pos 3
	     :cost-false-neg 3
	     :query #'secretary-query-sleep)
	    (secretary-activity-create
	     :name "studying"
	     :id "24553859-2214-4fb0-bdc9-84e7f3d04b2b"
	     :cost-false-pos 8
	     :cost-false-neg 8)
	    ))

(defun secretary-random-p (&rest _args)
  "Return t or nil, at random."
  (declare (side-effect-free t))
  (> 0 (random)))

;;;###autoload
(defun secretary-query-mood (&optional prompt)
  (interactive)
  (let* ((mood-desc (completing-read
		     (or prompt "Your mood: ")
		     (cl-sort (mapcar #'car secretary-mood-alist)
			      #'secretary-random-p)))
         (old-score (cdr (assoc mood-desc secretary-mood-alist)))
         (prompt-for-score
          (concat "Score from 1 to 5"
                  (when old-score " (default " old-score ")")
                  ":"))
         (score (read-string prompt-for-score nil nil old-score))
         (score-num (string-to-number score))
         (now (ts-now)))
    (secretary-append "/home/kept/Self_data/mood.tsv"
		      (ts-format now)
		      "\t" (s-replace "," "." score)
		      "\t" mood-desc)
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
(defun secretary-query-weight ()
  (interactive)
  (let* ((path "/home/kept/Self_data/weight.tsv")
         (last-wt (secretary-last-value-in-tsv path))
         (wt (completing-read "What do you weigh today? "
			      `(,last-wt
				"I don't know")))
         (now (ts-now)))
    (if (= 0 (string-to-number wt))
        (secretary-emit "Ok, I'll ask you again later.")
      (secretary-append path
			(ts-format now)
			"\t" (s-replace "," "." wt))
      (secretary-emit "Weight today: " (secretary-last-value-in-tsv path) " kg"))
    (sit-for secretary-sit-short)))

(defun secretary-check-yesterday-sleep ()
  (let* ((today-rows (secretary-get-all-today-in-date-indexed-csv
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
  (secretary-check-yesterday-sleep)
  (let* ((now (or ts (ts-now)))
         (wakeup-time
          (if (secretary-prompt "Did you wake around now?")
              (ts-dec 'minute 10 now)
            (let ((reply (completing-read "When did you wake? "
                                          `("I don't know"
					    "Now"
					    ,(ts-format "%H:%M")))))
              (when (-non-nil (parse-time-string reply))
                (ts-parse reply)))))
         (sleep-minutes
	  (secretary-parse-time-amount
	   (completing-read "How long did you sleep? "
                            `("I don't know"
			      ,(number-to-string
				(/ secretary-length-of-last-idle 60 60)))))))

    (secretary-emit (when wakeup-time
		      (concat "You woke at " (ts-format "%H:%M" wakeup-time) ". "))
		    (when sleep-minutes
		      (concat "You slept " (number-to-string sleep-minutes)
			      " minutes (" (number-to-string (/ sleep-minutes 60.0))
			      " hours).")))
    (secretary-append "/home/kept/Self_data/sleep.tsv"
		      (ts-format now)
		      "\t" (ts-format "%F" secretary--date)
		      "\t" (when wakeup-time (ts-format "%T" wakeup-time))
		      "\t" (number-to-string sleep-minutes))))

;; WIP
(defun secretary-read (prompt collection default)
  (let* ((background-info (concat "Applying to date: "
				  (ts-format "%Y-%B-%d" secretary--date) "\n"
				  secretary--last-msg "\n"
				  ))
	 (extra-collection '("Skip to presentations"
			     "Don't disturb me"))
	 (prompt2 (concat (ts-format "[%H:%M] ") prompt))
	 (result (completing-read (concat background-info
					  prompt2
					  (when default
					    " (default " default "): "))
				  (append collection extra-collection)
				  nil nil nil nil
				  default)))
    (setq secretary--last-msg prompt2)
    (secretary-emit prompt) ;; TODO: use prompt2
    (if (string-match-p "skip" result)
	(progn
	  ;; (minibuffer-keyboard-quit)
	  (secretary-present-diary)
	  (keyboard-quit))
      result)))

(defun secretary--visual-chime ()
  (let ((orig-fringe-bg (face-background 'fringe)))
    (set-face-attribute 'fringe nil :background "green")
    (run-with-timer .2 nil (lambda () (set-face-attribute 'fringe nil :background "#aca")))
    (run-with-timer .3 nil (lambda () (set-face-attribute 'fringe nil :background "#7a7")))
    (run-with-timer .4 nil (lambda () (set-face-attribute 'fringe nil :background "#696")))
    (run-with-timer .5 nil (lambda () (set-face-attribute 'fringe nil :background "#363")))
    (run-with-timer .6 nil (lambda () (set-face-attribute 'fringe nil :background orig-fringe-bg)))))

;; (secretary--chime-visual)
;; (secretary--chime-aural)

;; (set-face-attribute 'fringe nil :foreground "grey")
;; (secretary-read "question" nil nil)

(defun secretary-query-meditation (&optional date)
  (interactive)
  (when (secretary-prompt "Did you meditate today?")
    (let ((x (read-string "Do you know how long (in minutes)? ")))
      (secretary-append "/home/kept/Self_data/meditation.tsv"
			(ts-format date)
			"\t" "TRUE"
			"\t" x))))

(defun secretary-query-cold-shower (&optional date)
  (interactive)
  (let ((x (read-string "Cold rating? "))
	(path "/home/kept/Self_data/cold.tsv"))
    (secretary-append path
		      (ts-format date)
		      "\t" x)))


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

(defun secretary-special-handle-current-query ()
  (interactive)
  (let ((input (completing-read "Yes? " '(
					  "Never mind, let me reply normally."
					  "Remind me about this one later."
					  "Skip this."
					  "Back to previous query."
					  "Yes."
					  "No."
					  "Apply this query to a different date."
					  "Apply this query to yesterday."
					  "Tell me something profound."))))
    (cond ((string-match-p (rx (or "remind" "later" "l8r" "resched")) input)
           'reschedule)
          ((string-match-p (rx (or "food" "ingred")) input)
           (secretary-query-ingredients))
          ((string-match-p (rx (or "profound")) input)
           (secretary-emit (seq-random-elt secretary-aphorisms)))
          ((string-match-p (rx (or "yesterday" "yday")) input)
           (secretary-change-date-yesterday))
          ((string-match-p (rx "date") input)
           (secretary-change-date))
          ((string-match-p (rx (or "nvm" "never mind" (seq bow "nm" eow))) input)
           nil)
          ((string-match-p (rx (or "exit" "quit" "ttyl" "bye")) input)
           (keyboard-quit))
          (t
           (secretary-emit "Override not understood.")))))


;;;; Welcomers

;; TODO: Also call presenters &c.
;; Call all queries, new version.
(defun secretary--call2 ()
  (interactive)
  (setq secretary--date (ts-now))
  (secretary-with-frame
   (dolist (x secretary--active-queries)
     (secretary-do-query-maybe x secretary--date (interactive-p)))))

(defun secretary-call (&optional arg)
  "Call your secretary. Useful when you're unsure what to do."
  (interactive "P")
  (secretary-emit "Hello!")
  (sit-for secretary-sit-short)
  (secretary-check-neglect)
  (secretary-welcome arg))

(defun secretary-call-from-idle ()
  "Called by idleness-related hooks, doesn't do much if idle was not long."
  (unwind-protect
      (when (> secretary-length-of-last-idle secretary-long-idle-threshold)
        (unless (frame-focus-state)
          (notifications-notify :title secretary-ai-name :body (secretary-greeting)))
	(secretary-play-chime)
        (secretary-check-neglect)
	;; check neglect overlaps with the stuff asked in the following prompt.
        (when (secretary-prompt (secretary-greeting) " Do you have time for some questions?")
          (secretary-welcome t)))
    (setq secretary-length-of-last-idle 0)))
;; (run-with-timer 3 nil (lambda ()  (print (frame-focus-state))))
;; (secretary-call-from-idle)

(defun secretary-call-from-reschedule ()
  (secretary-play-chime)
  (secretary-emit "Hello, " secretary-user-short-title ". ")
  (sit-for secretary-sit-medium)
  (when (secretary-prompt "1 hour ago, you asked to be reminded. Is now a good time?")
    (secretary-check-neglect)
    (secretary-welcome)))
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
  (and (-all-p #'null (-map #'secretary-logged-today (-map #'car secretary-tsv-alist)))
       (secretary-prompt "Shall I come back in an hour?")
       (run-with-timer 3600 nil #'secretary-call-from-idle)))

(defun secretary-check-neglect ()
  (interactive)
  (dolist (cell secretary-tsv-alist)
    (when (file-exists-p (car cell))
      (let* ((path (car cell))
             (d (ts-parse (secretary-last-date-string-in-date-indexed-csv path)))
             (diff-days (round (/ (ts-diff (ts-now) d) 60 60 24))))
	(and (< 3 diff-days)
	     ;; TODO: ask about quitting to log this thing
	     (secretary-prompt "It's been " (number-to-string diff-days)
			       " days since you logged " (file-name-base path)
			       ". Do you want to log it now?")
	     (call-interactively (cadr cell)))))))


;;;; Presenters

(defvar secretary-plot-hook nil
  "Hook called to print plots. A convenient place to add your
custom plots.")

;;;###autoload
(defun secretary-report-mood ()
  (interactive)
  (view-file "/home/kept/Self_data/mood.tsv")
  (auto-revert-mode))

;;;###autoload
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
(defun secretary-plot-weight ()
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
(defun secretary-present-plots ()
  (interactive)
  (unless (null secretary-plot-hook)
    (switch-to-buffer (secretary-buffer-chat))
    (secretary-emit (seq-random-elt '("I have plotted the latest intel, boss."
				      "Here are some projections!"
				      "Data is beautiful, don't you think?")))
    (run-hooks 'secretary-plot-hook)))

(defvar secretary-past-sample-function #'secretary-past-sample-default)

(defun secretary-past-sample-default (&optional ts)
  "Return a list of ts.el objects referring to yesterday, this
weekday the last 4 weeks, this day of the month the last 12
months, and this date from all past years."
  (let ((now (or ts (ts-now))))
    (-uniq (append
            (--iterate (ts-dec 'day 1 it) now 1)
            (--iterate (ts-dec 'woy 1 it) now 4)
            (--iterate (ts-dec 'month 1 it) now 12)
            (--iterate (ts-dec 'year 1 it) now 99)))))

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
;;      (--iterate (ts-dec 'month 1 it) (ts-now) 40))

(defun secretary-existing-diary (&optional date dir file-format)
  "Return the first file in DIR matching FILE-FORMAT.
FILE-FORMAT is handled by `parse-time-string'. The value returned
is a full filesystem path or nil.

When DATE is nil, use today.
When DIR is nil, use `org-journal-dir'.
When FILE-FORMAT is nil, use `org-journal-file-format'."
  (let* ((dir (or dir org-journal-dir))
	 (file-format (or file-format (and (boundp 'org-journal-file-type)
					   (eq org-journal-file-type 'daily)
					   org-journal-file-format)))
	 (file (--find (string-match-p
			(ts-format file-format date) it)
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
;; TODO: show also the agenda for each date if not empty
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


;;;; Handle idling & reboots & crashes

(defvar secretary--timer nil)

(defvar secretary--idle-beginning (ts-now))

(defvar secretary-length-of-last-idle 0
  "Duration in seconds.")

(defcustom secretary-idle-threshold (* 10 60)
  "Duration in seconds, above which the user is considered idle."
  :group 'secretary
  :type 'number)

(defcustom secretary-long-idle-threshold (* 90 60)
  "Be idle at least this long to be greeted upon return."
  :group 'secretary
  :type 'number)

(defcustom secretary-return-from-idle-hook nil
  "Note: An Emacs startup also counts as a return from idleness.
You'll probably want your hook to be conditional on some value of
`secretary-length-of-last-idle', which at startup is calculated from
the last Emacs shutdown or crash (technically, last time
`secretary-mode' was running)."
  :group 'secretary
  :type '(restricted-sexp :match-alternatives #'listp))

(defcustom secretary-periodic-not-idle-hook nil
  "Hook run every minute when the user is not idle."
  :group 'secretary
  :type '(restricted-sexp :match-alternatives #'listp))

(defun secretary--start-next-timer (&optional assume-idle)
  "Start one or the other timer depending on idleness. If
ASSUME-IDLE is non-nil, skip the idle check and associated
overhead: useful if the caller has already checked it."
  (if (or assume-idle (secretary-idle-p))
      (named-timer-run :secretary 2 nil #'secretary--user-is-idle t)
    (named-timer-run :secretary 60 nil #'secretary--user-is-active)))

(defun secretary--user-is-active ()
  "This function is meant to be called by `secretary--start-next-timer'
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
  "This function is meant to be called by `secretary--start-next-timer'
repeatedly for as long as the user is idle.

When the user comes back, this function will be called one last
time, at which point the idleness condition will fail and it sets
`secretary-length-of-last-idle' and runs
`secretary-return-from-idle-hook'.  That it has to run exactly
once with a failing condition that normally succeeds, as opposed
to running zero or infinity times, is the reason it has to be a
separate function from `secretary--user-is-active'."
  (if (secretary-idle-p)
      (secretary--start-next-timer 'assume-idle)
    ;; Take the idle threshold into account and correct the idle begin point.
    (when decrement
      (ts-decf (ts-sec secretary--idle-beginning) secretary-idle-threshold))
    (setq secretary-length-of-last-idle (ts-diff (ts-now) secretary--idle-beginning))
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
  (when (file-exists-p "/tmp/secretary/pid")
    (let ((pid (string-to-number (f-read-bytes "/tmp/secretary/pid"))))
      (and (/= pid (emacs-pid))
	   (member pid (list-system-processes))))))

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

(defun secretary-unload-function ()
  "For `unload-feature' (is this unnecessary?)."
  (secretary-mode 0))

;;;###autoload
(define-minor-mode secretary-mode
  nil
  :global t
  (if secretary-mode
      (when (and
	     (cond ((eq system-type 'darwin)
		    (fset #'secretary--idle-seconds #'org-mac-idle-seconds))
                   ((and (eq window-system 'x)
                         (executable-find secretary--x11idle-program-name))
		    (fset #'secretary--idle-seconds #'secretary--x11-idle-seconds))
                   (t
		    (secretary-mode 0)
		    (message secretary-ai-name ": Not able to detect idleness, "
			     "I'll be useless. Disabling secretary-mode.")
		    nil))
	     (if (secretary--another-secretary-running-p)
		 (prog1 nil
		   (message "Another secretary active.")
		   (secretary-mode 0))
	       t))
	(mkdir "/tmp/secretary/" t)
	(f-write (number-to-string (emacs-pid))
	   'utf-8 "/tmp/secretary/pid")
        (add-hook 'secretary-return-from-idle-hook #'secretary-log-idle -90)
        (add-hook 'secretary-return-from-idle-hook #'secretary-call-from-idle 90)
	(add-hook 'secretary-periodic-not-idle-hook #'secretary--save-variables-to-disk)
	(add-hook 'secretary-periodic-not-idle-hook #'secretary--save-buffer-logs-to-disk)
        (add-hook 'window-buffer-change-functions #'secretary-log-buffer)
        (add-hook 'window-selection-change-functions #'secretary-log-buffer)
	(add-hook 'after-init-hook #'secretary--restore-variables-from-disk -1)
        (add-hook 'after-init-hook #'secretary--start-next-timer 91)
	(named-timer-run :secretary-keepalive 300 300 #'secretary--keepalive)
        ;; (add-function :after #'after-focus-change-function #'secretary-log-buffer)
        (when after-init-time
          (progn
            (when (-any #'null '(secretary-idle-beginning secretary-mood-alist))
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
