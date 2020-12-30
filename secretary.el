;;; secretary.el -*- lexical-binding: t; -*-

;; Author: Martin Edström <meedstrom@teknik.io>
;; URL: https://github.com/meedstrom/secretary
;; Version: 0.1
;; Created: 2020-12-03
;; Keywords: outlines convenience
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
(setc org-clock-x11idle-program-name "xprintidle")
(setc scr-user-name "Martin")
(setc scr-user-short-title "sir")
(setc scr-user-birthday "1991-12-07")
(setc scr-ai-name "Maya")
(add-hook 'scr-plot-hook #'scr-plot-mood 50)
(add-hook 'scr-plot-hook #'scr-plot-weight)

;; (require 'org-id)
;; (org-id-update-id-locations '("/home/kept/Emacs/secretary/test.org"))

(setc scr-activities-alist
      '(;; id            cost of misprediction (either false positive or false negative)
        ("7441f4e2-7251-47d8-ab06-1c2c205d1ae0"    0)  ;; unknown
        ("c1b1fdb8-ea87-4d5a-b01d-4214266c4f4b"    1)  ;; unknown (afk)
        ("24553859-2214-4fb0-bdc9-84e7f3d04b2b"    8)  ;; studying
        ("a0fdbb69-9cdb-418f-b5e8-37601af43c0d"    6)  ;; coding
        ("784d67a5-c15b-4c09-8c74-97b5767d70e6"    2)  ;; downtime
        ("7aaf9105-d58d-4e83-9d34-045a3c922ac5"   20)  ;; meditating
        ("ac93c132-ab74-455f-a456-71d7b5ee88a6"    3)  ;; sleep
        ))

(setc scr-activities-alist
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

(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'find-func)
(require 'ts)
(require 'f)
(require 's)
(require 'dash)
(require 'parse-csv)
(autoload #'scr-log-buffer "org-id")
(autoload #'org-mac-idle-seconds "org-clock")
(autoload #'org-x11-idle-seconds "org-clock")
(autoload #'org-clock-in "org-clock")
(autoload #'org-id-uuid "org-id")
(autoload #'org-id-goto "org-id")
(autoload #'notifications-notify "notifications")

(defcustom scr-location-diary-discrete "/home/kept/Diary/"
  "The location of your discrete diary files, with names such as
\"201228.org\".  Used by `scr-present-diary'.")

;; TODO: rename to main-datetree
(defcustom scr-location-diary-datetree "/home/kept/Journal/diary2.org"
  "The name of your main datetree, if you have one that you use
as a big archive file, see Info node `(org) Moving subtrees'.
Used by `scr-present-diary'.  ")

(defcustom scr-ai-name "Lex"
  nil)

(defcustom scr-user-birthday nil
  nil)

(defcustom scr-user-name (if (s-equals? user-full-name "")
                             "Mr. Bond"
                           (-first-item (s-split " " user-full-name)))
  nil)

(defcustom scr-user-short-title "sir"
  nil)

;; TODO: deprecate either this or the -file-name vars
(defcustom scr-dir (expand-file-name "secretary" user-emacs-directory)
  "Directory under which files should sit.")

(defcustom scr-idle-beginning-file-name (expand-file-name "idle-beginning" scr-dir)
  nil)

(defcustom scr-mood-alist-file-name (expand-file-name "scr-mood-alist" scr-dir)
  nil)

(defcustom scr-sit-long 1
  "A duration in seconds to pause for effect after certain kinds
of messages. See also `scr-sit-medium' and `scr-sit-short'.")

(defcustom scr-sit-medium .8
  "A duration in seconds to pause for effect after certain kinds
of messages. See also `scr-sit-long' and `scr-sit-short'.")

(defcustom scr-sit-short .5
  "A duration in seconds to pause for effect after certain kinds
of messages. See also `scr-sit-long' and `scr-sit-medium'.")

;; TODO: deprecate
(defcustom scr-activities-alist
  '(;; id            cost of misprediction (either false positive or false negative)
    ("some-org-id"    8)  ;; study
    ("some-org-id"    6)  ;; coding
    ("some-org-id"    2)  ;; downtime
    ("some-org-id"   20)  ;; meditating
    ("some-org-id"    3)  ;; sleep
    ("some-org-id"    1)  ;; unknown (afk)
    ("some-org-id"    0)  ;; unknown (must be 0 as the default guess)
    )
  nil)

;; REVIEW: see that there's no problem if you delete scr-dir
(defvar scr-mood-alist nil
  "Alist for suggesting a mood score in the `scr-log-mood'
prompt. Merely a convenience for auto-completion; the scores are
not forced.

The variable populates itself through use, and syncs with a file
at `scr-mood-alist-file-name'.")

;; (defvar scr-mood-alist '(("bemused" . "3")
;; 			 ("amused" . "5")
;; 			 ("great"  . "5")
;; 			 ("annoyed" . "3")
;; 			 ("depressed" . "1")))

(defvar scr-tsv-alist '(("/home/kept/Self_data/weight.tsv" scr-query-weight)
			("/home/kept/Self_data/mood.tsv" scr-query-mood)
			("/home/kept/Self_data/ingredients.tsv" scr-query-ingredients)
			("/home/kept/Self_data/sleep.tsv" scr-query-sleep)
			("/home/kept/Self_data/meditation.tsv" scr-query-meditation)
			("/home/kept/Self_data/cold.tsv" scr-query-cold)))

(defvar scr-log-alist '(("/home/kept/Self_data/buffers.tsv" scr-log-buffer)
			("/home/kept/Self_data/buffer_focus.tsv" scr-log-buffer)
			("/home/kept/Self_data/idle.tsv" scr-log-idle)))


(defvar scr-plot-hook nil
  "Hook called to print plots. A convenient place to add your
custom plots.")

(defvar scr--date (ts-now)
  "Can be set anytime during a welcome to override the date to
which some queries apply, for example to log something for
yesterday. This is not universal (yet), so check the source for
the welcomer you are using.")


(defvar scr-chime-sound-file
  (expand-file-name
   ;; From https://freesound.org/people/josepharaoh99/sounds/380482/
   "assets/Chime Notification-380482.wav"
   ;; From https://bigsoundbank.com/detail-0319-knock-on-a-glass-door-1.html
   ;; "assets/DOORKnck_Knock on a glass door 1 (ID 0319)_BSB.wav"
   (f-dirname (find-library-name "secretary")))
  "Sound to play when a welcomer is triggered unannounced.")

(defvar scr-greetings '("Welcome back, Master."
                       (concat "Nice to see you again, " scr-user-name ".")
                       (concat "Greetings, " scr-user-name "."))
  "Greetings which can work as first sentence in a longer message.")

;; What's cl-defstruct? https://nullprogram.com/blog/2018/02/14/
(cl-defstruct (scr-activity (:constructor scr-activity-create)
			    (:copier nil))
  name id cost-false-pos cost-false-neg querier)

(defun scr-activity-by-name (name)
  (declare (side-effect-free t))
  (--find (equal name (scr-activity-name it)) scr-activities))

(defun scr-play-chime ()
  (and (executable-find "aplay")
       (file-exists-p scr-chime-sound-file)
       (start-process "aplay" nil "aplay" scr-chime-sound-file)))


;; TODO: use length-of-last-idle to check it's not that i just restarted emacs
(defun scr--another-secretary-running-p ()
  (when (file-exists-p "/tmp/secretary/running")
    (let ((age (- (time-convert (current-time) 'integer)
		  (time-convert (file-attribute-modification-time
				 (file-attributes "/tmp/secretary/running"))
				'integer))))
      (> age (* 10 60)))))

(defun scr--mark-territory ()
  (mkdir "/tmp/secretary/" t)
  (f-touch "/tmp/secretary/running"))

;; TODO: deprecate
(defun scr-activities-names ()
  (->> scr-activities-alist
       (-map (lambda (x) (save-window-excursion
                      (org-id-goto (car x))
                      (-last-item (org--get-outline-path-1)))))))

;; TODO: Show when the user types a noncommittal "k" for "okay". User should
;; have room to express shades of feeling, even if we don't do anything with
;; it.
(defun scr-prompt (&rest strings)
  (let* (;; (default-y-or-n-p-map y-or-n-p-map)
         ;; (default-cmd (lookup-key y-or-n-p-map (kbd "k")))
         (prompt (string-join strings)))
    (unwind-protect
        (progn
          (switch-to-buffer (scr-buffer-chat))
          (unless (< 20 (car (window-fringes)))
            (set-window-fringes nil 20 20))
          (goto-char (point-max))
          (scr-emit prompt)
          (define-key y-or-n-p-map (kbd "o") #'scr-special-handle-current-query)
          (define-key y-or-n-p-map (kbd "i") #'scr-special-handle-current-query)
          (define-key y-or-n-p-map (kbd "k") #'y-or-n-p-insert-y)
          (if (y-or-n-p prompt)
              (progn
                (insert " y" "\n")
                t)
            (insert " n" "\n")
            nil))
      (dolist (x '("o" "i" "k"))
        (define-key y-or-n-p-map (kbd x) #'y-or-n-p-insert-other)))))
;; (scr-prompt "Test")
;; (y-or-n-p "test")

(defun scr--idle-seconds ()
  "Stub to be redefined."
  (warn "Code ended up in an impossible place."))

(defvar scr--last-edited (ts-now)
  "Timestamp updated whenever `scr-emit' runs.")

(defun scr-emit (&rest strings)
  (scr-print-new-date-maybe)
  (setq scr--last-edited (ts-now))
  (prog1 (message (string-join strings))
    (with-current-buffer (scr-buffer-chat)
      (goto-char (point-max))
      (insert "\n<" (ts-format "%H:%M") "> " (string-join strings)))))

(defun scr-print-new-date-maybe ()
  (when (/= (ts-day (ts-now))
            (ts-day scr--last-edited))
    (with-current-buffer (scr-buffer-chat)
      (goto-char (point-max))
      (insert "\n\n" (ts-format "%Y, %B %d") (scr--holiday-maybe)))))
;; (scr-print-new-date-maybe)

(defun scr--holiday-maybe ()
  (require 'calendar)
  (require 'holidays)
  (if-let (foo (calendar-check-holidays (calendar-current-date)))
      (concat " -- " foo)
    ""))

(defun scr--buffer-r ()
  (get-buffer-create (concat "*" scr-ai-name ": R*")))

(defun scr-reschedule ()
    (run-with-timer 3600 nil #'scr-call-from-reschedule))

;; (defmacro scr-with-file (path &rest body)
;;   (declare (pure t) (indent defun))
;;   `(with-temp-buffer
;;      (insert-file-contents-literally ,path)
;;      ,@body))

;; REVIEW
(defun scr-last-date-string-in-date-indexed-csv (path)
  (declare (side-effect-free t))
  (with-temp-buffer
    (insert-file-contents-literally path)
    (goto-char (point-max))
    (re-search-backward (rx bol (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)))
    (buffer-substring (point) (+ 10 (point)))))
;; (scr-last-date-string-in-date-indexed-csv "/home/kept/Self_data/weight.tsv")
;; (scr-last-date-string-in-date-indexed-csv "/home/kept/Self_data/mood.tsv")
;; (scr-last-date-string-in-file "/home/kept/Self_data/mood.tsv")

(defun scr-last-date-string-in-file (path)
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
(defun scr-get-all-today-in-date-indexed-csv (path &optional ts)
  (with-temp-buffer
    (insert-file-contents-literally path)
    (let (x)
      (while (search-forward (ts-format "%F" ts) nil t)
        (push (parse-csv->list (buffer-substring (line-beginning-position)
                                                 (line-end-position)))
              x))
      x)))
;; (scr-get-all-today-in-date-indexed-csv "/home/kept/Self_data/sleep.csv" (ts-dec 'day 1 (ts-now)))

;; (defun scr-update-or-append-in-date-indexed-csv (path &optional ts)
;;   (scr-get-first-today-in-date-indexed-csv path ts))

;; REVIEW
(defun scr-get-first-today-in-date-indexed-csv (path &optional ts)
  (with-temp-buffer
    (insert-file-contents path)
    (search-forward (ts-format "%F" ts))
    (line-beginning-position)
    (buffer-substring (line-beginning-position) (line-end-position))))
;; (scr-get-first-today-in-date-indexed-csv "/home/kept/Self_data/ingredients.csv")

(defun scr-last-value-in-tsv (path)
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents-literally path)
      (goto-char (point-max))
      (search-backward "\t")
      (forward-char)
      (buffer-substring (point) (line-end-position)))))

;; WONTFIX: check for recent activity (if user awake thru the night)
(defun scr-logged-today (file)
  (declare (side-effect-free t))
  (when (file-exists-p file)
    ;; don't act like it's a new day if the time is <5am.
    (let ((day (if (> 5 (ts-hour (ts-now)))
                   (ts-dec 'day 1 (ts-now))
                 (ts-now))))
      (with-temp-buffer
        (insert-file-contents-literally file)
        (ignore-errors (search-forward (ts-format "%F" day)))))))
;; (scr-logged-today "/home/kept/Self_data/weight.tsv")
;; (scr-logged-today "/home/kept/Self_data/buffers.tsv")

(defun scr-existing-diary (dir date)
  (declare (side-effect-free t))
  (let ((foo (car (--filter (string-match-p
                             (concat (ts-format "%y%m%d" date) ".*org$")
                             it)
                            (directory-files dir)))))
    (unless (null foo)
      (expand-file-name foo dir))))
;; (scr-existing-diary "/home/kept/Diary" (ts-dec 'month 1 (ts-now)))

(defun scr-buffer-mode (buffer-or-name)
  "Retrieve the `major-mode' of BUFFER-OR-NAME."
  (declare (side-effect-free t))
  (buffer-local-value 'major-mode (get-buffer buffer-or-name)))

(defun scr-idle-p ()
  (declare (side-effect-free t))
  (> (scr--idle-seconds) scr-idle-threshold))

(defun scr--x11-idle-seconds ()
  "Like `org-x11-idle-seconds' but doesn't need a /bin/sh, nor to
load org."
  (declare (side-effect-free t))
  (/ (scr--process-output-to-number org-clock-x11idle-program-name) 1000))

(defmacro scr--process-output-to-string (program &rest args)
  "Similar to `shell-command-to-string', but skips the shell
intermediary so you don't need a /bin/sh. PROGRAM and ARGS are
passed on to `call-process'."
  (declare (debug (&rest form))
	   (indent nil)
	   (side-effect-free t))
  `(with-temp-buffer
     (call-process ,program nil (current-buffer) nil ,@args)
     (buffer-string)))

(defmacro scr--process-output-to-number (program &rest args)
  "Pipe `scr--process-output-to-string' through `string-to-number'."
  (declare (debug (&rest form))
	   (indent nil)
	   (side-effect-free t))
  `(string-to-number (scr--process-output-to-string ,program ,@args)))

;; - Needs to be a function because something might kill the buffer.
;; - Can't use get-buffer-create because I want to turn on `visual-line-mode'.
(defun scr-buffer-chat ()
  (if-let* ((name (concat "*" scr-ai-name ": Chat log*"))
            (b (get-buffer name)))
      b
    (with-current-buffer (generate-new-buffer name)
      (visual-line-mode)
      (current-buffer))))

(defun scr-append (path &rest text)
  "Append TEXT to the file located at PATH, creating it and its
parent directories if it doesn't exist, and making sure the text
begins on a newline."
  (declare (indent defun))
  (unless (file-exists-p path)
    (make-empty-file path t)
    (start-process "chattr" nil "chattr" "+A" path))
  (let ((newline-maybe (if (s-ends-with-p "\n" (f-read-bytes path))
                           ""
                         "\n")))
    (f-append (concat newline-maybe (string-join text)) 'utf-8 path)))

;; REVIEW
(defun scr-change-date (&optional ts)
  (require 'org)
  (let ((target (if ts
		    ts
		  (ts-parse (org-read-date)))))
    (setq scr--date target)))

(defun scr-change-date-yesterday ()
  (message "Applying this query AND subsequent queries for now to yesterday.")
  (setq scr--date (ts-dec 'day 1 (ts-now))))

(defvar scr-aphorisms
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
    "If it happens once it's a mistake. If it happens twice, it's a choice."
    ))

;;;; Greetings

(defun scr-greeting-curt ()
  "Return a random greeting string appropriate in the midst of a
workday. If you've already exchanged good mornings, it's weird to
do so again."
  (seq-random-elt `("Hello" "Hi" "Hey")))

(defun scr-greeting ()
  "Return a random greeting string."
  ;; If it's morning, always use a variant of "good morning"
  (if (> 10 (ts-hour (ts-now)) 5)
      (seq-random-elt (scr--daytime-appropriate-greetings))
    (eval (seq-random-elt (append scr-greetings
                                (-list (scr--daytime-appropriate-greetings)))))))
;; (scr-greeting)

;; NOTE: I considered making external variables for morning, day and evening
;; lists, but users might also want to change the daytime boundaries or even add
;; new boundaries. Too many possibilities, this is a case where it's ok to make
;; the user override the defun as a primary means of customization.
(defun scr--daytime-appropriate-greetings ()
  (cond ((> 5 (ts-hour (ts-now)))
         (list "You're up late, Master."
               "Burning the midnight oil?"))
        ((> 10 (ts-hour (ts-now)))
         (list (concat "Good morning, " scr-user-name ".")
               "Good morning!"
               "The stars shone upon us last night."))
        ((> 16 (ts-hour (ts-now)))
         (list "Good day!"
               ))
        (t
         (list "Good evening!"
               "Pleasant evening to you!"))))

(defun scr-greeting-standalone ()
  "Return a greeting that expects to be followed by nothing: no
prompts, no debug message, no info. Suitable for
`notifications-notify' or `startup-echo-area-message'. A superset
of `scr-greeting'."
  (eval (seq-random-elt
         (append scr-greetings
                 (-list (scr--daytime-appropriate-greetings))
                 '("How may I help?")))))

;;;; Collect data

(defvar scr-last-buffer nil)

(defvar scr-known-buffers nil)

(defvar scr-buffer-focus-log nil)

(defun scr-log-idle ()
  (scr-append "/home/kept/Self_data/idle.tsv"
             (ts-format)
             "\t" (number-to-string (/ (round scr-length-of-last-idle) 60))))

;; TODO: Batch save. Instead of appending to a file line by line, append to a
;; buffer and save it once every 5 minutes or so.
;; TODO: When buffer mode changes, count it as a new buffer. Note that (assoc
;; buf scr-known-buffers) will still work.
(defun scr-log-buffer (&optional _arg)
  (unless (minibufferp)
    (let* ((buf (current-buffer))
           (mode (symbol-name (scr-buffer-mode buf)))
           (known (assoc buf scr-known-buffers))
           (timestamp (number-to-string (ts-unix (ts-now))))
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
      (unless (eq scr-last-buffer buf) ;; happens if you only entered and left minibuffer
        (setq scr-last-buffer buf)
        (unless known
          (push buffer-record scr-known-buffers)
	  (scr-append "/home/kept/Self_data/buffers.tsv"
		     (string-join (cdr buffer-record) "\t")))
        (push focus-record scr-buffer-focus-log)
        (scr-append "/home/kept/Self_data/buffer-focus.tsv"
                   (string-join focus-record "\t"))))))

;; (defun scr-known-buffers-buffer ()
;;   (or (get-file-buffer "/home/kept/Self_data/buffers.csv")
;;       (create-file-buffer "/home/kept/Self_data/buffers.csv")))

;; (defun scr-buffer-focus-log-buffer ()
;;   (or (get-file-buffer "/home/kept/Self_data/buffer-focus.csv")
;;       (create-file-buffer "/home/kept/Self_data/buffer-focus.csv")))



;; (setc completion-auto-help t)
;; (completing-read (concat "Score from 1 to 5: [" "]") nil nil nil nil nil (cdr (assoc "fine" scr-mood-alist)))
;; (read-string (concat "Score from 1 to 5: [" "]")  nil nil (cdr (assoc "fine" scr-mood-alist)))

;; TODO: make the dataset append-only
;;;###autoload
(defun scr-query-ingredients ()
  (interactive)
  (let* ((response (read-string "Comma-separated list of ingredients: "))
         (formatted-response (->> response
                                  (s-split (rx (+ (any "," blank))))
                                  (s-join ", ")
                                  (s-replace "\"" "'")))
         (path "/home/kept/Self_data/ingredients.tsv"))
    (scr-append path
	       (ts-format "%F")
	       "\t" "\"" formatted-response "\"")
    (scr-emit "Recorded so far today: "
             (s-replace "^.*?," ""
			(scr-get-first-today-in-date-indexed-csv path)))))

;; (defalias #'scr-query-food #'scr-query-ingredients)

(defun scr--sanity-check-csv ()
  ;; look for overlapping time intervals
  (pcsv-parse-file "/home/kept/Self_data/idle.csv")

  (pcsv-parse-file "/home/kept/Self_data/idle.csv")

  )

;;;###autoload
(defun scr-query-activity ()
  (interactive)
  (let* ((name (completing-read "What are you up to? " (scr-activities-names)))
	 (now (ts-now)))
    (scr-append "/home/kept/Self_data/activity.tsv"
               (ts-format now)
               "\t" name
               "\t" (scr-activity-id (scr-activity-by-name name)))))
;; (scr-query-activity)

(setq scr-activities
      (list (scr-activity-create
	     :name "sleep"
	     :id "ac93c132-ab74-455f-a456-71d7b5ee88a6"
	     :cost-false-pos 3
	     :cost-false-neg 3
	     :querier #'scr-query-sleep)
	    (scr-activity-create
	     :name "studying"
	     :id "24553859-2214-4fb0-bdc9-84e7f3d04b2b"
	     :cost-false-pos 8
	     :cost-false-neg 8)
	    ))

;;;###autoload
(defun scr-query-mood (&optional prompt)
  (interactive)
  (let* ((mood-desc (completing-read (or prompt "Your mood: ")
				     (--sort (> 0 (random))
					     (mapcar #'car scr-mood-alist))))
         (old-score (cdr (assoc mood-desc scr-mood-alist)))
         (prompt-for-score
          (concat "Score from 1 to 5"
                  (when old-score " (default " old-score ")")
                  ":"))
         (score (read-string prompt-for-score nil nil old-score))
         (score-num (string-to-number score))
         (now (ts-now)))
    (scr-append "/home/kept/Self_data/mood.tsv"
               (ts-format now)
               "\t" (s-replace "," "." score)
               "\t" mood-desc)
    ;; Update scr-mood-alist.
    (if (assoc mood-desc scr-mood-alist)
        (setq scr-mood-alist
              (--replace-where (string= (car it) mood-desc)
                               (cons (car it) score)
                               scr-mood-alist))
      (push (cons mood-desc score) scr-mood-alist))
    ;; Return the mood score, which can be useful for the caller of this
    ;; function. If the input was not a number, like "idk" or an empty string,
    ;; return 3 to be neutral.
    (if (= 0 score-num)
        3
      score-num)))

;;;###autoload
(defun scr-query-weight ()
  (interactive)
  (let* ((path "/home/kept/Self_data/weight.tsv")
         (last-wt (scr-last-value-in-tsv path))
         (wt (completing-read "What do you weigh today? "
			      `(,last-wt
				"I don't know")))
         (now (ts-now)))
    (if (= 0 (string-to-number wt))
        (scr-emit "Ok, I'll ask you again later.")
      (scr-append path
                 (ts-format now)
                 "\t" (s-replace "," "." wt))
      (scr-emit "Weight today: " (scr-last-value-in-tsv path) " kg"))
    (sit-for scr-sit-short)))

;; (defun scr-query-weight ()
;;   (interactive)
;;   (with-temp-buffer
;;     (let* ((require-final-newline nil)
;;            (last-wt (progn (insert-file-contents "/home/kept/Self_data/weight.csv")
;;                            (goto-char (point-max))
;;                            (search-backward ",")
;;                            (forward-char)
;;                            (buffer-substring (point) (line-end-position))))
;;            (wt (completing-read "What do you weigh today? " nil nil nil last-wt))
;;            (newline-maybe (if (string= "\n" (buffer-substring (- (point-max) 1) (point-max)))
;;                               ""
;;                             "\n")))
;;       (if (= 0 (string-to-number wt))
;;           (scr-emit "Ok, I'll ask you later.")
;;         (f-append (concat newline-maybe (ts-format "%F") "," wt) 'utf-8 "/home/kept/Self_data/weight.csv")))))

(defun scr-check-yesterday-sleep ()
  (let* ((foo (scr-get-all-today-in-date-indexed-csv
	       "/home/kept/Self_data/sleep.tsv" (ts-dec 'day 1 (ts-now))))
         (total-yesterday (-sum (--map (string-to-number (nth 2 it)) foo))))
    (if (> (* 60 4) total-yesterday)
        (if (scr-prompt "Yesterday, you slept "
		       (number-to-string (/ total-yesterday 60.0))
                       " hours, is this about right?")
            nil
          (scr-emit "You may edit the history at "
		   "/home/kept/Self_data/sleep.tsv")
          (sit-for scr-sit-short)))))

;; TODO: Fix the case where someone wakes up at 23:00 but replies to the query
;;       at 01:00.
;;;###autoload
(defun scr-query-sleep ()
  "Query you for wake-up time and sleep quantity for one sleep block today.
You are free to decline either query, but you should not later
register sleep quantity from this same block in order to \"get
the totals correct\" -- the program will interpret it as a
different sleep block and continue to count the original one as
having an unknown nonzero quantity of sleep on top of what you
add."
  (interactive)
  (scr-check-yesterday-sleep)
  (let* ((now (ts-now))
         (wakeup-time
          (if (scr-prompt "Did you wake around now?")
              (ts-dec 'minute 10 now)
            (let ((reply (completing-read "When did you wake? "
                                          `("I don't know"
					    "Now"
					    ,(ts-format "%H:%M")))))
              (when (-non-nil (parse-time-string reply))
                (ts-parse reply)))))
         (sleep-minutes
	  (scr-parse-time-amount
	   (completing-read "How long did you sleep? "
                            `("I don't know"
			      ,(number-to-string
				(/ scr-length-of-last-idle 60 60)))))))

    (scr-emit (when wakeup-time
	       (concat "You woke at " (ts-format "%H:%M" wakeup-time) ". "))
             (when sleep-minutes
	       (concat "You slept " (number-to-string sleep-minutes)
		       " minutes (" (number-to-string (/ sleep-minutes 60.0))
		       " hours).")))
    (scr-append "/home/kept/Self_data/sleep.tsv"
               (ts-format now)
	       "\t" (ts-format "%F" scr--date)
               "\t" (when wakeup-time (ts-format "%T" wakeup-time))
               "\t" (number-to-string sleep-minutes))))

(defun scr-query-meditation (&optional date)
  (interactive)
  (when (scr-prompt "Did you meditate today?")
    (let ((x (read-string "Do you know how long (in minutes)? ")))
      (scr-append "/home/kept/Self_data/meditation.tsv"
		 (ts-format date)
		 "\t" "TRUE"
		 "\t" x))))

(defun scr-query-cold-shower (&optional date)
  (interactive)
  (let ((x (read-string "Cold rating? "))
	(path "/home/kept/Self_data/cold.tsv"))
    (scr-append path
               (ts-format date)
	       "\t" x)))


;;;; Parsers

;; TODO: Catch typos like 03 meaning 30 minutes, not 3 hours
(defun scr-parse-time-amount (input)
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
;; (scr-parse-time-amount "30")

;; (define-key minibuffer-local-completion-map (kbd "C-o") #'scr-special-handle-current-query)

(defun scr-special-handle-current-query ()
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
           (scr-query-ingredients))
          ((string-match-p (rx (or "profound")) input)
           (scr-emit (seq-random-elt scr-aphorisms)))
          ((string-match-p (rx (or "yesterday" "yday")) input)
           (scr-change-date-yesterday))
          ((string-match-p (rx "date") input)
           (scr-change-date))
          ((string-match-p (rx (or "nvm" "never mind" (seq bow "nm" eow))) input)
           nil)
          ((string-match-p (rx (or "exit" "quit" "ttyl" "bye")) input)
           (keyboard-quit))
          (t
           (scr-emit "Override not understood.")))))

;;;; Callers

(defun scr-call (&optional arg)
  "Call your secretary. Useful when you're unsure what to do."
  (interactive "P")
  (scr-emit "Hello!")
  (sit-for scr-sit-short)
  (scr-check-neglect)
  (scr-welcome arg))

(defun scr-call-from-idle ()
  "Called by idleness-related hooks, doesn't do much if idle was not long."
  (unwind-protect
      (when (> scr-length-of-last-idle scr-long-idle-threshold)
        (unless (frame-focus-state)
          (notifications-notify :title scr-ai-name :body (scr-greeting)))
	(scr-play-chime)
        (scr-check-neglect)
        (when (scr-prompt (scr-greeting) " Do you have time for some questions?")
          (scr-welcome t)))
    (setq scr-length-of-last-idle 0)))
;; (run-with-timer 3 nil (lambda ()  (print (frame-focus-state))))
;; (scr-call-from-idle)

(defun scr-call-from-reschedule ()
  (scr-play-chime)
  (scr-emit "Hello, " scr-user-short-title ". ")
  (sit-for scr-sit-medium)
  (when (scr-prompt "1 hour ago, you asked to be reminded. Is now a good time?")
    (scr-check-neglect)
    (scr-welcome)))
;;(scr-call-from-reschedule)

;; TODO: Show the results of changing date via `scr-special-handle-current-query'.
(defun scr-welcome (&optional just-idled-p)
  (setq scr--date (ts-now))
  (scr-followup)
  (and just-idled-p
       (scr-prompt "Have you slept?")
       (scr-query-sleep))
  (unless (scr-logged-today "/home/kept/Self_data/weight.tsv")
    (scr-query-weight))
  ;; (and (scr-prompt "Up to reflect?")
  ;;      (scr-prompt "Have you learned something?")
  ;;      (org-capture nil "s"))
  ;; (if (scr-prompt (concat "How about some flashcards?"))
  ;;     (org-drill))
  ;; (if (scr-prompt "Have you stretched today?")
  ;;     nil
  ;;   (if (scr-prompt "Do you want reminders for why?")
  ;;       nil
  ;;     nil))
  ;; (if (scr-prompt "Did you photographe your face today?")
  ;;     nil)
  ;; ;; (unless (scr-just-slept)
  ;; (unless (scr-logged-today "/home/kept/Self_data/meditation.csv")
  ;;   (scr-query-meditation scr--date))
  ;; (unless (scr-logged-today "/home/kept/Self_data/cold.csv")
  ;;   (when (scr-prompt "Have you had a cold shower yet?")
  ;;     (scr-query-cold-shower scr--date)))
  ;; (if (scr-prompt "Have you paid for anything since yesterday?")
  ;;     (org-capture nil "ln"))
  ;; (if (scr-prompt "Shall I remind you of your life goals? Don't be shy.")
  ;;     (view-file "/home/kept/Journal/gtd2.org"))
  (and (>= 1 (scr-query-mood "How are you? "))
       (scr-prompt "Do you need to talk?")
       (scr-prompt "I can direct you to my colleague Eliza, though "
                  "she's not too bright. Will that do?")
       (doctor))
  (scr-present-plots)
  ;; and (scr-prompt "Would you like me to suggest an activity?")
  (scr-present-diary (ts-now))
  (and (-all-p #'null (-map #'scr-logged-today (-map #'car scr-tsv-alist)))
       (scr-prompt "Shall I come back in an hour?")
       (run-with-timer 3600 nil #'scr-call-from-idle)))

(defun scr-check-neglect ()
  (interactive)
  (dolist (cell scr-tsv-alist)
    (when (file-exists-p (car cell))
      (let* ((path (car cell))
             (d (ts-parse (scr-last-date-string-in-date-indexed-csv path)))
             (diff-days (/ (ts-diff (ts-now) d) 60 60 24)))
	(and (< 3 diff-days)
	     (scr-prompt "It's been " (number-to-string diff-days)
			 " days since you logged " (file-name-base path)
			 ". Do you want to log it now?")
	     (call-interactively (cadr cell)))))))

;;;; Presenters

;;;###autoload
(defun scr-report-mood ()
  (interactive)
  (view-file "/home/kept/Self_data/mood.tsv")
  (auto-revert-mode))

;;;###autoload
(defun scr-plot-mood ()
  (interactive)
  (let* ((default-directory "/tmp/secretary")
	 (pkg-dir (f-dirname (find-library-name "secretary")))
         (script (expand-file-name "R/sc_daily_plot.R" pkg-dir))
         (plot (expand-file-name "sc_mood.png" default-directory)))
    (scr-emit "Plotting mood...")
    (mkdir "/tmp/secretary" t)
    (set-process-sentinel
     (start-process scr-ai-name nil "Rscript" script "14" "-0.1")
     `(lambda (_process _event)
        (scr-emit "And that's your mood." "\n")
        (switch-to-buffer (scr-buffer-chat))
        (insert-image-file ,plot)
        (goto-char (point-max))
        (insert "\n")))))

;;;###autoload
(defun scr-plot-weight ()
  (let* ((default-directory "/tmp/secretary")
         (script (expand-file-name "R/sc_daily_plot.R"
				   (f-dirname (find-library-name "secretary"))))
         (plot (expand-file-name "sc_plot1.png" "/tmp/secretary")))
    (scr-emit "Plotting weight...")
    (mkdir "/tmp/secretary" t)
    (set-process-sentinel
     (start-process scr-ai-name nil "Rscript" script "14" "-0.1")
     `(lambda (_process _event)
        (scr-emit "And here's your weight, boss." "\n")
        (switch-to-buffer (scr-buffer-chat))
        (insert-image-file ,plot)
        ;; (delete-file ,plot)
        (goto-char (point-max))
        (insert "\n")))))

;;;###autoload
(defun scr-present-plots ()
  (interactive)
  (unless (null scr-plot-hook)
    (switch-to-buffer (scr-buffer-chat))
    (scr-emit (seq-random-elt '("I have plotted the latest intel, boss."
                               "Here are some projections!"
                               "Data is beautiful, don't you think?")))
    (run-hooks 'scr-plot-hook)))

;; Should I use such variables or encourage the user to make defuns?
;; (defcustom scr-look-back-years 99 nil)
;; (defcustom scr-look-back-months 12 nil)
;; (defcustom scr-look-back-weeks 4 nil)
;; (defcustom scr-look-back-days 1 nil)

(defvar scr-past-sample-function #'scr-past-sample-default)

(defun scr-past-sample-default ()
  (let ((now (ts-now)))
    (-uniq (append
	    ;; Yesterday, this weekday the last 4 weeks, this day of the month the
	    ;; last 12 months, and this date from every year in the past.
            (--iterate (ts-dec 'day 1 it) now 1)
            (--iterate (ts-dec 'woy 1 it) now 4)
            (--iterate (ts-dec 'month 1 it) now 12)
            (--iterate (ts-dec 'year 1 it) now 99)))))

(ert-deftest scr-test-ts-usage ()
  (let ((now (ts-now)))
    (should (equal (ts-dec 'month 12 now)
                   (ts-dec 'year 1 now)))
    (should (= 16 (length (-uniq (append
                                  (--iterate (ts-dec 'month 1 it) now 12)
                                  (--iterate (ts-dec 'year 1 it) now 5))))))))

(defun scr-make-indirect-datetree (buffer dates)
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
;; (scr-make-indirect-datetree (get-buffer-create "test") (--iterate (ts-dec 'month 1 it) (ts-now) 40))

;; TODO: allow it to check a different date
;; TODO: allow either discrete or datetree to be nil
;; TODO: allow a list of datetrees
;; TODO: make separate buffers for each datetree entry (use rename-buffer)
;; TODO: make the datetree buffer(s) the next in line when you pop the last
;;       discrete view
;; TODO: try creating a sparse tree, so user can edit in-place
;; TODO: show also the agenda for each date if not empty
;;;###autoload
(defun scr-present-diary (&optional _date)
  (interactive)
  (let* ((buffer (get-buffer-create (concat "*" scr-ai-name ": Selected diary entries*")))
         (dates-to-check (funcall scr-past-sample-function))
         (discrete-files-found (--keep (scr-existing-diary "/home/kept/Diary" it) dates-to-check))
         (datetree-found-count (scr-make-indirect-datetree buffer dates-to-check))
         (total-found-count (+ (length discrete-files-found) datetree-found-count)))
    (if (scr-prompt "Found " (int-to-string total-found-count) " past diary "
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
;; (scr-present-diary (ts-now))

;;;; Handle idling & reboots & crashes
;; Refactor? Make it easier to write unit tests that don't need to wait 60
;; seconds.

(defvar scr--timer nil)

(defvar scr--idle-beginning (ts-now))

(defvar scr-length-of-last-idle 0
  "Duration in seconds.")

(defcustom scr-idle-threshold (* 10 60)
  "Duration in seconds, beyond which the user is considered to be
idle.")

(defcustom scr-long-idle-threshold (* 90 60)
  "Duration in seconds that is the minimum for
`sc-call-from-idle' to trigger upon user return.")

(defcustom scr-return-from-idle-hook nil
  "Note: An Emacs startup also counts as a return from idleness.
You'll probably want your hook to be conditional on some value of
`scr-length-of-last-idle', which at startup is calculated from
the last Emacs shutdown or crash (technically, last time
`secretary-mode' was running).")

(defcustom scr-periodic-not-idle-hook nil
  "Hook run every minute when the user is not idle.")

;; REVIEW: Put the compuer to sleep MANUALLY (so you're not idle), go away 11+
;;         mins, come back and check if the idle.tsv has gained an entry.
(defun scr--start-next-timer (&optional assume-idle)
  "Start one or the other timer depending on idleness. If
ASSUME-IDLE is non-nil, skip the idle check and associated
overhead: useful if the caller has already checked it."
  (if (or assume-idle (scr-idle-p))
      (setq scr--timer (run-with-timer 2 nil #'scr--user-is-idle))
    (setq scr--timer (run-with-timer 60 nil #'scr--user-is-active))))

(defun scr--user-is-active ()
  "This function is meant to be called by `scr--start-next-timer'
repeatedly for as long as the user is active (not idle).

Refresh some variables and sync all variables to disk."
  ;; Patch the case where the user puts the computer to sleep manually, which
  ;; means this function will still be next to run when the computer wakes.  If
  ;; the time difference is suddenly big, hand off to the other function.
  (if (> (ts-diff (ts-now) scr--idle-beginning) scr-idle-threshold)
      (scr--user-is-idle t)
    (setq scr--idle-beginning (ts-now))
    (run-hooks 'scr-periodic-not-idle-hook)
    (scr--start-next-timer)))

(defun scr--user-is-idle (&optional dont-dec)
  "This function is meant to be called by `scr--start-next-timer'
repeatedly for as long as the user is idle.

When the user comes back, this function will be called one last
time, at which point it sets `scr-length-of-last-idle' and runs
`scr-return-from-idle-hook'. That it has to run exactly once with
a failing condition that normally succeeds is the reason it has
to be a separate function from `scr--user-is-active'."
  (if (scr-idle-p)
      (scr--start-next-timer t)
    (unless dont-dec ;; REVIEW: this guard clause failed before
      (ts-decf (ts-sec scr--idle-beginning) scr-idle-threshold))
    (setq scr-length-of-last-idle (ts-diff (ts-now) scr--idle-beginning))
    (run-hooks 'scr-return-from-idle-hook)
    (setq scr--idle-beginning (ts-now))
    (scr--start-next-timer)))

;; TODO
;; Test: save ts-now, wait, run scr--user-is-active, compare.
;; Use a special function that runs the timers very fast for testing.
;; (ert-deftest idle-beginning-does-update ()
;;   (lambda (&optional force-idle)
;;     (if (or force-idle (scr-idle-p))
;; 	(setq scr--timer (run-with-timer 1 nil #'scr--user-is-idle)))
;;     (setq scr--timer (run-with-timer 1 nil #'scr--user-is-active)))
;;   )

(defun scr--restore-variables-from-disk ()
  (when (f-exists-p scr-idle-beginning-file-name)
    (setq scr--idle-beginning
          (ts-parse (f-read scr-idle-beginning-file-name))))
  (when (f-exists-p scr-mood-alist-file-name)
    (setq scr-mood-alist
          (read (f-read scr-mood-alist-file-name)))))

(defun scr--save-variables-to-disk ()
  (make-directory scr-dir t)
  (f-write (ts-format scr--idle-beginning) 'utf-8 scr-idle-beginning-file-name)
  (f-write (prin1-to-string scr-mood-alist) 'utf-8 scr-mood-alist-file-name))

(defvar scr--dog nil)

(defun secretary-unload-function ()
  "For `unload-feature'."
  (secretary-mode 0))

;;;###autoload
(define-minor-mode secretary-mode nil
  :global t
  (if secretary-mode
      (when (cond ((eq system-type 'darwin)
                   (fset #'scr--idle-seconds #'org-mac-idle-seconds))
                  ((and (eq window-system 'x)
                        (executable-find org-clock-x11idle-program-name))
                   (fset #'scr--idle-seconds #'scr--x11-idle-seconds))
                  (t
                   (secretary-mode 0)
                   (message scr-ai-name ": Not able to detect idleness, "
                            "I'll be useless. Disabling secretary-mode.")
                   nil))
        (add-hook 'scr-return-from-idle-hook #'scr-log-idle -90)
        (add-hook 'scr-return-from-idle-hook #'scr-call-from-idle 90)
	(add-hook 'scr-periodic-not-idle-hook #'scr--save-variables-to-disk)
        (add-hook 'window-buffer-change-functions #'scr-log-buffer)
        (add-hook 'window-selection-change-functions #'scr-log-buffer)
	(add-hook 'after-init-hook #'scr--restore-variables-from-disk -1)
        (add-hook 'after-init-hook #'scr--start-next-timer 91)
	(when (null scr--dog)
	  (setq scr--dog (run-with-timer 0 300 #'scr--mark-territory)))
        ;; (add-function :after #'after-focus-change-function #'scr-log-buffer)
        (when after-init-time
          (progn
            (when (-any #'null '(scr-idle-beginning scr-mood-alist))
              (scr--restore-variables-from-disk))
	    (when (null scr--timer)
              (scr--start-next-timer)))))

    (remove-hook 'scr-return-from-idle-hook #'scr-log-idle)
    (remove-hook 'scr-return-from-idle-hook #'scr-call-from-idle)
    (remove-hook 'scr-periodic-not-idle-hook #'scr--save-variables-to-disk)
    (remove-hook 'window-buffer-change-functions #'scr-log-buffer)
    (remove-hook 'window-selection-change-functions #'scr-log-buffer)
    (remove-hook 'after-init-hook #'scr--restore-variables-from-disk)
    (remove-hook 'after-init-hook #'scr--start-next-timer)
    (unless (null scr--timer)
      (cancel-timer scr--timer)
      (setq scr--timer nil))
    (unless (null scr--dog)
      (cancel-timer scr--dog)
      (setq scr--dog nil))))

(provide 'secretary)

;;; secretary.el ends here
