;;; sc-lib.el -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Martin Edström

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'find-func)
(require 'ts)
(require 'dash)
(require 's)
(require 'parse-csv)
(autoload #'sc-log-buffer "org-id")

(defcustom sc-location-diary-discrete "/home/kept/Diary/"
  nil)

(defcustom sc-location-diary-datetree "/home/kept/Journal/diary2.org"
  nil)

(defcustom sc-ai-name "Secretary"
  nil)

(defcustom sc-user-birthday nil
  nil)

(defcustom sc-usrname (if (s-equals? user-full-name "")
                          "Mr. Bond"
                        (-first-item (s-split " " user-full-name)))
  nil)

(defcustom sc-usr-short-title "sir"
  nil)

(defcustom sc-activities-alist
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

(defvar sc-last-buffer nil)

(defvar sc-known-buffers nil)

(defvar sc-buffer-focus-log nil)

(defvar sc-guessed-activity-id nil)

(defvar sc-mood-alist
  '(("meh" . "3")
    ("good" . "4")
    ("great" . "5")
    ("bad" . "2")
    ("fine" . "3")
    ("depressed" . "1")
    ("motivated" . "4")
    ("moody" . "2")
    ("strong" . "5")
    ("inspired" . "5"))
  "Alist for suggesting a mood score in the `sc-log-mood'
prompt. Merely a convenience; the scores are not forced.

This variable is loaded from `sc-mood-alist-file-name', edit that
file.")

(defvar sc-dir (expand-file-name "secretary" user-emacs-directory))
(defvar sc-idle-beginning-file-name (expand-file-name "idle-beginning" sc-dir))
(defvar sc-mood-alist-file-name (expand-file-name "sc-mood-alist" sc-dir))

(defvar sc-sit-long 1)
(defvar sc-sit-medium .8)
(defvar sc-sit-short .5)

(defvar sc-csv-alist '(("/home/kept/Self_data/weight.tsv" sc-query-weight)
                       ("/home/kept/Self_data/mood.tsv" sc-query-mood)
                       ("/home/kept/Self_data/ingredients.tsv" sc-query-ingredients)
                       ("/home/kept/Self_data/sleep.tsv" sc-query-sleep)
                       ("/home/kept/Self_data/meditation.tsv" sc-query-meditation)
                       ("/home/kept/Self_data/cold.tsv" sc-query-cold)))

(defvar sc--date nil
  "Set during talking to the user.")

(defvar sc-plot-hook nil)

(defvar sc-length-of-last-idle 0
  "Amount of time in minutes, an integer.")
(defvar sc-length-of-last-idle-in-minutes 0)
(defvar sc-idle-threshold (* 10 60))
(defvar sc-idle-minutes-threshold 10)
(defvar sc-idle-ticker nil)
(defvar sc-idle-watcher nil)
(defvar sc-idle-beginning nil)
(defvar sc--timer nil)
(defvar sc--idle-beginning (ts-now))
(defvar sc-return-from-idle-hook nil
  "Note: An Emacs startup also counts as a return from idleness.
You'll probably want your hook to be conditional on some value of
`sc-length-of-last-idle-in-minutes', which at startup is
calculated from the last Emacs shutdown or crash (technically,
last time `secretary-mode' was running).")

(defvar sc-chime-sound-file
  (expand-file-name
   ;; From https://freesound.org/people/josepharaoh99/sounds/380482/
   "Chime Notification-380482.wav"
   ;; From https://bigsoundbank.com/detail-0319-knock-on-a-glass-door-1.html
   ;; "DOORKnck_Knock on a glass door 1 (ID 0319)_BSB.wav"
   (f-dirname (find-library-name "secretary"))))

;; What's cl-defstruct? https://nullprogram.com/blog/2018/02/14/
(cl-defstruct (sc-activity (:constructor sc-activity-create)
			   (:copier nil))
  name id cost-false-pos cost-false-neg querier)

(defun sc-activity-by-name (name)
  (declare (side-effect-free t))
  (--find (equal name (sc-activity-name it)) sc-activities))

(defun sc-play-chime ()
  (and (executable-find "aplay")
       (file-exists-p sc-chime-sound-file)
       (start-process "aplay" nil "aplay" sc-chime-sound-file)))


;; Throw this away when the other one is shown to work
;; (defun sc-print-new-date-maybe ()
;;   (with-current-buffer (sc-buffer-chat)
;;       (goto-char (point-max))
;;       (let* ((last-timestamp-pos
;;               (re-search-backward
;;                (rx bol "<" (group (= 2 digit) ":" (= 2 digit)) ">")))
;;              (last-timestamp (buffer-substring (+ 1 (point)) (+ 6 (point)))))
;;         ;; If the hour and minute last printed was greater than the current hour
;;         ;; and minute, it's obviously a new day.
;;         (when (ts> (ts-parse last-timestamp) (ts-now))
;;           (goto-char (point-max))
;;           (read-only-mode 0)
;;           (insert "\n" (ts-format "%Y-%b-%d"))))))
;; ;; (sc-print-new-date-maybe)

(defun sc-print-new-date-maybe ()
  (with-current-buffer (sc-buffer-chat)
    (when (> (ts-day (ts-now))
             (ts-day sc--last-edited))
      (goto-char (point-max))
      (read-only-mode 0)
      (insert "\n" (ts-format "%Y, %B %d") (sc--holiday-maybe)))))

(defun sc--holiday-maybe ()
  (when-let (foo (calendar-check-holidays (calendar-current-date)))
    (concat " -- " foo)))

(defun sc--another-secretary-running-p ()
  (when (file-exists-p "/tmp/secretary/running")
    (let ((age (- (time-convert (current-time) 'integer)
		  (time-convert (file-attribute-modification-time
				 (file-attributes "/tmp/secretary/running"))
				'integer))))
      (> age (* 10 60)))))

(defun sc--mark-territory ()
  (mkdir "/tmp/secretary/" t)
  (f-touch "/tmp/secretary/running"))

(defun sc-activities-names ()
  (->> sc-activities-alist
       (-map (lambda (x) (save-window-excursion
                      (org-id-goto (car x))
                      (-last-item (org--get-outline-path-1)))))))

;; TODO: Show when the user types a noncommittal "k" for "okay". User should
;; have room to express shades of feeling, even if we don't do anything with it.
(defun sc-prompt (&rest strings)
  (let* (;; (default-y-or-n-p-map y-or-n-p-map)
         ;; (default-cmd (lookup-key y-or-n-p-map (kbd "k")))
         (prompt (string-join strings)))
    (unwind-protect
        (progn
          (switch-to-buffer (sc-buffer-chat))
          (unless (< 20 (car (window-fringes)))
            (set-window-fringes nil 20 20))
          (goto-char (point-max))
          (sc-emit prompt)
          (define-key y-or-n-p-map (kbd "o") #'sc-special-handle-current-query)
          (define-key y-or-n-p-map (kbd "i") #'sc-special-handle-current-query)
          (define-key y-or-n-p-map (kbd "k") #'y-or-n-p-insert-y)
          (read-only-mode 0)
          (if (y-or-n-p prompt)
              (progn
                (insert " y" "\n")
                (view-mode)
                t)
            (insert " n" "\n")
            (view-mode)
            nil))
      (dolist (x '("o" "i" "k"))
        (define-key y-or-n-p-map (kbd x) #'y-or-n-p-insert-other)))))
;; (sc-prompt "Test")
;; (y-or-n-p "test")

(defun sc--idle-seconds ()
  "Stub to be overwritten."
  (warn "Code ended up in an impossible place."))

(defvar sc--last-edited (ts-now)
  "For use as buffer-local value in the chat buffer.")

(defun sc-emit (&rest strings)
  (setq-local sc--last-edited (ts-now))
  (prog1 (message (string-join strings))
    (with-current-buffer (sc-buffer-chat)
      (read-only-mode 0)
      (goto-char (point-max))
      (insert "\n<" (ts-format "%H:%M") "> " (string-join strings))
      (view-mode))))

(defun sc--buffer-r ()
  (get-buffer-create (concat "*" sc-ai-name ": R*")))

(defun sc-reschedule ()
    (run-with-timer 3600 nil #'sc-call-from-reschedule))

;; (defmacro sc-with-file (path &rest body)
;;   (declare (pure t) (indent defun))
;;   `(with-temp-buffer
;;      (insert-file-contents-literally ,path)
;;      ,@body))

(defun sc-last-date-string-in-date-indexed-csv (path)
  (declare (side-effect-free t))
  (with-temp-buffer
    (insert-file-contents-literally path)
    (goto-char (point-max))
    (re-search-backward (rx bol (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)))
    (buffer-substring (point) (+ 10 (point)))))
;; (sc-last-date-string-in-date-indexed-csv "/home/kept/Self_data/weight.csv")
;; (sc-last-date-string-in-date-indexed-csv "/home/kept/Self_data/mood.tsv")
;; (sc-last-date-string-in-file "/home/kept/Self_data/mood.tsv")

(defun sc-last-date-string-in-file (path)
  (declare (side-effect-free t))
  (with-temp-buffer
    (insert-file-contents-literally path)
    (goto-char (point-max))
    (re-search-backward
     (rx (or (group (= 4 digit) "-" (= 3 wordchar) "-" (= 2 digit))
             (group (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)))))
    (buffer-substring (point) (+ 11 (point)))))

;; WONTFIX: check for recent activity (if user awake thru the night)
(defun sc-logged-today (file)
  (when (file-exists-p file)
    ;; don't act like it's a new day if the time is <5am.
    (let ((day (if (> 5 (ts-hour (ts-now)))
                   (ts-dec 'day 1 (ts-now))
                 (ts-now))))
      (with-temp-buffer
        (insert-file-contents-literally file)
        (ignore-errors (search-forward (ts-format "%F" day)))))))
;; (sc-logged-today "/home/kept/Self_data/weight.csv")
;; (sc-logged-today "/home/kept/Self_data/buffers.csv")

(defun sc-existing-diary (dir date)
  (declare (side-effect-free t))
  (let ((foo (car (--filter (string-match-p
                             (concat (ts-format "%y%m%d" date) ".*org$")
                             it)
                            (directory-files dir)))))
    (unless (null foo)
      (expand-file-name foo dir))))
;; (sc-existing-diary "/home/kept/Diary" (ts-dec 'month 1 (ts-now)))

(defun sc-buffer-mode (buffer-or-name)
  "Retrieve the `major-mode' of BUFFER-OR-NAME."
  (declare (side-effect-free t))
  (buffer-local-value 'major-mode (get-buffer buffer-or-name)))

(defun sc-idle-p ()
  (declare (side-effect-free t))
  (< (* 60 sc-idle-minutes-threshold) (sc--idle-seconds)))

(defun sc--x11-idle-seconds ()
  "Like `org-x11-idle-seconds' but doesn't need a /bin/sh, or org."
  (declare (side-effect-free t))
  (/ (sc--process-output-to-number org-clock-x11idle-program-name) 1000))

(defmacro sc--process-output-to-string (program &rest args)
  "Similar to `shell-command-to-string', but skips the shell intermediary so
you don't need `/bin/sh'. PROGRAM and ARGS are passed on to `call-process'."
  (declare (debug (&rest form)) (indent nil) (side-effect-free t))
  `(with-temp-buffer
     (call-process ,program nil (current-buffer) nil ,@args)
     (buffer-string)))

(defmacro sc--process-output-to-number (program &rest args)
  "Same as `sc-process-output-to-string', but passes the result
through `string-to-number'."
  (declare (debug (&rest form)) (indent nil) (side-effect-free t))
  `(string-to-number (sc--process-output-to-string ,program ,@args)))

;; (parse-csv-string-rows
;;  (f-read "/home/kept/Self_data/weight.csv") (string-to-char ",") (string-to-char " ") "\n")

(defun sc-get-all-today-in-date-indexed-csv (path &optional ts)
  (with-temp-buffer
    (insert-file-contents-literally path)
    (let (x)
      (while (search-forward (ts-format "%F" ts) nil t)
        (push (parse-csv->list (buffer-substring (line-beginning-position)
                                                 (line-end-position)))
              x))
      x)))
;; (sc-get-all-today-in-date-indexed-csv "/home/kept/Self_data/sleep.csv" (ts-dec 'day 1 (ts-now)))

;; (defun sc-update-or-append-in-date-indexed-csv (path &optional ts)
;;   (sc-get-first-today-in-date-indexed-csv path ts))

(defun sc-get-first-today-in-date-indexed-csv (path &optional ts)
  (with-temp-buffer
    (insert-file-contents path)
    (search-forward (ts-format "%F" ts))
    (line-beginning-position)
    (buffer-substring (line-beginning-position) (line-end-position))))
;; (sc-get-first-today-in-date-indexed-csv "/home/kept/Self_data/ingredients.csv")

(defun sc-last-value-in-tsv (path)
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents-literally path)
      (goto-char (point-max))
      (search-backward "\t")
      (forward-char)
      (buffer-substring (point) (line-end-position)))))

;; - Needs to be a function because something might kill the buffer.
;; - Can't use get-buffer-create because I want to turn on `visual-line-mode'.
(defun sc-buffer-chat ()
  (if-let* ((name (concat "*" sc-ai-name ": Chat log*"))
            (b (get-buffer name)))
      b
    (with-current-buffer (generate-new-buffer name)
      (visual-line-mode)
      (current-buffer))))

(defun sc-append (path &rest text)
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

(defalias 'sc-append-to #'sc-append)

(defvar sc-aphorisms
  '("The affairs of the world will go on forever. Do not delay the practice of meditation."
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
    "Adaptive is not optimal."
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

(defun sc-greeting-curt ()
  "Return a random greeting string appropriate in the midst of a
workday. If you've already exchanged good mornings, it's weird to
do so again."
  (seq-random-elt `("Hello" "Hi" "Hey")))

(defvar sc-greetings '("Welcome back, Master."
                       (concat "Nice to see you again, " sc-usrname ".")
                       (concat "Greetings, " sc-usrname ".")
                       )
  "Greetings which can work as first sentence in a longer message.")

(defun sc-greeting ()
  "Return a random greeting string."
  ;; If it's morning, always use a variant of "good morning"
  (if (> 10 (ts-hour (ts-now)) 5)
      (seq-random-elt (sc--daytime-appropriate-greetings))
    (eval (seq-random-elt (append sc-greetings
                                (-list (sc--daytime-appropriate-greetings)))))))
;; (sc-greeting)

;; NOTE: I considered making external variables for morning, day and evening
;; lists, but users might also want to change the daytime boundaries or even add
;; new boundaries. Too many possibilities, this is a case where it's ok to make
;; the user override the defun as a primary means of customization.
(defun sc--daytime-appropriate-greetings ()
  (cond ((> 5 (ts-hour (ts-now)))
         (list "You're up late, Master."
               "Burning the midnight oil?"))
        ((> 10 (ts-hour (ts-now)))
         (list (concat "Good morning, " sc-usrname ".")
               "Good morning!"
               "The stars shone upon us last night."))
        ((> 16 (ts-hour (ts-now)))
         (list "Good day!"
               ))
        (t
         (list "Good evening!"
               "Pleasant evening to you!"))))

(defun sc-greeting-standalone ()
  "Return a greeting that expects to be followed by nothing: no
prompts, no debug message, no info. Suitable for
`notifications-notify' or `startup-echo-area-message'. A superset
of `sc-greeting'."
  (eval (seq-random-elt
         (append sc-greetings
                 (-list (sc--daytime-appropriate-greetings))
                 '("How may I help?")))))

(provide 'sc-lib)

;;; sc-lib.el ends here
