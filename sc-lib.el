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

(require 'ts)
(require 'dash)
(require 'subr-x)
(autoload #'org-user-idle-seconds "org-clock")
(autoload #'org-id-uuid "org-id")
(autoload #'sc-append* "s")
(autoload #'sc-append "s")

;; TODO: Show when the user types a noncommittal "k" for "okay". User should be
;; given room to express such shades of feeling.
(defun sc-prompt (&rest strings)
  (let ((default-cmd (lookup-key y-or-n-p-map (kbd "k")))
        (prompt (string-join strings)))
    (unwind-protect
        (progn
          (define-key y-or-n-p-map (kbd "k") #'y-or-n-p-insert-y)
          (switch-to-buffer (sc-chat-buffer))
          (unless (< 20 (car (window-fringes)))
            (set-window-fringes nil 20 20))
          (goto-char (point-max))
          (read-only-mode 0)
          (let ((beg (point)))
            (insert "\n\n" prompt)
            (add-text-properties beg (point) '(:foreground "blue")))
          ;; (insert (propertize prompt
          ;;                     :foreground (face-foreground font-lock-function-name-face)
          ;;                     :background (face-background font-lock-function-name-face)))
          (if (y-or-n-p prompt)
              (progn
                (insert " y" "\n")
                (view-mode)
                t)
            (insert " n" "\n")
            (view-mode)
            nil))
      (define-key y-or-n-p-map (kbd "k") default-cmd))))
;; (setq retval (sc-prompt "test"))

(defun sc-emit (&rest strings)
  (message (string-join strings))
  (with-current-buffer (sc-chat-buffer)
    (read-only-mode 0)
    (insert "\n" (string-join strings))
    (view-mode)))

;; WONTFIX: check for recent activity (user awake thru the night)
(defun sc-logged-today (file)
  ;; don't act like it's a new day if the time is <5.
  (let ((day (if (> 5 (ts-hour (ts-now)))
                 (ts-dec 'day 1 (ts-now))
               (ts-now))))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (ignore-errors (search-forward (ts-format "%F" day))))))
;; (sc-logged-today "/home/kept/Self_data/weight.csv")
;; (sc-logged-today "/home/kept/Self_data/buffers.csv")


(defun sc-existing-diary (dir date)
  (let ((foo (car (--filter (string-match-p
                             (concat (ts-format "%y%m%d" date) ".*org$")
                             it)
                            (directory-files dir)))))
    (unless (null foo)
      (expand-file-name foo dir))))
;; (sc-existing-diary "/home/kept/Diary" (ts-dec 'month 1 (ts-now)))

(defun sc-buffer-mode (buffer-or-name)
  "Retrieve the `major-mode' of BUFFER-OR-NAME."
  (with-current-buffer buffer-or-name
    major-mode))

;; Arguably simpler
(defun sc-idle-p ()
  (< sc-idle-threshold (org-user-idle-seconds)))

(defun sc-idle-p* ()
  (< (* 1000 sc-idle-threshold)
     (string-to-number (sc-process-output-to-string
                        org-clock-x11idle-program-name))))

(defmacro sc-process-output-to-string (program &rest args)
  "Similar to `shell-command-to-string', but skips the shell intermediary so
you don't need a `/bin/sh' installed. PROGRAM and ARGS are passed on to
 `call-process'."
  `(with-temp-buffer
     (call-process ,program nil (current-buffer) nil ,@args)
     (buffer-string)))

(defun sc-chat-buffer ()
  (get-buffer-create "*Secretary: Chat log"))

(defun sc-append* (path &rest text)
  (let ((newline-maybe (if (s-ends-with? "\n" (f-read-bytes path))
                           ""
                         "\n")))
    (f-append (concat newline-maybe (string-join text)) 'utf-8 path)))

(defun sc-append (text path)
  (let ((newline-maybe (if (s-ends-with? "\n" (sc-process-output-to-string "tail" path))
                           ""
                         "\n")))
    (f-append (concat newline-maybe text) 'utf-8 path)))
;;(sc-append "lel" "/home/kept/Self_data/weight.csv")



;; TODO: Catch typos like 03 meaning 30 minutes, not 3 hours
(defun sc-parse-time-amount (input)
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
;; (sc-parse-time-amount "30")


;; (defmacro sc-prompt (&rest sequences)
;;   "SEQUENCES are passed to `concat'."
;;   `(sc--prompt (concat ,@sequences)))


(defvar sc-aphorisms
  '("The affairs of the world will go on forever. Do not delay the practice of meditation."
    "Serve the Emperor today, tomorrow you may be dead."
    "It takes all the running you can do, to keep in the same place."
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
    ))

;;;; Greetings

(defvar sc-greetings '("Welcome back, Master."
                       (concat "Nice to see you again, " sc-usrname ".")
                       (concat "Greetings, " sc-usrname ".")
                       )
  "Greetings which can work as first sentence in a longer message.")

(defun sc-greeting ()
  ;; If it's morning, always use a variant of "good morning"
  (if (> 10 (ts-hour (ts-now)) 5)
      (seq-random-elt (sc-daytime-appropriate-greetings))
    (eval (seq-random-elt (append sc-greetings
                                (-list (sc-daytime-appropriate-greetings)))))))
;; (sc-greeting)

;; NOTE: I considered making external variables for morning, day and evening
;; lists, but users might also want to change the daytime boundaries or even add
;; new boundaries. Too many possibilities, this is a case where it's ok to make
;; the user override the defun as a primary means of customization.
(defun sc-daytime-appropriate-greetings ()
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
`notifications-notify' or `startup-echo-area-message'."
  (eval (seq-random-elt
         (append sc-greetings
                 (-list (sc-daytime-appropriate-greetings))
                 '("How may I help?")))))


(provide 'sc-lib)

;;; sc-lib.el ends here
