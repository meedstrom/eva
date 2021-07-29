;;; secretary-config.el -*- lexical-binding: t; nameless-current-name: "secretary"; -*-
;; Copyright (C) 2021 Martin Edström

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Example config.

;;; Code:

;; user setup

;; (require 'org-id)
;; (org-id-update-id-locations '("/home/kept/Emacs/secretary/test.org"))



;; must be set early
(setq secretary-ai-name "Maya")

;; probably must be set early
(setq secretary-fallback-to-emacs-idle-p t)

;; best set early, but not breaking if not
(setq secretary-user-short-title "sir")
(setq secretary-user-name "Martin")
(setq secretary-user-birthday "1991-12-07")
(setq secretary-debugp t)
;; (setq org-journal-dir)
;; (setq org-journal-file-format)

(require 'secretary)

(defconst secretary-memory '((secretary-user-short-title . "sir")
                             (secretary-user-name . "Martin")
                             (secretary-user-short-title . "1991-12-07"))
  "Alist of variable values.
Why not custom-file?  Because...  Remembering past info, even
stuff the user doesn't particularly want remembered, is a core
component of what makes a virtual secretary work.  People are
always wiping their custom-file, admittedly for a reason, but
this is no mere matter of configuration.")

(setq secretary-x11idle-program-name "x11idle")

(defvar secretary--excursion-buffer nil)

(setq secretary-items
      (list
       (secretary-item-create
        :fn (secretary-defun secretary-greet ()
              "Experimental greeter to drop into the queries infrastructure."
              (pop-to-buffer (secretary-buffer-chat))
              (message (secretary-emit (secretary-greeting)))
              (sit-for secretary-sit-medium))
        :min-hours-wait 1
        :lookup-posted-time t)
       (secretary-item-create
        :fn (secretary-defun secretary-present-ledger-file ()
              (message (secretary-emit "Sending you to your Ledger file. Sayonara!"))
              (sit-for secretary-sit-medium)
              (add-hook 'kill-buffer-hook #'secretary-return-from-excursion)
              (named-timer-run :secretary-excursion (* 5 60) nil #'secretary-end-session)
              (display-buffer (setq secretary--excursion-buffer
                                    (find-file-other-window secretary-ledger-file-name)))
              (setq secretary--queue
                     (remove secretary--current-fn secretary--queue))
              ;;(keyboard-quit) ;; FIXME
              ))
       (secretary-item-create :fn #'secretary-query-sleep
                              :dataset "/home/kept/Self_data/sleep.tsv"
                              :min-hours-wait 5
                              :lookup-posted-time t)
       (secretary-item-create
        :fn (secretary-defun secretary-present-org-agenda ()
              (message (secretary-emit "Sending you to the Org agenda."))
              (sit-for secretary-sit-short)
              (add-hook 'kill-buffer-hook #'secretary-return-from-excursion)
              (named-timer-run :secretary-excursion (* 5 60) nil #'secretary-end-session)
              (org-agenda-list)
              (setq secretary--excursion-buffer (current-buffer))))

       (secretary-item-create :fn #'secretary-query-weight
                              :dataset "/home/kept/Self_data/weight.tsv"
                              :max-entries-per-day 1)
       (secretary-item-create :fn #'secretary-query-mood
                              :dataset "/home/kept/Self_data/mood.tsv")
       (secretary-item-create :fn #'secretary-query-ingredients
                              :dataset "/home/kept/Self_data/ingredients.tsv"
                              :min-hours-wait 5)
       (secretary-item-create :fn #'secretary-query-cold-shower
                              :dataset "/home/kept/Self_data/cold.tsv"
                              :max-entries-per-day 1)
       (secretary-item-create :fn #'secretary-query-activity
                              :dataset "/home/kept/Self_data/activities.tsv")
       ))

(defun secretary-return-from-excursion ()
  (when (eq (current-buffer) secretary--excursion-buffer)
    (named-timer-cancel :secretary-excursion)
    (remove-hook 'kill-buffer-hook #'secretary-return-from-excursion)
    (secretary-resume)))

(defun secretary-end-session ()
  (remove-hook 'kill-buffer-hook #'secretary-return-from-excursion))

;; Surprisingly, sublists may not be necessary.
(setq secretary--queue-sublist-counter 0)

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
             :cost-false-neg 8)))

(add-hook 'secretary-plot-hook #'secretary-plot-mood-ascii)
(add-hook 'secretary-plot-hook #'secretary-plot-weight-ascii)

;; TODO: Merge with `secretary-read'
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

(setq secretary-aphorisms
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

(provide 'secretary-config)

;;; secretary-config.el ends here
