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

;; (define-package secretary
;;   0.1
;;   nil
;;   '(hydra "0.15.0"))

(require 'secretary-common)
(require 'secretary-data-collector)
(require 'secretary-presenter)
(require 'secretary-nlp)

(autoload #'org-mac-idle-seconds "org-clock")
(autoload #'org-x11-idle-seconds "org-clock")
(autoload #'org-clock-in "org-clock")
(autoload #'org-id-uuid "org-id")
(autoload #'org-id-goto "org-id")
(autoload #'notifications-notify "notifications")

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
