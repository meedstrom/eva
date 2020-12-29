;;; secretary.el -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Martin Edström

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

;; user setup
(setc org-clock-x11idle-program-name "xprintidle")
(setc scr-usrname "Martin")
(setc scr-usr-short-title "sir")
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

(require 'f)
(require 's)
(require 'ts)
(require 'dash)
(require 'seq)
;; (require 'bui)
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
  (scr-print-new-date-maybe)
  (scr-emit "Hello!")
  (sit-for scr-sit-short)
  (scr-welcome arg))

(defun scr-call-from-idle ()
  "Called by idleness-related hooks, doesn't do much if idle was not long."
  (unwind-protect
      (when (< 90 scr-length-of-last-idle-in-minutes)
        (unless (frame-focus-state)
          (notifications-notify :title scr-ai-name :body (scr-greeting)))
        (scr-print-new-date-maybe)
        (scr-play-chime)
        (when (scr-prompt (scr-greeting) " Do you have time for some questions?")
          (scr-welcome t)))
    (setq scr-length-of-last-idle-in-minutes 0)))
;; (run-with-timer 3 nil (lambda ()  (print (frame-focus-state))))
;; (scr-call-from-idle)

(defun scr-call-from-reschedule ()
  (scr-print-new-date-maybe)
  (scr-play-chime)
  (scr-emit "Hello, " scr-usr-short-title ". ")
  (sit-for scr-sit-medium)
  (when (scr-prompt "1 hour ago, you asked to be reminded. Is now a good time?")
    (scr-welcome)))
;;(scr-call-from-reschedule)

;; TODO: Apply changed date from `scr-special-handle-current-query' to the
;; appropriate places and show the results.
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
  (and (> 3 (scr-query-mood "How are you? "))
       (scr-prompt "Do you need to talk?")
       (scr-prompt "I can direct you to my colleague Eliza, though "
                  "she's not too bright. Will that do?")
       (doctor))
  (scr-present-plots)
  ;; and (scr-prompt "Would you like me to suggest an activity?")
  (scr-present-diary (ts-now))
  (and (-all-p #'null (-map #'scr-logged-today (-map #'car scr-csv-alist)))
       (scr-prompt "Shall I come back in an hour?")
       (run-with-timer 3600 nil #'scr-call-from-idle))
  )

(defun scr-check-neglect ()
  (dolist (cell scr-csv-alist)
    (when (file-exists-p (car cell))
      (let* ((path (car cell))
             (d (ts-parse (scr-last-date-string-in-date-indexed-csv path)))
             (diff-days (/ (ts-diff (ts-now) d) 60 60 24)))
	(when (< 3 diff-days)
	  (scr-prompt "It's been " (number-to-string diff-days)
                     " days since you logged " (file-name-base path)
                     ". Do you want to log it now?")
	  (call-interactively (cadr cell)))))))

;;;; Handle idling & reboots & crashes

;; (defsubst scr--start-next-timer ()
;;   (if (scr-idle-p)
;;       (progn
;;         (setq scr-idle-watcher nil)
;;         (setq scr-idle-ticker (run-with-timer 1 nil #'scr--react-if-idle-ended)))
;;     (setq scr-idle-watcher (run-with-timer 60 nil #'scr--react-if-idle-started))
;;     (setq scr-idle-ticker nil)))

;; (defun scr--react-if-idle-started ()
;;   (setq scr-idle-beginning (ts-now)) ;; remember the time we went idle
;;   (scr--save-variables-to-disk)
;;   (scr--start-next-timer))

;; (defun scr--react-if-idle-ended ()
;;   (unless (scr-idle-p)
;;     (setq scr-length-of-last-idle-in-minutes
;; 	  (+ scr-idle-threshold
;;              (round (/ (ts-diff (ts-now) scr-idle-beginning) 60))))
;;     (run-hooks 'scr-return-from-idle-hook))
;;   (scr--start-next-timer))

;; (defun scr--next-tick ()
;;   (if (> (scr--idle-seconds) scr-idle-threshold)
;;       (progn
;; 	(ts-decf (ts-sec scr--idle-beginning) scr-idle-threshold)
;; 	(setq scr-length-of-last-idle (ts-diff (ts-now) scr--idle-beginning))
;; 	(run-hooks 'scr-return-from-idle-hook)
;;         (setq scr--timer (run-with-timer 2 nil #'scr--next-tick)))
;;     (setq scr--idle-beginning (ts-now))
;;     (scr--save-variables-to-disk)
;;     (setq scr--timer (run-with-timer 60 nil #'scr--next-tick))))
;; ;; (scr--next-tick)

(defun scr--start-next-timer (&optional idle-p)
  (if (or idle-p (scr-idle-p))
      (setq scr--timer (run-with-timer 2 nil #'scr--user-is-idle)))
  (setq scr--timer (run-with-timer 60 nil #'scr--user-is-active)))

(defun scr--user-is-active ()
  ;; Explanation: We keep updating this variable as long as the user is active,
  ;; expecting to stop updating once they go idle.
  (setq scr-idle-beginning (ts-now))
  (scr--save-variables-to-disk)
  (scr--start-next-timer))

(defun scr--user-is-idle ()
  (if (scr-idle-p)
      (scr--start-next-timer t)
    (ts-decf (ts-sec scr--idle-beginning) scr-idle-threshold)
    (setq scr-length-of-last-idle (ts-diff (ts-now) scr--idle-beginning))
    (run-hooks 'scr-return-from-idle-hook)
    (scr--start-next-timer)))

(defun scr--start-next-timer (&optional idle)
  (if (or idle (scr-idle-p)) ;; don't re-call `scr-idle-p' if info was provided
      (progn
        (setq scr-idle-watcher nil)
        (setq scr-idle-ticker (run-with-timer 1 nil #'scr--user-is-idle)))
    (setq scr-idle-watcher (run-with-timer 60 nil #'scr--user-is-active))
    (setq scr-idle-ticker nil)))

(defun scr--user-is-active ()
  "This function is meant to be called by `scr--start-next-timer'
repeatedly for as long as the user is active (not idle).

Refresh some variables and sync to disk."
  ;; Explanation: We keep updating this variable as long as the user is active,
  ;; expecting to stop updating once they go idle.
  (setq scr-idle-beginning (ts-now))
  (scr--save-variables-to-disk)
  (scr--start-next-timer))

(defun scr--user-is-idle ()
  "This function is meant to be called by `scr--start-next-timer'
repeatedly for as long as the user is idle.  When the user comes
back, this function will be called one last time, at which point
it sets `scr-length-of-last-idle-in-minutes' and runs
`scr-return-from-idle-hook'. That it has to run once with a
failing condition that normally succeeds is the reason it had to
be a separate function from `scr--user-is-active'."
  (if (scr-idle-p)
      (scr--start-next-timer t)
    (setq scr-length-of-last-idle-in-minutes (scr--calc-last-idle-in-minutes))
    (run-hooks 'scr-return-from-idle-hook)
    (scr--start-next-timer)))

(defsubst scr--calc-last-idle-in-minutes ()
  (+ scr-idle-minutes-threshold
     (round (/ (ts-diff (ts-now) scr-idle-beginning) 60))))

(defun scr--restore-variables-from-disk ()
  (when (f-exists-p scr-idle-beginning-file-name)
    (setq scr-idle-beginning
          (ts-parse (f-read scr-idle-beginning-file-name))))
  (when (f-exists-p scr-mood-alist-file-name)
    (setq scr-mood-alist
          (read (f-read scr-mood-alist-file-name)))))

(defun scr--save-variables-to-disk ()
  (f-write (ts-format scr-idle-beginning) 'utf-8 scr-idle-beginning-file-name)
  (f-write (prin1-to-string scr-mood-alist) 'utf-8 scr-mood-alist-file-name))

;; WIP
(defvar scr-org-capture-templates
  `(
    ("s" "Secretary.el queries and presentations")
    ("sw" "weight" plain (function #'scr-query-weight)
     :immediate-finish t)
    ))

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
        (add-hook 'window-buffer-change-functions #'scr-log-buffer)
        (add-hook 'window-selection-change-functions #'scr-log-buffer)
        ;; (add-function :after #'after-focus-change-function #'scr-log-buffer)
        (if after-init-time
            (progn
              (when (-any #'null '(scr-idle-beginning scr-mood-alist))
                (scr--restore-variables-from-disk))
	      ;; (when (null scr--timer)
              ;;   (scr--start-next-timer))
              (when (-all-p #'null '(scr-idle-watcher scr-idle-ticker))
                (scr--start-next-timer))
	      )
          (add-hook 'after-init-hook #'scr--restore-variables-from-disk -1)
          (add-hook 'after-init-hook #'scr--start-next-timer 91)))
    (remove-hook 'scr-return-from-idle-hook #'scr-log-idle)
    (remove-hook 'scr-return-from-idle-hook #'scr-call-from-idle)
    (remove-hook 'window-buffer-change-functions #'scr-log-buffer)
    (remove-hook 'window-selection-change-functions #'scr-log-buffer)
    (remove-hook 'after-init-hook #'scr--restore-variables-from-disk)
    (remove-hook 'after-init-hook #'scr--start-next-timer)
    (unless (null scr--timer)
      (cancel-timer scr--timer)
      (setq scr--timer nil))
    (unless (null scr-idle-watcher)
      (cancel-timer scr-idle-watcher)
      (setq scr-idle-watcher nil))
    (unless (null scr-idle-ticker)
      (cancel-timer scr-idle-ticker)
      (setq scr-idle-ticker nil))))

(defun scr-report-mood ()
  (interactive)
  (view-file "/home/kept/Self_data/mood.tsv")
  (auto-revert-mode))

(provide 'secretary)

;;; secretary.el ends here
