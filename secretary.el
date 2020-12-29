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
(setc sc-usrname "Martin")
(setc sc-usr-short-title "sir")
(setc sc-user-birthday "1991-12-07")
(setc sc-ai-name "Maya")
(add-hook 'sc-plot-hook #'sc-plot-mood 50)
(add-hook 'sc-plot-hook #'sc-plot-weight)

;; (require 'org-id)
;; (org-id-update-id-locations '("/home/kept/Emacs/secretary/test.org"))

(setc sc-activities-alist
      '(;; id            cost of misprediction (either false positive or false negative)
        ("7441f4e2-7251-47d8-ab06-1c2c205d1ae0"    0)  ;; unknown
        ("c1b1fdb8-ea87-4d5a-b01d-4214266c4f4b"    1)  ;; unknown (afk)
        ("24553859-2214-4fb0-bdc9-84e7f3d04b2b"    8)  ;; studying
        ("a0fdbb69-9cdb-418f-b5e8-37601af43c0d"    6)  ;; coding
        ("784d67a5-c15b-4c09-8c74-97b5767d70e6"    2)  ;; downtime
        ("7aaf9105-d58d-4e83-9d34-045a3c922ac5"   20)  ;; meditating
        ("ac93c132-ab74-455f-a456-71d7b5ee88a6"    3)  ;; sleep
        ))

(setc sc-activities-alist
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
(require 'sc-lib)
(require 'sc-data-collector)
(require 'sc-presenter)
(require 'sc-nlp)

(autoload #'org-mac-idle-seconds "org-clock")
(autoload #'org-x11-idle-seconds "org-clock")
(autoload #'org-clock-in "org-clock")
(autoload #'org-id-uuid "org-id")
(autoload #'org-id-goto "org-id")
(autoload #'notifications-notify "notifications")

(defun sc-call (&optional arg)
  "Call your secretary. Useful when you're unsure what to do."
  (interactive "P")
  (sc-print-new-date-maybe)
  (sc-emit "Hello!")
  (sit-for sc-sit-short)
  (sc-welcome arg))

(defun sc-call-from-idle ()
  "Called by idleness-related hooks, doesn't do much if idle was not long."
  (unwind-protect
      (when (< 90 sc-length-of-last-idle-in-minutes)
        (unless (frame-focus-state)
          (notifications-notify :title sc-ai-name :body (sc-greeting)))
        (sc-print-new-date-maybe)
        (sc-play-chime)
        (when (sc-prompt (sc-greeting) " Do you have time for some questions?")
          (sc-welcome t)))
    (setq sc-length-of-last-idle-in-minutes 0)))
;; (run-with-timer 3 nil (lambda ()  (print (frame-focus-state))))
;; (sc-call-from-idle)

(defun sc-call-from-reschedule ()
  (sc-print-new-date-maybe)
  (sc-play-chime)
  (sc-emit "Hello, " sc-usr-short-title ". ")
  (sit-for sc-sit-medium)
  (when (sc-prompt "1 hour ago, you asked to be reminded. Is now a good time?")
    (sc-welcome)))
;;(sc-call-from-reschedule)

;; TODO: Apply changed date from `sc-special-handle-current-query' to the
;; appropriate places and show the results.
(defun sc-welcome (&optional just-idled-p)
  (setq sc--date (ts-now))
  (sc-followup)
  (and just-idled-p
       (sc-prompt "Have you slept?")
       (sc-query-sleep))
  (unless (sc-logged-today "/home/kept/Self_data/weight.tsv")
    (sc-query-weight))
  ;; (and (sc-prompt "Up to reflect?")
  ;;      (sc-prompt "Have you learned something?")
  ;;      (org-capture nil "s"))
  ;; (if (sc-prompt (concat "How about some flashcards?"))
  ;;     (org-drill))
  ;; (if (sc-prompt "Have you stretched today?")
  ;;     nil
  ;;   (if (sc-prompt "Do you want reminders for why?")
  ;;       nil
  ;;     nil))
  ;; (if (sc-prompt "Did you photographe your face today?")
  ;;     nil)
  ;; ;; (unless (sc-just-slept)
  ;; (unless (sc-logged-today "/home/kept/Self_data/meditation.csv")
  ;;   (sc-query-meditation sc--date))
  ;; (unless (sc-logged-today "/home/kept/Self_data/cold.csv")
  ;;   (when (sc-prompt "Have you had a cold shower yet?")
  ;;     (sc-query-cold-shower sc--date)))
  ;; (if (sc-prompt "Have you paid for anything since yesterday?")
  ;;     (org-capture nil "ln"))
  ;; (if (sc-prompt "Shall I remind you of your life goals? Don't be shy.")
  ;;     (view-file "/home/kept/Journal/gtd2.org"))
  (and (> 3 (sc-query-mood "How are you? "))
       (sc-prompt "Do you need to talk?")
       (sc-prompt "I can direct you to my colleague Eliza, though "
                  "she's not too bright. Will that do?")
       (doctor))
  (sc-present-plots)
  ;; and (sc-prompt "Would you like me to suggest an activity?")
  (sc-present-diary (ts-now))
  (and (-all-p #'null (-map #'sc-logged-today (-map #'car sc-csv-alist)))
       (sc-prompt "Shall I come back in an hour?")
       (run-with-timer 3600 nil #'sc-call-from-idle))
  )

(defun sc-check-neglect ()
  (dolist (cell sc-csv-alist)
    (when (file-exists-p (car cell))
      (let* ((path (car cell))
             (d (ts-parse (sc-last-date-string-in-date-indexed-csv path)))
             (diff-days (/ (ts-diff (ts-now) d) 60 60 24)))
	(when (< 3 diff-days)
	  (sc-prompt "It's been " (number-to-string diff-days)
                     " days since you logged " (file-name-base path)
                     ". Do you want to log it now?")
	  (call-interactively (cadr cell)))))))

;;;; Handle idling & reboots & crashes

;; (defsubst sc--start-next-timer ()
;;   (if (sc-idle-p)
;;       (progn
;;         (setq sc-idle-watcher nil)
;;         (setq sc-idle-ticker (run-with-timer 1 nil #'sc--react-if-idle-ended)))
;;     (setq sc-idle-watcher (run-with-timer 60 nil #'sc--react-if-idle-started))
;;     (setq sc-idle-ticker nil)))

;; (defun sc--react-if-idle-started ()
;;   (setq sc-idle-beginning (ts-now)) ;; remember the time we went idle
;;   (sc--save-variables-to-disk)
;;   (sc--start-next-timer))

;; (defun sc--react-if-idle-ended ()
;;   (unless (sc-idle-p)
;;     (setq sc-length-of-last-idle-in-minutes
;; 	  (+ sc-idle-threshold
;;              (round (/ (ts-diff (ts-now) sc-idle-beginning) 60))))
;;     (run-hooks 'sc-return-from-idle-hook))
;;   (sc--start-next-timer))

;; (defun sc--next-tick ()
;;   (if (> (sc--idle-seconds) sc-idle-threshold)
;;       (progn
;; 	(ts-decf (ts-sec sc--idle-beginning) sc-idle-threshold)
;; 	(setq sc-length-of-last-idle (ts-diff (ts-now) sc--idle-beginning))
;; 	(run-hooks 'sc-return-from-idle-hook)
;;         (setq sc--timer (run-with-timer 2 nil #'sc--next-tick)))
;;     (setq sc--idle-beginning (ts-now))
;;     (sc--save-variables-to-disk)
;;     (setq sc--timer (run-with-timer 60 nil #'sc--next-tick))))
;; ;; (sc--next-tick)

(defun sc--start-next-timer (&optional idle-p)
  (if (or idle-p (sc-idle-p))
      (setq sc--timer (run-with-timer 2 nil #'sc--user-is-idle)))
  (setq sc--timer (run-with-timer 60 nil #'sc--user-is-active)))

(defun sc--user-is-active ()
  ;; Explanation: We keep updating this variable as long as the user is active,
  ;; expecting to stop updating once they go idle.
  (setq sc-idle-beginning (ts-now))
  (sc--save-variables-to-disk)
  (sc--start-next-timer))

(defun sc--user-is-idle ()
  (if (sc-idle-p)
      (sc--start-next-timer t)
    (ts-decf (ts-sec sc--idle-beginning) sc-idle-threshold)
    (setq sc-length-of-last-idle (ts-diff (ts-now) sc--idle-beginning))
    (run-hooks 'sc-return-from-idle-hook)
    (sc--start-next-timer)))

(defun sc--start-next-timer (&optional idle)
  (if (or idle (sc-idle-p)) ;; don't re-call `sc-idle-p' if info was provided
      (progn
        (setq sc-idle-watcher nil)
        (setq sc-idle-ticker (run-with-timer 1 nil #'sc--user-is-idle)))
    (setq sc-idle-watcher (run-with-timer 60 nil #'sc--user-is-active))
    (setq sc-idle-ticker nil)))

(defun sc--user-is-active ()
  "This function is meant to be called by `sc--start-next-timer'
repeatedly for as long as the user is active (not idle).

Refresh some variables and sync to disk."
  ;; Explanation: We keep updating this variable as long as the user is active,
  ;; expecting to stop updating once they go idle.
  (setq sc-idle-beginning (ts-now))
  (sc--save-variables-to-disk)
  (sc--start-next-timer))

(defun sc--user-is-idle ()
  "This function is meant to be called by `sc--start-next-timer'
repeatedly for as long as the user is idle.  When the user comes
back, this function will be called one last time, at which point
it sets `sc-length-of-last-idle-in-minutes' and runs
`sc-return-from-idle-hook'. That it has to run once with a
failing condition that normally succeeds is the reason it had to
be a separate function from `sc--user-is-active'."
  (if (sc-idle-p)
      (sc--start-next-timer t)
    (setq sc-length-of-last-idle-in-minutes (sc--calc-last-idle-in-minutes))
    (run-hooks 'sc-return-from-idle-hook)
    (sc--start-next-timer)))

(defsubst sc--calc-last-idle-in-minutes ()
  (+ sc-idle-minutes-threshold
     (round (/ (ts-diff (ts-now) sc-idle-beginning) 60))))

(defun sc--restore-variables-from-disk ()
  (when (f-exists-p sc-idle-beginning-file-name)
    (setq sc-idle-beginning
          (ts-parse (f-read sc-idle-beginning-file-name))))
  (when (f-exists-p sc-mood-alist-file-name)
    (setq sc-mood-alist
          (read (f-read sc-mood-alist-file-name)))))

(defun sc--save-variables-to-disk ()
  (f-write (ts-format sc-idle-beginning) 'utf-8 sc-idle-beginning-file-name)
  (f-write (prin1-to-string sc-mood-alist) 'utf-8 sc-mood-alist-file-name))

;; WIP
(defvar sc-org-capture-templates
  `(
    ("s" "Secretary.el queries and presentations")
    ("sw" "weight" plain (function #'sc-query-weight)
     :immediate-finish t)
    ))

;;;###autoload
(define-minor-mode secretary-mode nil
  :global t
  (if secretary-mode
      (when (cond ((eq system-type 'darwin)
                   (fset #'sc--idle-seconds #'org-mac-idle-seconds))
                  ((and (eq window-system 'x)
                        (executable-find org-clock-x11idle-program-name))
                   (fset #'sc--idle-seconds #'sc--x11-idle-seconds))
                  (t
                   (secretary-mode 0)
                   (message sc-ai-name ": Not able to detect idleness, "
                            "I'll be useless. Disabling secretary-mode.")
                   nil))
        (add-hook 'sc-return-from-idle-hook #'sc-log-idle -90)
        (add-hook 'sc-return-from-idle-hook #'sc-call-from-idle 90)
        (add-hook 'window-buffer-change-functions #'sc-log-buffer)
        (add-hook 'window-selection-change-functions #'sc-log-buffer)
        ;; (add-function :after #'after-focus-change-function #'sc-log-buffer)
        (if after-init-time
            (progn
              (when (-any #'null '(sc-idle-beginning sc-mood-alist))
                (sc--restore-variables-from-disk))
	      ;; (when (null sc--timer)
              ;;   (sc--start-next-timer))
              (when (-all-p #'null '(sc-idle-watcher sc-idle-ticker))
                (sc--start-next-timer))
	      )
          (add-hook 'after-init-hook #'sc--restore-variables-from-disk -1)
          (add-hook 'after-init-hook #'sc--start-next-timer 91)))
    (remove-hook 'sc-return-from-idle-hook #'sc-log-idle)
    (remove-hook 'sc-return-from-idle-hook #'sc-call-from-idle)
    (remove-hook 'window-buffer-change-functions #'sc-log-buffer)
    (remove-hook 'window-selection-change-functions #'sc-log-buffer)
    (remove-hook 'after-init-hook #'sc--restore-variables-from-disk)
    (remove-hook 'after-init-hook #'sc--start-next-timer)
    (unless (null sc--timer)
      (cancel-timer sc--timer)
      (setq sc--timer nil))
    (unless (null sc-idle-watcher)
      (cancel-timer sc-idle-watcher)
      (setq sc-idle-watcher nil))
    (unless (null sc-idle-ticker)
      (cancel-timer sc-idle-ticker)
      (setq sc-idle-ticker nil))))

(provide 'secretary)

;;; secretary.el ends here
