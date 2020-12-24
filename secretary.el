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
(setc sc-ai-name "Mary")
(add-hook 'sc-plot-hook #'sc-plot-weight)

;;; Code:

(require 'f)
(require 'ts)
(require 'dash)
(require 'notifications)
(require 'seq)
(require 'sc-lib)
(require 'sc-data-collector)
(require 'sc-presenter)
(require 'sc-nlp)
(autoload #'org-id-uuid "org-id")

(defcustom sc-location-diary-discrete "/home/kept/Diary" nil)
(defcustom sc-ai-name "Secretary" nil)
(defcustom sc-usrname (if (string= user-full-name "")
                       "sir"
                       (car (s-split " " user-full-name))))
(defcustom sc-usr-short-title "sir")
(defvar sc-last-buffer nil)
(defvar sc-known-buffers nil)
(defvar sc-buffer-focus-log nil)
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
prompt. Merely a convenience; the scores are not forced.")
(defvar sc-dir (expand-file-name "secretary" user-emacs-directory))
(defvar sc-idle-beginning-file-name (expand-file-name "idle-beginning" sc-dir))
(defvar sc-mood-alist-file-name (expand-file-name "sc-mood-alist" sc-dir))
(defvar sc-sit-long 1)
(defvar sc-sit-medium .8)
(defvar sc-sit-short .5)



(defun sc-call (&optional arg)
  "Call your secretary. Useful when you're unsure what to do!"
  (interactive "P")
  (sc-emit "Hello!")
  (sit-for .5)
  (sc-welcome arg))

(defun sc-call-from-idle ()
  "Called by idleness-related hooks, doesn't do much if idle was not long."
  (unwind-protect
      (when (< 90 sc-length-of-last-idle)
        (and (executable-find org-clock-x11idle-program-name) ;; don't spam notification every 5 min
             (not (frame-focus-state))
             (notifications-notify :title "Emacs" :body (sc-greeting)))
        (when (sc-prompt (sc-greeting) " Do you have time for some questions?")
          (sc-welcome t)))
    (setq sc-length-of-last-idle 0)))
;; (run-with-timer 3 nil (lambda ()  (print (frame-focus-state))))
;; (sc-call-from-idle)

(defun sc-call-from-reschedule ()
  (sc-emit "Hello, " sc-usr-short-title ". ")
  (sit-for sc-sit-medium)
  (when (sc-prompt "1 hour ago, you asked to be reminded. Is now a good time?")
    (sc-welcome)))
;;(sc-call-from-reschedule)

;; TODO: Allow date change via `sc-special-handle-current-query'
(defvar sc--date nil)
(defun sc-welcome (&optional idled-p)
  (setq sc--date (ts-now))
  (and idled-p
       (sc-prompt "Have you slept?")
       (sc-query-sleep))
  (unless (sc-logged-today "/home/kept/Self_data/weight.csv")
    (sc-query-weight))
  (and (sc-prompt "Up to reflect?")
       (sc-prompt "Have you learned something?")
       (org-capture nil "s"))
  ;; (if (sc-prompt (concat "How about some flashcards?"))
  ;;     (org-drill))
  (if (sc-prompt "Have you stretched today?")
      nil
    (if (sc-prompt "Do you want reminders for why?")
        nil
      nil))
  (if (sc-prompt "Did you photographe your face today?")
      nil)
  ;; (unless (sc-just-slept)
  (unless (sc-logged-today "/home/kept/Self_data/meditation.csv")
    (sc-query-meditation sc--date))
  (unless (sc-logged-today "/home/kept/Self_data/cold.csv")
    (when (sc-prompt "Have you had a cold shower yet?")
      (sc-query-cold-shower sc--date)))
  (if (sc-prompt "Have you paid for anything since yesterday?")
      (org-capture nil "ln"))
  (if (sc-prompt "Shall I remind you of your life goals? Don't be shy.")
      (view-file "/home/kept/Journal/gtd2.org"))
  (and (> 3 (sc-query-mood "How are you? "))
       (sc-prompt "Do you need to talk?")
       (sc-prompt "Well, I can direct you to my colleague Eliza, though she's not too bright. Will that do?")
       (doctor))
  (and (sc-prompt "Would you like me to suggest an activity?")
       (sc-present-diary (ts-now)))
  (and (-all? #'null (-map #'sc-logged-today (-map #'car sc-csv-alist)))
       (sc-prompt "Shall I come back in an hour?")
       (run-with-timer 3600 nil #'sc-call-from-idle))
  )

(defvar sc-guessed-activity-id nil)

(defun sc-guess-activity ()
  (let ((script (expand-file-name "sc.R" (f-dirname (find-library-name "secretary")))))
    (set-process-sentinel
     (start-process sc-ai-name (sc-buffer-r) "Rscript" script)
     (lambda (_process _event)
       (setq sc-guessed-activity-id
             (with-current-buffer sc-buffer-r
               (goto-char (point-max))
               (buffer-substring (line-beginning-position) (line-end-position))))
       (run-with-timer 30 nil #'sc-guess-activity)))))

;; (run-with-timer 30 nil #'sc-guess-activity)

(defun sc-clock-in-to-guessed-activity ()
  (save-excursion
    (org-id-goto id)
    (org-clock-in)))

;;;###autoload
(defun sc-followup ()
  (interactive)
  (dolist (cell sc-csv-alist)
    (let* ((path (car cell))
           (d (ts-parse (sc-last-date-string-in-date-indexed-csv path)))
           (diff-days (/ (ts-diff (ts-now) d) 60 60 24)))
      (when (< 3 diff-days)
        (sc-prompt "It's been " (number-to-string diff-days)
                   " days since you logged " (file-name-base path)
                   ". Do you want to log it now?")
        (call-interactively (cadr cell))))))

(defvar sc-csv-alist '(("/home/kept/Self_data/weight.csv" sc-query-weight)
                       ("/home/kept/Self_data/mood.csv" sc-query-mood)
                       ("/home/kept/Self_data/ingredients.csv" sc-query-ingredients)
                       ("/home/kept/Self_data/sleep.csv" sc-query-sleep)))

;;;; Handle idling & reboots & crashes

(defvar sc-length-of-last-idle 0
  "Amount of time in minutes, an integer.")
(defvar sc-idle-threshold 10
  "Minutes.")
(defvar sc-idle-ticker nil)
(defvar sc-idle-watcher nil)
(defvar sc-idle-beginning nil)
(defvar sc-return-from-idle-hook nil
  "Note: An Emacs startup also counts as a return from idleness.
You'll probably want your hook to be conditional on some value of
`sc-length-of-last-idle', which at startup is calculated from
last Emacs shutdown or crash (so long as `secretary-mode' was
enabled in the last session).")

(defun sc-react-if-idle-started ()
  (setq sc-idle-beginning (ts-now)) ;; remember the time we went idle
  (sc-save-variables-to-disk)
  (if (sc-idle-p*)
      (setq sc-idle-ticker (run-with-timer 1 nil #'sc-react-if-lost-idle))
    (setq sc-idle-watcher (run-with-timer 60 nil #'sc-react-if-idle-started))))

(defun sc-react-if-lost-idle ()
  (if (sc-idle-p*)
      (setq sc-idle-ticker (run-with-timer 1 nil #'sc-react-if-lost-idle))
    (setq sc-idle-watcher (run-with-timer 60 nil #'sc-react-if-idle-started))
    (setq sc-length-of-last-idle (+ sc-idle-threshold
                                    (round (/ (ts-diff (ts-now) sc-idle-beginning) 60))))
    (run-hooks 'sc-return-from-idle-hook)))

(defun sc-restore-variables-from-disk ()
  (when (f-exists-p sc-idle-beginning-file-name)
    (setq sc-idle-beginning
          (ts-parse (f-read sc-idle-beginning-file-name))))
  (when (f-exists-p sc-mood-alist-file-name)
    (setq sc-mood-alist
          (read (f-read sc-mood-alist-file-name)))))

(defun sc-save-variables-to-disk ()
  (f-write (ts-format sc-idle-beginning) 'utf-8 sc-idle-beginning-file-name)
  (f-write (prin1-to-string sc-mood-alist) 'utf-8 sc-mood-alist-file-name))

;; wip
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
      (if (not (executable-find org-clock-x11idle-program-name))
          (prog1
              (secretary-mode 0)
            (warn "Executable defined in `org-clock-x11idle-program-name' not found, secretary-mode disabled."))
        (add-hook 'sc-return-from-idle-hook #'sc-log-idle -1)
        (add-hook 'sc-return-from-idle-hook #'sc-call-from-idle)
        (add-hook 'window-buffer-change-functions #'sc-log-buffer)
        (add-hook 'window-selection-change-functions #'sc-log-buffer)
        (if after-init-time
            (progn
              (when (-any #'null '(sc-idle-beginning sc-mood-alist))
                (sc-restore-variables-from-disk))
              (sc-react-if-lost-idle))
          (add-hook 'after-init-hook #'sc-restore-variables-from-disk -2)
          (add-hook 'after-init-hook #'sc-react-if-lost-idle -1)))
    (remove-hook 'sc-return-from-idle-hook #'sc-log-idle)
    (remove-hook 'sc-return-from-idle-hook #'sc-call-from-idle)
    (remove-hook 'window-buffer-change-functions #'sc-log-buffer)
    (remove-hook 'window-selection-change-functions #'sc-log-buffer)
    (remove-hook 'after-init-hook #'sc-restore-variables-from-disk)
    (remove-hook 'after-init-hook #'sc-react-if-lost-idle)
    (unless (null sc-idle-watcher)
      (cancel-timer sc-idle-watcher))
    (unless (null sc-idle-ticker)
      (cancel-timer sc-idle-ticker))))

(provide 'secretary)

;;; secretary.el ends here
