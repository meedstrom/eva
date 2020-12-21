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

;;; Code:

(require 'f)
(require 'ts)
(require 'dash)
(require 'notifications)
(require 'seq)
(require 'sc-lib)
(require 'sc-data-collector)
(autoload #'org-id-uuid "org-id")

(defvar sc-dir (expand-file-name "secretary/" user-emacs-directory))
(defvar sc-ai-name nil)
(defvar sc-last-buffer nil)
(defvar sc-known-buffers nil)
(defvar sc-buffer-focus-log nil)
(defvar sc-usrname "sir")
(defvar sc-usr-short-title "sir")
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
(defvar sc-idle-beginning-file-name (expand-file-name "idle-beginning" sc-dir))
(defvar sc-mood-alist-file-name (expand-file-name "sc-mood-alist" sc-dir))

(defun sc-call (&optional arg)
  "Call your secretary. Useful when you're unsure what to do!"
  (interactive "P")
  (sc-emit "Hello!")
  (sit-for 1)
  (sc-query-general arg))

(defun sc-call-noninteractively ()
  "Called by idleness-related hooks, doesn't do much if idle was not long."
  (when (< 30 sc-length-of-last-idle)
    (and (executable-find org-clock-x11idle-program-name) ;; don't spam this every 5 min
         (not (frame-focus-state))
         (notifications-notify :title "Emacs" :body (sc-greeting)))
    (when (sc-prompt (sc-greeting) " Do you have time for some questions?")
      (sc-query-general t)))
  (setq sc-length-of-last-idle 0))
;; (run-with-timer 3 nil (lambda ()  (print (frame-focus-state))))
;;
(defun sc-call-from-idle ()
  "Called by idleness-related hooks, doesn't do much if idle was not long."
  (unless (sc-idle-p)
    (sc-call-noninteractively)))

(defun sc-query-general (&optional idled-p)
  (and idled-p
       (sc-prompt "Have you slept?")
       (sc-query-sleep))
  (unless (sc-logged-today "/home/kept/Self_data/weight.csv")
    (sc-query-weight))
  (if (sc-prompt "Would you like me to suggest an activity?")
      (let* ((candidates (append (--iterate (ts-dec 'month 1 it) (ts-now) 11)
                                 (--iterate (ts-dec 'year 1 it) (ts-now) 10)))
             (found (--keep (sc-existing-diary "/home/kept/Diary" it) candidates)))
        (if (and (-non-nil found)
                 (sc-prompt "Found " (int-to-string (length found))
                            " diaries for this date" ;; (ts-format)
                            " from the past. Want me to open them?"))
            (dolist (x found)
              (view-file x))
          (if (sc-prompt "Search for diaries in the datetree?")
              (sc-indirect-datetree candidates)
            )
          (if (sc-prompt (concat "Up to reflect?"))
              (when (sc-prompt (concat "Have you learned something?"))
                (org-capture nil "s"))
            )
          ;; (if (sc-prompt (concat "How about some flashcards?"))
          ;;     (org-drill)
          ;;   )
          (if (sc-prompt (concat "Have you stretched today?"))
              nil
            (if (sc-prompt (concat "Do you want reminders for why?"))
                nil
              nil))
          (if (sc-prompt (concat "Did you photographe your face today?"))
              nil)
          (if (sc-prompt "Have you paid for anything since yesterday?")
              (org-capture nil "ln"))
          (if (sc-prompt (concat "Shall I remind you of your life goals? Don't be shy."))
              (view-file "/home/kept/Journal/gtd2.org"))
          ))
    (and (> 3 (sc-query-mood "How are you? "))
         (sc-prompt "Do you need to talk?")
         (sc-prompt "Well, I can direct you to my colleague Eliza, "
                    "though she's not too bright. Will that do?")
         (doctor))
    (and (sc-prompt "Shall I come back in an hour?")
         (run-with-timer 3600 nil #'sc-call-noninteractively))))

;;;; Handle idling & reboots & crashes

(defvar sc-length-of-last-idle 0
  "Amount of time in minutes, an integer.")
(defvar sc-idle-threshold (* 60 10))
(defvar sc-idle-ticker nil)
(defvar sc-idle-beginning nil)
(defvar sc-return-from-idle-hook nil)
(defvar sc-idle-watcher nil)
(defvar sc-idlehist-file-name (expand-file-name "idle-history" sc-dir))

;; TODO: write to a csv, not a emacs directory thing
(defun sc-write-idle-csv ()
  (sc-append* "/home/kept/Self_data/idle.csv"
              (number-to-string (ts-unix (ts-now))) "," (number-to-string sc-length-of-last-idle)))


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
    (setq sc-length-of-last-idle (round (/ (ts-diff (ts-now) sc-idle-beginning) 60)))
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

;;;###autoload
(define-minor-mode secretary-mode nil
  :global t
  (if secretary-mode
      (progn
        (add-hook 'sc-return-from-idle-hook #'sc-write-idle-csv -1)
        (add-hook 'sc-return-from-idle-hook #'sc-call-from-idle)
        (add-hook 'after-init-hook #'sc-restore-variables-from-disk -1)
        (add-hook 'after-init-hook #'sc-call-noninteractively)
        (add-hook 'window-buffer-change-functions #'sc-log-buffer)
        (add-hook 'window-selection-change-functions #'sc-log-buffer)
        (setq sc-idle-watcher
              (run-with-timer 60 nil #'sc-react-if-idle-started)))
    (progn
      (remove-hook 'sc-return-from-idle-hook #'sc-write-idle-csv)
      (remove-hook 'sc-return-from-idle-hook #'sc-call-from-idle)
      (remove-hook 'after-init-hook #'sc-restore-variables-from-disk)
      (remove-hook 'after-init-hook #'sc-call-noninteractively)
      (remove-hook 'window-buffer-change-functions #'sc-log-buffer)
      (remove-hook 'window-selection-change-functions #'sc-log-buffer)
      (cancel-timer sc-idle-watcher)
      (unless (null sc-idle-ticker)
        (cancel-timer sc-idle-ticker)))))

(provide 'secretary)

;;; secretary.el ends here
