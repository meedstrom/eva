;;; secretary.el --- Help the user meet goals -*- lexical-binding: t; -*-
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
(require 'a)
(require 'dash)
(require 'named-timer)
(require 'transient)

(defvar secretary-debugp nil)

(defgroup secretary nil "The Emacs in-house secretary."
  :prefix "secretary-"
  :group 'convenience)

(defcustom secretary-location-main-datetree
  "/home/kept/Journal/diary2.org"
  "The file name of your main datetree, if you have one.
Only relevant if you have one you use as a big archive file, see
Info node `(org) Moving subtrees', or you write/capture
everything directly into.  Used by `secretary-present-diary'."
  :group 'secretary
  :type 'string)

(defcustom secretary-ai-name "Val"
  "Your secretary's name."
  :group 'secretary
  :type 'string)
  ;; REVIEW: this? all work fine when loading from custom-file before secretary mode?
  ;; :set (lambda (sym val)
  ;;        (secretary--save-buffer-logs-to-disk)
  ;;        (secretary--save-variables-to-disk)
  ;;        (set-default sym val)))

(defcustom secretary-user-birthday nil
  "Your birthday."
  :group 'secretary
  :type 'string)

(defcustom secretary-user-name
  (if (s-equals? user-full-name "")
      "Mr. Bond"
    (-first-item (s-split " " user-full-name)))
  "Your name, that you prefer to be addressed by."
  :group 'secretary
  :type 'string
  :safe t)

(defcustom secretary-user-short-title "master"
  "A short title for you that works on its own, in lowercase."
  :group 'secretary
  :type 'string
  :safe t)

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


;;; Modes and keys

(defconst secretary-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'secretary-present-next)
    (define-key map (kbd "p") #'secretary-present-previous)
    (define-key map (kbd "p") #'secretary-present-previous)
    (define-key map (kbd "r") #'secretary-resume)
    (define-key map (kbd "+") #'secretary-increment-date)
    (define-key map (kbd "-") #'secretary-decrement-date)
    (define-key map (kbd "d") #'secretary-set-date)
    (define-key map (kbd "t") #'secretary-set-date-today)
    (define-key map (kbd "q") #'bury-buffer)

    ;; Specialties
    (define-key map (kbd "?") #'secretary-dispatch)

    map))

(defconst secretary-present-mode-map
  secretary-chat-mode-map)

(define-derived-mode secretary-chat-mode text-mode "Secretary-Chat"
  :group 'secretary-chat)

(define-derived-mode secretary-present-mode text-mode "Secretary-Present"
  :group 'secretary-present)

(transient-define-prefix secretary-dispatch ()
  ["Actions"
   :if-derived secretary-present-mode
   ("p" "Previous" secretary-present-previous)
   ("n" "Next" secretary-present-next)
   ]
  ["General actions"
   ("q" (lambda ()
          (concat "Quit (same as "
                  (key-description (car (where-is-internal #'keyboard-escape-quit)))
                  ")"))
    keyboard-escape-quit)
   ("l" "View Ledger report" secretary-present-ledger-report)
   ("f" "View finances report" secretary-present-ledger-report)
   ("a" "View Org agenda" org-agenda)
   ;; ("v" "Visit directory of log files" (lambda () (dired secretary-memory-dir)))
   ]
  [;; (lambda () (concat "Date (" (ts-format "%x" secretary--date) ")"))
   "Date"
   ("t" "Reset date to today (default)" secretary-set-date-today)
   ("-" "Decrement the date" secretary-decrement-date)
   ("+" "Increment the date" secretary-increment-date)
   ("d" "Set date..." secretary-set-date)
   ]
  )

(transient-define-prefix secretary-dispatch-pseudocode-wishlist ()
  ["Actions"
   :if query
   ("d" "Disable this query" skip)
   ]
  ["Actions"
   :if-derived secretary-chat-mode
   ("s" "Skip this one" skip)
   ("p" "Skip ahead to presentations (if any)" skip-to-presenters)
   ("g" "Goto a specific prompt" goto)
   ("b" "Go backwards" backwards)
   ]
  ["Actions"
   :if-derived secretary-present-mode
   ("g" "Goto a specific presentation" goto)
   ("p" "Previous" secretary-present-previous)
   ("n" "Next" secretary-present-next)
   ]
  ["General actions"
   ("q" (lambda ()
          (concat "Quit (same as "
                  (key-description (car (where-is-internal #'keyboard-quit)))
                  ")"))
    keyboard-quit)
   ("l" "View Ledger report" secretary-present-ledger-report)
   ("f" "View finances report" secretary-present-ledger-report)
   ("a" "View Org agenda" org-agenda)
   ("v" "Visit directory of log files" (lambda () (dired secretary-memory-dir)))
   ]
  ["Settings"
   ("d" "Set date to operate on..." date)
   ("y" "Set date to yesterday" date-yesterday)
   ("t" "Set date to today (default)" date-today)
   ]
  )


;;; Activity structs
;; Q: What's cl-defstruct?  A: https://nullprogram.com/blog/2018/02/14/

(cl-defstruct (secretary-activity
               (:constructor secretary-activity-create)
               (:copier nil))
  name
  id
  cost-false-pos
  cost-false-neg
  query)

(defvar secretary-activities)

(defun secretary-activity-by-name (name)
  (--find (equal name (secretary-activity-name it)) secretary-activities))

(defun secretary-activities-names ()
  (-map #'secretary-activity-name secretary-activities))


;;; Items
;; Structs of metadata about the user's interactions with the secretary regarding a dataset.

(cl-defstruct (secretary-item
               (:constructor secretary-item-create)
               (:copier nil))
  fn
  dataset
  (dismissals 0)
  max-entries-per-day
  (min-hours-wait 3)
  lookup-posted-time
  (last-called (make-ts :unix 0)) ;; prevent nil-value errors
  )

(defvar secretary-items)

(defun secretary--item-by-fn (fn)
  "Get the item associated with the query function FN."
  (--find (equal fn (secretary-item-fn it)) secretary-items))

(defun secretary--item-by-dataset (path)
  "Get the item associated with the log file at PATH."
  (--find (equal path (secretary-item-dataset it)) secretary-items))

;; NOTE: Do not move the check to secretary--pending-p, since it needs interactivity.
;; TODO: Update :last-called as part of each fn's boilerplate, and/or as part of the secretary-resume loop.
(defun secretary-do-fn-check-dismissals (fn)
  "Call a fn, but react specially if it's been dismissed many times.
Also update :last-called so that `secretary--pending-p' will
work correctly next time."
  (let* ((q (secretary--item-by-fn fn))
         (dismissals (secretary-item-dismissals q)))
    (unless (and (>= dismissals 3)
                 (when (secretary-ynp
                        "You have been dismissing "
                        (symbol-name fn)
                        ", shall I stop tracking it for now?")
                   (secretary-disable-fn fn)
                   t))
      ;; (setf (secretary-item-last-called q) (ts-now))
      (setf (secretary-item-dismissals q) 0)
      (funcall fn secretary--date))))

(defun secretary--pending-p (fn)
  (let* ((q (secretary--item-by-fn fn))
         (last-called (secretary-item-last-called q))
         (dataset (secretary-item-dataset q))
         (max-entries (secretary-item-max-entries-per-day q))
         (lookup-posted-time (secretary-item-lookup-posted-time q))
         (dismissals (secretary-item-dismissals q))
         (min-hrs (secretary-item-min-hours-wait q))
         (min-secs (* 60 60 min-hrs))
         (called-today (when last-called
                         (and (= (ts-day last-called) (ts-day (ts-now)))
                              (> (ts-H last-called) 4))))
         (recently-logged
          (when (and (stringp dataset)
                     (file-exists-p dataset))
            (> min-secs
               (if lookup-posted-time
                   (- (ts-unix (ts-now))
                      (string-to-number (car (secretary--last-in-tsv dataset))))
                 (ts-diff (ts-now)
                          (ts-parse (secretary-last-timestamp-in-tsv dataset)))))))
         ;; Even if we didn't log yet, we don't quite want to be that persistent
         (recently-called (> (ts-diff (ts-now) last-called)
                             ;; hours multiplied by n dismissals
                             (* dismissals 60 60))))
    (unless recently-logged
      (when (or (not called-today)
                (not (file-exists-p dataset))
                (null max-entries)
                (> max-entries (length (secretary--get-entries-in-tsv dataset))))
        (unless recently-called
          t)))))

;;(secretary--pending-p #'secretary-fn-weight)
;;(secretary--pending-p #'secretary-fn-sleep)
;;(secretary--pending-p #'secretary-fn-mood)

(defun secretary-disable-item (item)
  (f-write (prin1-to-string (remove item (secretary--enabled-items)))
           'utf-8
           (secretary-disabled-items-file-name)))

(defun secretary-disabled-items-file-name ()
  "Path to file holding list of disabled queries.
Needed to persist disablings across restarts."
  (expand-file-name "disabled-items.el" secretary-memory-dir))

(defun secretary--enabled-items ()
  (let ((all (-map #'secretary-item-fn secretary-items))
        (disabled (when (f-exists-p (secretary-disabled-items-file-name))
                    (read (f-read (secretary-disabled-items-file-name))))))
    (-difference all disabled)))


;;; Library

;; REVIEW: see that there's no problem if you delete secretary-memory-dir
(defvar secretary-mood-alist nil
  "For suggesting a score in the `secretary-log-mood' prompt.
Merely a convenience for auto-completion.

The variable populates itself through use, and syncs with a file
at `secretary-mood-alist-file-name'.")

(defvar secretary-aphorisms)

(defvar secretary--date
  (ts-now)
  "Date to which to apply the current fn.
Can be set anytime during a welcome to override the date to which
some queries apply, for example to log something for yesterday.
This may not apply, check the source for the welcomer you are
using.")

(defun secretary--buffer-r ()
  (get-buffer-create (concat "*" secretary-ai-name ": R*")))

;; (defun secretary-buffer-chat ()
;;   (or (find-buffer-visiting secretary-chat-log-file-name)
;;       (let ((buf (find-file-noselect secretary-chat-log-file-name)))
;;         (with-current-buffer buf
;;           (secretary-chat-mode)
;;           (setq-local buffer-read-only t)
;;           (setq-local auto-save-visited-mode nil)
;;           (setq-local require-final-newline nil)
;;           (buffer-disable-undo)
;;           (whitespace-cleanup)
;;           (visual-line-mode)
;;           (rename-buffer (concat "*" secretary-ai-name ": chat log*")))
;;         buf)))

(defun secretary-buffer-chat ()
  (or (get-buffer (concat "*" secretary-ai-name ": chat log*"))
      (let ((buf (get-buffer-create (concat "*" secretary-ai-name ": chat log*"))))
        (with-current-buffer buf
          (secretary-chat-mode)
          (setq-local auto-save-visited-mode nil)
          (setq-local require-final-newline nil)
          (buffer-disable-undo)
          ;;(whitespace-cleanup)
          (visual-line-mode)
          (and secretary-chat-log-file-name
               (file-exists-p secretary-chat-log-file-name)
               (insert-file-contents secretary-chat-log-file-name))
          (setq-local buffer-read-only t))
        buf)))

(defun secretary--save-chat-log-to-disk ()
  (with-current-buffer (secretary-buffer-chat)
    (whitespace-cleanup)
    (save-buffer)))

(defvar secretary--k nil)
(defun secretary--y-or-n-p-insert-k ()
  "Mostly like `y-or-n-p-insert-y'."
  (interactive)
  (delete-minibuffer-contents)
  (insert "y")
  (setq secretary--k t)
  (exit-minibuffer))

(defun secretary-ynp (&rest strings)
  "Wrapper around `y-or-n-p' for secretary-chat-mode."
  (let* (;; (default-y-or-n-p-map y-or-n-p-map)
         ;; (default-cmd (lookup-key y-or-n-p-map (kbd "k")))
         ;; TODO: Also show which log file we're applying to
         (background-info (concat "[Applying to date: " (ts-format "%Y-%B-%d" secretary--date) "]\n"))
         (prompt (string-join strings)))
    (unwind-protect
        (progn
          (switch-to-buffer (secretary-buffer-chat))
          (secretary-emit prompt)
          (define-key y-or-n-p-map (kbd "o") #'secretary-special-handle-current-query)
          (define-key y-or-n-p-map (kbd "i") #'secretary-special-handle-current-query)
          (define-key y-or-n-p-map (kbd "<SPC>") #'secretary-special-handle-current-query)
          (define-key y-or-n-p-map (kbd "k") #'secretary--y-or-n-p-insert-k)
          (setq-local buffer-read-only nil)
          (let ((result (y-or-n-p (concat background-info prompt))))
            (with-silent-modifications
              (if secretary--k
                  (progn
                    (setq secretary--k nil)
                    (secretary-emit-same-line " Okay..."))
                (if result
                    (secretary-emit-same-line " Yes.")
                  (secretary-emit-same-line " No."))))
            result))
      (setq-local buffer-read-only t)
      (dolist (x '("o" "i" "k" "<SPC>"))
        (define-key y-or-n-p-map (kbd x) #'y-or-n-p-insert-other)))))

(defun secretary--after-cancel-do-things ()
  (advice-remove 'abort-recursive-edit #'secretary--after-cancel-do-things) ;; needed
  (cl-incf (secretary-item-dismissals
            (secretary--item-by-fn secretary--current-fn)))
  ;; nonessential, but could prevent confusion sometime
  (setq secretary--current-fn nil))

(defun secretary-read (prompt &optional collection default)
  (secretary-emit prompt)
  (let* ((background-info (concat "[Applying to date: "
                                  (ts-format "%Y %b %d" secretary--date) "]\n"
                                  ))
         (extra-collection '("/skip"))
         (result (completing-read
                  (concat background-info
                          (ts-format "[%H:%M] ")
                          prompt
                          (when (stringp default)
                            " (default " default "): "))
                  (append collection extra-collection)
                  nil nil nil nil
                  (when (stringp default)
                    default))))
    (secretary-emit-same-line result)
    (if (string-match-p "skip" result)
        (progn
          (if (and (< 1 (length secretary--queue))
                   (member secretary--current-fn secretary--queue))
              ;; Try to proceed to next item
              (progn
                (setq secretary--queue
                      (remove secretary--current-fn secretary--queue))
                (secretary-resume))
            ;; Just cancel the session
            (abort-recursive-edit)))
      result)))

(defcustom secretary-chime-audio-file
  (expand-file-name
   ;; From https://freesound.org/people/josepharaoh99/sounds/380482/
   "assets/Chime Notification-380482.wav"
   ;; From https://bigsoundbank.com/detail-0319-knock-on-a-glass-door-1.html
   ;; "assets/DOORKnck_Knock on a glass door 1 (ID 0319)_BSB.wav"
   (f-dirname (find-library-name "secretary")))
  "Sound to play when a welcomer is triggered unannounced."
  :group 'secretary
  :type 'string)

(defcustom secretary-play-sounds-p nil
  "Whether to play sounds.")

(defun secretary--chime-aural ()
  (and secretary-play-sounds-p
       (executable-find "aplay")
       (file-exists-p secretary-chime-audio-file)
       (start-process "aplay" nil "aplay" secretary-chime-audio-file)))

;; TODO: Determine step length from secretary-sit-long.
(defun secretary--chime-visual ()
  (let ((colors '((.1 . "green")
                  (.2 . "#aca")
                  (.3 . "#7a7")
                  (.4 . "#696")
                  (.5 . "#363"))))
    (let ((orig (face-background 'fringe)))
      (dolist (x colors)
        (run-with-timer (car x) nil #'set-face-background 'fringe (cdr x)))
      (run-with-timer .6 nil #'set-face-background 'fringe orig))

    (when (facep 'solaire-fringe-face)
      (let ((orig (face-background 'solaire-fringe-face)))
        (dolist (x colors)
          (run-with-timer (car x) nil #'set-face-background 'solaire-fringe-face (cdr x)))
        (run-with-timer .6 nil #'set-face-background 'solaire-fringe-face orig)))
    nil))

(defun secretary-emit (&rest strings)
  "Write a line to the chat buffer, made from STRINGS.
Returns a string appropriate for passing to `message'."
  (let ((new-date-maybe (if (/= (ts-day (ts-now))
                                (ts-day secretary--last-chatted))
                            (concat "\n\n" (ts-format "%A, %e %B %Y") (secretary--holiday-maybe) "\n")
                          ""))
        (msg (concat "\n[" (ts-format "%H:%M") "] " (string-join strings))))
    (with-current-buffer (secretary-buffer-chat)
      (goto-char (point-max))
      (with-silent-modifications
        (delete-blank-lines)
        (insert new-date-maybe)
        (insert msg))))
  (setq secretary--last-chatted (ts-now))
  (string-join strings))

(defun secretary-emit-same-line (&rest strings)
  (let ((msg (string-join strings)))
    (with-current-buffer (secretary-buffer-chat)
      (goto-char (point-max))
      (with-silent-modifications
        (insert msg))))
  (setq secretary--last-chatted (ts-now)))

(defvar secretary--last-chatted
  (make-ts :unix 0)
  "Timestamp updated whenever the chat is written to.")

(defun secretary--holiday-maybe ()
  (require 'calendar)
  (require 'holidays)
  (if-let (foo (calendar-check-holidays (calendar-current-date)))
      (concat " -- " (s-join " " foo))
    ""))

(defun secretary--random-p (&rest _args)
  "Return t or nil, at random.
Can work as a predicate for `cl-sort'."
  (> 0 (random)))

(defun secretary-write-safely (text path)
  "Write TEXT to file at PATH if the content differs.
Also revert any buffer visiting it, or signal an error if there
are unsaved changes."
  (let ((buf (find-buffer-visiting path)))
    (and buf
         (buffer-modified-p buf)
         (error "Unsaved changes in open buffer: " (buffer-name buf)))
    (unless (and (f-exists-p path)
                 (string= text (f-read path 'utf-8)))
      (f-write text 'utf-8 path)
      (and buf (with-current-buffer buf (revert-buffer))))))

(defun secretary-append-safely (text path)
  "Append TEXT to file at PATH if the content differs.
Also revert any buffer visiting it, or signal an error if there
are unsaved changes."
  (let ((buf (find-buffer-visiting path)))
    (and buf
         (buffer-modified-p buf)
         (error "Unsaved changes in open buffer: " (buffer-name buf)))
    (unless (and (f-exists-p path)
                 (= 0 (length text))) ;; no unnecessary disk writes
      (f-append text 'utf-8 path)
      (and buf (with-current-buffer buf (revert-buffer))))))

(defun secretary--transact-buffer-onto-file (buffer path)
  "Append contents of BUFFER to file at PATH, emptying BUFFER."
  (with-current-buffer buffer
    (whitespace-cleanup)
    (secretary-append-safely (buffer-string) path)
    (delete-region (point-min) (point-max))))

(defmacro secretary--process-output-to-string (program &rest args)
  "Similar to `shell-command-to-string', but skips the shell
intermediary so you don't need a /bin/sh. PROGRAM and ARGS are
passed on to `call-process'."
  (declare (debug (&rest form)))
  `(with-temp-buffer
     (call-process ,program nil (current-buffer) nil ,@args)
     (buffer-string)))

(defmacro secretary--process-output-to-number (program &rest args)
  (declare (debug (&rest form)))
  `(string-to-number (secretary--process-output-to-string ,program ,@args)))

(defcustom secretary-x11idle-program-name "x11idle"
  nil
  :group 'secretary
  :type 'string)

(defcustom secretary-fallback-to-emacs-idle-p nil
  "Track Emacs idle rather than turn off under unknown OS/DE.
Not recommended.")

(defvar secretary--idle-seconds-fn nil)

(defun secretary--idle-seconds ()
  "Number of seconds user has been idle, as told by the system."
  (funcall secretary--idle-seconds-fn))

;; DEPRECATED: poor name-funcitonality match anyway
(defun secretary-idle-p ()
  (let ((user-idle-or-secretary-offline-seconds (max (secretary--idle-seconds)
                                                     (ts-diff (ts-now) secretary--last-online))))
    (> user-idle-or-secretary-offline-seconds secretary-idle-threshold-secs-short)))

(defun secretary-idle-p ()
  (> (secretary--idle-seconds) secretary-idle-threshold-secs-short))

(defun secretary--x11-idle-seconds ()
  "Like `org-x11-idle-seconds' without need for /bin/sh or org."
  (/ (secretary--process-output-to-number secretary-x11idle-program-name) 1000))

(defun secretary--gnome-idle-seconds ()
  "Check Mutter's idea of idle time, even on Wayland."
  ;; https://unix.stackexchange.com/questions/396911/how-can-i-tell-if-a-user-is-idle-in-wayland
  (let ((idle-ms
         (string-to-number
          (car (s-match (rx space (* digit) eol)
                        (secretary--process-output-to-string
                         "dbus-send"
                         "--print-reply"
                         "--dest=org.gnome.Mutter.IdleMonitor"
                         "/org/gnome/Mutter/IdleMonitor/Core"
                         "org.gnome.Mutter.IdleMonitor.GetIdletime"))))))
    (/ idle-ms 1000)))

(defun secretary--string-contains-number (input)
  (s-matches-p (rx num) input))

(defun secretary--coerce-to-hh-mm (input)
  "Coerce from inputs matching HH:MM, HH or H, to HH:MM (24-h).
If \"am\" or \"pm\" present, assume input is in 12-hour clock."
  (unless (s-matches-p (rx num) input)
    (error "%s" (concat "Invalid time: " input)))
  (let* ((hhmm (or (cdr (s-match (rx (group (= 2 num)) punct (group (= 2 num))) input))
                   (cdr (s-match (rx (group (= 1 num)) punct (group (= 2 num))) input))
                   (s-match (rx (= 2 num)) input)
                   (s-match (rx (= 1 num)) input)))
         (hour (string-to-number (car hhmm)))
         (minute (string-to-number (or (cadr hhmm) "00"))))
    (when (or (> hour 24)
              (and (> hour 12)
                   (s-matches-p (rx (or "pm" "am")) input)))
      (error "%s" (concat "Invalid time: " input)))
    (when (and (s-contains-p "pm" input)
               (/= 12 hour))
      (cl-incf hour 12))
    (when (and (s-contains-p "am" input)
               (= 12 hour))
      (setq hour 0))
    (when (= 24 hour)
      (setq hour 23)
      (setq minute 59))
    (concat (when (< hour 10) "0")
            (number-to-string hour) ":"
            (when (< minute 10) "0")
            (number-to-string minute))))

(defmacro secretary--run-async (program &rest args)
  "Wrapper for `start-process' with fewer arguments."
  `(start-process ,program (secretary--debug-buf) ,program ,@args))

(defmacro secretary--run (program &rest args)
  "Wrapper for `call-process' with fewer arguments."
  `(call-process ,program nil (secretary--debug-buf) nil ,@args))

(defun secretary--debug-buf ()
  (when secretary-debugp (get-buffer-create (concat secretary-ai-name "*Process Output*"))))


;;;; Commands

(defun secretary-decrement-date ()
  (interactive)
  (secretary-set-date (ts-dec 'day 1 secretary--date)))

(defun secretary-increment-date ()
  (interactive)
  (secretary-set-date (ts-inc 'day 1 secretary--date)))

(defun secretary-set-date-today ()
  (interactive)
  (secretary-set-date (ts-now)))

(defun secretary-set-date (&optional ts)
  (interactive)
  (if ts
      (setq secretary--date ts)
    (let* ((time (ts-format "%T"))
           (new-date (org-read-date))
           (new-datetime (ts-parse (concat new-date " " time))))
      (setq secretary--date new-datetime)))
  (secretary-emit "Operating as if the date is " (ts-format "%x" secretary--date) "."))

(defun secretary-reschedule ()
  (run-with-timer 3600 nil #'secretary-call-from-reschedule))


;;;; Library for handling datasets

(defun secretary-last-datestamp-in-file (path)
  "Get the last match of YYYY-MM-DD in PATH.
Beware that if PATH has instances of such where you don't expect
it (in additional columns), you might not get the datestamp you
meant to get."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (goto-char (point-max))
    (re-search-backward (rx (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)))
    (buffer-substring (point) (+ 10 (point)))))

(defun secretary-last-timestamp-in-tsv (path)
  "In file at PATH, get the second field of last row."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-max))
    (when (looking-back "^" nil) ;; if trailing newline
      (forward-line -1))
    (goto-char (line-beginning-position))
    (search-forward "\t")
    (buffer-substring (point) (- (search-forward "\t") 1))))

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

(defun secretary--last-in-tsv (path)
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-max))
    (when (looking-back "^" nil) ;; if empty line
      (forward-line -1))
    (split-string (buffer-substring (line-beginning-position)
                                    (line-end-position))
                  "\t")))

(defun secretary-get-first-today-line-in-file (path &optional ts)
  (with-temp-buffer
    (insert-file-contents path)
    (search-forward (ts-format "%F" ts))
    (buffer-substring (line-beginning-position) (line-end-position))))
;; (secretary-get-first-today-line-in-file "/home/kept/Self_data/ingredients.csv")

(defun secretary-last-value-in-tsv (path)
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents-literally path)
      (goto-char (point-max))
      (search-backward "\t")
      (forward-char)
      (buffer-substring (point) (line-end-position)))))

;; WONTFIX: check for recent activity (if user awake thru the night)
(defun secretary-logged-today (path)
  (when (file-exists-p path)
    ;; don't act like it's a new day if the time is <5am.
    (let ((day (if (> 5 (ts-hour (ts-now)))
                   (ts-dec 'day 1 (ts-now))
                 (ts-now))))
      (with-temp-buffer
        (insert-file-contents-literally path)
        (ignore-errors (search-forward (ts-format "%F" day)))))))

(defun secretary-append-tsv (path &rest fields)
  "Append a line to the file located at PATH.
Create the file and its parent directories if it doesn't exist,
and make sure the line begins on a newline.  Treat each argument
in FIELDS... as a separate data field, inserting a tab character
in between, and warn if a field contains a tab character.

For database purposes (which you may not need), FIELDS is
prepended with a field for the Unix timestamp representing right
now. If timestamps are an actual variable you want to track, add
a separate field containing something like the output
of `(ts-format secretary--date)'."
  (declare (indent defun))
  (unless (file-exists-p path)
    (make-empty-file path t))
  (let* ((fields (-replace nil "" fields))
         (newline-maybe (if (s-ends-with-p "\n" (f-read-bytes path))
                            ""
                          "\n"))
         (errors-path (concat path "_errors"))
         (posted (s-pad-right 18 "0" (number-to-string (ts-unix (ts-now)))))
         (text (string-join fields "\t"))
         (new-text (concat newline-maybe posted "\t" text))
         (maybe-buf (find-buffer-visiting path)))
    (cond
     ;; TODO: superfluous clause
     ((and maybe-buf (buffer-modified-p maybe-buf))
      (warn "Cancelled write because of unsaved open buffer at %s, wrote to %s" path errors-path)
      (f-append new-text 'utf-8 errors-path)
      nil)
     ((--any-p (s-contains-p "\t" it) fields)
      (warn "Entry had tabs inside fields, wrote to %s" errors-path)
      (f-append new-text 'utf-8 errors-path)
      nil)
     ((s-contains-p "\n" text)
      (warn "Entry had newlines, wrote to %s" errors-path)
      (f-append new-text 'utf-8 errors-path)
      nil)
     (t
      (secretary-append-safely new-text path)
      t))))


;;;; Greeting messages

(defvar secretary-greetings
  '((concat "Welcome back, Master.")
    (concat "Nice to see you again, " secretary-user-name ".")
    (concat "Greetings, " secretary-user-name "."))
  "Greeting phrases which can initiate a conversation.")

(defun secretary-greeting-curt ()
  "Return a greeting appropriate in the midst of a workday.
Because if you've already exchanged good mornings, it's weird to
do so again."
  (seq-random-elt `("Hello" "Hi" "Hey")))

(defun secretary-greeting ()
  "Return a greeting string."
  (let ((bday (ts-parse secretary-user-birthday)))
    (cond ((ts-in bday bday (ts-now))
           (concat "Happy birthday, " secretary-user-name "."))
          ;; If it's morning, always use a variant of "good morning"
          ((> 10 (ts-hour (ts-now)) 5)
           (eval (seq-random-elt (secretary--daytime-appropriate-greetings))
                 t))
          (t
           (eval (seq-random-elt (append secretary-greetings
                                         (-list (secretary--daytime-appropriate-greetings))))
                 t)))))

;; NOTE: I considered making external variables for morning, day and evening
;;       lists, but users might also want to change the daytime boundaries or
;;       even add new boundaries. Too many possibilities, this is a case where
;;       it's ok to make the user override the defun as a primary means of
;;       customization.
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


;;;; Buffer logger

(defvar secretary-last-buffer nil)

(defvar secretary-known-buffers nil)

(defun secretary-buffer-focus-log-buffer ()
  (get-buffer-create
   (concat (unless secretary-debugp " ")
           "*" secretary-ai-name ": Buffer focus log*")))

(defun secretary-buffer-existence-log-buffer ()
  (get-buffer-create
   (concat (unless secretary-debugp " ")
           "*" secretary-ai-name ": Buffer existence log*")))

(defun secretary--save-buffer-logs-to-disk ()
  (secretary--transact-buffer-onto-file (secretary-buffer-focus-log-buffer)
                                 "/home/kept/Self_data/buffer-focus.tsv")
  (secretary--transact-buffer-onto-file (secretary-buffer-existence-log-buffer)
                                 "/home/kept/Self_data/buffer-existence.tsv"))

;; TODO: When buffer major mode changes, count it as a new buffer. Note that
;; (assoc buf secretary-known-buffers) will still work.
;; TODO: When eww url changes, count it as a new buffer
;; TODO: When counting it as a new buffer, record a field for "previous uuid" just in case data analyst wants to merge these observations
;; TODO: Optimize (esp. cranking out the focus log)
(defun secretary-log-buffer (&optional _arg)
  "Log the buffer just switched to.
Put this on `window-buffer-change-functions' and
`window-selection-change-functions'."
  (unless (minibufferp)
    (autoload #'org-id-uuid "org-id")
    (let* ((buf (current-buffer))
           (mode (symbol-name (buffer-local-value 'major-mode buf)))
           (known (assoc buf secretary-known-buffers))
           (timestamp (s-pad-right 18 "0" (number-to-string (ts-unix (ts-now)))))
           ;; TODO: use this
           (eww-url (when (eq buf "eww-mode")
                      (eww-current-url)))
           (exist-record (unless (and known
                                      (string= mode (nth 4 known))) ;; doesnt do it
                           (list buf
                                 (org-id-uuid)
                                 (buffer-name buf)
                                 (buffer-file-name buf)
                                 mode
                                 timestamp ;; time the buffer was first opened
                                 )))
           (focus-record (list timestamp ;; time the buffer was switched to
                               (if known (cadr known) (cadr exist-record)) ;; uuid
                               )))
      (unless (eq secretary-last-buffer buf) ;; you only entered and left minibuffer e.g.
        (setq secretary-last-buffer buf)
        (unless known
          (push exist-record secretary-known-buffers)
          (with-current-buffer (secretary-buffer-existence-log-buffer)
            (goto-char (point-max))
            (insert "\n" (string-join (cdr exist-record) "\t"))))
        (with-current-buffer (secretary-buffer-focus-log-buffer)
          (goto-char (point-max))
          (insert "\n" (string-join focus-record "\t")))))))


;;; Queries

(defvar secretary--queue nil)

;; NOTE: update the test in secretary-tests.el with every change.
(defmacro secretary-defun (name arglist &optional docstring &rest body)
  "Boilerplate wrapper for `defun'.
To see what it expands to, try something like

    (macroexpand '(secretary-defun foo (x1 x2) (frobnicate)))

Or better, visit secretary-tests.el to read the tests of this macro.

Manages the external variables `secretary--current-fn' and
`secretary--queue'. If you use a simple `defun' in lieu of this
wrapper, you must set these!

In BODY, you have access to extra temporary variables:
- \"interactivep\" which is more reliable than the function `called-interactively-p'.
- \"this-dataset\" which is a reference to (secretary-item-dataset (secretary--item-by-fn secretary--current-fn))."
  (declare (indent defun) (doc-string 3))
  (unless (stringp docstring)
    (push docstring body))
  (let* ((user-spec (and (eq 'interactive (car-safe (car body)))
                                           (car-safe (cdr (car body)))))
         (user-spec-length (when user-spec
                             (length
                              (split-string user-spec "\n"))))
         (new-spec (if user-spec
                       `(interactive
                         ,(concat user-spec "\n"
                                  (cl-reduce #'concat
                                             (make-list (- (length arglist)
                                                           user-spec-length)
                                                        "i\n"))
                                  "p"))
                     `(interactive
                       ,(concat (cl-reduce #'concat
                                           (make-list (length arglist)
                                                      "i\n"))
                                "p"))))
         (new-body (if user-spec
                       (cdr body)
                     (cons (car body) (cdr body)))))
    `(defun ,name ,(-snoc arglist 'interactivep)
       ,@(if (stringp docstring)
             (list docstring
                   new-spec)
           (list new-spec))
       (setq secretary--current-fn #',name)
       (unless (secretary--item-by-fn secretary--current-fn)
         (error "%s not listed in secretary-items" (symbol-name secretary--current-fn)))
       (advice-add 'abort-recursive-edit :before #'secretary--after-cancel-do-things)
       (let ((this-dataset (secretary-item-dataset
                            (secretary--item-by-fn secretary--current-fn))))
         (unwind-protect
             (prog1 (progn ,@new-body)
               (setq secretary--queue
                     (remove secretary--current-fn secretary--queue))
               (setf (secretary-item-dismissals
                      (secretary--item-by-fn secretary--current-fn))
                     0))
           (advice-remove 'abort-recursive-edit #'secretary--after-cancel-do-things))))))

;;;###autoload
(secretary-defun secretary-query-ingredients ()
  (let* ((prompt "Comma-separated list of ingredients: ")
         (response (progn
                     (secretary-emit prompt)
                     (read-string prompt))))
    (secretary-append-tsv this-dataset
      (ts-format secretary--date)
      response)
    (secretary-emit-same-line
     (mapconcat #'-last-item (secretary--get-entries-in-tsv this-dataset) ", "))))

;;;###autoload
(secretary-defun secretary-query-activity ()
  (let* ((name (secretary-read "What are you up to? " (secretary-activities-names))))
    (secretary-append-tsv this-dataset
      (ts-format secretary--date) ;; the time the activity happened
      name
      (secretary-activity-id (secretary-activity-by-name name)))
    (secretary-emit-same-line name)))

;;;###autoload
(secretary-defun secretary-query-mood ()
  (let* ((mood-desc (secretary-read
                     (or prompt "Your mood: ")
                     (cl-sort (mapcar #'car secretary-mood-alist)
                              #'secretary--random-p)))
         (old-score (cdr (assoc mood-desc secretary-mood-alist)))
         (prompt-for-score
          (concat "Score from 1 to 5"
                  (when old-score " (default " old-score ")")
                  ": "))
         (score (progn (secretary-emit "Score from 1 to 5: ")
                       (read-string prompt-for-score nil nil old-score)))
         (score-num (string-to-number score)))
    (secretary-append-tsv this-dataset
      (ts-format)
      (s-replace "," "." score)
      mood-desc)
    (secretary-emit-same-line (nth 2 (secretary--last-in-tsv this-dataset)))
    ;; Update secretary-mood-alist.
    (if (assoc mood-desc secretary-mood-alist)
        (setq secretary-mood-alist
              (--replace-where (string= (car it) mood-desc)
                               (cons (car it) score)
                               secretary-mood-alist))
      (push (cons mood-desc score) secretary-mood-alist))))

;;;###autoload
(secretary-defun secretary-query-weight ()
  (let* ((last-wt (secretary-last-value-in-tsv this-dataset))
         (wt (secretary-read "What do you weigh today? "
                    `(,last-wt
                      "I don't know")
                    last-wt)))
    (if (= 0 (string-to-number wt)) ;; user typed a string with characters other than num and whitespace
        (secretary-emit "Ok, I'll ask you again later.")
      (secretary-append-tsv this-dataset
        (ts-format secretary--date)
        (s-replace "," "." wt))
      (secretary-emit "Weight today: " (secretary-last-value-in-tsv path) " kg"))))

(defun secretary-check-yesterday-sleep ()
  (let* ((dataset (secretary-item-dataset (secretary--item-by-fn secretary--current-fn)))
         (today-rows (secretary--get-entries-in-tsv dataset (ts-dec 'day 1 secretary--date)))
         (total-yesterday (-sum (--map (string-to-number (nth 3 it)) today-rows))))
    ;; Totalling less than 4 hours is unusual, implying possible anomaly in data.
    (if (> (* 60 4) total-yesterday)
        (if (secretary-ynp "Yesterday, you slept "
                    (number-to-string (round (/ total-yesterday 60.0)))
                    " hours, is this about right?")
            nil
          (secretary-emit "You may edit the history at "
                   dataset
                   ". For now, let's talk about today.")
          (sit-for secretary-sit-short)))))

;; TODO: (Feature) Look at when idle ended to suggest a response.
;; TODO: (Feature) Let user say "since 5" instead of quantity-art
;; TODO: Fix the case where someone wakes up at 23:00 but replies to the query
;;       at 01:00. Notice the unusual hour change and ask if user meant 23
;;       yesterday.
;;;###autoload
(secretary-defun secretary-query-sleep ()
  "Query you for wake-up time and sleep quantity for one sleep block today.
You are free to decline either query, but you should not later
register sleep quantity from this same block in order to \"get
the totals correct\" -- the database will interpret it as a
different sleep block and continue to count the original one as
having a censored (nonzero!) quantity of sleep on top of what you
add."
  ;; TODO: don't pester about an anomalous date more than once
  (secretary-check-yesterday-sleep)
  (let* ((recently-hhmm (ts-format "%H:%M" (ts-dec 'minute 10 secretary--date)))
         (recently-response (concat "Recently (" recently-hhmm ")"))
         (wakeup-time
          (let* ((reply (secretary-read "When did you wake? "
                                 `("I don't know"
                                   ,recently-response))))
            (cond ((equal reply recently-response)
                   recently-hhmm)
                  ((secretary--string-contains-number reply)
                   (secretary--coerce-to-hh-mm reply))
                  (t nil))))
         (sleep-minutes
          (secretary-parse-time-amount
           (secretary-read "How long did you sleep? "
                    `("I don't know"
                      ,(number-to-string
                        (/ secretary-length-of-last-idle 60 60)))))))
    (secretary-emit (when wakeup-time
             (concat "You woke at " wakeup-time ". "))
           (when sleep-minutes
             (concat "You slept " (number-to-string sleep-minutes)
                     " minutes (" (number-to-string (/ sleep-minutes 60.0))
                     " hours)."))
           (when (-all-p #'null '(wakeup-time sleep-minutes))
             (concat "One sleep block recorded without metrics.")))
    (secretary-append-tsv this-dataset
      (ts-format "%F" secretary--date) ;; date (no time component)
      wakeup-time ;; time (optional)
      (when sleep-minutes (number-to-string sleep-minutes)))))

;;;###autoload
(secretary-defun secretary-query-meditation ()
  (when (secretary-ynp "Did you meditate today?")
    (let* ((mins (read-string "Do you know how long (in minutes)? "))
           (cleaned-mins (number-to-string (string-to-number mins)))) ;; ugly, I know
      (secretary-append-tsv this-dataset
        (ts-format)
        "TRUE"
        (unless (string= "0" cleaned-mins) cleaned-mins)))))

;;;###autoload
(secretary-defun secretary-query-cold-shower ()
  (let ((rating (read-string "Cold rating? ")))
    (secretary-append-tsv this-dataset
      (ts-format secretary--date)
      rating)))


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


;;;; Welcomers

(defvar secretary--buffer-predicate-backup)

;; TODO: Name it better
(defun secretary-resume (&optional queue)
  "Run through `secretary--queue'."
  (interactive)
  (display-buffer (secretary-buffer-chat))
  (dolist (f (or queue secretary--queue))
    (unwind-protect
        (secretary-do-fn-check-dismissals f)
      (setf (secretary-item-last-called (secretary--item-by-fn f))
            (ts-now)))))

(defun secretary-execute (&optional queue)
  "Run through `secretary--queue'."
  (interactive)
  (setq secretary--buffer-predicate-backup (frame-parameter nil 'buffer-predicate))
  (save-window-excursion
    (unwind-protect
        (progn
          (set-frame-parameter nil 'buffer-predicate nil)
          (display-buffer (secretary-buffer-chat))
          (dolist (f (or queue secretary--queue))
            (unwind-protect
                (secretary-do-fn-check-dismissals f)
              (setf (secretary-item-last-called (secretary--item-by-fn f))
                    (ts-now)))))
      (set-frame-parameter nil 'buffer-predicate secretary--buffer-predicate-backup))))

(defalias 'secretary-resume #'secretary-execute)

(defconst secretary-debug-no-timid t)

(defun secretary--call-timidly ()
  "Butt-in if any queries are pending."
  (setq secretary--date (ts-now))
  (when-let ((fns (if secretary-debug-no-timid
                      (secretary--enabled-items)
                    (-filter #'secretary--pending-p (secretary--enabled-items)))))
    (setq secretary--queue fns)
    (unless (eq t (frame-focus-state))
      (require 'notifications)
      (notifications-notify :title secretary-ai-name :body (secretary-greeting)))
    (secretary--chime-aural)
    (secretary--chime-visual)
    (run-with-timer 1 nil #'secretary-resume)))
;; (secretary--call-timidly)
;; (named-timer-run :secretary-attempt (* 60 60) (* 60 60) #'secretary--call-timidly)

(defun secretary-new-session ()
  (interactive)
  (setq secretary--date (ts-now))
  (setq secretary--queue (-filter #'secretary--pending-p (secretary--enabled-items)))
  (secretary-build-presentations-async)
  (secretary-resume)
  (secretary-view-presentations)
  )

(defun secretary-new-session-force-all ()
  (interactive)
  (setq secretary--date (ts-now))
  (setq secretary--queue (secretary--enabled-items))
  (secretary-build-presentations-async)
  (secretary-resume)
  (secretary-view-presentations))

(defun secretary-call-from-idle ()
  "Called by idleness-related hooks, doesn't do much if idle was not long."
  (unless (< secretary-length-of-last-idle secretary-idle-threshold-secs-long)
    (secretary--call-timidly)))
;; (run-with-timer 3 nil (lambda ()  (print (frame-focus-state))))
;; (secretary-call-from-idle)



;;;; New presenter system

(defvar secretary--presentation-alist
  '((secretary-pres-make-plot-mood-async . "Not yet built")
    (secretary-pres-make-ledger-summary-async . "Not yet built")))

(defvar secretary--current-fn nil)

(defun secretary-buffer-presentations ()
  (let ((buf (get-buffer-create (concat "*" secretary-ai-name ": Presentations*"))))
    (with-current-buffer buf
      (buffer-disable-undo))
    buf))

;; (defun secretary-build-presentations-async ()
;;   (when secretary--presentation-alist
;;     (dolist (x secretary--presentation-alist)
;;       (let ((fn (car x))
;;             (result (ignore-errors (funcall (car x)))))
;;         (setq secretary--presentation-alist
;;               (if (stringp result)
;;                   (a-assoc secretary--presentation-alist fn result)
;;                 (a-assoc secretary--presentation-alist
;;                          fn
;;                          (concat (symbol-name fn) " failed."))))))
;;     (setq secretary-pres-index 0)))

(defun secretary-build-presentations-async ()
  (when secretary--presentation-alist
    (dolist (x secretary--presentation-alist)
      (funcall (car x)))
    (setq secretary-pres-index 0)))

;; for manual call
(defun secretary-view-presentations ()
  (interactive)
  (display-buffer (secretary-buffer-presentations)))

(defvar secretary--pres-i 0)

;; To be bound to "n" or so.
(defun secretary-present-next ()
  (interactive)
  (if (>= (1+ secretary--pres-i) (length secretary--presentation-alist))
      (message "No more items.  Press q to quit.")
    (cl-incf secretary--pres-i))
  (secretary--pres-insert secretary--pres-i))

;; To be bound to "p" or so.
(defun secretary-present-previous ()
  (interactive)
  (unless (>= 0 secretary--pres-i)
    (cl-decf secretary--pres-i))
  (secretary--pres-insert secretary--pres-i))

(defun secretary--pres-insert (i)
  (with-current-buffer (secretary-buffer-presentations)
    (delete-region (point-min) (point-max))
    (let ((presentation (nth i secretary--presentation-alist)))
      (insert (cdr presentation)))))

;; NOTE: these functions run asynchronously. This is not great for pureness, we
;; can't use their output the moment of calling them, instead they each have to
;; update the alist when finished, so run them well ahead of time.
;;
;; Async is often desirable, but I'm not sure here. The largest time sink for R
;; is probably importing libraries, unless you're running MCMC or doing big data,
;; so we should be able to spin up a persistent R process to talk to
;; synchronously, and then we can make code that's easier to reason about.

(defun secretary-pres-make-plot-mood-async ()
  (interactive)
  (setq secretary--current-fn #'secretary-pres-make-plot-mood-async)
  (secretary-pres-make-plot "mood.gnuplot"))

(defun secretary-pres-make-plot (gnuplot-script-basename)
  (interactive)
  (let* ((default-directory "/tmp/secretary")
         (pkg-dir (f-dirname (find-library-name "secretary")))
         (r-script (expand-file-name "R/make_data_for_plots.R" pkg-dir))
         (gnuplot-script (expand-file-name gnuplot-script-basename pkg-dir)))
    (mkdir "/tmp/secretary" t)
    (unless (executable-find "gnuplot")
      (error "gnuplot not found in PATH"))
    (set-process-sentinel
     (start-process secretary-ai-name nil "Rscript" r-script)
     `(lambda (_process event)
        (let ((error-p (s-contains-p "error" event))
              (new-result nil))
          (unless error-p
            (setq new-result (ignore-errors (with-temp-buffer
                                              (call-process "gnuplot" ,gnuplot-script t)
                                              (buffer-string))))
            (unless (stringp new-result)
              (setq error-p t)))
          (setq secretary--presentation-alist
                (if error-p
                    (a-assoc secretary--presentation-alist secretary--current-fn
                             (concat (symbol-name secretary--current-fn) " failed or didn't run."))
                  (a-assoc secretary--presentation-alist secretary--current-fn new-result))))))))

;;(require 'pfuture)
;;(pfuture-new "gnuplot" gnuplot-script-path)

;; Does not require ledger-mode.
(defun secretary-pres-make-ledger-summary-async ()
  (setq secretary--current-fn #'secretary-pres-make-ledger-summary-async)
  (if (executable-find "ledger")
      (let ((new-result
             (with-temp-buffer
               (call-process "ledger" nil t nil
                             "-f" secretary-ledger-file-name "register" "-M")
               (buffer-string))))
        (setq secretary--presentation-alist
              (a-assoc secretary--presentation-alist secretary--current-fn new-result)))
    (secretary-emit "Ledger executable not found, skipping.")))

(defun secretary-present-ledger-report ()
  "Jump to `secretary-ledger-file-name' and run `ledger-report'."
  (interactive)
  (when (ignore-errors (find-library-name "ledger-mode"))
    (require 'ledger-mode)
    (if (get-buffer ledger-report-buffer-name)
        (ledger-report-goto)
      (find-file secretary-ledger-file-name)
      (call-interactively #'ledger-report))))


;;;; Plot presenter

(defvar secretary-plot-hook nil
  "Hook called to print plots. A convenient place to add your
custom plots.")

(defmacro secretary--plot-ascii (gnuplot-script message &rest after-body)
  "Make an ascii plot.
Emit MESSAGE and run GNUPLOT-SCRIPT. After the plotting is done,
run any forms in AFTER-BODY."
  `(let* ((default-directory "/tmp/secretary")
          (r-script (expand-file-name "R/make_data_for_plots.R"
                                      (f-dirname (find-library-name "secretary")))))
     (mkdir "/tmp/secretary" t)
     ;; TODO: reuse secretary's R process to minimize package load time, so
     ;; we can use use call-process for easier reasoning.
     (set-process-sentinel
      (start-process secretary-ai-name nil "Rscript" r-script)
      (lambda (_1 _2)
        (secretary-emit ,message)
        (unless (get-buffer-window (secretary-buffer-chat))
          (display-buffer (secretary-buffer-chat)))
        (goto-char (point-max))
        (call-process "gnuplot" ,gnuplot-script (secretary-buffer-chat))
        ;; Strip formfeed inserted by gnuplot.
        (search-backward "\f")
        (replace-match "")
        (goto-char (point-max))
        ,@after-body))))

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

(defun secretary-plot-mood-ascii ()
  (interactive)
  (secretary--plot-ascii
   (expand-file-name "mood.gnuplot" (f-dirname (find-library-name "secretary")))
   "Plotting mood..."
   (search-backward "Plotting mood...")
   (forward-line 1)
   (goto-char (line-end-position))
   (just-one-space)
   (insert-rectangle (last
                      (split-string (f-read "/tmp/secretary/mood_desc.txt"))
                      16))
   (goto-char (point-max))))

(defun secretary-plot-weight ()
  (interactive)
  (let* ((default-directory "/tmp/secretary")
         (script (expand-file-name "R/sc_daily_plot.R"
                                   (f-dirname (find-library-name "secretary"))))
         (plot (expand-file-name "sc_plot1.png" "/tmp/secretary")))
    (secretary-emit "Plotting weight...")
    (mkdir "/tmp/secretary" t)
    (set-process-sentinel
     (start-process secretary-ai-name nil "Rscript" script "14" "-0.1")
     `(lambda (_process _event)
        (switch-to-buffer (secretary-buffer-chat))
        (if (insert-image-file ,plot)
            (progn
              (forward-char -1)
              (secretary-emit "And here's your weight, boss." "\n")
              (delete-file ,plot))
          (secretary-emit "Could not plot. :'( See `secretary-plot-weight' source."))
        (goto-char (point-max))
        (insert "\n")))))

(defun secretary-plot-weight-ascii ()
  (interactive)
  (let* ((default-directory "/tmp/secretary")
         (r-script (expand-file-name "R/make_data_for_plots.R"
                                     (f-dirname (find-library-name "secretary"))))
         (gnuplot-script (expand-file-name "weight.gnuplot"
                                           (f-dirname (find-library-name "secretary")))))
    (mkdir "/tmp/secretary" t)
    ;; TODO: reuse secretary's R process to minimize package load time.
    (set-process-sentinel
     ;; TODO: pass start-date (today minus 3mo) and projection incline, letting
     ;; user change the incline (persist for future plots too)
     (start-process secretary-ai-name nil "Rscript" r-script)
     `(lambda (_process _event)
        (secretary-emit "Plotting weight...")
        ;; (unless (get-buffer-window (secretary-buffer-chat)) (switch-to-buffer (secretary-buffer-chat)))
        (with-current-buffer (secretary-buffer-chat)
          (goto-char (point-max))
          (call-process "gnuplot" ,gnuplot-script (secretary-buffer-chat))
          ;; Strip formfeed inserted by gnuplot.
          (search-backward "\f")
          (replace-match "")
          (goto-char (point-max)))))))

(defun secretary-present-plots ()
  (interactive)
  (unless (null secretary-plot-hook)
    (secretary-emit (seq-random-elt '("I have plotted the latest intel, boss."
                               "Here are some projections!"
                               "Data is beautiful, don't you think?")))
    ;; HACK: because of async; want sync.
    (dolist (hook secretary-plot-hook)
      (funcall hook)
      (sit-for .5))))


;;;; Other presenters

(defun secretary-present-mood ()
  (interactive)
  (view-file "/home/kept/Self_data/mood.tsv")
  (auto-revert-mode))

(defun secretary-present-ledger-report ()
  (interactive)
  (when (or (featurep 'ledger-mode-autoloads)
            (fboundp #'ledger-report))
    (require 'ledger-mode)
    (let ((file secretary-ledger-file-name))
      ;; for some reason, (with-current-buffer (get-buffer-create file)) leads
      ;; to all kinds of errors, so we have to use find-file
      (find-file-noselect file)
      (call-interactively #'ledger-report))))

(defun secretary-make-ods-for-finance-2020 ()
  "Make and open an ODS spreadsheet generated from Ledger data."
  (interactive)
  (let* ((r-script "/home/kept/Journal/Finances/R/generate_an_ods.R")
         (default-directory (f-dirname (f-dirname r-script))))
    (call-process "Rscript" nil nil nil r-script "l.ledger" "2020.ods" "SEK")
    (start-process "soffice" nil "soffice" "2020.ods")))

(defun secretary-make-ods-for-finance ()
  "Make and open an ODS spreadsheet from Ledger data.
Requires the ssconvert program that comes with Gnumeric."
  (interactive)
  (let* ((script (expand-file-name "R/generate_an_ods.R"
                                   (f-dirname (find-library-name "secretary"))))
         (sheet (expand-file-name ".tmp_finances.ods"
                                  secretary-memory-dir))
         (default-directory (f-dirname script))
         (app (seq-find #'executable-find '("gnumeric"
                                            "soffice"
                                            "open"
                                            "mimeopen"
                                            "xdg-open"))))
    (secretary--run "Rscript" script secretary-ledger-file-name sheet)
    (secretary--run-async app sheet)))

(defcustom secretary-ledger-file-name
  "/home/kept/Journal/Finances/2021.ledger"
  "Ledger file to read or visit; we will never modify it."
  :group 'secretary
  :type 'string)

;; WIP
(defun secretary-make-ods ()
  "Make an ODS spreadsheet of variables for you to play with."
  (interactive))


;;;; Diary presenter

(defvar secretary-past-sample-function #'secretary-past-sample-default)

(defun secretary-past-sample-default (&optional ts)
  "Return a list of ts objects."
  "Return a list of ts objects referring to yesterday, this
weekday the last 4 weeks, this day of the month the last 12
months, and this date the past 50 years."
  (let ((now (or ts (ts-now))))
    (-uniq (append
            (--iterate (ts-dec 'day 1 it) now 1)
            (--iterate (ts-dec 'woy 1 it) now 4)
            (--iterate (ts-dec 'month 1 it) now 12)
            (--iterate (ts-dec 'year 1 it) now 50)))))

(defun secretary-make-indirect-datetree (buffer dates)
  (require 'org)
  (let ((dates (-sort 'ts<= dates))
        (counter 0))
    (switch-to-buffer buffer)
    (org-mode)
    (delete-region (point-min) (point-max))
    (with-temp-buffer
      (insert-file-contents secretary-location-main-datetree)
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

(defun secretary-existing-diary (&optional date dir file-format)
  "Return the first file in DIR matching FILE-FORMAT.
FILE-FORMAT is handled by `parse-time-string'. The value returned
is a full filesystem path or nil.

When DATE is nil, use today.  Should be a ts object.
When DIR is nil, use `org-journal-dir'.
When FILE-FORMAT is nil, use `org-journal-file-format'.

Note that org-journal is not needed."
  (let* ((dir (or dir (bound-and-true-p org-journal-dir)))
         (file-format (or file-format (and (boundp 'org-journal-file-type)
                                           (eq org-journal-file-type 'daily)
                                           org-journal-file-format)))
         (file (--find (string-match-p (ts-format file-format date)
                                       it)
                       (directory-files dir))))
    (unless (null file)
      (expand-file-name file dir))))
;; (secretary-existing-diary (ts-dec 'day 2 (ts-now)))
;; (secretary-existing-diary (ts-now) "/home/kept/Diary" )
;; (--keep (secretary-existing-diary it "/home/kept/Diary") (funcall secretary-past-sample-function))

;; TODO: allow either discrete or datetree to be nil
;; TODO: allow a list of datetrees
;; TODO: make separate buffers for each datetree entry (use rename-buffer)
;; TODO: make the datetree buffer(s) the next in line when you pop the last
;;       discrete view
;; TODO: try creating a sparse tree, so user can edit in-place
;; TODO: show also the agenda log for each date if not empty
;;;###autoload
(defun secretary-present-diary (&optional ts skip-prompt)
  (interactive)
  (let* ((buffer (get-buffer-create (concat "*" secretary-ai-name ": Selected diary entries*")))
         (today (or ts secretary--date))
         (dates-to-check (funcall secretary-past-sample-function today))
         (discrete-files-found (--keep (secretary-existing-diary it) dates-to-check))
         (datetree-found-count (secretary-make-indirect-datetree buffer dates-to-check))
         (total-found-count (+ (length discrete-files-found) datetree-found-count)))
    (if (= 0 total-found-count)
        (secretary-emit "No diary entries relevant to this date.")
      (if (or skip-prompt
              (secretary-ynp "Found " (int-to-string total-found-count) " past diary "
                    (if (= 1 total-found-count) "entry" "entries")
                    " relevant to this date. Want me to open "
                    (if (= 1 total-found-count) "it" "them")
                    "?"))
          (progn
            (switch-to-buffer buffer)
            (view-mode)
            (if (-non-nil discrete-files-found)
                (dolist (x discrete-files-found)
                  (view-file x))))
        (kill-buffer buffer)))))
;; (secretary-present-diary (ts-now))
;; (secretary-present-diary (ts-dec 'day 1 (ts-now)))


;;;; Handle idle & reboots & crashes

(defvar secretary--last-online
  nil)

(defvar secretary--idle-beginning
  nil)

(defvar secretary-length-of-last-idle 0
  "Length of the last idle period, in seconds.")

(defcustom secretary-idle-threshold-secs-short (* 10 60)
  "Duration in seconds, above which the user is considered idle."
  :group 'secretary
  :type 'number)

(defcustom secretary-idle-threshold-secs-long (* 90 60)
  "Be idle at least this many seconds to be greeted upon return."
  :group 'secretary
  :type 'number)

(defcustom secretary-return-from-idle-hook
  '(secretary-log-idle
    secretary-call-from-idle)
  "Hook run when user returns from a period of idleness.
Note: An Emacs startup also counts as a return from idleness.
You'll probably want your hook to be conditional on some value of
`secretary-length-of-last-idle', which at startup is calculated from
the last Emacs shutdown or crash (technically, last time
`secretary-mode' was running)."
  :group 'secretary
  :type '(repeat function))

(defcustom secretary-periodic-not-idle-hook
  '(secretary--save-variables-to-disk
    secretary--save-buffer-logs-to-disk)
  "Hook run every minute when the user is not idle."
  :group 'secretary
  :type '(repeat function))

(defun secretary--start-next-timer (&optional assume-idle)
  "Start one or the other timer depending on idleness.
If ASSUME-IDLE is non-nil, skip the idle check and associated
overhead."
  (if (or assume-idle (secretary-idle-p))
      (named-timer-run :secretary 2 nil #'secretary--user-is-idle t)
    (named-timer-run :secretary 60 nil #'secretary--user-is-active)))

(defun secretary--user-is-active ()
  "Do stuff assuming the user is active (not idle).
This function is called by `secretary--start-next-timer'
repeatedly for as long as the user is active (not idle).

Runs `secretary-periodic-not-idle-hook'."
  ;; Guard the case where the user puts the computer to sleep manually, which
  ;; means this function will still be queued to run when the computer wakes.  If
  ;; the time difference is suddenly big, hand off to the other function.
  (if (> (ts-diff (ts-now) secretary--last-online)
         secretary-idle-threshold-secs-short)
      (secretary--user-is-idle)
    (setq secretary--last-online (ts-now))
    (setq secretary--idle-beginning (ts-now))
    (secretary--start-next-timer)
    ;; Run hooks last, in case they contain bugs.
    (run-hooks 'secretary-periodic-not-idle-hook)))

(defun secretary--user-is-idle (&optional decrement)
  "Do stuff assuming the user is idle.
This function is called by `secretary--start-next-timer'
repeatedly for as long as the user is idle.

When the user comes back, this function will be called one last
time, at which point the idleness condition will fail and it sets
`secretary-length-of-last-idle' and runs
`secretary-return-from-idle-hook'.  That it has to run exactly
once with a failing condition that normally succeeds, as opposed
to running never or forever, is the reason it has to be a
separate function from `secretary--user-is-active'."
  (setq secretary--last-online (ts-now))
  (if (secretary-idle-p)
      (secretary--start-next-timer 'assume-idle)
    ;; Take the idle threshold into account and correct the idle begin point.
    (when decrement
      (ts-decf (ts-sec secretary--idle-beginning) secretary-idle-threshold-secs-short))
    (setq secretary-length-of-last-idle (ts-diff (ts-now) secretary--idle-beginning))
    (unwind-protect
        (run-hooks 'secretary-return-from-idle-hook)
      (setq secretary--idle-beginning (ts-now))
      (secretary--start-next-timer))))

(defcustom secretary-idle-file-name "/home/kept/Self_data/idle.tsv"
  nil)

(defun secretary-log-idle ()
  (secretary-append-tsv secretary-idle-file-name
    (ts-format)
    (number-to-string (/ (round secretary-length-of-last-idle) 60))))


;;;; Persistent variables

(defun secretary-last-online-file-name ()
  (expand-file-name "last-online" secretary-memory-dir))

(defun secretary-mood-alist-file-name ()
  (expand-file-name "mood-alist" secretary-memory-dir))

;; TODO: Calc reasonable defaults from dataset contents
(defun secretary--restore-variables-from-disk ()
  (when (f-exists-p (secretary-mood-alist-file-name))
    (setq secretary-mood-alist
          (read (f-read (secretary-mood-alist-file-name)))))
  (setq secretary--last-online
        (if (f-exists-p (secretary-last-online-file-name))
            (ts-parse (f-read (secretary-last-online-file-name)))
          (unless (not (null secretary--last-online))
            (make-ts :unix 0))))
  (setq secretary--idle-beginning secretary--last-online)
  (when (and secretary-chat-log-file-name
             (file-exists-p secretary-chat-log-file-name))
    (let ((chatfile-modtime
           (make-ts :unix (time-convert (file-attribute-modification-time
                                         (file-attributes secretary-chat-log-file-name))
                                        'integer))))
      (when (ts< secretary--last-chatted chatfile-modtime)
        (setq secretary--last-chatted chatfile-modtime)))))

(defun secretary--save-variables-to-disk ()
  (make-directory secretary-memory-dir t)
  (secretary-write-safely (ts-format secretary--last-online) (secretary-last-online-file-name))
  (secretary-write-safely (prin1-to-string secretary-mood-alist) (secretary-mood-alist-file-name))
  (when secretary-chat-log-file-name
    (secretary-write-safely (with-current-buffer (secretary-buffer-chat) (buffer-string))
                   secretary-chat-log-file-name)))

(defcustom secretary-memory-dir
  (expand-file-name "secretary" user-emacs-directory)
  "Directory for persistent files (not your datasets)."
  :group 'secretary
  :type 'string)

(defcustom secretary-chat-log-file-name
  (expand-file-name "chat.log" secretary-memory-dir)
  "Where to save chat log across sessions. Can be nil."
  :group 'secretary
  :type 'string)


;;;; "Main"

(defun secretary--keepalive ()
  (unless (member (named-timer-get :secretary) timer-list)
    (message "[%s] secretary timer found dead, reviving it."
             (format-time-string "%H:%M"))
    (secretary--start-next-timer)))

(defun secretary--another-secretary-running-p ()
  "Return t if another Emacs instance has secretary-mode on.
Return nil if only the current Emacs instance or none has it on.
If you've somehow forced it on in several Emacsen, the behavior
is unspecified, but it shouldn't be possible to do."
  (when (file-exists-p "/tmp/secretary/pid")
    (let ((pid (string-to-number (f-read-bytes "/tmp/secretary/pid"))))
      (and (/= pid (emacs-pid))
           (member pid (list-system-processes))))))

(defun secretary-unload-function ()
  "Unload the Secretary library."
  (secretary-mode 0)
  (with-demoted-errors nil
    (unload-feature 'secretary-tests)
    (unload-feature 'secretary-config))
  ;; Continue standard unloading.
  nil)

;;;###autoload
(define-minor-mode secretary-mode
  nil
  :global t
  (if secretary-mode
      (when (and
             (cond
              (secretary--idle-seconds-fn  ;; if preset, use that.
               t)
              ((eq system-type 'darwin)
               (autoload #'org-mac-idle-seconds "org-clock")
               (setq secretary--idle-seconds-fn #'org-mac-idle-seconds)
               t)
              ;; If under Mutter's Wayland compositor
              ((and (getenv "DESKTOP_SESSION")
                    (s-matches-p (rx (or "gnome" "ubuntu"))
                                 (getenv "DESKTOP_SESSION"))
                    (not (s-contains-p "xorg"
                                       (getenv "DESKTOP_SESSION"))))
               (setq secretary--idle-seconds-fn #'secretary--gnome-idle-seconds)
               t)
              ((and (eq window-system 'x) ;; true also under XWayland, I think
                    (executable-find secretary-x11idle-program-name))
               (setq secretary--idle-seconds-fn #'secretary--x11-idle-seconds)
               t)
              (secretary-fallback-to-emacs-idle-p
               (autoload #'org-emacs-idle-seconds "org-clock")
               (setq secretary--idle-seconds-fn #'org-emacs-idle-seconds)
               t)
              (t
               (message secretary-ai-name ": Not able to detect idleness, "
                        "I'll be useless. Disabling secretary-mode.")
               (secretary-mode 0)
               nil))
             (if (secretary--another-secretary-running-p)
                 (progn
                   (message "Another secretary active.")
                   (secretary-mode 0)
                   nil)
               t)
             (if (--all-p (and (boundp it)
                               (not (null it)))
                          '(secretary-aphorisms
                            secretary-items))
                 t
               (message "Needed variables not set, read manual or do %s."
                        "M-x load-library secretary-config")
               (secretary-mode 0)
               nil))
        (mkdir "/tmp/secretary" t)
        (f-write (number-to-string (emacs-pid)) 'utf-8 "/tmp/secretary/pid")
        (add-function :after after-focus-change-function #'secretary-log-buffer) ;; TODO: refine secretary-log-buffer
        (add-hook 'window-buffer-change-functions #'secretary-log-buffer)
        (add-hook 'window-selection-change-functions #'secretary-log-buffer)
        (add-hook 'after-init-hook #'secretary--restore-variables-from-disk -90)
        (add-hook 'after-init-hook #'secretary--start-next-timer 90)
        (named-timer-run :secretary-keepalive 300 300 #'secretary--keepalive)
        (when after-init-time
          (progn
            (when (or (null secretary-mood-alist)
                      (null secretary--last-online)
                      (= 0 (ts-unix secretary--last-online)))
              (secretary--restore-variables-from-disk))
            (secretary--user-is-active))))
    (secretary--save-variables-to-disk)
    (setq secretary--idle-seconds-fn nil)
    (ignore-errors
      (f-delete "/tmp/secretary/pid"))
    (remove-function after-focus-change-function #'secretary-log-buffer)
    (remove-hook 'window-buffer-change-functions #'secretary-log-buffer)
    (remove-hook 'window-selection-change-functions #'secretary-log-buffer)
    (remove-hook 'after-init-hook #'secretary--restore-variables-from-disk)
    (remove-hook 'after-init-hook #'secretary--start-next-timer)
    (named-timer-cancel :secretary)
    (named-timer-cancel :secretary-keepalive)))

(provide 'secretary)

;;; secretary.el ends here
