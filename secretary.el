;;; secretary.el --- Help the user meet goals -*- lexical-binding: t; -*-
;; Author: Martin Edström <meedstrom@teknik.io>
;; URL: https://github.com/meedstrom/secretary
;; Version: 0.1.0
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

;; See README.org or website: https://github.com/meedstrom/secretary

;;; Code:

;; builtins
(require 'seq)
(require 'map)
(require 'subr-x)
(require 'cl-lib)
(require 'find-func)
(require 'transient) ;; Emacs 28 builtin

;; external
(require 'ts) ;; essential
(require 'named-timer) ;; essential
(require 'ess)
(require 'dash)
(require 's)
(require 'f) ;; f-read is just nice
(require 'pfuture)

(defvar secretary-debug-p nil)

(defgroup secretary nil "The Emacs in-house secretary."
  :prefix "secretary-"
  :group 'convenience)

(defcustom secretary-ai-name "Val" ;; short for Virtual Assistant Library
  "Your secretary's name."
  :group 'secretary
  :type 'string
  :risky t)
;; REVIEW: this? all work fine when loading from custom-file before secretary mode?
;; :set (lambda (sym val)
;;        (secretary--save-buffer-logs-to-disk)
;;        (secretary--save-variables-to-disk)
;;        (set-default sym val)))

(defcustom secretary-user-birthday nil
  "Your birthday."
  :group 'secretary
  :type 'string
  :safe t)

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
  :type 'number
  :safe t)

(defcustom secretary-sit-medium .8
  "A duration in seconds to pause for effect after certain kinds
of messages. See also `secretary-sit-long' and
`secretary-sit-short'."
  :group 'secretary
  :type 'number
  :safe t)

(defcustom secretary-sit-short .5
  "A duration in seconds to pause for effect after certain kinds
of messages. See also `secretary-sit-long' and
`secretary-sit-medium'."
  :group 'secretary
  :type 'number
  :safe t)

(defcustom secretary-presumptive-p nil
  "Whether to skip some prompts and assume yes."
  :group 'secretary
  :type 'boolean)

;; probably going to be deprecated
(defcustom secretary-memory-dir
  (expand-file-name "secretary" user-emacs-directory)
  "Directory for persistent files (not your datasets)."
  :group 'secretary
  :type 'string
  :risky t)

(defcustom secretary-mem-loc
  (convert-standard-filename
   (expand-file-name "memory.tsv" secretary-memory-dir))
  nil
  :group 'secretary
  :type 'string
  :risky t)

(defcustom secretary-chat-log-file-name
  (convert-standard-filename
   (expand-file-name "chat.log" secretary-memory-dir))
  "Where to save chat log across sessions. Can be nil."
  :group 'secretary
  :type 'string
  :risky t)


;;;; Handle idle & reboots & crashes
;; It's big brain time.

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

(defcustom secretary-idle-file-name
  (convert-standard-filename "~/idle.tsv")
  "Location of the idleness log."
  :group 'secretary
  :type 'string)

(defun secretary-log-idle ()
  (secretary-append-tsv secretary-idle-file-name
    (ts-format)
    (number-to-string (/ (round secretary-length-of-last-idle) 60))))

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


;;; Items
;; Q: What's cl-defstruct?  A: https://nullprogram.com/blog/2018/02/14/

;; NOTE: If you change the order of keys, secretary--recover-memory will set
;; the wrong values!! Be sure you have a good reason.

;; TODO: remove some/all initvalues?
(cl-defstruct (secretary-item
               (:constructor secretary-item-create)
               (:copier nil))
  (dismissals 0)
  max-calls-per-day
  (max-successes-per-day nil :documentation "Alias of max-entries-per-day, more semantically meaningful where there is no dataset.")
  max-entries-per-day
  (min-hours-wait 3)
  lookup-posted-time
  successes
  fn ;; primary key (must be unique)
  dataset
  (last-called (make-ts :unix 0)) ;; prevent nil-value errors
  ;; name ;; truly-unique key (if reusing fn in two instances for some reason)
  )

;; (secretary-item-create :fn 'foo)

(defvar secretary-items)

(defun secretary--item-by-fn (fn)
  "Get the item associated with the query function FN."
  (--find (equal fn (secretary-item-fn it)) secretary-items))

(defun secretary-disabled-items-file-name ()
  "Path to file holding list of disabled queries.
Needed to persist disablings across restarts."
  (expand-file-name "disabled-items.el" secretary-memory-dir))

(defun secretary--enabled-items ()
  (let ((all (-map #'secretary-item-fn secretary-items))
        (disabled (when (f-exists-p (secretary-disabled-items-file-name))
                    (read (f-read (secretary-disabled-items-file-name))))))
    (-difference all disabled)))

(defun secretary-reenable-item ()
  (interactive)
  (let* ((disabled-fns (when (f-exists-p (secretary-disabled-items-file-name))
                         (read (f-read (secretary-disabled-items-file-name)))))
         (coll (when (< 0 (length disabled-fns))
                 (-map #'symbol-name disabled-fns))))
    (if coll
        ;; TODO
        (completing-read "Reenable: " coll)
      (message "There are no disabled items"))))

(defun secretary-disable-item-by-fn (fn)
  (f-write (prin1-to-string (remove fn (secretary--enabled-items)))
           'utf-8
           (secretary-disabled-items-file-name)))

(defun secretary-ask-disable (fn)
  (if (secretary-ynp
       "You have been dismissing " (symbol-name fn)
       ", shall I stop tracking it for now?")
      (progn (secretary-disable-item-by-fn fn)
             t)
    (setf (secretary-item-dismissals (secretary--item-by-fn fn)) 0)
    nil))

;; NOTE: Do not move the check to secretary--pending-p, since it needs interactivity.
(defun secretary-call-fn-check-dismissals (fn)
  "Call FN, but ask to disable if been dismissed many times."
  (interactive "CCommand: ")
  (unless (and (<= 3 (secretary-item-dismissals (secretary--item-by-fn fn)))
               (secretary-ask-disable fn))
    (funcall fn)))

(defun secretary--pending-p (fn)
  (let* ((i (secretary--item-by-fn fn))
         (dataset (secretary-item-dataset i))
         (alt-dataset (expand-file-name (concat "successes-" (symbol-name fn)) secretary-memory-dir))
         ;; max-successes is meant as an alias for max-entries. if both are
         ;; defined, entries has precedence.
         (max-entries (secretary-item-max-entries-per-day i))
         (max-successes (or max-entries (secretary-item-max-successes-per-day i)))
         (max-entries (or max-successes (secretary-item-max-entries-per-day i)))
         (lookup-posted-time (secretary-item-lookup-posted-time i))
         (dismissals (secretary-item-dismissals i))
         (min-hrs-wait (secretary-item-min-hours-wait i))
         (min-secs-wait (* 60 60 min-hrs-wait))
         (successes-today (secretary--count-successes-today fn))
         (successes-specified-and-exceeded (and successes-today
                                                max-successes
                                                (>= successes-today max-successes)))
         (last-called (secretary-item-last-called i))
         (called-today (and (= (ts-day last-called) (ts-day (ts-now)))
                            (> (ts-hour last-called) 4)))
         (recently-logged
          (when (and (stringp dataset)
                     (file-exists-p dataset))
            (> min-secs-wait
               (if lookup-posted-time
                   (- (ts-unix (ts-now))
                      (string-to-number (car (secretary--last-in-tsv dataset))))
                 (ts-diff (ts-now)
                          (ts-parse (secretary-last-timestamp-in-tsv dataset)))))))
         ;; Even if we didn't log yet, we don't quite want to be that persistent
         (recently-called (< (ts-diff (ts-now) last-called)
                             ;; hours multiplied by n dismissals
                             (* dismissals 60 60))))
    (unless recently-logged
      (when (or (not called-today)
                (not (and (stringp dataset)
                          (file-exists-p dataset)))
                (null max-entries)
                (> max-entries (length (secretary--get-entries-in-tsv dataset))))
        (unless recently-called
          (unless successes-specified-and-exceeded
            t))))))

;;(secretary--pending-p #'secretary-query-weight)
;;(secretary--pending-p #'secretary-greet)
;;(secretary--pending-p #'secretary-query-sleep)
;;(secretary--pending-p #'secretary-query-mood)


;;; Library

(defvar secretary--current-fn nil)

(defvar secretary--queue nil)

(defvar secretary--date
  (ts-now)
  "Date to which to apply the current fn.
Can be set anytime during a welcome to override the date to which
some queries apply, for example to log something for yesterday.
This may not apply, check the source for the welcomer you are
using.")

(defun secretary--new-uuid ()
  "Same as `org-id-uuid', but avoid relying on Org."
  (let ((rnd (md5 (format "%s%s%s%s%s%s%s"
                          (random)
                          (seconds-to-time (float-time))
                          (user-uid)
                          (emacs-pid)
                          (user-full-name)
                          user-mail-address
                          (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
            (substring rnd 0 8)
            (substring rnd 8 12)
            (substring rnd 13 16)
            (format "%x"
                    (logior
                     #b10000000
                     (logand
                      #b10111111
                      (string-to-number
                       (substring rnd 16 18) 16))))
            (substring rnd 18 20)
            (substring rnd 20 32))))

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

(defvar secretary-buffer-r nil)

(defun secretary--init-r ()
  "Spin up an R process and load needed R libraries.
Uses `run-ess-r' which is full of sanity checks (e.g. for cygwin
and text encoding), but creates an interactive R buffer which
unfortunately may surprise the user when they go to work on their
own R project."
  (let ((default-directory (f-dirname (find-library-name "secretary"))))
    (save-window-excursion
      (setq secretary-buffer-r (run-ess-r)))
    ;; gotcha: only use `ess-with-current-buffer' for temp output buffers, not for the process buffer
    (with-current-buffer secretary-buffer-r
      ;; TODO: How to check if the script errors out?
      (ess-execute "source(\"make_data_for_plots.R\")" 'buffer))))

(defvar secretary--k nil)
(defun secretary--y-or-n-p-insert-k ()
  "Mostly like `y-or-n-p-insert-y'."
  (interactive nil minibuffer-mode)
  (delete-minibuffer-contents)
  (insert "y")
  (setq secretary--k t)
  (exit-minibuffer))

;; Trivia: look at map-y-or-n-p in (require 'map-ynp). Cool!
(defun secretary-ynp (&rest strings)
  "Wrapper around `y-or-n-p' for secretary-chat-mode."
  (let* (;; (default-y-or-n-p-map y-or-n-p-map)
         ;; (default-cmd (lookup-key y-or-n-p-map (kbd "k")))
         ;; TODO: Also show which log file we're applying to
         (background-info (concat "[Applying to date: " (ts-format "%Y %b %d" secretary--date) "]\n"))
         (prompt (string-join strings)))
    (unwind-protect
        (progn
          (pop-to-buffer (secretary-buffer-chat))
          (secretary-emit prompt)
          (define-key y-or-n-p-map (kbd "h") #'secretary-dispatch)
          (define-key y-or-n-p-map (kbd "<SPC>") #'secretary-dispatch)
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

(defun secretary-check-special-input (input)
  (when (string-match-p "/skip" input)
    (progn
      (if (and (< 1 (length secretary--queue))
               (member secretary--current-fn secretary--queue))
          ;; Try to proceed to next item
          (progn
            (setq secretary--queue
                  (cl-remove secretary--current-fn secretary--queue :count 1))
            (secretary-resume))
        ;; Just cancel the session
        (abort-recursive-edit)))))

;; TODO: Bind C-- and C-+ to date increment/decrement (C-0 to reset)
(defun secretary-read (prompt &optional collection default)
  "Read user input, and allow special responses like \"skip\".
Echo both prompts and responses to the chat buffer."
  (secretary-emit prompt)
  (let* ((background-info (concat "[Applying to date: "
                                  (ts-format "%Y %b %d" secretary--date) "]\n"))
         (extra-collection '("/skip"))
         (input (completing-read
                 (concat background-info
                         (ts-format "[%H:%M] ")
                         prompt
                         (when (stringp default)
                           " (default " default "): "))
                 (append collection extra-collection)
                 nil nil nil nil
                 (when (stringp default)
                   default))))
    (secretary-emit-same-line input)
    ;; TODO: help!! develop midprompt-dispatch
    ;; (when (string-match-p "help" input)
    ;;   (secretary-dispatch)
    ;;   (funcall secretary--current-fn))
    (secretary-check-special-input input)
    input))

(defun secretary-read-string (prompt)
  "Like `secretary-read' but call `read-string' internally."
  (secretary-emit prompt)
  (let* ((background-info (concat "[Applying to date: "
                                  (ts-format "%Y %b %d" secretary--date) "]\n"))
         (input (read-string
                 (concat background-info
                         (ts-format "[%H:%M] ")
                         prompt))))
    (secretary-emit-same-line input)
    (secretary-check-special-input input)
    input))

(defcustom secretary-chime-audio-file
  (convert-standard-filename
   (expand-file-name
    ;; From https://freesound.org/people/josepharaoh99/sounds/380482/
    "assets/Chime Notification-380482.wav"
    ;; From https://bigsoundbank.com/detail-0319-knock-on-a-glass-door-1.html
    ;; "assets/DOORKnck_Knock on a glass door 1 (ID 0319)_BSB.wav"
    (f-dirname (find-library-name "secretary"))))
  "Sound to play when a welcomer is triggered unannounced."
  :group 'secretary
  :type 'string)

(defcustom secretary-play-sounds-p nil
  "Whether to play sounds."
  :group 'secretary
  :type 'boolean)

(defun secretary--chime-aural ()
  (and secretary-play-sounds-p
       (executable-find "aplay")
       (file-exists-p secretary-chime-audio-file)
       (pfuture-new "aplay" secretary-chime-audio-file)))

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

;; TODO: nil initvalue?
(defvar secretary--last-chatted
  (make-ts :unix 0)
  "Timestamp updated whenever the chat is written to.")

(defun secretary-emit (&rest strings)
  "Write a line to the chat buffer, made from STRINGS.
Returns the completed string so you can pass it to `message', for
example."
  (let ((new-date-maybe (if (/= (ts-day (ts-now))
                                (ts-day secretary--last-chatted))
                            (concat "\n\n" (ts-format "%A, %d %B %Y") (secretary--holiday-maybe) "\n")
                          ""))
        (msg (concat "\n<" (ts-format "%H:%M") "> " (string-join strings))))
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
        (insert msg)))
    (setq secretary--last-chatted (ts-now))
    msg))

(defun secretary--holiday-maybe ()
  (require 'calendar)
  (require 'holidays)
  (if-let (foo (calendar-check-holidays (calendar-current-date)))
      (concat " -- " (s-join " " foo))
    ""))

(defun secretary--random-p (&rest _args)
  "Return t or nil, at random. Can be a predicate for `sort'."
  (> 0 (random)))

(defun secretary-write-safely (text path)
  "Write TEXT to file at PATH if the content differs.
Also revert any buffer visiting it, or signal an error if there
are unsaved changes."
  (let ((buf (find-buffer-visiting path)))
    (and buf
         (buffer-modified-p buf)
         (error "Unsaved changes in open buffer: %s" (buffer-name buf)))
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
         (error "Unsaved changes in open buffer: %s" (buffer-name buf)))
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

(defvar secretary--x11idle-program-name "x11idle")

(defcustom secretary-fallback-to-emacs-idle-p nil
  "Track Emacs idle rather than turn off under unknown OS/DE.
Not recommended, as the idleness log will be meaningless unless
you never use a graphical program. You'll end up with the
situation where returning to Emacs from a long Firefox session
triggers the return-from-idle-hook."
  :group 'secretary
  :type 'boolean)

(defvar secretary--idle-seconds-fn nil)

(defun secretary--idle-seconds ()
  "Number of seconds user has been idle, as told by the system."
  (funcall secretary--idle-seconds-fn))

(defun secretary-idle-p ()
  (> (secretary--idle-seconds) secretary-idle-threshold-secs-short))

(defun secretary--x11-idle-seconds ()
  "Like `org-x11-idle-seconds' without need for /bin/sh or org."
  (/ (secretary--process-output-to-number secretary--x11idle-program-name) 1000))

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

(defun secretary--string-contains-number (input)
  (s-matches-p (rx num) input))

(defun secretary--coerce-to-hh-mm (input)
  "Coerce from inputs matching HH:MM, HH or H, to HH:MM (24-h).
If \"am\" or \"pm\" present, assume input is in 12-hour clock."
  (declare (pure t) (side-effect-free t))
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

(defvar secretary--excursion-buffers nil)

(defun secretary--check-return-from-excursion ()
  (let ((others (remove (current-buffer) secretary--excursion-buffers)))
    (when (-none-p #'buffer-live-p others)
      (remove-hook 'kill-buffer-hook #'secretary--check-return-from-excursion)
      (named-timer-cancel :secretary-excursion)
      (setq secretary--excursion-buffers nil) ;; hygiene
      (when (null (secretary-item-dataset (secretary--item-by-fn secretary--current-fn)))
        (secretary-append-tsv
          (expand-file-name (concat "successes-" (symbol-name secretary--current-fn))
                            secretary-memory-dir)))
      (setq secretary--queue
            (cl-remove secretary--current-fn secretary--queue :count 1))
      ;; HACK Because the current-buffer is still active, wait to be sure the
      ;; kill-buffer completes.  I would like an after-kill-buffer-hook so I
      ;; don't need this timer.
      (run-with-timer 0.25 nil #'secretary-resume))))

;; (add-hook 'kill-buffer-hook (defun lol ()
                              ;; (print (buffer-live-p (current-buffer)))))

(defun secretary--stop-watching-excursion ()
  "Called after 5 minutes on an excursion."
  (named-timer-cancel :secretary-excursion)
  (remove-hook 'kill-buffer-hook #'secretary--check-return-from-excursion))

(defun secretary--after-cancel-do-things ()
  (advice-remove 'abort-recursive-edit #'secretary--after-cancel-do-things) ;; needed
  (cl-incf (secretary-item-dismissals
             (secretary--item-by-fn secretary--current-fn)))
  ;; Re-add the fn to the queue because it got removed (so I expect); after a
  ;; cancel, we want it to remain queued up.
  (cl-pushnew secretary--current-fn secretary--queue)
  (setq secretary--current-fn nil)) ;; hygiene

;; Ok, here's the shenanigans.  Observe that keyboard-quit and
;; abort-recursive-edit are distinct.  When the user cancels a "query" by
;; typing C-g, they were in the minibuffer, so it calls abort-recursive-edit.
;; You can define an "excursion" by putting a keyboard-quit in BODY.  With
;; this, we ensure different behavior for queries and excursions.  How?  We
;; advise abort-recursive-edit to do things that we only want when a query is
;; cancelled.  In this macro, pay attention to the placement of NEW-BODY, since
;; it may contain a keyboard-quit.  Thus, everything coming after NEW-BODY will
;; never be called for excursions, unless of course you use unwind-protect.
(defmacro secretary-defquery-and-excursion (name args &rest body)
  (declare (indent defun) (doc-string 3))
  (let* ((parsed-body (macroexp-parse-body body))
          (declarations (car parsed-body))
          (new-body (cdr parsed-body)))
    `(cl-defun ,name ,args
       ;; Ensure it's always interactive
       ,@(if (member 'interactive (-map #'car-safe declarations))
           declarations
           (-snoc declarations '(interactive)))
       (setq secretary--current-fn #',name)
       (unless (secretary--item-by-fn secretary--current-fn)
         (error "%s not listed in secretary-items" (symbol-name secretary--current-fn)))
       ;; Set up watchers in case any "excursion" happens.
       ;; (unless (called-interactively-p 'any) ;; allow manual use via M-x without triggering shenanigans
       (add-hook 'kill-buffer-hook #'secretary--check-return-from-excursion 96)
       (named-timer-run :secretary-excursion (* 5 60) nil #'secretary--stop-watching-excursion)
       ;; Set up watcher for cancelled prompt.
       (advice-add 'abort-recursive-edit :before #'secretary--after-cancel-do-things)
       (let* ((current-item (secretary--item-by-fn secretary--current-fn))
              (current-dataset (secretary-item-dataset current-item)))
         (unwind-protect
             (prog1 (progn
                      ;; I suppose we could infer from last-called afterwards
                      ;; whether the excursion was a failure?
                      (setf (secretary-item-last-called current-item) (ts-now))
                      ,@new-body)
               ;; All below this line will only happen for pure queries, and only after success.
               (setq secretary--queue
                     (cl-remove secretary--current-fn secretary--queue :count 1))
               (setf (secretary-item-dismissals current-item) 0)
               ;; Save timestamp of this successful run, even if there's no user-specified dataset.
               (when (null current-dataset)
                 (secretary-append-tsv
                   (expand-file-name ,(concat "successes-" (symbol-name name)) secretary-memory-dir)))
               ;; Clean up, because this wasn't an excursion.
               (named-timer-cancel :secretary-excursion)
               (remove-hook 'kill-buffer-hook #'secretary--check-return-from-excursion))
           ;; All below this line will happen for both queries and excursions, success or no.
           (advice-remove 'abort-recursive-edit #'secretary--after-cancel-do-things)
           ;; maybe the reason the variable's always nil
           ;; (when (called-interactively-p 'any)
             ;; (setq secretary--excursion-buffers nil))
           )))))

(defmacro secretary-defquery (name args &rest body)
  "Boilerplate wrapper for `cl-defun'.
To see what it expands to, visit secretary-tests.el and read the
tests of this macro.

Manages the external variables `secretary--current-fn' and
`secretary--queue', zeroes `-item-dismissals' on success, and
advises `abort-recursive-edit' (in common parlance C-g). If you
use a simple `defun' in lieu of this wrapper, you must replicate
these features!

In BODY, you have access to the extra temporary variable:
- \"current-dataset\" which is a reference to (secretary-item-dataset (secretary--item-by-fn secretary--current-fn))."
  (declare (indent defun) (doc-string 3))
  (let* ((parsed-body (macroexp-parse-body body))
         (declarations (car parsed-body))
         (new-body (cdr parsed-body)))
    `(cl-defun ,name ,args
       ;; Ensure it's always interactive
       ,@(if (member 'interactive (-map #'car-safe declarations))
             declarations
           (-snoc declarations '(interactive)))
       (setq secretary--current-fn #',name)
       (unless (secretary--item-by-fn secretary--current-fn)
         (error "%s not listed in secretary-items" (symbol-name secretary--current-fn)))
       (advice-add 'abort-recursive-edit :before #'secretary--after-cancel-do-things)
       (let ((current-dataset (secretary-item-dataset
                               (secretary--item-by-fn secretary--current-fn))))
         (unwind-protect
             (prog1 (progn ,@new-body)
               (setq secretary--queue
                     (cl-remove secretary--current-fn secretary--queue :count 1))
               (setf (secretary-item-dismissals
                      (secretary--item-by-fn secretary--current-fn))
                     0)
               ;; TODO: actually, just increment a lisp variable, later synced
               ;;       to disk. Let's get around to having a big list instead of a
               ;;       separate var for each thing.
               ;; Save timestamp of this successful run.
               (when (null current-dataset)
                 (secretary-append-tsv
                   (expand-file-name ,(concat "successes-" (symbol-name name)) secretary-memory-dir))))
           (advice-remove 'abort-recursive-edit #'secretary--after-cancel-do-things))))))

(defmacro secretary-defexcursion (name arglist &optional docstring &rest body)
  "Boilerplate wrapper for `cl-defun'.
To see what it expands to, visit secretary-tests.el and read the
tests of this macro.  Alternatively, try something like

(macroexpand '(secretary-defpresenter foo (x)
                 \"docstr\"
                 (bar)))

Manages the external variables `secretary--current-fn' and
`secretary--queue', zeroes `-item-dismissals' on success, and
advises `abort-recursive-edit' (in common parlance C-g). If you
use a simple `defun' in lieu of this wrapper, you must replicate
these features!

In BODY, you have access to the extra temporary variable:
- \"current-dataset\" which is a reference to (secretary-item-dataset (secretary--item-by-fn secretary--current-fn))."
  (declare (indent defun) (doc-string 3))
  (unless (stringp docstring)
    (push docstring body))
  (let* ((interactive-spec (and (eq 'interactive (car-safe (car body)))
                                (car-safe (cdr (car body)))))
         (new-body (if interactive-spec
                       (cdr body)
                     (cons (car body) (cdr body)))))
    `(cl-defun ,name ,arglist
       ,@(cond ((and (stringp docstring)
                     interactive-spec)
                (list docstring
                      `(interactive ,interactive-spec)))
               ((stringp docstring)
                (list docstring
                      '(interactive)))
               (interactive-spec
                (list `(interactive ,interactive-spec)))
               (t
                (list '(interactive))))
       (setq secretary--current-fn #',name)
       (unless (secretary--item-by-fn secretary--current-fn)
         (error "%s not listed in secretary-items" (symbol-name secretary--current-fn)))
       (unless (called-interactively-p 'any) ;; allow manual use via M-x without triggering shenanigans
         (add-hook 'kill-buffer-hook #'secretary--check-return-from-excursion)
         (named-timer-run :secretary-excursion (* 5 60) nil #'secretary--stop-watching-excursion))
       (let ((current-dataset (secretary-item-dataset
                               (secretary--item-by-fn secretary--current-fn))))
         (unwind-protect
             (progn
               ,@new-body
               (setq secretary--queue
                     (cl-remove secretary--current-fn secretary--queue :count 1))
               (keyboard-quit))  ;; REVIEW: sane?
           ;; If something in BODY broke, clean up. The keyboard-quit above
           ;; means we never arrive here on success.
           (remove-hook 'kill-buffer-hook #'secretary--check-return-from-excursion)
           (named-timer-cancel :secretary-excursion))))))


;;;; Commands

(defun secretary-decrement-date ()
  (interactive nil secretary-chat-mode)
  (secretary-set-date (ts-dec 'day 1 secretary--date)))

(defun secretary-increment-date ()
  (interactive nil secretary-chat-mode)
  (secretary-set-date (ts-inc 'day 1 secretary--date)))

(defun secretary-set-date-today ()
  (interactive)
  (secretary-set-date (ts-now)))

(defun secretary-set-date (&optional ts)
  (interactive)
  (require 'org)
  (if ts
      (setq secretary--date ts)
    (let* ((time (ts-format "%T"))
           (new-date (org-read-date))
           (new-datetime (ts-parse (concat new-date " " time))))
      (setq secretary--date new-datetime)))
  (secretary-emit "Operating as if the date is " (ts-format "%x" secretary--date) "."))


;;;; Library for handling datasets
;; TODO: Improve names

(defun secretary--get-all-entries-in-tsv (path)
  "Return the contents of a .tsv at PATH as a Lisp list."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (flush-lines (rx bol eol))
    (let ((rows (s-split "\n" (buffer-string))))
      (--map (s-split "\t" it) rows))))

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
  (if (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents-literally path)
        (let (x)
          (while (search-forward (ts-format "%F" ts) nil t)
            (push (split-string (buffer-substring (line-beginning-position)
                                                  (line-end-position))
                                "\t")
                  x)
            (goto-char (line-end-position)))
          x))
    (warn "File doesn't exist: %s" path)
    nil))

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

(defun secretary--count-successes-today (fn)
  (let ((dataset (secretary-item-dataset (secretary--item-by-fn fn)))
        (log (expand-file-name (concat "successes-" (symbol-name fn)) secretary-memory-dir)))
    (if (and dataset
             (f-exists-p dataset))
        (length (secretary--get-entries-in-tsv dataset))
      (if (f-exists-p log)
          (length (secretary--get-entries-in-tsv log))
        (message "No dataset or log file found for %s." (symbol-name fn))
        0))))

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
         (posted (ts-format "%s.%7N")) ;; 7 digits matches `ts-unix' and `float-time'
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
   (concat (unless secretary-debug-p " ")
           "*" secretary-ai-name ": Buffer focus log*")))

(defun secretary-buffer-info-buffer ()
  (get-buffer-create
   (concat (unless secretary-debug-p " ")
           "*" secretary-ai-name ": Buffer info*")))

(defcustom secretary-buffer-focus-log-file-name
  (convert-standard-filename "~/buffer-focus.tsv")
  nil
  :group 'secretary
  :type 'string)

(defcustom secretary-buffer-info-file-name
  (convert-standard-filename "~/buffer-info.tsv")
  nil
  :group 'secretary
  :type 'string)

(defun secretary--save-buffer-logs-to-disk ()
  (secretary--transact-buffer-onto-file (secretary-buffer-focus-log-buffer)
                                        secretary-buffer-focus-log-file-name)
  (secretary--transact-buffer-onto-file (secretary-buffer-info-buffer)
                                        secretary-buffer-info-file-name))

;; TODO: When buffer major mode changes, count it as a new buffer. Note that
;;       (assoc buf secretary-known-buffers) will still work.
;; TODO: When eww url changes, count it as a new buffer
;; TODO: When counting it as a new buffer, record a field for "previous uuid" just in case data analyst wants to merge these observations
;; TODO: Optimize?
(defun secretary-log-buffer (&optional _arg)
  "Log the buffer just switched to.
Put this on `window-buffer-change-functions' and
`window-selection-change-functions'."
  (unless (minibufferp)
    (let* ((buf (current-buffer))
           (mode (symbol-name (buffer-local-value 'major-mode buf)))
           (known (assoc buf secretary-known-buffers))
           (timestamp (s-pad-right 18 "0" (number-to-string (ts-unix (ts-now)))))
           (visiting (if (equal mode "dired-mode")
                         default-directory
                       buffer-file-name))
           (eww-url (when (equal mode "eww-mode")
                      (eww-current-url)))
           (exist-record (unless (and known
                                      ;; TODO: make a new exist-record when mode changes
                                      (equal mode (nth 4 known))) ;; doesnt do it
                           (list buf
                                 (secretary--new-uuid)
                                 (buffer-name)
                                 visiting
                                 mode
                                 timestamp ;; time the buffer was first opened
                                 eww-url
                                 (when (equal mode "exwm-mode") exwm-class-name)
                                 (when (equal mode "exwm-mode") exwm-title)
                                 )))
           (focus-record (list timestamp ;; time the buffer was switched to
                               ;; the buffer's uuid
                               (if known (cadr known) (cadr exist-record))
                               )))
      (unless (eq secretary-last-buffer buf) ;; you only entered and left minibuffer e.g.
        (setq secretary-last-buffer buf)
        (unless known
          (push exist-record secretary-known-buffers)
          (with-current-buffer (secretary-buffer-info-buffer)
            (goto-char (point-max))
            (insert "\n" (string-join (cdr exist-record) "\t"))))
        (with-current-buffer (secretary-buffer-focus-log-buffer)
          (goto-char (point-max))
          (insert "\n" (string-join focus-record "\t")))))))


;;;; Welcomers
;; TODO: Use consistent naming.

(defun secretary-execute (&optional queue)
  "Call every function from QUEUE, default `secretary--queue'.
Does some checks and sets up a good environment, in particular
nulling the 'buffer-predicate frame parameter so that no buffers
spawned by the functions will be skipped by
`switch-to-next-buffer'."
  (interactive)
  (named-timer-cancel :secretary-excursion) ;; hygiene
  (let ((bufpred-backup (frame-parameter nil 'buffer-predicate)))
    (unwind-protect
        (progn
          (set-frame-parameter nil 'buffer-predicate nil)
          (pop-to-buffer (secretary-buffer-chat))
          (dolist (f (or queue secretary--queue))
            (secretary-call-fn-check-dismissals f)))
      ;; FIXME: Actually, this will executed at the first keyboard-quit, so we
      ;; will never have a nil predicate. We need to preserve it during an
      ;; excursion.
      (set-frame-parameter nil 'buffer-predicate bufpred-backup))))

(defalias 'secretary-resume #'secretary-execute)

(defconst secretary-debug-no-timid nil)

;; (secretary--count-successes-today #'secretary-present-diary)

;; TODO: Wait far longer than 5 mins if the idle time never becomes high and
;; the buffer never strays outside excursion-buffers or org-capture.


(defun secretary--call-timidly ()
  "Butt in if any queries are pending."
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
    (run-with-timer 1 nil #'secretary-execute)))
;; (secretary--call-timidly)
;; (named-timer-run :secretary-attempt (* 60 60) (* 60 60) #'secretary--call-timidly)

(defun secretary-new-session ()
  (interactive)
  (setq secretary--date (ts-now))
  (setq secretary--queue (-filter #'secretary--pending-p (secretary--enabled-items)))
  (secretary-execute))

(defun secretary-new-session-force-all ()
  (interactive)
  (setq secretary--date (ts-now))
  (setq secretary--queue (secretary--enabled-items))
  (secretary-execute))

(defun secretary-call-from-idle ()
  "Called by idleness-related hooks, doesn't do much if idle was not long."
  (unless (< secretary-length-of-last-idle secretary-idle-threshold-secs-long)
    (secretary--call-timidly)))
;; (run-with-timer 3 nil (lambda ()  (print (frame-focus-state))))
;; (secretary-call-from-idle)


;;;; Persistent variables

(defvar secretary-memory
  nil
  "Alist of all relevant variable values.
We log these values to disk at `secretary-mem-loc', so we can
recover older values as needed. Why not custom-file? People are
always wiping their custom-file, admittedly for a reason, but
this is a matter of greater importance.")

(defun secretary-memory-put (key value)
  "In `secretary-memory', assign KEY to VALUE.
Replace if KEY already exists. Destructive."
  (if (assoc key secretary-memory)
      (map-put! secretary-memory key value)
    (setq secretary-memory (map-insert secretary-memory key value))))

(defun secretary-memory-apply (key fn &rest args)
  "In `secretary-memory', at KEY, apply FN with extra args ARGS.
Destructive; modifies in place."
  (secretary-memory-put
   key (apply #'funcall fn (map-elt secretary-memory key) args)))

; (setq secretary-memory (map-insert secretary-memory 'secretary-ai-name "foo"))

;"/home/me/doom-emacs/secretary/memory.tsv"
;(secretary--last-value-of-variable 'secretary-debug-p)
;(secretary-save-memory)

;; Guess this works, just takes up a lot of disk space over time.
(defun secretary-save-memory ()
  (cl-loop for cell in secretary-memory
           do (progn
                (secretary-append-tsv secretary-mem-loc
                  (prin1-to-string (car cell))
                  (prin1-to-string (cdr cell))))))

;; DEPRECATED
(defun secretary--recover-memory-deterministic ()
  (defun secretary--last-value-of-variable (var)
    (let* ((table (nreverse (secretary--get-all-entries-in-tsv secretary-mem-loc)))
           (ok t))
      (cl-block nil
        (while ok
          (let ((row (pop table)))
            (when (eq (read (nth 1 row)) var)
              (setq ok nil)
              (cl-return (read (nth 2 row)))))))))
  (cl-loop for cell in secretary-memory
           do (map-put! secretary-memory (car cell)
                        (secretary--last-value-of-variable (car cell)))))

(defun secretary--recover-memory-without-reference ()
  "Grab the newest values from file at `secretary-mem-loc' and
assign them in `secretary-memory'."
  (let* ((table (-map #'cdr (nreverse (secretary--get-all-entries-in-tsv secretary-mem-loc)))))
    (while (/= 0 (length table))
      (let ((row (pop table)))
        (unless (member (read (car row)) (-map #'car secretary-memory))
          (setq secretary-memory (cons (cons (read (car row)) (read (cadr row)))
                                       secretary-memory)))))))

;(secretary--recover-memory-without-reference)

(defcustom secretary-load-vars-hook nil
  "Invoked right after populating `secretary-memory' from disk.
The most recent values are therefore available.  If you've
previously saved data in that list (typically via
`secretary-save-vars-hook'), it should now be back even if Emacs
has restarted, so you can run something like the following.

    (setq my-var (map-elt secretary-memory 'my-var))"
  :group 'secretary
  :type '(repeat function))

(defcustom secretary-save-vars-hook nil
  "Invoked right before saving `secretary-memory' to disk.
You should add to that list anything you want to persist across
reboots, using something like the following.

    (secretary-memory-put 'my-var my-var)

Of course, you can do it at any time, doing it via this hook
isn't needed unless you do a lot of things with 'my-var at
indeterminate times."
  :group 'secretary
  :type '(repeat function))

(defun secretary--restore-item-metadata-from-mem ()
  (dolist (disk-item (map-elt secretary-memory 'secretary-items))
    (unless (ignore-errors (let* ((fn-sym (secretary-item-fn disk-item))
                                   (active-item (when (fboundp fn-sym)
                                                  (secretary--item-by-fn fn-sym))))
                             ;; if it reflects something we have defined currently
                             (when (fboundp fn-sym)
                               ;;  update the current one's :dismissals etc to match on-disk values.
                               (setf (secretary-item-dismissals active-item)
                                 (secretary-item-dismissals disk-item))
                               (setf (secretary-item-last-called active-item)
                                 (secretary-item-last-called disk-item)))
                             t))
      (warn
        (s-join "\n"
          '("secretary--restore-item-metadata-from-mem failed. "
             " Did you change the secretary-item defstruct?"
             " Proceeding because not critical.  May self-correct next sync."))))))

;; TODO: Calc reasonable defaults from dataset contents
(defun secretary--restore-variables-from-disk ()
  (secretary--recover-memory-without-reference)
  (setq secretary--last-online (ts-fill
                                (or (map-elt secretary-memory 'secretary--last-online)
                                    (make-ts :unix 0)))) ;; TODO emit error if there are older non-nil values and it's now nil
  (when (and secretary-chat-log-file-name
             (file-exists-p secretary-chat-log-file-name))
    (let ((chatfile-modtime-unix
           (time-convert (file-attribute-modification-time
                          (file-attributes secretary-chat-log-file-name))
                         'integer))
          (remembered (map-elt secretary-memory 'secretary--last-chatted)))
      (setq secretary--last-chatted
            (ts-fill
             (make-ts :unix (max chatfile-modtime-unix
                                 (if secretary--last-chatted
                                     (ts-unix secretary--last-chatted)
                                   0)
                                 (if remembered
                                     (ts-unix remembered)
                                   0)))))))
  (when (and (boundp 'secretary--last-chatted)
             (ts-p secretary--last-chatted)
             (ts< secretary--last-online secretary--last-chatted))
    (setq secretary--last-online secretary--last-chatted))
  (setq secretary--idle-beginning secretary--last-online)
  (secretary--restore-item-metadata-from-mem)
  (run-hooks 'secretary-load-vars-hook))

;; (secretary-memory-put 'secretary-items secretary-items)

;; TODO: ensure in some way that restore-variables has been called before we
;; proceed, so we don't accidentally blank out our files.  Sanity checks:
;; variables to write are not null? At least when file on disk contains data?
;; Catch a mass-blanking event: when most variables suddenly null.
(defun secretary--save-variables-to-disk ()
  (secretary-memory-put 'secretary--last-online secretary--last-online)
  ;; TODO: Should I encode like this or keep track of secretary-items as a
  ;; whole? Because the latter is a bit clunky. Also TODO: I notice I could
  ;; assign the entire structs to a variable name that's the same as the
  ;; function name, is that sensible? Could hook the :constructor to do that
  ;; automatically.
  ;; (dolist (i secretary-items)
  ;;   (secretary-memory-put (secretary-item-fn i) i))
  (secretary-memory-put 'secretary-items secretary-items)
  (make-directory secretary-memory-dir t)
  (when secretary-chat-log-file-name
    (secretary-write-safely (with-current-buffer (secretary-buffer-chat) (buffer-string))
                            secretary-chat-log-file-name))
  (run-hooks 'secretary-save-vars-hook)
  (secretary-save-memory))


;;; Modes and keys

(defconst secretary-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'secretary-resume)
    (define-key map (kbd "+") #'secretary-increment-date)
    (define-key map (kbd "-") #'secretary-decrement-date)
    (define-key map (kbd "0") #'secretary-set-date-today)
    (define-key map (kbd "d") #'secretary-set-date)
    (define-key map (kbd "q") #'bury-buffer)
    (define-key map (kbd "?") #'secretary-dispatch)
    (define-key map (kbd "h") #'secretary-dispatch)
    map))

(define-derived-mode secretary-chat-mode text-mode "Secretary-Chat"
  :group 'secretary-chat)

(transient-define-prefix secretary-dispatch ()
  ["General actions"
   ("q" "Quit the chat" bury-buffer)
   ;; ("v" "Visit directory of log files" (lambda () (dired secretary-memory-dir)))
   ]
  ;; TODO: make these nonexiting
  [;; (lambda () (concat "Date (" (ts-format "%x" secretary--date) ")"))
   "Date"
   ("0" "Reset date to today (default)" secretary-set-date-today :transient t)
   ("-" "Decrement the date" secretary-decrement-date :transient t)
   ("+" "Increment the date" secretary-increment-date :transient t)
   ("d" "Set date..." secretary-set-date :transient t)
   ])

;; This needs careful coding.
;; Should the transient take care of resuming the query or should secretary-read do it?
;; The former of course! Who knows what actions we'll want to put in.
;;
;; So we need to be able to append `secretary-resume' to some of these commands
;; but not others. Assume that the previous prompt died calling this dispatch
;; without modifying `secretary--queue'.
;;
;; We could also provide information taken from the current fn, perhaps its
;; docstring. Where to put a lambda to insert a string?
(transient-define-prefix secretary-midprompt-dispatch ()
  ["General actions"
   ("q" "Quit" bury-buffer)
   ]
  ["Date"
   ("t" "Reset date to today (default)" secretary-set-date-today)
   ("-" "Decrement the date" (lambda () (secretary-decrement-date) (secretary-resume)))
   ("+" "Increment the date" secretary-increment-date)
   ("d" "Set date..." secretary-set-date)
   ])


;;;; "Main"

(defun secretary--check-for-time-anomalies ()
  "Check for timestamps that don't look right.
Good to run after enabling `secretary-mode' or changing
`secretary-items'."
  (let* ((datasets (--map (secretary-item-dataset it) secretary-items))
         (logs (list secretary-idle-file-name
                     secretary-buffer-focus-log-file-name))
         (files (-non-nil (append datasets logs)))
         (anomalous-files nil))
    (dolist (f files)
      (when (f-exists-p f)
        (let ((stamps (->> (secretary--get-all-entries-in-tsv f)
                           (map-keys) ;; first elem of each row
                           (-map #'string-to-number))))
          (unless (<= stamps)
            (message (concat "Timestamps not strictly increasing in: " f)))
          ;; Check that no timestamp bigger than current time.
          (if (--any-p (> it (float-time)) stamps)
              (push f anomalous-files)))))
    (when anomalous-files
      (warn "%s"
            (->> (append '("Secretary: Anomalous timestamps found in my logs."
                           "You probably have or have had a wrong system clock."
                           "These files have timestamps exceeding the current time:")
                         anomalous-files)
                 (s-join "\n"))))))

(defun secretary--keepalive ()
  "Re-start the :secretary timer if dead.
Very good while hacking on the package."
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
  "Wake up the secretary."
  :global t
  (if secretary-mode
      ;; Check to see whether it should even turn on.
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
              ((and (eq window-system 'x) ;; true also under XWayland, so this condition must be below any check for Wayland
                    (setq secretary--x11idle-program-name
                          (seq-find #'executable-find '("x11idle" "xprintidle"))))
               (setq secretary--idle-seconds-fn #'secretary--x11-idle-seconds)
               t)
              ((symbol-value 'secretary-fallback-to-emacs-idle-p)
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
                          '(secretary-items))
                 t
               (message "Needed variables not set, read manual or do %s."
                        "M-x load-library secretary-config")
               (secretary-mode 0)
               nil))
        ;; All OK, turn on.
        (mkdir "/tmp/secretary" t)
        (f-write (number-to-string (emacs-pid)) 'utf-8 "/tmp/secretary/pid")
        (add-function :after after-focus-change-function #'secretary-log-buffer)
        (add-hook 'window-buffer-change-functions #'secretary-log-buffer)
        (add-hook 'window-selection-change-functions #'secretary-log-buffer)
        (add-hook 'after-init-hook #'secretary--restore-variables-from-disk -90)
        (add-hook 'after-init-hook #'secretary--init-r)
        (add-hook 'after-init-hook #'secretary--check-for-time-anomalies)
        (add-hook 'after-init-hook #'secretary--start-next-timer 90)
        (named-timer-run :secretary-keepalive 300 300 #'secretary--keepalive)
        (when after-init-time
          (progn
            (when (or (null secretary--last-online)
                      (= 0 (ts-unix secretary--last-online)))
              (secretary--restore-variables-from-disk))
            (secretary--init-r)
            (secretary--check-for-time-anomalies)
            (secretary--user-is-active))))
    ;; Turn off.
    (secretary--save-variables-to-disk)
    (setq secretary--idle-seconds-fn nil)
    (ignore-errors
      (f-delete "/tmp/secretary/pid"))
    (remove-function after-focus-change-function #'secretary-log-buffer)
    (remove-hook 'window-buffer-change-functions #'secretary-log-buffer)
    (remove-hook 'window-selection-change-functions #'secretary-log-buffer)
    (remove-hook 'after-init-hook #'secretary--restore-variables-from-disk)
    (remove-hook 'after-init-hook #'secretary--init-r)
    (remove-hook 'after-init-hook #'secretary--check-for-time-anomalies)
    (remove-hook 'after-init-hook #'secretary--start-next-timer)
    (named-timer-cancel :secretary)
    (named-timer-cancel :secretary-keepalive)))

(provide 'secretary)

;;; secretary.el ends here
