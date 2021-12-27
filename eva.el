;;; eva.el --- Emacs virtual assistant -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021 Martin Edström

;; Author: Martin Edström <meedstrom@teknik.io>
;; URL: https://github.com/meedstrom/eva
;; Version: 0.5-pre
;; Created: 2020-12-03
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1") (ts "0.3-pre") (s "1.12") (dash "2.19") (f "0.20.0") (ess "18.10.3snapshot") (named-timer "0.1") (transient "0.3.6"))

;; This file is not part of GNU Emacs.

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

;; See ./README.org or website: https://github.com/meedstrom/eva.
;; See also ./doc/eva.org or the Info node (eva).

;;; Code:

;; builtins
(require 'seq)
(require 'map)
(require 'subr-x)
(require 'cl-lib)
(require 'find-func) ;; find-library-name
(require 'transient) ;; Emacs 28 builtin

;; external
(require 'ts) ;; essential
(require 'named-timer) ;; essential
(require 'ess) ;; TODO: Drop this
(require 'dash)
(require 's)
(require 'f) ;; f-read and f-append are just nice

;; Mollify the byte compiler
(declare-function calendar-check-holidays "holidays")
(declare-function calendar-current-date "calendar")
(declare-function run-ess-r "ess-r-mode")
(declare-function ess-process-live-p "ess-inf")
(declare-function ess-execute "ess-inf")
(declare-function eww-current-url "eww")
(declare-function notifications-notify "notifications")
(declare-function notifications-get-capabilities "notifications")
(declare-function org-mac-idle-seconds "org-clock")
(declare-function org-read-date "org")
(defvar exwm-class-name)
(defvar exwm-title)

;; DEPRECATED: Old variable names
(defvaralias 'eva-ai-name 'eva-va-name "Renamed 2021-08-23.")
(defvaralias 'eva-dbg-fn 'eva-debug-fn "Renamed 2021-08-23.")


;;; Some user options

(defgroup eva nil "The Emacs virtual assistant."
  :prefix "eva-"
  :group 'convenience)


(defcustom eva-va-name "Alfred"
  "Your VA's name."
  :group 'eva
  :type 'string
  :risky t)

(defcustom eva-user-birthday nil
  "Your birthday, an YYYY-MM-DD string."
  :group 'eva
  :type 'string
  :safe t)

(defcustom eva-user-name
  (if (s-blank? user-full-name)
      "Mr. Bond"
    (-first-item (s-split " " user-full-name)))
  "Name by which you prefer the VA to address you."
  :group 'eva
  :type 'string
  :safe t)

(defcustom eva-user-short-title "Master"
  "A short title for you that works on its own.
May be capitalized or not, e.g. \"Sojourner\" or \"sir\"."
  :group 'eva
  :type 'string
  :safe t)

(defcustom eva-sit-long 1
  "Duration in seconds to pause for effect.
See also `eva-sit-medium' and `eva-sit-short'."
  :group 'eva
  :type 'float
  :safe t)

(defcustom eva-sit-medium .8
  "Duration in seconds to pause for effect.
See also `eva-sit-long' and `eva-sit-short'."
  :group 'eva
  :type 'float
  :safe t)

(defcustom eva-sit-short .5
  "Duration in seconds to pause for effect.
See also `eva-sit-long' and `eva-sit-medium'."
  :group 'eva
  :type 'float
  :safe t)

(defcustom eva-presumptive nil
  "Whether to skip some prompts and assume yes."
  :group 'eva
  :type 'boolean)

(defcustom eva-cache-dir-path
  (expand-file-name "eva" user-emacs-directory)
  "Directory for persistent files (not user datasets)."
  :group 'eva
  :type 'directory
  :risky t)


;;; Common library

(defcustom eva-debug init-file-debug
  "Whether to do debug stuff."
  :group 'eva
  :type 'boolean)

(defcustom eva-debug-fn (when eva-debug #'message)
  "Control the behavior of `eva-dbg'.
Recommended options are nil, `message', `warn' and `error'."
  :group 'eva
  :type 'function
  :safe t)

(defcustom eva-init-r t
  "Whether to initialize an R session on startup."
  :group 'eva
  :type 'boolean)

(defvar eva--buffer-r nil)

(defvar eva-curr-fn nil)

(defvar eva-curr-dataset nil)

(defvar eva-curr-item nil)

(defvar eva-date (ts-now)
  "Date to which to apply the current fn.
Can be set anytime to override the date to which some queries
apply, for example to log something for yesterday.

Mind that starting a new session resets this to today.")

(defvar eva--r-process nil)

(defun eva--init-r ()
  "Spin up an R process and load needed R libraries.
Uses `run-ess-r' which is full of sanity checks (e.g. for cygwin
and text encoding), but creates an interactive R buffer which
unfortunately may surprise the user when they go to work on their
own R project."
  (let ((default-directory (f-dirname (find-library-name "eva"))))
    (when eva-init-r
      (unless (and (buffer-live-p eva--buffer-r)
                   (ess-process-live-p eva--r-process))
        (save-window-excursion
          (setq eva--buffer-r (run-ess-r)))
        (bury-buffer eva--buffer-r)
        ;; gotcha: only use `ess-with-current-buffer' for temp output buffers, not
        ;; for the process buffer
        (with-current-buffer eva--buffer-r
          (setq eva--r-process (get-process ess-local-process-name))
          ;; TODO: How to check if the script errors out?
          (ess-execute "source(\"init.R\")" 'buffer))))))

(defun eva-dbg (&rest strings)
  "Concat STRINGS and print them via function `eva-debug-fn'.
Do nothing if that is nil.  Note that we don't do the
`format-message' business usual for `error' and its cousins.
Use the real `error' for that."
  (when eva-debug-fn
    (funcall eva-debug-fn (s-join " " strings))))

;; TODO: Catch typos like 03 meaning 30 minutes, not 3 hours.
(defun eva-parse-time-amount (input)
  "Translate INPUT from hours or minutes into minutes.
If INPUT contains no \"h\" or \"m\", assume numbers above 20 are
minutes and numbers below are hours."
  (declare (pure t) (side-effect-free t))
  (let ((numeric-part (string-to-number input)))
    (cond ((= 0 numeric-part) ;; strings without any number result in 0
           nil) ;; save as a NA observation
          ((and (s-matches? "h.*m" input) (> numeric-part 0))
           (warn "I'm not sophisticated enough to parse that"))
          ((s-matches? "h" input)
           (* 60 numeric-part))
          ((s-matches? "m" input)
           numeric-part)
          ((-> numeric-part (>= 20))
           numeric-part)
          (t
           (* 60 numeric-part)))))

(defun eva-coerce-to-hh-mm (input)
  "Coerce from INPUT matching HH:MM, HH or H, to HH:MM (24-h).
If \"am\" or \"pm\" present, assume input is in 12-hour clock."
  (declare (pure t) (side-effect-free t))
  (unless (s-matches? (rx num) input)
    (error "%s" (concat "Invalid time: " input)))
  (let* ((hhmm (or (cdr (s-match (rx (group (= 2 num)) punct (group (= 2 num)))
                                 input))
                   (cdr (s-match (rx (group (= 1 num)) punct (group (= 2 num)))
                                 input))
                   (s-match (rx (= 2 num)) input)
                   (s-match (rx (= 1 num)) input)))
         (hour (string-to-number (car hhmm)))
         (minute (string-to-number (or (cadr hhmm) "00"))))
    (when (or (> hour 24)
              (and (> hour 12)
                   (s-matches? (rx (or "pm" "am")) input)))
      (error "%s" (concat "Invalid time: " input)))
    (when (and (s-contains? "pm" input)
               (/= 12 hour))
      (cl-incf hour 12))
    (when (and (s-contains? "am" input)
               (= 12 hour))
      (setq hour 0))
    (when (= 24 hour)
      (setq hour 23)
      (setq minute 59))
    (concat (when (< hour 10) "0")
            (number-to-string hour) ":"
            (when (< minute 10) "0")
            (number-to-string minute))))

(defun eva-one-decimal (string)
  "If STRING is a number, drop decimals beyond the first."
  (when (stringp string)
    (if (s-numeric? string)
        ;; leave integer as is
        string
      (car (s-match (rx (* nonl) "\." nonl) string)))))

(defmacro eva--process-output-to-string (program &rest args)
  "Like `shell-command-to-string' without the shell intermediary.
You don't need a /bin/sh.  PROGRAM and ARGS are passed on to
`call-process'."
  (declare (debug (&rest form)))
  `(with-temp-buffer
     (call-process ,program nil (current-buffer) nil ,@args)
     (buffer-string)))

(defmacro eva--process-output-to-number (program &rest args)
  "Like `shell-command-to-string' without the shell intermediary.
Also converts the result to number.  PROGRAM and ARGS are passed
on to `call-process'."
  (declare (debug (&rest form)))
  `(string-to-number (eva--process-output-to-string ,program ,@args)))


;;; Library for interactivity

(defcustom eva-chat-log-path
  (convert-standard-filename
   (expand-file-name "chat.log" eva-cache-dir-path))
  "Where to save chat log across sessions.  Can be nil."
  :group 'eva
  :type 'file
  :safe t)

(defvar eva--queue nil)

(defvar eva--midprompt-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C--") #'eva-decrement-date)
    (define-key map (kbd "C-+") #'eva-increment-date)
    (define-key map (kbd "C-0") #'eva-set-date-today)))

(defvar eva--just-typed-k nil)

(defvar eva--last-chatted nil
  "Timestamp updated whenever the chat is written to.")

(defun eva-buffer-chat ()
  "Buffer where the VA sends its messages."
  (or (get-buffer (concat "*" eva-va-name ": chat log*"))
      (let ((buf (get-buffer-create
                  (concat "*" eva-va-name ": chat log*"))))
        (with-current-buffer buf
          (eva-chat-mode)
          (setq-local auto-save-visited-mode nil)
          (setq-local require-final-newline nil)
          (buffer-disable-undo)
          (visual-line-mode)
          (and eva-chat-log-path
               (f-exists? eva-chat-log-path)
               (insert-file-contents eva-chat-log-path))
          (setq-local buffer-read-only t))
        buf)))

(defun eva--y-or-n-p-insert-k ()
  "Mostly like `y-or-n-p-insert-y'."
  (interactive)
  (delete-minibuffer-contents)
  (insert "y")
  (setq eva--just-typed-k t)
  (exit-minibuffer))

(defun eva-ynp (&rest strings)
  "Wrapper around `y-or-n-p'.
Concatenates STRINGS into one prompt, prints it to the chat
buffer, binds certain hotkeys."
  (let* (;; (default-y-or-n-p-map y-or-n-p-map)
         ;; (default-cmd (lookup-key y-or-n-p-map (kbd "k")))
         ;; TODO: Also show which log file we're applying to
         (background-info (concat "[Applying to date: "
                                  (ts-format "%Y %b %d" eva-date)
                                  "]\n"))
         (prompt (string-join strings)))
    (unwind-protect
        (progn
          (pop-to-buffer (eva-buffer-chat))
          (eva-emit prompt)
          (define-key y-or-n-p-map (kbd "h") #'eva-dispatch)
          (define-key y-or-n-p-map (kbd "<SPC>") #'eva-dispatch)
          (define-key y-or-n-p-map (kbd "k") #'eva--y-or-n-p-insert-k)
          (setq-local buffer-read-only nil)
          (let ((result (y-or-n-p (concat background-info prompt))))
            (with-silent-modifications
              (if eva--just-typed-k
                  (progn
                    (setq eva--just-typed-k nil)
                    (eva-emit-same-line " Okay..."))
                (if result
                    (eva-emit-same-line " Yes.")
                  (eva-emit-same-line " No."))))
            result))
      (setq-local buffer-read-only t)
      (dolist (x '("o" "i" "k" "<SPC>"))
        (define-key y-or-n-p-map (kbd x) #'y-or-n-p-insert-other)))))

(defun eva-read (prompt &optional collection default)
  "Wrapper for `completing-read'.
PROMPT, COLLECTION and DEFAULT are as in that function.

Echo both prompts and responses to the chat buffer, prepend
metadata to PROMPT, check for special keyword input, etc."
  (eva-emit prompt)
  (set-transient-map eva--midprompt-keymap #'minibufferp)
  (let* ((background-info (concat "[Applying to date: "
                                  (ts-format "%Y %b %d" eva-date)
                                  "]\n"))
         (extra-collection '("/skip" "/help"))
         (input (completing-read
                 (concat background-info
                         (ts-format "<%H:%M> ")
                         prompt
                         (when (stringp default)
                           (concat
                            " (default " default "): ")))
                 (append collection extra-collection)
                 nil nil nil nil
                 (when (stringp default)
                   default))))
    (eva-emit-same-line input)
    (if (eva-check-special-input input)
        input
      nil)))

(defun eva-read-string
    (prompt &optional initial-input history default-value)
  "Like `eva-read' but call `read-string' internally.
All of PROMPT, INITIAL-INPUT, HISTORY, DEFAULT-VALUE are passed
to that function, though PROMPT is prepended with extra info."
  (eva-emit prompt)
  (set-transient-map eva--midprompt-keymap #'minibufferp)
  (let* ((background-info (concat "[Applying to date: "
                                  (ts-format "%Y, %b %d]\n" eva-date)
                                  "[Type /skip to skip, or /help for help.]\n"))
         (input (read-string
                 (concat background-info
                         (ts-format "<%H:%M> ")
                         prompt)
                 initial-input
                 history
                 default-value)))
    (eva-emit-same-line input)
    (if (eva-check-special-input input)
        input
      nil)))

(defun eva-emit (&rest strings)
  "Write a line to the chat buffer, made from STRINGS.
Returns the completed string so you can pass it to `message', for
example."
  (let ((new-date-maybe (if (/= (ts-day (ts-now))
                                (ts-day eva--last-chatted))
                            (concat "\n\n"
                                    (ts-format "%A, %d %B %Y")
                                    (eva--holiday-maybe)
                                    "\n")
                          ""))
        (msg (concat "\n<" (ts-format "%H:%M") "> " (string-join strings))))
    (with-current-buffer (eva-buffer-chat)
      (goto-char (point-max))
      (with-silent-modifications
        (delete-blank-lines)
        (insert new-date-maybe)
        (insert msg))))
  (setq eva--last-chatted (ts-fill (ts-now)))
  (string-join strings))

(defun eva-emit-same-line (&rest strings)
  "Print STRINGS to the chat buffer without newline."
  (let ((msg (string-join strings)))
    (with-current-buffer (eva-buffer-chat)
      (goto-char (point-max))
      (with-silent-modifications
        (insert msg)))
    (setq eva--last-chatted (ts-fill (ts-now)))
    msg))


;;; Library for greeting messages

(defvar eva-greetings
  '((concat "Welcome back, " eva-user-short-title ".")
    (concat "Nice to see you again, " eva-user-name ".")
    (concat "Greetings, " eva-user-name "."))
  "Greeting phrases which can initiate a conversation.
A quoted list of expressions.")

;; NOTE: I considered making external variables for morning, day and evening
;;       lists, but users might also want to change the daytime boundaries or
;;       even add new boundaries.
(defun eva-daytime-appropriate-greetings ()
  "Return different greeting strings appropriate to daytime."
  (cond ((> 5 (ts-hour (ts-now)))
         (list (concat "You're up late, " eva-user-short-title)
               "Burning the midnight oil?"))
        ((> 10 (ts-hour (ts-now)))
         (list (concat "Good morning, " eva-user-name ".")
               "Good morning!"
               "The stars shone upon us last night."))
        ((> 16 (ts-hour (ts-now)))
         (list "Good day!"))
        (t
         (list "Good evening!"
               "Pleasant evening to you!"))))

(defun eva--holiday-maybe ()
  "If today's a holiday, return a suitable string for `eva-emit'.
Else return a blank string."
  (require 'calendar)
  (require 'holidays)
  (if-let (foo (calendar-check-holidays (calendar-current-date)))
      (concat " -- " (s-join " " foo))
    ""))

(defun eva-greeting-curt ()
  "Return a greeting appropriate in the midst of a workday.
Because if you've already exchanged good mornings, it's weird to
do so again."
  (seq-random-elt `("Hello" "Hi" "Hey")))

(defun eva-greeting ()
  "Return a greeting string."
  (let ((bday (ts-parse eva-user-birthday)))
    (cond ((equal (ts-format "%F" bday) (ts-format "%F" (ts-now)))
           (concat "Happy birthday, " eva-user-name "."))
          ;; If it's morning, always use a variant of "good morning"
          ((> 10 (ts-hour (ts-now)) 5)
           (eval (seq-random-elt (eva-daytime-appropriate-greetings))
                 t))
          (t
           (eval (seq-random-elt
                  (append eva-greetings
                          (-list (eva-daytime-appropriate-greetings))))
                 t)))))

(defun eva-greeting-standalone ()
  "Return a greeting that expects to be followed by nothing.
No prompts, no debug message, no info.  Suitable for
`notifications-notify' or `startup-echo-area-message'.  A
superset of `eva-greeting'.  Mutually exclusive with
`eva-greeting-curt'."
  (eval (seq-random-elt
         (append eva-greetings
                 (-list (eva-daytime-appropriate-greetings))
                 '("How may I help?")))))


;;; Library for chimes

(defcustom eva-chime-sound-path
  (convert-standard-filename
   (expand-file-name
    ;; From https://freesound.org/people/josepharaoh99/sounds/380482/
    "assets/Chime Notification-380482.wav"
    ;; From https://bigsoundbank.com/detail-0319-knock-on-a-glass-door-1.html
    ;; "assets/DOORKnck_Knock on a glass door 1 (ID 0319)_BSB.wav"
    (f-dirname (find-library-name "eva"))))
  "Sound to play when a welcomer is triggered unannounced."
  :group 'eva
  :type 'file)

(defcustom eva-play-sounds nil
  "Whether to play sounds."
  :group 'eva
  :type 'boolean)

(defun eva--chime-aural ()
  "Play a sound."
  (and eva-play-sounds
       (executable-find "aplay")
       (f-exists? eva-chime-sound-path)
       (start-process "aplay" nil "aplay" eva-chime-sound-path)))

(defun eva--chime-visual ()
  "Give the fringes a flash of color and fade out."
  (let ((colors '((.1 . "green")
                  (.2 . "#aca")
                  (.3 . "#7a7")
                  (.4 . "#696")
                  (.5 . "#363"))))
    (let ((orig (face-background 'fringe)))
      (dolist (x colors)
        (run-with-timer (car x) nil
                        #'set-face-background 'fringe (cdr x)))
      (run-with-timer .6 nil #'set-face-background 'fringe orig))

    (when (facep 'solaire-fringe-face)
      (let ((orig (face-background 'solaire-fringe-face)))
        (dolist (x colors)
          (run-with-timer (car x) nil
                          #'set-face-background 'solaire-fringe-face (cdr x)))
        (run-with-timer .6 nil
                        #'set-face-background 'solaire-fringe-face orig)))
    nil))


;;; Library for files

(defun eva--transact-buffer-onto-file (buffer path)
  "Append contents of BUFFER to file at PATH, emptying BUFFER."
  (mkdir (f-dirname path) t)
  (with-current-buffer buffer
    (eva-append-safely (buffer-string) path)
    (delete-region (point-min) (point-max))))

(defun eva--count-successes-today (fn)
  "Add up occurrences of timestamps for FN in related log files."
  (let ((dataset (eva-item-dataset (eva-item-by-fn fn)))
        (log (expand-file-name (concat "successes-" (symbol-name fn))
                               eva-cache-dir-path)))
    (if (and dataset
             (f-exists? dataset))
        (length (eva-tsv-entries-by-date dataset))
      ;; FIXME: this has only unixstamps, while eva-tsv-entries-by-date scans
      ;;        for datestamps, so this will always be zero
      (if (f-exists? log)
          (length (eva-tsv-entries-by-date log))
        (message "No dataset or log file found for %s %s."
                 (symbol-name fn)
                 "(may simply not exist yet)")
        0))))

(defun eva-write-safely (text path)
  "Write TEXT to file at PATH if the content differs.
Also revert any buffer visiting it, or signal an error if there
are unsaved changes."
  (let ((buf (find-buffer-visiting path)))
    (and buf
         (buffer-modified-p buf)
         (error "Unsaved changes in open buffer: %s" (buffer-name buf)))
    (unless (and (f-exists? path)
                 (string= text (f-read path 'utf-8)))
      (f-write text 'utf-8 path)
      (and buf (with-current-buffer buf
                 (revert-buffer))))))

(defun eva-append-safely (text path)
  "Append TEXT to file at PATH.
Also revert any buffer visiting it, or warn if there are unsaved
changes and append to a file named PATH_errors."
  (let ((buf (find-buffer-visiting path))
        (errors-path (concat path "_errors")))
    (and buf
         (buffer-modified-p buf)
         (warn "Unsaved changes in open buffer: %s, writing to %s"
               (buffer-name buf) errors-path)
         (f-append text 'utf-8 errors-path))
    (unless (= 0 (length text)) ;; no unnecessary disk writes
      (f-append text 'utf-8 path)
      (and buf (with-current-buffer buf
                 (revert-buffer))))))

;; NOTE: Actually unused inside this package, but may be useful.
;; WONTFIX: check for recent activity (user awake thru the night) and keep
;;          returning t
(defun eva-logged-today-p (path)
  "Check for references to today's date inside file at PATH.
Does this by searching for a YYYY-MM-DD datestamp.  Returns t on
success, nil on failure."
  (when (f-exists? path)
    ;; don't act like it's a new day if the time is <5am.
    (let ((day (if (> 5 (ts-hour (ts-now)))
                   (ts-dec 'day 1 (ts-now))
                 (ts-now))))
      (with-temp-buffer
        (insert-file-contents-literally path)
        (when (search-forward (ts-format "%F" day) nil t)
          t)))))

(defun eva-first-today-line-in-file (path &optional ts)
  "In file at PATH, get the first line that refers to today.
Does this by searching for a YYYY-MM-DD datestamp matching today
or a ts object TS."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (search-forward (ts-format "%F" ts))
    (buffer-substring (line-beginning-position) (line-end-position))))

(defun eva-last-datestamp-in-file (path)
  "Get the last match of YYYY-MM-DD in PATH.
Beware that if PATH has instances of such where you don't expect
it (in additional columns), you might not get the datestamp you
meant to get."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (goto-char (point-max))
    (re-search-backward (rx (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)))
    (buffer-substring (point) (+ 10 (point)))))

(defun eva-tsv-all-entries (path)
  "Return the contents of a .tsv at PATH as a Lisp list."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (flush-lines (rx bol eol))
    (let ((rows (s-split "\n" (buffer-string) t)))
      (--map (s-split "\t" it) rows))))

;; HACK: too strong assumption
(defun eva-tsv-last-timestamp* (path)
  "In .tsv at PATH, get the second field of last row."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (goto-char (point-max))
    (when (looking-back "^" nil) ;; if trailing newline
      (forward-line -1))
    (goto-char (line-beginning-position))
    (search-forward "\t")
    (buffer-substring (point) (- (search-forward "\t") 1))))

;; TODO: Search for unix-stamps too.
(defun eva-tsv-entries-by-date (path &optional ts)
  "Return the contents of a .tsv at PATH as a Lisp list.
Filters for rows containing a YYYY-MM-DD datestamp matching
either today or the date of optional ts object TS."
  (if (f-exists? path)
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

(defun eva-tsv-last-row (path)
  "In .tsv at PATH, get last row as a Lisp list."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (goto-char (point-max))
    (when (looking-back "^" nil) ;; if empty line
      (forward-line -1))
    (split-string (buffer-substring (line-beginning-position)
                                    (line-end-position))
                  "\t")))

(defun eva-tsv-last-value (path)
  "In .tsv at PATH, get the value of last row, last field."
  (when (f-exists? path)
    (with-temp-buffer
      (insert-file-contents-literally path)
      (goto-char (point-max))
      (search-backward "\t")
      (forward-char)
      (buffer-substring (point) (line-end-position)))))

(cl-defun eva-tsv-append
    (path &rest fields &key float-time &allow-other-keys)
  "Append a line to the file located at PATH.
Create the file and its parent directories if it doesn't exist,
and make sure the line begins on a newline.  Treat each argument
in FIELDS... as a separate data field, inserting a tab character
in between, and warn if a field contains a tab character.

For database purposes (which you may not need), FIELDS is
prepended with a field for the Unix timestamp representing
\"posted time\" i.e. right now, the time the row was added.  If
time is also an actual variable you want to track, add a separate
field containing something like the output of `(ts-format
eva-date)'.  The first field is not for that.  Optional
key FLOAT-TIME, if non-nil, means to use a float instead of
integer for the first field."
  (declare (indent defun))
  (unless (f-exists? path)
    (make-empty-file path t))
  (let* ((fields (-replace nil "" fields))
         (newline-maybe (if (s-ends-with-p "\n" (f-read-bytes path))
                            ""
                          "\n"))
         ;; don't do a blank initial line on a new file
         (newline-maybe-really (if (string= "" (f-read-bytes path))
                                   ""
                                 newline-maybe))
         (posted (if float-time
                     (ts-format "%s.%N")
                   (ts-format "%s")))
         (text (s-join "\t" fields))
         (new-text (concat newline-maybe-really posted "\t" text))
         (errors-path (concat path "_errors")))
    (cond
     ((--any-p (s-contains? "\t" it) fields)
      (warn "Entry had tabs inside fields, wrote to %s" errors-path)
      (f-append new-text 'utf-8 errors-path)
      nil)
     ((s-contains? "\n" text)
      (warn "Entry had newlines, wrote to %s" errors-path)
      (f-append new-text 'utf-8 errors-path)
      nil)
     (t
      (eva-append-safely new-text path)
      t))))


;;; Handle idle & reboots & crashes

(defcustom eva-idle-log-path
  (convert-standard-filename "~/self-data/idle.tsv")
  "Location of the idleness log."
  :group 'eva
  :type 'file)

(defcustom eva-fallback-to-emacs-idle nil
  "Track Emacs idle rather than turn off under unknown OS/DE.
Not recommended, as the idleness log will be meaningless unless
you never use a graphical program.  You'll end up with the
situation where returning to Emacs from a long Firefox session
triggers the return-from-idle-hook.

Even EXWM will not update `current-idle-time' while an X window
is in focus."
  :group 'eva
  :type 'boolean)

(defcustom eva-idle-threshold-secs-short (* 10 60)
  "Duration in seconds, above which the user is considered idle."
  :group 'eva
  :type 'integer)

(defcustom eva-idle-threshold-secs-long (* 90 60)
  "Be idle at least this many seconds to start a session on return."
  :group 'eva
  :type 'integer)

(defcustom eva-return-from-idle-hook
  '(eva--log-idle
    eva-session-from-idle)
  "Hook run when user returns from a period of idleness.
Note: An Emacs startup also counts as a return from idleness.
You'll probably want your hook to be conditional on some value of
`eva-length-of-last-idle', which at startup is calculated
from the last Emacs shutdown or crash (technically, last time
the mode was enabled)."
  :group 'eva
  :type '(hook :options (eva--log-idle
                         eva-session-from-idle)))

(defcustom eva-periodic-hook
  '(eva--save-vars-to-disk
    eva--save-buffer-logs-to-disk)
  "Hook run periodically as long as the user is not idle.
Many things do not need to be done while the user is idle, so
think about whether your function does.  If not, put them here."
  :group 'eva
  :type '(hook :options (eva--save-vars-to-disk
                         eva--save-buffer-logs-to-disk)))

(defvar eva--x11idle-program-name nil)

(defvar eva--idle-secs-fn nil)

(defvar eva--last-online nil)

(defvar eva--idle-beginning nil)

(defvar eva-length-of-last-idle 0
  "Length of the last idle/offline period, in seconds.
Becomes set after that period ends and should be available at the
time `eva-return-from-idle-hook' is run.")

(defun eva--idle-secs ()
  "Number of seconds user has now been idle, as told by the system.
Not to be confused with `eva-length-of-last-idle'."
  (funcall eva--idle-secs-fn))

(defun eva--idle-secs-x11 ()
  "Like `org-x11-idle-seconds' without /bin/sh or org."
  (/ (eva--process-output-to-number eva--x11idle-program-name)
     1000))

(defun eva--idle-secs-emacs ()
  "Same as `org-emacs-idle-seconds'.
Digression: Should honestly be submitted to Emacs,
`current-idle-time' is... not normal."
  (let ((idle-time (current-idle-time)))
    (if idle-time
        (float-time idle-time)
      0)))

(defun eva--idle-secs-gnome ()
  "Check Mutter's idea of idle time, even on Wayland."
  ;; https://unix.stackexchange.com/questions/396911/how-can-i-tell-if-a-user-is-idle-in-wayland
  (let ((idle-ms
         (string-to-number
          (car (s-match (rx space (* digit) eol)
                        (eva--process-output-to-string
                         "dbus-send"
                         "--print-reply"
                         "--dest=org.gnome.Mutter.IdleMonitor"
                         "/org/gnome/Mutter/IdleMonitor/Core"
                         "org.gnome.Mutter.IdleMonitor.GetIdletime"))))))
    (/ idle-ms 1000)))

(defun eva--log-idle ()
  "Log chunk of idle time to disk."
  (eva-tsv-append eva-idle-log-path
    (ts-format)
    (number-to-string (/ (round eva-length-of-last-idle) 60))))

;; This trio of functions handles many edge cases elegantly.  Modify with care.
(defun eva--start-next-timer (&optional assume-idle)
  "Start one or the other timer depending on idleness.
If ASSUME-IDLE is non-nil, skip the idle check and associated
overhead."
  (if (or assume-idle (eva-idle-p))
      (named-timer-run :eva 2 nil #'eva--user-is-idle t)
    (named-timer-run :eva 111 nil #'eva--user-is-present)))

(defun eva--user-is-present ()
  "Do stuff assuming the user is active (not idle).
This function is called by `eva--start-next-timer'
repeatedly for as long as the user is active (not idle).

Runs `eva-periodic-hook'."
  ;; Guard the case where the user puts the computer to sleep manually, which
  ;; means this function will still be queued to run when the computer wakes.
  ;; If the time difference is suddenly big, hand off to the other function.
  (if (> (ts-diff (ts-now) eva--last-online)
         eva-idle-threshold-secs-short)
      (eva--user-is-idle)
    (setq eva--last-online (ts-fill (ts-now)))
    (setq eva--idle-beginning (ts-fill (ts-now)))
    (eva--start-next-timer)
    ;; Run hooks last, in case they contain bugs.
    (run-hooks 'eva-periodic-hook)))

;; NOTE: This runs rapidly b/c it should react quickly on user returning.
(defun eva--user-is-idle (&optional decrement)
  "Do stuff assuming the user is idle.
This function is called by `eva--start-next-timer'
repeatedly for as long as the user is idle.

When DECREMENT is non-nil, decrement `eva--idle-beginning'
to correct for the time it took to reach idle status.

When the user comes back, this function will be called one last
time, at which point the idleness condition will fail and it sets
`eva-length-of-last-idle' and runs
`eva-return-from-idle-hook'.  That it has to run exactly
once with a failing condition that normally succeeds, as opposed
to running never or forever, is the reason it has to be a
separate function from `eva--user-is-present'."
  (setq eva--last-online (ts-now))
  (if (eva-idle-p)
      (eva--start-next-timer 'assume-idle)
    ;; Take the idle threshold into account and correct the idle begin point.
    (when decrement
      (ts-decf (ts-sec eva--idle-beginning)
               eva-idle-threshold-secs-short))
    (setq eva-length-of-last-idle (ts-diff (ts-now) eva--idle-beginning))
    (unwind-protect
        (run-hooks 'eva-return-from-idle-hook)
      (setq eva--idle-beginning (ts-fill (ts-now)))
      (eva--start-next-timer))))

(defun eva-idle-p ()
  "Idled longer than `eva-idle-threshold-secs-short'?"
  (> (eva--idle-secs) eva-idle-threshold-secs-short))


;;; Items
;; Q: What's cl-defstruct?  A: https://nullprogram.com/blog/2018/02/14/

;; NOTE: If you change the order of keys, eva--mem-recover will set the
;; wrong values henceforth!  You'd better use `eva--mem-nuke-var' on
;; `eva-items' then.
(cl-defstruct (eva-item
               (:constructor eva-item-create)
               (:copier nil))
  (dismissals 0)
  (min-hours-wait 3)
  last-called ;; almost always filled-in
  fn ;; primary key (must be unique)
  max-calls-per-day
  max-successes-per-day
  max-entries-per-day
  lookup-posted-time
  dataset
  ;; name ;; truly-unique key (if reusing fn in two objects for some reason)
  )

(defvar eva-items)

(defvar eva-disabled-fns nil
  "Which members of `eva-items' to avoid processing.
Referred to by their :fn value.")

(defun eva-check-special-input (input)
  "Check INPUT for keywords like \"/skip\" and react specially."
  (cond ((string-match-p "^/s" input) ;; /skip
         (if (and (< 1 (length eva--queue))
                  (member eva-curr-fn eva--queue))
             ;; Try to proceed to next item
             ;;
             ;; REVIEW: does this actually work now since it checks
             ;;         minibufferp?  we need maybe to call
             ;;         abort-recursive-edit too?
             (progn
               (cl-incf (eva-item-dismissals eva-curr-item))
               (setq eva--queue (cl-remove eva-curr-fn eva--queue :count 1))
               (eva-resume))
           ;; Just cancel the session
           (abort-recursive-edit))
         nil)
        ((string-match-p "^/h" input) ;; /help
         (eva-dispatch)
         (abort-recursive-edit)
         nil)
        (t input)))

(defun eva--pending-p (fn)
  "Return t if FN is due to be called."
  (let* ((i (eva-item-by-fn fn))
         (dataset (eva-item-dataset i))
         (max-entries (eva-item-max-entries-per-day i))
         (max-successes (eva-item-max-successes-per-day i))
         (lookup-posted-time (eva-item-lookup-posted-time i))
         (dismissals (eva-item-dismissals i))
         (min-hrs-wait (eva-item-min-hours-wait i))
         (min-secs-wait (* 60 60 min-hrs-wait))
         (successes-today (eva--count-successes-today fn))
         (successes-specified-and-exceeded
          (and successes-today
               max-successes
               (>= successes-today max-successes)))
         (last-called (make-ts :unix (or (eva-item-last-called i) 0)))
         (called-today (and (= (ts-day last-called) (ts-day (ts-now)))
                            (> (ts-hour last-called) 4)))
         (recently-logged
          (when (and (stringp dataset)
                     (f-exists? dataset))
            (> min-secs-wait
               (if lookup-posted-time
                   (- (ts-unix (ts-now))
                      (string-to-number (car (eva-tsv-last-row dataset))))
                 (ts-diff (ts-now)
                          (ts-parse (eva-tsv-last-timestamp* dataset)))))))
         ;; Even if we didn't log yet, we don't want to be that persistent.
         (recently-called (< (ts-diff (ts-now) last-called)
                             ;; hours multiplied by n dismissals
                             (* dismissals 60 60))))
    (unless recently-logged
      (when (or (not called-today)
                (not (and (stringp dataset)
                          (f-exists? dataset)))
                (null max-entries)
                (> max-entries (length (eva-tsv-entries-by-date dataset))))
        (unless recently-called
          (unless successes-specified-and-exceeded
            t))))))

(defun eva-item-by-fn (fn)
  "Get the item associated with the query function FN."
  (--find (equal fn (eva-item-fn it)) eva-items))

(defun eva-dataset-by-fn (fn)
  "Get the dataset associated with the query function FN."
  (eva-item-dataset (eva-item-by-fn fn)))

(defun eva-enabled-fns ()
  "Subset of `eva-items' not known as disabled.
Referred to by their :fn value."
  (-difference (-map #'eva-item-fn eva-items) eva-disabled-fns))

(defun eva-reenable-fn ()
  "Prompt to reenable one of the disabled items."
  (interactive)
  (if (< 0 (length eva-disabled-fns))
      (let ((response
             (completing-read "Re-enable: "
                              (-map #'symbol-name eva-disabled-fns))))
        (setq eva-disabled-fns (remove response eva-disabled-fns)))
    (message "There are no disabled items")))

(defun eva-disable (fn)
  "Disable a member of `eva-items' so it will not be processed.
Refer to the member by its FN key."
  (interactive "CCommand: ")
  (push fn eva-disabled-fns)
  (setq eva--queue (remove fn eva--queue)))

(defun eva-ask-disable (fn)
  "Ask to disable item indicated by FN.
Return non-nil on yes, and nil on no."
  (if (eva-ynp "You have been dismissing "
               (symbol-name fn)
               ", shall I stop tracking it for now?")
      (progn (push fn eva-disabled-fns)
             (setq eva--queue (remove fn eva--queue)))
    (setf (eva-item-dismissals (eva-item-by-fn fn)) 0)
    nil))

;; NOTE: Do not move the check to eva--pending-p, that is passive and
;; this needs interactivity.
(defun eva-call-fn-check-dismissals (fn)
  "Call FN, but ask to disable if it's been dismissed many times."
  (interactive "CCommand: ")
  (unless (and (<= 3 (eva-item-dismissals (eva-item-by-fn fn)))
               (eva-ask-disable fn))
    (funcall fn)))


;;; The big boilerplate
;; As you may note, the main way of passing info between functions is
;; eva-curr-fn.

(defvaralias 'eva--on-excursion 'eva--stop-queue)

(defvar eva--stop-queue nil)

(defvar eva-excursion-buffers nil
  "Buffers included in the current or last excursion.
Note that there is a variable watcher `eva--add-local-hook' on
this list.")

;; Make sure the end user doesn't have to write add-hook in their eva-defun
;; definitions; it'll be enough to just push a buffer onto
;; eva-excursion-buffers (or in any way modify it) and all members
;; automatically get a buffer-local hook set.
(add-variable-watcher
 'eva-excursion-buffers
 (defun eva--add-local-hook (_sym new _op _where)
   "Add buffer-local hooks to all members of NEW.
NEW is assumed to be a list of buffers\; most likely, it is
`eva-excursion-buffers'."
   (cl-loop for buf in new
            do (if (bufferp buf)
                   (when (buffer-live-p buf)
                     (with-current-buffer buf
                       ;; (eva-dbg "Adding local hook to: " (buffer-name))
                       (add-hook 'kill-buffer-hook
                                 #'eva--if-excursion-complete-do-stuff
                                 96 t)))
                 (error "New value of %s contains a non-buffer"
                        "eva-excursion-buffers")))))

(defun eva--if-excursion-complete-do-stuff ()
  "If the current excursion appears done, do things."
  (eva-dbg "Running eva--if-excursion-complete-do-stuff."
           " Current buffer: " (buffer-name))
  (when eva--on-excursion
    (let ((others (remove (current-buffer) eva-excursion-buffers)))
      (when (-none-p #'buffer-live-p others)
        (remove-hook 'kill-buffer-hook #'eva--if-excursion-complete-do-stuff)
        (named-timer-cancel :eva-excursion)
        (when (null (eva-item-dataset
                     (eva-item-by-fn eva-curr-fn)))
          (eva-tsv-append
            (expand-file-name (concat "successes-"
                                      (symbol-name eva-curr-fn))
                              eva-cache-dir-path)))
        (setq eva--queue (cl-remove eva-curr-fn eva--queue :count 1))
        ;; TODO: if a fn is called in isolation manually, don't resume
        ;; HACK Because the current-buffer is still active, wait to be sure the
        ;; kill-buffer completes.  I would like an after-kill-buffer-hook so I
        ;; don't need this timer.
        (run-with-timer .2 nil #'eva-resume)))))

(defun eva--after-cancel-prompt-do-stuff ()
  "Actions after user cancels a minibuffer spawned from `eva-defun'."
  (eva-dbg "Running eva--after-cancel-prompt-do-stuff")
  (advice-remove 'abort-recursive-edit #'eva--after-cancel-prompt-do-stuff)
  (when (null eva-curr-fn)
    (error "Unexpectedly null: eva-curr-fn"))
  (cl-incf (eva-item-dismissals eva-curr-item))
  ;; Re-add the fn to the queue because it got removed (so I expect); after a
  ;; cancel, we want it to remain queued up.
  (cl-pushnew eva-curr-fn eva--queue))

(defun eva--prep-for-interaction ()
  "Set up variables and hooks prior to calling `eva-curr-fn'.
Return t if there was no problem with setup, nil otherwise."
  (let ((ok t))
    (setq eva-curr-item (eva-item-by-fn eva-curr-fn))
    ;; TODO: proceed even though it's not listed (need sanity checks elsewhere)
    (unless eva-curr-item
      (setq ok nil)
      (error "%s not listed in eva-items" (symbol-name eva-curr-fn)))
    (setq eva-curr-dataset (eva-item-dataset eva-curr-item))
    (when (minibufferp)
      (message "Was in minibuffer when %s called, not proceeding."
               (symbol-name eva-curr-fn))
      (eva-stop-queue)
      (setq ok nil))
    (setq eva-excursion-buffers nil)
    (setf (eva-item-last-called eva-curr-item)
          (time-convert (current-time) 'integer))
    ;; Set up watcher for cancelled prompt.
    (advice-add 'abort-recursive-edit :before
                #'eva--after-cancel-prompt-do-stuff)
    ok))

(defun eva--after-pure-query-cleanup ()
  "Update variables counting this as a successful run."
  (setq eva--queue (cl-remove eva-curr-fn eva--queue :count 1))
  (setf (eva-item-dismissals eva-curr-item) 0)
  ;; Save timestamp of this successful run.
  (eva-tsv-append
    (expand-file-name (concat "successes-" (symbol-name eva-curr-fn))
                      eva-cache-dir-path))
  ;; Clean up, because this wasn't an excursion.
  (named-timer-cancel :eva-excursion)
  (remove-hook 'kill-buffer-hook #'eva--if-excursion-complete-do-stuff))

;; TODO: add itself to queue pls (why?)
;;
;; Ok, here's the shenanigans.  When the user cancels a "query" by typing C-g,
;; they were in the minibuffer, so it calls abort-recursive-edit (at least
;; under selectrum-mode).  With this, we ensure different behavior for queries
;; and excursions.  How?  We advise abort-recursive-edit to do things that we
;; only want when a query is cancelled.
(defmacro eva-defun (name args &rest body)
  "Boilerplate wrapper for `cl-defun'.
NAME, ARGS and BODY are as in `cl-defun'.  To see what it expands
to, try the command `emacs-lisp-macroexpand'.

Manages the external variables `eva-curr-fn' and `eva--queue',
zeroes `eva-item-dismissals' on success, advises
`abort-recursive-edit' (\\<minibuffer-local-map>
\\[minibuffer-keyboard-quit]) while in a prompt spawned within BODY,
and so on.  If you use a simple `defun' in lieu of this wrapper,
you should replicate these features!"
  (declare (indent defun) (doc-string 3))
  (let* ((parsed-body (macroexp-parse-body body))
         (declarations (car parsed-body))
         (main-body (cdr parsed-body)))
    `(cl-defun ,name ,args
       ;; Ensure it's always interactive
       ,@(if (member 'interactive (-map #'car-safe declarations))
             declarations
           (-snoc declarations '(interactive)))
       (setq eva-curr-fn #',name)
       (when (eva--prep-for-interaction)
         (unwind-protect
             (prog1 (progn ,@main-body)
               (unless eva--on-excursion
                 (eva--after-pure-query-cleanup)))
           ;; All below this line will always happen.
           (advice-remove 'abort-recursive-edit
                          #'eva--after-cancel-prompt-do-stuff))))))


;;; Persistent variables memory

(defcustom eva-mem-history-path
  (convert-standard-filename
   (expand-file-name "memory.tsv" eva-cache-dir-path))
  nil
  :group 'eva
  :type 'file
  :risky t)
;; TODO: Test this
;; :set (lambda (sym val)
;;        (eva--save-buffer-logs-to-disk)
;;        (eva--save-vars-to-disk)
;;        (set-default sym val)))

(defcustom eva-after-load-vars-hook nil
  "Invoked right after populating `eva-mem' from disk.
The most recent values are therefore available.  If you've
previously saved data in that list (typically via
`eva-before-save-vars-hook'), it should now be back even if Emacs
has restarted, so you can run something like the following.

    (setq my-var (map-elt eva-mem 'my-var))"
  :group 'eva
  :type 'hook)

(defcustom eva-before-save-vars-hook nil
  "Invoked right before syncing `eva-mem' to disk.
You should add to that list any variable you want to persist
across reboots, using the following.

    (eva-mem-push 'my-var)
or
    (eva-mem-push-alt my-var)

Of course, you can do that at any time instead of putting it on
this hook.  The hook can be a reassurance if you do things with
'my-var at indeterminate times and you want to be sure what goes
in before it gets written to disk.

After a reboot, we won't set the variable globally for you, it'll
only be in `eva-mem'.  See `eva-load-vars-hook'."
  :group 'eva
  :type 'hook)

(defvar eva-mem nil
  "Alist of all relevant variable values.
We log these values to disk at `eva-mem-history-path', so we can
recover older values as needed.")

(defvar eva--mem-timestamp-variables '(eva--last-online
                                       eva--last-chatted)
  "List of Lisp variables that contain ts objects.
Members that appear in `eva-mem' as ts objects will be saved to
`eva-mem-history-path' as plain numbers instead of ts objects for
legibility.")

(defvar eva--has-restored-variables nil)

(defun eva-mem-push (var)
  "In `eva-mem', store variable VAR's current value.
You should quote VAR, like with `set', not `setq'.  I.e.:

    (eva-mem-push 'VAR)"
  (if (assoc var eva-mem)
      (map-put! eva-mem var (symbol-value var))
    (setq eva-mem
          (map-insert eva-mem var (symbol-value var)))))

(defmacro eva-mem-push-alt (var)
  "In `eva-mem', store variable VAR's current value.
Unlike `eva-mem-push', quotes VAR for you, and it works in some
cases (pushing let-bound variables) where that function won't."
  `(if (assoc ',var eva-mem)
       (map-put! eva-mem ',var ,var)
     (setq eva-mem
           (map-insert eva-mem ',var ,var))))

(defun eva-mem-get (var)
  "Get VAR's value from `eva-mem'.
Equivalent to (map-elt eva-mem 'var), but is a more consistent
interface considering we use `eva-mem-push' to set variables."
  (map-elt eva-mem var))

(defun eva--read-lisp (s)
  "Check that string S isn't blank, then `read' it.
Otherwise signal an error, unlike `read' or `read-from-string'."
  (if (and (stringp s)
           (not (s-blank? s)))
      (car (read-from-string s))
    (error "Input should be string containing valid lisp: %s" s)))

(defun eva--mem-nuke-var (var)
  "Remove all instances of VAR from file at `eva-mem-history-path'.
Use with care.  Mainly for development use.

It uses `flush-lines', which is prone to mistakes (perhaps you
have multiline values...) and may flush other variables that
merely refer to the variable name in their value."
  (f-copy eva-mem-history-path "/tmp/eva-mem.backup")
  (with-temp-buffer
    (insert-file-contents-literally eva-mem-history-path)
    (flush-lines (symbol-name var))
    (write-file eva-mem-history-path)))

(defun eva--mem-filter-for-variable (var)
  "Get all occurrences of VAR from `eva-mem-history-path'.
Return a list looking like
\((TIMESTAMP KEY VALUE) (TIMESTAMP KEY VALUE) ... )."
  (--filter (eq var (eva--read-lisp (cadr it)))
            (eva-tsv-all-entries eva-mem-history-path)))

(defun eva--mem-check-history-sanity ()
  "Check that the mem history is sane."
  (unless (--all-p (= 3 (length it))
                   (eva-tsv-all-entries eva-mem-history-path))
    (error "Memory history looks corrupt: not all lines have 3 fields.  \n%s"
           "See `eva-mem-history-path'")))

(defun eva--mem-save-only-changed-vars ()
  "Save new or changed `eva-mem' values to disk."
  (eva--mem-check-history-sanity)
  (cl-loop for cell in eva-mem
           do (let ((subset (eva--mem-filter-for-variable (car cell)))
                    (write? nil))
                (if (null subset)
                    (setq write? t)
                  (let ((historic-value (eva--read-lisp
                                         (nth 2 (-last-item subset))))
                        (mem-value
                         (if (member (car cell) eva--mem-timestamp-variables)
                             (floor (ts-unix (cdr cell)))
                           (cdr cell))))
                    (unless (equal historic-value mem-value)
                      (setq write? t))))
                (when write?
                  (let ((print-level nil)
                        (print-length nil))
                    (eva-tsv-append eva-mem-history-path
                      (prin1-to-string (car cell))
                      (if (ts-p (cdr cell))
                          ;; Convert ts structs because they're clunky to read
                          (ts-format "%s" (cdr cell))
                        (prin1-to-string (cdr cell)))))))))

(defun eva--mem-last-value-of-variable (var)
  "Get the most recent stored value of VAR from disk."
  (let* ((table (nreverse (eva-tsv-all-entries
                           eva-mem-history-path)))
         (ok t))
    (cl-block nil
      (while ok
        (let ((row (pop table)))
          (when (eq (eva--read-lisp (nth 1 row)) var)
            (setq ok nil)
            (cl-return (read (nth 2 row)))))))))

(defun eva--mem-recover ()
  "Read the newest values from file at `eva-mem-history-path'.
Assign them to the same names inside the alist
`eva-mem'."
  (let* ((table (-map #'cdr (nreverse (eva-tsv-all-entries
                                       eva-mem-history-path)))))
    (while (/= 0 (length table))
      (let* ((row (pop table))
             (parsed-row (-map #'eva--read-lisp row)))
        (unless (member (car parsed-row) (map-keys eva-mem))
          ;; Convert numbers back into ts objects.
          (when (member (car parsed-row) eva--mem-timestamp-variables)
            (setf (cadr parsed-row)
                  (ts-fill (make-ts :unix (cadr parsed-row)))))
          (setq eva-mem (cons (cons (car parsed-row) (cadr parsed-row))
                              eva-mem)))))))

(defun eva--mem-restore-items-values ()
  "Sync some values of current `eva-items' members from disk.
The values are :last-called and :dismissals, because they are of
interest to persist across sessions."
  (dolist (mem-item (map-elt eva-mem 'eva-items))
    ;; TODO: Don't use ignore-errors
    (unless (ignore-errors
              (let* ((fn-sym (eva-item-fn mem-item))
                     (active-item (eva-item-by-fn fn-sym)))
                ;; if reflects something we have defined currently
                (when (and (fboundp fn-sym)
                           (member active-item eva-items))
                  ;; Update the :dismissals etc to match values from history.
                  (setf (eva-item-dismissals active-item)
                        (eva-item-dismissals mem-item))
                  (setf (eva-item-last-called active-item)
                        (eva-item-last-called mem-item)))
                t))
      (warn
       (s-join "\n"
               '("eva--mem-restore-items-values failed. "
                 " Did you change the eva-item defstruct?"
                 " Not critical so proceeding.  May self-correct next sync."))))))

;; TODO: Calc all reasonable defaults we can from known dataset contents (we
;;       already do it some but we can do more).
(defun eva--init ()
  "Master function restoring all relevant variables.
Appropriate on init."
  (mkdir eva-cache-dir-path t)
  (f-touch eva-mem-history-path)
  (eva--mem-recover)
  ;; TODO: error if there are non-nil times in history and most current is nil
  (let* ((online (map-elt eva-mem 'eva--last-online))
         (chatted (map-elt eva-mem 'eva--last-chatted))
         (modtime (and eva-chat-log-path
                       (f-exists? eva-chat-log-path)
                       (time-convert (file-attribute-modification-time
                                      (file-attributes eva-chat-log-path))
                                     'integer)))
         (max
          (ts-fill (make-ts :unix (max (if online (ts-unix online) 0)
                                       (if chatted (ts-unix chatted) 0)
                                       (or modtime 0)))))
         (max-chatted
          (ts-fill (make-ts :unix (max (if chatted (ts-unix chatted) 0)
                                       (or modtime 0))))))
    (setq eva--idle-beginning max)
    (setq eva--last-online max)
    (setq eva--last-chatted max-chatted))
  (eva--mem-restore-items-values)
  (run-hooks 'eva-after-load-vars-hook)
  (setq eva--has-restored-variables t)
  (add-hook 'kill-emacs-hook #'eva--save-vars-to-disk))

(defun eva--save-vars-to-disk ()
  "Sync all relevant variables to disk."
  (if (eva--another-eva-running-p)
      (warn "Another VA running, not saving variables.")
    (unless eva--has-restored-variables
      (error "\n%s\n%s\n%s\n%s"
             "Attempted to save variables to disk, but never fully"
             "restored them from disk first, so the variables would have been"
             "as on a new setup, possibly all blank.  Please post an issue:"
             "https://github.com/meedstrom/eva even if you fix it"))
    (eva-mem-push 'eva--last-online)
    (eva-mem-push 'eva--last-chatted)
    (eva-mem-push 'eva-items)
    (eva-mem-push 'eva-disabled-fns)
    (make-directory eva-cache-dir-path t)
    (when eva-chat-log-path
      (eva-write-safely (with-current-buffer (eva-buffer-chat)
                          (buffer-string))
                        eva-chat-log-path))
    (run-hooks 'eva-before-save-vars-hook)
    (eva--mem-save-only-changed-vars)))


;;; Interactive sessions

(defvar eva-debug-do-all-items nil)

(defun eva-stop-queue ()
  "If `eva-run-queue' is currently looping, quit."
  (setq eva--stop-queue t))

(defalias 'eva-resume #'eva-run-queue)

(defun eva-run-queue ()
  "Call every function from QUEUE, default `eva--queue'.
Does some checks and sets up a good environment, in particular
nulling the 'buffer-predicate frame parameter so that no buffers
spawned by the functions will be skipped by
`switch-to-next-buffer'."
  (interactive)
  (eva-dbg "Running eva-run-queue")
  (if (minibufferp) ; user busy
      ;; NOTE: named-timer is great insurance.  We've had bugs spawn many
      ;;       copies of this timer, causing a predictably terrible UX.
      (named-timer-run :eva-retry 20 nil #'eva-run-queue)
    (let ((bufpred-backup (frame-parameter nil 'buffer-predicate)))
      (unwind-protect
          (progn
            (set-frame-parameter nil 'buffer-predicate nil)
            (when (null eva--queue)
              (message (eva-emit "All done for now.")))
            (when eva--stop-queue
              (setq eva--queue (cl-remove eva-curr-fn eva--queue :count 1)))
            (setq eva--stop-queue nil)
            (cl-loop for f in eva--queue
                     while (not eva--stop-queue)
                     do (eva-call-fn-check-dismissals f)))
        (set-frame-parameter nil 'buffer-predicate bufpred-backup)))))

(defun eva-session-butt-in-gently ()
  "Butt in if any queries are pending, with an introductory chime."
  ;; If a session is already active, don't start a new one.
  (eva-dbg "Running eva-session-butt-in-gently")
  (unless eva--on-excursion
    (if (minibufferp) ; user busy
        (named-timer-run :eva-retry 20 nil #'eva-session-butt-in-gently)
      (setq eva-date (ts-now))
      (when-let ((fns (if eva-debug-do-all-items
                          (eva-enabled-fns)
                        (-filter #'eva--pending-p (eva-enabled-fns)))))
        (setq eva--queue fns)
        (unless (eq t (frame-focus-state))
          (require 'notifications)
          (when (notifications-get-capabilities)
            (notifications-notify :title eva-va-name :body (eva-greeting))))
        (eva--chime-aural)
        (eva--chime-visual)
        (run-with-timer 1 nil #'eva-run-queue)))))

(defun eva-session-from-idle ()
  "Start a session if idle was long."
  (eva-dbg "Running eva-session-from-idle")
  (unless (< eva-length-of-last-idle eva-idle-threshold-secs-long)
    (eva-session-butt-in-gently)))

(defun eva-session-new ()
  "Recalculate what items are pending and run them."
  (interactive)
  (eva-dbg "Running eva-session-new")
  (unless eva--on-excursion
    (setq eva-date (ts-now))
    (setq eva--queue (-filter #'eva--pending-p (eva-enabled-fns)))
    (eva-run-queue)))

(defun eva-session-new-force-all ()
  "Run through all enabled items."
  (interactive)
  (eva-dbg "Running eva-session-new-force-all")
  (eva-stop-queue)
  (setq eva-date (ts-now))
  (setq eva--queue (eva-enabled-fns))
  (eva-run-queue))


;;; Buffer logger
;; Unlike most data sources which make only the occasional datapoint, this
;; logger produces constant reams of new data, so we write them temporarily to
;; a buffer, bundling up what would otherwise be many disk write ops.

(defcustom eva-buffer-focus-log-path
  (convert-standard-filename "~/self-data/buffer-focus.tsv")
  "Where to save the log of buffer focus changes."
  :group 'eva
  :type 'file)

(defcustom eva-buffer-info-path
  (convert-standard-filename "~/self-data/buffer-info.tsv")
  "Where to save the log of buffer metadata."
  :group 'eva
  :type 'file)

(defvar eva--last-buffer nil)

(defvar eva--known-buffers nil
  "Buffers the user has entered this Emacs session.")

(defvar eva--buffer-focus-log-buffer
  (get-buffer-create
   (concat (unless eva-debug " ") "*Eva: Buffer focus log*"))
  "Buffer for not-yet-saved log lines.")

(defvar eva--buffer-info-buffer
  (get-buffer-create
   (concat (unless eva-debug " ") "*Eva: Buffer info*"))
  "Buffer for not-yet-saved log lines.")

(defun eva--new-uuid ()
  "Same as `org-id-uuid', but avoid relying on Org."
  (declare (side-effect-free t))
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

(defun eva--save-buffer-logs-to-disk ()
  "Append as-yet unwritten log lines to disk files."
  (eva--transact-buffer-onto-file eva--buffer-focus-log-buffer
                                  eva-buffer-focus-log-path)
  (eva--transact-buffer-onto-file eva--buffer-info-buffer
                                  eva-buffer-info-path))

;; TODO: When buffer major mode changes, count it as a new buffer. Note that
;;       (assoc buf eva--known-buffers) will still work.
;; TODO: When eww url changes, count it as a new buffer
;; TODO: When counting it as a new buffer, record a field for "previous uuid"
;;       just in case the data analyst wants to merge these observations
(defun eva--log-buffer (&optional _arg)
  "Log the buffer just switched to.
Put this on `window-buffer-change-functions' and
`window-selection-change-functions'."
  (unless (minibufferp)
    (let* ((buf (current-buffer))
           (mode (symbol-name major-mode))
           (known (assoc buf eva--known-buffers))
           (timestamp (ts-format "%s.%N"))
           (visiting (if (eq major-mode 'dired-mode)
                         default-directory
                       buffer-file-name))
           (eww-url (when (eq major-mode 'eww-mode)
                      (eww-current-url)))
           (exist-record (unless (and known
                                      ;; TODO: new exist-record if mode changed
                                      (equal mode (nth 4 known))) ; doesnt do it
                           (list buf
                                 (eva--new-uuid)
                                 (buffer-name)
                                 visiting
                                 mode
                                 timestamp ;; time the buffer was first opened
                                 eww-url
                                 (when (equal mode "exwm-mode") exwm-class-name)
                                 (when (equal mode "exwm-mode") exwm-title))))
           (focus-record (list timestamp ;; time the buffer was switched to
                               ;; the buffer's uuid
                               (if known (cadr known) (cadr exist-record)))))
      (unless (eq eva--last-buffer buf) ; only entered/left minibuffer
        (setq eva--last-buffer buf)
        (unless known
          (push exist-record eva--known-buffers)
          (with-current-buffer eva--buffer-info-buffer
            (goto-char (point-max))
            (insert "\n" (s-join "\t" (cdr exist-record)))))
        (with-current-buffer eva--buffer-focus-log-buffer
          (goto-char (point-max))
          (insert "\n" (s-join "\t" focus-record)))))))


;;; Commands

(defun eva-version (&optional interactive)
  "Return the Eva package version.
Argument INTERACTIVE is set internally."
  (interactive "p")
  (if interactive
      (message "Eva version 0.5-pre")
    "0.5-pre"))

(defun eva-decrement-date ()
  "Decrement `eva-date'."
  (interactive)
  (eva-set-date (ts-dec 'day 1 eva-date)))

(defun eva-increment-date ()
  "Increment `eva-date'."
  (interactive)
  (eva-set-date (ts-inc 'day 1 eva-date)))

(defun eva-set-date-today ()
  "Set `eva-date' to today."
  (interactive)
  (eva-set-date (ts-now)))

(defun eva-set-date (&optional ts)
  "Decrement `eva-date' from `org-read-date'.
Optional arg TS skips the prompt and sets date from that."
  (interactive)
  (require 'org)
  (if ts
      (setq eva-date ts)
    (let* ((time (ts-format "%T"))
           (new-date (org-read-date))
           (new-datetime (ts-parse (concat new-date " " time))))
      (setq eva-date new-datetime)))
  (eva-emit
   "Operating as if the date is " (ts-format "%x" eva-date) "."))


;;; Modes and keys

(defvar eva-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'eva-resume)
    (define-key map (kbd "+") #'eva-increment-date)
    (define-key map (kbd "-") #'eva-decrement-date)
    (define-key map (kbd "0") #'eva-set-date-today)
    (define-key map (kbd "d") #'eva-set-date)
    (define-key map (kbd "q") #'bury-buffer)
    (define-key map (kbd "?") #'eva-dispatch)
    (define-key map (kbd "h") #'eva-dispatch)
    map))

(define-derived-mode eva-chat-mode text-mode "VA chat")

(transient-define-prefix eva-dispatch ()
  ["General actions"
   ("q" "Quit the chat" bury-buffer)
   ]
  ["Date"
   ("0" "Reset date to today (default)" eva-set-date-today :transient t)
   ("-" "Decrement the date" eva-decrement-date :transient t)
   ("+" "Increment the date" eva-increment-date :transient t)
   ("d" "Set date..." eva-set-date :transient t)
   ])


;;; "Main": startup stuff

(defun eva--check-for-time-anomalies ()
  "Check for timestamps that don't look right.
Good to run after enabling `eva-mode' or changing `eva-items'."
  (let* ((datasets (--map (eva-item-dataset it) eva-items))
         (logs (list eva-idle-log-path eva-buffer-focus-log-path))
         (files (-non-nil (append datasets logs)))
         (anomalous-files nil))
    (dolist (f files)
      (when (f-exists? f)
        (let ((stamps (->> (eva-tsv-all-entries f)
                           (map-keys)
                           (-map #'string-to-number))))
          (unless (<= stamps)
            (warn (concat "Timestamps not strictly increasing in: " f)))
          ;; Check that no timestamp bigger than current time.
          (if (--any-p (> it (float-time)) stamps)
              (push f anomalous-files)))))
    (when anomalous-files
      (warn (->> (append
                  '("Eva: Anomalous timestamps found in my logs."
                    "You probably have or have had a wrong system clock."
                    "These files have timestamps exceeding the current time:")
                  anomalous-files)
                 (s-join "\n"))))))

(defun eva--keepalive ()
  "Re-start the :eva timer if dead.
Indispensable while hacking on the package."
  (unless (member (named-timer-get :eva) timer-list)
    (message "[%s] eva timer found dead, reviving it."
             (format-time-string "%H:%M"))
    (eva--start-next-timer)))

;; BUG: /tmp/eva/pid can be empty
(defun eva--another-eva-running-p ()
  "Return non-nil if another Emacs instance has eva-mode on.
Return nil if only the current Emacs instance is running it, or
if no Emacsen are running it.  If you've somehow forced it on in
several Emacsen, the behavior is unspecified, but it shouldn't be
possible to do."
  (when (f-exists? "/tmp/eva/pid")
    (let ((pid (string-to-number (f-read-bytes "/tmp/eva/pid"))))
      (and (/= pid (emacs-pid))
           (member pid (list-system-processes))))))

(defun eva--emacs-init-message ()
  "Emit the message that Emacs has started.
So that we can see in the chat log when Emacs was (re)started,
creating some context."
  (when eva--has-restored-variables ; guard clause needed b/c of emit's code
    (eva-emit "------ Emacs (re)started. ------")))

(defun eva--delayed-start-timer ()
  "Wait a bit, then start the VA's heartbeat timer.
The delay works around issues of an open prompt on init causing
a XELB timeout."
  (add-hook 'exwm-init-hook #'eva--user-is-present)
  ;; (run-with-timer 4 nil #'eva--user-is-present)
  )

(defun eva--idle-set-fn ()
  "Set `eva--idle-secs-fn' to an appropriate function.
Return the function on success, nil otherwise."
  (or (symbol-value 'eva--idle-secs-fn)  ; if preset, use that.
      (and (eq system-type 'darwin)
           (autoload #'org-mac-idle-seconds "org-clock")
           (setq eva--idle-secs-fn #'org-mac-idle-seconds))
      ;; If under Mutter's Wayland compositor
      (and (getenv "DESKTOP_SESSION")
           (s-matches? (rx (or "gnome" "ubuntu")) (getenv "DESKTOP_SESSION"))
           (not (s-contains? "xorg" (getenv "DESKTOP_SESSION")))
           (setq eva--idle-secs-fn #'eva--idle-secs-gnome))
      ;; NOTE: This condition is true under XWayland, so it must come
      ;; after any check for Wayland if we want it to mean X only.
      (and (eq window-system 'x)
           (setq eva--x11idle-program-name
                 (seq-find #'executable-find '("x11idle" "xprintidle")))
           (setq eva--idle-secs-fn #'eva--idle-secs-x11))
      (and (symbol-value 'eva-fallback-to-emacs-idle)
           (setq eva--idle-secs-fn #'eva--idle-secs-emacs))))

(defun eva-unload-function ()
  "Unload the Eva library."
  (eva-mode 0)
  (kill-buffer (eva-buffer-chat))
  (kill-buffer eva--buffer-r)
  (kill-buffer eva--buffer-focus-log-buffer)
  (kill-buffer eva--buffer-info-buffer)
  (with-demoted-errors nil
    (unload-feature 'eva-test)
    (unload-feature 'eva-activity)
    (unload-feature 'eva-builtin))
  ;; Continue standard unloading.
  nil)


;; DEPRECATED: Old function names
(defalias 'eva-mem-pushnew #'eva-mem-push "Renamed 2021-08-24")
(defalias 'eva-mem-pushnew-alt #'eva-mem-push-alt "Renamed 2021-08-24")

;;;###autoload
(define-minor-mode eva-mode
  "Wake up the virtual assistant."
  :global t
  (if eva-mode
      (progn
        (when eva-debug
          (message "------ (Eva debug) Trying to turn on. ------"))
        ;; Check to see whether it's ok to turn on.
        (when (and (or (eva--idle-set-fn)
                       (prog1 nil
                         (message
                          (concat eva-va-name
                                  ": Not able to detect idleness, I'll be"
                                  " useless.  Disabling eva-mode."))
                         (eva-mode 0)))
                   (if (eva--another-eva-running-p)
                       (prog1 nil
                         (message
                          (concat "Another VA active, I won't get"
                                  " in their way.  Disabling eva-mode."))
                         (eva-mode 0))
                     t))
          ;; All OK, turn on.
          (mkdir "/tmp/eva" t)
          (f-write (number-to-string (emacs-pid)) 'utf-8 "/tmp/eva/pid")
          (add-function :after after-focus-change-function #'eva--log-buffer)
          (add-hook 'window-buffer-change-functions #'eva--log-buffer)
          (add-hook 'window-selection-change-functions #'eva--log-buffer)
          (add-hook 'after-init-hook #'eva--init -90)
          (add-hook 'after-init-hook #'eva--emacs-init-message 1)
          (add-hook 'after-init-hook #'eva--check-for-time-anomalies 2)
          (add-hook 'after-init-hook #'eva--init-r 3)
          (add-hook 'after-init-hook #'eva--delayed-start-timer 90)
          (when after-init-time
            (progn
              (eva--init)
              (eva--check-for-time-anomalies)
              (eva--init-r)
              (eva--delayed-start-timer)))
          (named-timer-run :eva-keepalive 300 300 #'eva--keepalive)
          (when eva-debug
            (message "------ (Eva debug) Mode turned on. ----------"))))
    ;; Turn off.
    (unless (eva--another-eva-running-p)
      (eva-emit "Turning off.")
      (eva--save-vars-to-disk)
      (ignore-errors (f-delete "/tmp/eva/pid")))
    (setq eva--idle-secs-fn nil)
    (remove-function after-focus-change-function #'eva--log-buffer)
    (remove-hook 'window-buffer-change-functions #'eva--log-buffer)
    (remove-hook 'window-selection-change-functions #'eva--log-buffer)
    (remove-hook 'after-init-hook #'eva--init)
    (remove-hook 'after-init-hook #'eva--emacs-init-message)
    (remove-hook 'after-init-hook #'eva--check-for-time-anomalies)
    (remove-hook 'after-init-hook #'eva--init-r)
    (remove-hook 'after-init-hook #'eva--delayed-start-timer)
    (remove-hook 'kill-emacs-hook #'eva--save-vars-to-disk)
    (named-timer-cancel :eva)
    (named-timer-cancel :eva-retry)
    (named-timer-cancel :eva-excursion)
    (named-timer-cancel :eva-keepalive)))

(provide 'eva)

;;; eva.el ends here
