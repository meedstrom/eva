;;; ass-builtin.el --- Premade applications -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Martin Edström

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

;; Premade queries, hooks and other uses of the core library. I dogfood these,
;; you can use them as-is or define your own alternatives.

;;; Code:

(require 'ass)

;; Calm the compiler.
(declare-function ess-execute "ess")
(declare-function ledger-report "ledger-report")
(declare-function ledger-report-goto "ledger-report")
(declare-function org-agenda-archives-mode "org-agenda")
(declare-function org-agenda-log-mode "org-agenda")
(declare-function org-map-region "org")
(declare-function org-next-visible-heading "org")
(declare-function org-outline-level "org")
(declare-function org-promote "org")
(declare-function org-reduced-level "org")
(declare-function org-with-wide-buffer "org-macs")
(defvar ledger-report-buffer-name)
(defvar ledger-reports)
(defvar org-agenda-files)
(defvar org-capture-templates)
(defvar org-fontify-done-headline)
(defvar org-journal-dir)
(defvar org-journal-file-format)
(defvar org-journal-file-type)


;;; Collection of basic stuff
;; Improvements to the core library should mean we can simplify the more
;; complex definitions and move them to this section.

(ass-wrap ass-greet ()
  (message (ass-emit (ass-greeting)))
  (sit-for ass-sit-long))

(ass-wrap ass-query-meditation ()
  "Ask user whether they meditated and how long."
  (when (ass-ynp "Did you meditate today?")
    (let* ((mins (ass-read-string "Do you know how long (in minutes)? "))
           (cleaned-mins (number-to-string (string-to-number mins)))) ; HACK
      (ass-tsv-append ass-curr-dataset
        (ts-format)
        "TRUE"
        (unless (string= "0" cleaned-mins) cleaned-mins)))))

(ass-wrap ass-query-cold-shower ()
  "Ask user to rate their cold exposure today."
  (let ((rating (ass-read-string "Cold rating? ")))
    (ass-tsv-append ass-curr-dataset
      (ts-format ass-date)
      rating)))

(ass-wrap ass-query-ingredients ()
  "Ask user for a description of what they ate."
  (let* ((response (ass-read-string
                    "What ingredients did you eat recently? ")))
    (ass-tsv-append ass-curr-dataset
      (ts-format ass-date)
      response)
    (ass-emit "Ingredients recorded today: "
              (->> (ass-tsv-entries-by-date ass-curr-dataset)
                   (nreverse)
                   (-map #'-last-item)
                   (s-join ", ")
                   (s-replace ",," ",")))))

(ass-wrap ass-present-org-agenda ()
  "Send the user to an Org agenda log with archives enabled.
Near equivalent to typing l v A after entering `org-agenda-list'."
  (require 'org-agenda)
  (message (ass-emit "Here's the agenda archive as of now."))
  ;; (message (ass-emit "Sending you to the Org agenda log + archive."))
  (sit-for ass-sit-medium)
  (org-agenda-list)
  (org-agenda-log-mode t)
  (org-agenda-archives-mode t)
  (push (current-buffer) ass-excursion-buffers)
  (keyboard-quit)
  (keyboard-quit))


;;; Mood

(defvar ass-mood-alist nil
  "For suggesting a score in the `ass-query-mood' prompt.
Merely a convenience for auto-completion. The variable populates
itself through use.")

(ass-wrap ass-query-mood ()
  "Ask user about their mood."
  (let* ((first-response (ass-read
                          "Your mood: "
                          (--sort (> 0 (random))
                                  (map-keys ass-mood-alist))))
         (old-score (cdr (assoc first-response ass-mood-alist)))
         (prompt-for-score
          (concat "Score from 1 to 10"
                  (when old-score " (default " old-score ")")
                  ": "))
         (second-response
          (if (s-numeric? first-response)
              (ass-read "Mood description (optional): ")
            (ass-read-string prompt-for-score nil nil old-score)))
         (mood-desc (if (s-numeric? first-response)
                        second-response
                      first-response))
         (score (if (s-numeric? first-response)
                    first-response
                  second-response))
         (score-num (string-to-number score)))
    (ass-tsv-append ass-curr-dataset
      (ts-format)
      (s-replace "," "." score)
      mood-desc)
    (when ass-debug
      (ass-emit "------ (debug message) Recorded mood: "
                (s-join " " (cdr (ass-tsv-last-row
                                  ass-curr-dataset)))))
    ;; Update ass-mood-alist.
    (if (assoc mood-desc ass-mood-alist)
        (setq ass-mood-alist
              (--replace-where (string= (car it) mood-desc)
                               (cons (car it) score)
                               ass-mood-alist))
      (push (cons mood-desc score) ass-mood-alist))
    (when (and (<= score-num 1)
               (ass-ynp "Do you want to talk?")
               (ass-ynp "I can direct you to my colleague ELIZA, though"
                        " she's not too bright.  Will that do?"))
      (doctor)
      (ass-stop-watching-excursion)
      (keyboard-quit))
    score-num))

(add-hook 'ass-after-load-vars-hook
          (defun ass-mood-load ()
            "Reload `ass-mood-alist' from memory."
            (setq ass-mood-alist
                  (map-elt ass-mem 'ass-mood-alist))))

(add-hook 'ass-before-save-vars-hook
          (defun ass-mood-save ()
            "Save `ass-mood-alist' to memory."
            (ass-mem-pushnew 'ass-mood-alist)))


;;; Weight

(ass-wrap ass-query-weight ()
  "Ask user about their weight."
  (let* ((last-wt (ass-tsv-last-value ass-curr-dataset))
         (wt (ass-read "What do you weigh today? "
                       `(,last-wt
                         "I don't know")
                       last-wt)))
    (if (= 0 (string-to-number wt))
        ;; user typed only non-numeric characters
        (ass-emit "Ok, I'll ask you again later.")
      (ass-tsv-append ass-curr-dataset
        (ts-format ass-date)
        (s-replace "," "." wt))
      (ass-emit "Weight today: "
                (ass-tsv-last-value ass-curr-dataset)
                " kg"))))

;; TODO: Pass start-date (today minus 3mo) and projection incline, letting
;;       user change the incline
;; TODO: Persist the buffer content across restarts
(defun ass-plot-weight ()
  "Present a plot of the user's weight."
  (interactive)
  (setq ass-curr-fn #'ass-plot-weight)
  (mkdir "/tmp/ass" t)
  ;; Refresh data with R.
  (let ;; FIXME: Allow custom item fns, and allow empty `ass-items'
      ((weight-path (ass-item-dataset
                     (ass-item-by-fn #'ass-query-weight)))
       (mood-path (ass-item-dataset
                   (ass-item-by-fn #'ass-query-mood))))
    (with-current-buffer ass--buffer-r
      (ess-execute (concat "weight_dataset <- '" weight-path "'") 'buffer)
      (ess-execute (concat "mood_dataset <- '" mood-path "'") 'buffer)
      (ess-execute "source(\"make_data_for_plots.R\")" 'buffer)))
  ;; Plot with gnuplot.
  (let* ((pkg-loc (convert-standard-filename
                   (f-dirname (find-library-name "ass"))))
         (gnuplot-script-path (convert-standard-filename
                               (expand-file-name "weight.gnuplot" pkg-loc))))
    (with-current-buffer (get-buffer-create
                          (concat "*" (symbol-name ass-curr-fn) "*"))
      (let ((reserve (buffer-string)))
        (delete-region (point-min) (point-max))
        (message (ass-emit "Plotting your weight..."))
        ;; TODO: make it more informative for debugging, don't use
        ;; ignore-errors. Ideally bury the buffer upon error and emit
        ;; stderr to the chat log.
        (unless (ignore-errors (call-process "gnuplot" gnuplot-script-path t))
          ;; On error, keep showing the old plot. Hopefully error messages will
          ;; trail below.
          (goto-char (point-min))
          (insert reserve)))
      (display-buffer (current-buffer)))))


;;; Sleep

;; TODO: (Feature) Look at when idle ended to suggest a quantity.
;; TODO: (Feature) Let user say "since 21" instead of quantity
;; TODO: Fix the case where someone wakes up at 23:00 but replies to the query
;;       at 01:00. Notice the unusual hour change and ask if user meant 23
;;       yesterday.
;; TODO: Generally react when it's 00-04 or so.
(ass-wrap ass-query-sleep ()
  "Query you for wake-up time and sleep quantity for one sleep block today.
You are free to decline either query, but you should not later
register sleep quantity from this same block in order to \"get
the totals correct\" -- the database will interpret it as a
different sleep block and continue to count the original one as
having a censored (nonzero!) quantity of sleep on top of what you
add."
  (let* ((today-rows (ass-tsv-entries-by-date
                      ass-curr-dataset
                      (ts-dec 'day 1 ass-date)))
         (total-yesterday (-sum (--map (string-to-number (nth 3 it))
                                       today-rows))))
    ;; Totalling less than 4 hours is unusual, so a possible anomaly in data.
    (if (> (* 60 4) total-yesterday)
        (if (ass-ynp "Yesterday, you slept "
                     (number-to-string (round (/ total-yesterday 60.0)))
                     " hours, is this about right?")
            nil
          (when (ass-ynp "Edit \"" ass-curr-dataset "\"?")
            (find-file ass-curr-dataset)
            (push (current-buffer) ass-excursion-buffers)
            ;; prevent counting this run as a success
            (ass-stop-watching-excursion)
            (keyboard-quit)))))
  (let* ((recently-hhmm (ts-format "%H:%M" (ts-dec 'minute 10 ass-date)))
         (recently-response (concat "Recently (" recently-hhmm ")"))
         (wakeup-time
          (let* ((reply (ass-read
                         (concat "I assume you have slept today ("
                                 (ts-format "%A" ass-date)
                                 "). When did you wake? ")
                         (list "I don't know"
                               recently-response))))
            (cond ((equal reply recently-response)
                   recently-hhmm)
                  ((s-match (rx num) reply)
                   (ass-coerce-to-hh-mm reply))
                  (t nil))))
         (sleep-minutes
          (ass-parse-time-amount
           (ass-read "How long did you sleep? "
                     `("I don't know"
                       ,(number-to-string
                         (/ ass-length-of-last-idle 60 60)))))))
    (ass-emit (when wakeup-time
                (concat "You woke at " wakeup-time ". "))
              (when sleep-minutes
                (concat "You slept " (number-to-string sleep-minutes)
                        " minutes (" (number-to-string
                                      (/ sleep-minutes 60.0))
                        " hours)."))
              (when (-all-p #'null '(wakeup-time sleep-minutes))
                (concat "One sleep block recorded without metrics.")))
    (ass-tsv-append ass-curr-dataset
      (ts-format "%F" ass-date) ;; date (no time component)
      wakeup-time ;; time (optional)
      (when sleep-minutes (number-to-string sleep-minutes)))))


;;; Ledger & finances

(defcustom ass-main-ledger-path
  (convert-standard-filename "~/my.ledger")
  "File used by `ass-present-ledger-report'."
  :group 'ass
  :type 'file)

(ass-wrap ass-present-ledger-report ()
  "Jump to `ass-main-ledger-path' and run `ledger-report'.
Uses the first command specified in `ledger-reports'."
  (cond ((not (f-exists-p ass-main-ledger-path))
         (message (ass-emit
                   "ass-main-ledger-path does not refer to existing"
                   " file, skipping Ledger report.")))
        ((not (require 'ledger-mode nil t))
         (message (ass-emit
                   "Ledger-mode failed to load, skipping Ledger report.")))
        (t
         (message (ass-emit "Here's your Ledger report, have fun."))
         (if (get-buffer ledger-report-buffer-name)
             (ledger-report-goto)
           (with-current-buffer (find-file-noselect ass-main-ledger-path)
             (ledger-report (caar ledger-reports) nil)))
         (push (get-buffer ledger-report-buffer-name)
               ass-excursion-buffers)
         (keyboard-quit))))

(ass-wrap ass-present-ledger-file ()
  (unless (f-exists-p ass-main-ledger-path)
    (warn "not found: ass-main-ledger-path"))
  (message (ass-emit "Here's your ledger.  Please, edit."))
  (sit-for ass-sit-medium)
  (view-file-other-window ass-main-ledger-path)
  (push (current-buffer) ass-excursion-buffers)
  (goto-char (point-max))
  (keyboard-quit))

(defun ass-make-ods-for-finance ()
  "Make and open an ODS spreadsheet from Ledger data.
Requires the ssconvert program that comes with Gnumeric."
  (interactive)
  (unless (f-exists-p ass-main-ledger-path)
    (warn "not found: ass-main-ledger-path"))
  (let* ((script (expand-file-name "generate_an_ods.R"
                                   (f-dirname
                                    (find-library-name "ass"))))
         (sheet (expand-file-name "tmp_finances.ods"
                                  (temporary-file-directory)))
         (default-directory (f-dirname script))
         (app (seq-find #'executable-find '("gnumeric"
                                            "soffice"
                                            "open"
                                            "mimeopen"
                                            "xdg-open"))))
    (if (= 0 (call-process "Rscript" nil nil nil
                           script ass-main-ledger-path sheet))
        (pfuture-new app sheet)
      (message (ass-emit "Error running " script)))))


;;; Diary
(defcustom ass-main-datetree-path
  "~/org/archive.org"
  "The file name of your main datetree, if you have one.
Only relevant if you have one you use as a big archive file, see
Info node `(org) Moving subtrees', or you write/capture
diary entries directly into.  Checked by `ass-present-diary'."
  :group 'ass
  :type 'file)

(defvar ass-past-sample-function #'ass-past-sample-greedy)

(defun ass-past-sample-greedy (&optional ts)
  "Return a list of ts objects.
They refer to yesterday, this weekday the last 4 weeks, this day
of the month the last 12 months, and this date the last 50
years. Optionally, the point of reference can be TS instead of
today."
  (let ((now (or ts (ts-now))))
    (-uniq (append
            (--iterate (ts-dec 'day 1 it) now 1) ;; yesterday
            (--iterate (ts-dec 'woy 1 it) now 4)
            (--iterate (ts-dec 'month 1 it) now 12)
            (--iterate (ts-dec 'year 1 it) now 50)))))

(defun ass-past-sample-casual (&optional ts)
  "Return a list of ts objects.
They refer to to yesterday, this this day of the month the last 6
months, and this date the last 50 years. Optionally, the point of
reference can be TS instead of today."
  (let ((now (or ts (ts-now))))
    (-uniq (append
            (--iterate (ts-dec 'day 1 it) now 1)
            (--iterate (ts-dec 'month 1 it) now 6)
            (--iterate (ts-dec 'year 1 it) now 50)))))

;; TODO: Allow a list of datetrees
(defun ass-make-indirect-datetree (buffer dates)
  "Replace BUFFER contents with a datetree of archive entries.
Searches `ass-main-datetree-path' for entries matching
members in DATES (ts objects). Return the count of dates that
were found to have entries."
  (require 'org)
  (let ((dates (-sort 'ts<= dates))
        (counter 0)
        ;; Doom greys out the entire headline, making it hard to read, which is
        ;; good in working files, but bad for perusing an archive.
        (org-fontify-done-headline nil))
    (switch-to-buffer buffer)
    (org-mode)
    (delete-region (point-min) (point-max))
    (with-temp-buffer
      (insert-file-contents ass-main-datetree-path)
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
        (progn
          (dotimes (_ 2)
            (org-map-region #'org-promote (point-min) (point-max)))
          (org-global-cycle '(4)))
      (kill-buffer buffer))
    counter))

(defun ass-existing-diary (&optional date dir file-format)
  "Return the first file in DIR matching FILE-FORMAT.
FILE-FORMAT is handled by `parse-time-string'. The value returned
is a full filesystem path or nil.

When DATE is nil, use `ass-date'.  Should be a ts object.
When DIR is nil, use `org-journal-dir'.
When FILE-FORMAT is nil, use `org-journal-file-format'; if that's
 unset, use \"%F.org\".

Note that org-journal is not needed."
  (let* ((dir (or dir
                  (bound-and-true-p org-journal-dir)))
         (file-format (or file-format
                          (and (boundp 'org-journal-file-type)
                               (boundp 'org-journal-file-format)
                               (eq org-journal-file-type 'daily)
                               org-journal-file-format)
                          "%F.org"))
         (file (--find (s-contains-p (ts-format file-format
                                                (or date ass-date))
                                     it)
                       (directory-files dir))))
    (unless (null file)
      (expand-file-name file dir))))

;; TODO: (Feature) Make separate buffers for each datetree-based entry (use
;;                 rename-buffer?) and interleave with discrete files so it's
;;                 all in chrono order.
;; TODO: (Feature) Try creating a sparse tree, so user can edit in-place
;; TODO: (Feature) Maybe show the agenda log taken from each date?
(ass-wrap ass-present-diary ()
  "Show user a selection of past diary entries."
  (let* ((dates-to-check (funcall ass-past-sample-function ass-date))
         (discrete-files-found (--keep (ass-existing-diary it) dates-to-check))
         (buffer (get-buffer-create (concat "*" ass-ai-name ": Selected diary entries*")))
         (datetree-found-count (ass-make-indirect-datetree buffer dates-to-check))
         (total-found-count (+ (length discrete-files-found) datetree-found-count)))
    (if (= 0 total-found-count)
        (message (ass-emit "No diary entries relevant to this date."))
      (when (or (when ass-presumptive
                  (ass-emit "Opening " (int-to-string total-found-count) " diary entries.")
                  t)
                (ass-ynp "Found " (int-to-string total-found-count) " past diary "
                         (if (= 1 total-found-count) "entry" "entries")
                         " relevant to this date. Want me to open "
                         (if (= 1 total-found-count) "it" "them")
                         "?"))
        (if (= 0 datetree-found-count)
            (kill-buffer buffer)
          ;; TODO: pressing q should kill it!
          (view-buffer buffer #'kill-buffer-if-not-modified)
          (push (current-buffer) ass-excursion-buffers))
        (when (-non-nil discrete-files-found)
          (dolist (x discrete-files-found)
            (view-file x)
            (push (current-buffer) ass-excursion-buffers)))
        (keyboard-quit)))))


;;; Org

(add-hook 'ass-before-save-vars-hook
          (defun ass-save-org-variables ()
            "Sync certain org settings to mem."
            (when (featurep 'org-clock)
              (ass-mem-pushnew 'org-clock-current-task))
            (when (featurep 'org-agenda)
              (ass-mem-pushnew 'org-agenda-files))
            ;; Transform newlines; ass-tsv-append correctly refuses them.
            (when (featurep 'org-capture)
              (let ((transformed-org-templates
                     (cl-loop for template in org-capture-templates
                              collect (--map (if (stringp it)
                                                 (s-replace "\n" "\\n" it)
                                               it)
                                             template))))
                (ass-mem-pushnew-alt transformed-org-templates)))))

(defun ass-check-org-variables ()
  "Alert user if certain Org settings have changed.
Suitable on `ass-after-load-vars-hook'."
  (let ((restored-templates
         (cl-loop for template in (map-elt ass-mem 'transformed-org-templates)
                  collect (--map (if (stringp it)
                                     (s-replace "\\n" "\n" it)
                                   it)
                                 template))))
    (when ass-debug
      (if (equal restored-templates org-capture-templates)
          (message (ass-emit "org-capture-templates unchanged"))
        (message (ass-emit "org-capture-templates changed!")))
      (if (equal org-agenda-files (map-elt ass-mem 'org-agenda-files))
          (message (ass-emit "org-agenda-files unchanged"))
        (message (ass-emit "org-agenda-files changed!"))))))

;; UNTESTED
(defun ass-check-clock ()
  "If there's a dangling clock, prompt to load Org.
Suitable on `ass-after-load-vars-hook'."
  (and (map-elt ass-mem 'org-clock-current-task)
       (ass-ynp "Dangling clock found, activate Org?")
       (require 'org-clock)))

(provide 'ass-builtin)

;; Local Variables:
;; nameless-current-name: "ass"
;; End:

;;; ass-builtin.el ends here
