;;; eva-builtin.el --- Premade applications -*- lexical-binding: t; -*-

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

;; Premade queries, hooks and other uses of the core library.  I dogfood these,
;; you can use them as-is or define your own alternatives.

;;; Code:

(require 'eva)
;; (require 'l) ;; cool

;; Silence the compiler.
(declare-function ess-execute "ess-inf")
(declare-function ess-wait-for-process "ess-inf")
(declare-function ledger-report "ext:ledger-report")
(declare-function ledger-report-goto "ext:ledger-report")
(declare-function org-agenda-archives-mode "org-agenda")
(declare-function org-agenda-log-mode "org-agenda")
(declare-function org-map-region "org")
(declare-function org-next-visible-heading "org")
(declare-function org-outline-level "org")
(declare-function org-promote "org")
(declare-function org-reduced-level "org")
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
;; complex definitions further below and move them to this section.

(eva-defun eva-greet ()
  (message (eva-emit (eva-greeting)))
  (sit-for eva-sit-long))

(eva-defun eva-query-meditation ()
  "Ask user whether they meditated and how long."
  (when (eva-ynp "Did you meditate today?")
    (let* ((mins (eva-read-string "Great! Do you know how long (in minutes)? "))
           (cleaned-mins (when mins (number-to-string (string-to-number mins))))) ; HACK
      (when mins
        (eva-tsv-append eva-curr-dataset
          (ts-format)
          "TRUE"
          (unless (string= "0" cleaned-mins) cleaned-mins))))))

(eva-defun eva-query-cold-shower ()
  "Ask user to rate their cold exposure today."
  (let ((rating (eva-read-string "Cold rating? ")))
    (when rating
      (eva-tsv-append eva-curr-dataset
        (ts-format eva-date)
        rating))))

(eva-defun eva-query-ingredients ()
  "Ask user for a description of what they ate."
  (let* ((response (eva-read-string
                    "What ingredients did you eat recently? ")))
    (when response
      (eva-tsv-append eva-curr-dataset
        (ts-format eva-date)
        response)
      (eva-emit "Ingredients recorded today: "
                (->> (eva-tsv-entries-by-date eva-curr-dataset)
                     (nreverse)
                     (-map #'-last-item)
                     (s-join ", ")
                     (s-replace ",," ","))))))

(eva-defun eva-query-mood-numeric ()
  "Ask user about their mood."
  (let* ((score (eva-read-string "How do you feel? (score from 1-10): "))
         (score-num (string-to-number score)))
    (when score
      (eva-tsv-append eva-curr-dataset
        (ts-format)
        (s-replace "," "." score)
        nil) ;; use the same dataset as eva-query-mood
      (when (and (s-numeric? score) ;; needed b/c typing numless junk makes it 0
                 (<= score-num 1)
                 (eva-ynp "Do you want to talk?")
                 (eva-ynp "I can direct you to my colleague ELIZA, though"
                          " she's not too bright.  Will that do?"))
        (doctor)
        (eva-stop-queue))
      score-num)))

(defalias 'eva-present-org-agenda #'eva-present-org-agenda-log-archive
  "Deprecated 2021-09-17 -- misnamed")

(eva-defun eva-present-org-agenda-log-archive ()
  "Send the user to an Org agenda with log and archives enabled.
Near equivalent to typing l v A after entering `org-agenda-list'."
  (require 'org-agenda)
  (message (eva-emit "Here's the agenda archive as of now."))
  ;; (message (eva-emit "Sending you to the Org agenda log + archive."))
  (sit-for eva-sit-short)
  (org-agenda-list)
  (org-agenda-log-mode t)
  (org-agenda-archives-mode t)
  (push (current-buffer) eva-excursion-buffers)
  (eva-stop-queue))

(eva-defun eva-present-org-agenda-log ()
  "Send the user to an Org agenda with log enabled.
Equivalent to typing l after entering `org-agenda-list'."
  (require 'org-agenda)
  (message (eva-emit "Here's the agenda as of now."))
  ;; (message (eva-emit "Sending you to the Org agenda log + archive."))
  (sit-for eva-sit-short)
  (org-agenda-list)
  (org-agenda-log-mode t)
  (push (current-buffer) eva-excursion-buffers)
  (eva-stop-queue))


;;; Activity

(cl-defstruct (eva-activity
               (:constructor eva-activity-create)
               (:copier nil))
  name
  id
  cost-false-pos
  cost-false-neg
  query)

(defvar eva-activity-list)

(defun eva-activity-by-name (name)
  "Get the first activity in `eva-activitys' matching NAME."
  (--find (equal name (eva-activity-name it)) eva-activity-list))

(defun eva-activity-names ()
  "Get the :name of all members of `eva-activity-list'."
  (-map #'eva-activity-name eva-activity-list))

;; TODO: Get all informally named activities from the item's dataset.
(eva-defun eva-query-activity ()
  "Ask user what they're up to."
  (let* ((name (eva-read "What are you up to? " (eva-activity-names)))
         (name-corrected
          (--find (member it (eva-activity-names))
                  (list name (capitalize name) (downcase name))))
         (name (if name-corrected
                   name-corrected
                 name))
         (activity (eva-activity-by-name name)))
    (when name
      (eva-tsv-append eva-curr-dataset
        (ts-format eva-date) ;; the time the activity happened
        name
        (when activity
          (eva-activity-id activity))))))


;;; Mood

(defvar eva-mood-alist nil
  "For suggesting a score in the `eva-query-mood' prompt.
Merely a convenience for auto-completion.  The variable populates
itself through use.")

(eva-defun eva-query-mood ()
  "Ask user about their mood."
  (let* ((first-response (eva-read
                          "Your mood: "
                          (sort (map-keys eva-mood-alist)
                                (lambda (_1 _2) (> 0 (random))))))
         (old-score (cdr (assoc first-response eva-mood-alist)))
         (prompt-for-score
          (concat "Score from 1 to 10"
                  (when old-score " (default " old-score ")")
                  ": "))
         (second-response
          (if (s-numeric? first-response)
              (eva-read "Mood description (optional): ")
            (eva-read-string prompt-for-score nil nil old-score)))
         (mood-desc (if (s-numeric? first-response)
                        second-response
                      first-response))
         (score (if (s-numeric? first-response)
                    first-response
                  second-response))
         (score-num (string-to-number score)))
    (when (and first-response second-response)
      (eva-tsv-append eva-curr-dataset
        (ts-format)
        (s-replace "," "." score)
        mood-desc)
      (when eva-debug
        (eva-emit "------ (debug message) Recorded mood: "
                  (s-join " " (cdr (eva-tsv-last-row
                                    eva-curr-dataset)))))
      ;; Update eva-mood-alist.
      (if (assoc mood-desc eva-mood-alist)
          (setq eva-mood-alist
                (--replace-where (string= (car it) mood-desc)
                                 (cons (car it) score)
                                 eva-mood-alist))
        (push (cons mood-desc score) eva-mood-alist))
      (when (and  (s-numeric? score) ;; needed b/c typing numless junk makes it 0
                  (<= score-num 1)
                  (eva-ynp "Do you want to talk?")
                  (eva-ynp "I can direct you to my colleague ELIZA, though"
                           " she's not too bright.  Will that do?"))
        (doctor)
        (eva-stop-queue))
      score-num)))

(add-hook 'eva-after-load-vars-hook
          (defun eva-mood-load ()
            "Reload `eva-mood-alist' from memory."
            (setq eva-mood-alist
                  (map-elt eva-mem 'eva-mood-alist))))

(add-hook 'eva-before-save-vars-hook
          (defun eva-mood-save ()
            "Save `eva-mood-alist' to memory."
            (eva-mem-push 'eva-mood-alist)))


;;; Weight

(eva-defun eva-query-weight ()
  "Ask user about their weight."
  (let* ((last-wt (eva-tsv-last-value eva-curr-dataset))
         (wt (eva-read "What do you weigh today? "
                       `(,last-wt
                         "I don't know")
                       last-wt)))
    (if (= 0 (string-to-number wt))
        ;; user typed only non-numeric characters
        (eva-emit "Ok, I'll ask you again later.")
      (when wt
        (eva-tsv-append eva-curr-dataset
          (ts-format eva-date)
          (s-replace "," "." wt))
        (eva-emit "Weight today: "
                  (eva-tsv-last-value eva-curr-dataset)
                  " kg")))))

;; TODO: Let's pass the datasets as args, from eva-item-create :args
;; TODO: Pass start-date (today minus 3mo) and projection incline, letting
;;       user change the incline
;; TODO: Persist the buffer content across restarts
(defun eva-plot-weight ()
  "Present a plot of the user's weight."
  (interactive)
  (setq eva-curr-fn #'eva-plot-weight)
  (mkdir "/tmp/eva" t)
  ;; Refresh data with R.
  (let ;; FIXME: Allow any item fns instead of hardcoding these two
      ((weight-path (eva-item-dataset (eva-item-by-fn #'eva-query-weight)))
       (mood-path (eva-item-dataset (or
                                     (eva-item-by-fn #'eva-query-mood-numeric)
                                     (eva-item-by-fn #'eva-query-mood)))))
    (with-current-buffer eva--buffer-r
      (ess-execute (concat "weight_dataset <- '" weight-path "'") 'buffer)
      (ess-execute (concat "mood_dataset <- '" mood-path "'") 'buffer)
      (ess-execute "source(\"make_data_for_plots.R\")" 'buffer)))
  ;; Plot with gnuplot.
  (let* ((pkg-loc (convert-standard-filename
                   (f-dirname (find-library-name "eva"))))
         (gnuplot-script-path (convert-standard-filename
                               (expand-file-name "weight.gnuplot" pkg-loc))))
    (with-current-buffer (get-buffer-create
                          (concat "*" (symbol-name eva-curr-fn) "*"))
      (let ((reserve (buffer-string)))
        (delete-region (point-min) (point-max))
        (message (eva-emit "Plotting your weight..."))
        (ess-wait-for-process eva--r-process t 0.1 nil 2)
        ;; TODO: Make it more informative for debugging. Ideally emit stderr to
        ;;       the chat log.
        (if (= 0 (call-process "gnuplot" gnuplot-script-path t))
            (progn
              (push (current-buffer) eva-excursion-buffers)
              (eva-dbg "Gnuplot success.")
              (view-buffer (current-buffer) #'kill-buffer)
              (eva-stop-queue))
          ;; On error, keep showing the old plot. Hopefully error messages will
          ;; trail below.
          (eva-dbg "Gnuplot failed.")
          (goto-char (point-min))
          (insert reserve))))))


;;; Sleep

;; TODO: (Feature) Look at when idle ended to suggest a quantity.
;; TODO: (Feature) Let user say "since 21" instead of quantity
;; TODO: Fix the case where someone wakes up at 23:00 but replies to the query
;;       at 01:00. Notice the unusual hour change and ask if user meant 23
;;       yesterday.
;; TODO: Generally react when it's 00-04 or so.
(eva-defun eva-query-sleep ()
  "Query you for wake-up time and sleep quantity for one sleep block today.
You are free to decline either query, but you should not later
register sleep quantity from this same block in order to \"get
the totals correct\" -- the database will interpret it as a
different sleep block and continue to count the original one as
having a censored (nonzero!) quantity of sleep on top of what you
add."
  (let* ((today-rows (eva-tsv-entries-by-date
                      eva-curr-dataset
                      (ts-dec 'day 1 eva-date)))
         (total-yesterday (-sum (--map (string-to-number (nth 3 it))
                                       today-rows))))
    ;; Totalling less than 4 hours is unusual, so a possible anomaly in data.
    (if (> (* 60 4) total-yesterday)
        (if (eva-ynp "Yesterday, you slept "
                     (number-to-string (round (/ total-yesterday 60.0)))
                     " hours, is this about right?")
            nil
          (when (eva-ynp "Edit \"" eva-curr-dataset "\"?")
            (find-file eva-curr-dataset)
            (push (current-buffer) eva-excursion-buffers)
            ;; TODO: don't count this run as a success ...
            ;; Use the old stop-watching-excursion function
            (eva-stop-queue)))))
  (let* ((recently-hhmm (ts-format "%H:%M" (ts-dec 'minute 10 eva-date)))
         (recently-response (concat "Recently (" recently-hhmm ")"))
         (wakeup-time
          (let* ((reply (eva-read
                         (concat "I assume you have slept today ("
                                 (ts-format "%A" eva-date)
                                 "). When did you wake? ")
                         (list "I don't know"
                               recently-response)
                         recently-hhmm)))
            (cond ((equal reply recently-response)
                   recently-hhmm)
                  ((s-match (rx num) reply)
                   (eva-coerce-to-hh-mm reply))
                  (t nil))))
         (suggested-length (concat (eva-one-decimal
                                    (number-to-string
                                     (/ eva-length-of-last-idle 60 60))) " h"))
         (sleep-minutes
          (eva-parse-time-amount
           (eva-read "How long did you sleep? "
                     (list suggested-length
                           "I don't know")
                     suggested-length))))
    ;; TODO: save data even if user typed /skip on the second q
    (when (and wakeup-time sleep-minutes)
      (eva-emit (when wakeup-time
                  (concat "You woke at " wakeup-time ". "))
                (when sleep-minutes
                  (concat "You slept " (number-to-string (round sleep-minutes))
                          " minutes (" (eva-one-decimal (number-to-string
                                                         (/ sleep-minutes 60.0)))
                          " hours)."))
                (when (-all-p #'null '(wakeup-time sleep-minutes))
                  (concat "One sleep block recorded without metrics.")))
      (eva-tsv-append eva-curr-dataset
        (ts-format "%F" eva-date) ;; date (no time component)
        wakeup-time ;; time (optional)
        (when sleep-minutes (number-to-string (round sleep-minutes)))))))


;;; Ledger & finances

(defcustom eva-main-ledger-path
  (convert-standard-filename "~/my.ledger")
  "File used by `eva-present-ledger-report'."
  :group 'eva
  :type 'file)

(eva-defun eva-present-ledger-report ()
  "Jump to `eva-main-ledger-path' and run `ledger-report'.
Uses the first command specified in `ledger-reports'."
  (cond ((not (f-exists-p eva-main-ledger-path))
         (message (eva-emit "eva-main-ledger-path does not refer to existing"
                            " file, skipping Ledger report.")))
        ((not (require 'ledger-mode nil t))
         (message (eva-emit "Ledger-mode failed to load, skipping report.")))
        (t
         (message (eva-emit "Here's your Ledger report, have fun."))
         (sit-for eva-sit-short)
         (if (get-buffer ledger-report-buffer-name)
             (ledger-report-goto)
           (with-current-buffer (find-file-noselect eva-main-ledger-path)
             (ledger-report (caar ledger-reports) nil)))
         (push (get-buffer ledger-report-buffer-name) eva-excursion-buffers)
         (eva-stop-queue))))

(eva-defun eva-present-ledger-file ()
  (unless (f-exists-p eva-main-ledger-path)
    (warn "not found: eva-main-ledger-path"))
  (message (eva-emit "Here's your ledger.  Please, edit."))
  (view-file-other-window eva-main-ledger-path)
  (sit-for eva-sit-medium)
  (push (current-buffer) eva-excursion-buffers)
  (unless save-place-mode
    (goto-char (point-max)))
  (eva-stop-queue))

;; TODO: Explain/screencast in docs
(defun eva-make-ods-for-finance ()
  "Make and open an ODS spreadsheet from Ledger data.
Requires the ssconvert program that comes with Gnumeric."
  (interactive)
  (unless (f-exists-p eva-main-ledger-path)
    (warn "not found: eva-main-ledger-path"))
  (let* ((script (expand-file-name "generate_an_ods.R"
                                   (f-dirname
                                    (find-library-name "eva"))))
         (sheet (expand-file-name "tmp_finances.ods"
                                  (temporary-file-directory)))
         (default-directory (f-dirname script))
         (app (seq-find #'executable-find '("gnumeric"
                                            "soffice"
                                            "open"
                                            "mimeopen"
                                            "xdg-open"))))
    (if (= 0 (call-process "Rscript" nil nil nil
                           script eva-main-ledger-path sheet))
        (start-process app nil app sheet)
      (message (eva-emit "Error running " script)))))


;;; Diary
(defcustom eva-main-datetree-path
  "~/org/archive.org"
  "The file name of your main datetree, if you have one.
Only relevant if you have one you use as a big archive file, see
Info node `(org) Moving subtrees', or one to which you
write/capture diary entries directly, in the same style as Doom's
`+org-capture-journal-file'.  This file is scanned by
`eva-present-diary'."
  :group 'eva
  :type 'file)

(defvar eva-past-sample-function #'eva-past-sample-greedy)

(defun eva-past-sample-greedy (&optional ts)
  "Return a list of ts objects.
They refer to yesterday, this weekday the last 4 weeks, this day
of the month the last 12 months, and this date the last 50 years.
Optionally, the point of reference can be TS instead of today."
  (let ((now (or ts (ts-now))))
    (-uniq (append
            (--iterate (ts-dec 'day 1 it) now 1) ;; yesterday
            (--iterate (ts-dec 'woy 1 it) now 4)
            (--iterate (ts-dec 'month 1 it) now 12)
            (--iterate (ts-dec 'year 1 it) now 50)))))

(defun eva-past-sample-casual (&optional ts)
  "Return a list of ts objects.
They refer to to yesterday, this this day of the month the last 6
months, and this date the last 50 years.  Optionally, the point
of reference can be TS instead of today."
  (let ((now (or ts (ts-now))))
    (-uniq (append
            (--iterate (ts-dec 'day 1 it) now 1)
            (--iterate (ts-dec 'month 1 it) now 6)
            (--iterate (ts-dec 'year 1 it) now 50)))))

;; TODO: Allow a list of datetrees
(defun eva--make-indirect-datetree (buffer dates)
  "Replace BUFFER contents with a datetree of archive entries.
Searches `eva-main-datetree-path' for entries matching members in
DATES (ts objects).  Return the count of dates that were found to
have entries."
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
      (insert-file-contents eva-main-datetree-path)
      (goto-char (point-min))
      (dolist (date dates)
        (when (search-forward (concat "* " (ts-format "%F" date)) nil t)
          (setq counter (1+ counter)) ;; for summary in prompt
          (goto-char (line-beginning-position)) ;;(beginning-of-line)
          (let ((beg (point)))
            (org-next-visible-heading 1)
            (while (< 3 (org-reduced-level (org-outline-level)))
              (org-next-visible-heading 1))
            (append-to-buffer (get-buffer buffer) beg (point))))))
    (if (> counter 0)
        (progn
          (dotimes (_ 2)
            (org-map-region #'org-promote (point-min) (point-max)))
          (org-global-cycle '(4)))
      (kill-buffer buffer))
    counter))

(defun eva--existing-diary (&optional date dir file-format)
  "Return the first file in DIR matching FILE-FORMAT.
FILE-FORMAT is handled by `parse-time-string'.  The value
returned is a full filesystem path or nil.

When DATE is nil, use `eva-date'.  Should be a ts object.
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
                                                (or date eva-date))
                                     it)
                       (directory-files dir))))
    (unless (null file)
      (expand-file-name file dir))))

(eva-defun eva-present-diary ()
  "Show user a selection of past diary entries."
  (let* ((dates-to-check (funcall eva-past-sample-function eva-date))
         (discrete-files-found
          (--keep (eva--existing-diary it) dates-to-check))
         (datetree-buf (get-buffer-create
                        (concat "*" eva-va-name ": Selected diary entries*")))
         (datetree-found-count
          (eva--make-indirect-datetree datetree-buf dates-to-check))
         (total-found-count
          (+ (length discrete-files-found) datetree-found-count)))
    (if (= 0 total-found-count)
        (message (eva-emit "No diary entries relevant to this date."))
      (if (or (when eva-presumptive
                (eva-emit "Opening "
                          (int-to-string total-found-count)
                          " diary entries.")
                t)
              (eva-ynp "Found " (int-to-string total-found-count) " past diary "
                       (if (= 1 total-found-count) "entry" "entries")
                       " relevant to this date. Want me to open "
                       (if (= 1 total-found-count) "it" "them")
                       "?"))
          (progn
            (if (= 0 datetree-found-count)
                (kill-buffer datetree-buf)
              ;; TODO: pressing q should kill it!
              (view-buffer datetree-buf #'kill-buffer)
              (push (current-buffer) eva-excursion-buffers))
            (when (-non-nil discrete-files-found)
              (dolist (x discrete-files-found)
                (view-file x)
                (push (current-buffer) eva-excursion-buffers)))
            (eva-stop-queue))
        (kill-buffer datetree-buf)))))


;;; Org

(add-hook 'eva-before-save-vars-hook
          (defun eva--save-org-variables ()
            "Sync certain Org settings to memory."
            (when (featurep 'org-clock)
              (eva-mem-push 'org-clock-current-task))
            (when (featurep 'org-agenda)
              (eva-mem-push 'org-agenda-files))
            ;; Transform newlines; eva-tsv-append correctly refuses them.
            (when (featurep 'org-capture)
              (let ((transformed-org-templates
                     (cl-loop for template in org-capture-templates
                              collect (--map (if (stringp it)
                                                 (s-replace "\n" "\\n" it)
                                               it)
                                             template))))
                (eva-mem-push-alt transformed-org-templates)))))

(defcustom eva-check-org-vars-load-modules t
  "Whether `eva-check-org-vars' should load certain Org modules.
Note that it still waits for Org init before doing so.  This
ensures that the relevant variables (as of 2021-08-29
`org-agenda-files' and `org-capture-templates') are set to their
final values by the time the VA compares them to its remembered
values."
  :group 'eva
  :type 'boolean)

(defun eva-check-org-vars-1 ()
  "Alert user if certain Org settings have changed.
Hook that runs once and removes itself, since the compiler said
`eval-after-load' is mainly for user init files."
  (remove-hook 'org-mode-hook #'eva-check-org-vars-1)
  (let ((restored-templates
         (cl-loop for template in (map-elt eva-mem 'transformed-org-templates)
                  collect (--map (if (stringp it)
                                     (s-replace "\\n" "\n" it)
                                   it)
                                 template)))
        (restored-agenda-files
         (map-elt eva-mem 'org-agenda-files)))
    (when eva-check-org-vars-load-modules
      (require 'org-agenda)
      (require 'org-capture))
    (when (and (bound-and-true-p org-capture-templates)
               restored-templates)
      (if (equal restored-templates org-capture-templates)
          (eva-dbg "org-capture-templates unchanged")
        (message (eva-emit "org-capture-templates changed!"))))
    (when (and (bound-and-true-p org-agenda-files)
               restored-agenda-files)
      (if (equal restored-agenda-files org-agenda-files)
          (eva-dbg "org-agenda-files unchanged")
        (message (eva-emit "org-agenda-files changed!"))))))

(defun eva-check-org-vars ()
  "Alert user if certain Org settings have changed.
Suitable on `eva-after-load-vars-hook'."
  (add-hook 'org-mode-hook #'eva-check-org-vars-1 91))

(defalias 'eva-check-org-variables #'eva-check-org-vars "Renamed 2021-08-29")

;; (defvar eva--org-vars-checked nil)

;; (defun eva-check-org-variables ()
;;   "Alert user if certain Org settings have changed.
;; Wait until Org loads.  Suitable on `eva-after-load-vars-hook'."
;;   (with-eval-after-load 'org
;;     ;; Hopefully I run only once, but user could have a misconfigured setup
;;     ;; such that `load' is called repeatedly on Org.  Ensure we only run once.
;;     (unless eva--org-vars-checked
;;       (eva--org-vars-check)
;;       (setq eva--org-vars-checked t))))

;; UNTESTED
(defun eva-check-dangling-clock ()
  "If there's a dangling clock, prompt to load Org.
Suitable on `eva-after-load-vars-hook'."
  (and (map-elt eva-mem 'org-clock-current-task)
       (eva-ynp "Dangling clock found, activate Org?")
       (require 'org-clock)))

(provide 'eva-builtin)

;; Local Variables:
;; nameless-current-name: "eva"
;; End:

;;; eva-builtin.el ends here
