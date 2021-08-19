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

;; Premade queries, hooks and other uses of the core library. I dogfood these,
;; you can use them as-is or define your own alternatives.

;;; Code:

(require 'eva)

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

(eva-wrap eva-greet ()
  (message (eva-emit (eva-greeting)))
  (sit-for eva-sit-long))

(eva-wrap eva-query-meditation ()
  "Ask user whether they meditated and how long."
  (when (eva-ynp "Did you meditate today?")
    (let* ((mins (eva-read-string "Do you know how long (in minutes)? "))
           (cleaned-mins (number-to-string (string-to-number mins)))) ; HACK
      (eva-tsv-append eva-curr-dataset
        (ts-format)
        "TRUE"
        (unless (string= "0" cleaned-mins) cleaned-mins)))))

(eva-wrap eva-query-cold-shower ()
  "Ask user to rate their cold exposure today."
  (let ((rating (eva-read-string "Cold rating? ")))
    (eva-tsv-append eva-curr-dataset
      (ts-format eva-date)
      rating)))

(eva-wrap eva-query-ingredients ()
  "Ask user for a description of what they ate."
  (let* ((response (eva-read-string
                    "What ingredients did you eat recently? ")))
    (eva-tsv-append eva-curr-dataset
      (ts-format eva-date)
      response)
    (eva-emit "Ingredients recorded today: "
              (->> (eva-tsv-entries-by-date eva-curr-dataset)
                   (nreverse)
                   (-map #'-last-item)
                   (s-join ", ")
                   (s-replace ",," ",")))))

(eva-wrap eva-present-org-agenda ()
  "Send the user to an Org agenda log with archives enabled.
Near equivalent to typing l v A after entering `org-agenda-list'."
  (require 'org-agenda)
  (message (eva-emit "Here's the agenda archive as of now."))
  ;; (message (eva-emit "Sending you to the Org agenda log + archive."))
  (sit-for eva-sit-short)
  (org-agenda-list)
  (org-agenda-log-mode t)
  (org-agenda-archives-mode t)
  (push (current-buffer) eva-excursion-buffers)
  (keyboard-quit)
  (keyboard-quit))


;;; Mood

(defvar eva-mood-alist nil
  "For suggesting a score in the `eva-query-mood' prompt.
Merely a convenience for auto-completion. The variable populates
itself through use.")

(eva-wrap eva-query-mood ()
  "Ask user about their mood."
  (let* ((first-response (eva-read
                          "Your mood: "
                          (--sort (> 0 (random))
                                  (map-keys eva-mood-alist))))
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
    (when (and (<= score-num 1)
               (eva-ynp "Do you want to talk?")
               (eva-ynp "I can direct you to my colleague ELIZA, though"
                        " she's not too bright.  Will that do?"))
      (doctor)
      (eva-stop-watching-excursion)
      (keyboard-quit))
    score-num))

(add-hook 'eva-after-load-vars-hook
          (defun eva-mood-load ()
            "Reload `eva-mood-alist' from memory."
            (setq eva-mood-alist
                  (map-elt eva-mem 'eva-mood-alist))))

(add-hook 'eva-before-save-vars-hook
          (defun eva-mood-save ()
            "Save `eva-mood-alist' to memory."
            (eva-mem-pushnew 'eva-mood-alist)))


;;; Weight

(eva-wrap eva-query-weight ()
  "Ask user about their weight."
  (let* ((last-wt (eva-tsv-last-value eva-curr-dataset))
         (wt (eva-read "What do you weigh today? "
                       `(,last-wt
                         "I don't know")
                       last-wt)))
    (if (= 0 (string-to-number wt))
        ;; user typed only non-numeric characters
        (eva-emit "Ok, I'll ask you again later.")
      (eva-tsv-append eva-curr-dataset
        (ts-format eva-date)
        (s-replace "," "." wt))
      (eva-emit "Weight today: "
                (eva-tsv-last-value eva-curr-dataset)
                " kg"))))

;; TODO: Pass start-date (today minus 3mo) and projection incline, letting
;;       user change the incline
;; TODO: Persist the buffer content across restarts
(defun eva-plot-weight ()
  "Present a plot of the user's weight."
  (interactive)
  (setq eva-curr-fn #'eva-plot-weight)
  (mkdir "/tmp/eva" t)
  ;; Refresh data with R.
  (let ;; FIXME: Allow custom item fns, and allow empty `eva-items'
      ((weight-path (eva-item-dataset (eva-item-by-fn #'eva-query-weight)))
       (mood-path (eva-item-dataset (eva-item-by-fn #'eva-query-mood))))
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
(eva-wrap eva-query-sleep ()
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
            ;; prevent counting this run as a success
            (eva-stop-watching-excursion)
            (keyboard-quit)))))
  (let* ((recently-hhmm (ts-format "%H:%M" (ts-dec 'minute 10 eva-date)))
         (recently-response (concat "Recently (" recently-hhmm ")"))
         (wakeup-time
          (let* ((reply (eva-read
                         (concat "I assume you have slept today ("
                                 (ts-format "%A" eva-date)
                                 "). When did you wake? ")
                         (list "I don't know"
                               recently-response))))
            (cond ((equal reply recently-response)
                   recently-hhmm)
                  ((s-match (rx num) reply)
                   (eva-coerce-to-hh-mm reply))
                  (t nil))))
         (sleep-minutes
          (eva-parse-time-amount
           (eva-read "How long did you sleep? "
                     `("I don't know"
                       ,(number-to-string
                         (/ eva-length-of-last-idle 60 60)))))))
    (eva-emit (when wakeup-time
                (concat "You woke at " wakeup-time ". "))
              (when sleep-minutes
                (concat "You slept " (number-to-string sleep-minutes)
                        " minutes (" (number-to-string
                                      (/ sleep-minutes 60.0))
                        " hours)."))
              (when (-all-p #'null '(wakeup-time sleep-minutes))
                (concat "One sleep block recorded without metrics.")))
    (eva-tsv-append eva-curr-dataset
      (ts-format "%F" eva-date) ;; date (no time component)
      wakeup-time ;; time (optional)
      (when sleep-minutes (number-to-string sleep-minutes)))))


;;; Ledger & finances

(defcustom eva-main-ledger-path
  (convert-standard-filename "~/my.ledger")
  "File used by `eva-present-ledger-report'."
  :group 'eva
  :type 'file)

(eva-wrap eva-present-ledger-report ()
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
         (keyboard-quit))))

(eva-wrap eva-present-ledger-file ()
  (unless (f-exists-p eva-main-ledger-path)
    (warn "not found: eva-main-ledger-path"))
  (message (eva-emit "Here's your ledger.  Please, edit."))
  (sit-for eva-sit-medium)
  (view-file-other-window eva-main-ledger-path)
  (push (current-buffer) eva-excursion-buffers)
  (unless save-place-mode
    (goto-char (point-max)))
  (keyboard-quit))

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
        (pfuture-new app sheet)
      (message (eva-emit "Error running " script)))))


;;; Diary
(defcustom eva-main-datetree-path
  "~/org/archive.org"
  "The file name of your main datetree, if you have one.
Only relevant if you have one you use as a big archive file, see
Info node `(org) Moving subtrees', or you write/capture
diary entries directly into.  Checked by `eva-present-diary'."
  :group 'eva
  :type 'file)

(defvar eva-past-sample-function #'eva-past-sample-greedy)

(defun eva-past-sample-greedy (&optional ts)
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

(defun eva-past-sample-casual (&optional ts)
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
(defun eva--make-indirect-datetree (buffer dates)
  "Replace BUFFER contents with a datetree of archive entries.
Searches `eva-main-datetree-path' for entries matching
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
      (insert-file-contents eva-main-datetree-path)
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

(defun eva--existing-diary (&optional date dir file-format)
  "Return the first file in DIR matching FILE-FORMAT.
FILE-FORMAT is handled by `parse-time-string'. The value returned
is a full filesystem path or nil.

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

;; TODO: (Feature) Make separate buffers for each datetree-based entry (use
;;                 rename-buffer?) and interleave with discrete files so it's
;;                 all in chrono order.
;; TODO: (Feature) Try creating a sparse tree, so user can edit in-place
;; TODO: (Feature) Maybe show the agenda log taken from each date?
(eva-wrap eva-present-diary ()
  "Show user a selection of past diary entries."
  (let* ((dates-to-check (funcall eva-past-sample-function eva-date))
         (discrete-files-found
          (--keep (eva--existing-diary it) dates-to-check))
         (buffer (get-buffer-create
                  (concat "*" eva-ai-name ": Selected diary entries*")))
         (datetree-found-count
          (eva--make-indirect-datetree buffer dates-to-check))
         (total-found-count
          (+ (length discrete-files-found) datetree-found-count)))
    (if (= 0 total-found-count)
        (message (eva-emit "No diary entries relevant to this date."))
      (when (or (when eva-presumptive
                  (eva-emit "Opening "
                            (int-to-string total-found-count)
                            " diary entries.")
                  t)
                (eva-ynp "Found " (int-to-string total-found-count) " past diary "
                         (if (= 1 total-found-count) "entry" "entries")
                         " relevant to this date. Want me to open "
                         (if (= 1 total-found-count) "it" "them")
                         "?"))
        (if (= 0 datetree-found-count)
            (kill-buffer buffer)
          ;; TODO: pressing q should kill it!
          (view-buffer buffer #'kill-buffer)
          (push (current-buffer) eva-excursion-buffers))
        (when (-non-nil discrete-files-found)
          (dolist (x discrete-files-found)
            (view-file x)
            (push (current-buffer) eva-excursion-buffers)))
        (keyboard-quit)))))


;;; Org

(add-hook 'eva-before-save-vars-hook
          (defun eva--save-org-variables ()
            "Sync certain org settings to mem."
            (when (featurep 'org-clock)
              (eva-mem-pushnew 'org-clock-current-task))
            (when (featurep 'org-agenda)
              (eva-mem-pushnew 'org-agenda-files))
            ;; Transform newlines; eva-tsv-append correctly refuses them.
            (when (featurep 'org-capture)
              (let ((transformed-org-templates
                     (cl-loop for template in org-capture-templates
                              collect (--map (if (stringp it)
                                                 (s-replace "\n" "\\n" it)
                                               it)
                                             template))))
                (eva-mem-pushnew-alt transformed-org-templates)))))

(defvar eva--org-vars-checked nil)

(defun eva-check-org-variables ()
  "Alert user if certain Org settings have changed.
Suitable on `eva-after-load-vars-hook'."
  (let ((restored-templates
         (cl-loop for template in (map-elt eva-mem 'transformed-org-templates)
                  collect (--map (if (stringp it)
                                     (s-replace "\\n" "\n" it)
                                   it)
                                 template)))
        (restored-agenda-files
         (map-elt eva-mem 'org-agenda-files)))
    (when eva-debug ; TODO: remove this condition, but ensure it's not annoying
      (when restored-templates
        (if (equal restored-templates org-capture-templates)
            (message (eva-emit "org-capture-templates unchanged."))
          (message (eva-emit "org-capture-templates changed!"))))
      (when restored-agenda-files
        (if (equal restored-agenda-files org-agenda-files)
            (message (eva-emit "org-agenda-files unchanged."))
          (message (eva-emit "org-agenda-files changed!")))))))

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
