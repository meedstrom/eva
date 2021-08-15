;;; secretary-builtin.el -*- lexical-binding: t; nameless-current-name: "secretary"; -*-

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

(require 'secretary)

;;;###autoload
(secretary-defquery secretary-query-meditation ()
  (when (secretary-ynp "Did you meditate today?")
    (let* ((mins (read-string "Do you know how long (in minutes)? "))
           (cleaned-mins (number-to-string (string-to-number mins)))) ;; ugly, I know
      (secretary-append-tsv current-dataset
        (ts-format)
        "TRUE"
        (unless (string= "0" cleaned-mins) cleaned-mins)))))

;;;###autoload
(secretary-defquery secretary-query-cold-shower ()
  (let ((rating (read-string "Cold rating? ")))
    (secretary-append-tsv current-dataset
      (ts-format secretary--date)
      rating)))

;;;###autoload
(secretary-defquery-and-excursion secretary-present-org-agenda ()
  (require 'org-agenda)
  (message (secretary-emit "Sending you to the Org agenda log + archive."))
  (sit-for secretary-sit-medium)
  (org-agenda-list)
  (org-agenda-log-mode)
  (org-agenda-archives-mode t)
  (push (current-buffer) secretary--excursion-buffers)
  (keyboard-quit))

;;;###autoload
(secretary-defquery secretary-query-ingredients ()
  (pop-to-buffer (secretary-buffer-chat))
  (let* ((response (secretary-read-string "What ingredients did you eat recently? ")))
    (secretary-append-tsv current-dataset
      (ts-format secretary--date)
      response)
    (secretary-emit "Ingredients recorded today: "
                    (->> (nreverse (secretary--get-entries-in-tsv current-dataset))
                         (-map #'-last-item)
                         (s-join ", ")
                         (s-replace ",," ",")))))

(defvar secretary--mood-alist nil
  "For suggesting a score in the `secretary-query-mood' prompt.
Merely a convenience for auto-completion. The variable populates
itself through use.")

;;;###autoload
(secretary-defquery secretary-query-mood ()
  (let* ((mood-desc (secretary-read
                     "Your mood: "
                     (sort (mapcar #'car secretary--mood-alist)
                           #'secretary--random-p)))
         (old-score (cdr (assoc mood-desc secretary--mood-alist)))
         (prompt-for-score
          (concat "Score from 1 to 5"
                  (when old-score " (default " old-score ")")
                  ": "))
         (score (progn (secretary-emit "Score from 1 to 5: ")
                       (read-string prompt-for-score nil nil old-score)))
         (score-num (string-to-number score)))
    (secretary-append-tsv current-dataset
      (ts-format)
      (s-replace "," "." score)
      mood-desc)
    (secretary-emit-same-line (nth 2 (secretary--last-in-tsv current-dataset)))
    ;; Update secretary--mood-alist.
    (if (assoc mood-desc secretary--mood-alist)
        (setq secretary--mood-alist
              (--replace-where (string= (car it) mood-desc)
                               (cons (car it) score)
                               secretary--mood-alist))
      (push (cons mood-desc score) secretary--mood-alist))
    score-num))

(add-hook 'secretary-after-load-vars-hook
          (defun secretary--mood-load ()
            (setq secretary--mood-alist
                  (map-elt secretary-memory 'secretary--mood-alist))))

(add-hook 'secretary-before-save-vars-hook
          (defun secretary--mood-save ()
            (secretary-memory-pushnew 'secretary--mood-alist)))

;;;###autoload
(secretary-defquery secretary-query-weight ()
  (let* ((last-wt (secretary-last-value-in-tsv current-dataset))
         (wt (secretary-read "What do you weigh today? "
                             `(,last-wt
                               "I don't know")
                             last-wt)))
    (if (= 0 (string-to-number wt)) ;; user typed a string with characters other than num and whitespace
        (secretary-emit "Ok, I'll ask you again later.")
      (secretary-append-tsv current-dataset
        (ts-format secretary--date)
        (s-replace "," "." wt))
      (secretary-emit "Weight today: "
                      (secretary-last-value-in-tsv current-dataset)
                      " kg"))))

;; TODO: pass start-date (today minus 3mo) and projection incline, letting
;; user change the incline (persist for future plots toeo)
;; TODO: Persist the buffer content across restarts
;; TODO: pass the weight log file name instead of hardcoding in the R script
;;;###autoload
(defun secretary-plot-weight ()
  (interactive)
  (setq secretary--current-fn #'secretary-plot-weight-ascii)
  ;; Refresh data with R.
  (with-current-buffer secretary-buffer-r
    (ess-execute "source(\"make_data_for_plots.R\")" 'buffer))
  ;; Plot with gnuplot.
  (let* ((pkg-loc (convert-standard-filename (f-dirname (find-library-name "secretary"))))
         (gnuplot-script-path (convert-standard-filename (expand-file-name "weight.gnuplot" pkg-loc))))
    (with-current-buffer (get-buffer-create
                          (concat "*" (symbol-name secretary--current-fn) "*"))
      (let ((reserve (buffer-string)))
        (delete-region (point-min) (point-max))
        (message (secretary-emit "Plotting your weight..."))
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

;; TODO: This is both a query and excursion, uses another query's dataset, and
;; the dialogue wording could benefit from merging with the other query ("Now
;; let's talk about today"). Is merging the best way to go about it? Is there a
;; way to define a combined query-and-excursion that fits in with our concepts,
;; or a better set of concepts that will cover use cases like this?
(secretary-defquery secretary-check-yesterday-sleep ()
  (let* ((dataset (secretary-item-dataset (secretary--item-by-fn #'secretary-query-sleep)))
         (today-rows (secretary--get-entries-in-tsv dataset (ts-dec 'day 1 secretary--date)))
         (total-yesterday (-sum (--map (string-to-number (nth 3 it)) today-rows))))
    ;; Totalling less than 4 hours is unusual, implying a possible anomaly in data.
    (if (> (* 60 4) total-yesterday)
        (if (secretary-ynp "Yesterday, you slept "
                           (number-to-string (round (/ total-yesterday 60.0)))
                           " hours, is this about right?")
            nil
          (when (secretary-ynp "Edit " dataset "?")
            (find-file dataset)
            (keyboard-quit))))))

;; Old one.
;; TODO: make it only ask once
(defun secretary-check-yesterday-sleep ()
  (let* ((dataset (secretary-item-dataset (secretary--item-by-fn secretary--current-fn)))
         (today-rows (secretary--get-entries-in-tsv dataset (ts-dec 'day 1 secretary--date)))
         (total-yesterday (-sum (--map (string-to-number (nth 3 it)) today-rows))))
    ;; Totalling less than 4 hours is unusual, implying a possible anomaly in data.
    (if (> (* 60 4) total-yesterday)
        (if (secretary-ynp "Yesterday, you slept "
                           (number-to-string (round (/ total-yesterday 60.0)))
                           " hours, is this about right?")
            nil
          (secretary-emit "You may edit the history at "
                          dataset
                          ". For now, let's talk about today.")))))

;; TODO: (Feature) Look at when idle ended to suggest a response.
;; TODO: (Feature) Let user say "since 5" instead of quantity-art
;; TODO: Fix the case where someone wakes up at 23:00 but replies to the query
;;       at 01:00. Notice the unusual hour change and ask if user meant 23
;;       yesterday.
;;;###autoload
(secretary-defquery secretary-query-sleep ()
  "Query you for wake-up time and sleep quantity for one sleep block today.
You are free to decline either query, but you should not later
register sleep quantity from this same block in order to \"get
the totals correct\" -- the database will interpret it as a
different sleep block and continue to count the original one as
having a censored (nonzero!) quantity of sleep on top of what you
add."
  (secretary-check-yesterday-sleep)
  (let* ((recently-hhmm (ts-format "%H:%M" (ts-dec 'minute 10 secretary--date)))
         (recently-response (concat "Recently (" recently-hhmm ")"))
         (wakeup-time
          (let* ((reply (secretary-read "I assume you have slept. When did you wake? "
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
    (secretary-append-tsv current-dataset
      (ts-format "%F" secretary--date) ;; date (no time component)
      wakeup-time ;; time (optional)
      (when sleep-minutes (number-to-string sleep-minutes)))))


;;; Ledger & finances

(defcustom secretary-ledger-file-name
  (convert-standard-filename "~/my.ledger")
  "File used by `secretary-present-ledger-report'."
  :group 'secretary
  :type 'file)

;;;###autoload
(secretary-defquery-and-excursion secretary-present-ledger-report ()
  "Jump to `secretary-ledger-file-name' and run `ledger-report'.
Uses the first command specified in `ledger-reports'."
  (cond ((not (f-exists-p secretary-ledger-file-name))
         (message (secretary-emit
                   "secretary-ledger-file-name does not refer to existing file,"
                   " skipping Ledger report.")))
         ((not (require 'ledger-mode nil t))
          (message (secretary-emit
                    "Ledger-mode failed to load, skipping Ledger report.")))
         (t
          (message (secretary-emit "Sending you to your Ledger report."))
          (if (get-buffer ledger-report-buffer-name)
              (ledger-report-goto)
            (with-current-buffer (find-file-noselect secretary-ledger-file-name)
              (ledger-report (caar ledger-reports) nil)))
          (push (get-buffer ledger-report-buffer-name) secretary--excursion-buffers)
          (keyboard-quit))))

(secretary-defquery-and-excursion secretary-present-ledger-report ()
  "Jump to `secretary-ledger-file-name' and run `ledger-report'.
Uses the first command specified in `ledger-reports'."
  (unless (f-exists-p secretary-ledger-file-name)
    (error "secretary-ledger-file-name does not refer to existing file."))
  (when (ignore-errors (find-library-name "ledger-mode"))
    (require 'ledger-mode)
    (message (secretary-emit "Sending you to your Ledger report."))
    (if (get-buffer ledger-report-buffer-name)
        (progn
          (ledger-report-goto)
          (push (current-buffer) secretary--excursion-buffers))
      (with-current-buffer (find-file-noselect secretary-ledger-file-name)
        (ledger-report (caar ledger-reports) nil)
        (push (get-buffer ledger-report-buffer-name) secretary--excursion-buffers)))
    (keyboard-quit)))

;;;###autoload
(secretary-defexcursion secretary-present-ledger-file ()
  (message (secretary-emit "Sending you to your Ledger file. Sayonara!"))
  (sit-for secretary-sit-medium)
  ;; (pop-to-buffer (find-file-noselect secretary-ledger-file-name))
  ;; (view-mode)
  (view-file-other-window secretary-ledger-file-name) ;; testing this form of the above
  (push (current-buffer) secretary--excursion-buffers)
  (goto-char (point-max)))

(defun secretary-make-ods-for-finance ()
  "Make and open an ODS spreadsheet from Ledger data.
Requires the ssconvert program that comes with Gnumeric."
  (interactive)
  (let* ((script (expand-file-name "generate_an_ods.R"
                                   (f-dirname (find-library-name "secretary"))))
         (sheet (expand-file-name "tmp_finances.ods"
                                  (temporary-file-directory)))
         (default-directory (f-dirname script))
         (app (seq-find #'executable-find '("gnumeric"
                                            "soffice"
                                            "open"
                                            "mimeopen"
                                            "xdg-open"))))
    (if (= 0 (call-process "Rscript" nil nil nil script secretary-ledger-file-name sheet))
        (pfuture-new app sheet)
      (message (secretary-emit "Error running " script)))))


;;; Diary

(defcustom secretary-location-main-datetree
  "~/org/archive.org"
  "The file name of your main datetree, if you have one.
Only relevant if you have one you use as a big archive file, see
Info node `(org) Moving subtrees', or you write/capture
diary entries directly into.  Checked by `secretary-present-diary'."
  :group 'secretary
  :type 'file)

(defvar secretary-past-sample-function #'secretary-past-sample-greedy)

(defun secretary-past-sample-greedy (&optional ts)
  "Return a list of ts objects.
They refer to yesterday, this weekday the last 4 weeks, this day
of the month the last 12 months, and this date the last 50
years."
  (let ((now (or ts (ts-now))))
    (-uniq (append
            (--iterate (ts-dec 'day 1 it) now 1) ;; yesterday
            (--iterate (ts-dec 'woy 1 it) now 4)
            (--iterate (ts-dec 'month 1 it) now 12)
            (--iterate (ts-dec 'year 1 it) now 50)))))

(defun secretary-past-sample-casual (&optional ts)
  "Return a list of ts objects.
They refer to to yesterday, this this day of the month the last 6
months, and this date the last 50 years."
  (let ((now (or ts (ts-now))))
    (-uniq (append
            (--iterate (ts-dec 'day 1 it) now 1)
            (--iterate (ts-dec 'month 1 it) now 6)
            (--iterate (ts-dec 'year 1 it) now 50)))))

;; TODO: Allow a list of datetrees
(defun secretary-make-indirect-datetree (buffer dates)
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
        (progn
          (dotimes (_ 2)
            (org-map-region #'org-promote (point-min) (point-max)))
          (org-global-cycle '(4)))
      (kill-buffer buffer))
    counter))
;; (secretary-make-indirect-datetree (get-buffer-create "test")

(defun secretary-existing-diary (&optional date dir file-format)
  "Return the first file in DIR matching FILE-FORMAT.
FILE-FORMAT is handled by `parse-time-string'. The value returned
is a full filesystem path or nil.

When DATE is nil, use `secretary--date'.  Should be a ts object.
When DIR is nil, use `org-journal-dir'.
When FILE-FORMAT is nil, use `org-journal-file-format'; if that's
 unset, use \"%F.org\".

Note that org-journal is not needed."
  (let* ((dir (or dir
                  (bound-and-true-p org-journal-dir)))
         (file-format (or file-format
                          (and (boundp 'org-journal-file-type)
                               (boundp 'org-journal-file-format) ;; silence the compiler
                               (eq org-journal-file-type 'daily)
                               org-journal-file-format)
                          "%F.org"))
         (file (--find (s-contains-p (ts-format file-format
                                                (or date secretary--date))
                                     it)
                       (directory-files dir))))
    (unless (null file)
      (expand-file-name file dir))))
;; (secretary-existing-diary (ts-dec 'day 2 (ts-now)))
;; (secretary-existing-diary (ts-now) "/home/kept/Diary" )
;; (--keep (secretary-existing-diary it "/home/kept/Diary") (funcall secretary-past-sample-function))

;; FIXME: defquery with keyboard-quit does not write to the alt-dataset, so
;; :max-successes-per-day is never invoked
;;
;; TODO: (Feature) Make separate buffers for each datetree-based entry (use
;;                 rename-buffer?) and interleave with discrete files so it's
;;                 all in chrono order.
;; TODO: (Feature) Try creating a sparse tree, so user can edit in-place
;; TODO: (Feature) Maybe show the agenda log taken from each date?
;;;###autoload
(secretary-defquery-and-excursion secretary-present-diary ()
  (let* ((dates-to-check (funcall secretary-past-sample-function secretary--date))
         (discrete-files-found (--keep (secretary-existing-diary it) dates-to-check))
         (buffer (get-buffer-create (concat "*" secretary-ai-name ": Selected diary entries*")))
         (datetree-found-count (secretary-make-indirect-datetree buffer dates-to-check))
         (total-found-count (+ (length discrete-files-found) datetree-found-count)))
    (if (= 0 total-found-count)
        (message (secretary-emit "No diary entries relevant to this date."))
      (when (or (when secretary-presumptive-p
                  (secretary-emit "Opening " (int-to-string total-found-count) " diary entries.")
                  t)
                (secretary-ynp "Found " (int-to-string total-found-count) " past diary "
                               (if (= 1 total-found-count) "entry" "entries")
                               " relevant to this date. Want me to open "
                               (if (= 1 total-found-count) "it" "them")
                               "?"))
        (if (= 0 datetree-found-count)
            (kill-buffer buffer)
          ;; TODO: pressing q should kill it!
          (view-buffer buffer #'kill-buffer-if-not-modified)
          (push (current-buffer) secretary--excursion-buffers))
        (when (-non-nil discrete-files-found)
          (dolist (x discrete-files-found)
            (view-file x)
            (push (current-buffer) secretary--excursion-buffers)))
        (keyboard-quit)))))

;; (secretary-present-diary (ts-now))
;; (secretary-present-diary (ts-dec 'day 1 (ts-now)))


;;; Org
;; TODO: Expand this stuff.

(add-hook 'secretary-before-save-vars-hook
          (defun secretary--save-org-variables ()
            (when (featurep 'org-clock)
              (secretary-memory-pushnew 'org-clock-current-task))
            (when (featurep 'org-agenda)
              (secretary-memory-pushnew 'org-agenda-files))
            ;; Transform newlines; secretary-append-tsv correctly refuses them.
            (let ((transformed-org-templates
                   (cl-loop for template in org-capture-templates
                            collect (--map (if (stringp it)
                                               (s-replace "\n" "\\n" it)
                                             it)
                                           template))))
              (secretary-memory-pushnew-alt transformed-org-templates))))

;; UNTESTED
(defun secretary--check-org-variables ()
  "Check for changes to certain variables.
Suitable on `secretary-after-load-vars-hook'."
  (let ((restored-templates
         (cl-loop for template in (map-elt secretary-memory 'transformed-org-templates)
                  collect (--map (if (stringp it)
                                     (s-replace "\\n" "\n" it)
                                   it)
                                 template))))
    (when secretary-debug-p
      (if (equal restored-templates org-capture-templates)
          (message (secretary-emit "org-capture-templates unchanged"))
        (message (secretary-emit "org-capture-templates changed!")))
      (if (equal org-agenda-files (map-elt secretary-memory 'org-agenda-files))
          (message (secretary-emit "org-agenda-files unchanged"))
        (message (secretary-emit "org-agenda-files changed!"))))))

;; UNTESTED
(defun secretary--check-clock ()
  "If there's a dangling clock, prompt to load Org.
Suitable on `secretary-after-load-vars-hook'."
  (and (map-elt secretary-memory 'org-clock-current-task)
       (secretary-ynp "Dangling clock found, activate Org?")
       (require 'org-clock)))

(provide 'secretary-builtin)

;;; secretary-builtin.el ends here
