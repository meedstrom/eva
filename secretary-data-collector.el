;;; secretary-data-collector.el -*- lexical-binding: t; -*-
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

;;; Code:

(require 'secretary-common)

(defvar scr-last-buffer nil)

(defvar scr-known-buffers nil)

(defvar scr-buffer-focus-log nil)

(defun scr-log-idle ()
  (scr-append "/home/kept/Self_data/idle.tsv"
             (ts-format)
             "\t" (number-to-string (/ (round scr-length-of-last-idle) 60))))

;; TODO: Batch save. Instead of appending to a file line by line, append to a
;; buffer and save it once every 5 minutes or so.
;; TODO: When buffer mode changes, count it as a new buffer. Note that (assoc
;; buf scr-known-buffers) will still work.
(defun scr-log-buffer (&optional _arg)
  (unless (minibufferp)
    (let* ((buf (current-buffer))
           (mode (symbol-name (scr-buffer-mode buf)))
           (known (assoc buf scr-known-buffers))
           (timestamp (number-to-string (ts-unix (ts-now))))
           (buffer-record (unless (and known
                                       (string= mode (nth 4 known))) ;; doesnt do it
                            (list buf
                                  (org-id-uuid)
                                  (buffer-name buf)
                                  (buffer-file-name buf)
                                  mode
                                  timestamp ;; time the buffer was first opened
                                  )))
           (focus-record (list timestamp ;; time the buffer was switched to
                               (if known (cadr known) (cadr buffer-record)) ;; uuid
                               )))
      (unless (eq scr-last-buffer buf) ;; happens if you only entered and left minibuffer
        (setq scr-last-buffer buf)
        (unless known
          (push buffer-record scr-known-buffers)
	  (scr-append "/home/kept/Self_data/buffers.tsv"
		     (string-join (cdr buffer-record) "\t")))
        (push focus-record scr-buffer-focus-log)
        (scr-append "/home/kept/Self_data/buffer-focus.tsv"
                   (string-join focus-record "\t"))))))

;; (defun scr-known-buffers-buffer ()
;;   (or (get-file-buffer "/home/kept/Self_data/buffers.csv")
;;       (create-file-buffer "/home/kept/Self_data/buffers.csv")))

;; (defun scr-buffer-focus-log-buffer ()
;;   (or (get-file-buffer "/home/kept/Self_data/buffer-focus.csv")
;;       (create-file-buffer "/home/kept/Self_data/buffer-focus.csv")))



;; (setc completion-auto-help t)
;; (completing-read (concat "Score from 1 to 5: [" "]") nil nil nil nil nil (cdr (assoc "fine" scr-mood-alist)))
;; (read-string (concat "Score from 1 to 5: [" "]")  nil nil (cdr (assoc "fine" scr-mood-alist)))

;; TODO: make the dataset append-only
;;;###autoload
(defun scr-query-ingredients ()
  (interactive)
  (let* ((response (read-string "Comma-separated list of ingredients: "))
         (formatted-response (->> response
                                  (s-split (rx (+ (any "," blank))))
                                  (s-join ", ")
                                  (s-replace "\"" "'")))
         (path "/home/kept/Self_data/ingredients.tsv"))
    (scr-append path
	       (ts-format "%F")
	       "\t" "\"" formatted-response "\"")
    (scr-emit "Recorded so far today: "
             (s-replace "^.*?," ""
			(scr-get-first-today-in-date-indexed-csv path)))))

;; (defalias #'scr-query-food #'scr-query-ingredients)

(defun scr--sanity-check-csv ()
  ;; look for overlapping time intervals
  (pcsv-parse-file "/home/kept/Self_data/idle.csv")

  (pcsv-parse-file "/home/kept/Self_data/idle.csv")

  )

;;;###autoload
(defun scr-query-activity ()
  (interactive)
  (let* ((name (completing-read "What are you up to? " (scr-activities-names)))
	 (now (ts-now)))
    (scr-append "/home/kept/Self_data/activity.tsv"
               (ts-format now)
               "\t" name
               "\t" (scr-activity-id (scr-activity-by-name name)))))
;; (scr-query-activity)

(setq scr-activities
      (list (scr-activity-create
	     :name "sleep"
	     :id "ac93c132-ab74-455f-a456-71d7b5ee88a6"
	     :cost-false-pos 3
	     :cost-false-neg 3
	     :querier #'scr-query-sleep)
	    (scr-activity-create
	     :name "studying"
	     :id "24553859-2214-4fb0-bdc9-84e7f3d04b2b"
	     :cost-false-pos 8
	     :cost-false-neg 8)
	    ))

;;;###autoload
(defun scr-query-mood (&optional prompt)
  (interactive)
  (let* ((mood-desc (completing-read (or prompt "Your mood: ")
				     (--sort (> 0 (random))
					     (mapcar #'car scr-mood-alist))))
         (old-score (cdr (assoc mood-desc scr-mood-alist)))
         (prompt-for-score
          (concat "Score from 1 to 5"
                  (when old-score " (default " old-score ")")
                  ":"))
         (score (read-string prompt-for-score nil nil old-score))
         (score-num (string-to-number score))
         (now (ts-now)))
    (scr-append "/home/kept/Self_data/mood.tsv"
               (ts-format now)
               "\t" (s-replace "," "." score)
               "\t" mood-desc)
    ;; Update scr-mood-alist.
    (if (assoc mood-desc scr-mood-alist)
        (setq scr-mood-alist
              (--replace-where (string= (car it) mood-desc)
                               (cons (car it) score)
                               scr-mood-alist))
      (push (cons mood-desc score) scr-mood-alist))
    ;; Return the mood score, which can be useful for the caller of this
    ;; function. If the input was not a number, like "idk" or an empty string,
    ;; return 3 to be neutral.
    (if (= 0 score-num)
        3
      score-num)))

;;;###autoload
(defun scr-query-weight ()
  (interactive)
  (let* ((path "/home/kept/Self_data/weight.tsv")
         (last-wt (scr-last-value-in-tsv path))
         (wt (completing-read "What do you weigh today? "
			      `(,last-wt
				"I don't know")))
         (now (ts-now)))
    (if (= 0 (string-to-number wt))
        (scr-emit "Ok, I'll ask you again later.")
      (scr-append path
                 (ts-format now)
                 "\t" (s-replace "," "." wt))
      (scr-emit "Weight today: " (scr-last-value-in-tsv path) " kg"))
    (sit-for scr-sit-short)))

;; (defun scr-query-weight ()
;;   (interactive)
;;   (with-temp-buffer
;;     (let* ((require-final-newline nil)
;;            (last-wt (progn (insert-file-contents "/home/kept/Self_data/weight.csv")
;;                            (goto-char (point-max))
;;                            (search-backward ",")
;;                            (forward-char)
;;                            (buffer-substring (point) (line-end-position))))
;;            (wt (completing-read "What do you weigh today? " nil nil nil last-wt))
;;            (newline-maybe (if (string= "\n" (buffer-substring (- (point-max) 1) (point-max)))
;;                               ""
;;                             "\n")))
;;       (if (= 0 (string-to-number wt))
;;           (scr-emit "Ok, I'll ask you later.")
;;         (f-append (concat newline-maybe (ts-format "%F") "," wt) 'utf-8 "/home/kept/Self_data/weight.csv")))))

(defun scr-check-yesterday-sleep ()
  (let* ((foo (scr-get-all-today-in-date-indexed-csv
	       "/home/kept/Self_data/sleep.tsv" (ts-dec 'day 1 (ts-now))))
         (total-yesterday (-sum (--map (string-to-number (nth 2 it)) foo))))
    (if (> (* 60 4) total-yesterday)
        (if (scr-prompt "Yesterday, you slept "
		       (number-to-string (/ total-yesterday 60.0))
                       " hours, is this about right?")
            nil
          (scr-emit "You may edit the history at "
		   "/home/kept/Self_data/sleep.tsv")
          (sit-for scr-sit-short)))))

;; TODO: Fix the case where someone wakes up at 23:00 but replies to the query
;;       at 01:00.
;;;###autoload
(defun scr-query-sleep ()
  "Query you for wake-up time and sleep quantity for one sleep block today.
You are free to decline either query, but you should not later
register sleep quantity from this same block in order to \"get
the totals correct\" -- the program will interpret it as a
different sleep block and continue to count the original one as
having an unknown nonzero quantity of sleep on top of what you
add."
  (interactive)
  (scr-check-yesterday-sleep)
  (let* ((now (ts-now))
         (wakeup-time
          (if (scr-prompt "Did you wake around now?")
              (ts-dec 'minute 10 now)
            (let ((reply (completing-read "When did you wake? "
                                          `("I don't know"
					    "Now"
					    ,(ts-format "%H:%M")))))
              (when (-non-nil (parse-time-string reply))
                (ts-parse reply)))))
         (sleep-minutes
	  (scr-parse-time-amount
	   (completing-read "How long did you sleep? "
                            `("I don't know"
			      ,(number-to-string
				(/ scr-length-of-last-idle 60 60)))))))

    (scr-emit (when wakeup-time
	       (concat "You woke at " (ts-format "%H:%M" wakeup-time) ". "))
             (when sleep-minutes
	       (concat "You slept " (number-to-string sleep-minutes)
		       " minutes (" (number-to-string (/ sleep-minutes 60.0))
		       " hours).")))
    (scr-append "/home/kept/Self_data/sleep.tsv"
               (ts-format now)
	       "\t" (ts-format "%F" scr--date)
               "\t" (when wakeup-time (ts-format "%T" wakeup-time))
               "\t" (number-to-string sleep-minutes))))

(defun scr-query-meditation (&optional date)
  (interactive)
  (when (scr-prompt "Did you meditate today?")
    (let ((x (read-string "Do you know how long (in minutes)? ")))
      (scr-append "/home/kept/Self_data/meditation.tsv"
		 (ts-format date)
		 "\t" "TRUE"
		 "\t" x))))

(defun scr-query-cold-shower (&optional date)
  (interactive)
  (let ((x (read-string "Cold rating? "))
	(path "/home/kept/Self_data/cold.tsv"))
    (scr-append path
               (ts-format date)
	       "\t" x)))

(provide 'secretary-data-collector)

;;; secretary-data-collector.el ends here
