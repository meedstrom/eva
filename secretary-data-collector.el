;;; sc-data-collector.el --- description -*- lexical-binding: t; -*-

(require 'sc-lib)

(defun sc-log-idle ()
  (sc-append "/home/kept/Self_data/idle.tsv"
             (ts-format)
             "\t" (number-to-string sc-length-of-last-idle-in-minutes)))

;; TODO: Batch save. Instead of appending to a file line by line, append to a
;; buffer and save it once every 5 minutes or so.
;; TODO: When buffer mode changes, count it as a new buffer. Note that (assoc
;; buf sc-known-buffers) will still work.
(defun sc-log-buffer (&optional _arg)
  (unless (minibufferp)
    (let* ((buf (current-buffer))
           (mode (symbol-name (sc-buffer-mode buf)))
           (known (assoc buf sc-known-buffers))
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
      (unless (eq sc-last-buffer buf) ;; happens if you only entered and left minibuffer
        (setq sc-last-buffer buf)
        (unless known
          (push buffer-record sc-known-buffers)
	  (sc-append "/home/kept/Self_data/buffers.tsv"
		     (string-join (cdr buffer-record) "\t")))
        (push focus-record sc-buffer-focus-log)
        (sc-append "/home/kept/Self_data/buffer-focus.tsv"
                   (string-join focus-record "\t"))))))

;; (defun sc-known-buffers-buffer ()
;;   (or (get-file-buffer "/home/kept/Self_data/buffers.csv")
;;       (create-file-buffer "/home/kept/Self_data/buffers.csv")))

;; (defun sc-buffer-focus-log-buffer ()
;;   (or (get-file-buffer "/home/kept/Self_data/buffer-focus.csv")
;;       (create-file-buffer "/home/kept/Self_data/buffer-focus.csv")))



;; (setc completion-auto-help t)
;; (completing-read (concat "Score from 1 to 5: [" "]") nil nil nil nil nil (cdr (assoc "fine" sc-mood-alist)))
;; (read-string (concat "Score from 1 to 5: [" "]")  nil nil (cdr (assoc "fine" sc-mood-alist)))

;; TODO: make the dataset append-only
;;;###autoload
(defun sc-query-ingredients ()
  (interactive)
  (let* ((response (read-string "Comma-separated list of ingredients: "))
         (formatted-response (->> response
                                  (s-split (rx (+ (any "," blank))))
                                  (s-join ", ")
                                  (s-replace "\"" "'")))
         (path "/home/kept/Self_data/ingredients.tsv"))
    (sc-append path
	       (ts-format "%F")
	       "\t" "\"" formatted-response "\"")
    (sc-emit "Recorded so far today: "
             (s-replace "^.*?," ""
			(sc-get-first-today-in-date-indexed-csv path)))))

;; (defalias #'sc-query-food #'sc-query-ingredients)

(defun sc--sanity-check-csv ()
  ;; look for overlapping time intervals
  (pcsv-parse-file "/home/kept/Self_data/idle.csv")

  (pcsv-parse-file "/home/kept/Self_data/idle.csv")

  )

;;;###autoload
(defun sc-query-activity ()
  (interactive)
  (let* ((name (completing-read "What are you up to? " (sc-activities-names)))
	 (now (ts-now)))
    (sc-append "/home/kept/Self_data/activity.tsv"
               (ts-format now)
               "\t" name
               "\t" (sc-activity-id (sc-activity-by-name name)))))
;; (sc-query-activity)

(setq sc-activities
      (list (sc-activity-create
	     :name "sleep"
	     :id "ac93c132-ab74-455f-a456-71d7b5ee88a6"
	     :cost-false-pos 3
	     :cost-false-neg 3
	     :querier #'sc-query-sleep)
	    (sc-activity-create
	     :name "studying"
	     :id "24553859-2214-4fb0-bdc9-84e7f3d04b2b"
	     :cost-false-pos 8
	     :cost-false-neg 8)
	    ))

;;;###autoload
(defun sc-query-mood (&optional prompt)
  (interactive)
  (let* ((mood-desc (completing-read (or prompt "Your mood: ") (mapcar #'car sc-mood-alist)))
         (old-score (cdr (assoc mood-desc sc-mood-alist)))
         (prompt-for-score
          (concat "Score from 1 to 5"
                  (when old-score " (default " old-score ")")
                  ":"))
         (score (read-string prompt-for-score nil nil old-score))
         (score-num (string-to-number score))
         (now (ts-now)))
    (sc-append "/home/kept/Self_data/mood.tsv"
               (ts-format now)
               "\t" (s-replace "," "." score)
               "\t" mood-desc)
    ;; Update sc-mood-alist.
    (if (assoc mood-desc sc-mood-alist)
        (setq sc-mood-alist
              (--replace-where (string= (car it) mood-desc)
                               (cons (car it) score)
                               sc-mood-alist))
      (push (cons mood-desc score) sc-mood-alist))
    ;; Return the mood score, which can be useful for the caller of this
    ;; function. If the input was not a number, like "idk" or an empty string,
    ;; return 3 to be neutral.
    (if (= 0 score-num)
        3
      score-num)))

;;;###autoload
(defun sc-query-weight ()
  (interactive)
  (let* ((path "/home/kept/Self_data/weight.tsv")
         (last-wt (sc-last-value-in-tsv path))
         (wt (completing-read "What do you weigh today? "
			      `(,last-wt
				"I don't know")))
         (now (ts-now)))
    (if (= 0 (string-to-number wt))
        (sc-emit "Ok, I'll ask you again later.")
      (sc-append path
                 (ts-format now)
                 "\t" (s-replace "," "." wt))
      (sc-emit "Weight today: " (sc-last-value-in-tsv path) " kg"))
    (sit-for sc-sit-short)))

;; (defun sc-query-weight ()
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
;;           (sc-emit "Ok, I'll ask you later.")
;;         (f-append (concat newline-maybe (ts-format "%F") "," wt) 'utf-8 "/home/kept/Self_data/weight.csv")))))

(defun sc-check-yesterday-sleep ()
  (let* ((foo (sc-get-all-today-in-date-indexed-csv
	       "/home/kept/Self_data/sleep.tsv" (ts-dec 'day 1 (ts-now))))
         (total-yesterday (-sum (--map (string-to-number (nth 2 it)) foo))))
    (if (> (* 60 4) total-yesterday)
        (if (sc-prompt "Yesterday, you slept "
		       (number-to-string (/ total-yesterday 60.0))
                       " hours, is this about right?")
            nil
          (sc-emit "You may edit the history at "
		   "/home/kept/Self_data/sleep.tsv")
          (sit-for sc-sit-short)))))

;; TODO: Fix the case where someone wakes up at 23:00 but replies to the query
;;       at 01:00.
;;;###autoload
(defun sc-query-sleep ()
  "Query you for wake-up time and sleep quantity for one sleep block today.
You are free to decline either query, but you should not later
register sleep quantity from this same block in order to \"get
the totals correct\" -- the program will interpret it as a
different sleep block and continue to count the original one as
having an unknown nonzero quantity of sleep on top of what you
add."
  (interactive)
  (sc-check-yesterday-sleep)
  (let* ((now (ts-now))
         (wakeup-time
          (if (sc-prompt "Did you wake around now?")
              (ts-dec 'minute 10 now)
            (let ((reply (completing-read "When did you wake? "
                                          `("I don't know"
					    "Now"
					    ,(ts-format "%H:%M")))))
              (when (-non-nil (parse-time-string reply))
                (ts-parse reply)))))
         (sleep-minutes
	  (sc-parse-time-amount
	   (completing-read "How long did you sleep? "
                            '("I don't know"
			      (number-to-string
			       (/ sc-length-of-last-idle-in-minutes 60)))))))

    (sc-emit (when wakeup-time
	       (concat "You woke at " (ts-format "%H:%M" wakeup-time) ". "))
             (when sleep-minutes
	       (concat "You slept " (number-to-string sleep-minutes)
		       " minutes (" (number-to-string (/ sleep-minutes 60.0))
		       " hours).")))
    (sc-append "/home/kept/Self_data/sleep.tsv"
               (ts-format now)
	       "\t" (ts-format "%F" sc--date)
               "\t" (when wakeup-time (ts-format "%T" wakeup-time))
               "\t" (number-to-string sleep-minutes))))

(defun sc-query-meditation (&optional date)
  (interactive)
  (when (sc-prompt "Did you meditate today?")
    (let ((x (read-string "Do you know how long (in minutes)? ")))
      (sc-append "/home/kept/Self_data/meditation.tsv"
		 (ts-format date)
		 "\t" "TRUE"
		 "\t" x))))

(defun sc-query-cold-shower (&optional date)
  (interactive)
  (let ((x (read-string "Cold rating? "))
	(path "/home/kept/Self_data/cold.tsv"))
    (sc-append path
               (ts-format date)
	       "\t" x)))

(provide 'sc-data-collector)
;;; sc-data-collector.el ends here
