;;; sc-data-collector.el --- description -*- lexical-binding: t; -*-


;;;; Loggers (that work in the background without input, unlike queriers)

(defun sc-log-idle ()
  (sc-append* "/home/kept/Self_data/idle.csv"
              (number-to-string (ts-unix (ts-now))) "," (number-to-string sc-length-of-last-idle)))

;; TODO: Instead of appending to a file, append to a buffer and save it only once every 5 minutes or so.
;; TODO: When buffer mode changes, count it as a new buffer (assoc will still work).
(defun sc-log-buffer (_arg)
  (unless (minibufferp)
    (let* ((buf (current-buffer))
           (mode (symbol-name (sc-buffer-mode buf)))
           (known (assoc buf sc-known-buffers))
           (timestamp (number-to-string (ts-unix (ts-now))))
           (buffer-observation (unless (and known
                                            (string= mode (nth 4 known))) ;; doesnt do it
                                 (list buf
                                       (org-id-uuid)
                                       (buffer-name buf)
                                       (buffer-file-name buf)
                                       mode
                                       timestamp ;; time the buffer was first opened
                                       )))
           (focus-observation (list timestamp ;; time the buffer was switched to
                                    (if known (cadr known) (cadr buffer-observation)) ;; uuid
                                    )))
      (unless (eq sc-last-buffer buf) ;; happens if you only entered and left minibuffer
        (setq sc-last-buffer buf)
        (unless known
          (push buffer-observation sc-known-buffers)
          ;; (with-current-buffer (sc-known-buffers-buffer)
          ;;   (insert (concat "\n" (string-join (cdr buffer-observation) ","))))
          ;; (when-let (foo (get-file-buffer "/home/kept/Self_data/buffers.csv"))
            ;; (kill-buffer foo))
          (f-append (concat "\n" (string-join (cdr buffer-observation) ","))
                    'utf-8
                    "/home/kept/Self_data/buffers.csv"))
        (push focus-observation sc-buffer-focus-log)
        ;; (with-current-buffer (sc-buffer-focus-log-buffer)
        ;;  (insert (concat "\n" (string-join focus-observation ","))))
        ;; (when-let (foo (get-file-buffer "/home/kept/Self_data/buffer-focus.csv"))
          ;; (kill-buffer foo))
        (f-append (concat "\n" (string-join focus-observation ","))
                  'utf-8
                  "/home/kept/Self_data/buffer-focus.csv"))
      )))

;; (defun sc-known-buffers-buffer ()
;;   (or (get-file-buffer "/home/kept/Self_data/buffers.csv")
;;       (create-file-buffer "/home/kept/Self_data/buffers.csv")))

;; (defun sc-buffer-focus-log-buffer ()
;;   (or (get-file-buffer "/home/kept/Self_data/buffer-focus.csv")
;;       (create-file-buffer "/home/kept/Self_data/buffer-focus.csv")))



;; (setc completion-auto-help t)
;; (completing-read (concat "Score from 1 to 5: [" "]") nil nil nil nil nil (cdr (assoc "fine" sc-mood-alist)))
;; (read-string (concat "Score from 1 to 5: [" "]")  nil nil (cdr (assoc "fine" sc-mood-alist)))

;;;###autoload
(defun sc-query-ingredients ()
  (interactive)
  (let* ((response (read-string "Comma-separated list of ingredients: "))
         (formatted-response (->> response
                                  (s-split (rx (+ (any "," blank))))
                                  (s-join ", ")
                                  (s-replace "\"" "'")))
         (path "/home/kept/Self_data/ingredients.csv"))
    (sc-append* path (ts-format "%F") ",\"" formatted-response "\"")
    (sc-emit "Recorded so far today: "
             (replace-regexp-in-string "^.*?," "" (sc-get-first-today-in-date-indexed-csv path)))))

;; (defalias #'sc-query-food #'sc-query-ingredients)

;;;###autoload
(defun sc-query-mood (&optional prompt)
  (interactive)
  (let* ((str (completing-read (or prompt "Your mood: ") (mapcar #'car sc-mood-alist)))
         (default (cdr (assoc str sc-mood-alist)))
         (score (read-string (concat "Score from 1 to 5 (default " default "):") nil nil default))
         (score-num (string-to-number score)))
    (sc-append* "/home/kept/Self_data/mood.csv"
                (ts-format "%F") "," str "," (replace-regexp-in-string "," "." score))
    ;; Update sc-mood-alist
    (if (assoc str sc-mood-alist)
        (setq sc-mood-alist
              (--replace-where (string= (car it) str)
                               (cons (car it) score)
                               sc-mood-alist))
      (push (cons str score) sc-mood-alist))
    ;; Return 3 if the input was not a number, like "idk" or blank string. Not
    ;; saved to disk, just useful for the caller.
    (if (= 0 score-num)
        3
      score-num)))

;;;###autoload
(defun sc-query-weight ()
  (interactive)
  (let* ((path "/home/kept/Self_data/weight.csv")
         (last-wt (with-temp-buffer
                    (insert-file-contents-literally path)
                    (goto-char (point-max))
                    (search-backward ",")
                    (forward-char)
                    (buffer-substring (point) (line-end-position))))
         (wt (completing-read "What do you weigh today? " `(,last-wt "I don't know"))))
    (if (= 0 (string-to-number wt))
        (sc-emit "Ok, I'll ask you again later.")
      (sc-append* path
                  (ts-format "%F") "," (replace-regexp-in-string "," "." wt))
      (sc-emit "Weight today: "
               (replace-regexp-in-string "^.*?," "" (sc-get-first-today-in-date-indexed-csv path))
               " kg"))
    (sit-for .5)))

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
  (let* ((foo (sc-get-all-today-in-date-indexed-csv "/home/kept/Self_data/sleep.csv" (ts-dec 'day 1 (ts-now))))
         (total-yesterday (-sum (--map (string-to-number (nth 2 it)) foo))))
    (if (> (* 60 4) total-yesterday)
        (if (sc-prompt "Yesterday, you slept " (number-to-string (/ total-yesterday 60.0))
                       " hours, is this about right?")
            nil
          (sc-emit "You may edit the history at " "/home/kept/Self_data/sleep.csv")
          (sit-for .5)))))

;; TODO: Fix the case where someone wakes up at 23:00 but replies to the query
;; at 01:00
;;;###autoload
(defun sc-query-sleep ()
  "Query you for wake-up time and sleep quantity for one sleep block today.
You are free to decline either query, but do not later inform us
of sleep quantity from this same block in order to \"get the
totals correct\" -- the program will interpret it as a different
sleep block and continue to count the original one as having an
unknown nonzero quantity of sleep on top of what you add."
  (interactive)
  (sc-check-yesterday-sleep)
  (let* ((waketime (if (sc-prompt "Did you wake around now?")
                       (ts-dec 'minute 10 (ts-now))
                     (let ((reply (completing-read "When did you wake? "
                                                   `("I don't know" "Now" ,(ts-format "%H:%M")) ;; not shown
                                                   nil nil (ts-format "%H:%M"))))
                       (when (-non-nil (parse-time-string reply))
                         (ts-parse reply)))))
         (sleepamount (sc-parse-time-amount
                       (completing-read "How long did you sleep? " '("I don't know")
                                        nil nil (number-to-string (/ sc-length-of-last-idle 60))))))

    (sc-emit (when waketime (concat "You woke at " (ts-format "%H:%M" waketime) ". "))
             (when sleepamount (concat "You slept " (number-to-string sleepamount) " minutes ("
                                       (number-to-string (/ sleepamount 60.0)) " hours).")))
    (sc-append* "/home/kept/Self_data/sleep.csv"
                (ts-format "%F")
                "," (when waketime (ts-format "%T" waketime))
                "," (number-to-string sleepamount))))

(defun sc-query-meditation (&optional date)
  (interactive)
  (when (sc-prompt "Did you meditate today?")
    (sc-append* "/home/kept/Self_data/meditation.csv"
                (ts-format "%F" date) ",TRUE")))

(defun sc-query-cold-shower (&optional date)
  (interactive)
  (let ((x (read-string "Cold rating? ")))
    (sc-append* "/home/kept/Self_data/cold.csv"
                (ts-format "%F" date) "," x)))

(provide 'sc-data-collector)
;;; sc-data-collector.el ends here
