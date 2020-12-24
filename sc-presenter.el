;;; sc-presenter.el --- description -*- lexical-binding: t; -*-

;; WIP
(defun sc-plot-finances ()
  (set-process-sentinel
     (start-process sc-ai-name nil "Rscript" script "14")
     `(lambda (_process _event)
        (sc-emit "And here's your weight, boss." "\n")
        (switch-to-buffer (sc-chat-buffer))
        (read-only-mode 0)
        (insert-image-file ,plot)
        ;; (delete-file ,plot)
        (goto-char (point-max))
        (insert "\n")
        (read-only-mode)
        ))
  (call-process sc-ai-name nil "Rscript" "/home/kept/Journal/Finances/some-plot.R")
  (insert-image-file "")
  (sc-emit "And that's the plot of your finances, " sc-usr-short-title "."))

(defun sc-plot-weight ()
  (let* ((default-directory (f-dirname (find-library-name "secretary")))
         (script (expand-file-name "sc_daily_plot.R"))
         (plot (expand-file-name "plot1.png")))
    (sc-emit "Plotting weight...")
    (set-process-sentinel
     (start-process sc-ai-name nil "Rscript" script "14")
     `(lambda (_process _event)
        (sc-emit "And here's your weight, boss." "\n")
        (switch-to-buffer (sc-chat-buffer))
        (read-only-mode 0)
        (insert-image-file ,plot)
        ;; (delete-file ,plot)
        (goto-char (point-max))
        (insert "\n")
        (read-only-mode)
        ))))

(defun sc-present-plots ()
  (unless (null sc-plot-hook)
    (switch-to-buffer (sc-chat-buffer))
    (sc-emit (seq-random-elt '("I have plotted the latest intel, boss."
                               "Here are some projections!"
                               "Data is beautiful, don't you think?")))
    (sit-for 1)
    (run-hooks 'sc-plot-hook)))

(defun sc-make-indirect-datetree (buffer dates)
  (require 'org)
  (let ((dates (-sort 'ts<= dates))
        (counter 0))
    (switch-to-buffer buffer)
    (org-mode)
    (delete-region (point-min) (point-max))
    (with-temp-buffer
      (insert-file-contents  "/home/kept/Journal/diary2.org")
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
;; (sc-make-indirect-datetree (get-buffer-create "test") (--iterate (ts-dec 'month 1 it) (ts-now) 40))

;; Should I use such variables or encourage the user to make defuns?
(defcustom sc-look-back-years 99 nil)
(defcustom sc-look-back-months 12 nil)
(defcustom sc-look-back-weeks 4 nil)
(defcustom sc-look-back-days 1 nil)


(defvar sc-past-sample-function #'sc-past-sample-default)
(defun sc-past-sample-default ()
  (let ((now (ts-now)))
    (-uniq (append
            (--iterate (ts-dec 'day 1 it) now 1)
            (--iterate (ts-dec 'woy 1 it) now 4)
            (--iterate (ts-dec 'month 1 it) now 12)
            (--iterate (ts-dec 'year 1 it) now 99)))))

(ert-deftest sc-test-ts-works ()
  (let ((now (ts-now)))
    (should (equal (ts-dec 'month 12 now)
                   (ts-dec 'year 1 now)))
    (should (= 16 (length (-uniq (append
                                  (--iterate (ts-dec 'month 1 it) now 12)
                                  (--iterate (ts-dec 'year 1 it) now 5))))))))

;; TODO: Ok, use candidates as an index, use `--map-indexed' for each subtype of
;;       thing to review, so the results can be sorted chronologically
(defun sc-present-diary-as-datetree ()
  "Unused"
  (interactive)
  (let* ((buffer (get-buffer-create (concat "*" sc-ai-name ": Selected diary entries*")))
         (dates-to-check (sc-past-sample-default))
         (discrete-files-found (--keep (sc-existing-diary "/home/kept/Diary" it) dates-to-check))
         (datetree-found-count (sc-make-indirect-datetree buffer dates-to-check))
         (total-found-count (+ (length discrete-files-found) datetree-found-count)))
    (if (and (< 0 total-found-count)
             (sc-prompt (concat "Found " (int-to-string total-found-count) " diary "
                                (if (= 1 total-found-count) "entry" "entries")
                                " for this date from the past. Want me to open "
                                (if (= 1 total-found-count) "it" "them")
                                "?")))
        (progn
          (switch-to-buffer (get-buffer buffer))
          (dolist (f discrete-files-found)
            (goto-char (point-max))
            ;; org-paste-subtree may be better
            (insert "\n* " f "\n")
            (insert-file-contents f))
          (view-mode)
          (read-only-mode)
          (goto-char (point-min)))
      (kill-buffer buffer))))

;; TODO: use rename-buffer to make separate buffers for each datetree entry
(defun sc-present-diary (date)
  (let* ((buffer (get-buffer-create (concat "*" sc-ai-name ": Selected diary entries*")))
         (dates-to-check (funcall sc-past-sample-function))
         (discrete-files-found (--keep (sc-existing-diary "/home/kept/Diary" it) dates-to-check))
         (datetree-found-count (sc-make-indirect-datetree buffer dates-to-check))
         (total-found-count (+ (length discrete-files-found) datetree-found-count)))
    (if (sc-prompt "Found " (int-to-string total-found-count) " past diary "
                    (if (= 1 total-found-count) "entry" "entries")
                    " relevant to this date. Want me to open "
                    (if (= 1 total-found-count) "it" "them")
                    "?")
        (progn
          (switch-to-buffer buffer)
          (view-mode)
          (if (-non-nil discrete-files-found)
              (dolist (x discrete-files-found)
                (view-file x))))
      (kill-buffer buffer))))
;; (sc-present-diary (ts-now))

;; TODO: open multiple agenda buffers ??
(defun foobar ()
  (require 'org-agenda)
  (let ((dates (--> dates
                    (-sort #'ts>=)
                    (-map (lambda (x) (ts-format "%F" x))))))
    (dolist (x dates)
      (org-agenda-list nil x 1)
      (rename-buffer (concat "Agenda " x))
      )))
;; (foobar)

(provide 'sc-presenter)

;;; sc-presenter.el ends here
