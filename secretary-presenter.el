;;; sc-presenter.el --- description -*- lexical-binding: t; -*-
(require 'sc-lib)

;;;###autoload
(defun sc-plot-mood ()
  (interactive)
  (let* ((default-directory (f-dirname (find-library-name "secretary")))
         (script (expand-file-name "sc_daily_plot.R"))
         (plot (expand-file-name "sc_mood.png" "/tmp/secretary")))
    (sc-emit "Plotting mood...")
    (mkdir "/tmp/secretary" t)
    (set-process-sentinel
     (start-process sc-ai-name nil "Rscript" script "14" "-0.1")
     `(lambda (_process _event)
        (sc-emit "And that's your mood." "\n")
        (switch-to-buffer (sc-buffer-chat))
        (read-only-mode 0)
        (insert-image-file ,plot)
        (goto-char (point-max))
        (insert "\n")
        (read-only-mode)
        ))))

;;;###autoload
(defun sc-plot-weight ()
  (let* ((default-directory (f-dirname (find-library-name "secretary")))
         (script (expand-file-name "sc_daily_plot.R"))
         (plot (expand-file-name "sc_plot1.png" "/tmp/secretary")))
    (sc-emit "Plotting weight...")
    (mkdir "/tmp/secretary" t)
    (set-process-sentinel
     (start-process sc-ai-name nil "Rscript" script "14" "-0.1")
     `(lambda (_process _event)
        (sc-emit "And here's your weight, boss." "\n")
        (switch-to-buffer (sc-buffer-chat))
        (read-only-mode 0)
        (insert-image-file ,plot)
        ;; (delete-file ,plot)
        (goto-char (point-max))
        (insert "\n")
        (read-only-mode)
        ))))

;;;###autoload
(defun sc-present-plots ()
  (interactive)
  (unless (null sc-plot-hook)
    (switch-to-buffer (sc-buffer-chat))
    (sc-emit (seq-random-elt '("I have plotted the latest intel, boss."
                               "Here are some projections!"
                               "Data is beautiful, don't you think?")))
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

;; TODO: use rename-buffer to make separate buffers for each datetree entry
;;;###autoload
(defun sc-present-diary (date)
  (interactive)
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

(provide 'sc-presenter)

;;; sc-presenter.el ends here
