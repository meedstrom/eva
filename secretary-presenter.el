;;; secretary-presenter.el -*- lexical-binding: t; -*-
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

;;;###autoload
(defun scr-report-mood ()
  (interactive)
  (view-file "/home/kept/Self_data/mood.tsv")
  (auto-revert-mode))

;;;###autoload
(defun scr-plot-mood ()
  (interactive)
  (let* ((default-directory "/tmp/secretary")
	 (pkg-dir (f-dirname (find-library-name "secretary")))
         (script (expand-file-name "R/sc_daily_plot.R" pkg-dir))
         (plot (expand-file-name "sc_mood.png" default-directory)))
    (scr-emit "Plotting mood...")
    (mkdir "/tmp/secretary" t)
    (set-process-sentinel
     (start-process scr-ai-name nil "Rscript" script "14" "-0.1")
     `(lambda (_process _event)
        (scr-emit "And that's your mood." "\n")
        (switch-to-buffer (scr-buffer-chat))
        (read-only-mode 0)
        (insert-image-file ,plot)
        (goto-char (point-max))
        (insert "\n")
        (read-only-mode)
        ))))

;;;###autoload
(defun scr-plot-weight ()
  (let* ((default-directory "/tmp/secretary")
         (script (expand-file-name "R/sc_daily_plot.R"
				   (f-dirname (find-library-name "secretary"))))
         (plot (expand-file-name "sc_plot1.png" "/tmp/secretary")))
    (scr-emit "Plotting weight...")
    (mkdir "/tmp/secretary" t)
    (set-process-sentinel
     (start-process scr-ai-name nil "Rscript" script "14" "-0.1")
     `(lambda (_process _event)
        (scr-emit "And here's your weight, boss." "\n")
        (switch-to-buffer (scr-buffer-chat))
        (read-only-mode 0)
        (insert-image-file ,plot)
        ;; (delete-file ,plot)
        (goto-char (point-max))
        (insert "\n")
        (read-only-mode)
        ))))

;;;###autoload
(defun scr-present-plots ()
  (interactive)
  (unless (null scr-plot-hook)
    (switch-to-buffer (scr-buffer-chat))
    (scr-emit (seq-random-elt '("I have plotted the latest intel, boss."
                               "Here are some projections!"
                               "Data is beautiful, don't you think?")))
    (run-hooks 'scr-plot-hook)))

;; Should I use such variables or encourage the user to make defuns?
(defcustom scr-look-back-years 99 nil)
(defcustom scr-look-back-months 12 nil)
(defcustom scr-look-back-weeks 4 nil)
(defcustom scr-look-back-days 1 nil)

(defvar scr-past-sample-function #'scr-past-sample-default)

(defun scr-past-sample-default ()
  (let ((now (ts-now)))
    (-uniq (append
	    ;; Yesterday, this weekday the last 4 weeks, this day of the month the
	    ;; last 12 months, and this date from every year in the past.
            (--iterate (ts-dec 'day 1 it) now 1)
            (--iterate (ts-dec 'woy 1 it) now 4)
            (--iterate (ts-dec 'month 1 it) now 12)
            (--iterate (ts-dec 'year 1 it) now 99)))))

(ert-deftest scr-test-ts-usage ()
  (let ((now (ts-now)))
    (should (equal (ts-dec 'month 12 now)
                   (ts-dec 'year 1 now)))
    (should (= 16 (length (-uniq (append
                                  (--iterate (ts-dec 'month 1 it) now 12)
                                  (--iterate (ts-dec 'year 1 it) now 5))))))))

(defun scr-make-indirect-datetree (buffer dates)
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
;; (scr-make-indirect-datetree (get-buffer-create "test") (--iterate (ts-dec 'month 1 it) (ts-now) 40))

;; TODO: use rename-buffer to make separate buffers for each datetree entry
;;;###autoload
(defun scr-present-diary (date)
  (interactive)
  (let* ((buffer (get-buffer-create (concat "*" scr-ai-name ": Selected diary entries*")))
         (dates-to-check (funcall scr-past-sample-function))
         (discrete-files-found (--keep (scr-existing-diary "/home/kept/Diary" it) dates-to-check))
         (datetree-found-count (scr-make-indirect-datetree buffer dates-to-check))
         (total-found-count (+ (length discrete-files-found) datetree-found-count)))
    (if (scr-prompt "Found " (int-to-string total-found-count) " past diary "
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
;; (scr-present-diary (ts-now))

(provide 'secretary-presenter)

;;; secretary-presenter.el ends here
