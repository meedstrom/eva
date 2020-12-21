;;; sc-presentations.el --- description -*- lexical-binding: t; -*-

(add-hook
 'sc-plot-hook
 (defun my-custom-plot ()
   (call-process "R" nil "Rscript" "/home/kept/Journal/Finances/some-plot.R")
   (insert-image-file "")
   (sc-emit "And that's the plot of your finances, " sc-usr-short-title ".")
   ))

;; wip
(defun sc-present-plots ()
  (switch-to-buffer (sc-chat-buffer))
  (sc-emit (seq-random-elt '("I have plotted the latest intel, boss."
                             "Here are some projections!"
                             "Data is beautiful, don't you think?")))
  (sit-for 1)
  (sc-plot)
  (run-hooks 'sc-plot-hook))

(defun sc-plot ()
  (start-process "R (Secretary.el)" nil "Rscript" (expand-file-name "daily_plot.R"))
  (insert-image-file "daily_plot.png"))


;; TODO: Also collect entries from separate files
(defun sc-indirect-datetree (dates)
  (require 'org)
  (let ((dates (-sort 'ts<= dates)))
    (switch-to-buffer (get-buffer-create "*Secretary: Selected diary entries*"))
    (org-mode)
    (delete-region (point-min) (point-max))
    (save-mark-and-excursion
      (find-file "/home/kept/Journal/diary2.org")
      (org-with-wide-buffer
       (goto-char (point-min))
       (dolist (date dates)
         (when (search-forward (concat "* " (ts-format "%F" date)) nil t)
           (goto-char (line-beginning-position)) ;;(beginning-of-line)
           (let ((beg (point)))
             (org-next-visible-heading 1)
             (while (< 3 (org-reduced-level (org-outline-level)))
               (org-next-visible-heading 1))
             (append-to-buffer (get-buffer "*Secretary: Selected diary entries*") beg (point)))))))
    (switch-to-buffer (get-buffer "*Secretary: Selected diary entries*"))
    (dotimes (_ 2)
      (org-map-region #'org-promote (point-min) (point-max)))
    (goto-char (point-min))
    (view-mode)))
;;(sc-indirect-datetree (--iterate (ts-dec 'year 1 it) (ts-now) 10))

(defun sc-make-indirect-datetree (buffer dates)
  (require 'org)
  (let ((dates (-sort 'ts<= dates))
        (counter 0))
    (switch-to-buffer (get-buffer-create buffer))
    (org-mode)
    (delete-region (point-min) (point-max))
    (save-mark-and-excursion
      (find-file "/home/kept/Journal/diary2.org")
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
    (dotimes (_ 2)
      (org-map-region #'org-promote (point-min) (point-max)))
    counter))

;; TODO: Ok, use candidates as an index, use `--map-indexed' for each subtype of
;;       thing to review, so the results can be sorted chronologically
;; TODO: use rename-buffer to make separate buffers for each datetree entry (instead of a single indirect datetree)
;;;###autoload
(defun sc-present-old-diary ()
  (interactive)
  (let* ((buffer "*Secretary: Selected diary entries*")
         (candidates (append (--iterate (ts-dec 'month 1 it) (ts-now) 11)
                             (--iterate (ts-dec 'year 1 it) (ts-now) 10)))
         (discrete-files-found (--keep (sc-existing-diary "/home/kept/Diary" it) candidates))
         (datetree-found-count (sc-make-indirect-datetree buffer candidates))
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
;; (foo)

;; (sc-indirect-datetree (list (ts-now)
;;                             (ts-dec 'year 1 (ts-now))
;;                             (ts-dec 'year 2 (ts-now))
;;                             (ts-dec 'year 3 (ts-now))
;;                             (ts-dec 'day 3 (ts-dec 'year 3 (ts-now)))
;;                             (ts-dec 'year 4 (ts-now))
;;                             (ts-parse "2017-12-13")
;;                             ))

;; TODO: open multiple agenda buffers ??
(defun foobar ()
  (require 'org-agenda)
  (let ((dates (--> dates
                    (-sort #'ts>=)
                    (--map (ts-format "%F" it)))))
    (dolist (x dates)
      (org-agenda-list nil x 1)
      (rename-buffer (concat "Agenda " x))
      )))
;; (foobar)

(provide 'sc-presentations)
;;; sc-presentations.el ends here
