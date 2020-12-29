;;; sc-mothball.el --- unused stuff -*- lexical-binding: t; -*-

(require 'sc-lib)

(defun sc-timestamp-for-chat ()
  (concat "<" (ts-format "%H:%M") "> "))

(defun sc-human-readable-ts (&optional ts)
  (declare (side-effect-free t))
  (ts-format "%Y-%b-%d %H:%M" ts))

(defun sc-unix-ts (&optional ts)
  "Return an Unix timestamp at time TS, with zero-padded
decimals, as a string, typically to be written to a file."
  (declare (side-effect-free t))
  (s-pad-right 18 "0" (number-to-string (ts-unix (or ts (ts-now))))))

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

(provide 'sc-mothball)
;;; sc-mothball.el ends here
