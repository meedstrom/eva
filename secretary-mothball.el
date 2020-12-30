;;; secretary-mothball.el -*- lexical-binding: t; -*-
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

(defun scr-timestamp-for-chat ()
  (concat "<" (ts-format "%H:%M") "> "))

(defun scr-human-readable-ts (&optional ts)
  (declare (side-effect-free t))
  (ts-format "%Y-%b-%d %H:%M" ts))

(defun scr-unix-ts (&optional ts)
  "Return an Unix timestamp at time TS, with zero-padded
decimals, as a string, typically to be written to a file."
  (declare (side-effect-free t))
  (s-pad-right 18 "0" (number-to-string (ts-unix (or ts (ts-now))))))

;; TODO: Ok, use candidates as an index, use `--map-indexed' for each subtype of
;;       thing to review, so the results can be sorted chronologically
(defun scr-present-diary-as-datetree ()
  "Unused"
  (interactive)
  (let* ((buffer (get-buffer-create (concat "*" scr-ai-name ": Selected diary entries*")))
         (dates-to-check (scr-past-sample-default))
         (discrete-files-found (--keep (scr-existing-diary "/home/kept/Diary" it) dates-to-check))
         (datetree-found-count (scr-make-indirect-datetree buffer dates-to-check))
         (total-found-count (+ (length discrete-files-found) datetree-found-count)))
    (if (and (< 0 total-found-count)
             (scr-prompt (concat "Found " (int-to-string total-found-count) " diary "
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

(provide 'secretary-mothball)
;;; secretary-mothball.el ends here
