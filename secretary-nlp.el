;;; sc-nlp.el -*- lexical-binding: t; -*-
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

(require 'sc-lib)

;; TODO: Catch typos like 03 meaning 30 minutes, not 3 hours
(defun sc-parse-time-amount (input)
  (let ((numeric-part (string-to-number input)))
    (cond ((= 0 numeric-part) ;; strings without any number result in 0
           nil) ;; save as a NA observation
          ((and (string-match-p "h.*m" input) (> numeric-part 0))
           (warn "I'm not sophisticated enough to parse that"))
          ((string-match-p "h" input)
           (* 60 numeric-part))
          ((string-match-p "m" input)
           numeric-part)
          ((-> numeric-part (>= 20))
           numeric-part)
          (t
           (* 60 numeric-part)))))
;; (sc-parse-time-amount "30")

;; (define-key minibuffer-local-completion-map (kbd "C-o") #'sc-special-handle-current-query)

(defun sc-special-handle-current-query ()
  (interactive)
  (let ((input (completing-read "Yes? " '(
                                    "Never mind, let me reply normally."
                                    "Remind me about this one later."
                                    "Skip this."
                                    "Back to previous query."
                                    "Yes."
                                    "No."
                                    "Apply this query to a different date."
                                    "Apply this query to yesterday."
                                    "Tell me something profound."
                                    ))))
    (cond ((string-match-p (rx (or "remind" "later" "l8r" "resched")) input)
           'reschedule)
          ((string-match-p (rx (or "food" "ingred")) input)
           (sc-query-ingredients))
          ((string-match-p (rx (or "profound")) input)
           (sc-emit (seq-random-elt sc-aphorisms)))
          ((string-match-p (rx (or "yesterday" "yday")) input)
           (sc-change-date-yesterday))
          ((string-match-p (rx "date") input)
           (sc-change-date))
          ((string-match-p (rx (or "nvm" "never mind" (seq bow "nm" eow))) input)
           nil)
          ((string-match-p (rx (or "exit" "quit" "ttyl" "bye")) input)
           (keyboard-quit))
          (t
           (sc-emit "Override not understood.")))
    ))

(defun sc-change-date ()
  (require 'org)
  (org-read-date)
  (warn "Unimplemented")
  )

(defun sc-change-date-yesterday ()
  (message "Applying this query AND subsequent queries to yesterday.")
  (setq sc--date (ts-dec 'day 1 (ts-now))))



(provide 'sc-nlp)

;;; sc-nlp.el ends here
