;;; secretary-tests.el -*- lexical-binding: t; -*-
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

;; TODO
;; Test: save ts-now, wait, run secretary--user-is-active, compare.
;; Use a special function that runs the timers very fast for testing.
;; (ert-deftest idle-beginning-does-update ()
;;   (lambda (&optional force-idle)
;;     (if (or force-idle (secretary-idle-p))
;; 	(setq secretary--timer (run-with-timer 1 nil #'secretary--user-is-idle)))
;;     (setq secretary--timer (run-with-timer 1 nil #'secretary--user-is-active)))
;;   )


(ert-deftest ts-usage ()
  (let ((now (ts-now)))
    (should (equal (ts-dec 'month 12 now)
                   (ts-dec 'year 1 now)))
    (should (= 16 (length (-uniq (append
                                  (--iterate (ts-dec 'month 1 it) now 12)
                                  (--iterate (ts-dec 'year 1 it) now 5))))))))

;; (secretary-last-datestamp-in-file "/home/kept/Self_data/weight.tsv")
;; (secretary-last-datestamp-in-file "/home/kept/Self_data/mood.tsv")
;; (secretary-last-timestamp-in-tsv "/home/kept/Self_data/sleep.tsv")
;; (secretary-get-all-today-in-tsv "/home/kept/Self_data/sleep.csv" (ts-dec 'day 1 (ts-now)))

(let ((var1-file-name "/tmp/...")
      (var2-file-name "/tmp/...")))

(ert-deftest all-queries-write-to-disk-correctly

  )

(ert-deftest all-queries-can-be-cancelled
    (let ((var1-file-name "/tmp/...")
          (var2-file-name "/tmp/...")))
    )

(ert-deftest welcomer-produces-no-error
    (let ((var1-file-name "/tmp/...")
          (var2-file-name "/tmp/...")))
    )
(provide 'secretary-tests)


;;; secretary-tests.el ends here
