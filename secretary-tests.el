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

(ert-deftest ts-usage ()
  (let ((now (ts-now)))
    (should (equal (ts-dec 'month 12 now)
                   (ts-dec 'year 1 now)))
    (should (= 16 (length (-uniq (append
                                  (--iterate (ts-dec 'month 1 it) now 12)
                                  (--iterate (ts-dec 'year 1 it) now 5))))))))

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
