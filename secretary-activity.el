;;; secretary-activity.el -*- lexical-binding: t; -*-
;; Copyright (C) 2021 Martin Edström

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

;; Things related to activity tracking.  Still experimental.

;;; Code:

(require 'secretary)

(cl-defstruct (secretary-activity
               (:constructor secretary-activity-create)
               (:copier nil))
  name
  id
  cost-false-pos
  cost-false-neg
  query)

(defvar secretary-activities)

(defun secretary-activity-by-name (name)
  (--find (equal name (secretary-activity-name it)) secretary-activities))

(defun secretary-activities-names ()
  (-map #'secretary-activity-name secretary-activities))

(secretary-defquery secretary-query-activity ()
  (let* ((name (secretary-read "What are you up to? " (secretary-activities-names))))
    (secretary-append-tsv current-dataset
      (ts-format secretary--date) ;; the time the activity happened
      name
      (secretary-activity-id (secretary-activity-by-name name)))
    (secretary-emit-same-line name)))

(provide 'secretary-activity)

;;; secretary-activity.el ends here
