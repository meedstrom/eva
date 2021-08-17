;;; secretary-activity.el -*- lexical-binding: t; nameless-current-name: "secretary"; -*-
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

;; (require 'org-id)
;; (org-id-update-id-locations '("/home/kept/Emacs/secretary/test.org"))

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
  "Get the first activity in `secretary-activities' matching NAME."
  (--find (equal name (secretary-activity-name it)) secretary-activities))

(defun secretary-activities-names ()
  "Get the :name of all members of `secretary-activities'."
  (-map #'secretary-activity-name secretary-activities))

;; TODO: Get all informally named activities from the dataset.
(secretary-defquery secretary-query-activity ()
  "Ask user what they're up to."
  (let* ((name (secretary-read "What are you up to? "
                               (secretary-activities-names)))
         (name-corrected
          (--find (member it (secretary-activities-names))
                  (list name
                        (capitalize name)
                        (downcase name))))
         (name (if name-corrected
                   name-corrected
                 name))
         (activity (secretary-activity-by-name name)))
    (secretary-append-tsv current-dataset
      (ts-format secretary--date) ;; the time the activity happened
      name
      (when activity
        (secretary-activity-id activity)))))

(provide 'secretary-activity)

;;; secretary-activity.el ends here
