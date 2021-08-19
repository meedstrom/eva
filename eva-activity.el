;;; eva-activity.el --- activity tracking -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Martin Edström

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Things related to activity tracking.  Still experimental.

;;; Code:

(require 'eva)

(cl-defstruct (eva-activity
               (:constructor eva-activity-create)
               (:copier nil))
  name
  id
  cost-false-pos
  cost-false-neg
  query)

(defvar eva-activity-list)

(defun eva-activity-by-name (name)
  "Get the first activity in `eva-activitys' matching NAME."
  (--find (equal name (eva-activity-name it)) eva-activity-list))

(defun eva-activity-names ()
  "Get the :name of all members of `eva-activitys'."
  (-map #'eva-activity-name eva-activity-list))

;; TODO: Get all informally named activities from the dataset.
(eva-defun eva-query-activity ()
  "Ask user what they're up to."
  (let* ((name (eva-read "What are you up to? " (eva-activity-names)))
         (name-corrected
          (--find (member it (eva-activity-names))
                  (list name (capitalize name) (downcase name))))
         (name (if name-corrected
                   name-corrected
                 name))
         (activity (eva-activity-by-name name)))
    (eva-tsv-append eva-curr-dataset
      (ts-format eva-date) ;; the time the activity happened
      name
      (when activity
        (eva-activity-id activity)))))

(provide 'eva-activity)

;; Local Variables:
;; nameless-current-name: "eva"
;; End:

;;; eva-activity.el ends here
