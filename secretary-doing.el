;;; secretary-doing.el --- activity tracking -*- lexical-binding: t; -*-

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

(cl-defstruct (secretary-doing
               (:constructor secretary-doing-create)
               (:copier nil))
  name
  id
  cost-false-pos
  cost-false-neg
  query)

(defvar secretary-doings)

(defun secretary-doing-by-name (name)
  "Get the first doing in `secretary-doings' matching NAME."
  (--find (equal name (secretary-doing-name it)) secretary-doings))

(defun secretary-doings-names ()
  "Get the :name of all members of `secretary-doings'."
  (-map #'secretary-doing-name secretary-doings))

;; TODO: Get all informally named doings from the dataset.
(secretary-defquery secretary-query-doing ()
  "Ask user what they're up to."
  (let* ((name (secretary-read "What are you up to? "
                               (secretary-doings-names)))
         (name-corrected
          (--find (member it (secretary-doings-names))
                  (list name
                        (capitalize name)
                        (downcase name))))
         (name (if name-corrected
                   name-corrected
                 name))
         (doing (secretary-doing-by-name name)))
    (secretary-tsv-append current-dataset
      (ts-format secretary-date) ;; the time the doing happened
      name
      (when doing
        (secretary-doing-id doing)))))

(provide 'secretary-doing)

;; Local Variables:
;; nameless-current-name: "secretary"
;; End:

;;; secretary-doing.el ends here
