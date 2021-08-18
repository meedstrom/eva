;;; ass-doing.el --- activity tracking -*- lexical-binding: t; -*-

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

;; Things related to tracking doings.  Still experimental.

;;; Code:

(require 'ass)

(cl-defstruct (ass-doing
               (:constructor ass-doing-create)
               (:copier nil))
  name
  id
  cost-false-pos
  cost-false-neg
  query)

(defvar ass-doings)

(defun ass-doing-by-name (name)
  "Get the first doing in `ass-doings' matching NAME."
  (--find (equal name (ass-doing-name it)) ass-doings))

(defun ass-doings-names ()
  "Get the :name of all members of `ass-doings'."
  (-map #'ass-doing-name ass-doings))

;; TODO: Get all informally named doings from the dataset.
(ass-wrap ass-query-doing ()
          "Ask user what they're up to."
          (let* ((name (ass-read "What are you up to? "
                                 (ass-doings-names)))
                 (name-corrected
                  (--find (member it (ass-doings-names))
                          (list name
                                (capitalize name)
                                (downcase name))))
                 (name (if name-corrected
                           name-corrected
                         name))
                 (doing (ass-doing-by-name name)))
            (ass-tsv-append ass-curr-dataset
                            (ts-format ass-date) ;; the time the doing happened
                            name
                            (when doing
                              (ass-doing-id doing)))))

(provide 'ass-doing)

;; Local Variables:
;; nameless-current-name: "ass"
;; End:

;;; ass-doing.el ends here
