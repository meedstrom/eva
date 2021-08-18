;;; eva-doing.el --- activity tracking -*- lexical-binding: t; -*-

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

(require 'eva)

(cl-defstruct (eva-doing
               (:constructor eva-doing-create)
               (:copier nil))
  name
  id
  cost-false-pos
  cost-false-neg
  query)

(defvar eva-doings)

(defun eva-doing-by-name (name)
  "Get the first doing in `eva-doings' matching NAME."
  (--find (equal name (eva-doing-name it)) eva-doings))

(defun eva-doings-names ()
  "Get the :name of all members of `eva-doings'."
  (-map #'eva-doing-name eva-doings))

;; TODO: Get all informally named doings from the dataset.
(eva-wrap eva-query-doing ()
          "Ask user what they're up to."
          (let* ((name (eva-read "What are you up to? "
                                 (eva-doings-names)))
                 (name-corrected
                  (--find (member it (eva-doings-names))
                          (list name
                                (capitalize name)
                                (downcase name))))
                 (name (if name-corrected
                           name-corrected
                         name))
                 (doing (eva-doing-by-name name)))
            (eva-tsv-append eva-curr-dataset
                            (ts-format eva-date) ;; the time the doing happened
                            name
                            (when doing
                              (eva-doing-id doing)))))

(provide 'eva-doing)

;; Local Variables:
;; nameless-current-name: "eva"
;; End:

;;; eva-doing.el ends here
