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

(require 'secretary-config)
(require 'ert)

;; (setq secretary--idle-beginning (setq secretary--last-online (make-ts :unix 0)))

;; HACK: sit times specific to my machine
(ert-deftest secretary-test-pid ()
  (secretary-mode 0)
  (let ((p (secretary--run-async "emacs" "--with-profile=doom")))
    (sit-for 5)
    (should (secretary--another-secretary-running-p))
    (kill-process p)
    (sit-for 1)
    (should-not (secretary--another-secretary-running-p))))

(ert-deftest secretary-test-idle1 ()
  (secretary-mode 0)
  (setq secretary--idle-seconds-fn #'org-emacs-idle-seconds)
  (secretary--start-next-timer t)
  (sit-for .05)
  (should (eq (seq-elt (named-timer-get :secretary) 5) 'secretary--user-is-idle))
  (sit-for 2)
  (should (eq (seq-elt (named-timer-get :secretary) 5) 'secretary--user-is-active)))

(ert-deftest secretary-test-keepalive ()
  (secretary-mode 0)
  (setq secretary--idle-seconds-fn #'org-emacs-idle-seconds)
  (secretary--keepalive)
  (should (named-timer-get :secretary))
  ;; Takedown
  (named-timer-cancel :secretary-keepalive)
  (named-timer-cancel :secretary))

(ert-deftest secretary-test-defun ()
  (should
   (equal
    (macroexpand '(secretary-defquery foo (x1 x2)
                    "docstr"
                    (bar)
                    (baz)))
    (macroexpand '(cl-defun foo (x1 x2)
                    "docstr"
                    (interactive)
                    (setq secretary--current-fn #'foo)
                    (unless (secretary--item-by-fn secretary--current-fn)
                      (error "%s not listed in secretary-items" (symbol-name secretary--current-fn)))
                    (advice-add 'abort-recursive-edit :before #'secretary--after-cancel-do-things)
                    (let ((current-dataset (secretary-item-dataset
                                            (secretary--item-by-fn secretary--current-fn))))
                      (unwind-protect
                          (prog1 (progn
                                   (bar)
                                   (baz))
                            (setq secretary--queue
                                  (remove secretary--current-fn secretary--queue))
                            (setf (secretary-item-dismissals
                                   (secretary--item-by-fn secretary--current-fn))
                                  0)
			    (when (null current-dataset)
			      (secretary-append-tsv
			       (expand-file-name "successes-foo" secretary-memory-dir))))
                        (advice-remove 'abort-recursive-edit #'secretary--after-cancel-do-things))))))))

(ert-deftest secretary-test-ts-usage ()
  (let ((now (ts-now)))
    (should (equal (ts-dec 'month 12 now)
                   (ts-dec 'year 1 now)))
    (should (= 16 (length (-uniq (append
                                  (--iterate (ts-dec 'month 1 it) now 12)
                                  (--iterate (ts-dec 'year 1 it) now 5))))))
    ;; (should (= 66 (length (secretary-past-sample-default))))
    (should (-all-p #'ts-p (secretary-past-sample-greedy)))))

(provide 'secretary-tests)

;;; secretary-tests.el ends here
