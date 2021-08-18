;;; ass-test.el --- Unit tests -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021 Martin Edström

;; This file is not part of GNU Emacs.

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

;; Unit tests.

;;; Code:

(require 'ass-config)
(require 'ert)

;; (setq ass--idle-beginning (setq ass--last-online (make-ts :unix 0)))

;; (ass--count-successes-today #'ass-present-diary)
;; (ass--call-timidly)
;; (named-timer-run :ass-attempt (* 60 60) (* 60 60) #'ass--call-timidly)
;; (run-with-timer 3 nil (lambda ()  (print (frame-focus-state))))
;; (ass-call-from-idle)
;; (ass-memory-put 'ass-items ass-items)

;; HACK: sit times specific to my machine
(ert-deftest ass-test-pid ()
  (ass-mode 0)
  (let ((p (pfuture-new "emacs" "--with-profile=doom")))
    (sit-for 5)
    (should (ass--another-ass-running-p))
    (kill-process p)
    (sit-for 1)
    (should-not (ass--another-ass-running-p))))

(ert-deftest ass-test-minibuffer-cancel-fn ()
  (run-with-timer .3 nil (lambda () (should (eq 'abort-recursive-edit (key-binding (kbd "C-g"))))))
  (ass-query-weight))

(ert-deftest ass-test-idle1 ()
  (ass-mode 0)
  (setq ass--idle-seconds-fn #'ass--emacs-idle-seconds)
  (ass--start-next-timer t)
  (sit-for .05)
  (should (eq (seq-elt (named-timer-get :ass) 5)
              'ass--user-is-idle))
  (sit-for 2)
  (should (eq (seq-elt (named-timer-get :ass) 5)
              'ass--user-is-active)))

(ert-deftest ass-test-keepalive ()
  (ass-mode 0)
  (setq ass--idle-seconds-fn #'ass--emacs-idle-seconds)
  (ass--keepalive)
  (should (named-timer-get :ass))
  ;; Takedown
  (named-timer-cancel :ass-keepalive)
  (named-timer-cancel :ass))

(ert-deftest ass-test-ts-usage ()
  (let ((now (ts-now)))
    (should (equal (ts-dec 'month 12 now)
                   (ts-dec 'year 1 now)))
    (should (= 16 (length (-uniq (append
                                  (--iterate (ts-dec 'month 1 it) now 12)
                                  (--iterate (ts-dec 'year 1 it) now 5))))))
    ;; (should (= 66 (length (ass-past-sample-default))))
    (should (-all-p #'ts-p (ass-past-sample-greedy)))))

(provide 'ass-test)

;;; ass-test.el ends here
