;;; secretary-test.el --- Unit tests -*- lexical-binding: t; -*-

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

(require 'secretary-config)
(require 'ert)

;; (setq secretary--idle-beginning (setq secretary--last-online (make-ts :unix 0)))

;; (secretary--count-successes-today #'secretary-present-diary)
;; (secretary--call-timidly)
;; (named-timer-run :secretary-attempt (* 60 60) (* 60 60) #'secretary--call-timidly)
;; (run-with-timer 3 nil (lambda ()  (print (frame-focus-state))))
;; (secretary-call-from-idle)
;; (secretary-memory-put 'secretary-items secretary-items)

;; HACK: sit times specific to my machine
(ert-deftest secretary-test-pid ()
  (secretary-mode 0)
  (let ((p (pfuture-new "emacs" "--with-profile=doom")))
    (sit-for 5)
    (should (secretary--another-secretary-running-p))
    (kill-process p)
    (sit-for 1)
    (should-not (secretary--another-secretary-running-p))))

(ert-deftest secretary-test-minibuffer-cancel-fn ()
  (run-with-timer .3 nil (lambda () (should (eq 'abort-recursive-edit (key-binding (kbd "C-g"))))))
  (secretary-query-weight))

(ert-deftest secretary-test-idle1 ()
  (secretary-mode 0)
  (setq secretary--idle-seconds-fn #'secretary--emacs-idle-seconds)
  (secretary--start-next-timer t)
  (sit-for .05)
  (should (eq (seq-elt (named-timer-get :secretary) 5)
              'secretary--user-is-idle))
  (sit-for 2)
  (should (eq (seq-elt (named-timer-get :secretary) 5)
              'secretary--user-is-active)))

(ert-deftest secretary-test-keepalive ()
  (secretary-mode 0)
  (setq secretary--idle-seconds-fn #'secretary--emacs-idle-seconds)
  (secretary--keepalive)
  (should (named-timer-get :secretary))
  ;; Takedown
  (named-timer-cancel :secretary-keepalive)
  (named-timer-cancel :secretary))

(ert-deftest secretary-test-ts-usage ()
  (let ((now (ts-now)))
    (should (equal (ts-dec 'month 12 now)
                   (ts-dec 'year 1 now)))
    (should (= 16 (length (-uniq (append
                                  (--iterate (ts-dec 'month 1 it) now 12)
                                  (--iterate (ts-dec 'year 1 it) now 5))))))
    ;; (should (= 66 (length (secretary-past-sample-default))))
    (should (-all-p #'ts-p (secretary-past-sample-greedy)))))

(provide 'secretary-test)

;;; secretary-test.el ends here
