;;; eva-test.el --- Unit tests -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021 Martin Edström

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

;; Unit tests.

;;; Code:

(require 'eva-builtin)
(require 'eva)
(require 'ert)

;; (setq eva--idle-beginning (setq eva--last-online (make-ts :unix 0)))

;; (eva--count-successes-today #'eva-present-diary)
;; (eva--call-timidly)
;; (named-timer-run :eva-attempt (* 60 60) (* 60 60) #'eva--call-timidly)
;; (run-with-timer 3 nil (lambda ()  (print (frame-focus-state))))
;; (eva-call-from-idle)
;; (eva-memory-put 'eva-items eva-items)

;; HACK: sit times specific to my machine. Also the chemacs profile
;; (ert-deftest eva-test-pid ()
;;   (eva-mode 0)
;;   (let ((p (pfuture-new "emacs" "--with-profile=doom")))
;;     (sit-for 5)
;;     (should (eva--another-eva-running-p))
;;     (kill-process p)
;;     (sit-for 1)
;;     (should-not (eva--another-eva-running-p))))

;; (ert-deftest eva-test-idle1 ()
;;   (let ((was-on eva-mode))
;;     (eva-mode 0)
;;     (setq eva--idle-secs-fn #'eva--idle-secs-emacs)
;;     (eva--start-next-timer t)
;;     (sit-for .05)
;;     (should (eq (seq-elt (named-timer-get :eva) 5)
;;                 'eva--user-is-idle))
;;     (sit-for 2)
;;     (should (eq (seq-elt (named-timer-get :eva) 5)
;;                 'eva--user-is-present))
;;     (when was-on
;;       (eva-mode))))

(ert-deftest eva-test-keepalive ()
  (eva-mode 0)
  (setq eva--idle-secs-fn #'eva--idle-secs-emacs)
  (eva--keepalive)
  (should (named-timer-get :eva))
  ;; Takedown
  (named-timer-cancel :eva-keepalive)
  (named-timer-cancel :eva-retry)
  (named-timer-cancel :eva))

(ert-deftest eva-test-ts-usage ()
  (let ((now (ts-now)))
    (should (equal (ts-dec 'month 12 now)
                   (ts-dec 'year 1 now)))
    (should (= 16 (length (-uniq (append
                                  (--iterate (ts-dec 'month 1 it) now 12)
                                  (--iterate (ts-dec 'year 1 it) now 5))))))
    ;; (should (= 66 (length (eva-past-sample-default))))
    (should (-all-p #'ts-p (eva-past-sample-greedy)))))

(provide 'eva-test)

;;; eva-test.el ends here
