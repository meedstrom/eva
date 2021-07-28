;;; secretary-mothball.el -*- lexical-binding: t; -*-
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

(require 'secretary-common)

(defun secretary-timestamp-for-chat ()
  (concat "<" (ts-format "%H:%M") "> "))

(defun secretary-human-readable-ts (&optional ts)
  (declare (side-effect-free t))
  (ts-format "%Y-%b-%d %H:%M" ts))

(defun secretary-unix-ts (&optional ts)
  "Return an Unix timestamp at time TS, with zero-padded
decimals, as a string, typically to be written to a file."
  (declare (side-effect-free t))
  (s-pad-right 18 "0" (number-to-string (ts-unix (or ts (ts-now))))))

;; TODO: Ok, use candidates as an index, use `--map-indexed' for each subtype of
;;       thing to review, so the results can be sorted chronologically
(defun secretary-present-diary-as-datetree ()
  "Unused"
  (interactive)
  (let* ((buffer (get-buffer-create (concat "*" secretary-ai-name ": Selected diary entries*")))
         (dates-to-check (secretary-past-sample-default))
         (discrete-files-found (--keep (secretary-existing-diary "/home/kept/Diary" it) dates-to-check))
         (datetree-found-count (secretary-make-indirect-datetree buffer dates-to-check))
         (total-found-count (+ (length discrete-files-found) datetree-found-count)))
    (if (and (< 0 total-found-count)
             (secretary-prompt (concat "Found " (int-to-string total-found-count) " diary "
                                (if (= 1 total-found-count) "entry" "entries")
                                " for this date from the past. Want me to open "
                                (if (= 1 total-found-count) "it" "them")
                                "?")))
        (progn
          (switch-to-buffer (get-buffer buffer))
          (dolist (f discrete-files-found)
            (goto-char (point-max))
            ;; org-paste-subtree may be better
            (insert "\n* " f "\n")
            (insert-file-contents f))
          (view-mode)
          (read-only-mode)
          (goto-char (point-min)))
      (kill-buffer buffer))))


(defun va-welcome (&optional just-idled-p)
  (setq va--date (ts-now))
  (va-check-neglect)
  (and just-idled-p
       (va-ynp "Have you slept?")
       (va-query-sleep))
  (unless (va-logged-today "/home/kept/Self_data/weight.tsv")
    (va-query-weight))
  ;; (and (va-ynp "Up to reflect?")
  ;;      (va-ynp "Have you learned something?")
  ;;      (org-capture nil "s"))
  ;; (if (va-ynp (concat "How about some flashcards?"))
  ;;     (org-drill))
  ;; (if (va-ynp "Have you stretched today?")
  ;;     nil
  ;;   (if (va-ynp "Do you want reminders for why?")
  ;;       nil
  ;;     nil))
  ;; (if (va-ynp "Did you photographe your face today?")
  ;;     nil)
  ;; ;; (unless (va-just-slept)
  ;; (unless (va-logged-today "/home/kept/Self_data/meditation.csv")
  ;;   (va-query-meditation va--date))
  ;; (unless (va-logged-today "/home/kept/Self_data/cold.csv")
  ;;   (when (va-ynp "Have you had a cold shower yet?")
  ;;     (va-query-cold-shower va--date)))
  ;; (if (va-ynp "Have you paid for anything since yesterday?")
  ;;     (org-capture nil "ln"))
  ;; (if (va-ynp "Shall I remind you of your life goals? Don't be shy.")
  ;;     (view-file "/home/kept/Journal/gtd2.org"))
  (and (>= 1 (va-query-mood "How are you? "))
       (va-ynp "Do you need to talk?")
       (va-ynp "I can direct you to my colleague Eliza, though "
               "she's not too bright. Will that do?")
       (doctor))
  (va-present-plots)
  ;; and (va-ynp "Would you like me to suggest an activity?")
  (va-present-diary (ts-now))
  (and (-all-p #'not
               (-map #'va-logged-today
                     (-map #'va-scheme-log-file va-schemes)))
       (va-ynp "Shall I come back in an hour?")
       (run-with-timer 3600 nil #'va-call-from-idle)))

(defun secretary-midquery-keyboard-quit ()
  "Quit, and record which query was quit."
  (interactive)
  (if (or (> (recursion-depth) 1)
          ;; see minibuffer-keyboard-quit code
          (and (minibufferp) (region-active-p) delete-selection-mode))
      (minibuffer-keyboard-quit)
    (internal-pop-keymap secretary-query-keymap 'overriding-terminal-local-map) ;; just in case
    ;; the main reason for this wrapper
    (cl-incf (secretary-scheme-dismissals
              (secretary--scheme-by-query secretary--current-query)))
    (setq secretary--current-query nil) ;; not essential, just could prevent confusion sometime
    (if (minibufferp)
        (abort-recursive-edit)
      (keyboard-quit))))


(defvar secretary--current-query nil
  "Information used by `secretary-midquery-keyboard-quit'.")

(defvar secretary-query-keymap (make-sparse-keymap))
(define-key secretary-query-keymap [remap keyboard-quit] #'secretary-midquery-keyboard-quit)
(define-key secretary-query-keymap [remap minibuffer-keyboard-quit] #'secretary-midquery-keyboard-quit)
(define-key secretary-query-keymap [remap abort-recursive-edit] #'secretary-midquery-keyboard-quit)
(define-key secretary-query-keymap [remap doom/escape] #'secretary-midquery-keyboard-quit)

(defmacro secretary-defquery1.0 (name arglist &optional docstring &rest body)
  "Boilerplate wrapper for `defun'.
To see what it expands to, try something like

    (macroexpand '(secretary-defquery foo (x1 x2) (frobnicate)))

Or better, visit secretary-tests.el to read the tests of this macro.

Manages the external variables `secretary--current-fn' and
`secretary--queue'. If you use a simple `defun' in lieu of this
wrapper, you must set these!

In BODY, you have access to extra temporary variables:
- \"interactivep\" which is more reliable than the function `called-interactively-p'.
- \"this-dataset\" which is a reference to (secretary-action-dataset (secretary--action-by-fn secretary--current-fn))."
  (declare (indent defun) (doc-string 3))
  (unless (stringp docstring)
    (push docstring body))
  (let* ((user-spec (and (eq 'interactive (car-safe (car body)))
                                           (car-safe (cdr (car body)))))
         (user-spec-length (when user-spec
                             (length
                              (split-string user-spec "\n"))))
         (new-spec (if user-spec
                       `(interactive
                         ,(concat user-spec "\n"
                                  (cl-reduce #'concat
                                             (make-list (- (length arglist)
                                                           user-spec-length)
                                                        "i\n"))
                                  "p"))
                     `(interactive
                       ,(concat (cl-reduce #'concat
                                           (make-list (length arglist)
                                                      "i\n"))
                                "p")))))
    `(defun ,name ,(-snoc arglist 'interactivep)
       ,@(if (stringp docstring)
             (list docstring
                   new-spec)
           (list new-spec))
       (setq secretary--current-query #',name)
       (unless (secretary--action-by-query secretary--current-query)
         (error "%s not listed in secretary-actions" (symbol-name secretary--current-query)))
       (let ((this-dataset (secretary-action-dataset
                            (secretary--action-by-query secretary--current-query))))
         ,(unless user-spec (car body))
         (prog1 (progn ,@(cdr body))
           (setq secretary--queue
                 (remove secretary--current-query secretary--queue)))))))


(provide 'secretary-mothball)

;;; secretary-mothball.el ends here
