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



;;;; New presenter system

(defvar secretary--presentation-alist
  '((secretary-pres-make-plot-mood-async . "Not yet built")
    (secretary-pres-make-ledger-summary-async . "Not yet built")))

(defvar secretary--current-fn nil)

(defun secretary-buffer-presentations ()
  (let ((buf (get-buffer-create (concat "*" secretary-ai-name ": Presentations*"))))
    (with-current-buffer buf
      (buffer-disable-undo))
    buf))

;; (defun secretary-build-presentations-async ()
;;   (when secretary--presentation-alist
;;     (dolist (x secretary--presentation-alist)
;;       (let ((fn (car x))
;;             (result (ignore-errors (funcall (car x)))))
;;         (setq secretary--presentation-alist
;;               (if (stringp result)
;;                   (a-assoc secretary--presentation-alist fn result)
;;                 (a-assoc secretary--presentation-alist
;;                          fn
;;                          (concat (symbol-name fn) " failed."))))))
;;     (setq secretary-pres-index 0)))

(defun secretary-build-presentations-async ()
  (when secretary--presentation-alist
    (dolist (x secretary--presentation-alist)
      (funcall (car x)))
    (setq secretary-pres-index 0)))

;; for manual call
(defun secretary-view-presentations ()
  (interactive)
  (display-buffer (secretary-buffer-presentations)))

(defvar secretary--pres-i 0)

;; To be bound to "n" or so.
(defun secretary-present-next ()
  (interactive)
  (if (>= (1+ secretary--pres-i) (length secretary--presentation-alist))
      (message "No more items.  Press q to quit.")
    (cl-incf secretary--pres-i))
  (secretary--pres-insert secretary--pres-i))

;; To be bound to "p" or so.
(defun secretary-present-previous ()
  (interactive)
  (unless (>= 0 secretary--pres-i)
    (cl-decf secretary--pres-i))
  (secretary--pres-insert secretary--pres-i))

(defun secretary--pres-insert (i)
  (with-current-buffer (secretary-buffer-presentations)
    (delete-region (point-min) (point-max))
    (let ((presentation (nth i secretary--presentation-alist)))
      (insert (cdr presentation)))))

;; NOTE: these functions run asynchronously. This is not great for pureness, we
;; can't use their output the moment of calling them, instead they each have to
;; update the alist when finished, so run them well ahead of time.
;;
;; Async is often desirable, but I'm not sure here. The largest time sink for R
;; is probably importing libraries, unless you're running MCMC or doing big data,
;; so we should be able to spin up a persistent R process to talk to
;; synchronously, and then we can make code that's easier to reason about.

(defun secretary-pres-make-plot-mood-async ()
  (interactive)
  (setq secretary--current-fn #'secretary-pres-make-plot-mood-async)
  (secretary-pres-make-plot "mood.gnuplot"))

(defun secretary-pres-make-plot (gnuplot-script-basename)
  (interactive)
  (let* ((default-directory "/tmp/secretary")
         (pkg-dir (f-dirname (find-library-name "secretary")))
         (r-script (expand-file-name "R/make_data_for_plots.R" pkg-dir))
         (gnuplot-script (expand-file-name gnuplot-script-basename pkg-dir)))
    (mkdir "/tmp/secretary" t)
    (unless (executable-find "gnuplot")
      (error "gnuplot not found in PATH"))
    (set-process-sentinel
     (start-process secretary-ai-name nil "Rscript" r-script)
     `(lambda (_process event)
        (let ((error-p (s-contains-p "error" event))
              (new-result nil))
          (unless error-p
            (setq new-result (ignore-errors (with-temp-buffer
                                              (call-process "gnuplot" ,gnuplot-script t)
                                              (buffer-string))))
            (unless (stringp new-result)
              (setq error-p t)))
          (setq secretary--presentation-alist
                (if error-p
                    (a-assoc secretary--presentation-alist secretary--current-fn
                             (concat (symbol-name secretary--current-fn) " failed or didn't run."))
                  (a-assoc secretary--presentation-alist secretary--current-fn new-result))))))))

;;(require 'pfuture)
;;(pfuture-new "gnuplot" gnuplot-script-path)

;; Does not require ledger-mode.
(defun secretary-pres-make-ledger-summary-async ()
  (setq secretary--current-fn #'secretary-pres-make-ledger-summary-async)
  (if (executable-find "ledger")
      (let ((new-result
             (with-temp-buffer
               (call-process "ledger" nil t nil
                             "-f" secretary-ledger-file-name "register" "-M")
               (buffer-string))))
        (setq secretary--presentation-alist
              (a-assoc secretary--presentation-alist secretary--current-fn new-result)))
    (secretary-emit "Ledger executable not found, skipping.")))

(defun secretary-present-ledger-report ()
  "Jump to `secretary-ledger-file-name' and run `ledger-report'."
  (interactive)
  (when (ignore-errors (find-library-name "ledger-mode"))
    (require 'ledger-mode)
    (if (get-buffer ledger-report-buffer-name)
        (ledger-report-goto)
      (find-file secretary-ledger-file-name)
      (call-interactively #'ledger-report))))


;;;; Plot presenter

(defvar secretary-plot-hook nil
  "Hook called to print plots. A convenient place to add your
custom plots.")

(defmacro secretary--plot-ascii (gnuplot-script message &rest after-body)
  "Make an ascii plot.
Emit MESSAGE and run GNUPLOT-SCRIPT. After the plotting is done,
run any forms in AFTER-BODY."
  `(let* ((default-directory "/tmp/secretary")
          (r-script (expand-file-name "R/make_data_for_plots.R"
                                      (f-dirname (find-library-name "secretary")))))
     (mkdir "/tmp/secretary" t)
     ;; TODO: reuse secretary's R process to minimize package load time, so
     ;; we can use use call-process for easier reasoning.
     (set-process-sentinel
      (start-process secretary-ai-name nil "Rscript" r-script)
      (lambda (_1 _2)
        (secretary-emit ,message)
        (unless (get-buffer-window (secretary-buffer-chat))
          (display-buffer (secretary-buffer-chat)))
        (goto-char (point-max))
        (call-process "gnuplot" ,gnuplot-script (secretary-buffer-chat))
        ;; Strip formfeed inserted by gnuplot.
        (search-backward "\f")
        (replace-match "")
        (goto-char (point-max))
        ,@after-body))))

(defun secretary-plot-mood ()
  (interactive)
  (let* ((default-directory "/tmp/secretary")
         (pkg-dir (f-dirname (find-library-name "secretary")))
         (script (expand-file-name "R/sc_daily_plot.R" pkg-dir))
         (plot (expand-file-name "sc_mood.png" default-directory)))
    (secretary-emit "Plotting mood...")
    (mkdir "/tmp/secretary" t)
    (set-process-sentinel
     (start-process secretary-ai-name nil "Rscript" script "14" "-0.1")
     `(lambda (_process _event)
        (secretary-emit "And that's your mood." "\n")
        (switch-to-buffer (secretary-buffer-chat))
        (insert-image-file ,plot)
        (goto-char (point-max))
        (insert "\n")))))

(defun secretary-plot-mood-ascii ()
  (interactive)
  (secretary--plot-ascii
   (expand-file-name "mood.gnuplot" (f-dirname (find-library-name "secretary")))
   "Plotting mood..."
   (search-backward "Plotting mood...")
   (forward-line 1)
   (goto-char (line-end-position))
   (just-one-space)
   (insert-rectangle (last
                      (split-string (f-read "/tmp/secretary/mood_desc.txt"))
                      16))
   (goto-char (point-max))))

(defun secretary-plot-weight ()
  (interactive)
  (let* ((default-directory "/tmp/secretary")
         (script (expand-file-name "R/sc_daily_plot.R"
                                   (f-dirname (find-library-name "secretary"))))
         (plot (expand-file-name "sc_plot1.png" "/tmp/secretary")))
    (secretary-emit "Plotting weight...")
    (mkdir "/tmp/secretary" t)
    (set-process-sentinel
     (start-process secretary-ai-name nil "Rscript" script "14" "-0.1")
     `(lambda (_process _event)
        (switch-to-buffer (secretary-buffer-chat))
        (if (insert-image-file ,plot)
            (progn
              (forward-char -1)
              (secretary-emit "And here's your weight, boss." "\n")
              (delete-file ,plot))
          (secretary-emit "Could not plot. :'( See `secretary-plot-weight' source."))
        (goto-char (point-max))
        (insert "\n")))))

(defun secretary-plot-weight-ascii ()
  (interactive)
  (let* ((default-directory "/tmp/secretary")
         (r-script (expand-file-name "R/make_data_for_plots.R"
                                     (f-dirname (find-library-name "secretary"))))
         (gnuplot-script (expand-file-name "weight.gnuplot"
                                           (f-dirname (find-library-name "secretary")))))
    (mkdir "/tmp/secretary" t)
    ;; TODO: reuse secretary's R process to minimize package load time.
    (set-process-sentinel
     ;; TODO: pass start-date (today minus 3mo) and projection incline, letting
     ;; user change the incline (persist for future plots too)
     (start-process secretary-ai-name nil "Rscript" r-script)
     `(lambda (_process _event)
        (secretary-emit "Plotting weight...")
        ;; (unless (get-buffer-window (secretary-buffer-chat)) (switch-to-buffer (secretary-buffer-chat)))
        (with-current-buffer (secretary-buffer-chat)
          (goto-char (point-max))
          (call-process "gnuplot" ,gnuplot-script (secretary-buffer-chat))
          ;; Strip formfeed inserted by gnuplot.
          (search-backward "\f")
          (replace-match "")
          (goto-char (point-max)))))))

(defun secretary-present-plots ()
  (interactive)
  (unless (null secretary-plot-hook)
    (secretary-emit (seq-random-elt '("I have plotted the latest intel, boss."
                               "Here are some projections!"
                               "Data is beautiful, don't you think?")))
    ;; HACK: because of async; want sync.
    (dolist (hook secretary-plot-hook)
      (funcall hook)
      (sit-for .5))))


;;;; Other presenters

(defun secretary-present-mood ()
  (interactive)
  (view-file "/home/kept/Self_data/mood.tsv")
  (auto-revert-mode))

(defun secretary-present-ledger-report ()
  (interactive)
  (when (or (featurep 'ledger-mode-autoloads)
            (fboundp #'ledger-report))
    (require 'ledger-mode)
    (let ((file secretary-ledger-file-name))
      ;; for some reason, (with-current-buffer (get-buffer-create file)) leads
      ;; to all kinds of errors, so we have to use find-file
      (find-file-noselect file)
      (call-interactively #'ledger-report))))

(defun secretary-make-ods-for-finance-2020 ()
  "Make and open an ODS spreadsheet generated from Ledger data."
  (interactive)
  (let* ((r-script "/home/kept/Journal/Finances/R/generate_an_ods.R")
         (default-directory (f-dirname (f-dirname r-script))))
    (call-process "Rscript" nil nil nil r-script "l.ledger" "2020.ods" "SEK")
    (start-process "soffice" nil "soffice" "2020.ods")))

(defun secretary-make-ods-for-finance ()
  "Make and open an ODS spreadsheet from Ledger data.
Requires the ssconvert program that comes with Gnumeric."
  (interactive)
  (let* ((script (expand-file-name "R/generate_an_ods.R"
                                   (f-dirname (find-library-name "secretary"))))
         (sheet (expand-file-name ".tmp_finances.ods"
                                  secretary-memory-dir))
         (default-directory (f-dirname script))
         (app (seq-find #'executable-find '("gnumeric"
                                            "soffice"
                                            "open"
                                            "mimeopen"
                                            "xdg-open"))))
    (secretary--run "Rscript" script secretary-ledger-file-name sheet)
    (secretary--run-async app sheet)))

(defcustom secretary-ledger-file-name
  "/home/kept/Journal/Finances/2021.ledger"
  "Ledger file to read or visit; we will never modify it."
  :group 'secretary
  :type 'string)

;; WIP
(defun secretary-make-ods ()
  "Make an ODS spreadsheet of variables for you to play with."
  (interactive))


(provide 'secretary-mothball)

;;; secretary-mothball.el ends here
