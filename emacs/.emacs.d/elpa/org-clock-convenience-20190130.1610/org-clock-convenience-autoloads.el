;;; org-clock-convenience-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-clock-convenience" "org-clock-convenience.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-clock-convenience.el

(autoload 'org-clock-convenience-forward-log-line "org-clock-convenience" "\
Move cursor to the next agenda log line.

\(fn)" t nil)

(autoload 'org-clock-convenience-backward-log-line "org-clock-convenience" "\
Move cursor to the previous agenda log line.

\(fn)" t nil)

(autoload 'org-clock-convenience-goto-ts "org-clock-convenience" "\
From agenda log line goto to corresponding timestamp position in org file.

Goto to position inside of the timestamp in the agenda file corresponding
to the current position of point in the agenda log line.

\(fn)" t nil)

(autoload 'org-clock-convenience-timestamp-up "org-clock-convenience" "\
Increase the date item at the cursor by one.
Used in a clocked line from the agenda view.  If the cursor is on
the hour field, change the hour.  If it is on the minutes field,
change the minutes.  With prefix ARG, change by that many units.

\(fn &optional ARG)" t nil)

(autoload 'org-clock-convenience-timestamp-down "org-clock-convenience" "\
Increase the date item at the cursor by one.
Used in a clocked line from the agenda view.  If the cursor is on
the hour field, change the hour.  If it is on the minutes field,
change the minutes.  With prefix ARG, change by that many units.

\(fn &optional ARG)" t nil)

(autoload 'org-clock-convenience-fill-gap "org-clock-convenience" "\
Modify timestamp at cursor to connect to previous/next timerange.
Used from the agenda buffer by placing point on a log line of a
clocked entry.  If point is on the start time, the start time will
be modified to connect to the end time of the previous clocked
task.  If works accordingly if point is on the end time of the
current log entry.  If there is no newer logged clock line, the
end time will be set to the current time.

For performance reasons the previous/next clock item is found
based on a search for the previous/next clocked log line in the
agenda buffer, so it can only connect to a time range visible in
the current agenda buffer.

\(fn)" t nil)

(autoload 'org-clock-convenience-fill-gap-both "org-clock-convenience" "\
Modify both timestamps at cursor to fill gap to last/next timerange.
Performs `org-clock-convenience-fill-gap' sequentially on the
starting time and the ending of the time range. Can be executed
from anywhere within a valid clocked time range line.

\(fn)" t nil)

(autoload 'org-clock-convenience-goto-last-clockout "org-clock-convenience" "\
Jump to the position of the last clockout in BUFFER.

\(fn &optional BUFFER)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-clock-convenience" '("org-clock-convenience-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-clock-convenience-autoloads.el ends here
