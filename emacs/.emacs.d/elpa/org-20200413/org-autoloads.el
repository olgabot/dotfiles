;;; org-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ol" "ol.el" (24215 26300 498281 457000))
;;; Generated autoloads from ol.el

(autoload 'org-next-link "ol" "\
Move forward to the next link.
If the link is in hidden text, expose it.  When SEARCH-BACKWARD
is non-nil, move backward.

\(fn &optional SEARCH-BACKWARD)" t nil)

(autoload 'org-previous-link "ol" "\
Move backward to the previous link.
If the link is in hidden text, expose it.

\(fn)" t nil)

(autoload 'org-toggle-link-display "ol" "\
Toggle the literal or descriptive display of links.

\(fn)" t nil)

(autoload 'org-store-link "ol" "\
Store a link to the current location.
\\<org-mode-map>
This link is added to `org-stored-links' and can later be inserted
into an Org buffer with `org-insert-link' (`\\[org-insert-link]').

For some link types, a `\\[universal-argument]' prefix ARG is interpreted.  A single
`\\[universal-argument]' negates `org-context-in-file-links' for file links or
`org-gnus-prefer-web-links' for links to Usenet articles.

A `\\[universal-argument] \\[universal-argument]' prefix ARG forces skipping storing functions that are not
part of Org core.

A `\\[universal-argument] \\[universal-argument] \\[universal-argument]' prefix ARG forces storing a link for each line in the
active region.

Assume the function is called interactively if INTERACTIVE? is
non-nil.

\(fn ARG &optional INTERACTIVE\\=\\?)" t nil)

(autoload 'org-insert-link "ol" "\
Insert a link.  At the prompt, enter the link.

Completion can be used to insert any of the link protocol prefixes in use.

The history can be used to select a link previously stored with
`org-store-link'.  When the empty string is entered (i.e. if you just
press `RET' at the prompt), the link defaults to the most recently
stored link.  As `SPC' triggers completion in the minibuffer, you need to
use `M-SPC' or `C-q SPC' to force the insertion of a space character.

You will also be prompted for a description, and if one is given, it will
be displayed in the buffer instead of the link.

If there is already a link at point, this command will allow you to edit
link and description parts.

With a `\\[universal-argument]' prefix, prompts for a file to link to.  The file name can be
selected using completion.  The path to the file will be relative to the
current directory if the file is in the current directory or a subdirectory.
Otherwise, the link will be the absolute path as completed in the minibuffer
\(i.e. normally ~/path/to/file).  You can configure this behavior using the
option `org-link-file-path-type'.

With a `\\[universal-argument] \\[universal-argument]' prefix, enforce an absolute path even if the file is in
the current directory or below.

A `\\[universal-argument] \\[universal-argument] \\[universal-argument]' prefix negates `org-link-keep-stored-after-insertion'.

If the LINK-LOCATION parameter is non-nil, this value will be used as
the link location instead of reading one interactively.

If the DESCRIPTION parameter is non-nil, this value will be used as the
default description.  Otherwise, if `org-link-make-description-function'
is non-nil, this function will be called with the link target, and the
result will be the default link description.  When called non-interactively,
don't allow to edit the default description.

\(fn &optional COMPLETE-FILE LINK-LOCATION DESCRIPTION)" t nil)

(autoload 'org-insert-all-links "ol" "\
Insert all links in `org-stored-links'.
When a universal prefix, do not delete the links from `org-stored-links'.
When `ARG' is a number, insert the last N link(s).
`PRE' and `POST' are optional arguments to define a string to
prepend or to append.

\(fn ARG &optional PRE POST)" t nil)

(autoload 'org-insert-last-stored-link "ol" "\
Insert the last link stored in `org-stored-links'.

\(fn ARG)" t nil)

(autoload 'org-insert-link-global "ol" "\
Insert a link like Org mode does.
This command can be called in any mode to insert a link in Org syntax.

\(fn)" t nil)

(autoload 'org-update-radio-target-regexp "ol" "\
Find all radio targets in this file and update the regular expression.
Also refresh fontification if needed.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "org" "org.el" (24215 26300 558457 275000))
;;; Generated autoloads from org.el

(autoload 'org-babel-do-load-languages "org" "\
Load the languages defined in `org-babel-load-languages'.

\(fn SYM VALUE)" nil nil)

(autoload 'org-babel-load-file "org" "\
Load Emacs Lisp source code blocks in the Org FILE.
This function exports the source code using `org-babel-tangle'
and then loads the resulting file using `load-file'.  With
optional prefix argument COMPILE, the tangled Emacs Lisp file is
byte-compiled before it is loaded.

\(fn FILE &optional COMPILE)" t nil)

(autoload 'org-version "org" "\
Show the Org version.
Interactively, or when MESSAGE is non-nil, show it in echo area.
With prefix argument, or when HERE is non-nil, insert it at point.
In non-interactive uses, a reduced version string is output unless
FULL is given.

\(fn &optional HERE FULL MESSAGE)" t nil)

(autoload 'org-clock-persistence-insinuate "org" "\
Set up hooks for clock persistence.

\(fn)" nil nil)

(autoload 'org-mode "org" "\
Outline-based notes management and organizer, alias
\"Carsten's outline-mode for keeping track of everything.\"

Org mode develops organizational tasks around a NOTES file which
contains information about projects as plain text.  Org mode is
implemented on top of Outline mode, which is ideal to keep the content
of large files well structured.  It supports ToDo items, deadlines and
time stamps, which magically appear in the diary listing of the Emacs
calendar.  Tables are easily created with a built-in table editor.
Plain text URL-like links connect to websites, emails (VM), Usenet
messages (Gnus), BBDB entries, and any files related to the project.
For printing and sharing of notes, an Org file (or a part of it)
can be exported as a structured ASCII or HTML file.

The following commands are available:

\\{org-mode-map}

\(fn)" t nil)

(autoload 'org-cycle "org" "\
TAB-action and visibility cycling for Org mode.

This is the command invoked in Org mode by the `TAB' key.  Its main
purpose is outline visibility cycling, but it also invokes other actions
in special contexts.

When this function is called with a `\\[universal-argument]' prefix, rotate the entire
buffer through 3 states (global cycling)
  1. OVERVIEW: Show only top-level headlines.
  2. CONTENTS: Show all headlines of all levels, but no body text.
  3. SHOW ALL: Show everything.

With a `\\[universal-argument] \\[universal-argument]' prefix argument, switch to the startup visibility,
determined by the variable `org-startup-folded', and by any VISIBILITY
properties in the buffer.

With a `\\[universal-argument] \\[universal-argument] \\[universal-argument]' prefix argument, show the entire buffer, including
any drawers.

When inside a table, re-align the table and move to the next field.

When point is at the beginning of a headline, rotate the subtree started
by this line through 3 different states (local cycling)
  1. FOLDED:   Only the main headline is shown.
  2. CHILDREN: The main headline and the direct children are shown.
               From this state, you can move to one of the children
               and zoom in further.
  3. SUBTREE:  Show the entire subtree, including body text.
If there is no subtree, switch directly from CHILDREN to FOLDED.

When point is at the beginning of an empty headline and the variable
`org-cycle-level-after-item/entry-creation' is set, cycle the level
of the headline by demoting and promoting it to likely levels.  This
speeds up creation document structure by pressing `TAB' once or several
times right after creating a new headline.

When there is a numeric prefix, go up to a heading with level ARG, do
a `show-subtree' and return to the previous cursor position.  If ARG
is negative, go up that many levels.

When point is not at the beginning of a headline, execute the global
binding for `TAB', which is re-indenting the line.  See the option
`org-cycle-emulate-tab' for details.

As a special case, if point is at the beginning of the buffer and there is
no headline in line 1, this function will act as if called with prefix arg
\(`\\[universal-argument] TAB', same as `S-TAB') also when called without prefix arg, but only
if the variable `org-cycle-global-at-bob' is t.

\(fn &optional ARG)" t nil)

(autoload 'org-global-cycle "org" "\
Cycle the global visibility.  For details see `org-cycle'.
With `\\[universal-argument]' prefix ARG, switch to startup visibility.
With a numeric prefix, show all headlines up to that level.

\(fn &optional ARG)" t nil)

(autoload 'org-run-like-in-org-mode "org" "\
Run a command, pretending that the current buffer is in Org mode.
This will temporarily bind local variables that are typically bound in
Org mode to the values they have in Org mode, and then interactively
call CMD.

\(fn CMD)" nil nil)

(autoload 'org-open-file "org" "\
Open the file at PATH.
First, this expands any special file name abbreviations.  Then the
configuration variable `org-file-apps' is checked if it contains an
entry for this file type, and if yes, the corresponding command is launched.

If no application is found, Emacs simply visits the file.

With optional prefix argument IN-EMACS, Emacs will visit the file.
With a double \\[universal-argument] \\[universal-argument] prefix arg, Org tries to avoid opening in Emacs
and to use an external application to visit the file.

Optional LINE specifies a line to go to, optional SEARCH a string
to search for.  If LINE or SEARCH is given, the file will be
opened in Emacs, unless an entry from `org-file-apps' that makes
use of groups in a regexp matches.

If you want to change the way frames are used when following a
link, please customize `org-link-frame-setup'.

If the file does not exist, throw an error.

\(fn PATH &optional IN-EMACS LINE SEARCH)" nil nil)

(autoload 'org-open-at-point-global "org" "\
Follow a link or a time-stamp like Org mode does.
Also follow links and emails as seen by `thing-at-point'.
This command can be called in any mode to follow an external
link or a time-stamp that has Org mode syntax.  Its behavior
is undefined when called on internal links like fuzzy links.
Raise a user error when there is nothing to follow.

\(fn)" t nil)

(autoload 'org-offer-links-in-entry "org" "\
Offer links in the current entry and return the selected link.
If there is only one link, return it.
If NTH is an integer, return the NTH link found.
If ZERO is a string, check also this string for a link, and if
there is one, return it.

\(fn BUFFER MARKER &optional NTH ZERO)" nil nil)

(autoload 'org-switchb "org" "\
Switch between Org buffers.

With `\\[universal-argument]' prefix, restrict available buffers to files.

With `\\[universal-argument] \\[universal-argument]' prefix, restrict available buffers to agenda files.

\(fn &optional ARG)" t nil)

(autoload 'org-cycle-agenda-files "org" "\
Cycle through the files in `org-agenda-files'.
If the current buffer visits an agenda file, find the next one in the list.
If the current buffer does not, find the first agenda file.

\(fn)" t nil)

(autoload 'org-submit-bug-report "org" "\
Submit a bug report on Org via mail.

Don't hesitate to report any problems or inaccurate documentation.

If you don't have setup sending mail from (X)Emacs, please copy the
output buffer into your mail program, as it gives us important
information about your Org version and configuration.

\(fn)" t nil)

(autoload 'org-reload "org" "\
Reload all Org Lisp files.
With prefix arg UNCOMPILED, load the uncompiled versions.

\(fn &optional UNCOMPILED)" t nil)

(autoload 'org-customize "org" "\
Call the customize function with org as argument.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "org-agenda" "org-agenda.el" (24215 26300 499883
;;;;;;  462000))
;;; Generated autoloads from org-agenda.el

(autoload 'org-toggle-sticky-agenda "org-agenda" "\
Toggle `org-agenda-sticky'.

\(fn &optional ARG)" t nil)

(autoload 'org-agenda "org-agenda" "\
Dispatch agenda commands to collect entries to the agenda buffer.
Prompts for a command to execute.  Any prefix arg will be passed
on to the selected command.  The default selections are:

a     Call `org-agenda-list' to display the agenda for current day or week.
t     Call `org-todo-list' to display the global todo list.
T     Call `org-todo-list' to display the global todo list, select only
      entries with a specific TODO keyword (the user gets a prompt).
m     Call `org-tags-view' to display headlines with tags matching
      a condition  (the user is prompted for the condition).
M     Like `m', but select only TODO entries, no ordinary headlines.
e     Export views to associated files.
s     Search entries for keywords.
S     Search entries for keywords, only with TODO keywords.
/     Multi occur across all agenda files and also files listed
      in `org-agenda-text-search-extra-files'.
<     Restrict agenda commands to buffer, subtree, or region.
      Press several times to get the desired effect.
>     Remove a previous restriction.
#     List \"stuck\" projects.
!     Configure what \"stuck\" means.
C     Configure custom agenda commands.

More commands can be added by configuring the variable
`org-agenda-custom-commands'.  In particular, specific tags and TODO keyword
searches can be pre-defined in this way.

If the current buffer is in Org mode and visiting a file, you can also
first press `<' once to indicate that the agenda should be temporarily
\(until the next use of `\\[org-agenda]') restricted to the current file.
Pressing `<' twice means to restrict to the current subtree or region
\(if active).

\(fn &optional ARG ORG-KEYS RESTRICTION)" t nil)

(autoload 'org-batch-agenda "org-agenda" "\
Run an agenda command in batch mode and send the result to STDOUT.
If CMD-KEY is a string of length 1, it is used as a key in
`org-agenda-custom-commands' and triggers this command.  If it is a
longer string it is used as a tags/todo match string.
Parameters are alternating variable names and values that will be bound
before running the agenda command.

\(fn CMD-KEY &rest PARAMETERS)" nil t)

(autoload 'org-batch-agenda-csv "org-agenda" "\
Run an agenda command in batch mode and send the result to STDOUT.
If CMD-KEY is a string of length 1, it is used as a key in
`org-agenda-custom-commands' and triggers this command.  If it is a
longer string it is used as a tags/todo match string.
Parameters are alternating variable names and values that will be bound
before running the agenda command.

The output gives a line for each selected agenda item.  Each
item is a list of comma-separated values, like this:

category,head,type,todo,tags,date,time,extra,priority-l,priority-n

category     The category of the item
head         The headline, without TODO kwd, TAGS and PRIORITY
type         The type of the agenda entry, can be
                todo               selected in TODO match
                tagsmatch          selected in tags match
                diary              imported from diary
                deadline           a deadline on given date
                scheduled          scheduled on given date
                timestamp          entry has timestamp on given date
                closed             entry was closed on given date
                upcoming-deadline  warning about deadline
                past-scheduled     forwarded scheduled item
                block              entry has date block including g. date
todo         The todo keyword, if any
tags         All tags including inherited ones, separated by colons
date         The relevant date, like 2007-2-14
time         The time, like 15:00-16:50
extra        String with extra planning info
priority-l   The priority letter if any was given
priority-n   The computed numerical priority
agenda-day   The day in the agenda where this is listed

\(fn CMD-KEY &rest PARAMETERS)" nil t)

(autoload 'org-store-agenda-views "org-agenda" "\
Store agenda views.

\(fn &rest PARAMETERS)" t nil)

(autoload 'org-batch-store-agenda-views "org-agenda" "\
Run all custom agenda commands that have a file argument.

\(fn &rest PARAMETERS)" nil t)

(autoload 'org-agenda-list "org-agenda" "\
Produce a daily/weekly view from all files in variable `org-agenda-files'.
The view will be for the current day or week, but from the overview buffer
you will be able to go to other days/weeks.

With a numeric prefix argument in an interactive call, the agenda will
span ARG days.  Lisp programs should instead specify SPAN to change
the number of days.  SPAN defaults to `org-agenda-span'.

START-DAY defaults to TODAY, or to the most recent match for the weekday
given in `org-agenda-start-on-weekday'.

When WITH-HOUR is non-nil, only include scheduled and deadline
items if they have an hour specification like [h]h:mm.

\(fn &optional ARG START-DAY SPAN WITH-HOUR)" t nil)

(autoload 'org-search-view "org-agenda" "\
Show all entries that contain a phrase or words or regular expressions.

With optional prefix argument TODO-ONLY, only consider entries that are
TODO entries.  The argument STRING can be used to pass a default search
string into this function.  If EDIT-AT is non-nil, it means that the
user should get a chance to edit this string, with cursor at position
EDIT-AT.

The search string can be viewed either as a phrase that should be found as
is, or it can be broken into a number of snippets, each of which must match
in a Boolean way to select an entry.  The default depends on the variable
`org-agenda-search-view-always-boolean'.
Even if this is turned off (the default) you can always switch to
Boolean search dynamically by preceding the first word with  \"+\" or \"-\".

The default is a direct search of the whole phrase, where each space in
the search string can expand to an arbitrary amount of whitespace,
including newlines.

If using a Boolean search, the search string is split on whitespace and
each snippet is searched separately, with logical AND to select an entry.
Words prefixed with a minus must *not* occur in the entry.  Words without
a prefix or prefixed with a plus must occur in the entry.  Matching is
case-insensitive.  Words are enclosed by word delimiters (i.e. they must
match whole words, not parts of a word) if
`org-agenda-search-view-force-full-words' is set (default is nil).

Boolean search snippets enclosed by curly braces are interpreted as
regular expressions that must or (when preceded with \"-\") must not
match in the entry.  Snippets enclosed into double quotes will be taken
as a whole, to include whitespace.

- If the search string starts with an asterisk, search only in headlines.
- If (possibly after the leading star) the search string starts with an
  exclamation mark, this also means to look at TODO entries only, an effect
  that can also be achieved with a prefix argument.
- If (possibly after star and exclamation mark) the search string starts
  with a colon, this will mean that the (non-regexp) snippets of the
  Boolean search must match as full words.

This command searches the agenda files, and in addition the files
listed in `org-agenda-text-search-extra-files' unless a restriction lock
is active.

\(fn &optional TODO-ONLY STRING EDIT-AT)" t nil)

(autoload 'org-todo-list "org-agenda" "\
Show all (not done) TODO entries from all agenda file in a single list.
The prefix arg can be used to select a specific TODO keyword and limit
the list to these.  When using `\\[universal-argument]', you will be prompted
for a keyword.  A numeric prefix directly selects the Nth keyword in
`org-todo-keywords-1'.

\(fn &optional ARG)" t nil)

(autoload 'org-tags-view "org-agenda" "\
Show all headlines for all `org-agenda-files' matching a TAGS criterion.
The prefix arg TODO-ONLY limits the search to TODO entries.

\(fn &optional TODO-ONLY MATCH)" t nil)

(autoload 'org-agenda-list-stuck-projects "org-agenda" "\
Create agenda view for projects that are stuck.
Stuck projects are project that have no next actions.  For the definitions
of what a project is and how to check if it stuck, customize the variable
`org-stuck-projects'.

\(fn &rest IGNORE)" t nil)

(autoload 'org-diary "org-agenda" "\
Return diary information from org files.
This function can be used in a \"sexp\" diary entry in the Emacs calendar.
It accesses org files and extracts information from those files to be
listed in the diary.  The function accepts arguments specifying what
items should be listed.  For a list of arguments allowed here, see the
variable `org-agenda-entry-types'.

The call in the diary file should look like this:

   &%%(org-diary) ~/path/to/some/orgfile.org

Use a separate line for each org file to check.  Or, if you omit the file name,
all files listed in `org-agenda-files' will be checked automatically:

   &%%(org-diary)

If you don't give any arguments (as in the example above), the default value
of `org-agenda-entry-types' is used: (:deadline :scheduled :timestamp :sexp).
So the example above may also be written as

   &%%(org-diary :deadline :timestamp :sexp :scheduled)

The function expects the lisp variables `entry' and `date' to be provided
by the caller, because this is how the calendar works.  Don't use this
function from a program - use `org-agenda-get-day-entries' instead.

\(fn &rest ARGS)" nil nil)

(autoload 'org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item "org-agenda" "\
Do we have a reason to ignore this TODO entry because it has a time stamp?

\(fn &optional END)" nil nil)

(autoload 'org-agenda-set-restriction-lock "org-agenda" "\
Set restriction lock for agenda to current subtree or file.
When in a restricted subtree, remove it.

The restriction will span over the entire file if TYPE is `file',
or if type is '(4), or if the cursor is before the first headline
in the file. Otherwise, only apply the restriction to the current
subtree.

\(fn &optional TYPE)" t nil)

(autoload 'org-calendar-goto-agenda "org-agenda" "\
Compute the Org agenda for the calendar date displayed at the cursor.
This is a command that has to be installed in `calendar-mode-map'.

\(fn)" t nil)

(autoload 'org-agenda-to-appt "org-agenda" "\
Activate appointments found in `org-agenda-files'.

With a `\\[universal-argument]' prefix, refresh the list of appointments.

If FILTER is t, interactively prompt the user for a regular
expression, and filter out entries that don't match it.

If FILTER is a string, use this string as a regular expression
for filtering entries out.

If FILTER is a function, filter out entries against which
calling the function returns nil.  This function takes one
argument: an entry from `org-agenda-get-day-entries'.

FILTER can also be an alist with the car of each cell being
either `headline' or `category'.  For example:

  \\='((headline \"IMPORTANT\")
    (category \"Work\"))

will only add headlines containing IMPORTANT or headlines
belonging to the \"Work\" category.

ARGS are symbols indicating what kind of entries to consider.
By default `org-agenda-to-appt' will use :deadline*, :scheduled*
\(i.e., deadlines and scheduled items with a hh:mm specification)
and :timestamp entries.  See the docstring of `org-diary' for
details and examples.

If an entry has a APPT_WARNTIME property, its value will be used
to override `appt-message-warning-time'.

\(fn &optional REFRESH FILTER &rest ARGS)" t nil)

;;;***

;;;### (autoloads nil "org-capture" "org-capture.el" (24215 26300
;;;;;;  505214 24000))
;;; Generated autoloads from org-capture.el

(autoload 'org-capture-string "org-capture" "\
Capture STRING with the template selected by KEYS.

\(fn STRING &optional KEYS)" t nil)

(autoload 'org-capture "org-capture" "\
Capture something.
\\<org-capture-mode-map>
This will let you select a template from `org-capture-templates', and
then file the newly captured information.  The text is immediately
inserted at the target location, and an indirect buffer is shown where
you can edit it.  Pressing `\\[org-capture-finalize]' brings you back to the previous
state of Emacs, so that you can continue your work.

When called interactively with a `\\[universal-argument]' prefix argument GOTO, don't
capture anything, just go to the file/headline where the selected
template stores its notes.

With a `\\[universal-argument] \\[universal-argument]' prefix argument, go to the last note stored.

When called with a `C-0' (zero) prefix, insert a template at point.

When called with a `C-1' (one) prefix, force prompting for a date when
a datetree entry is made.

ELisp programs can set KEYS to a string associated with a template
in `org-capture-templates'.  In this case, interactive selection
will be bypassed.

If `org-capture-use-agenda-date' is non-nil, capturing from the
agenda will use the date at point as the default date.  Then, a
`C-1' prefix will tell the capture process to use the HH:MM time
of the day at point (if any) or the current HH:MM time.

\(fn &optional GOTO KEYS)" t nil)

(autoload 'org-capture-import-remember-templates "org-capture" "\
Set `org-capture-templates' to be similar to `org-remember-templates'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "org-colview" "org-colview.el" (24215 26300
;;;;;;  508295 933000))
;;; Generated autoloads from org-colview.el

(autoload 'org-columns-remove-overlays "org-colview" "\
Remove all currently active column overlays.

\(fn)" t nil)

(autoload 'org-columns-get-format-and-top-level "org-colview" "\


\(fn)" nil nil)

(autoload 'org-columns "org-colview" "\
Turn on column view on an Org mode file.

Column view applies to the whole buffer if point is before the
first headline.  Otherwise, it applies to the first ancestor
setting \"COLUMNS\" property.  If there is none, it defaults to
the current headline.  With a `\\[universal-argument]' prefix argument, turn on column
view for the whole buffer unconditionally.

When COLUMNS-FMT-STRING is non-nil, use it as the column format.

\(fn &optional GLOBAL COLUMNS-FMT-STRING)" t nil)

(autoload 'org-columns-compute "org-colview" "\
Summarize the values of PROPERTY hierarchically.
Also update existing values for PROPERTY according to the first
column specification.

\(fn PROPERTY)" t nil)

(autoload 'org-dblock-write:columnview "org-colview" "\
Write the column view table.

PARAMS is a property list of parameters:

`:id' (mandatory)

    The ID property of the entry where the columns view should be
    built.  When the symbol `local', call locally.  When `global'
    call column view with the cursor at the beginning of the
    buffer (usually this means that the whole buffer switches to
    column view).  When \"file:path/to/file.org\", invoke column
    view at the start of that file.  Otherwise, the ID is located
    using `org-id-find'.

`:exclude-tags'

    List of tags to exclude from column view table.

`:format'

    When non-nil, specify the column view format to use.

`:hlines'

    When non-nil, insert a hline before each item.  When
    a number, insert a hline before each level inferior or equal
    to that number.

`:indent'

    When non-nil, indent each ITEM field according to its level.

`:match'

    When set to a string, use this as a tags/property match filter.

`:maxlevel'

    When set to a number, don't capture headlines below this level.

`:skip-empty-rows'

    When non-nil, skip rows where all specifiers other than ITEM
    are empty.

`:vlines'

    When non-nil, make each column a column group to enforce
    vertical lines.

\(fn PARAMS)" nil nil)

(autoload 'org-columns-insert-dblock "org-colview" "\
Create a dynamic block capturing a column view table.

\(fn)" t nil)

(autoload 'org-agenda-columns "org-colview" "\
Turn on or update column view in the agenda.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "org-compat" "org-compat.el" (24215 26300 510176
;;;;;;  442000))
;;; Generated autoloads from org-compat.el

(autoload 'org-check-version "org-compat" "\
Try very hard to provide sensible version strings.

\(fn)" nil t)

;;;***

;;;### (autoloads nil "org-duration" "org-duration.el" (24215 26300
;;;;;;  515543 885000))
;;; Generated autoloads from org-duration.el

(autoload 'org-duration-set-regexps "org-duration" "\
Set duration related regexps.

\(fn)" t nil)

(autoload 'org-duration-p "org-duration" "\
Non-nil when string S is a time duration.

\(fn S)" nil nil)

(autoload 'org-duration-to-minutes "org-duration" "\
Return number of minutes of DURATION string.

When optional argument CANONICAL is non-nil, ignore
`org-duration-units' and use standard time units value.

A bare number is translated into minutes.  The empty string is
translated into 0.0.

Return value as a float.  Raise an error if duration format is
not recognized.

\(fn DURATION &optional CANONICAL)" nil nil)

(autoload 'org-duration-from-minutes "org-duration" "\
Return duration string for a given number of MINUTES.

Format duration according to `org-duration-format' or FMT, when
non-nil.

When optional argument CANONICAL is non-nil, ignore
`org-duration-units' and use standard time units value.

Raise an error if expected format is unknown.

\(fn MINUTES &optional FMT CANONICAL)" nil nil)

(autoload 'org-duration-h:mm-only-p "org-duration" "\
Non-nil when every duration in TIMES has \"H:MM\" or \"H:MM:SS\" format.

TIMES is a list of duration strings.

Return nil if any duration is expressed with units, as defined in
`org-duration-units'.  Otherwise, if any duration is expressed
with \"H:MM:SS\" format, return `h:mm:ss'.  Otherwise, return
`h:mm'.

\(fn TIMES)" nil nil)

;;;***

;;;### (autoloads nil "org-goto" "org-goto.el" (24215 26300 528196
;;;;;;  111000))
;;; Generated autoloads from org-goto.el

(autoload 'org-goto-location "org-goto" "\
Let the user select a location in current buffer.
This function uses a recursive edit.  It returns the selected
position or nil.

\(fn &optional BUF HELP)" nil nil)

(autoload 'org-goto "org-goto" "\
Look up a different location in the current file, keeping current visibility.

When you want look-up or go to a different location in a
document, the fastest way is often to fold the entire buffer and
then dive into the tree.  This method has the disadvantage, that
the previous location will be folded, which may not be what you
want.

This command works around this by showing a copy of the current
buffer in an indirect buffer, in overview mode.  You can dive
into the tree in that copy, use org-occur and incremental search
to find a location.  When pressing RET or `Q', the command
returns to the original buffer in which the visibility is still
unchanged.  After RET it will also jump to the location selected
in the indirect buffer and expose the headline hierarchy above.

With a prefix argument, use the alternative interface: e.g., if
`org-goto-interface' is `outline' use `outline-path-completion'.

\(fn &optional ALTERNATIVE-INTERFACE)" t nil)

;;;***

;;;### (autoloads nil "org-keys" "org-keys.el" (24215 26300 535883
;;;;;;  588000))
;;; Generated autoloads from org-keys.el

(autoload 'org-babel-describe-bindings "org-keys" "\
Describe all keybindings behind `org-babel-key-prefix'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "org-lint" "org-lint.el" (24215 26300 537173
;;;;;;  976000))
;;; Generated autoloads from org-lint.el

(autoload 'org-lint "org-lint" "\
Check current Org buffer for syntax mistakes.

By default, run all checkers.  With a `\\[universal-argument]' prefix ARG, select one
category of checkers only.  With a `\\[universal-argument] \\[universal-argument]' prefix, run one precise
checker by its name.

ARG can also be a list of checker names, as symbols, to run.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "org-macs" "org-macs.el" (24215 26300 542429
;;;;;;  999000))
;;; Generated autoloads from org-macs.el

(autoload 'org-load-noerror-mustsuffix "org-macs" "\
Load FILE with optional arguments NOERROR and MUSTSUFFIX.

\(fn FILE)" nil t)

;;;***

;;;### (autoloads nil "org-num" "org-num.el" (24215 26300 546309
;;;;;;  455000))
;;; Generated autoloads from org-num.el

(autoload 'org-num-default-format "org-num" "\
Default numbering display function.
NUMBERING is a list of numbers.

\(fn NUMBERING)" nil nil)

(autoload 'org-num-mode "org-num" "\
Dynamic numbering of headlines in an Org buffer.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "org-version" "org-version.el" (24215 26300
;;;;;;  556974 106000))
;;; Generated autoloads from org-version.el

(autoload 'org-release "org-version" "\
The release version of Org.
Inserted by installing Org mode or when a release is made.

\(fn)" nil nil)

(autoload 'org-git-version "org-version" "\
The Git version of Org mode.
Inserted by installing Org or when a release is made.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("ob-C.el" "ob-J.el" "ob-R.el" "ob-abc.el"
;;;;;;  "ob-asymptote.el" "ob-awk.el" "ob-calc.el" "ob-clojure.el"
;;;;;;  "ob-comint.el" "ob-coq.el" "ob-core.el" "ob-css.el" "ob-ditaa.el"
;;;;;;  "ob-dot.el" "ob-ebnf.el" "ob-emacs-lisp.el" "ob-eshell.el"
;;;;;;  "ob-eval.el" "ob-exp.el" "ob-forth.el" "ob-fortran.el" "ob-gnuplot.el"
;;;;;;  "ob-groovy.el" "ob-haskell.el" "ob-hledger.el" "ob-io.el"
;;;;;;  "ob-java.el" "ob-js.el" "ob-latex.el" "ob-ledger.el" "ob-lilypond.el"
;;;;;;  "ob-lisp.el" "ob-lob.el" "ob-lua.el" "ob-makefile.el" "ob-matlab.el"
;;;;;;  "ob-maxima.el" "ob-mscgen.el" "ob-ocaml.el" "ob-octave.el"
;;;;;;  "ob-org.el" "ob-perl.el" "ob-picolisp.el" "ob-plantuml.el"
;;;;;;  "ob-processing.el" "ob-python.el" "ob-ref.el" "ob-ruby.el"
;;;;;;  "ob-sass.el" "ob-scheme.el" "ob-screen.el" "ob-sed.el" "ob-shell.el"
;;;;;;  "ob-shen.el" "ob-sql.el" "ob-sqlite.el" "ob-stan.el" "ob-table.el"
;;;;;;  "ob-tangle.el" "ob-vala.el" "ob.el" "ol-bbdb.el" "ol-bibtex.el"
;;;;;;  "ol-docview.el" "ol-eshell.el" "ol-eww.el" "ol-gnus.el" "ol-info.el"
;;;;;;  "ol-irc.el" "ol-mhe.el" "ol-rmail.el" "ol-w3m.el" "org-archive.el"
;;;;;;  "org-attach-git.el" "org-attach.el" "org-clock.el" "org-crypt.el"
;;;;;;  "org-ctags.el" "org-datetree.el" "org-element.el" "org-entities.el"
;;;;;;  "org-faces.el" "org-feed.el" "org-footnote.el" "org-habit.el"
;;;;;;  "org-id.el" "org-indent.el" "org-inlinetask.el" "org-install.el"
;;;;;;  "org-list.el" "org-loaddefs.el" "org-macro.el" "org-mobile.el"
;;;;;;  "org-mouse.el" "org-pcomplete.el" "org-pkg.el" "org-plot.el"
;;;;;;  "org-protocol.el" "org-src.el" "org-table.el" "org-tempo.el"
;;;;;;  "org-timer.el" "ox-ascii.el" "ox-beamer.el" "ox-html.el"
;;;;;;  "ox-icalendar.el" "ox-latex.el" "ox-man.el" "ox-md.el" "ox-odt.el"
;;;;;;  "ox-org.el" "ox-publish.el" "ox-texinfo.el" "ox.el") (24215
;;;;;;  26300 588244 138000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; org-autoloads.el ends here
