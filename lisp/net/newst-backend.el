;;; newst-backend.el --- Retrieval backend for newsticker  -*- lexical-binding:t -*-

;; Copyright (C) 2003-2024 Free Software Foundation, Inc.

;; Author:      Ulf Jasper <ulf.jasper@web.de>
;; Filename:    newst-backend.el
;; URL:         https://www.nongnu.org/newsticker
;; Keywords:    News, RSS, Atom
;; Package:     newsticker

;; ======================================================================

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;; ======================================================================

;;; Commentary:

;; See newsticker.el

;; ======================================================================
;;; Code:

(require 'xml)
(require 'url-parse)
(require 'iso8601)

;; Silence warnings
(defvar newsticker-groups)

(defvar newsticker--retrieval-timer-list nil
  "List of timers for news retrieval.
This is an alist, each element consisting of (feed-name . timer).")

(defvar newsticker--sentinel-callback nil
  "Function called at end of `newsticker--sentinel'.")

;;;###autoload
(defun newsticker-running-p ()
  "Check whether newsticker is running.
Return t if newsticker is running, nil otherwise.  Newsticker is
considered to be running if the newsticker timer list is not empty."
  (> (length newsticker--retrieval-timer-list) 0))

;; ======================================================================
;;; Customization
;; ======================================================================
(defgroup newsticker nil
  "Aggregator for RSS and Atom feeds."
  :group 'applications)

;; Hard-coding URLs like this is a recipe for propagating obsolete info.
(defconst newsticker--raw-url-list-defaults
  '(("Debian Security Advisories"
     "https://www.debian.org/security/dsa.en.rdf")
    ("Debian Security Advisories - Long format"
     "https://www.debian.org/security/dsa-long.en.rdf")
    ("Emacs Wiki"
     "https://www.emacswiki.org/emacs?action=rss"
     nil
     3600)
    ("LWN (Linux Weekly News)"
     "https://lwn.net/headlines/rss")
    ("Quote of the day"
     "https://feeds.feedburner.com/quotationspage/qotd"
     "07:00"
     86400)
    ("The Register"
     "https://www.theregister.co.uk/headlines.rss")
    ("slashdot"
     "https://rss.slashdot.org/Slashdot/slashdot"
     nil
     3600)                        ;/. will ban you if under 3600 seconds!
    ("Wired News"
     "https://www.wired.com/feed/rss")
    ("Heise News (german)"
     "https://www.heise.de/newsticker/heise.rdf")
    ("Tagesschau (german)"
     "https://www.tagesschau.de/newsticker.rdf"
     nil
     1800))
  "Default URL list in raw form.
This list is fed into defcustom via `newsticker--splicer'.")

(defun newsticker--splicer (item)
  "Convert ITEM for splicing into `newsticker-url-list-defaults'."
  (let ((result (list 'list :tag (nth 0 item) (list 'const (nth 0 item))))
        (element (cdr item)))
    (while element
      (setq result (append result (list (list 'const (car element)))))
      (setq element (cdr element)))
    result))

(defun newsticker--set-customvar-retrieval (symbol value)
  "Set retrieval related newsticker-variable SYMBOL value to VALUE.
Calls all actions which are necessary in order to make the new
value effective."
  (if (or (not (boundp symbol))
          (equal (symbol-value symbol) value))
      (set symbol value)
    ;; something must have changed
    (let ((need-restart nil)
          (new-or-changed-feeds nil)
          (removed-feeds))
      (cond ((eq symbol 'newsticker-retrieval-interval)
             (setq need-restart t))
            ((memq symbol '(newsticker-url-list-defaults newsticker-url-list))
             (dolist (elt value)
               (unless (member elt (symbol-value symbol))
                 (setq new-or-changed-feeds (cons elt new-or-changed-feeds))))
             (dolist (elt (symbol-value symbol))
               (unless (member elt value)
                 (setq removed-feeds (cons elt removed-feeds))))))
      (cond (need-restart
             (set symbol value)
             (when (newsticker-running-p)
               (message "Restarting newsticker")
               (newsticker-stop)
               (newsticker-start)))
            (t
             (dolist (feed removed-feeds)
               (message "Stopping feed `%s'" (car feed))
               (newsticker--stop-feed (car feed)))
             (dolist (feed new-or-changed-feeds)
               (message "Starting feed `%s'" (car feed))
               (newsticker--stop-feed (car feed))
               (newsticker--start-feed feed))
             (unless new-or-changed-feeds
               (when newsticker--sentinel-callback
                 (funcall newsticker--sentinel-callback)))))
      (set symbol value))))

;; ======================================================================
;; retrieval
(defgroup newsticker-retrieval nil
  "Settings for news retrieval."
  :group 'newsticker)

(defcustom newsticker-url-list-defaults
  '(("Emacs Wiki"
     "https://www.emacswiki.org/emacs?action=rss"
     nil
     3600))
  "A customizable list of news feeds to select from.
These were mostly extracted from the Radio Community Server
<http://rcs.userland.com/>.

You may add other entries in `newsticker-url-list'."
  :type `(set ,@(mapcar #'newsticker--splicer
                        newsticker--raw-url-list-defaults))
  :set #'newsticker--set-customvar-retrieval
  :group 'newsticker-retrieval)

(defcustom newsticker-url-list nil
  "The news feeds which you like to watch.

This alist will be used in addition to selection made customizing
`newsticker-url-list-defaults'.

This is an alist.  Each element consists of two items: a LABEL and a URL,
optionally followed by a START-TIME, INTERVAL specifier and WGET-ARGUMENTS.

The LABEL gives the name of the news feed.  It can be an arbitrary string.

The URL gives the location of the news feed.  It must point to a valid
RSS or Atom file.  The file is retrieved by calling wget, or whatever you
specify as `newsticker-wget-name'.

URL may also be a function which returns news data.  In this case
`newsticker-retrieval-method' etc. are ignored for this feed.

The START-TIME can be either a string, or nil.  If it is a string it
specifies a fixed time at which this feed shall be retrieved for the
first time.  (Examples: \"11:00pm\", \"23:00\".)  If it is nil (or
unspecified), this feed will be retrieved immediately after calling
`newsticker-start'.

The INTERVAL specifies the time between retrievals for this feed.  If it
is nil (or unspecified) the default interval value as set in
`newsticker-retrieval-interval' is used.

\(newsticker.el calls `run-at-time'. The newsticker-parameters START-TIME
and INTERVAL correspond to the `run-at-time'-parameters TIME and REPEAT.)

WGET-ARGUMENTS specifies arguments for wget (see `newsticker-wget-name')
which apply for this feed only, overriding the value of
`newsticker-wget-arguments'."
  :type '(repeat (list :tag "News feed"
                       (string :tag "Label")
                       (choice :tag "URI"
                               (string :tag "String")
                               (function :tag "Function"))
                       (choice :tag "Start"
                               (const   :tag "Default" nil)
                               (string  :tag "Fixed Time"))
                       (choice :tag "Interval"
                               (const   :tag "Default" nil)
                               (const   :tag "Hourly" 3600)
                               (const   :tag "Daily" 86400)
                               (const   :tag "Weekly" 604800)
                               (integer :tag "Interval"))
                       (choice :tag "Wget Arguments"
                               (const  :tag "Default arguments" nil)
                               (repeat :tag "Special arguments" string))))
  :set #'newsticker--set-customvar-retrieval
  :group 'newsticker-retrieval)

(defcustom newsticker-retrieval-method
  'intern
  "Method for retrieving news from the web, either `intern' or `extern'.
Default value `intern' uses Emacs's built-in asynchronous download
capabilities (`url-retrieve').  If set to `extern' the external
program wget is used, see `newsticker-wget-name'."
  :type '(choice :tag "Method"
                 (const :tag "Intern" intern)
                 (const :tag "Extern" extern))
  :group 'newsticker-retrieval)

(defcustom newsticker-wget-name
  "wget"
  "Name of the program which is called to retrieve news from the web.
The canonical choice is wget but you may take any other program which is
able to return the contents of a news feed file on stdout."
  :type 'string
  :group 'newsticker-retrieval)

(defcustom newsticker-wget-arguments
  '("-q" "-O" "-")
  "Arguments which are passed to wget.
There is probably no reason to change the default settings, unless you
are living behind a firewall."
  :type '(repeat (string :tag "Argument"))
  :group 'newsticker-retrieval)

(defcustom newsticker-retrieval-interval
  3600
  "Time interval for retrieving new news items (seconds).
If this value is not positive (i.e. less than or equal to 0)
items are retrieved only once!
Please note that some feeds, e.g. Slashdot, will ban you if you
make it less than 1800 seconds (30 minutes)!"
  :type '(choice :tag "Interval"
                 (const   :tag "No automatic retrieval" 0)
                 (const   :tag "Hourly" 3600)
                 (const   :tag "Daily" 86400)
                 (const   :tag "Weekly" 604800)
                 (integer :tag "Interval"))
  :set #'newsticker--set-customvar-retrieval
  :group 'newsticker-retrieval)

(defcustom newsticker-desc-comp-max
  100
  "Relevant length of headline descriptions.
This value gives the maximum number of characters which will be
taken into account when newsticker compares two headline
descriptions."
  :type 'integer
  :group 'newsticker-retrieval)

;; ======================================================================
;; headline processing
(defgroup newsticker-headline-processing nil
  "Settings for the automatic processing of headlines."
  :group 'newsticker)

(defcustom newsticker-automatically-mark-items-as-old
  t
  "Decides whether to automatically mark items as old.
If t a new item is considered as new only after its first retrieval.  As
soon as it is retrieved a second time, it becomes old.  If not t all
items stay new until you mark them as old.  This is done in the
*newsticker* buffer."
  :type 'boolean
  :group 'newsticker-headline-processing)

(defcustom newsticker-automatically-mark-visited-items-as-old
  t
  "Decides whether to automatically mark visited items as old.
If t an item is marked as old as soon as the associated link is
visited, i.e. after pressing RET or mouse2 on the item's
headline."

  :type 'boolean
  :group 'newsticker-headline-processing)

(defcustom newsticker-keep-obsolete-items
  t
  "Decides whether to keep unread items which have been removed from feed.
If t a new item, which has been removed from the feed, is kept in
the cache until it is marked as read."
  :type 'boolean
  :group 'newsticker-headline-processing)

(defcustom newsticker-obsolete-item-max-age
  (* 60 60 24)
  "Maximal age of obsolete items, in seconds.
Obsolete items which are older than this value will be silently
deleted at the next retrieval."
  :type 'integer
  :group 'newsticker-headline-processing)

(defcustom newsticker-auto-mark-filter-list
  nil
  "A list of filters for automatically marking headlines.

This is an alist of the form (FEED-NAME PATTERN-LIST).  I.e. each
element consists of a FEED-NAME a PATTERN-LIST.  Each element of
the pattern-list has the form (AGE TITLE-OR-DESCRIPTION REGEXP).
AGE must be one of the symbols `old' or `immortal'.
TITLE-OR-DESCRIPTION must be one of the symbols `title',
`description', or `all'.  REGEXP is a regular expression, i.e., a
string.

This filter is checked after a new headline has been retrieved.
If FEED-NAME matches the name of the corresponding news feed, the
pattern-list is checked: The new headline will be marked as AGE
if REGEXP matches the headline's TITLE-OR-DESCRIPTION.

If, for example, `newsticker-auto-mark-filter-list' looks like
 ((slashdot (\\='old \\='title \"^Forget me!$\") (\\='immortal \\='title \"Read me\")
  (\\='immortal \\='all \"important\"))))

then all articles from slashdot are marked as old if they have
the title \"Forget me!\".  All articles with a title containing
the string \"Read me\" are marked as immortal.  All articles which
contain the string \"important\" in their title or their
description are marked as immortal."
  :type '(repeat (list :tag "Auto mark filter"
                       (string :tag "Feed name")
                       (repeat
                        (list :tag "Filter element"
                              (choice
                               :tag "Auto-assigned age"
                               (const :tag "Old" old)
                               (const :tag "Immortal" immortal))
                              (choice
                               :tag "Title/Description"
                               (const :tag "Title" title)
                               (const :tag "Description" description)
                               (const :tag "All" all))
                              (regexp :tag "Regexp")))))
  :group 'newsticker-headline-processing)

;; ======================================================================
;; hooks
(defgroup newsticker-hooks nil
  "Settings for newsticker hooks."
  :group 'newsticker)

(defcustom newsticker-start-hook
  nil
  "Hook run when starting newsticker.
This hook is run at the very end of `newsticker-start'."
  :options '(newsticker-start-ticker)
  :type 'hook
  :group 'newsticker-hooks)

(defcustom newsticker-stop-hook
  nil
  "Hook run when stopping newsticker.
This hook is run at the very end of `newsticker-stop'."
  :options nil
  :type 'hook
  :group 'newsticker-hooks)

(defcustom newsticker-new-item-functions
  nil
  "List of functions run after a new headline has been retrieved.
Each function is called with the following two arguments:
FEEDNAME  the name of the corresponding news feed,
ITEM      the decoded headline.

See `newsticker-new-item-functions-sample',
`newsticker-download-images', and
`newsticker-download-enclosures' for sample functions.

Please note that these functions are called only once for a
headline after it has been retrieved for the first time."
  :type 'hook
  :options '(newsticker-download-images
             newsticker-download-enclosures)
  :group 'newsticker-hooks)

;; ======================================================================
;; miscellaneous
(defgroup newsticker-miscellaneous nil
  "Miscellaneous newsticker settings."
  :group 'newsticker)

(defcustom newsticker-dir
  (locate-user-emacs-file "newsticker/" ".newsticker/")
  "Directory where newsticker saves data."
  :type 'directory
  :group 'newsticker-miscellaneous)

;; debugging
(defcustom newsticker-debug
  nil
  "Enables some features needed for debugging newsticker.el.

If set to t newsticker.el will print lots of debugging messages, and the
buffers *newsticker-wget-<feed>* will not be closed."
  :type 'boolean
  :group 'newsticker-miscellaneous)

;; ======================================================================
;;; Internal variables
;; ======================================================================
(defvar newsticker--buffer-uptodate-p nil
  "Tells whether the newsticker buffer is up to date.")
(defvar newsticker--latest-update-time (current-time)
  "The time at which the latest news arrived.")
(defvar newsticker--process-ids nil
  "List of PIDs of active newsticker processes.")

(defvar newsticker--cache nil "Cached newsticker data.
This is a list of the form

 ((label1
   (title description link time age index preformatted-contents
    preformatted-title extra-elements)
   ...)
  (label2
   (title description link time age index preformatted-contents
    preformatted-title extra-elements)
   ...)
  ...)

where LABEL is a symbol.  TITLE, DESCRIPTION, and LINK are
strings.  TIME is a time value as returned by `current-time'.
AGE is a symbol: `new', `old', `immortal', and `obsolete' denote
ordinary news items, whereas `feed' denotes an item which is not a
headline but describes the feed itself.  INDEX denotes the
original position of the item -- used for restoring the original
order.  PREFORMATTED-CONTENTS and PREFORMATTED-TITLE hold the
formatted contents of the item's description and title.  This
speeds things up if HTML rendering is used, which is rather
slow.  EXTRA-ELEMENTS is an alist containing additional elements.")

(defvar newsticker--auto-narrow-to-feed nil
  "Automatically narrow to current news feed.
If non-nil only the items of the current news feed are visible.")

(defvar newsticker--auto-narrow-to-item nil
  "Automatically narrow to current news item.
If non-nil only the current headline is visible.")

(defconst newsticker--error-headline
  "[COULD NOT DOWNLOAD HEADLINES!]"
  "Title of error headline which will be inserted if news retrieval fails.")

;; ======================================================================
;;; Shortcuts
;; ======================================================================
(defsubst newsticker--title (item)
  "Return title of ITEM."
  (nth 0 item))
(defsubst newsticker--desc (item)
  "Return description of ITEM."
  (nth 1 item))
(defsubst newsticker--link (item)
  "Return link of ITEM."
  (nth 2 item))
(defsubst newsticker--time (item)
  "Return time of ITEM."
  (nth 3 item))
(defsubst newsticker--age (item)
  "Return age of ITEM."
  (nth 4 item))
(defsubst newsticker--pos (item)
  "Return position/index of ITEM."
  (nth 5 item))
(defsubst newsticker--preformatted-contents (item)
  "Return pre-formatted text of ITEM."
  (nth 6 item))
(defsubst newsticker--preformatted-title (item)
  "Return pre-formatted title of ITEM."
  (nth 7 item))
(defsubst newsticker--extra (item)
  "Return extra attributes of ITEM."
  (nth 8 item))
(defsubst newsticker--guid-to-string (guid)
  "Return string representation of GUID."
  (if (stringp guid)
      guid
    (car (xml-node-children guid))))
(defsubst newsticker--guid (item)
  "Return guid of ITEM."
  (newsticker--guid-to-string (assoc 'guid (newsticker--extra item))))
(defsubst newsticker--enclosure (item)
  "Return enclosure element of ITEM in the form (...FIXME...) or nil."
  (let ((enclosure (assoc 'enclosure (newsticker--extra item))))
    (if enclosure
        (xml-node-attributes enclosure))))
(defun newsticker--real-feed-name (feed)
  "Return real name of FEED."
  (catch 'name
    (mapc (lambda (item)
            (if (eq (newsticker--age item) 'feed)
                (throw 'name (newsticker--title item))))
          (cdr (newsticker--cache-get-feed feed)))
    (symbol-name feed)))


;; ======================================================================
;;; User fun
;; ======================================================================

(defun newsticker--start-feed (feed &optional do-not-complain-if-running)
  "Start retrieval timer for FEED.
If timer is running already a warning message is printed unless
DO-NOT-COMPLAIN-IF-RUNNING is not nil.  Add the started
name/timer pair to `newsticker--retrieval-timer-list'."
  (let* ((feed-name (car feed))
         (start-time (nth 2 feed))
         (interval (or (nth 3 feed)
                       newsticker-retrieval-interval))
         (timer (assoc (car feed)
                       newsticker--retrieval-timer-list)))
    (if timer
        (or do-not-complain-if-running
            (message "Timer for %s is running already!"
                     feed-name))
      (newsticker--debug-msg "Starting timer for %s: %s, %d"
                             feed-name start-time interval)
      ;; do not repeat retrieval if interval not positive
      (if (<= interval 0)
          (setq interval nil))
      (setq timer (run-at-time start-time interval
                               #'newsticker-get-news feed-name))
      (if interval
          (add-to-list 'newsticker--retrieval-timer-list
                       (cons feed-name timer))))))

;;;###autoload
(defun newsticker-start (&optional _do-not-complain-if-running)
  "Start the newsticker.
Start the timers for display and retrieval.  If the newsticker, i.e. the
timers, are running already a warning message is printed unless
DO-NOT-COMPLAIN-IF-RUNNING is not nil.
Run `newsticker-start-hook' if newsticker was not running already."
  (interactive)
  (let ((running (newsticker-running-p)))
    ;; read old cache if it exists and newsticker is not running
    (unless running
      (newsticker--cache-read))
    ;; start retrieval timers -- one timer for each feed
    (dolist (feed (append newsticker-url-list-defaults newsticker-url-list))
      (newsticker--start-feed feed))
    (unless running
      (run-hooks 'newsticker-start-hook)
      (message "Newsticker started!"))))

(defun newsticker--stop-feed (feed-name)
  "Stop retrieval for feed FEED-NAME.
Delete the stopped name/timer pair from `newsticker--retrieval-timer-list'."
  (let ((name-and-timer (assoc feed-name newsticker--retrieval-timer-list)))
    (when name-and-timer
      (cancel-timer (cdr name-and-timer))
      (setq newsticker--retrieval-timer-list
            (delete name-and-timer newsticker--retrieval-timer-list)))))

(defun newsticker-stop ()
  "Stop the newsticker and the newsticker-ticker.
Cancel the timers for display and retrieval.  Run `newsticker-stop-hook'
if newsticker has been running."
  (interactive)
  (newsticker--cache-save)
  (when (fboundp 'newsticker-stop-ticker) ; silence compiler warnings
    (newsticker-stop-ticker))
  (when (newsticker-running-p)
    (dolist (name-and-timer newsticker--retrieval-timer-list)
      (newsticker--stop-feed (car name-and-timer)))
    (setq newsticker--retrieval-timer-list nil)
    (run-hooks 'newsticker-stop-hook)
    (message "Newsticker stopped!")))

(defun newsticker-get-all-news ()
  "Launch retrieval of news from all configured newsticker sites.
This does NOT start the retrieval timers."
  (interactive)
  ;; launch retrieval of news
  (dolist (item (append newsticker-url-list-defaults newsticker-url-list))
    (newsticker-get-news (car item))))

(defun newsticker-save-item (feed item)
  "Save FEED ITEM."
  (interactive)
  (let ((filename (read-string "Filename: "
                               (concat feed ":_"
                                       (string-replace
                                        " " "_" (newsticker--title item))
                                       ".html"))))
    (with-temp-buffer
      (insert (newsticker--desc item))
      (write-file filename t))))

(defun newsticker-add-url (url name)
  "Add given URL under given NAME to `newsticker-url-list'.
If URL is nil it is searched at point."
  (interactive
   (list
    (read-string "URL: "
                 (save-excursion
                   (end-of-line)
                   (and
                    (re-search-backward
                     (rx "http" (? "s") "://")
                     (if (> (point) (+ (point-min) 100))
                         (- (point) 100)
                       (point-min))
                     t)
                    (re-search-forward
                     (rx "http" (? "s") "://" (zero-or-more (any "-a-zA-Z0-9&/_.")))
                     (if (< (point) (- (point-max) 200))
                         (+ (point) 200)
                       (point-max))
                     t)
                    (buffer-substring-no-properties (match-beginning 0)
                                                    (match-end 0)))))
    (read-string "Name: ")))
  (add-to-list 'newsticker-url-list (list name url nil nil nil) t)
  (customize-variable 'newsticker-url-list))

(defun newsticker-customize-feed (feed-name)
  "Open customization buffer for `newsticker-url-list' and jump to FEED-NAME."
  (interactive
   (list (completing-read "Name of feed or group to edit: "
                          (mapcar #'car newsticker-url-list))))
  (customize-variable 'newsticker-url-list)
  (when (search-forward (concat "Label: " feed-name) nil t)
    (forward-line -1)))

(defun newsticker-customize ()
  "Open the newsticker customization group."
  (interactive)
  (delete-other-windows)
  (customize-group "newsticker"))

;; ======================================================================
;;; Local stuff
;; ======================================================================
(defun newsticker--get-news-by-funcall (feed-name function)
  "Get news for the site FEED-NAME by calling FUNCTION.
See `newsticker-get-news'."
  (let ((buffername (concat " *newsticker-funcall-" feed-name "*")))
    (with-current-buffer (get-buffer-create buffername)
      (erase-buffer)
      (newsticker--insert-bytes (funcall function feed-name))
      (newsticker--sentinel-work nil t feed-name function
                                 (current-buffer)))))

(defun newsticker--get-news-by-url (feed-name url)
  "Get news for the site FEED-NAME from address URL using `url-retrieve'.
See `newsticker-get-news'."
  (let ((coding-system-for-read 'no-conversion))
    (condition-case error-data
        (url-retrieve url 'newsticker--get-news-by-url-callback
                      (list feed-name))
      (error (message "Error retrieving news from %s: %s" feed-name
                      error-data))))
  (force-mode-line-update))

(defun newsticker--get-news-by-url-callback (status feed-name)
  "Callback function for `newsticker--get-news-by-url'.
STATUS is the return status as delivered by `url-retrieve', and
FEED-NAME is the name of the feed that the news were retrieved
from."
  (let ((buf (get-buffer-create (concat " *newsticker-url-" feed-name "*")))
        (result (buffer-string)))
    (set-buffer buf)
    (erase-buffer)
    (newsticker--insert-bytes result)
    ;; remove MIME header
    (goto-char (point-min))
    (search-forward "\n\n" nil t)
    (delete-region (point-min) (point))
    ;; read the rss/atom contents
    (newsticker--sentinel-work nil
                               (or (not status)
                                   (not (eq (car status) :error)))
                               feed-name "url-retrieve"
                               (current-buffer))
    (when status
      (let ((status-type (car status))
            (status-details (cdr status)))
        (cond ((eq status-type :redirect)
               ;; don't care about redirects
               )
              ((eq status-type :error)
               (message "%s: Error while retrieving news from %s: %s: \"%s\""
                        (format-time-string "%A, %H:%M")
                        feed-name
                        (car status-details) (cdr status-details))))))))

(defun newsticker--get-news-by-wget (feed-name url wget-arguments)
  "Get news for the site FEED-NAME from address URL using wget.
WGET-ARGUMENTS is a list of arguments for wget.
See `newsticker-get-news'."
  (let ((buffername (concat " *newsticker-wget-" feed-name "*")))
    (with-current-buffer (get-buffer-create buffername)
      (erase-buffer)
      ;; throw an error if there is an old wget-process around
      (if (get-process feed-name)
          (error "Another wget-process is running for %s" feed-name))
      ;; start wget
      (let* ((args (append wget-arguments (list url)))
             (proc (apply #'start-process feed-name buffername
                          newsticker-wget-name args)))
        (set-process-coding-system proc 'no-conversion 'no-conversion)
        (set-process-sentinel proc #'newsticker--sentinel)
        (process-put proc 'nt-feed-name feed-name)
        (setq newsticker--process-ids (cons (process-id proc)
                                            newsticker--process-ids))
        (force-mode-line-update)))))

(defun newsticker-get-news (feed-name)
  "Get news from the site FEED-NAME and load feed logo.
FEED-NAME must be a string which occurs as the label (i.e. the first element)
in an element of `newsticker-url-list' or `newsticker-url-list-defaults'."
  (newsticker--debug-msg "%s: Getting news for %s"
                         (format-time-string "%A, %H:%M")
                         feed-name)
  (let* ((item (or (assoc feed-name newsticker-url-list)
                   (assoc feed-name newsticker-url-list-defaults)
                   (error
                    "Cannot get news for %s: Check newsticker-url-list"
                    feed-name)))
         (url (cadr item))
         (wget-arguments (or (car (cdr (cdr (cdr (cdr item)))))
                             newsticker-wget-arguments)))
    (if (functionp url)
        (newsticker--get-news-by-funcall feed-name url)
      (if (eq newsticker-retrieval-method 'intern)
          (newsticker--get-news-by-url feed-name url)
        (newsticker--get-news-by-wget feed-name url wget-arguments)))))

;; ======================================================================
;; Parsing
;; ======================================================================

(defun newsticker--sentinel (process event)
  "Sentinel for extracting news titles from an RDF buffer.
Argument PROCESS is the process which has just changed its state.
Argument EVENT tells what has happened to the process."
  (let ((p-status (process-status process))
        (exit-status (process-exit-status process))
        (feed-name (process-get  process 'nt-feed-name))
        (command (process-command process))
        (buffer (process-buffer process)))
    (newsticker--sentinel-work event
                               (and (eq p-status 'exit)
                                    (= exit-status 0))
                               feed-name command buffer)))

(defun newsticker--sentinel-work (event status-ok feed-name command buffer)
  "Actually do the sentinel work.
Argument EVENT tells what has happened to the retrieval process.
Argument STATUS-OK is the final status of the retrieval process,
non-nil meaning retrieval was successful.
Argument FEED-NAME is the name of the retrieved feed.
Argument COMMAND is the command of the retrieval process.
Argument BUFFER is the buffer of the retrieval process."
  (let ((time (current-time))
        (name-symbol (intern feed-name))
        (something-was-added nil)
        (ct (current-time)))
    ;; catch known errors (zombie processes, rubbish-xml etc.
    ;; if an error occurs the news feed is not updated!
    (catch 'oops
      (unless status-ok
        (setq newsticker--cache
              (newsticker--cache-add
               newsticker--cache
               name-symbol
               newsticker--error-headline
               (format-message
                (concat "%s: Newsticker could not retrieve news from %s.\n"
                        "Return status: `%s'\n"
                        "Command was `%s'")
                (format-time-string "%A, %H:%M")
                feed-name event command)
               ""
               ct
               'new
               0 '((guid nil "newsticker--download-error"))
               ct))
        (message "%s: Error while retrieving news from %s"
                 (format-time-string "%A, %H:%M")
                 feed-name)
        (throw 'oops nil))
      (let* ((coding-system 'utf-8)
             (node-list
              (save-current-buffer
                (set-buffer buffer)
                (unless (fboundp 'libxml-parse-xml-region)
                  (newsticker--do-xml-workarounds))
                ;; check coding system
                (goto-char (point-min))
                (if (re-search-forward "encoding=['\"]\\([^\"]+?\\)['\"]"
                                       nil t)
                    (setq coding-system (intern (downcase (match-string 1))))
                  (setq coding-system
                        (condition-case nil
                            (check-coding-system coding-system)
                          (coding-system-error
                           (message
                            "newsticker.el: ignoring coding system %s for %s"
                            coding-system feed-name)
                           nil))))
                ;; Decode if possible
                (when coding-system
                  (decode-coding-region (point-min) (point-max)
                                        coding-system))
                (condition-case errordata
                    ;; The xml parser might fail or the xml might be bugged.
                    (if (fboundp 'libxml-parse-xml-region)
                        (progn
                          (xml-remove-comments (point-min) (point-max))
                          (list (libxml-parse-xml-region (point-min) (point-max)
                                                         nil)))
                      (xml-parse-region (point-min) (point-max)))
                  (error (message "Could not parse %s: %s"
                                  (buffer-name) (cadr errordata))
                         (throw 'oops nil)))))
             (topnode (car node-list))
             (image-url nil)
             (icon-url nil))
        ;; mark all items as obsolete
        (newsticker--cache-replace-age newsticker--cache
                                       name-symbol
                                       'new 'obsolete-new)
        (newsticker--cache-replace-age newsticker--cache
                                       name-symbol
                                       'old 'obsolete-old)
        (newsticker--cache-replace-age newsticker--cache
                                       name-symbol
                                       'feed 'obsolete-old)

        ;; check Atom/RSS version and call corresponding parser
        (condition-case error-data
            (if (cond
                 ;; RSS 0.91
                 ((and (eq 'rss (xml-node-name topnode))
                       (string= "0.91" (xml-get-attribute topnode 'version)))
                  (setq image-url (newsticker--get-logo-url-rss-0.91 topnode))
                  (newsticker--parse-rss-0.91 feed-name time topnode))
                 ;; RSS 0.92
                 ((and (eq 'rss (xml-node-name topnode))
                       (string= "0.92" (xml-get-attribute topnode 'version)))
                  (setq image-url (newsticker--get-logo-url-rss-0.92 topnode))
                  (newsticker--parse-rss-0.92 feed-name time topnode))
                 ;; RSS 1.0
                 ((or (eq 'RDF (xml-node-name topnode))
                      (eq 'rdf:RDF (xml-node-name topnode)))
                  (setq image-url (newsticker--get-logo-url-rss-1.0 topnode))
                  (newsticker--parse-rss-1.0 feed-name time topnode))
                 ;; RSS 2.0
                 ((and (eq 'rss (xml-node-name topnode))
                       (string= "2.0" (xml-get-attribute topnode 'version)))
                  (setq image-url (newsticker--get-logo-url-rss-2.0 topnode))
                  (newsticker--parse-rss-2.0 feed-name time topnode))
                 ;; Atom 0.3
                 ((and (eq 'feed (xml-node-name topnode))
                       (string= "http://purl.org/atom/ns#"
                                (xml-get-attribute topnode 'xmlns)))
                  (setq image-url (newsticker--get-logo-url-atom-0.3 topnode))
                  (newsticker--parse-atom-0.3 feed-name time topnode))
                 ;; Atom 1.0
                 (t
                  ;; The test for Atom 1.0 does not work when using
                  ;; libxml, as with libxml the namespace attribute is
                  ;; not in the xml tree.  For the time being we skip
                  ;; the check and assume that we are dealing with an
                  ;; Atom 1.0 feed.

                  ;; (and (eq 'feed (xml-node-name topnode))
                  ;;      (string= "https://www.w3.org/2005/Atom"
                  ;;               (xml-get-attribute topnode 'xmlns)))
                  (setq image-url (newsticker--get-logo-url-atom-1.0 topnode))
                  (setq icon-url (newsticker--get-icon-url-atom-1.0 topnode))
                  (newsticker--parse-atom-1.0 feed-name time topnode))
                 ;; unknown feed type
                 ;; (t
                 ;;  (newsticker--debug-msg "Feed type unknown: %s: %s"
                 ;;                         (xml-node-name topnode) feed-name)
                 ;;  nil)
                 )
                (setq something-was-added t))
          (error (message "sentinelerror in %s: %s" feed-name error-data)))

        ;; Remove those old items from cache which have been removed from
        ;; the feed
        (newsticker--cache-replace-age newsticker--cache
                                       name-symbol 'obsolete-old 'deleteme)
        (newsticker--cache-remove newsticker--cache name-symbol
                                  'deleteme)
        ;; Remove those new items from cache which have been removed from
        ;; the feed.  Or keep them as `obsolete'
        (if (not newsticker-keep-obsolete-items)
            (newsticker--cache-remove newsticker--cache
                                      name-symbol 'obsolete-new)
          (setq newsticker--cache
                (newsticker--cache-mark-expired
                 newsticker--cache name-symbol 'obsolete 'obsolete-expired
                 newsticker-obsolete-item-max-age))
          (newsticker--cache-remove newsticker--cache
                                    name-symbol 'obsolete-expired)
          (newsticker--cache-replace-age newsticker--cache
                                         name-symbol 'obsolete-new
                                         'obsolete))
        (newsticker--update-process-ids)
        ;; setup scrollable text
        (when (= 0 (length newsticker--process-ids))
          (when (fboundp 'newsticker--ticker-text-setup) ;silence
                                        ;compiler
                                        ;warnings
            (newsticker--ticker-text-setup)))
        (setq newsticker--latest-update-time (current-time))
        (when something-was-added
          ;; FIXME: should we care about removed items as well?
          (newsticker--cache-save-feed
           (newsticker--cache-get-feed name-symbol))
          (when (fboundp 'newsticker--buffer-set-uptodate) ;silence
                                        ;compiler
                                        ;warnings
            (newsticker--buffer-set-uptodate nil)))
        ;; kill the process buffer if wanted
        (unless newsticker-debug
          (kill-buffer buffer))
        ;; launch retrieval of images
        (when (and (boundp 'newsticker-download-logos)
                   newsticker-download-logos)
          ;; feed logo
          (when image-url
            (newsticker--image-get feed-name feed-name (newsticker--images-dir)
                                   image-url))
          ;; icon / favicon
          (setq icon-url
                (or icon-url
                    (let* ((feed-url (newsticker--link (cadr (newsticker--cache-get-feed
                                                              (intern feed-name)))))
                           (uri (url-generic-parse-url feed-url)))
                      (when (and feed-url uri)
                        (setf (url-filename uri) nil)
                        (setf (url-target uri) nil)
                        (concat (url-recreate-url uri) "favicon.ico")))))
          (when icon-url
            (newsticker--image-get feed-name
                                   (concat feed-name "."
                                           (file-name-extension icon-url))
                                   (newsticker--icons-dir)
                                   icon-url))))))
  (when newsticker--sentinel-callback
    (funcall newsticker--sentinel-callback)))

(defun newsticker--do-xml-workarounds ()
  "Fix all issues which `xml-parse-region' could be choking on."

  ;; a very very dirty workaround to overcome the
  ;; problems with the newest (20030621) xml.el:
  ;; remove all unnecessary whitespace
  (goto-char (point-min))
  (while (re-search-forward ">[ \t\r\n]+<" nil t)
    (replace-match "><" nil t))
  ;; and another brutal workaround (20031105)!  For some
  ;; reason the xml parser does not like the colon in the
  ;; doctype name "rdf:RDF"
  (goto-char (point-min))
  (if (re-search-forward "<!DOCTYPE[ \t\n]+rdf:RDF" nil t)
      (replace-match "<!DOCTYPE rdfColonRDF" nil t))
  ;; finally.... ~##^°!!!!!
  (goto-char (point-min))
  (while (search-forward "\r\n" nil t)
    (replace-match "\n" nil t))
  ;; still more brutal workarounds (20040309)!  The xml
  ;; parser does not like doctype rss
  (goto-char (point-min))
  (if (re-search-forward "<!DOCTYPE[ \t\n]+rss[ \t\n]*>" nil t)
      (replace-match "" nil t))
  ;; And another one (20050618)! (Fixed in GNU Emacs 22.0.50.18)
  ;; Remove comments to avoid this xml-parsing bug:
  ;; "XML files can have only one toplevel tag"
  (goto-char (point-min))
  (while (search-forward "<!--" nil t)
    (let ((start (match-beginning 0)))
      (unless (search-forward "-->" nil t)
        (error "Can't find end of comment"))
      (delete-region start (point))))
  ;; And another one (20050702)! If description is HTML
  ;; encoded and starts with a `<', wrap the whole
  ;; description in a CDATA expression.  This happened for
  ;; https://www.thefreedictionary.com/_/WoD/rss.aspx?type=quote
  (goto-char (point-min))
  (while (re-search-forward
          "<description>\\(<img.*?\\)</description>" nil t)
    (replace-match
     "<description><![CDATA[ \\1 ]]></description>"))
  ;; And another one (20051123)! XML parser does not
  ;; like this: <yweather:location city="Frankfurt/Main"
  ;; region="" country="GM" />
  ;; try to "fix" empty attributes
  ;; This happened for
  ;; http://xml.weather.yahoo.com/forecastrss?p=GMXX0040&u=f
  (goto-char (point-min))
  (while (re-search-forward "\\(<[^>]*\\)=\"\"" nil t)
    (replace-match "\\1=\" \""))
  ;;
  (set-buffer-modified-p nil))


(defun newsticker--get-logo-url-atom-1.0 (node)
  "Return logo URL from atom 1.0 data in NODE."
  (car (xml-node-children
        (car (xml-get-children node 'logo)))))

(defun newsticker--get-icon-url-atom-1.0 (node)
  "Return icon URL from atom 1.0 data in NODE."
  (car (xml-node-children
        (car (xml-get-children node 'icon)))))

(defun newsticker--get-logo-url-atom-0.3 (node)
  "Return logo URL from atom 0.3 data in NODE."
  (car (xml-node-children
        (car (xml-get-children (car (xml-get-children node 'image)) 'url)))))

(defun newsticker--get-logo-url-rss-2.0 (node)
  "Return logo URL from RSS 2.0 data in NODE."
  (car (xml-node-children
        (car (xml-get-children
              (car (xml-get-children
                    (car (xml-get-children node 'channel)) 'image)) 'url)))))

(defun newsticker--get-logo-url-rss-1.0 (node)
  "Return logo URL from RSS 1.0 data in NODE."
  (car (xml-node-children
        (car (xml-get-children (car (xml-get-children node 'image)) 'url)))))

(defun newsticker--get-logo-url-rss-0.92 (node)
  "Return logo URL from RSS 0.92 data in NODE."
  (car (xml-node-children
        (car (xml-get-children (car (xml-get-children node 'image)) 'url)))))

(defun newsticker--get-logo-url-rss-0.91 (node)
  "Return logo URL from RSS 0.91 data in NODE."
  (car (xml-node-children
        (car (xml-get-children (car (xml-get-children node 'image)) 'url)))))

(defun newsticker--parse-atom-0.3 (name time topnode)
  "Parse Atom 0.3 data.
Return value as well as arguments NAME, TIME, and TOPNODE are the
same as in `newsticker--parse-atom-1.0'."
  (newsticker--debug-msg "Parsing Atom 0.3 feed %s" name)
  (let (new-feed new-item)
    (setq new-feed (newsticker--parse-generic-feed
                    name time
                    ;; title
                    (car (xml-node-children
                          (car (xml-get-children topnode 'title))))
                    ;; desc
                    (car (xml-node-children
                          (car (xml-get-children topnode 'content))))
                    ;; link
                    (xml-get-attribute
                     (car (xml-get-children topnode 'link)) 'href)
                    ;; extra-elements
                    (xml-node-children topnode)))
    (setq new-item (newsticker--parse-generic-items
                    name time (xml-get-children topnode 'entry)
                    ;; title-fn
                    (lambda (node)
                      (car (xml-node-children
                            (car (xml-get-children node 'title)))))
                    ;; desc-fn
                    (lambda (node)
                      (or (car (xml-node-children
                                (car (xml-get-children node 'content))))
                          (car (xml-node-children
                                (car (xml-get-children node 'summary))))))
                    ;; link-fn
                    (lambda (node)
                      (xml-get-attribute
                       (car (xml-get-children node 'link)) 'href))
                    ;; time-fn
                    (lambda (node)
                      (newsticker--decode-rfc822-date
                       (car (xml-node-children
                             (car (xml-get-children node 'modified))))))
                    ;; guid-fn
                    (lambda (node)
                      (newsticker--guid-to-string
                       (assoc 'guid (xml-node-children node))))
                    ;; extra-fn
                    (lambda (node)
                      (xml-node-children node))))
    (or new-item new-feed)))

(defun newsticker--unxml (node)
  "Reverse parsing of an xml string.
Restore an xml-string from a an xml NODE that was returned by xml-parse..."
  (if (or (not node) (stringp node))
      node
    (newsticker--unxml-node node)))

(defun newsticker--unxml-node (node)
  "Actually restore xml-string of an xml NODE."
  (let ((qname (symbol-name (car node)))
        (att-list (cadr node))
        (children (cddr node)))
    (concat "<" qname
            (when att-list " ")
            (mapconcat #'newsticker--unxml-attribute att-list " ")
            ">"
            (mapconcat #'newsticker--unxml children "") "</" qname ">")))

(defun newsticker--unxml-attribute (attribute)
  "Actually restore xml-string of an ATTRIBUTE of an xml node."
  (let ((name (symbol-name (car attribute)))
        (value (cdr attribute)))
    (concat name "=\"" value "\"")))

(defun newsticker--parse-atom-1.0 (name time topnode)
  "Parse Atom 1.0 data.
Argument NAME gives the name of a news feed.  TIME gives the
system time at which the data have been retrieved.  TOPNODE
contains the feed data as returned by the xml parser.

For the Atom 1.0 specification see
URL `http://www.atompub.org/2005/08/17/draft-ietf-atompub-format-11.html'"
  (newsticker--debug-msg "Parsing Atom 1.0 feed %s" name)
  (let (new-feed new-item)
    (setq new-feed (newsticker--parse-generic-feed
                    name time
                    ;; title
                    (car (xml-node-children
                          (car (xml-get-children topnode 'title))))
                    ;; desc
                    (car (xml-node-children
                          (car (xml-get-children topnode 'subtitle))))
                    ;; link
                    (lambda (node)
                      (xml-get-attribute
                       (car (xml-get-children node 'link)) 'href))
                    ;; extra-elements
                    (xml-node-children topnode)))
    (setq new-item (newsticker--parse-generic-items
                    name time (xml-get-children topnode 'entry)
                    ;; title-fn
                    (lambda (node)
                      (car (xml-node-children
                            (car (xml-get-children node 'title)))))
                    ;; desc-fn
                    (lambda (node)
                      ;; unxml the content or the summary node. Atom
                      ;; allows for integrating (x)html into the atom
                      ;; structure but we need the raw html string.
                      ;; e.g. https://www.heise.de/open/news/news-atom.xml
                      ;; https://feeds.feedburner.com/ru_nix_blogs
                      (or (newsticker--unxml
                           (car (xml-node-children
                                 (car (xml-get-children node 'content)))))
                          (newsticker--unxml
                           (car (xml-node-children
                                 (car (xml-get-children node 'summary)))))
                          (car (xml-node-children
                                (car (xml-get-children node 'summary))))))
                    ;; link-fn
                    (lambda (node)
                      (xml-get-attribute
                       (car (xml-get-children node 'link)) 'href))
                    ;; time-fn
                    (lambda (node)
                      (newsticker--decode-iso8601-date
                       (or (car (xml-node-children
                                 (car (xml-get-children node 'updated))))
                           (car (xml-node-children
                                 (car (xml-get-children node 'published)))))))
                    ;; guid-fn
                    (lambda (node)
                      (car (xml-node-children
                            (car (xml-get-children node 'id)))))
                    ;; extra-fn
                    (lambda (node)
                      (xml-node-children node))))
    (or new-item new-feed)))

(defun newsticker--parse-rss-0.91 (name time topnode)
  "Parse RSS 0.91 data.
Return value as well as arguments NAME, TIME, and TOPNODE are the
same as in `newsticker--parse-atom-1.0'.

For the RSS 0.91 specification see URL `http://backend.userland.com/rss091'."
  (newsticker--debug-msg "Parsing RSS 0.91 feed %s" name)
  (let* ((channelnode (car (xml-get-children topnode 'channel)))
         is-new-feed has-new-items)
    (setq is-new-feed (newsticker--parse-generic-feed
                       name time
                       ;; title
                       (car (xml-node-children
                             (car (xml-get-children channelnode 'title))))
                       ;; desc
                       (car (xml-node-children
                             (car (xml-get-children channelnode
                                                    'description))))
                       ;; link
                       (car (xml-node-children
                             (car (xml-get-children channelnode 'link))))
                       ;; extra-elements
                       (xml-node-children channelnode)))
    (setq has-new-items (newsticker--parse-generic-items
                         name time (xml-get-children channelnode 'item)
                         ;; title-fn
                         (lambda (node)
                           (car (xml-node-children
                                 (car (xml-get-children node 'title)))))
                         ;; desc-fn
                         (lambda (node)
                           (car (xml-node-children
                                 (car (xml-get-children node 'description)))))
                         ;; link-fn
                         (lambda (node)
                           (car (xml-node-children
                                 (car (xml-get-children node 'link)))))
                         ;; time-fn
                         (lambda (node)
                           (newsticker--decode-rfc822-date
                            (car (xml-node-children
                                  (car (xml-get-children node 'pubDate))))))
                         ;; guid-fn
                         (lambda (_node)
                           nil)
                         ;; extra-fn
                         (lambda (node)
                           (xml-node-children node))))
    (or has-new-items is-new-feed)))

(defun newsticker--parse-rss-0.92 (name time topnode)
  "Parse RSS 0.92 data.
Return value as well as arguments NAME, TIME, and TOPNODE are the
same as in `newsticker--parse-atom-1.0'.

For the RSS 0.92 specification see URL `http://backend.userland.com/rss092'."
  (newsticker--debug-msg "Parsing RSS 0.92 feed %s" name)
  (let* ((channelnode (car (xml-get-children topnode 'channel)))
         is-new-feed has-new-items)
    (setq is-new-feed (newsticker--parse-generic-feed
                       name time
                       ;; title
                       (car (xml-node-children
                             (car (xml-get-children channelnode 'title))))
                       ;; desc
                       (car (xml-node-children
                             (car (xml-get-children channelnode
                                                    'description))))
                       ;; link
                       (car (xml-node-children
                             (car (xml-get-children channelnode 'link))))
                       ;; extra-elements
                       (xml-node-children channelnode)))
    (setq has-new-items (newsticker--parse-generic-items
                         name time (xml-get-children channelnode 'item)
                         ;; title-fn
                         (lambda (node)
                           (car (xml-node-children
                                 (car (xml-get-children node 'title)))))
                         ;; desc-fn
                         (lambda (node)
                           (car (xml-node-children
                                 (car (xml-get-children node 'description)))))
                         ;; link-fn
                         (lambda (node)
                           (car (xml-node-children
                                 (car (xml-get-children node 'link)))))
                         ;; time-fn
                         (lambda (node)
                           (newsticker--decode-rfc822-date
                            (car (xml-node-children
                                  (car (xml-get-children node 'pubDate))))))
                         ;; guid-fn
                         (lambda (_node)
                           nil)
                         ;; extra-fn
                         (lambda (node)
                           (xml-node-children node))))
    (or has-new-items is-new-feed)))

(defun newsticker--parse-rss-1.0 (name time topnode)
  "Parse RSS 1.0 data.
Return value as well as arguments NAME, TIME, and TOPNODE are the
same as in `newsticker--parse-atom-1.0'.

For the RSS 1.0 specification see URL `https://web.resource.org/rss/1.0/spec'."
  (newsticker--debug-msg "Parsing RSS 1.0 feed %s" name)
  (let* ((channelnode (car (xml-get-children topnode 'channel)))
         is-new-feed has-new-items)
    (setq is-new-feed (newsticker--parse-generic-feed
                       name time
                       ;; title
                       (car (xml-node-children
                             (car (xml-get-children channelnode 'title))))
                       ;; desc
                       (or (car (xml-node-children
                                 (car (xml-get-children channelnode
                                                        'encoded))))
                           (car (xml-node-children
                                 (car (xml-get-children channelnode
                                                        'content:encoded))))
                           (car (xml-node-children
                                 (car (xml-get-children channelnode
                                                        'description)))))
                       ;; link
                       (car (xml-node-children
                             (car (xml-get-children channelnode 'link))))
                       ;; extra-elements
                       (xml-node-children channelnode)))
    (setq has-new-items (newsticker--parse-generic-items
                         name time (xml-get-children topnode 'item)
                         ;; title-fn
                         (lambda (node)
                           (car (xml-node-children
                                 (car (xml-get-children node 'title)))))
                         ;; desc-fn
                         (lambda (node)
                           (car (xml-node-children
                                 (car (xml-get-children node
                                                        'description)))))
                         ;; link-fn
                         (lambda (node)
                           (car (xml-node-children
                                 (car (xml-get-children node 'link)))))
                         ;; time-fn
                         (lambda (node)
                           (newsticker--decode-iso8601-date
                            (or (car (xml-node-children
                                      (car (xml-get-children node 'dc:date))))
                                (car (xml-node-children
                                      (car (xml-get-children node 'date)))))))
                         ;; guid-fn
                         (lambda (_node)
                           nil)
                         ;; extra-fn
                         (lambda (node)
                           (xml-node-children node))))
    (or has-new-items is-new-feed)))

(defun newsticker--parse-rss-2.0 (name time topnode)
  "Parse RSS 2.0 data.
Return value as well as arguments NAME, TIME, and TOPNODE are the
same as in `newsticker--parse-atom-1.0'.

For the RSS 2.0 specification see URL `https://cyber.harvard.edu/rss/'."
  (newsticker--debug-msg "Parsing RSS 2.0 feed %s" name)
  (let* ((channelnode (car (xml-get-children topnode 'channel)))
         is-new-feed has-new-items)
    (setq is-new-feed (newsticker--parse-generic-feed
                       name time
                       ;; title
                       (car (xml-node-children
                             (car (xml-get-children channelnode 'title))))
                       ;; desc
                       (or (car (xml-node-children
                                 (car (xml-get-children channelnode
                                                        'encoded))))
                           (car (xml-node-children
                                 (car (xml-get-children channelnode
                                                        'content:encoded))))
                           (car (xml-node-children
                                 (car (xml-get-children channelnode
                                                        'description)))))
                       ;; link
                       (car (xml-node-children
                             (car (xml-get-children channelnode 'link))))
                       ;; extra-elements
                       (xml-node-children channelnode)))
    (setq has-new-items (newsticker--parse-generic-items
                         name time (xml-get-children channelnode 'item)
                         ;; title-fn
                         (lambda (node)
                           (car (xml-node-children
                                 (car (xml-get-children node 'title)))))
                         ;; desc-fn
                         (lambda (node)
                           (or (car (xml-node-children
                                     (car (xml-get-children node
                                                            'encoded))))
                               (car (xml-node-children
                                     (car (xml-get-children node
                                                            'content:encoded))))
                               (car (xml-node-children
                                     (car (xml-get-children node
                                                            'description))))))
                         ;; link-fn
                         (lambda (node)
                           (car (xml-node-children
                                 (car (xml-get-children node 'link)))))
                         ;; time-fn
                         (lambda (node)
                           (newsticker--decode-rfc822-date
                            (car (xml-node-children
                                  (car (xml-get-children node 'pubDate))))))
                         ;; guid-fn
                         (lambda (node)
                           (newsticker--guid-to-string
                            (assoc 'guid (xml-node-children node))))
                         ;; extra-fn
                         (lambda (node)
                           (xml-node-children node))))
    (or has-new-items is-new-feed)))

(defun newsticker--parse-generic-feed (name time title desc link
                                            extra-elements)
  "Parse generic news feed data.
Argument NAME gives the name of a news feed.  TIME gives the
system time at which the data have been retrieved.

The arguments TITLE, DESC, LINK, and EXTRA-ELEMENTS give the feed's title,
description, link, and extra elements resp."
  (let ((title (or title "[untitled]"))
        (link (or link ""))
        (position 0)
        (something-was-added nil))
    ;; decode numeric entities
    (setq title (xml-substitute-numeric-entities title))
    (setq desc  (xml-substitute-numeric-entities desc))
    (setq link  (xml-substitute-numeric-entities link))
    ;; remove whitespace from title, desc, and link
    (setq title (newsticker--remove-whitespace title))
    (setq desc (newsticker--remove-whitespace desc))
    (setq link (newsticker--remove-whitespace link))

    ;; handle the feed itself
    (unless (newsticker--cache-contains newsticker--cache
                                        (intern name) title
                                        desc link 'feed)
      (setq something-was-added t))
    (setq newsticker--cache
          (newsticker--cache-add newsticker--cache (intern name)
                                 title desc link time 'feed position
                                 extra-elements time 'feed))
    something-was-added))

(defun newsticker--parse-generic-items (name time itemlist
                                             title-fn desc-fn
                                             link-fn time-fn
                                             guid-fn extra-fn)
  "Parse generic news feed data.
Argument NAME gives the name of a news feed.  TIME gives the
system time at which the data have been retrieved.  ITEMLIST
contains the news items returned by the xml parser.

The arguments TITLE-FN, DESC-FN, LINK-FN, TIME-FN, GUID-FN, and
EXTRA-FN give functions for extracting title, description, link,
time, guid, and extra-elements resp.  They are called with one
argument, which is one of the items in ITEMLIST."
  (let ((position 0)
        (something-was-added nil))
    ;; gather all items for this feed
    (dolist (node itemlist)
      (setq position (1+ position))
      (let ((title (or (funcall title-fn node) "[untitled]"))
            (desc (funcall desc-fn node))
            (link (or (funcall link-fn node) "")))
        (setq time (or (funcall time-fn node) time))
        ;; It happened that the title or description
        ;; contained evil HTML code that confused the
        ;; xml parser.  Therefore:
        (unless (stringp title)
          (setq title (prin1-to-string title)))
        (unless (or (stringp desc) (not desc))
          (setq desc (prin1-to-string desc)))
        ;; ignore items with empty title AND empty desc
        (when (or (> (length title) 0)
                  (> (length desc) 0))
          ;; decode numeric entities
          (setq title (xml-substitute-numeric-entities title))
          (when desc
            (setq desc (xml-substitute-numeric-entities desc)))
          (setq link (xml-substitute-numeric-entities link))
          ;; remove whitespace from title, desc, and link
          (setq title (newsticker--remove-whitespace title))
          (setq desc (newsticker--remove-whitespace desc))
          (setq link (newsticker--remove-whitespace link))
          ;; add data to cache
          ;; do we have this item already?
          (let ((old-item
                 (let* ((guid (funcall guid-fn node)))
                   ;;(message "guid=%s" guid)
                   (newsticker--cache-contains newsticker--cache
                                               (intern name) title
                                               desc link nil guid)))
                (age1 'new)
                (age2 'old)
                (item-new-p nil))
            ;; Add this item, or mark it as old, or do nothing
            (if old-item
                (let ((prev-age (newsticker--age old-item)))
                  (unless newsticker-automatically-mark-items-as-old
                    ;; Some feeds deliver items multiply, the
                    ;; first time we find an 'obsolete-old one in
                    ;; the cache, the following times we find an
                    ;; 'old one
                    (if (memq prev-age '(obsolete-old old))
                        (setq age2 'old)
                      (setq age2 'new)))
                  (if (eq prev-age 'immortal)
                      (setq age2 'immortal))
                  (setq time (newsticker--time old-item)))
              ;; item was not there
              (setq item-new-p t)
              (setq something-was-added t))
            (let ((extra-elements-with-guid (funcall extra-fn node)))
              (unless (assoc 'guid extra-elements-with-guid)
                (setq extra-elements-with-guid
                      (cons `(guid nil ,(funcall guid-fn node))
                            extra-elements-with-guid)))
              (setq newsticker--cache
                    (newsticker--cache-add
                     newsticker--cache (intern name) title desc link
                     time age1 position extra-elements-with-guid
                     time age2)))
            (when item-new-p
              (let ((item (newsticker--cache-contains
                           newsticker--cache (intern name) title
                           desc link nil)))
                (if newsticker-auto-mark-filter-list
                    (newsticker--run-auto-mark-filter name item))
                (run-hook-with-args
                 'newsticker-new-item-functions name item)))))))
    something-was-added))

;; ======================================================================
;;; Misc
;; ======================================================================

(defun newsticker--insert-bytes (bytes)
  "Decode BYTES and insert in current buffer."
  (insert (decode-coding-string bytes 'binary)))

(defun newsticker--remove-whitespace (string)
  "Remove leading and trailing whitespace from STRING."
  (when (stringp string)
    (string-trim string)))

(defun newsticker--do-forget-preformatted (item)
  "Forget pre-formatted data for ITEM.
Remove the pre-formatted from `newsticker--cache'."
  (if (nthcdr 7 item)
      (setcar (nthcdr 7 item) nil))
  (if (nthcdr 6 item)
      (setcar (nthcdr 6 item) nil)))

(defun newsticker--forget-preformatted ()
  "Forget all cached pre-formatted data.
Remove the pre-formatted from `newsticker--cache'."
  (mapc (lambda (feed)
          (mapc #'newsticker--do-forget-preformatted
                (cdr feed)))
        newsticker--cache)
  (when (fboundp 'newsticker--buffer-set-uptodate)
    (newsticker--buffer-set-uptodate nil)))

(defun newsticker--debug-msg (string &rest args)
  "Print newsticker debug messages.
This function calls `message' with arguments STRING and ARGS, if
`newsticker-debug' is non-nil."
  (and newsticker-debug
       ;;(not (active-minibuffer-window))
       ;;(not (current-message))
       (apply #'message string args)))

(defun newsticker--decode-iso8601-date (string)
  "Return ISO8601-encoded STRING in format like `encode-time'.
Converts from ISO-8601 to Emacs representation.  If no time zone
is present, this function defaults to universal time."
  (if string
      (condition-case nil
          (encode-time (decoded-time-set-defaults (iso8601-parse string) 0))
        (wrong-type-argument
         (message "Cannot decode \"%s\"" string)
         nil))
    nil))

(defun newsticker--decode-rfc822-date (rfc822-string)
  "Convert RFC822-STRING to a Lisp timestamp.
RFC822-STRING should use RFC 822 (or later) format.
Examples:
Sat, 07 September 2002 00:00:01 +0100
Sat, 07 September 2002 00:00:01 MET
Sat, 07 Sep 2002 00:00:01 GMT
07 Sep 2002 00:00:01 GMT
07 Sep 2002"
  (if (and rfc822-string (stringp rfc822-string))
      (when (string-match
             (concat
              "\\s-*"
              ;; week day
              "\\(\\(Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\)\\s-*,?\\)?\\s-*"
              ;; day
              "\\([0-9]\\{1,2\\}\\)\\s-+"
              ;; month
              "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|"
              "Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\).*?\\s-+"
              ;; year
              "\\([0-9]\\{2,4\\}\\)"
              ;; time may be missing
              "\\(\\s-+"
              ;; hour
              "\\([0-9]\\{2\\}\\)"
              ;; minute
              ":\\([0-9]\\{2\\}\\)"
              ;; second
              "\\(:\\([0-9]\\{2\\}\\)\\)?"
              ;; zone
              "\\(\\s-+\\("
              "UT\\|GMT\\|EST\\|EDT\\|CST\\|CDT\\|MST\\|MDT\\|PST\\|PDT"
              "\\|\\([-+]\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)"
              "\\)\\)?"
              "\\)?")
             rfc822-string)
        (let ((day (read (match-string 3 rfc822-string)))
              (month-name (match-string 4 rfc822-string))
              (month 0)
              (year (read (match-string 5 rfc822-string)))
              (hour (read (or (match-string 7 rfc822-string) "0")))
              (minute (read (or (match-string 8 rfc822-string) "0")))
              (second (read (or (match-string 10 rfc822-string) "0")))
              (zone (match-string 12 rfc822-string))
              (sign (match-string 13 rfc822-string))
              (offset-hour (read (or (match-string 14 rfc822-string)
                                     "0")))
              (offset-minute (read (or (match-string 15 rfc822-string)
                                       "0"))))
          (when zone
            (cond ((string= sign "+")
                   (setq hour (- hour offset-hour))
                   (setq minute (- minute offset-minute)))
                  ((string= sign "-")
                   (setq hour (+ hour offset-hour))
                   (setq minute (+ minute offset-minute)))
                  ((or (string= zone "UT") (string= zone "GMT"))
                   nil)
                  ((string= zone "EDT")
                   (setq hour (+ hour 4)))
                  ((or (string= zone "EST") (string= zone "CDT"))
                   (setq hour (+ hour 5)))
                  ((or (string= zone "CST") (string= zone "MDT"))
                   (setq hour (+ hour 6)))
                  ((or (string= zone "MST") (string= zone "PDT"))
                   (setq hour (+ hour 7)))
                  ((string= zone "PST")
                   (setq hour (+ hour 8)))))
          (condition-case error-data
              (let ((i 1))
                (dolist (m '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
                             "Sep" "Oct" "Nov" "Dec"))
                  (if (string= month-name m)
                      (setq month i))
                  (setq i (1+ i)))
                (encode-time second minute hour day month year t))
            (error
             (message "Cannot decode \"%s\": %s %s" rfc822-string
                      (car error-data) (cdr error-data))
             nil))))
    nil))

(defun newsticker--update-process-ids ()
  "Update list of ids of active newsticker processes.
Checks list of active processes against list of newsticker processes."
  (let ((new-list nil))
    (dolist (proc (process-list))
      (let ((id (process-id proc)))
        (if (memq id newsticker--process-ids)
            (setq new-list (cons id new-list)))))
    (setq newsticker--process-ids new-list))
  (force-mode-line-update))

;; ======================================================================
;;; Images
;; ======================================================================
(defun newsticker--images-dir ()
  "Return directory where feed images are saved."
  (expand-file-name "images/" newsticker-dir))

(defun newsticker--icons-dir ()
  "Return directory where feed icons are saved."
  (expand-file-name "icons/" newsticker-dir))

(defun newsticker--image-get (feed-name filename directory url)
  "Get image for FEED-NAME by returning FILENAME from DIRECTORY.
If the file does no exist or if it is older than 24 hours
download it from URL first."
  (let ((image-name (concat directory feed-name)))
    (if (and (file-exists-p image-name)
             (time-less-p nil
                          (time-add (file-attribute-modification-time
				     (file-attributes image-name))
				    86400)))
        (newsticker--debug-msg "%s: Getting image for %s skipped"
                               (format-time-string "%A, %H:%M")
                               feed-name)
      ;; download
      (newsticker--debug-msg "%s: Getting image for %s"
                             (format-time-string "%A, %H:%M")
                             feed-name)
      (if (eq newsticker-retrieval-method 'intern)
          (newsticker--image-download-by-url feed-name filename directory url)
        (newsticker--image-download-by-wget feed-name filename directory url)))))

(defun newsticker--image-download-by-wget (feed-name filename directory url)
  "Download image for FEED-NAME using external program.
Save image as FILENAME in DIRECTORY, download it from URL."
  (let* ((proc-name (concat feed-name "-" filename))
         (buffername (concat " *newsticker-wget-image-" proc-name "*"))
         (item (or (assoc feed-name newsticker-url-list)
                   (assoc feed-name newsticker-url-list-defaults)
                   (error
                    "Cannot get image for %s: Check newsticker-url-list"
                    feed-name)))
         (wget-arguments (or (car (cdr (cdr (cdr (cdr item)))))
                             newsticker-wget-arguments)))
    (with-current-buffer (get-buffer-create buffername)
      (erase-buffer)
      ;; throw an error if there is an old wget-process around
      (if (get-process feed-name)
          (error "Another wget-process is running for image %s"
                 feed-name))
      ;; start wget
      (let* ((args (append wget-arguments (list url)))
             (proc (apply #'start-process proc-name buffername
                          newsticker-wget-name args)))
        (set-process-coding-system proc 'no-conversion 'no-conversion)
        (set-process-sentinel proc #'newsticker--image-sentinel)
        (process-put proc 'nt-directory directory)
        (process-put proc 'nt-feed-name feed-name)
        (process-put proc 'nt-filename filename)))))

(defun newsticker--image-sentinel (process _event)
  "Sentinel for image-retrieving PROCESS caused by EVENT."
  (let* ((p-status (process-status process))
         (exit-status (process-exit-status process))
         (feed-name (process-get process 'nt-feed-name))
         (directory (process-get process 'nt-directory))
         (filename (process-get process 'nt-filename)))
    ;; catch known errors (zombie processes, rubbish-xml, etc.)
    ;; if an error occurs the news feed is not updated!
    (catch 'oops
      (unless (and (eq p-status 'exit)
                   (= exit-status 0))
        (message "%s: Error while retrieving image from %s"
                 (format-time-string "%A, %H:%M")
                 feed-name)
        (newsticker--image-remove directory feed-name)
        (throw 'oops nil))
      (newsticker--image-save (process-buffer process) directory filename))))

(defun newsticker--image-save (buffer directory file-name)
  "Save contents of BUFFER in DIRECTORY as FILE-NAME.
Finally kill buffer."
  (with-current-buffer buffer
    (let ((image-name (concat directory file-name)))
      (set-buffer-file-coding-system 'no-conversion)
      ;; make sure the cache dir exists
      (unless (file-directory-p directory)
        (make-directory directory))
      ;; write and close buffer
      (let ((require-final-newline nil)
            (backup-inhibited t)
            (coding-system-for-write 'no-conversion))
        (write-region nil nil image-name nil 'quiet))
      (set-buffer-modified-p nil)
      (kill-buffer buffer))))

(defun newsticker--image-remove (directory file-name)
  "In DIRECTORY remove FILE-NAME."
  (let ((image-name (concat directory file-name)))
    (when (file-exists-p file-name)
      (delete-file image-name))))

(defun newsticker--image-download-by-url (feed-name filename directory url)
  "Download image for FEED-NAME using `url-retrieve'.
Save image as FILENAME in DIRECTORY, download it from URL."
  (let ((coding-system-for-read 'no-conversion))
    (condition-case error-data
        (url-retrieve url 'newsticker--image-download-by-url-callback
                      (list feed-name directory filename))
      (error (message "Error retrieving image from %s: %s" feed-name
                      error-data))))
  (force-mode-line-update))

(defun newsticker--image-download-by-url-callback (status feed-name directory filename)
  "Callback function for `newsticker--image-download-by-url'.
STATUS is the return status as delivered by `url-retrieve'.
FEED-NAME is the name of the feed that the news were retrieved
from.
The image is saved in DIRECTORY as FILENAME."
  (let ((do-save
         (or (not status)
             ;; (let ((status-type (car status)))
             ;;   (cond ((eq status-type :redirect)
             ;;          ;; don't care about redirects
             ;;          t)
             ;;         ((eq status-type :error)
             ;;          ;; silently ignore errors
             ;;          nil)))
             (eq (car status) :redirect))))
    (when do-save
      (let ((buf (get-buffer-create (concat " *newsticker-url-image-" feed-name "-"
                                            directory "*")))
            (result (buffer-string)))
        (set-buffer buf)
        (erase-buffer)
        (newsticker--insert-bytes result)
        ;; remove MIME header
        (goto-char (point-min))
        (search-forward "\n\n")
        (delete-region (point-min) (point))
        ;; save
        (newsticker--image-save buf directory filename)))))

(defun newsticker--insert-image (img string)
  "Insert IMG with STRING at point."
  (insert-image img string))

;; ======================================================================
;;; HTML rendering
;; ======================================================================
(defun newsticker-htmlr-render (pos1 pos2) ;
  "Replacement for `htmlr-render'.
Renders the HTML code in the region POS1 to POS2 using htmlr."
  (let ((str (buffer-substring-no-properties pos1 pos2)))
    (delete-region pos1 pos2)
    (insert
     (with-temp-buffer
       (insert str)
       (goto-char (point-min))
       ;; begin original htmlr-render
       (when (fboundp 'htmlr-reset) (htmlr-reset))
       ;; something omitted here...
       (when (fboundp 'htmlr-step)
         (while (< (point) (point-max))
           (htmlr-step)))
       ;; end original htmlr-render
       (newsticker--remove-whitespace (buffer-string))))))

;; ======================================================================
;;; Manipulation of cached data
;; ======================================================================
(defun newsticker--cache-set-preformatted-contents (item contents)
  "Set preformatted contents of ITEM to CONTENTS."
  (if (nthcdr 6 item)
      (setcar (nthcdr 6 item) contents)
    (setcdr (nthcdr 5 item) (list contents))))

(defun newsticker--cache-set-preformatted-title (item title)
  "Set preformatted title of ITEM to TITLE."
  (if (nthcdr 7 item)
      (setcar (nthcdr 7 item) title)
    (setcdr (nthcdr 6 item) title)))

(defun newsticker--cache-replace-age (data feed old-age new-age)
  "Mark all items in DATA in FEED which carry age OLD-AGE with NEW-AGE.
If FEED is `any' it applies to all feeds.  If OLD-AGE is `any',
all marks are replaced by NEW-AGE.  Removes all pre-formatted contents."
  (mapc (lambda (a-feed)
          (when (or (eq feed 'any)
                    (eq (car a-feed) feed))
            (let ((items (cdr a-feed)))
              (mapc (lambda (item)
                      (when (or (eq old-age 'any)
                                (eq (newsticker--age item) old-age))
                        (setcar (nthcdr 4 item) new-age)
                        (newsticker--do-forget-preformatted item)))
                    items))))
        data)
  data)

(defun newsticker--cache-mark-expired (data feed old-age new-age time)
  "Mark all expired entries.
This function sets the age entries in DATA in the feed FEED.  If
an item's age is OLD-AGE it is set to NEW-AGE if the item is
older than TIME."
  (mapc
   (lambda (a-feed)
     (when (or (eq feed 'any)
               (eq (car a-feed) feed))
       (let ((items (cdr a-feed)))
         (mapc
          (lambda (item)
            (when (eq (newsticker--age item) old-age)
	      (let ((exp-time (time-add (newsticker--time item) time)))
                (when (time-less-p exp-time nil)
                  (newsticker--debug-msg
                   "Item `%s' from %s has expired on %s"
                   (newsticker--title item)
                   (format-time-string "%Y-%02m-%d, %H:%M"
                                       (newsticker--time item))
                   (format-time-string "%Y-%02m-%d, %H:%M" exp-time))
                  (setcar (nthcdr 4 item) new-age)))))
          items))))
   data)
  data)

(defun newsticker--cache-contains (data feed title desc link _age
                                        &optional guid)
  "Check DATA whether FEED contains an item with the given properties.
This function returns the contained item or nil if it is not
contained.
The properties which are checked are TITLE, DESC, LINK, AGE, and
GUID.  In general all properties must match in order to return a
certain item, except for the following cases.

If AGE equals `feed' the TITLE, DESCription and LINK do not
matter.  If DESC is nil it is ignored as well.  If
`newsticker-desc-comp-max' is non-nil, only the first
`newsticker-desc-comp-max' characters of DESC are taken into
account.

If GUID is non-nil it is sufficient to match this value, and the
other properties are ignored."
  ;;(newsticker--debug-msg "Looking for %s guid=%s" title  guid)
  (condition-case nil
      (catch 'found
        (when (and desc newsticker-desc-comp-max
                   (> (length desc) newsticker-desc-comp-max))
          (setq desc (substring desc 0 newsticker-desc-comp-max)))
        (mapc
         (lambda (this-feed)
           (when (eq (car this-feed) feed)
             (mapc (lambda (anitem)
                     (when (cond (guid
                                  ;; global unique id can match
                                  (string= guid (newsticker--guid anitem)))
                                 (t;;FIXME?
                                  (or
                                   ;; or title, desc, etc.
                                   (and
                                    ;;(or (not (eq age 'feed))
                                    ;;  (eq (newsticker--age anitem) 'feed))
                                    (string= (newsticker--title anitem)
                                             title)
                                    (or (not link)
                                        (string= (newsticker--link anitem)
                                                 link))
                                    (or (not desc)
                                        (if (and desc newsticker-desc-comp-max
                                                 (> (length (newsticker--desc
                                                             anitem))
                                                    newsticker-desc-comp-max))
                                            (string= (substring
                                                      (newsticker--desc anitem)
                                                      0
                                                      newsticker-desc-comp-max)
                                                     desc)
                                          (string= (newsticker--desc anitem)
                                                   desc)))))))
                       ;;(newsticker--debug-msg "Found %s guid=%s"
                       ;; (newsticker--title anitem)
                       ;;                     (newsticker--guid anitem))
                       (throw 'found anitem)))
                   (cdr this-feed))))
         data)
        ;;(newsticker--debug-msg "Found nothing")
        nil)
    (error nil)))

(defun newsticker--cache-add (data feed-name-symbol title desc link time age
                                   position extra-elements
                                   &optional updated-time updated-age
                                   preformatted-contents
                                   preformatted-title)
  "Add another item to cache data.
Add to DATA in the FEED-NAME-SYMBOL an item with TITLE, DESC,
LINK, TIME, AGE, POSITION, and EXTRA-ELEMENTS.  If this item is
contained already, its time is set to UPDATED-TIME, its mark is
set to UPDATED-AGE, and its pre-formatted contents is set to
PREFORMATTED-CONTENTS and PREFORMATTED-TITLE.  Returns the age
which the item got."
  (let* ((guid (newsticker--guid-to-string (assoc 'guid extra-elements)))
         (item (newsticker--cache-contains data feed-name-symbol title desc link
                                           age guid)))
    ;;(message "guid=%s" guid)
    (if item
        ;; does exist already -- change age, update time and position
        (progn
          ;;(newsticker--debug-msg "Updating item %s %s %s %s %s -> %s %s
          ;;                   (guid %s -> %s)"
          ;;                   feed-name-symbol title link time age
          ;;                 updated-time updated-age
          ;;               guid (newsticker--guid item))
          (if (nthcdr 5 item)
              (setcar (nthcdr 5 item) position)
            (setcdr (nthcdr 4 item) (list position)))
          (setcar (nthcdr 4 item) updated-age)
          (if updated-time
              (setcar (nthcdr 3 item) updated-time))
          ;; replace cached pre-formatted contents
	  (newsticker--cache-set-preformatted-contents
	   item preformatted-contents)
	  (newsticker--cache-set-preformatted-title
	   item preformatted-title))
      ;; did not exist or age equals 'feed-name-symbol
      (setq item (list title desc link time age position preformatted-contents
                       preformatted-title extra-elements))
      ;;(newsticker--debug-msg "Adding item %s" item)
      (let ((this-feed (assq feed-name-symbol data)))
        (if this-feed
            (setcdr this-feed (nconc (cdr this-feed) (list item)))
          ;; The feed is not contained.
          (setq data (append data (list (list feed-name-symbol item)))))))
    data))

(defun newsticker--cache-remove (data feed-symbol age)
  "Remove all entries from DATA in the feed FEED-SYMBOL with AGE.
FEED-SYMBOL may be `any'.  Entries from old feeds, which are no longer in
`newsticker-url-list' or `newsticker-url-list-defaults', are removed as
well."
  (let* ((pos data)
         (feed (car pos))
         (last-pos nil))
    (while feed
      (if (or (assoc (symbol-name (car feed)) newsticker-url-list)
              (assoc (symbol-name (car feed)) newsticker-url-list-defaults))
          ;; feed is still valid=active
          ;; (message "Keeping feed %s" (car feed))
          (if  (or (eq feed-symbol 'any)
                   (eq feed-symbol (car feed)))
              (let* ((item-pos (cdr feed))
                     (item (car item-pos))
                     (prev-pos nil))
                (while item
                  ;;(message "%s" (car item))
                  (if (eq age (newsticker--age item))
                      ;; remove this item
                      (progn
                        ;;(message "Removing item %s" (car item))
                        (if prev-pos
                            (setcdr prev-pos (cdr item-pos))
                          (setcdr feed (cdr item-pos))))
                    ;;(message "Keeping item %s" (car item))
                    (setq prev-pos item-pos))
                  (setq item-pos (cdr item-pos))
                  (setq item (car item-pos)))))
        ;; feed is not active anymore
        ;; (message "Removing feed %s" (car feed))
        (if last-pos
            (setcdr last-pos (cdr pos))
          (setq data (cdr pos))))
      (setq last-pos pos)
      (setq pos (cdr pos))
      (setq feed (car pos)))))

;; ======================================================================
;;; Sorting
;; ======================================================================
(defun newsticker--cache-item-compare-by-time (item1 item2)
  "Compare two news items ITEM1 and ITEM2 by comparing their time values."
  (catch 'result
    (let ((age1 (newsticker--age item1))
          (age2 (newsticker--age item2)))
      (if (not (eq age1 age2))
          (cond ((eq age1 'obsolete)
                 (throw 'result nil))
                ((eq age2 'obsolete)
                 (throw 'result t)))))
    (time-less-p (newsticker--time item2)
		 (newsticker--time item1))))

(defun newsticker--cache-item-compare-by-title (item1 item2)
  "Compare ITEM1 and ITEM2 by comparing their titles."
  (catch 'result
    (let ((age1 (newsticker--age item1))
          (age2 (newsticker--age item2)))
      (if (not (eq age1 age2))
          (cond ((eq age1 'obsolete)
                 (throw 'result nil))
                ((eq age2 'obsolete)
                 (throw 'result t)))))
    (string< (newsticker--title item1) (newsticker--title item2))))

(defun newsticker--cache-item-compare-by-position (item1 item2)
  "Compare ITEM1 and ITEM2 by comparing their original positions."
  (catch 'result
    (let ((age1 (newsticker--age item1))
          (age2 (newsticker--age item2)))
      (if (not (eq age1 age2))
          (cond ((eq age1 'obsolete)
                 (throw 'result nil))
                ((eq age2 'obsolete)
                 (throw 'result t)))))
    (< (or (newsticker--pos item1) 0) (or (newsticker--pos item2) 0))))

(defun newsticker--cache-get-feed (feed)
  "Return the cached data for the feed FEED.
FEED is a symbol!"
  (assoc feed newsticker--cache))

(defun newsticker--cache-dir ()
  "Return directory for saving cache data."
  (expand-file-name "feeds/" newsticker-dir))

(defun newsticker--cache-save ()
  "Save cache data for all feeds."
  (unless (file-directory-p newsticker-dir)
    (make-directory newsticker-dir t))
  (mapc #'newsticker--cache-save-feed newsticker--cache)
  nil)

(defun newsticker--cache-save-feed (feed)
  "Save cache data for FEED."
  (let ((dir (file-name-as-directory
              (expand-file-name (symbol-name (car feed))
                                (newsticker--cache-dir)))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (let ((coding-system-for-write 'utf-8))
      (with-temp-file (expand-file-name "data" dir)
        (insert ";; -*- coding: utf-8 -*-\n")
        (prin1 (cdr feed) (current-buffer) t)))))

(defun newsticker--cache-read ()
  "Read cache data."
  (setq newsticker--cache nil)
  (dolist (f (append newsticker-url-list-defaults newsticker-url-list))
    (newsticker--cache-read-feed (car f))))

(defun newsticker--cache-read-feed (feed-name)
  "Read cache data for feed named FEED-NAME."
  (let ((file-name (expand-file-name
                    "data" (expand-file-name
                            feed-name (newsticker--cache-dir))))
        (coding-system-for-read 'utf-8))
    (when (file-exists-p file-name)
      (with-temp-buffer
        (insert-file-contents file-name)
        (goto-char (point-min))
        (condition-case nil
            (add-to-list 'newsticker--cache (cons (intern feed-name)
                                                  (read (current-buffer))))
          (error
           (message "Error while reading newsticker cache file %s!"
                    file-name)
           (setq newsticker--cache nil)))))))

;; ======================================================================
;;; Statistics
;; ======================================================================
(defun newsticker--stat-num-items (feed &rest ages)
  "Return number of items in the given FEED which have one of the given AGES.
If AGES is nil, the total number of items is returned."
  (let ((items (cdr (newsticker--cache-get-feed feed)))
        (num 0))
    (while items
      (if ages
          (if (memq (newsticker--age (car items)) ages)
              (setq num (1+ num)))
        (if (memq (newsticker--age (car items)) '(new old immortal obsolete))
            (setq num (1+ num))))
      (setq items (cdr items)))
    num))

(defun newsticker--stat-num-items-total (&optional age)
  "Return total number of items in all feeds which have the given AGE.
If AGE is nil, the total number of items is returned."
  (apply #'+
         (mapcar (lambda (feed)
                   (if age
                       (newsticker--stat-num-items (intern (car feed)) age)
                     (newsticker--stat-num-items (intern (car feed)))))
                 (append newsticker-url-list-defaults newsticker-url-list))))

;; ======================================================================
;;; OPML
;; ======================================================================
(defun newsticker-opml-export ()
  "OPML subscription export.
Export subscriptions to a buffer in OPML Format."
  (interactive)
  (with-current-buffer (get-buffer-create "*OPML Export*")
    (erase-buffer)
    (set-buffer-file-coding-system 'utf-8)
    (insert (concat
             "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
             "<!-- OPML generated by Emacs newsticker.el -->\n"
             "<opml version=\"1.0\">\n"
             "  <head>\n"
             "    <title>Emacs newsticker subscriptions</title>\n"
             "    <dateCreated>" (format-time-string "%a, %d %b %Y %T %z")
             "</dateCreated>\n"
             "    <ownerEmail>" user-mail-address "</ownerEmail>\n"
             "    <ownerName>" (user-full-name) "</ownerName>\n"
             "  </head>\n"
             "  <body>\n"))
    (let ((feeds (append newsticker-url-list newsticker-url-list-defaults))
          ;; insert the feed groups and all feeds that are contained
          (saved-feed-names (newsticker--opml-insert-elt newsticker-groups 2)))
      ;; to be safe: insert all feeds that are not contained in any group
      (dolist (f feeds)
        (unless (seq-find (lambda (sfn) (string= (car f) sfn)) saved-feed-names)
          (newsticker--opml-insert-feed (car f) 4)))
      (insert "  </body>\n</opml>\n")))
  (pop-to-buffer "*OPML Export*")
  (sgml-mode))

(defun newsticker--opml-insert-elt (elt depth)
  "Insert an OPML ELT with indentation level DEPTH."
  (if (listp elt)
      (newsticker--opml-insert-group elt (+ 2 depth))
    (newsticker--opml-insert-feed elt (+ 2 depth))))

(defun newsticker--opml-insert-group (group depth)
  "Insert an OPML GROUP with indentation level DEPTH."
  (let (saved-feeds)
    (insert (make-string depth ? ) "<outline type=\"folder\" text=\"" (car group) "\">\n")
    (setq saved-feeds (mapcar (lambda (e)
                                (newsticker--opml-insert-elt e depth))
                              (cdr group)))
    (insert (make-string depth ? ) "</outline>\n")
    (flatten-tree saved-feeds)))

(defun newsticker--opml-insert-feed (feed-name depth)
  "Insert an OPML FEED-NAME with indentation level DEPTH."
  (let* ((feed-definition (seq-find (lambda (f)
                                      (string= feed-name (car f)))
                                    (append newsticker-url-list newsticker-url-list-defaults)))
         (url (nth 1 feed-definition))
         (url-string (if (functionp url) (prin1-to-string url)
                       (xml-escape-string url))))
    (insert (make-string depth ? ) "<outline text=\"" feed-name
            "\" xmlUrl=\"" url-string
            "\"/>\n"))
  feed-name)

(defun newsticker--opml-import-outlines (outlines)
  "Recursively import OUTLINES from OPML data."
  (mapcar (lambda (outline)
            (let ((name (xml-get-attribute outline 'text))
                  (url (xml-get-attribute outline 'xmlUrl))
                  (children (xml-get-children outline 'outline)))
              (unless (string= "" url)
                (add-to-list 'newsticker-url-list
                             (list name url nil nil nil) t))
              (if children
                        (append (list name)
                                (newsticker--opml-import-outlines children))
                      name)))
          outlines))

(defun newsticker-opml-import (filename)
  "Import OPML data from FILENAME.
Feeds are added to `newsticker-url-list' and `newsticker-groups'
preserving the outline structure."
  (interactive "fOPML file: ")
  (set-buffer (find-file-noselect filename))
  (goto-char (point-min))
  (let* ((node-list (xml-parse-region (point-min) (point-max)))
         (title (car (xml-node-children
                      (car (xml-get-children
                            (car (xml-get-children (car node-list) 'head))
                            'title)))))
         (body (car (xml-get-children (car node-list) 'body)))
         (outlines (xml-get-children body 'outline))
         (imported-groups-data (newsticker--opml-import-outlines outlines)))
    (add-to-list 'newsticker-groups (cons title imported-groups-data) t))
  (customize-variable 'newsticker-url-list))

;; ======================================================================
;;; Auto marking
;; ======================================================================
(defun newsticker--run-auto-mark-filter (feed item)
  "Automatically mark an item as old or immortal.
This function checks the variable `newsticker-auto-mark-filter-list'
for an entry that matches FEED and ITEM."
  (let ((case-fold-search t))
    (dolist (filter newsticker-auto-mark-filter-list)
      (let ((filter-feed (car filter))
            (pattern-list (cadr filter)))
        (when (string-match filter-feed feed)
          (newsticker--do-run-auto-mark-filter item pattern-list))))))

(defun newsticker--do-run-auto-mark-filter (item list)
  "Actually compare ITEM against the pattern-LIST.
LIST must be an element of `newsticker-auto-mark-filter-list'."
  (dolist (pattern list)
    (let ((place  (nth 1 pattern))
          (regexp (nth 2 pattern))
          (title (newsticker--title item))
          (desc  (newsticker--desc item)))
      (when (or (eq place 'title) (eq place 'all))
        (when (and title (string-match regexp title))
          (newsticker--process-auto-mark-filter-match item pattern)))
      (when (or (eq place 'description) (eq place 'all))
        (when (and desc (string-match regexp desc))
          (newsticker--process-auto-mark-filter-match item pattern))))))

(defun newsticker--process-auto-mark-filter-match (item pattern)
  "Process ITEM that matches an auto-mark-filter PATTERN."
  (let ((age (nth 0 pattern))
        (place  (nth 1 pattern))
        (regexp (nth 2 pattern)))
    (newsticker--debug-msg "Auto-mark-filter %s matches `%s'"
                           pattern (newsticker--title item))
    (setcar (nthcdr 4 item) age)
    (nconc (newsticker--extra item)
           (list (list 'newsticker-auto-mark nil
                       (format "age=%s, title/desc=%s, regexp=%s"
                               age place regexp))))))

;; ======================================================================
;;; Hook samples
;; ======================================================================
(defun newsticker-new-item-functions-sample (feedname item)
  "Demonstrate the use of the `newsticker-new-item-functions' hook.
This function just prints out the values of the FEEDNAME and title of the ITEM."
  (message (concat "newsticker-new-item-functions-sample: feed=`%s', "
                   "title=`%s'")
           feedname (newsticker--title item)))

(defun newsticker-download-images (feedname item)
  "Download the first image.
If FEEDNAME equals \"imagefeed\" download the first image URL
found in the description=contents of ITEM to the directory
`temporary-file-directory'/newsticker/FEEDNAME/TITLE where TITLE
is the title of the item."
  (when (string= feedname "imagefeed")
    (let ((title (newsticker--title item))
          (desc (newsticker--desc item)))
      (when (string-match "<img src=\"\\(http://[^ \"]+\\)\"" desc)
        (let ((url (substring desc (match-beginning 1) (match-end 1)))
		(temp-dir (file-name-as-directory
                           (expand-file-name
                            title (expand-file-name
                                   feedname (expand-file-name
                                             "newsticker"
                                             temporary-file-directory)))))
              (org-dir default-directory))
          (unless (file-directory-p temp-dir)
            (make-directory temp-dir t))
          (cd temp-dir)
          (message "Getting image %s" url)
          (apply #'start-process "wget-image"
                 " *newsticker-wget-download-images*"
                 newsticker-wget-name
                 (list url))
          (cd org-dir))))))

(defun newsticker-download-enclosures (feedname item)
  "In all feeds download the enclosed object of the news ITEM.
The object is saved to the directory
`temporary-file-directory'/newsticker/FEEDNAME/TITLE, which
is created if it does not exist.  TITLE is the title of the news
item.  Argument FEEDNAME is ignored.
This function is suited for adding it to `newsticker-new-item-functions'."
  (let ((title (newsticker--title item))
        (enclosure (newsticker--enclosure item)))
    (when enclosure
      (let ((url (cdr (assoc 'url enclosure)))
            (temp-dir (file-name-as-directory
                       (expand-file-name
                        title (expand-file-name
                               feedname (expand-file-name
                                         "newsticker"
                                         temporary-file-directory)))))
            (org-dir default-directory))
        (unless (file-directory-p temp-dir)
          (make-directory temp-dir t))
        (cd temp-dir)
        (message "Getting enclosure %s" url)
        (apply #'start-process "wget-enclosure"
               " *newsticker-wget-download-enclosures*"
               newsticker-wget-name
               (list url))
        (cd org-dir)))))

;; ======================================================================
;;; Retrieve samples
;; ======================================================================
(defun newsticker-retrieve-random-message (_feed-name)
  "Return an artificial RSS string under the name FEED-NAME."
  (concat "<?xml version=\"1.0\" encoding=\"iso-8859-1\" ?><rss version=\"0.91\">"
          "<channel>"
          "<title>newsticker-retrieve-random-message</title>"
          "<description>Sample retrieval function</description>"
          "<pubDate>FIXME Sat, 07 Sep 2005 00:00:01 GMT</pubDate>"
          "<item><title>" (format "Your lucky number is %d" (random 10000))
          "</title><description>" (format "Or maybe it is %d" (random 10000))
          "</description></item></channel></rss>"))

(provide 'newst-backend)

;;; newst-backend.el ends here
