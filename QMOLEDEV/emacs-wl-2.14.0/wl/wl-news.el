;;; wl-news.el --- Create notification from NEWS(.ja) for Wanderlust.

;; Copyright (C) 2002 Yoichi NAKAYAMA <yoichi@eken.phys.nagoya-u.ac.jp>
;; Copyright (C) 2002 Kenichi OKADA <okada@opaopa.org>

;; Author: Yoichi NAKAYAMA <yoichi@eken.phys.nagoya-u.ac.jp>
;;	Kenichi OKADA <okada@opaopa.org>
;; Keywords: mail, net news

;; This file is part of Wanderlust (Yet Another Message Interface on Emacsen).

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:
;;

;;; Code:
;;

(require 'elmo)
(require 'wl-vars)
(require 'wl-util)
(require 'wl-address)
(require 'wl-folder)

(defvar wl-news-version-file-name "previous-version")
(defvar wl-news-default-previous-version '(2 0 0))

(defvar wl-news-lang
  (if (and (boundp 'current-language-environment)
	   (string-equal "Japanese"
			 (symbol-value 'current-language-environment)))
      '("ja" "en") '("en" "ja"))
	"The list of languages to show NEWS. (order sensitive)")

(defun wl-news-check ()
  (let* ((updated (not (wl-news-already-current-p))))
    (if updated
	(if (and wl-news-lang
		 (wl-news-check-news
		  (cdr (wl-news-previous-version-load))
		  wl-news-lang)
		 (not (memq 'wl-news wl-hook)))
	    (add-hook 'wl-hook 'wl-news))
      ;; update wl-news-version-file
      (wl-news-previous-version-save
       (product-version (product-find 'wl-version))
       (cdr (wl-news-previous-version-load))))
    updated))

;;; -*- news-list -*-

(defconst wl-news-news-alist
  '(("en" ((2 14 0) . "* Changes in 2.14.0 from 2.12.2

** New folder type `access' is added.
   In `access' folder, sub-folders of the specified folder can be treated
   as one folder.

** Synchronization speed of the folder is improved.
   The function which calculates list diff is re-wrote and is faster
   than previous implementation, especially in the folders with large
   number of the messages.

** New event handling mechanism is incorporated.

** Improved the disconnected operations for IMAP draft saving.
   There was a bug of message numbering in the disconnected imap draft.
   It is fixed using new event handling mechanism.

** `Shimbun' summary buffers are updated dynamically.
   Some shimbun folder does not have correct information.
   In this version, they are corrected using the message body after retrieval.
   It is implemented with new event handling mechanism.

** Many bug fixes.
") ((2 12 2) . "* Changes in 2.12.2 from 2.12.0
  Version 2.12.2 is a bug fix version of 2.12.0.

** Now Maildir is usable on Windows systems.
   Note that it does not conform to the Maildir standard.

** Fixed the problem of the cache flag inconsistency on the filter folder etc.
   There was a problem that the summary buffer displays cached messages as
   uncached in some folders.

** Fixed the bug that the new flag cannot be changed in some cases.

** Fixed the bug that flag are not taken over correctly from Maildir.
   Only the flag of the first message was taken over in earlier versions.

** Fixed the problem in display module for IMAP messages.
   Now partially fetched messages are displayed correctly.
   If a message included child messages, their headers were not displayed.

** Fixed the problem that %INBOX is not appeared as a subfolder of %INBOX.
   In cyrus-imapd, this problem occurred.

** Now user defined flags are appeared in the completions for search conditions.

** Fixed the problem that a wrong flag folders are created by some flag names.
   If a flag contains a character other than [a-z], the problem occurred.

** Now expansion of the access group \"@/\" works correctly.

** Fixed the problem to cause an error on automatic draft saving.

** Fixed the problem to cause an error on invoking address manager.
   A message which includes a string \"To:\" etc. caused an error.

** Fixed the problem in the flag inheritance function of the filter folder.

** New option `wl-summary-resend-use-cache'.
   You can resend messages using cache in the offline status.

** New option `elmo-network-session-idle-timeout'.
   Network sessions which are not used longer than this value (in seconds)
   are thrown away and new session is created.

** Improved redisplay functions for \"H\" and \"M\" key.
   MIME structure and buffer is reused for redisplay.

** Now attributes for netnews are displayed in the draft preview.
") ((2 12 0) . "* Changes in 2.12.0 from 2.10.1

** The structure of the message database is improved.
   Following setting is to convert legacy msgdb to the new one when you
   select a folder.
   (setq elmo-msgdb-default-type 'standard
         elmo-msgdb-convert-type 'auto)
   (which is initial setting.)

** The temporary mark and corresponding action is now customizable.

   By default, following mark-and-actions are defined.
   mark-and-actions which are defined as before.
    \"o\" refile   (same as before)
    \"O\" copy     (same as before)
    \"d\" dispose  (formerly delete, 'D' mark. Messages are moved to
    		  wl-trash-folder. Its behavior is decided by
		  wl-dispose-folder-alist.)
   New mark-and-actions which are newly introduced.
    \"D\" delete   (remove message immediately)
    \"i\" prefetch (prefetch message)
    \"~\" resend   (resend message)
   Press 'x' to execute actions which corresponds to the mark.
   mark-and-actions can be define by the new variable, 
   'wl-summary-mark-action-list'. See its docstring for more in detail.

** SPAM filter module is added.
   Following spam filter libraries are supported.
   bogofilter
   spamfilter
   bsfilter
   SpamAssassin
   SpamOracle
   Regular Expressions Header Matching

** 'mark folder is renamed to 'flag folder.
   Related to this, original message location is displayed as help-echo on summary
   line in the 'flag folder (you can control the behavior by
   wl-highlight-summary-line-help-echo-alist).

** Now you can put arbitrary user defined flag on message.
   You can specify its flag by \"F\" in the summary mode.

** New marks, 'A' and 'a' are added for answered messages.
   Now answered messages have its own mark in the summary mode.
   'A' is displayed for uncached messages and 'a' is for cached messages.

** New mark,s 'F' and 'f' are added for forwarded messages.
   Now forwarded messages have its own mark in the summary mode.
   'F' is displayed for uncached messages and 'f' is for cached messages.

** New search condition 'Flag' (Status of the message) is added.
   There are flags 'unread', 'important', 'answered',
   'digest' (unread or important) and 'any' (any of the flag).
   For example, following filter folder contains only unread or important
   messages in the %inbox folder.

   /flag:digest/%inbox

** Draft save function is improved.
   Now you can set wl-draft-folder to IMAP folder, Maildir folder, and so on.

** Automatically save draft buffers by using idle-timer.
   You can control behavior by the variable `wl-auto-save-drafts-interval'.

** 'H' key(display all header) and 'M' key(display without MIME analysis)
   are now toggle key.
   Now you can cite messages displayed by 'M'.

** Now you can sort summary lines into descending order.

** Abbreviate too long header extended to lines in message buffer.

** Persistent mark string in summary buffer is changed.
   Default setting indicates cached state by its upper/lower case.

** It displays draft preview on sending confirmation.

** Sending parameters are displayed on draft preview.
   See description of the variable wl-draft-preview-attribute for detail.

** You can run biff with idle-timer by setting wl-biff-use-idle-timer.

** Now wl-draft-kill confirms with yes-or-no-p.

** Summary thread will be divided if its depth is larger than certain amount.
   The limit is controlled by the variable wl-summary-max-thread-depth.

** Emacs multi-tty support is supported.
   (http://lorentey.hu/project/emacs.html)

** New sort spec 'size' is added in the summary mode.
   Now you can sort the summary by message size.

** The variable wl-refile-policy-alist is abolished.

** Batch processing module is added.

** In the multi-folder, status of messages are synchronized with original
   folder.
   For example, unread status of '+inbox' is updated to '*+inbox,+outbox'.

** The function wl-summary-resend-message is abolished.
   you can put mark for resending by wl-summary-resend instead.

** Variables renamed
   wl-delete-folder-alist is renamed to wl-dispose-folder-alist.

** POP3 folder existence check is simplified (by default).
   The default value for elmo-pop3-exists-exactly is changed to nil.

** POP3 response code extension defined in the RFC2449 is supported.
   If a login failure occurred because of user's another POP3 session, 
   entered password is not cleared and used in the future login.

** IMAP4 commands EXPUNGE and CHECK are now send asynchronously.

** Default value of wl-folder-hierarchy-access-folders has been changed.

** Access group \"@/\" of shimbun folders can be used now.

** Show contents of NEWS(.ja) when you start Wanderlust newer than the
   one you used previously.

** Default values of wl-draft-reply-*-list are changed. 
   See samples/en/dot.wl for old values.

** wl-draft-reply-myself-*-list are abolished and integrated into
   wl-draft-reply-*-list.

** You can control initial cursor position for replying draft.
   Set variable wl-draft-reply-default-position appropriately.

** Changed the way to specify configuration of draft buffer window.
   You can choose keep, full or split as values of wl-draft-buffer-style
   and wl-draft-reply-buffer-style.

** Commands to verify/decrypt non-MIME PGP message are added.
   C-c:v, C-c:d in message buffer to verify or decrypt respectively.

** New hooks
   wl-draft-reply-hook
   wl-summary-reply-hook
   wl-draft-forward-hook
   wl-summary-forward-hook
   wl-draft-kill-pre-hook
   wl-summary-resend-hook

** Abolished hook
   wl-reply-hook

** New face

   wl-highlight-summary-disposed-face
   wl-highlight-summary-prefetch-face
   wl-highlight-summary-resend-face
   wl-highlight-summary-answered-face
   wl-highlight-action-argument-face

** Abolished face

   wl-highlight-refile-destination-face
   (renamed to wl-highlight-action-argument-face)
") ((2 10 1) . "* Changes in 2.10.1 from 2.10.0
  Version 2.10.1 is a bug fix version of 2.10.0.

** Fixed the problem that msgdb be destroyed when print-length or
   print-level has Non-nil value.

** wl-summary-pack-number in pipe folder is disabled temporarily
   since it didn't work. Invoke it in destination folder instead.

** Fixed a problem that wl-folder-move-cur-folder doesn't work.

** Fixed a problem that wl-draft-reedit doesn't work properly on Meadow.

** Fixed a problem that wl-summary-pack-number doesn't work on Maildir and
   shimbun folders.

** Fixed a problem that cache file is not protected even if it is marked
   as important.

** Fixed a problem that %# in wl-summary-line-format cannot handle large
   number.

** Fixed a problem to remove password even if SMTP AUTH failed at non-auth
   phase.

** Default value of wl-message-buffer-prefetch-folder-type-list,
   wl-message-buffer-prefetch-idle-time, and
   wl-message-buffer-prefetch-depth are changed.

** Fixed to compile on XEmacs without mule feature.
") ((2 10 0) . "* Changes in 2.10.0 from 2.8.1

** You can alter the format of summary lines.
   Specify format by wl-summary-line-format. If you want to change ones
   according to folder names, use wl-folder-summary-line-format-alist.

** Save format for the draft folder has been changed. Messages are encoded
   before saved by wl-draft-save.

** elmo-split is newly established. It provides a way to split messages
   according to some rule a la procmail.

** Buffer prefetch works fine now. Messages of the number specified by
   wl-message-buffer-prefetch-depth are loaded into buffer in advance.

** elmo-dop-queue-flush flushes queue that concerns plugged folder.

** Starting Wanderlust on the new frame is possible now. Set as
   (autoload 'wl-other-frame \"wl\" \"Wanderlust on new frame.\" t)

** In Folder mode, you can go into virtual folder which consists of messages
   with some specified condition (wl-folder-virtual). It is binded to \"V\".

** In Folder mode, you can search folders containing messages with some
   specified condition (wl-folder-pick). It is binded to \"?\".

** Now you can rename access group folders.

** You can specify ON/OFF of thread view for newly created summary.
   Set wl-summary-default-view, wl-summary-default-view-alist.

** Temporary marks are kept when you exit from sticky summary by q or g. 

** Key bindings concerning the sticky summary have been changed.
   By C-u g, the sticky summary is destroyed as well as C-u q. In summary or
   folder mode, G opens the sticky summary.

** You can go round summary buffers by C-cC-n and C-cC-p.

** Members of the list wl-folder-hierarchy-access-folders is now some REGEXP
   for access group names instead of exact group names.

** In header part of the draft buffer C-a brings cursor to the beginning of
   the line or the beginning of the header body.

** You can send encapsulated blind carbon copies. Its default field name is
   \"Ecc:\".

** C-c C-y (Draft) can cite region of the message.
   It affects if transient-mark-mode (Emacs) or zmacs-regions (XEmacs) is
   Non-nil and the region is active.

** You can delete a part from multipart message.
   It is binded as \"D\" in message buffer.

** You can easily configure server settings to post news article.
   Set wl-nntp-posting-config-alist appropriately. See Info for an example.

** You can specify some function in wl-draft-reply-with-argument-list etc.
   for setting the recipients in draft by the return value of it.

** The interface of the function wl-draft has been changed.
   The initial set of headers are handed as an association list.

** The uses of wl-generate-mailer-string-function has been changed.
   Specify a function which returns some string to appear in User-Agent header.

** The Reference Card (doc/wl-refcard.tex) describes important key bindings.

** Many bug fixes.
") ((2 8 0) . "* Changes in 2.8.0 from 2.6.1

** Nemacs, Mule 2.3 based on Emacs 19.28 are not supported any longer.

** Wanderlust might not work with FLIM 1.14.2 and older.
   It is recommended to use FLIM 1.14.3 or newer and associated SEMI.

** Now available `make check' environment test for user.

** If you set obsolete variables (e.g. renamed ones) in .wl etc, Wanderlust
   shows warning messages and urge you to change settings.
   Change your settings according to the messages, please.
   If you want to suppress warnings, set elmo-obsolete-variable-show-warnings
   to nil.

** Added new internal folders: 'sendlog folder

** Added new type of folders: shimbun folder

   Format: '@' 'virtual server name' '.' 'group name'

** Added new type of folders: namazu folder

   Format:  '[' 'search condition' ']' [ 'absolute path of namazu index' ]

** With pipe folder, now you can preserve messages on the server.
   At the next time you access it, only new messages will be copied.

   Format:  '|' 'source folder' '|:' 'destination folder'

** Address manager is now available (start by C-c C-a).
   You can edit address book and import recipients to draft from it.

** ACAP (RFC2244) is supported(experimental).

** Now you can preserve IMAP4 message by part as a cache.
   If you skipped enormous part, you can read other than skipped part when
   you are off line.

** Wanderlust also creates message view through prefetching.
   Displaying of prefetched messages speeded up because of this.

** Truncation of lines in message buffer or draft buffer is now controllable.
   Non-nil value of wl-message-truncate-lines or wl-draft-truncate-lines
   means truncating long lines at window width.

** Bitmap image for opening demo is removed from wl-demo.elc and now loaded
   from wl-icon-directory.
   Special logo is displayed through the Christmas season :)

** Overall elmo module is rewritten.

** Variables depending on elmo backends are renamed to \"elmo-backend-*\".
   e.g. elmo-default-imap4-server is renamed to elmo-imap4-default-server.

** Variables named xxx-func are renamed to xxx-function.

** X-Face utility 1.3.6.12 or older is not supported any longer.
   Please install X-Face utility 1.3.6.13 or later, if necessary.

** Wanderlust distinguishes stream-type on plugged mode. They are treated as
   different entries.

** msgdb path for archive and multi folders are changed.
   No problem for running wanderlust even if you do not deal with them.
   But if you don't want to leave useless data on the disk, delete under
   .elmo/multi and .elmo/archive in advance.

** Variables named xxx-dir are renamed to xxx-directory.
   e.g. wl-icon-dir is renamed to wl-icon-directory.
   Take attention if you set for display of startup logo, etc.

** elmo-cache-dirname is abolished and elmo-cache-directory is newly created.
   You can put cache directory to another place by setting
   elmo-cache-directory.

** Default value of elmo-enable-disconnected-operation is now `t'.
   When the relevant messages are cached, you can do some operations
   even in the off-line state.

** Now messages with \"$\" mark is not remained in the summary buffer when
   the actual message itself is deleted.
   Please visit the 'mark folder to review the messages with the \"$\" mark.
") ((2 6 1) . "* Changes in 2.6.1 from 2.6.0
  Version 2.6.1 is basically a bug fix version of 2.6.0.

** Fixed a problem that Emacs 21 causes `Recursive load...' error.

** Fixed a problem that thread character is broken in XEmacs 21.1.

** Fixed a problem that in IMAP4 folder, progress bar is remained in XEmacs .

** Fixed a problem that searching is failed for the header fields that
   begins with X-.

** Some other fixes.
") ((2 6 0) . "* Changes in 2.6.0 from 2.4.1

** FLIM 1.13.x is not supported any longer.
   Please install FLIM 1.14.1 or later.

** Now folder and summary buffer can be opened in a separate frame.
   If `wl-folder-use-frame' is set as t, `M-x wl' creates a new frame
   for folder mode. If `wl-summary-use-frame' is set as t, new frames
   are created for each summary window.

** Cursor moving speed ('N' or 'P' in summary) is greatly improved.

** Folder checking speed for filter folder of localdir
   folder using `last' or `first' (Ex. /last:100/+inbox) is improved.

** Retrieval progress of each message is displayed in POP and IMAP folder.

** Coloring of summary buffer is processed on demand (only on Emacs).
   If `wl-summary-lazy-highlight' is non-nil, 
   only visible portion of the buffer is colored.

** Customizable biff notify.
   New hook `wl-biff-notify-hook' and `wl-biff-unnotify-hook' is
   now available.
   e.g. (add-hook wl-biff-notify-hook 'ding)

** Many bug fixes.
") ((2 4 1) . "* Changes in 2.4.1 from 2.4.0
  Version 2.4.1 is basically a bug fix version of 2.4.0.

** Wanderlust 2.4.1 now works on FLIM 1.14.x. 

** Fixed a problem that POP connection remains after POP before SMTP.

** The specification of IMAP4 authentication method for clear password
   is changed.

In 2.4.0, To use clear password authentication method in IMAP4
\(Logging in with LOGIN command), you have to set the variable
`elmo-default-imap4-authenticate-type' as 'plain (or nil).
But in 2.4.1, it is changed to 'clear (or nil).
Example:
\(setq elmo-default-imap4-authenticate-type 'plain)
should be changed to
\(setq elmo-default-imap4-authenticate-type 'clear)
") ((2 4 0) . "* Changes in 2.4.0 from 1.1.1

** Version Number
The version numbering convention for Wanderlust is changed. 

In earlier versions, 1.x were stable version and from 2.0.x to 2.2.x
were beta version. But since version 2.3.0, the second (minor) version
number implies the stability of the Wanderlust. Even minor number
corresponds to a stable version, and an odd minor number corresponds
to a development version. This version numbering is based on the
widespread convention of open source development.

On the open CVS server cvs.m17n.org, main trunk contains the current
beta (newest experimental) version, and branches contain the stable
version.  (If the version is 2.4.x, the branch name is wl-2_4)

** Install

*** FLIM 1.12 is not supported anymore.
See the file INSTALL for details.

*** APEL 10.2 or later is required.
tm-8 users should check the version of APEL (tm-8.8 contains old APEL).

** New feature

*** LDAP support
Complete e-mail address in draft by searching LDAP server.
If the variable wl-use-ldap is non-nil, LDAP feature is enabled
\(Initial setting is nil).

*** UIDL support in POP3 folder
POP3 folder now saves the status of summary and it improves summary
update speed. If the variable elmo-pop3-use-uidl is non-nil, UIDL is
used (Initial setting is t).

*** Emacs 21 support
Wanderlust has started on supporting Standard Emacs 21.
Toolbars and icon images can be shown in almost Wanderlust
frames like XEmacs.

*** biff feature
Server mailbox is checked periodically.
If new mail is arrived, Wanderlust changes the biff (icon) on the modeline
and updates folder mode content.

*** expire-hide 
Now expire mechanism has new feature `hide', it does not remove
messages actually from folder but hides messages from summary. It
improves processing speed for large folders.

*** Message thread restoring feature
Automatic correction of broken threads by subject matching is now available.
Thread modification by hand (M-w (copy) and C-y (paste) in summary mode)
is also available.

*** Password expiration timer
Password cache expires after elmo-passwd-life-time is passed.
\(nil means no expiration. Initial setting is nil)

*** killed-list
Deleted messages in the NNTP folder are saved to `killed-list'.  The
messages in the killed-list are treated as if it were not exist on the
server. Non-nil value for elmo-use-killed-list enables this feature
\(Initial setting is t). By this feature, NNTP pipe folder works correctly.

*** Maildir pack is now available
M-x wl-summary-pack-number in the summary mode of Maildir folder
re-numbers the messages.

** Searching

*** Complex condition can be specified for filter folder
AND condition, OR condition, NOT condition, and their combination can be
 specified. Syntax of the condition part is changed. See Info for details.

Caution for those who upgrade from 1.1.1:
By this change, saving directory for the msgdb of filter folder is altered.
Former msgdbs are not needed anymore. It does not cause any problem but
if you don't want to keep useless disk, you should remove files
under the directory '.elmo/filter/' beforehand.

*** Searching of the NNTP folder is available
Now you can make NNTP filter folder.
\(If only your NNTP server responds to XHDR command.)

*** Pick, Virtual in summary mode now accepts complex condition.
You can set AND condition and OR condition by typing
'AND' or 'OR' instead of field name.

** Session, Authentication

*** elmo-default-*-authenticate-type only accepts symbol(used be a string)
Example:
\(setq elmo-default-imap4-authenticate-type \"cram-md5\")
should be changed to
\(setq elmo-default-imap4-authenticate-type 'cram-md5)

*** stream-type can be defined.
You can define stream type by
elmo-network-{imap4-,pop3-,nntp-,}stream-type-alist.
Some SSL related variables are abolished(renamed).
You can access to the networked folders (IMAP4, NNTP, POP3) via SOCKS
if you specify the folder name end with \"!socks\".

** Draft

*** group-list is now available
You can specify address like 'Group: foo@gohome.org, bar@gohome.org;'.
If wl-draft-remove-group-list-contents is non-nil, the contents of 
group-list is removed before sending.

*** The draft preview displays recipient addresses on minibuffer 
You can confirm the group-list recipients by this.

*** Initial setting considers Reply-To:.
Default setting of wl-draft-reply-without-argument-list considers Reply-To: 
field (Set to To: field).

*** Replying rules for the messages sent from yourself.
You can define replying rules for the messages sent from yourself by
setting wl-draft-reply-myself-with-argument-list and
wl-draft-reply-myself-without-argument-list.

*** Full name is used in the reply address.
If wl-draft-reply-use-address-with-full-name is non-nil, then full
name is inserted in with e-mail addresses on the replied message
\(Initial setting is t).

*** In-Reply-To: format is changed.
In-Reply-To: format is changed to simple one. It is based on 
draft-ietf-drums-msg-fmt-09.txt.

** misc

*** Message thread processing is improved.

*** Renamed variables
wl-refile-guess-func-list => wl-refile-guess-functions
wl-summary-temp-above => wl-summary-target-above

*** You can set function to wl-fcc.
You can change fcc folder name dynamically. For example, change folder name
by month.

*** elmo-search-mime-charset is abolished.
Charset is guessed from the string you typed.

*** Useless headers are removed when you forward the message.
You can specify removed headers by wl-ignored-forwarded-headers.

*** wl-highlight-group-folder-by-numbers is abolished.
It is renamed to wl-highlight-folder-by-numbers and has following meaning.
  `t'   : Whole line is colored by message number.
  `nil' : Whole line is colored by folder status.
   Number (ex. `1') : Line is colored by message number and folder status.

*** Header visibility control is changed.
Header visibility is controlled by Wanderlust (was controlled by SEMI).
You can change header visibility by wl-message-ignored-field-list and 
wl-message-visible-field-list.

*** DEMO is changed.
Less colors are used by DEMO pixmap.
Emacsen on character based terminal also display suitable DEMO.
") ((1 1 1) . "* Changes in 1.1.1 from 1.1.0
  Version 1.1.1 is a bug fix version of 1.1.0 with minor user-visible changes.

** Development on the CVS server is started.

** Flush operation and sending queues if Wanderlust is  started
   in plugged status.

** Directory structure is changed.

*** 00README, 00README.ja is renamed to README, README.ja.

*** All wl-* files are moved to the directory 'wl'.

** Syntax of wl-refile-rule-alist is extended (compatible with older one).

** progress gauge
Progress gauge is displayed while processing in the Emacsen with
progress gauge feature.
") ((1 1 0) . "* Changes in 1.1.0 from 1.0.3

** Install

*** tm7 is not supported anymore.
see the file INSTALL for details.

*** WL_PREFIX and ELMO_PREFIX default as \"wl\"
\(defvar WL_PREFIX \"wl\")
\(defvar ELMO_PREFIX \"wl\")

e.g. install directory is
  1.0.3  /usr/local/share/emacs/site-lisp/
  1.1.0  /usr/local/share/emacs/site-lisp/wl/

*** Change default macro in Makefile.
EMACS   = emacs
XEMACS  = xemacs
use $(XEMACS), `package' and `install-package' target.

*** Install not only *.elc, but also *.el.

*** English document (wl.texi).

** New feature

*** Modified UTF7 support.
Now international mailbox name can be used in IMAP4 in the Emacsen
with unicode feature.

*** Scoring support.

*** New plugged system.

*** IMAP4 support became more generic.
Many IMAP4 servers are supported.

*** New authentication type
  IMAP4: CRAM-MD5, DIGEST-MD5, STARTTLS
  POP3:  CRAM-MD5, DIGEST-MD5, SCRAM-MD5, STARTTLS
  NNTP:  STARTTLS
  SMTP:  STARTTLS

*** New folder type
  |      Pipe Folder     Incorporate message.
  .      Maildir Folder  Now Maildir is one of the folder type.
  'cache Cache Folder    View internal cache.

*** Message buffer cache
Next message is prefetched while idle time.

*** Sticky summary is enhanced.
Now message buffer is also sticky.
You can specify always-sticky summary.

** misc

*** Eliminated wl-draft-prepared-config-alist
unified with wl-draft-config-alist.

*** POP-before-SMTP variables are re-arranged.

*** Ask non-existing folder.
 When FCC: contains new folder.
 When auto-refile specified new folder.

*** Change fetch threshold and confirm settings.
wl-prefetch-confirm-threshold, wl-cache-fetch-threshold.

*** Can use petname for completion.

*** Change Message-ID generator.

*** wl-demo.el support bitmap-mule.

*** Allow function type `smtp-server' value.

*** Make sendlog when `wl-draft-sendlog' is non-nil.

*** `wl-summary-incorporate-marks'

*** Reserve prefetching while off-line status.

*** Draft use new frame when `wl-draft-use-frame' is non-nil.

*** New variable `wl-user-mail-address-list' .

*** New variable `wl-local-domain' for set FQDN.

*** Server side unread status is used in IMAP4 folder.

*** Change defaults
  wl-mime-charset         iso-2022-jp  =>  x-ctext
  wl-summary-move-order   'new  =>  'unread
  wl-tmp-dir              TMPDIR  =>  ~/tmp/

*** New hooks
  wl-draft-send-hook
  wl-draft-reedit-hook
  wl-mime-edit-preview-message-hook
  wl-folder-suspend-hook
  wl-summary-toggle-disp-folder-message-resumed-hook
  wl-summary-line-inserted-hook
  wl-thread-update-children-number-hook
  mmelmo-header-inserted-hook
  mmelmo-entity-content-inserted-hook

*** New function
  wl-save
  wl-summary-write
  wl-summary-supersedes-message
  wl-fldmgr-delete
  wl-refile-guess-by-msgid
  wl-address-user-mail-address-p
  wl-summary-jump-to-msg-by-message-id-via-nntp
  wl-summary-temp-mark-pick
")) ("ja" ((2 14 0) . #("* 2.12.2 $(A$+$i(B 2.14.0 $(A$X$N$(I+y$(A8|5c(B

** $(APB$7$$%U%)%k%@PM(B access $(A%U%)%k%@$,W7<S$5$l$^$7$?!#(B
   $(AV86($5$l$?%U%)%k%@$NEdOB$N%5%V%U%)%k%@$r$(I"o$(AOk5D$KR;$D$N%U%)%k%@$H$7$F$(H$/$(A$((B
   $(A$k$h$&$K$9$k%U%)%k%@$G$9!#(B

** $(A%U%)%k%@$N%"%C%W%G$(B!<$(A%H$,8_KY;/$5$l$^$7$?!#(B
   $(A%j%9%H$N2n7V$r$(GSS$(AKc$9$k$(IJ=$(AJ}$,$(GUs$(A$-V1$5$l!"LX$K6`$/$N%a%C%;$(B!<$(A%8$r:,$`%U%)(B
   $(A%k%@$G$N$(GY/$(AWw$,8_KY$K$J$j$^$7$?!#(B

** $(APB$7$$%$%Y%s%H%O%s%I%j%s%0$(Gq"$(AD\$,W7<S$5$l$^$7$?!#(B

** $(A%I%i%U%H%U%)%k%@$K(B IMAP $(A%U%)%k%@$rV86($7$F$$$k$(G^[$(A:O$N2;>_:O$,8DIF$5$l$^$7$?!#(B
   $(A%*%U%i%$%sW4$(Ghh$(A$G%I%i%U%H$r1#4f$9$k$H$-$N$(I12$(GY/$(A$K%P%0$,$"$j$^$7$?$,!"PB$7$$(B
   $(A%$%Y%s%H%O%s%I%j%s%0$(Gq"$(AD\$rSC$$$FP^U}$5$l$^$7$?!#(B

** `Shimbun' $(A$N%5%^%j$,$(GY/$(A5D$K8|PB$5$l$^$9!#(B
   $(A$$$/$D$+$N(B shimbun $(A%U%)%k%@$O!"%5%^%j$NGi$(G^^$(A$,U}$7$/$"$j$^$;$s!#(B
   $(A$3$N%P$(B!<$(A%8%g%s$+$i!"%a%C%;$(B!<$(A%8$rH!$j$h$;$?$H$-$NGi$(G^^$(A$rSC$$$F%5%^%j$,(B
   $(AWT$(GY/$(A5D$KP^U}$5$l$k$h$&$K$J$j$^$7$?!#$3$N$(I(M$(AW0$K$O!"PB$7$$%$%Y%s%H%O%s%I(B
   $(A%j%s%0$(Gq"$(AD\$,SC$$$i$l$F$$$^$9!#(B

** $(A$=$NK{6`$/$N%P%0P^U}!#(B
" 0 526 (charset japanese-jisx0208))) ((2 12 2) . #("* 2.12.0 $(A$+$i(B 2.12.2 $(A$X$N$(I+y$(A8|5c(B
  2.12.2 $(A$O!"(B2.12.0 $(A$N%P%0P^U}0f$G$9!#(B

** Windows $(A$G(B Maildir $(A$,J9$($k$h$&$K$J$j$^$7$?!#(B
   $(A$?$@$7!"$(G]=$(A8q$K$(GP,$(A$C$F$$$^$;$s$N$G!"(BUNIX $(AIO$N(B Maildir $(A$H;%$(G_P$(APT$,$"$j$^$;$s!#(B

** $(A%U%#%k%?%U%)%k%@5H$G$N%-%c%C%7%e%^$(B!<$(A%/2;U{:O$N2;>_:O$,=bO{$5$l$^$7$?!#(B
   $(A%-%c%C%7%e$5$l$F$b%-%c%C%7%e$5$l$F$$$J$$1mJ>$H$J$k$(G^[$(A:O$,$"$j$^$7$?$,(B
   $(AP^U}$5$l$^$7$?!#(B

** $(A%U%i%0$,PB$(G]=$(A$N$^$^$(I+y$(A8|$5$l$J$/$J$k$(G^[$(A:O$,$"$k%P%0$NP^U}!#(B

** Maildir $(A$G$(Gno$(AJ}%a%C%;$(B!<$(A%8RF$(GY/Uk$(A!"%U%i%0$,U}$7$/R}$-$(ICc$(A$,$l$J$$%P%0$NP^U}!#(B
   $(AWn3u$N%a%C%;$(B!<$(A%8$7$+%U%i%0$,R}$-$(ICc$(A$,$l$^$;$s$G$7$?$,!"P^U}$5$l$^$7$?!#(B

** $(AHk$lWS$K$J$C$?%a%C%;$(B!<$(A%8$N(B IMAP $(A$K$h$k1mJ>$(GUk$(A$N2;>_:O$,=bO{$5$l$^$7$?!#(B
   $(AHk$lWS$K$J$C$?%a%C%;$(B!<$(A%8$r%Q$(B!<$(A%H%U%'%C%A$7$?$(G^[$(A:O!"%X%C%@$,1mJ>$5$l$J$$(B
   $(G^[$(A:O$,$"$j$^$7$?$,!"U}$7$/1mJ>$5$l$k$h$&$K$J$j$^$7$?!#(B

** %INBOX $(A$N%5%V%U%)%k%@$K(B %INBOX $(AWTLe$,:,$^$l$J$$$(G^[$(A:O$,$"$k$(GYBwn$(A$K$(I%\$(B=h$(A$7$^$7$?!#(B
   cyrus-imapd $(A$G!"51$(GfZ$(A$N$(GYBwn$(A$,3v$F$$$^$7$?$,P^U}$5$l$^$7$?!#(B

** $(A%U%i%0$,!"$(I<D$(AKw$(GUk$(A5H$N$(GfP$(AMj:r$(GfP$(A$H$7$F$(G\"$(A$l$k$h$&$K$J$j$^$7$?!#(B

** Folder mode $(A$G%"%/%;%9%0%k$(B!<$(A%W(B \"@/\" $(A$NU9$(Gbd$(A$,U}$7$/$(GY/$(AWw$7$^$9!#(B

** $(A%U%i%0C{$K$h$C$F$O$(Gbfg0$(A$C$?%U%i%0%U%)%k%@$,Ww3I$5$l$k$(GYBwn$(A$,P^U}$5$l$^$7$?!#(B
   $(A%U%i%0$K(B [a-z]$(ARTMb$NNDWV$rJ9$C$?$H$-$N$(GYBwn$(A$K$(I%\$(B=h$(A$7$^$7$?!#(B

** $(A%I%i%U%H$NWT$(GY/$(A1#4f$(GUk$(A$K%(%i$(B!<$(A$K$J$C$F$7$^$&$(GYBwn$(A$,P^U}$5$l$^$7$?!#(B

** $(A%"%I%l%9%^%M$(B!<$(A%8%c$rFp$(GY/$(A$7$?$(Gkc$(A$K%(%i$(B!<$(A$K$J$k$(GYBwn$(A$,P^U}$5$l$^$7$?!#(B
   $(A1>ND$K(B To: $(A5H$NNDWVAP$,$"$k$H%(%i$(B!<$(A$,$(BH/$(AIz$7$F$$$^$7$?$,!"P^U}$5$l$^$7$?!#(B

** $(A%U%#%k%?%U%)%k%@$+$i$N%3%T$(B!<$(A5H$G%U%i%0$,1#4f$5$l$J$$$(GYBwn$(A$,P^U}$5$l$^$7$?!#(B

** $(APB$(G]=$(A%*%W%7%g%s(B wl-summary-resend-use-cache $(A$,W7<S$5$l$^$7$?!#(B
   $(A%*%U%i%$%sW4$(Ghh$(A$G$b%-%c%C%7%e$rSC$$$?TYKM(B(resend)$(A$,$G$-$^$9!#(B

** $(APB$(G]=$(A%*%W%7%g%s(B elmo-network-session-idle-timeout $(A$,W7<S$5$l$^$7$?!#(B
   $(AV86($7$?$(GUkbf$(ARTIO%"%$%I%kW4$(Ghh$(A$H$J$C$?%;%C%7%g%s$rTY@{SC$7$^$;$s!#(B

** 'H' $(A$d(B 'M' $(A$G$N%a%C%;$(B!<$(A%8TY1mJ>$,$(I'`$(ABJ;/$5$l$^$7$?!#(B
   $(ATY1mJ>$(GUk$(A$K(B MIME $(A$N$(Gi,$(ATl!"%P%C%U%!$rTY@{SC$9$k$h$&$K$J$j$^$7$?!#(B

** $(A%I%i%U%H$N%W%l%S%e$(B!<$(GUk$(A$K1mJ>$5$l$kJtPT1mJ>$,(B netnews $(A$K$(I%\%w$(A$7$^$7$?!#(B
" 0 1161 (charset japanese-jisx0208))) ((2 12 0) . #("* 2.10.1 $(A$+$i(B 2.12.0 $(A$X$N$(I+y$(A8|5c(B

** $(A%a%C%;$(B!<$(A%8%G$(B!<$(A%?%Y$(B!<$(A%9$N$(Gi,$(ATl$,8DIF$5$l$^$7$?!#(B
   $(ARTOB$N$(G]C$(A6($r$9$l$P!"%U%)%k%@$(GrY$(I&1$(GUk$(A$KWT$(GY/$(A5D$K>I@4$N%?%$%W$N(B msgdb $(A$r(B
   $(APB$7$$%?%$%W$N$b$N$K$(I+y$(G_P$(A$7$^$9!#(B
   (setq elmo-msgdb-default-type 'standard
         elmo-msgdb-convert-type 'auto)
   $(A3uFZ$(I/N$(A$O!"IO$(GX4$(A$NM($j$H$J$C$F$$$^$9!#(B

** $(AR;$(GUk$(A%^$(B!<$(A%/$H!"$=$l$K$(I%\$(A$9$k%"%/%7%g%s$rWTSI$K6($(Gex$(A$G$-$k$h$&$K$J$j$^$7$?!#(B

   $(A%G%U%)%k%H$G$ORTOB$N%^$(B!<$(A%/$H%"%/%7%g%s$r6($(Gex$(A$7$F$$$^$9!#(B
   $(I0q$(A@4$+$iR}$-$(ICc$(A$,$l$?%^$(B!<$(A%/$H%"%/%7%g%s(B
    \"o\" refile   ($(I0q$(A@4$N%j%U%!%$%k$HM,$8(B)
    \"O\" copy     ($(I0q$(A@4$N%3%T$(B!<$(A$HM,$8(B)
    \"d\" dispose  ($(A>I(B delete, D $(A%^$(B!<$(A%/!#(Bwl-trash-folder $(A$KRF$(GY/$(A!#(B
    		  wl-dispose-folder-alist $(A$N$(I/N$(A$K$h$j$(I12$(GY/$(A$,$(GJn$(A$^$k!#(B)
   $(APB$?$KW7<S$5$l$?%^$(B!<$(A%/$H%"%/%7%g%s(B
    \"D\" delete   ($(A$$$-$J$jO{H%(B)
    \"i\" prefetch ($(A%W%j%U%'%C%A(B)
    \"~\" resend   ($(ATYKM(B)
   $(A%5%^%j$G(B x $(A%-$(B!<$(A$rQ:$9$H%^$(B!<$(A%/$K$(I%\%w$(A$7$?%"%/%7%g%s$,$9$Y$F$(I(M$(APP$5$l$^$9!#(B
   $(A%^$(B!<$(A%/$H%"%/%7%g%s$O!"PB$(G]=$(I+y$(AJ}(B wl-summary-mark-action-list $(A$K$h$C$F6($(Gex(B
   $(A$G$-$^$9!#$(Gf[$(A$7$/$OM,$(I+y$(AJ}$N(B docstring $(A$r2NUU$7$F$/$@$5$$!#(B

** $(A%9%Q%`%U%#%k%?%b%8%e$(B!<$(A%k$,PB$?$KW7<S$5$l$^$7$?!#(B
   $(ARTOB$N%9%Q%`%U%#%k%?$K$(I%\%w$(A$7$F$$$^$9!#(B
   bogofilter
   spamfilter
   bsfilter
   SpamAssassin
   SpamOracle
   $(AU}$(G]=$(A1m$(G\"$(A$K$h$k%X%C%@$(I<D-4(B

** 'mark $(A%U%)%k%@$O8DC{$5$l!"(B'flag $(A%U%)%k%@$K$J$j$^$7$?!#(B
   $(A$3$l$K$(IJ=$(G]Y$(A$7$F!"(B'flag $(A%U%)%k%@$N%5%^%j$GT*%a%C%;$(B!<$(A%8$,$I$3$K$"$k$+$r(B
   help-echo $(A$H$7$F1mJ>$9$k$h$&$K$J$j$^$7$?(B($(A$3$l$N$U$k$^$$$O(B
   wl-highlight-summary-line-help-echo-alist $(A$GVFSy$G$-$^$9(B)$(A!#(B

** $(A%a%C%;$(B!<$(A%8$K$(I%\$(A$7$FHNRb$N%f$(B!<$(A%66($(Gex$(A$N%U%i%0$r86$1$i$l$k$h$&$K$J$j$^$7$?!#(B
   $(A%5%^%j$K$*$$$F(B \"F\" $(A$G%U%i%0$NV86($,$G$-$^$9!#(B

** $(A75PE$(I6v$(A$_%^$(B!<$(A%/(B A,a $(A$,W7<S$5$l$^$7$?!#(B
   $(A%5%^%j$K$*$$$F!"75PE$7$?%a%C%;$(B!<$(A%8$K(B A $(A%^$(B!<$(A%/(B($(A%-%c%C%7%e$J$7$N$(G^[$(A:O(B)
   $(A$b$7$/$O(B a $(A%^$(B!<$(A%/(B($(A%-%c%C%7%e$"$j$N$(G^[$(A:O(B) $(A$,1mJ>$5$l$^$9!#(B

** $(BE>$(AKM$(I6v$(A$_%^$(B!<$(A%/(B F,f $(A$,W7<S$5$l$^$7$?!#(B
   $(A%5%^%j$K$*$$$F!"$(BE>$(AKM$7$?%a%C%;$(B!<$(A%8$K(B F $(A%^$(B!<$(A%/(B($(A%-%c%C%7%e$J$7$N$(G^[$(A:O(B)
   $(A$b$7$/$O(B f $(A%^$(B!<$(A%/(B($(A%-%c%C%7%e$"$j$N$(G^[$(A:O(B) $(A$,1mJ>$5$l$^$9!#(B

** $(A%U%)%k%@$N$(I<D$(AKwLu<~$K!"(B'$(A%U%i%0(B' ($(A%a%C%;$(B!<$(A%8$NW4$(Ghh(B) $(A$,W7<S$5$l$^$7$?!#(B
   $(A%U%i%0$K$O!"(Bunread($(AN4$(BFI(B), important($(AVXR*(B), answered($(A75PE$(I6v$(A$_(B),
   digest ($(AN4$(BFI$(A$^$?$OVXR*(B), any ($(AN4$(BFI$(A$^$?$OVXR*$^$?$O75PE$(I6v$(A$_(B)$(A$,$"$j$^$9!#(B
   $(A@}$($P!"4N$N%U%#%k%?%U%)%k%@$O!"(B%inbox $(A$N$&$A!"N4$(BFI$(A$^$?$OVXR*$J(B
   $(A%a%C%;$(B!<$(A%8$N$_$,3v$(G\"$(A$7$^$9!#(B
   /flag:digest/%inbox

** $(A%I%i%U%H$N1#4f$(Gq"$(AD\$,8DIF$5$l$^$7$?!#(B
   IMAP $(A%U%)%k%@$d!"(BMaildir $(A%U%)%k%@$r(B wl-draft-folder $(A$KV86($G$-$k$h$&$K(B
   $(A$J$j$^$7$?!#(B

** idle-timer $(A$r@{SC$7$F%I%i%U%H%P%C%U%!$NWT$(GY/$(A1#4f$r$(I(M$(APP$7$^$9!#(B
   $(I+y$(AJ}(B `wl-auto-save-drafts-interval' $(A$G$(I12$(GY/$(A$r$(I+y$(A$($i$l$^$9!#(B

** 'H' $(A%-$(B!<(B($(A%X%C%@H+1mJ>(B) $(A$*$h$S(B 'M' $(A%-$(B!<(B(MIME$(A$J$71mJ>(B)$(A$,%H%0%k$K$J$j$^$7$?!#(B
   $(A$^$?!"(B'M' $(A$G1mJ>$7$?%a%C%;$(B!<$(A%8$rR}SC$G$-$k$h$&$K$J$j$^$7$?!#(B

** non-MIME PGP $(A%a%C%;$(B!<$(A%8$N$(I<D$(Gb%$(A!"$(G_&$(A:E;/$N$?$a$N%3%^%s%I$,W7<S$5$l$^$7$?!#(B
   $(A%a%C%;$(B!<$(A%8%P%C%U%!$K$*$$$F(B C-c:v, C-c:d $(A$G$=$l$>$l$(I<D$(Gb%$(A!"$(G_&$(A:E;/$7$^$9!#(B

** $(A%5%^%j$r$(GKd$(A$YLf$(Lu<~$NDf$(Gb{$(A$G%=$(B!<$(A%H$G$-$k$h$&$K$J$j$^$7$?!#(B

** $(A%a%C%;$(B!<$(A%8%P%C%U%!$G$(Gno$(AJ}PP$K$o$?$k$(GO[$(A$$%X%C%@$rJ!BT1mJ>$7$^$9!#(B

** $(A%5%^%j%P%C%U%!$NS@$(BB3$(A%^$(B!<$(A%/$NNDWVAP$,$(I+y$(A8|$5$l$^$7$?!#(B
   $(A%G%U%)%k%H$N$(G]C$(A6($G$O%-%c%C%7%e$NSP$(G`R$(A$r4sNDWVP!NDWV$GJ>$7$^$9!#(B

** $(AKMPE$(Gm}k%$(A$N$(Gkc$(A$K%I%i%U%H$N%W%l%S%e$(B!<$(A$r1mJ>$9$k$h$&$K$J$j$^$7$?!#(B

** $(A%I%i%U%H$N%W%l%S%e$(B!<$(A$N$(Gkc$(A$KKMPE%Q%i%a$(B!<$(A%?$r1mJ>$9$k$h$&$K$J$j$^$7$?!#(B
   $(Gf[$(A$7$/$O$(I+y$(AJ}(B wl-draft-preview-attributes $(A$N$(IIf$(ACw$r$(GKD$(A$FOB$5$$!#(B

** wl-biff-use-idle-timer $(A$r$(G]C$(A6($9$k$H(B biff $(A$,(B idle-timer $(A$GW_$j$^$9!#(B

** wl-draft-kill $(A$O(B yes-or-no-p $(A$G$(Gm}k%$(A$rGs$a$k$h$&$K$J$j$^$7$?!#(B

** $(A%5%^%j$GR;6(RTIOIn$$%9%l%C%I$O7V8n$5$l$^$9!#(B
   $(I+y$(AJ}(B wl-summary-max-thread-depth $(A$GO^=g$r$(I+y$(A8|$G$-$^$9!#(B

** Emacs multi-tty support $(A$K$(I%\%w$(A$7$^$7$?!#(B
   (http://lorentey.hu/project/emacs.html)

** $(A%5%^%j$N$(GKd$(A$YLf$(Lu<~$K(B 'size' $(A$,W7<S$5$l$^$7$?!#(B
   $(A%a%C%;$(B!<$(A%8%5%$%:$K$h$k%5%^%j$N$(GKd$(A$YLf$($,?ID\$K$J$j$^$7$?!#(B

** $(I+y$(AJ}(B wl-refile-policy-alist $(A$O$(LHE$(AV9$5$l$^$7$?!#(B

** $(A%P%C%A$(B=h$(A@mSC$N%b%8%e$(B!<$(A%k$,PB$?$KW7<S$5$l$^$7$?!#(B

** $(A%^%k%A%U%)%k%@$H%*%j%8%J%k%U%)%k%@$NW4$(Ghh$(A$,M,FZ$5$l$k$h$&$K$J$j$^$7$?!#(B
   $(A@}$($P!"(B+inbox $(A$NN4$(BFI$(AGi$(G^^$(A$,!"(B*+inbox,+outbox $(A$K$b74S3$5$l$^$9!#(B

** $(IJ=$(AJ}(B wl-summary-resend-message $(A$O$(LHE$(AV9$5$l$^$7$?!#(B
   $(A$=$N4z$o$j$K(B wl-summary-resend $(A$rJ9$($PTYKM%^$(B!<$(A%/$r86$1$k$3$H$,$G$-$^$9!#(B

** $(I+y$(AJ}(B wl-delete-folder-alist $(A$O(B wl-dispose-folder-alist $(A$K(B
   $(AC{G0$,$(I+y$(A8|$5$l$^$7$?!#(B

** POP3 $(A%U%)%k%@$N4fTZ%A%'%C%/$N%G%U%)%k%H$(I12$(GY/$(A$r$(Gv|$(ABT;/(B
   elmo-pop3-exists-exactly $(A$N%G%U%)%k%H$(I/N$(A$r(B nil $(A$K$7$^$7$?!#(B

** RFC2449 $(A$N(B POP3 $(I)/$(GZ/$(A$N%l%9%]%s%9%3$(B!<$(A%I$K$(I%\%w$(A$7$^$7$?!#(B
   $(AK{$N%;%C%7%g%s$,J9SCVP$G$"$k$?$a$K$(Gk%b%$(A$KJ'$(GZu$(A$7$?$H$-$K$O!"%Q%9%o$(B!<$(A%I(B
   $(A$,%/%j%"$5$l$J$/$J$j$^$7$?!#(B

** IMAP4 $(A$K$*$$$F!"(BEXPUNGE, CHECK $(A%3%^%s%I$r7GM,FZ$GKMPE$9$k$h$&$K$7$^$7$?!#(B

** wl-folder-hierarchy-access-folders $(A$N3uFZ$(I/N$(A$,$(I+y$(A8|$5$l$^$7$?!#(B

** $(APB$(GjL$(A%U%)%k%@$N%"%/%;%9%0%k$(B!<$(A%W(B \"@/\" $(A$,J9$($k$h$&$K$J$j$^$7$?!#(B

** $(AG0$KJ9$C$F$$$?$b$N$h$jPB$7$$(B Wanderlust $(A$rFp$(GY/$(A$9$k$H(B NEWS(.ja) $(A$NDZH]$r(B
   $(A1mJ>$7$^$9!#(B

** wl-draft-reply-*-list $(A$N3uFZ$(I/N$(A$,$(I+y$(A8|$5$l$^$7$?!#(B
   $(ARTG0$N$(G]C$(A6($O!"(Bsamples/ja/dot.wl $(A$r2NUU$7$FOB$5$$!#(B

** wl-draft-reply-myself-*-list $(A$O$(LHE$(AV9$5$l!"(Bwl-draft-reply-*-list $(A$K$(G\S$(A:O(B
   $(A$5$l$^$7$?!#(B

** $(A75PESC%I%i%U%H$N%+$(B!<$(A%=%k$N3uFZN;VC$rV86($G$-$^$9!#(B
   $(I+y$(AJ}(B wl-draft-reply-default-position $(A$r$(G]C$(A6($7$FOB$5$$!#(B

** $(A%I%i%U%H%P%C%U%!%&%#%s%I%&$NEdVC$NV86($NJK7=$,$(I+y$(A8|$5$l$^$7$?!#(B
   wl-draft-buffer-style $(A$H(B wl-draft-reply-buffer-style $(A$K(B keep,full,split
   $(A$N$$$:$l$+$rV86($7$^$9!#(B

** $(APB$(G]=(B hook
   wl-draft-reply-hook
   wl-summary-reply-hook
   wl-draft-forward-hook
   wl-summary-forward-hook
   wl-draft-kill-pre-hook
   wl-summary-resend-hook

** $(LHE$(AV9$5$l$?(B hook
   wl-reply-hook

** $(APB$(G]=(B face

   wl-highlight-summary-disposed-face
   wl-highlight-summary-prefetch-face
   wl-highlight-summary-resend-face
   wl-highlight-summary-answered-face
   wl-highlight-action-argument-face

** $(LHE$(AV9$5$l$?(B face

   wl-highlight-refile-destination-face
   (wl-highlight-action-argument-face $(A$K$(I+y$(AC{(B)
" 0 4008 (charset japanese-jisx0208))) ((2 10 1) . #("* 2.10.0 $(A$+$i(B 2.10.1 $(A$X$N$(I+y$(A8|5c(B
  2.10.1 $(A$O(B 2.10.0 $(A$N%P%0P^U}0f$G$9!#(B

** print-length $(A$d(B print-level $(A$,(B Non-nil $(A$N$H$-$K(B msgdb $(A$,$(IPI$(A$l$k$(GYBwn$(A$,(B
   $(AP^U}$5$l$^$7$?!#(B

** $(A%Q%$%W%U%)%k%@$K$*$$$F(B wl-summary-pack-number $(A$,$&$^$/$(GY/$(A$+$J$+$C$?(B
   $(A$?$a$(I'`$(A$+$J$/$7$F$"$j$^$9!#1XR*$G$"$l$PH!$j$(B9~$(A$_OH%U%)%k%@$N7=$G$(I(M$(APP(B
   $(A$7$FOB$5$$!#(B

** wl-folder-move-cur-folder $(A$,$(I'`$(A$$$F$$$J$+$C$?$(GYBwn$(A$,P^U}$5$l$^$7$?!#(B

** Meadow $(AIO$G(B wl-draft-reedit $(A$,$&$^$/$(GY/$(A$+$J$$$(GYBwn$(A$,P^U}$5$l$^$7$?!#(B

** wl-summary-pack-number $(A$,(B Maildir $(A$d(B shimbun $(A%U%)%k%@$G$(GY/$(A$+$J$$(B
   $(GYBwn$(A$,P^U}$5$l$^$7$?!#(B

** $(AVXR*%^$(B!<$(A%/$D$-%a%C%;$(B!<$(A%8$N%-%c%C%7%e$K$(I%\$(A$9$k1#$(G{,q"$(AD\$,$(I@>$(A$+$J$$(B
   $(A2;>_:O$,P^U}$5$l$^$7$?!#(B

** wl-summary-line-format $(A$N(B %# $(A$G4s$-$JJ}$(I/N$(A$rU}$7$/$(H$/$(A$($J$$2;>_:O(B
   $(A$,P^U}$5$l$^$7$?!#(B

** SMTP AUTH $(A$G$(Gk%b%$(ARTMb$N%(%i$(B!<$(A$G$b%Q%9%o$(B!<$(A%IO{H%$5$l$k2;>_:O$,P^(B
   $(AU}$5$l$^$7$?!#(B

** wl-message-buffer-prefetch-folder-type-list,
   wl-message-buffer-prefetch-idle-time,
   wl-message-buffer-prefetch-depth $(A$N%G%U%)%k%H$(I/N$(A$,$(I+y$(A8|$5$l$^$7$?!#(B

** XEmacs without mule $(A$G%3%s%Q%$%k$G$-$J$$$(GYBwn$(A$,P^U}$5$l$^$7$?!#(B
" 0 751 (charset japanese-jisx0208))) ((2 10 0) . #("* 2.8.1 $(A$+$i(B 2.10.0 $(A$X$N$(I+y$(A8|5c(B

** $(A%5%^%jPP$N1mJ>PNJ=$r$(I+y$(A8|$G$-$k$h$&$K$J$j$^$7$?!#(B
   wl-summary-line-format $(A$G$(GUs$(AJ=$r$(G]C$(A6($G$-$^$9!#%U%)%k%@$(I#z$(A$K$(GUs$(AJ=$r$(I+y$(A$($?$$(B
   $(G^[$(A:O$O(B wl-folder-summary-line-format-alist $(A$rSC$$$FOB$5$$!#(B

** $(A%I%i%U%H%U%)%k%@$X$N1#4fPNJ=$,$(I+y$(A8|$5$l$^$7$?!#(Bwl-draft-save $(A$N$(Gkc$(A$K$O(B
   $(A%(%s%3$(B!<$(A%I$7$F1#4f$5$l$^$9!#(B

** elmo-split $(A$,PB$(G]C$(A$5$l$^$7$?!#Sk$($?%k$(B!<$(A%k$KQX$C$F(B procmail $(GSx$(A$K%a%C%;(B
   $(B!<$(A%8$rUq$j7V$1$k$3$H$,$G$-$^$9!#(B

** $(A%P%C%U%!%W%j%U%'%C%A$,$(I(M$(AW0$5$l$^$7$?!#(Bwl-message-buffer-prefetch-depth
   $(A$NJ}$@$1!"%a%C%;$(B!<$(A%8$r%P%C%U%!$KOH$(BFI$(A$_$7$^$9!#(B

** elmo-dop-queue-flush $(A$O$(B7R$(A$,$C$F$$$k%]$(B!<$(A%H$K$(IJ=$(A$9$k%-%e$(B!<$(A$r(B flush $(A$7$^$9!#(B

** $(APB$7$$%U%l$(B!<$(A%`$r$(Gbd$(A$$$F(B Wanderlust $(A$rFp$(GY/$(A$G$-$k$h$&$K$J$j$^$7$?!#(B
   (autoload 'wl-other-frame \"wl\" \"Wanderlust on new frame.\" t)
   $(A$N$h$&$K$(G]C$(A6($7$FOB$5$$!#(B

** $(A%U%)%k%@%b$(B!<$(A%I$+$i!"Sk$($i$l$?Lu<~$r$(I<h$(A$?$9%a%C%;$(B!<$(A%8$+$i$J$k$(I"o$(AOk%U%)%k%@(B
   $(A$XRF$(GY/$(A$G$-$^$9(B (wl-folder-virtual)$(A!#(B\"V\" $(A$K%P%$%s%I$5$l$F$$$^$9!#(B

** $(A%U%)%k%@%b$(B!<$(A%I$G!"Sk$($i$l$?Lu<~$r$(I<h$(A$?$9%a%C%;$(B!<$(A%8$r:,$`%U%)%k%@$rL=$;$k(B
   $(A$h$&$K$J$j$^$7$?(B (wl-folder-pick)$(A!#(B\"?\" $(A$K%P%$%s%I$5$l$F$$$^$9!#(B 

** $(A%"%/%;%9%0%k$(B!<$(A%W%U%)%k%@$N8DC{$,3v@4$k$h$&$K$J$j$^$7$?!#(B

** $(APB$(G]=$(A%5%^%j$K$(I%\$(A$9$k%9%l%C%I1mJ>$N(B ON/OFF $(A$rV86($G$-$k$h$&$K$J$j$^$7$?!#(B
   wl-summary-default-view, wl-summary-default-view-alist $(A$r$(G]C$(A6($7$FOB$5$$!#(B

** $(A%9%F%#%C%-$(B!<$(A%5%^%j$r(B q $(A$d(B g $(A$G$(I&4$(A$1$k$(Gkc$(A$K!"R;$(GUk$(A5D%^$(B!<$(A%/$r1#3V$9$k$h$&$K(B
   $(A$J$j$^$7$?!#(B

** $(A%9%F%#%C%-$(B!<$(A%5%^%j$K$(IJ=$(A$9$k%-$(B!<$(A%P%$%s%I$,$(I+y$(A8|$K$J$j$^$7$?!#(B
   $(A%5%^%j$G(B C-u g $(A$9$k$H(B C-u q $(A$HM,$(ILT$(A$K%5%^%j$rFF$(G[9$(A$7$^$9!#%5%^%j$d%U%)%k%@(B
   $(A%b$(B!<$(A%I$+$i(B G $(A$G%5%^%j$KRF$(GY/$(A$9$k$H!"PB$(G]=$(A%5%^%j$,%9%F%#%C%-$(B!<$(A$K$J$j$^$9!#(B

** C-cC-n $(A$d(B C-cC-p $(A$G%5%^%j%P%C%U%!$(Gbf$(A$rQ2;X$G$-$^$9!#(B

** $(A%j%9%H(B wl-folder-hierarchy-access-folders $(A$N8wR*KX$O!"%"%/%;%9%0%k$(B!<$(A%W$K(B
   $(A$D$$$F$NU}$(G]=$(A1m$(G\"$(A$K$J$j$^$7$?(B($(A$3$l$^$G$OU}$(Gm}$(A$J%0%k$(B!<$(A%WC{$G$7$?(B)$(A!#(B

** $(A%I%i%U%H$N%X%C%@2?7V$G(B C-a $(A$9$k$H!"PP$(Gs$$(A$b$7$/$O%X%C%@$NOH$(Gs$$(A$K%+$(B!<$(A%=%k$,(B
   $(ARF$(GY/$(A$7$^$9!#(B

** $(A%+%W%;%k;/(B Blind Carbon Copy $(A$rKM$l$k$h$&$K$J$j$^$7$?!#(B
   $(A%G%U%)%k%H$N%U%#$(B!<$(A%k%IC{$O(B \"Ecc:\" $(A$G$9!#(B

** $(A%I%i%U%H$N(B C-c C-y $(A$G%j$(B!<$(A%8%g%s$rR}SC$G$-$k$h$&$K$J$j$^$7$?!#(B
   transient-mark-mode (Emacs) $(A$b$7$/$O(B zmacs-regions (XEmacs)
   $(A$,(B Non-nil $(A$G!"%j$(B!<$(A%8%g%s$,SP$(I'`$(A$N$H$-$KWwSC$7$^$9!#(B

** $(A%^%k%A%Q$(B!<$(A%H$N%a%C%;$(B!<$(A%8$+$i%Q$(B!<$(A%H$rOw3}$G$-$k$h$&$K$J$j$^$7$?!#(B
   $(A%a%C%;$(B!<$(A%8%P%C%U%!$G(B \"D\" $(A$K%P%$%s%I$5$l$F$$$^$9!#(B

** $(A%K%e$(B!<$(A%9$(GX4$(AJB$rM68e$9$k%5$(B!<$(A%P$r$(Gv|$(I+J$(A$K$(G]C$(A6($G$-$k$h$&$K$J$j$^$7$?!#(B
   Info $(A$N@}$K$(G\d$(A$C$F(B wl-nntp-posting-config-alist $(A$r$(G]C$(A6($7$FOB$5$$!#(B

** $(I+y$(AJ}(B wl-draft-reply-with-argument-list $(A5H$G!"$(IJ=$(AJ}$N75$j$(I/N$(A$+$iMpOH$r(B
   $(GJn$(A$a$i$l$k$h$&$K$J$j$^$7$?!#(B

** $(IJ=$(AJ}(B wl-draft $(A$N%$%s%?$(B!<$(A%U%'$(B!<$(A%9$,$(I+y$(A8|$5$l$^$7$?!#(B
   $(AWn3u$KSCRb$9$k%X%C%@$r$(IJ=$(G]Y$(A%j%9%H$NPN$G6I$9$h$&$K$J$j$^$7$?!#(B

** wl-generate-mailer-string-function $(A$NJ9SC7($,$(I+y$(A8|$K$J$j$^$7$?!#(B
   User-Agent $(A%U%#$(B!<$(A%k%I$KHk$kNDWVAP$r75$9$(IJ=$(AJ}$rV86($7$FOB$5$$!#(B

** Reference Card (doc/wl-refcard-ja.tex) $(A$KVw$J%-$(B!<$(A2YWw$rAP$(I12$(A$7$^$7$?!#(B

** $(A$=$NK{$$$/$D$+$NP^U}!#(B
" 0 1909 (charset japanese-jisx0208))) ((2 8 0) . #("* 2.6.1 $(A$+$i(B 2.8.0 $(A$X$N$(I+y$(A8|5c(B

** Nemacs, Mule 2.3 based on Emacs 19.28 $(A$O%5%]$(B!<$(A%H$5$l$J$/$J$j$^$7$?!#(B

** FLIM 1.14.2 $(ARTG0$N(B FLIM $(A$G$O$&$^$/$(GY/$(A$+$J$$$(G^[$(A:O$,$"$j$^$9!#(B
   FLIM 1.14.3 $(ART=5$*$h$S$(I%\%w$(A$7$?(B SEMI $(A$r%$%s%9%H$(B!<$(A%k$7$F$/$@$5$$!#(B

** make check $(A$G$(Gv|$(I+J$(A$J$(Gt?$(A>3%F%9%H$,$G$-$k$h$&$K$J$j$^$7$?!#(B

** $(AC{G0$(I+y$(A8|5H$GJ9$o$l$J$/$J$C$?$(I+y$(AJ}$r(B .wl $(A5H$G$(G]C$(A6($7$F$$$k$H!">/8f$,1mJ>$5$l(B
   $(A$^$9!#%a%C%;$(B!<$(A%8$r2N?<$K$7$F!"$(G]C$(A6($r$(I+y$(A8|$7$F$/$@$5$$!#(B
   $(A$b$7:N$i$+$N@mSI$G>/8f$N1mJ>$rRVVF$7$?$$$(G^[$(A:O$K$O!"$(I+y$(AJ}(B
   elmo-obsolete-variable-show-warnings $(A$r(B nil $(A$K$7$F$/$@$5$$!#(B

** $(APB$(G]=$(ADZ2?%U%)%k%@(B 'sendlog $(A$,W7<S$5$l$^$7$?!#(B

** $(APB$(G]=$(A%U%)%k%@(B shimbun $(A%U%)%k%@$,W7<S$5$l$^$7$?!#(B

   $(GUs$(AJ=(B: '@' '$(I"o$(AOk%5$(B!<$(A%PC{(B' '.' '$(A%0%k$(B!<$(A%WC{(B'

** $(APB$(G]=$(A%U%)%k%@(B namazu $(A%U%)%k%@$,W7<S$5$l$^$7$?!#(B

   $(GUs$(AJ=(B:  '[' 'namazu $(I<D$(AKwJ=(B' ']' [ 'namazu index $(A$N%Q%9#($(I>3%\$(A%Q%9#)(B' ]

** $(A%Q%$%W%U%)%k%@$G%5$(B!<$(A%P$K%a%C%;$(B!<$(A%8$r2P$9$3$H$,$G$-$k$h$&$K$J$j$^$7$?(B
   $(A4N$N%"%/%;%9$(GUk$(A$K$O!"PB$7$$%a%C%;$(B!<$(A%8$N$_%3%T$(B!<$(A$7$^$9!#(B

   $(GUs$(AJ=(B:  '|' '$(AH!$j$(B9~$(A$_T*(B' '|:' '$(AH!$j$(B9~$(A$_OH(B'

** $(A%"%I%l%9%^%M$(B!<$(A%8%c$,PB$(G]C$(A$5$l$^$7$?(B(C-c C-a $(A$GFp$(GY/(B)$(A!#(B
   $(A%"%I%l%9$(GZ($(A$N$(Gn>$(A</$r$7$?$j!"$=$3$+$i%I%i%U%H$KMpOH$rHkA&$9$k$3$H$,$G$-$^$9!#(B

** ACAP (RFC2244) $(A$K$(I%\%w$(A$7$^$7$?(B($(I(M$(B83$(A5D(B)$(A!#(B

** IMAP4 $(A$N%a%C%;$(B!<$(A%8$r%Q$(B!<$(A%H$(I#z$(A$K%-%c%C%7%e$H$7$F1#4f$G$-$k$h$&$K$J$j$^$7$?!#(B
   $(A>^4s$J%Q$(B!<$(A%H$r%9%-%C%W$7$?$(G^[$(A:O$G$b!"%9%-%C%W$7$?%Q$(B!<$(A%HRTMb$O(B
   $(A%*%U%i%$%sW4$(Ghh$(A$G$(BFI$(A$_75$9$3$H$,$G$-$k$h$&$K$J$j$^$7$?!#(B

** $(A%a%C%;$(B!<$(A%8$N%W%j%U%'%C%A$G%a%C%;$(B!<$(A%8%S%e$(B!<$(A$^$GWw3I$9$k$h$&$K$J$j$^$7$?!#(B
   $(A%W%j%U%'%C%A$5$l$?%a%C%;$(B!<$(A%8$N1mJ>$,8_KY;/$5$l$^$7$?!#(B

** $(A%a%C%;$(B!<$(A%8%P%C%U%!!"%I%i%U%H%P%C%U%!$G$(GO[$(A$$PP$NU[$j75$7$r$(G]C$(A6($G$-$k$h$&$K(B
   $(A$J$j$^$7$?!#$(I+y$(AJ}(B wl-message-truncate-lines, wl-draft-truncate-lines $(A$,(B
   non-nil $(A$J$i!"$=$l$>$l$K$D$$$F$(GO[$(A$$PP$r(B window $(A7y$GGP$j$(Gtc$(A$a$^$9!#(B

** $(A%*$(B!<$(A%W%K%s%0%G%b$KJ9$o$l$k%S%C%H%^%C%W;-Oq$O(B wl-demo.elc $(A$+$iH!$j3}$+$l!"(B
   wl-icon-directory $(A$+$i$(BFI$(A$_$(B9~$(A$`$h$&$K$J$j$^$7$?!#(B
   $(A%/%j%9%^%9$N$(GUk$(AFZ$K$OLX$(GI1$(A$J;-Oq$,1mJ>$5$l$^$9(B :)

** elmo $(A%b%8%e$(B!<$(A%k$,H+Le5D$K$(GUs$(A$-V1$5$l$^$7$?!#(B

** elmo $(A$N%P%C%/%(%s%I$KR@4f$7$?$(I+y$(AJ}$O(B \"elmo-$(A%P%C%/%(%s%IC{(B-*\" 
   $(A$H$$$&C{G0$K$(I+y$(A8|$5$l$^$7$?!#(B
   $(A@}$($P!"(B elmo-default-imap4-server $(A$,(B elmo-imap4-default-server $(A$K(B
   $(I+y$(A8|$5$l$F$$$^$9!#(B

** xxx-func $(A$H$$$&C{G0$N$(I+y$(AJ}$O(B xxx-function $(A$H$$$&C{G0$K$(I+y$(A8|(B $(A$5$l$^$7$?!#(B

** X-Face utility 1.3.6.12 $(ARTG0$O%5%]$(B!<$(A%H$5$l$J$/$J$j$^$7$?!#(B
   $(A1XR*$J$i(B X-Face utility 1.3.6.13 $(ART=5$r%$%s%9%H$(B!<$(A%k$7$F$/$@$5$$!#(B

** plugged $(A%b$(B!<$(A%I$G!"(Bstream-type $(A$,$(Gg0$(A$&$b$N$O$(GI1$(A%(%s%H%j$H$7$F$(H$/$(A$o$l$k$h$&$K(B
   $(A$J$j$^$7$?!#(B

** $(A%"$(B!<$(A%+%$%V(B, $(A%^%k%A%U%)%k%@SC$N(B msgdb path $(A$,$(I+y$(A8|$5$l$^$7$?!#(B
   $(A$=$N$^$^$G$b$(GYBwn$(A$"$j$^$;$s$,!"%G%#%9%/$K$(G`R$(IJj$(A$J%G$(B!<$(A%?$r2P$7$?$/$J$$7=$O(B
   .elmo/multi, .elmo/archive $(ARTOB$r$"$i$+$8$aOw3}$7$F$*$$$F$/$@$5$$!#(B

** xxx-dir $(A$H$$$&C{G0$N$(I+y$(AJ}$O(B xxx-directory $(A$H$$$&C{G0$K$(I+y$(A8|$5$l$^$7$?!#(B
   $(A@}$($P!"(Bwl-icon-dir $(A$O(B wl-icon-directory $(A$K$(I+y$(A8|$5$l$F$$$^$9!#(B
   Emacs21 $(A$G(B logo $(A1mJ>$J$I$N$(G]C$(A6($r$7$F$$$k7=$OLX$KW"Rb$7$F$/$@$5$$!#(B

** elmo-cache-dirname $(A$r$(LHE$(AV9$7$F(B elmo-cache-directory $(A$rPB$(G]C$(A$7$^$7$?!#(B
   elmo-cache-directory $(A$r$(G]C$(A6($9$k$3$H$K$h$C$F%-%c%C%7%e$@$1$rH+$/$(GI1$(A$N(B
   $(A%G%#%l%/%H%j$KVC$/$3$H$,$G$-$^$9!#(B

** elmo-enable-disconnected-operation $(A$N%G%U%)%k%H$(I/N$(A$,(B t $(A$K$J$j$^$7$?!#(B
   $(A%*%U%i%$%sW4$(Ghh$(A$G$b%a%C%;$(B!<$(A%8$,%-%c%C%7%e$5$l$F$$$l$P!"$"$k3L6H$N(B
   $(A%a%C%;$(B!<$(A%82YWw$,?ID\$G$9!#(B

** \"$\" $(A%^$(B!<$(A%/$N86$$$?%a%C%;$(B!<$(A%8$O!"%a%C%;$(B!<$(A%8$N$(I(M$(ALe$,O{$($?$(G^[$(A:O$K$O%5%^%j$+$i(B
   $(A$bO{$($k$h$&$K$J$j$^$7$?!#(B
   \"$\" $(A%^$(B!<$(A%/$N86$$$?%a%C%;$(B!<$(A%8$r$(GKD$(AV1$7$?$$$(G^[$(A:O$O(B 'mark $(A%U%)%k%@$r2NUU$7$F(B
   $(A$/$@$5$$!#(B
" 0 2223 (charset japanese-jisx0208))) ((2 6 1) . #("* 2.6.0 $(A$+$i(B 2.6.1 $(A$X$N$(I+y$(A8|5c(B
  2.6.1 $(A$O(B 2.6.0 $(A$NP^U}0f$G$9!#(B

** Emacs 21 $(A$G(B Recursive load... $(A$H3v$k2;>_:O$,P^U}$5$l$^$7$?!#(B

** XEmacs 21.1 $(A$GNDWV;/$1$9$k$(GYBwn$(A$,P^U}$5$l$^$7$?!#(B

** XEmacs $(A$G(B IMAP4 $(A$rSC$$$k$H%W%m%0%l%9%P$(B!<$(A$,3v$C$Q$J$7$K$J$k$(GYBwn$(A$,(B
   $(AP^U}$5$l$^$7$?!#(B

** X- $(A$GJ<$^$k%U%#$(B!<$(A%k%I$N$(I<D$(AKw$,$G$-$J$$$(GYBwn$(A$,P^U}$5$l$^$7$?!#(B

** $(A$=$NK{$$$/$D$+$NP^U}!#(B
" 0 239 (charset japanese-jisx0208))) ((2 6 0) . #("* 2.4.1 $(A$+$i(B 2.6.0 $(A$X$N$(I+y$(A8|5c(B

** FLIM 1.13.x $(A$O%5%]$(B!<$(A%H$5$l$J$/$J$j$^$7$?!#(B
   FLIM 1.14.1 $(ART=5$r%$%s%9%H$(B!<$(A%k$7$F$/$@$5$$!#(B

** $(A%U%)%k%@!"%5%^%j$r$(GI1$(A%U%l$(B!<$(A%`$GFp$(GY/$(A$G$-$k$h$&$K$J$j$^$7$?!#(B
   $(APB$(G]=$(I+y$(AJ}(B wl-folder-use-frame$(A!"(Bwl-summary-use-frame $(A$,(B non-nil $(A$J$i(B
   $(A$=$l$>$l%U%)%k%@!"%5%^%j$r$(GI1$(A%U%l$(B!<$(A%`$K$(Gbd$(A$-$^$9(B($(A%G%U%)%k%H$O(B nil)$(A!#(B

** $(A%5%^%j$G$N(B 'N' $(A$d(B 'P' $(A$K$h$k%+$(B!<$(A%=%kRF$(GY/$(A$,8_KY;/$5$l$^$7$?!#(B

** $(A%m$(B!<$(A%+%k%U%)%k%@$r$(I%\$(AOs$H$7$?!"(Blast $(A$*$h$S(B first $(ALu<~$N%U%#%k%?%U%)%k%@(B
   ($(A@}$($P!"(B/last:100/+inbox $(A$N$h$&$J%U%)%k%@(B) $(A$N%A%'%C%/$,8_KY;/$5$l$^$7$?!#(B

** POP, IMAP $(A$G4s$-$J%a%C%;$(B!<$(A%8$rH!$j<D$;$k$H$-$K!"(B
   $(GbP$(H/L$(A$,1mJ>$5$l$k$h$&$K$J$j$^$7$?!#(B

** $(A%5%^%j$NI+$E$1$,%*%s%G%^%s%I$K$J$j$^$7$?(B(Emacs $(A$N$_(B)$(A!#(B
   $(APB$(G]=$(I+y$(AJ}(B wl-summary-lazy-highlight $(A$,(B non-nil $(A$J$i%5%^%j$N1mJ>2?7V$N$_$r(B
   $(AWT$(GY/$(A5D$KI+86$1$7$^$9(B (Emacs $(A$N$_(B)$(A!#(B

** biff $(A$NM(V*7=7($,%+%9%?%^%$%:?ID\$K$J$j$^$7$?!#(B
   $(APB$(G]=$(A%U%C%/(B wl-biff-notify-hook, wl-biff-unnotify-hook $(A$G$(G]C$(A6($G$-$^$9!#(B
   $(A@}(B: (add-hook wl-biff-notify-hook 'ding)

** $(A6`$/$N%P%0%U%#%C%/%9(B
" 0 685 (charset japanese-jisx0208))) ((2 4 1) . #("* 2.4.0 $(A$+$i(B 2.4.1 $(A$X$N$(I+y$(A8|5c(B
  2.4.1 $(A$O(B 2.4.0 $(A$NP^U}0f$G$9!#(B

** FLIM 1.14.x $(AIO$G$b$(GY/$(AWw$9$k$h$&$K$J$j$^$7$?!#(B

** POP before SMTP $(A$G(B POP $(A%3%M%/%7%g%s$,GP$l$J$$2;>_:O$,P^U}$5$l$^$7$?!#(B

** IMAP4 $(A$G$NIz%Q%9%o$(B!<$(A%I$K$h$k$(Gk%b%$(A$NV86(7=7($,$(I+y$(A8|$K$J$j$^$7$?!#(B

$(A$3$l$^$G!"(BIMAP4 $(A$GIz%Q%9%o$(B!<$(A%I$N$(Gk%b%(B(LOGIN $(A%3%^%s%I$K$h$k%m%0%$%s(B)$(A$r$9$k$K$O!"(B
$(I+y$(AJ}(B elmo-default-imap4-authenticate-type $(A$K(B 'plain ($(A$^$?$O(B nil) 
$(A$r$(G]C$(A6($9$k$3$H$K$J$C$F$$$^$7$?$,!"(B'clear ($(A$^$?$O(B nil)$(A$K$(I+y$(A8|$5$l$^$7$?!#(B
$(A@}$($P!"(B
\(setq elmo-default-imap4-authenticate-type 'plain)
$(A$H$$$&$(G]C$(A6($O(B
\(setq elmo-default-imap4-authenticate-type 'clear)
$(A$K$(I+y$(A8|$9$k1XR*$,$"$j$^$9!#(B
" 0 448 (charset japanese-jisx0208))) ((2 4 0) . #("* 1.1.1 $(A$+$i(B 2.4.0 $(A$X$N$(I+y$(A8|5c(B

** $(A%P$(B!<$(A%8%g%s7,:E(B
$(A%P$(B!<$(A%8%g%s7,:E$N86$17=$,$(I+y$(A$o$j$^$7$?!#(B
$(A$3$l$^$G!"(B1.x $(A$,026(0f!"(B2.0.x$(B!A(B2.2.x $(A$,$(Gbd$(BH/$(A0f$H$J$C$F$$$^$7$?$,!"(B2.3.0 
$(ART=5$O!"5Z6~7,:E$,E<J}$J$i026(0f!"FfJ}$J$i$(Gbd$(BH/$(A0f$H$J$j$^$7$?!#$3$N%P$(B!<(B
$(A%8%g%s7,:E$N86$17=$OR;0c5D$J%*$(B!<$(A%W%s%=$(B!<$(A%9$(Gbd$(BH/$(A$N$(Ghk\d$(A$K;y$E$/$b$N$G$9!#(B

$(A9+$(Gbd(B CVS $(A%5$(B!<$(A%P(B cvs.m17n.org $(AIO(B $(A$G$O!"$(Gcl$(A$,(B beta ($(Gbd$(BH/(B)$(A0f!"(B
$(AV&$,(B stable ($(A026((B)$(A0f(B ($(AV&C{$O!"(B2.4.x $(A$J$i(B wl-2_4) $(A$H$J$j$^$9!#(B

** $(A%$%s%9%H$(B!<$(A%k(B

*** FLIM 1.12 $(A$O%5%]$(B!<$(A%H$5$l$J$/$J$j$^$7$?!#(B
$(A$/$o$7$/$O(B INSTALL.ja $(A$rSy$(IRf$(AOB$5$$!#(B

*** APEL 10.2 $(ART=5$,1XR*$K$J$j$^$7$?!#(B
tm-8 $(A$rJ9SC$9$k7=$OLX$K$4W"Rb$/$@$5$$!#(B

** $(APB$(Gq"$(AD\(B

*** LDAP $(A%5%]$(B!<$(A%H(B
LDAP $(A%5$(B!<$(A%P$(B!<$(A$H=S$(BB3$(A$7!"%"%I%l%9$N$(GfP$(AMj$rPP$($^$9!#(B
$(I+y$(AJ}(B wl-use-ldap $(A$,(B non-nil $(A$K$(G]C$(A6($5$l$F$$$k$H(B LDAP $(A$r@{SC$7$^$9(B
\($(A3uFZ$(G]C$(A6($O(B nil)$(A!#(B

*** POP3 $(A%U%)%k%@$G(B UIDL $(A%5%]$(B!<$(A%H(B
POP3 $(A%U%)%k%@$G%5%^%j$NW4$(Ghh$(A$r1#4f$G$-$k$h$&$K$J$j!"%"%/%;%9$,8_KY;/$5$l$^$7$?!#(B
$(I+y$(AJ}(B elmo-pop3-use-uidl $(A$,(B non-nil $(A$K$(G]C$(A6($5$l$F$$$k$H(B UIDL $(A$rJ9SC$7$^$9(B
\($(A3uFZ$(G]C$(A6($O(B t)$(A!#(B

*** Emacs 21 $(A%5%]$(B!<$(A%H(B
Standard Emacs 21 $(A$N%5%]$(B!<$(A%H$r$(Gbd$(AJ<$7$^$7$?!#(BWanderlust $(A$N$[$H$s(B
$(A$I$N%U%l$(B!<$(A%`$K!"(BXEmacs $(A$HM,$8$h$&$K%D$(B!<$(A%k%P$(B!<$(A$d%"%$%3%s;-Oq$r1m(B
$(AJ>$7$^$9!#(B

*** biff $(Gq"$(AD\(B
$(AR;6($(GUkbf$(A$*$-$K%5$(B!<$(A%P$K%a$(B!<$(A%k$,=l$$$F$$$k$+$(Gm}k%$(A$7$^$9!#(B
$(A=l$$$F$$$l$P%b$(B!<$(A%I%i%$%s$K1mJ>$7!"%U%)%k%@R;$(IRf$(A%b$(B!<$(A%I$r8|PB$7$^$9!#(B

*** expire-hide
$(GX4$(AJBWTLe$OO{$9$3$H$J$/!"%5%^%j$K$(GKD$(A$($k$(GX4$(AJBJ}$rR;6($K1#$D$3$H$,$G(B
$(A$-$k$h$&$K$J$j$^$7$?!#$R$H$D$N%U%)%k%@$K4sA?$K$(GX4$(AJB$rAo$a$F$$$k$(G^[(B
$(A:O$G$b!"KY6H5MOB$rRV$($k$3$H$G$-$^$9!#(B

*** $(A%9%l%C%IP^$(G_&q"$(AD\(B
$(A%5%V%8%'%/%H$+$iMF$(G`A$(A$7$?%9%l%C%I$NWT$(GY/$(A$N$D$J$.V1$7!"<0$S(B
$(AJV$(GY/$(A$G$N$D$J$.V1$7(B($(A%5%^%j$G(B M-w ($(A%3%T$(B!<(B)$(A$H(B C-y ($(A%Z$(B!<$(A%9%H(B)) $(A$,?ID\$K$J$j$^$7$?!#(B

*** $(A%Q%9%o$(B!<$(A%I$N%?%$%^$(G]C$(A6((B
$(I+y$(AJ}(B elmo-passwd-life-time $(A$G$(G]C$(A6($G$-$^$9!#(B
\(nil $(A$J$i%?%$%^$J$7!#3uFZ$(G]C$(A6($O(B nil)$(A!#(B

*** killed-list
NNTP $(A%U%)%k%@$GOw3}$7$?%a%C%;$(B!<$(A%8$O(B killed-list $(A$K1#4f$7$^$9!#(B
killed-list $(A$K$"$k%a%C%;$(B!<$(A%8$O%5$(B!<$(A%PIO$K$b4fTZ$7$J$$$+$N$h$&$K$(H$/(B
$(A$$$^$9!#$(I+y$(AJ}(B elmo-use-killed-list $(A$,(B non-nil $(A$J$i(B killed-list $(A$r(B
$(AJ9SC$7$^$9(B($(A%G%U%)%k%H$O(B t)$(A!#(B
$(A$3$l$K$h$C$F(B NNTP $(A$r@{SC$7$?%Q%$%W%U%)%k%@$b$(I(M$(G\"$(A$G$-$k$h$&$K$J$j$^$7$?!#(B

*** Maildir $(A$G(B pack ($(A7,:E$(Gf^$(A$a(B) $(A$,$G$-$k$h$&$K$J$j$^$7$?!#(B
Maildir $(A$N%5%^%j$G(B M-x wl-summary-pack-number $(A$r$(I(M$(APP$9$k$H%a%C%;$(B!<$(A%87,:E$r(B
1 $(A$+$i$(Gb{$(A$KUq$jV1$7$^$9!#(B

** $(I<D$(AKw(B

*** $(A%U%#%k%?%U%)%k%@$K$(Gno$(IJI$(A$JLu<~V86($rV86($G$-$k$h$&$K$J$j$^$7$?!#(B
AND $(ALu<~!"(BOR $(ALu<~!"7q6(Lu<~!"$*$h$S$=$l$i$N$(G\Z$(A:O$;$rV86($G$-$^$9!#(B
$(A$3$l$K$H$b$J$$!"Lu<~V86(2?7V$N%7%s%?%C%/%9$,$(I+y$(A8|$5$l$^$7$?!#(B
$(A$/$o$7$/$O(B Info $(A$rSy$(IRf$(AOB$5$$!#(B

$(AW"Rb#:(B1.1.1 $(A$+$iRFPP$5$l$k7=$X(B
$(AIO$(GX4$(I+y$(A8|$K0i$$!"%U%#%k%?%U%)%k%@$N(B msgdb $(A$NVC$-$(G^[$(AKy$,$(I+y$(A$o$j$^$7$?!#(B
$(A$3$N$?$a!"$(I0q$(A@4$N(B msgdb $(A$O2;R*$H$J$j$^$9!#$=$N$^$^$G$b$(GYBwn$(A$"$j$^$;$s$,!"(B
$(A%G%#%9%/$K$(G`R$(IJj$(A$J%G$(B!<$(A%?$r2P$7$?$/$J$$7=$O(B .elmo/filter/ $(ARTOB$r(B
$(A$"$i$+$8$aOw3}$7$F$*$$$F$/$@$5$$!#(B

*** NNTP $(A$G$N$(I<D$(AKw$(Gq"$(AD\$,$(GZ0$(A;/$5$l$^$7$?!#(B
NNTP $(A$K$(I%\$(A$9$k%U%#%k%?%U%)%k%@$rWw$l$k$h$&$K$J$j$^$7$?!#(B
\(NNTP $(A%5$(B!<$(A%P$,(B XHDR $(A%3%^%s%I$K$(I%\%w$(A$7$F$$$k$(G^[$(A:O$N$_(B)

*** $(A%5%^%j$G$N(B Pick$(A!"(BVirtual $(A$G$(Gno$(A:OLu<~$rHkA&$G$-$k$h$&$K$J$j$^$7$?!#(B
AND $(ALu<~$d(B OR $(ALu<~$bHkA&$G$-$^$9!#(B
$(AHkA&7=7($O!"%U%#$(B!<$(A%k%IC{$N$+$o$j$K(B 'AND' $(A$d(B 'OR' $(A$rHkA&$9$k$@$1$G$9!#(B

** $(A=S$(BB3$(A!$$(Gk%b%(B

*** elmo-default-*-authenticate-type $(A$O%7%s%\%k$G$(G]C$(A6($9$k$h$&$K$J$j$^$7$?!#(B
$(A@}$($P!"(B
\(setq elmo-default-imap4-authenticate-type \"cram-md5\")
$(A$H$$$&$(G]C$(A6($O!"(B
\(setq elmo-default-imap4-authenticate-type 'cram-md5)
$(A$K$(I+y$(A8|$9$k1XR*$,$"$j$^$9!#(B

*** stream-type $(A$N6($(Gex$(A7=7($r$(I+y$(A8|$7$^$7$?!#(B
$(I+y$(AJ}(B elmo-network-{imap4-,pop3-,nntp-,}stream-type-alist $(A$G$(G]C$(A6(?ID\$G$9!#(B
SSL $(IJ=$(G]Y$(A$N$$$/$D$+$N$(I+y$(AJ}$,$(LHE$(AV9$5$l$^$7$?(B($(A8DC{(B)$(A!#(B 
$(A$^$?!"PB$?$K(B \"!socks\" $(A$G$(G\\$(A$o$k%M%C%H%o$(B!<$(A%/O5%U%)%k%@(B(IMAP4, NNTP, POP3)$(A$O(B
SOCKS $(I8+$(ASI$G%"%/%;%9$5$l$k$h$&$K$J$j$^$7$?!#(B

** $(A%I%i%U%H(B

*** group-list $(A$K$(I%\%w$(A$7$^$7$?!#(B
$(AMpOH$K(B Group: foo@gohome.org, bar@gohome.org; $(A$N$h$&$K$(GUs$(A$1$k$h$&$K(B
$(A$J$j$^$7$?!#$(I+y$(AJ}(B wl-draft-remove-group-list-contents $(A$,(B t $(A$J$i(B
group-list $(A$NDZH]$rOw3}$7$FKMPE$7$^$9!#(B

*** $(A%I%i%U%H$N%W%l%S%e$(B!<$(A$GJ\H!HK$N%"%I%l%9$,%_%K%P%C%U%!$K1mJ>$5$l$^$9!#(B
group-list $(A$K$b$(I%\%w$(A$7$F$$$^$9!#(B

*** $(A3uFZ$(G]C$(A6($G(B Reply-To: $(A$r?<$(Gle$(A$9$k$h$&$K$J$j$^$7$?!#(B
wl-draft-reply-without-argument-list $(A$N3uFZ$(G]C$(A6($G!"(BReply-To: 
$(A%U%#$(B!<$(A%k%I$O(B To: $(A$X$(I1<$(AHk$9$k$(G]C$(A6($K$J$j$^$7$?!#(B

*** $(AWT7V$N%a$(B!<$(A%k$X$N75PE%k$(B!<$(A%k(B
$(I+y$(AJ}(B wl-draft-reply-myself-with-argument-list,
wl-draft-reply-myself-without-argument-list $(A$GWT7V$,3v$7$?%a$(B!<$(A%k(B
$(A$X$N75PE$9$k$H$-$N%k$(B!<$(A%k$,$(G]C$(A6($G$-$^$9!#(B

*** $(A75PE%"%I%l%9$K%U%k%M$(B!<$(A%`(B
$(I+y$(AJ}(B wl-draft-reply-use-address-with-full-name $(A$,(B non-nil $(A$J$i75(B
$(APE%"%I%l%9$K%U%k%M$(B!<$(A%`$,Hk$j$^$9!#(B($(A%G%U%)%k%H$O(B t)$(A!#(B 

*** In-Reply-To: $(A%U%#$(B!<$(A%k%I$NPNJ=$r$(I+y$(A8|$7$^$7$?!#(B
draft-ietf-drums-msg-fmt-09.txt $(A$K$(I0q$(A$&$h$&$K$J$j$^$7$?!#(B

** $(A$=$NK{$N$(I+y$(A8|5c(B

*** $(A%9%l%C%I$N8_KY;/$H6`$/$N%P%0%U%#%C%/%9!#(B

*** $(I+y$(AJ}C{$N$(I+y$(A8|!#(B
wl-refile-guess-func-list => wl-refile-guess-functions
wl-summary-temp-above => wl-summary-target-above

*** wl-fcc $(A$K$(IJ=$(AJ}$r$(G]C$(A6($G$-$^$9!#(B
$(ATB$(I#z$(A$K%U%)%k%@$r$(I+y$(A$($?$$$(G^[$(A:O$J$I$KJ9SC$G$-$^$9!#(B

*** elmo-search-mime-charset $(A$O$(LHE$(AV9$5$l$^$7$?!#(B
charset $(A$OHkA&NDWVAP$+$iEP6($5$l$^$9!#(B

*** $(BE>$(AKM$(GUk$(A$KS`$(GSS$(A$J%X%C%@$rOw3}$7$^$9!#(B
$(I+y$(AJ}(B wl-ignored-forwarded-headers $(A$G!"$(BE>$(AKM$(GUk$(A$KOw3}$9$k%X%C%@$r$(G]C(B
$(A6($G$-$^$9!#(B

*** wl-highlight-group-folder-by-numbers $(A$O$(LHE$(AV9$5$l$^$7$?!#(B
wl-highlight-folder-by-numbers $(A$K8DC{$5$l!"$(I/N$(A$K$(I%w$(A$8$FRTOB$NRbN6$r3V$D$h$&$K(B
$(A$J$j$^$7$?!#(B
  `t'   $(A#:PPH+Le$K%a%C%;$(B!<$(A%8J}$K$(I%w$(A$8$?I+$r86$1$^$9!#(B
  `nil' $(A#:%U%)%k%@$NW4$(Ghh$(A$K$(I%w$(A$8$?I+$r86$1$^$9!#(B
   $(AJ}WV(B ($(A@}$($P(B `1') $(A#:%a%C%;$(B!<$(A%8J}$H%U%)%k%@$NW4$(Ghh$(A$N$(I"a$(A7=$K$(I%w$(A$8$?I+$r86$1$^$9!#(B

*** $(A%a%C%;$(B!<$(A%8%P%C%U%!$G$N%X%C%@1mJ>$rVFSy$G$-$^$9!#(B
$(I+y$(AJ}(B wl-message-ignored-field-list,
wl-message-visible-field-list $(A$G!"(BWanderlust $(A%l%Y%k$G$(G]C$(A6($,?ID\(B
$(A$K$J$j$^$7$?!#(B($(A=q$^$G$O(B SEMI $(A$G$(G]C$(A6($9$k1XR*$,$"$j$^$7$?(B)

*** DEMO $(A$N1mJ>7=7($,$(I+y$(A$o$j$^$7$?!#(B
$(A%+%i$(B!<$(A$N%T%C%/%9%^%C%W$G@{SC$9$kI+J}$,Ow$(G`5$(A$5$l$^$7$?!#(B
$(A$^$?!"NDWV$N$_$7$+1mJ>$G$-$J$$$(Gt?$(A>3$G$b$=$l$J$j$N%G%b$,1mJ>$5$l$k$h$&$K(B
$(A$J$j$^$7$?!#(B
" 0 3773 (charset japanese-jisx0208))) ((1 1 1) . #("* 1.1.0 $(A$+$i(B 1.1.1 $(A$X$N$(I+y$(A8|5c(B
  1.1.1 $(A$O(B 1.1.0 $(A$N%P%0P^U}0f$G$9!#$$$/$D$+$N$(G\X$(A$+$$P^U}$,<S$o$C$F$$$^$9!#(B

** CVS $(A%5$(B!<$(A%PIO$G$N$(Gbd$(BH/$(A$,J<$a$i$l$^$7$?!#(B

** $(A%G%#%l%/%H%j$(Gi,$(A3I$,$+$o$j$^$7$?!#(B

*** 00README, 00README.ja $(A$O(B README, README.ja $(A$K$(I+y$(A8|$5$l$^$7$?!#(B

*** wl-* $(A$N%U%!%$%k$O(B 'wl' $(A%G%#%l%/%H%j$KRF$(GY/$(A$7$^$7$?!#(B

** wl-refile-rule-alist $(A$N$(GX4$(AJv7=7($,$(I)/$(GZ/$(A$5$l$^$7$?(B($(ARTG0$H;%$(G_P$(APT$,$"$j$^$9(B)$(A!#(B

** progress gauge $(A1mJ>$(Gq"$(AD\$r@{SC$9$k$h$&$K$J$j$^$7$?!#(B
progress gauge $(A$N1mJ>$(Gq"$(AD\$r$b$D(B Emacs $(A$G$O!"$(B=h$(A@m$N$(GbP$(H/L$(A$,(B progress gauge $(A$K(B
$(A1mJ>$5$l$k$h$&$K$J$j$^$7$?!#(B
" 0 372 (charset japanese-jisx0208))) ((1 1 0) . #("* 1.0.3 $(A$+$i(B 1.1.0 $(A$X$N$(I+y$(A8|5c(B 

** $(A%$%s%9%H$(B!<$(A%k(B

*** tm7 $(A$O%5%]$(B!<$(A%H$5$l$J$/$J$j$^$7$?!#(B

$(A$/$o$7$/$O(B INSTALL.ja $(A$rSy$(IRf$(AOB$5$$!#(B

*** WL_PREFIX $(A$H(B ELMO_PREFIX $(A$N3uFZ$(G]C$(A6($,(B \"wl\" $(A$K$J$j$^$7$?!#(B
\(defvar WL_PREFIX \"wl\")
\(defvar ELMO_PREFIX \"wl\")

$(A@}$($P!"%$%s%9%H$(B!<$(A%k%G%#%l%/%H%j$O!"(B
  1.0.3  /usr/local/share/emacs/site-lisp/
  1.1.0  /usr/local/share/emacs/site-lisp/wl/
$(A$H$J$j$^$9!#(B

*** Makefile $(A$N$(I+y$(AJ}$N%G%U%)%k%H$(I/N$(A$,$(I+y$(A$o$j$^$7$?!#(B

EMACS   = emacs
XEMACS  = xemacs
$(XEMACS) $(A$O!"(B`package' $(A$d(B `install-package' $(A$N(B target $(A$G2NUU$5$l$^$9!#(B

*** *.el $(A%U%!%$%k$b%$%s%9%H$(B!<$(A%k$5$l$k$h$&$K$J$j$^$7$?!#(B

*** $(AS"$(Gk#$(A0f%I%-%e%a%s%H(B (wl.texi) $(A$,86$-$^$7$?!#(B

** $(APB$(Gq"$(AD\(B

*** Modified UTF7 $(A$,%5%]$(B!<$(A%H$5$l$^$7$?!#(B
$(A%f%K%3$(B!<$(A%I$,$(H$/$(A$($k(B Emacs $(A$G$O!"(BIMAP4 $(A$GHU1>$(Gk#$(A%a$(B!<$(A%k%\%C%/%9C{$rV86($G$-$^$9!#(B

*** $(A%9%3%"$(Gq"$(AD\$,86$-$^$7$?!#(B

*** $(A%W%i%09\@m$(Gq"$(AD\$,86$-$^$7$?!#(B

*** IMAP4 $(A$,$h$j$(GHG$(ASC5D$K$J$j$^$7$?!#(B
$(A6`$/$N(B IMAP4 $(A%5$(B!<$(A%P$G$(GY/$(A$/$h$&$K$J$j$^$7$?!#(B

*** $(A$$$/$D$+$N$(Gk%b%$(A7=J=$,%5%]$(B!<$(A%H$5$l$^$7$?!#(B
  IMAP4: CRAM-MD5, DIGEST-MD5, STARTTLS
  POP3:  CRAM-MD5, DIGEST-MD5, SCRAM-MD5, STARTTLS
  NNTP:  STARTTLS
  SMTP:  STARTTLS

*** $(APB$7$$%U%)%k%@PM$,<S$o$j$^$7$?!#(B
  |      $(A%Q%$%W%U%)%k%@(B     $(A%a%C%;$(B!<$(A%8$rH!$j$(B9~$(A$`%U%)%k%@$G$9!#(B
  .      Maildir $(A%U%)%k%@(B   Maildir $(A$,$R$H$D$N%U%)%k%@PM$K$J$j$^$7$?!#(B
  'cache $(A%-%c%C%7%e%U%)%k%@(B $(ADZ2?%-%c%C%7%e$r%U%)%k%@$H$7$F$(IOURf$(A$G$-$^$9!#(B

*** $(A%a%C%;$(B!<$(A%8%P%C%U%!$N%W%j%U%'%C%A$(Gq"$(AD\$,86$-$^$7$?!#(B
$(BFI$(A$s$G$$$k$(Gbf$(A$K4N$N%a%C%;$(B!<$(A%8$r$(BFI$(A$_$(B9~$(A$_$^$9!#(B

*** $(A%9%F%#%C%-$(B!<$(A%5%^%j(B($(AO{$($J$$%5%^%j(B)$(A$,$(I)/$(GZ/$(A$5$l$^$7$?!#(B
$(A%a%C%;$(B!<$(A%8%P%C%U%!$b%5%^%j$K$(I%\%w$(A$7$FSCRb$5$l$k$h$&$K$J$j$^$7$?!#(B
$(A3#$K%9%F%#%C%-$(B!<$(A$K$J$k%U%)%k%@$r$(G]C$(A6($G$-$k$h$&$K$J$j$^$7$?!#(B

** $(A$=$NK{(B

*** $(I+y$(AJ}(B wl-draft-prepared-config-alist $(A$O$(LHE$(AV9$5$l$^$7$?!#(B
wl-draft-config-alist $(A$K$(G\S$(A:O$5$l$^$7$?!#(B

*** POP-before-SMTP $(IJ=$(G]Y$(A$N$(I+y$(AJ}$,U{@m$5$l$^$7$?!#(B

*** $(A4fTZ$7$J$$%U%)%k%@$rWw$k$+$I$&$+$(Gm}k%$(A$9$k$h$&$K$J$j$^$7$?!#(B
 FCC: $(A$KPB$7$$%U%)%k%@C{$rV86($7$?$H$-$d!"(Bauto-refile $(A$G(B
 $(APB$7$$%U%)%k%@C{$rV86($7$?$H$-$K%U%)%k%@$rWw$k$+$I$&$+$(Gm}k%$(A$7$^$9!#(B

*** $(A%W%j%U%'%C%A$N$(Gm}k%$(A$K$(IJ=$(A$9$k$(G]C$(A6($N$(I+y$(AJ}$,<S$o$j$^$7$?!#(B
wl-prefetch-confirm-threshold, wl-cache-fetch-threshold.

*** $(A%U%)%k%@C{$N$"$@C{$r%U%)%k%@C{HkA&$G$(GfP$(AMj$G$-$k$h$&$K$J$j$^$7$?!#(B

*** Message-ID $(A$NIz3I7=7($,$(I+y$(A$o$j$^$7$?!#(B

*** Mule $(A$G$O%S%C%H%^%C%W$N%*$(B!<$(A%W%K%s%0%G%b;-Cf$,3v$k$h$&$K$J$j$^$7$?!#(B

*** `smtp-server' $(A$K$(IJ=$(AJ}$rV86($G$-$^$9!#(B

*** $(AKMPE%m%0$,1#4f$5$l$k$h$&$K$J$j$^$7$?!#(B
`wl-draft-sendlog' $(A$,(B non-nil $(A$N$(G^[$(A:O!"(B'sendlog' $(A%U%!%$%k$K1#4f$5$l$^$9!#(B

*** $(A%*%U%i%$%s$(B=h$(A@m$G%W%j%U%'%C%A$rSh$(GR|$(A$G$-$k$h$&$K$J$j$^$7$?!#(B

*** `wl-summary-incorporate-marks'

*** `wl-draft-use-frame' $(A$,(B non-nil $(A$J$i%U%l$(B!<$(A%`$rIz3I$7$^$9!#(B

*** $(APB$(G]=$(I+y$(AJ}(B `wl-user-mail-address-list'$(A!#(B

*** $(APB$(G]=$(I+y$(AJ}(B `wl-local-domain'$(A!#(B

*** IMAP4 $(A$G%5$(B!<$(A%P$(GY!$(A$NN4$(BFI$(AW4$(Ghh$(A$r2NUU$9$k$h$&$K$J$j$^$7$?!#(B

*** $(A3uFZ$(G]C$(A6($,$(I+y$(A8|$5$l$?$(I+y$(AJ}(B
  wl-mime-charset         iso-2022-jp  =>  x-ctext
  wl-summary-move-order   'new  =>  'unread
  wl-tmp-dir              TMPDIR  =>  ~/tmp/

*** $(APB$(G]=(B hook
  wl-draft-send-hook
  wl-draft-reedit-hook
  wl-mime-edit-preview-message-hook
  wl-folder-suspend-hook
  wl-summary-toggle-disp-folder-message-resumed-hook
  wl-summary-line-inserted-hook
  wl-thread-update-children-number-hook
  mmelmo-header-inserted-hook
  mmelmo-entity-content-inserted-hook

*** $(APB$(G]=$(A%3%^%s%I(B
  wl-save
  wl-summary-write
  wl-summary-supersedes-message
  wl-fldmgr-delete
  wl-refile-guess-by-msgid
  wl-address-user-mail-address-p
  wl-summary-jump-to-msg-by-message-id-via-nntp
  wl-summary-temp-mark-pick
" 0 198 (charset japanese-jisx0208) 198 2592 (charset japanese-jisx0208))))))

;;; -*- news-list-end -*-

(defun wl-news-previous-version-load ()
  (with-temp-buffer
    (let ((filename (expand-file-name
		     wl-news-version-file-name
		     elmo-msgdb-directory))
	  insert-file-contents-pre-hook
	  insert-file-contents-post-hook
	  ret-val)
      (if (not (file-readable-p filename))
	  (cons wl-news-default-previous-version
		wl-news-default-previous-version)
	(insert-file-contents filename)
	(condition-case nil
	    (read (current-buffer))
	  (error nil nil))))))

(defun wl-news-previous-version-save (current-version previous-version)
  (with-temp-buffer
    (let ((filename (expand-file-name
		     wl-news-version-file-name
		     elmo-msgdb-directory))
	  print-length print-level)
      (prin1 (cons current-version previous-version) (current-buffer))
      (princ "\n" (current-buffer))
      (if (file-writable-p filename)
	  (write-region (point-min) (point-max)
			filename nil 'no-msg)
	(message "%s is not writable." filename)))))

(defun wl-news-append-news (lang previous-version &optional no-mime-tag)
  (require 'wl-mime)
  (let ((news-list (cdr (assoc lang wl-news-news-alist)))
	ret)
    (when news-list
      (if no-mime-tag
	  (insert "--------------\n")
	(mime-edit-insert-tag "text" "plain" "" ""))
      (while (< 0
		(product-version-compare
		 (car (car news-list))
		 previous-version))
	(setq ret t)
	(insert (cdr (car news-list)) "\n\n")
	(setq news-list (cdr news-list))))
    ret))

(defun wl-news-check-news (version news-lang)
  (let ((lang news-lang)
	news-list ret)
    (while (car lang)
      (setq news-list (cdr (assoc (car lang) wl-news-news-alist)))
      (while (< 0
		(product-version-compare
		 (car (car news-list)) version))
	(setq ret t)
	(setq news-list (cdr news-list)))
      (setq lang (cdr lang)))
    ret))

(defun wl-news-already-current-p ()
  (>= 0 (product-version-compare
	 (product-version (product-find 'wl-version))
	 (car (wl-news-previous-version-load)))))

(defun wl-news-send-news (version news-lang folder)
  (require 'wl-draft)
  (let ((lang (if (listp wl-news-lang)
		  wl-news-lang
		(list wl-news-lang)))
	send-buffer
	wl-fcc wl-bcc ret)
    (save-window-excursion
      (set-buffer
       (setq send-buffer (wl-draft-create-buffer)))
      (wl-draft-create-contents
       (list (cons 'From "WL Release 'Bot <wl@lists.airs.net>")
	     (cons 'To (wl-draft-eword-encode-address-list wl-from))
	     (cons 'Subject "Wanderlust NEWS")
	     (cons 'Date (wl-make-date-string))
	     (cons 'User-Agent wl-generate-mailer-string-function)))
      (wl-draft-insert-mail-header-separator)
      (wl-draft-prepare-edit)
      (goto-char (point-max))
      (insert "\nThis message is automatically generated by Wanderlust.\n\n")
      ;; insert news
      (while (car lang)
	(wl-news-append-news (car lang) version)
	(setq lang (cdr lang)))
      ;; encode
      (let ((mime-header-encode-method-alist
	     '((eword-encode-unstructured-field-body))))
	(mime-edit-translate-buffer))
      (wl-draft-get-header-delimiter t)
      (setq ret
	    (and (elmo-folder-writable-p
		  (wl-folder-get-elmo-folder folder))
		 (elmo-folder-append-buffer
		  (wl-folder-get-elmo-folder folder))))
      (wl-draft-hide send-buffer)
      (wl-draft-delete send-buffer))
    ret))

;;; wl-news-mode

(defvar wl-news-buf-name "NEWS")
(defvar wl-news-mode-map nil)
(defvar wl-news-winconf nil)
(defvar wl-news-buffer-oldest-version nil)
(make-variable-buffer-local 'wl-news-buffer-oldest-version)

(unless wl-news-mode-map
  (setq wl-news-mode-map (make-sparse-keymap))
  (define-key wl-news-mode-map "q"     'wl-news-exit)
  (define-key wl-news-mode-map "Q"     'wl-news-force-exit)
  (define-key wl-news-mode-map "\C-xk" 'wl-news-exit)
  (define-key wl-news-mode-map "a"     'wl-news-show-all)
  (define-key wl-news-mode-map "m"     'wl-news-append-to-folder)
  (define-key wl-news-mode-map "\C-m"  'wl-news-next-line)
  (define-key wl-news-mode-map " "     'wl-news-next-page)
  (define-key wl-news-mode-map "\177"  'wl-news-previous-page)
  ;; re-bind commands of outline-mode
  (define-key wl-news-mode-map "n"     'outline-next-visible-heading)
  (define-key wl-news-mode-map "p"     'outline-previous-visible-heading)
  (define-key wl-news-mode-map "u"     'outline-up-heading)
  (define-key wl-news-mode-map "N"     'outline-forward-same-level)
  (define-key wl-news-mode-map "P"     'outline-backward-same-level))

(require 'derived)
(define-derived-mode wl-news-mode outline-mode "NEWS"
  "Mode for Wanderlust NEWS(.ja)."
  (setq buffer-read-only t))

(defun wl-news (&optional arg)
  (interactive "P")
  (remove-hook 'wl-hook 'wl-news)
  (let* ((previous-version (if arg wl-news-default-previous-version
			     (cdr (wl-news-previous-version-load))))
	 (lang wl-news-lang)
	 window-lines lines)
    (if (or (get-buffer wl-news-buf-name)
	    (if (wl-news-check-news previous-version wl-news-lang)
		(progn
		  (setq wl-news-winconf (current-window-configuration))
		  (set-buffer (get-buffer-create wl-news-buf-name))
		  (wl-news-mode)
		  (setq wl-news-buffer-oldest-version previous-version)
		  (buffer-disable-undo (current-buffer))
		  ;; insert news
		  (let ((buffer-read-only nil))
		    (insert "--- Wanderlust NEWS ---  press 'a' to show all NEWS\n")
		    (insert "                         press 'm' to mail this NEWS to your folder\n")
		    (insert "                         press 'q' to quit\n")
		    (insert "                         press 'Q' to force quit\n\n")
		    (while (car lang)
		      (wl-news-append-news
		       (car lang) previous-version t)
		      (setq lang (cdr lang))))
		  t)
	      (message "No NEWS.")
	      nil))
	(progn
	  (switch-to-buffer wl-news-buf-name)
	  (delete-other-windows)
	  (goto-char (point-min))))))

(defun wl-news-next-line ()
  (interactive)
  (scroll-up 1))

(defun wl-news-next-page ()
  (interactive)
  (scroll-up))

(defun wl-news-previous-page ()
  (interactive)
  (scroll-down))

(defun wl-news-show-all ()
  (interactive)
  (when (eq major-mode 'wl-news-mode)
    (kill-buffer (current-buffer))
    (wl-news t)))

(defun wl-news-exit ()
  (interactive)
  (let* ((oldest-version (cdr (wl-news-previous-version-load)))
	 (current-version (product-version (product-find 'wl-version)))
	 (new-old-version current-version)
	 (buf (get-buffer wl-news-buf-name)))
    (when buf
      (if (wl-news-check-news oldest-version wl-news-lang)
	  (if (y-or-n-p "Do you want to see this message again? ")
	      (progn
		(message "Please M-x wl-news if you want to see it.")
		(setq new-old-version oldest-version))))
      (wl-news-previous-version-save
       current-version new-old-version)
      (kill-buffer (current-buffer))
      (if wl-news-winconf
	  (set-window-configuration wl-news-winconf))
      (kill-buffer buf)
      (if wl-news-winconf
	  (set-window-configuration wl-news-winconf)))))

(defun wl-news-append-to-folder ()
  (interactive)
  (let* ((current-version (product-version (product-find 'wl-version)))
	 (new-old-version current-version)
	 (folder wl-default-folder))
    (if (or (and (elmo-folder-writable-p
		  (wl-folder-get-elmo-folder folder))
		 (y-or-n-p (format
			    "Do you want to append this message to %s ? "
			    wl-default-folder)))
	    (setq folder
		  (wl-summary-read-folder wl-default-folder "to append ")))
	(or (wl-news-send-news wl-news-buffer-oldest-version wl-news-lang folder)
	    (error "Cannot append NEWS mail to %s" folder)))))

(defun wl-news-force-exit ()
  (interactive)
  (let ((buf))
    (when (setq buf (get-buffer wl-news-buf-name))
      (wl-news-previous-version-save
       (product-version (product-find 'wl-version))
       (cdr (wl-news-previous-version-load)))
      (kill-buffer buf)
      (if wl-news-winconf
	  (set-window-configuration wl-news-winconf)))))


(require 'product)
(product-provide (provide 'wl-news) (require 'wl-version))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; wl-news.el ends here
