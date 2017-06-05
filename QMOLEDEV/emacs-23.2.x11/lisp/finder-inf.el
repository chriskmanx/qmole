;;; finder-inf.el --- automatically extracted keyword-to-package mapping
;;
;;; Code:

(setq finder-package-info '(
    ("abbrev.el"
        "abbrev mode commands for Emacs"
        (abbrev convenience))
    ("abbrevlist.el"
        "list one abbrev table alphabetically ordered"
        (abbrev))
    ("add-log.el"
        "change log maintenance commands for Emacs"
        (tools))
    ("align.el"
        "align text to a specific column, by regexp"
        (convenience languages lisp))
    ("allout.el"
        "extensive outline mode for use alone and with other modes"
        (outlines wp languages))
    ("ansi-color.el"
        "translate ANSI escape sequences into faces"
        (comm processes terminals services))
    ("apropos.el"
        "apropos commands for users and programmers"
        (help))
    ("arc-mode.el"
        "simple editing of archives"
        (files archives msdog editing major-mode))
    ("array.el"
        "array editing commands for GNU Emacs"
        (extensions))
    ("autoarg.el"
        "make digit keys supply prefix args"
        (abbrev emulations))
    ("autoinsert.el"
        "automatic mode-dependent insertion of text into new files"
        (convenience))
    ("autorevert.el"
        "revert buffers when files on disk change"
        (convenience))
    ("avoid.el"
        "make mouse pointer stay out of the way of editing"
        (mouse))
    ("battery.el"
        "display battery status information"
        (hardware))
    ("bindings.el"
        "define standard key bindings and some variables"
        (internal))
    ("bookmark.el"
        "set bookmarks, maybe annotate them, jump to them later"
        (bookmarks placeholders annotations))
    ("bs.el"
        "menu for selecting and displaying buffers"
        (convenience))
    ("buff-menu.el"
        "buffer menu main function and support functions"
        (convenience))
    ("button.el"
        "clickable buttons"
        (extensions))
    ("calculator.el"
        "a [not so] simple calculator for Emacs"
        (tools convenience))
    ("case-table.el"
        "code to extend the character set and support case tables"
        (i18n))
    ("cdl.el"
        "Common Data Language (CDL) utility functions for GNU Emacs"
        (data))
    ("chistory.el"
        "list command history"
        (convenience))
    ("cmuscheme.el"
        "Scheme process in a buffer. Adapted from tea.el"
        (processes lisp))
    ("comint.el"
        "general command interpreter in a window stuff"
        (processes))
    ("compare-w.el"
        "compare text between windows for Emacs"
        (convenience files))
    ("complete.el"
        "partial completion mechanism plus other goodies"
        (abbrev convenience special thanks to hallvard furuseth for his many ideas and contributions.))
    ("completion.el"
        "dynamic word-completion code"
        (abbrev convenience))
    ("composite.el"
        "support character composition"
        (mule multilingual character composition))
    ("cus-dep.el"
        "find customization dependencies"
        (internal))
    ("cus-edit.el"
        "tools for customizing Emacs and Lisp packages"
        (help faces))
    ("cus-face.el"
        "customization support for faces"
        (help faces))
    ("cus-start.el"
        "define customization properties of builtins"
        (internal))
    ("cus-theme.el"
        "custom theme creation user interface"
        (help faces))
    ("custom.el"
        "tools for declaring and initializing options"
        (help faces))
    ("cvs-status.el"
        "major mode for browsing `cvs status' output"
        (pcl-cvs cvs status tree tools))
    ("dabbrev.el"
        "dynamic abbreviation package"
        (abbrev expand completion convenience))
    ("delim-col.el"
        "prettify all columns in a region or rectangle"
        (internal))
    ("delsel.el"
        "delete selection if you insert"
        (convenience emulations))
    ("descr-text.el"
        "describe text mode"
        (faces i18n unicode multilingual))
    ("desktop.el"
        "save partial status of Emacs when killed"
        (convenience))
    ("dframe.el"
        "dedicate frame support modes"
        (file tags tools))
    ("diff-mode.el"
        "a mode for viewing/editing context diffs"
        (convenience patch diff))
    ("diff.el"
        "run `diff' in compilation-mode"
        (unix tools))
    ("dired-aux.el"
        "less commonly used parts of dired"
        (files))
    ("dired-x.el"
        "extra Dired functionality"
        (dired extensions files))
    ("dired.el"
        "directory-browsing commands"
        (files))
    ("dirtrack.el"
        "Directory Tracking by watching the prompt"
        (processes))
    ("disp-table.el"
        "functions for dealing with char tables"
        (i18n))
    ("dnd.el"
        "drag and drop support."
        (window drag drop))
    ("doc-view.el"
        "View PDF/PostScript/DVI files in Emacs"
        (files pdf ps dvi))
    ("dos-fns.el"
        "MS-Dos specific functions"
        (internal))
    ("dos-vars.el"
        "MS-Dos specific user options"
        (internal))
    ("dos-w32.el"
        "Functions shared among MS-DOS and W32 (NT/95) platforms"
        (internal))
    ("double.el"
        "support for keyboard remapping with double clicking"
        (i18n))
    ("ebuff-menu.el"
        "electric-buffer-list mode"
        (convenience))
    ("echistory.el"
        "Electric Command History Mode"
        nil)
    ("ediff-diff.el"
        "diff-related utilities"
        nil)
    ("ediff-help.el"
        "Code related to the contents of Ediff help buffers"
        nil)
    ("ediff-hook.el"
        "setup for Ediff's menus and autoloads"
        nil)
    ("ediff-init.el"
        "Macros, variables, and defsubsts used by Ediff"
        nil)
    ("ediff-merg.el"
        "merging utilities"
        nil)
    ("ediff-mult.el"
        "support for multi-file/multi-buffer processing in Ediff"
        nil)
    ("ediff-ptch.el"
        "Ediff's  patch support"
        nil)
    ("ediff-util.el"
        "the core commands and utilities of ediff"
        nil)
    ("ediff-vers.el"
        "version control interface to Ediff"
        nil)
    ("ediff-wind.el"
        "window manipulation utilities"
        nil)
    ("ediff.el"
        "a comprehensive visual interface to diff & patch"
        (comparing merging patching tools unix))
    ("edmacro.el"
        "keyboard macro editor"
        (abbrev))
    ("ehelp.el"
        "bindings for electric-help mode"
        (help extensions))
    ("electric.el"
        "window maker and Command loop for `electric' modes"
        (extensions))
    ("elide-head.el"
        "hide headers in files"
        (outlines tools))
    ("emacs-lock.el"
        "prevents you from exiting Emacs if a buffer is locked"
        (extensions processes))
    ("emerge.el"
        "merge diffs under Emacs control"
        (unix tools))
    ("env.el"
        "functions to manipulate environment variables"
        (processes unix))
    ("epa-dired.el"
        "the EasyPG Assistant, dired extension"
        (pgp gnupg))
    ("epa-file.el"
        "the EasyPG Assistant, transparent file encryption"
        (pgp gnupg))
    ("epa-hook.el"
        "preloaded code to enable epa-file.el"
        (pgp gnupg))
    ("epa-mail.el"
        "the EasyPG Assistant, minor-mode for mail composer"
        (pgp gnupg mail message))
    ("epa.el"
        "the EasyPG Assistant"
        (pgp gnupg))
    ("epg-config.el"
        "configuration of the EasyPG Library"
        (pgp gnupg))
    ("epg.el"
        "the EasyPG Library"
        (pgp gnupg))
    ("expand.el"
        "make abbreviations more usable"
        (abbrev))
    ("ezimage.el"
        "Generalized Image management"
        (file tags tools))
    ("face-remap.el"
        "Functions for managing `face-remapping-alist'"
        (faces face remapping display user commands))
    ("facemenu.el"
        "create a face menu for interactively adding fonts to text"
        (faces))
    ("faces.el"
        "Lisp faces"
        (internal))
    ("ffap.el"
        "find file (or url) at point"
        (files hypermedia matching mouse convenience))
    ("filecache.el"
        "find files using a pre-loaded cache"
        (convenience))
    ("files-x.el"
        "extended file handling commands"
        (files))
    ("files.el"
        "file input and output commands for Emacs"
        nil)
    ("filesets.el"
        "handle group of files"
        (filesets convenience))
    ("find-cmd.el"
        "Build a valid find(1) command with sexps"
        nil)
    ("find-dired.el"
        "run a `find' command and dired the output"
        (unix))
    ("find-file.el"
        "find a file corresponding to this one given a pattern"
        (c matching tools))
    ("find-lisp.el"
        "emulation of find in Emacs Lisp"
        (unix))
    ("finder.el"
        "topic & keyword-based code finder"
        (help))
    ("flow-ctrl.el"
        "help for lusers on cu(1) or ttys with wired-in ^S/^Q flow control"
        (hardware))
    ("foldout.el"
        "folding extensions for outline-mode and outline-minor-mode"
        (folding outlines))
    ("follow.el"
        "synchronize windows showing the same buffer"
        (display window minor-mode convenience))
    ("font-core.el"
        "Core interface to font-lock"
        (languages faces))
    ("font-lock.el"
        "Electric font lock mode"
        (languages faces))
    ("font-setting.el"
        "Support dynamic font changes"
        (font system-font))
    ("format-spec.el"
        "functions for formatting arbitrary formatting strings"
        (tools))
    ("format.el"
        "read and save files in multiple formats"
        nil)
    ("forms-d2.el"
        "demo forms-mode"
        nil)
    ("forms-pass.el"
        "passwd file demo for forms-mode"
        nil)
    ("forms.el"
        "Forms mode: edit a file as a form to fill in"
        nil)
    ("frame.el"
        "multi-frame management independent of window systems"
        (internal))
    ("fringe.el"
        "fringe setup and control"
        (frames))
    ("generic-x.el"
        "A collection of generic modes"
        (generic comment font-lock))
    ("gs.el"
        "interface to Ghostscript"
        (internal))
    ("help-at-pt.el"
        "local help through the keyboard"
        (help))
    ("help-fns.el"
        "Complex help functions"
        (help internal))
    ("help-macro.el"
        "makes command line help such as help-for-help"
        nil)
    ("help-mode.el"
        "`help-mode' used by *Help* buffers"
        (help internal))
    ("help.el"
        "help commands for Emacs"
        (help internal))
    ("hex-util.el"
        "Functions to encode/decode hexadecimal string."
        (data))
    ("hexl.el"
        "edit a file in a hex dump format using the hexl filter"
        (data))
    ("hfy-cmap.el"
        "Fallback colour name -> rgb mapping for `htmlfontify'"
        (colour rgb))
    ("hi-lock.el"
        "minor mode for interactive automatic highlighting"
        (faces minor-mode matching display))
    ("hilit-chg.el"
        "minor mode displaying buffer changes with special face"
        (faces))
    ("hippie-exp.el"
        "expand text trying various ways to find its expansion"
        (abbrev convenience))
    ("hl-line.el"
        "highlight the current line"
        (faces frames emulation))
    ("htmlfontify.el"
        "htmlise a buffer/source tree with optional hyperlinks"
        (html hypermedia markup etags))
    ("ibuf-ext.el"
        "extensions for ibuffer"
        (buffer convenience))
    ("ibuf-macs.el"
        "macros for ibuffer"
        (buffer convenience))
    ("ibuffer.el"
        "operate on buffers like dired"
        (buffer convenience))
    ("icomplete.el"
        "minibuffer completion incremental feedback"
        (help abbrev))
    ("ido.el"
        "interactively do things with buffers and files."
        (extensions convenience))
    ("ielm.el"
        "interaction mode for Emacs Lisp"
        (lisp))
    ("iimage.el"
        "Inline image minor mode."
        (multimedia))
    ("image-dired.el"
        "use dired to browse and manipulate your images"
        (multimedia))
    ("image-file.el"
        "support for visiting image files"
        (multimedia))
    ("image-mode.el"
        "support for visiting image files"
        (multimedia))
    ("image.el"
        "image API"
        (multimedia))
    ("imenu.el"
        "framework for mode-specific buffer indexes"
        (tools convenience))
    ("indent.el"
        "indentation commands for Emacs"
        nil)
    ("info-look.el"
        "major-mode-sensitive Info index lookup facility"
        (help languages))
    ("info-xref.el"
        "check external references in an Info document"
        (docs))
    ("info.el"
        "info package for Emacs"
        (help))
    ("informat.el"
        "info support functions package for Emacs"
        (help))
    ("isearch.el"
        "incremental search minor mode"
        (matching))
    ("isearchb.el"
        "a marriage between iswitchb and isearch"
        (lisp))
    ("iswitchb.el"
        "switch between buffers using substrings"
        (completion convenience))
    ("jit-lock.el"
        "just-in-time fontification"
        (faces files))
    ("jka-cmpr-hook.el"
        "preloaded code to enable jka-compr.el"
        (data))
    ("jka-compr.el"
        "reading/writing/loading compressed files"
        (data))
    ("json.el"
        "JavaScript Object Notation parser / generator"
        (convenience))
    ("kermit.el"
        "additions to shell mode for use with kermit"
        (comm))
    ("kmacro.el"
        "enhanced keyboard macros"
        (keyboard convenience))
    ("ledit.el"
        "Emacs side of ledit interface"
        (languages))
    ("linum.el"
        "display line numbers in the left margin"
        (convenience))
    ("loadhist.el"
        "lisp functions for working with feature groups"
        (internal))
    ("loadup.el"
        "load up standardly loaded Lisp files for Emacs"
        (internal))
    ("locate.el"
        "interface to the locate command"
        (unix files))
    ("log-edit.el"
        "Major mode for editing CVS commit messages"
        (pcl-cvs cvs commit log))
    ("log-view.el"
        "Major mode for browsing RCS/CVS/SCCS log output"
        (rcs sccs cvs log version-control tools))
    ("longlines.el"
        "automatically wrap long lines"
        (convenience wp))
    ("lpr.el"
        "print Emacs buffer on line printer"
        (unix))
    ("ls-lisp.el"
        "emulate insert-directory completely in Emacs Lisp"
        (unix dired))
    ("macros.el"
        "non-primitive commands for keyboard macros"
        (abbrev))
    ("makesum.el"
        "generate key binding summary for Emacs"
        (help))
    ("man.el"
        "browse UNIX manual pages"
        (help))
    ("master.el"
        "make a buffer the master over another buffer"
        (comm))
    ("mb-depth.el"
        "Indicate minibuffer-depth in prompt"
        (convenience))
    ("md4.el"
        "MD4 Message Digest Algorithm."
        (md4))
    ("menu-bar.el"
        "define a default menu bar"
        (internal mouse))
    ("midnight.el"
        "run something every midnight, e.g., kill old buffers"
        (utilities))
    ("minibuf-eldef.el"
        "Only show defaults in prompts when applicable"
        (convenience))
    ("minibuffer.el"
        "Minibuffer completion functions"
        nil)
    ("misc.el"
        "some nonstandard basic editing commands for Emacs"
        (convenience))
    ("misearch.el"
        "isearch extensions for multi-buffer search"
        (matching))
    ("mouse-copy.el"
        "one-click text copy and move"
        (mouse))
    ("mouse-drag.el"
        "use mouse-2 to do a new style of scrolling"
        (mouse))
    ("mouse-sel.el"
        "multi-click selection support for Emacs 19"
        (mouse))
    ("mouse.el"
        "window system-independent mouse support"
        (hardware mouse))
    ("mpc.el"
        "A client for the Music Player Daemon"
        (multimedia))
    ("msb.el"
        "customizable buffer-selection with multiple menus"
        (mouse buffer menu))
    ("mwheel.el"
        "Wheel mouse support"
        (mouse))
    ("newcomment.el"
        "(un)comment regions of buffers"
        (comment uncomment))
    ("novice.el"
        "handling of disabled commands (\"novice mode\") for Emacs"
        (internal help))
    ("outline.el"
        "outline mode commands for Emacs"
        (outlines))
    ("paren.el"
        "highlight matching paren"
        (languages faces))
    ("password-cache.el"
        "Read passwords, possibly using a password cache."
        (password cache passphrase key))
    ("patcomp.el"
        "used by patch files to update Emacs releases"
        nil)
    ("paths.el"
        "define pathnames for use by various Emacs commands"
        (internal))
    ("pcmpl-cvs.el"
        "functions for dealing with cvs completions"
        nil)
    ("pcmpl-gnu.el"
        "completions for GNU project tools"
        nil)
    ("pcmpl-linux.el"
        "functions for dealing with GNU/Linux completions"
        nil)
    ("pcmpl-rpm.el"
        "functions for dealing with rpm completions"
        nil)
    ("pcmpl-unix.el"
        "standard UNIX completions"
        nil)
    ("pcomplete.el"
        "programmable completion"
        (processes abbrev))
    ("pcvs-defs.el"
        "variable definitions for PCL-CVS"
        (pcl-cvs))
    ("pcvs-info.el"
        "internal representation of a fileinfo entry"
        (pcl-cvs))
    ("pcvs-parse.el"
        "the CVS output parser"
        (pcl-cvs))
    ("pcvs-util.el"
        "utility functions for PCL-CVS"
        (pcl-cvs))
    ("pcvs.el"
        "a front-end to CVS"
        (cvs version control release management))
    ("pgg-def.el"
        "functions/macros for defining PGG functions"
        (pgp openpgp gnupg))
    ("pgg-gpg.el"
        "GnuPG support for PGG."
        (pgp openpgp gnupg))
    ("pgg-parse.el"
        "OpenPGP packet parsing"
        (pgp openpgp gnupg))
    ("pgg-pgp.el"
        "PGP 2.* and 6.* support for PGG."
        (pgp openpgp))
    ("pgg-pgp5.el"
        "PGP 5.* support for PGG."
        (pgp openpgp))
    ("pgg.el"
        "glue for the various PGP implementations."
        (pgp))
    ("printing.el"
        "printing utilities"
        (wp print postscript))
    ("proced.el"
        "operate on system processes like dired"
        (processes unix))
    ("ps-bdf.el"
        "BDF font file handler for ps-print"
        (wp bdf font postscript))
    ("ps-def.el"
        "XEmacs and Emacs definitions for ps-print"
        (wp print postscript))
    ("ps-mule.el"
        "provide multi-byte character facility to ps-print"
        (wp print postscript multibyte mule))
    ("ps-print.el"
        "print text from the buffer as PostScript"
        (wp print postscript))
    ("ps-samp.el"
        "ps-print sample setup code"
        (wp print postscript))
    ("recentf.el"
        "setup a menu of recently opened files"
        (files))
    ("rect.el"
        "rectangle functions for GNU Emacs"
        (internal))
    ("register.el"
        "register commands for Emacs"
        (internal))
    ("repeat.el"
        "convenient way to repeat the previous command"
        (convenience vi repeat))
    ("replace.el"
        "replace commands for Emacs"
        nil)
    ("reposition.el"
        "center a Lisp function or comment on the screen"
        nil)
    ("reveal.el"
        "Automatically reveal hidden text at point"
        (outlines))
    ("rfn-eshadow.el"
        "Highlight `shadowed' part of read-file-name input text"
        (convenience minibuffer))
    ("rot13.el"
        "display a buffer in ROT13"
        nil)
    ("ruler-mode.el"
        "display a ruler in the header line"
        (convenience))
    ("s-region.el"
        "set region using shift key"
        (terminals))
    ("savehist.el"
        "Save minibuffer history."
        (minibuffer))
    ("saveplace.el"
        "automatically save place in files"
        (bookmarks placeholders))
    ("sb-image.el"
        "Image management for speedbar"
        (file tags tools))
    ("scroll-all.el"
        "scroll all buffers together minor mode"
        (scroll crisp brief lock))
    ("scroll-bar.el"
        "window system-independent scroll bar support"
        (hardware))
    ("scroll-lock.el"
        "Scroll lock scrolling."
        nil)
    ("select.el"
        "lisp portion of standard selection support"
        (internal))
    ("server.el"
        "Lisp code for GNU Emacs running as server process"
        (processes))
    ("ses.el"
        "Simple Emacs Spreadsheet"
        (spreadsheet))
    ("sha1.el"
        "SHA1 Secure Hash Algorithm in Emacs-Lisp"
        (sha1 fips 180-1))
    ("shadowfile.el"
        "automatic file copying"
        (comm files))
    ("shell.el"
        "specialized comint.el for running the shell"
        (processes))
    ("simple.el"
        "basic editing commands for Emacs"
        (internal))
    ("skeleton.el"
        "Lisp language extension for writing statement skeletons"
        (extensions abbrev languages tools))
    ("smerge-mode.el"
        "Minor mode to resolve diff3 conflicts"
        (tools revision-control merge diff3 cvs conflict))
    ("sort.el"
        "commands to sort text in an Emacs buffer"
        (unix))
    ("soundex.el"
        "implement Soundex algorithm"
        (matching))
    ("speedbar.el"
        "quick access to files and tags in a frame"
        (file tags tools))
    ("startup.el"
        "process Emacs shell arguments"
        (internal))
    ("strokes.el"
        "control Emacs through mouse strokes"
        (lisp mouse extensions))
    ("subr.el"
        "basic lisp subroutines for Emacs"
        (internal))
    ("t-mouse.el"
        "mouse support within the text terminal"
        (mouse gpm linux))
    ("tabify.el"
        "tab conversion commands for Emacs"
        nil)
    ("talk.el"
        "allow several users to talk to each other through Emacs"
        (comm frames))
    ("tar-mode.el"
        "simple editing of tar files from GNU emacs"
        (unix))
    ("tempo.el"
        "Flexible template insertion"
        (extensions languages tools))
    ("term.el"
        "general command interpreter in a window stuff"
        (processes))
    ("terminal.el"
        "terminal emulator for GNU Emacs"
        (comm terminals))
    ("thingatpt.el"
        "get the `thing' at point"
        (extensions matching mouse))
    ("thumbs.el"
        "Thumbnails previewer for images files"
        (multimedia))
    ("time-stamp.el"
        "Maintain last change time stamps in files edited by Emacs"
        (tools))
    ("time.el"
        "display time, load and mail indicator in mode line of Emacs"
        nil)
    ("timezone.el"
        "time zone package for GNU Emacs"
        (news))
    ("tmm.el"
        "text mode access to menu-bar"
        (convenience))
    ("tool-bar.el"
        "setting up the tool bar"
        (mouse frames))
    ("tooltip.el"
        "show tooltip windows"
        (help c mouse tools))
    ("tree-widget.el"
        "Tree widget"
        (extensions))
    ("tutorial.el"
        "tutorial for Emacs"
        (help internal))
    ("type-break.el"
        "encourage rests from typing at appropriate intervals"
        (extensions timers))
    ("uniquify.el"
        "unique buffer names dependent on file name"
        (files))
    ("userlock.el"
        "handle file access contention between multiple users"
        (internal))
    ("vc-annotate.el"
        "VC Annotate Support"
        (tools))
    ("vc-arch.el"
        "VC backend for the Arch version-control system"
        nil)
    ("vc-bzr.el"
        "VC backend for the bzr revision control system"
        (tools))
    ("vc-cvs.el"
        "non-resident support for CVS version-control"
        nil)
    ("vc-dav.el"
        "vc.el support for WebDAV"
        (url vc))
    ("vc-dir.el"
        "Directory status display under VC"
        (tools))
    ("vc-dispatcher.el"
        "generic command-dispatcher facility."
        (tools))
    ("vc-git.el"
        "VC backend for the git version control system"
        (tools))
    ("vc-hg.el"
        "VC backend for the mercurial version control system"
        (tools))
    ("vc-hooks.el"
        "resident support for version-control"
        nil)
    ("vc-mtn.el"
        "VC backend for Monotone"
        nil)
    ("vc-rcs.el"
        "support for RCS version-control"
        nil)
    ("vc-sccs.el"
        "support for SCCS version-control"
        nil)
    ("vc-svn.el"
        "non-resident support for Subversion version-control"
        nil)
    ("vc.el"
        "drive a version-control system from within Emacs"
        (tools))
    ("vcursor.el"
        "manipulate an alternative (\"virtual\") cursor"
        (virtual cursor convenience))
    ("version.el"
        "record version number of Emacs"
        (internal))
    ("view.el"
        "peruse file or buffer without editing"
        (files))
    ("vt-control.el"
        "Common VTxxx control functions"
        (terminals))
    ("vt100-led.el"
        "functions for LED control on VT-100 terminals & clones"
        (hardware))
    ("w32-fns.el"
        "Lisp routines for Windows NT"
        (internal))
    ("w32-vars.el"
        "MS-Windows specific user options"
        (internal))
    ("wdired.el"
        "Rename files editing their names in dired buffers"
        (dired environment files renaming))
    ("whitespace.el"
        "minor mode to visualize TAB, (HARD) SPACE, NEWLINE"
        (data wp))
    ("wid-browse.el"
        "functions for browsing widgets"
        (extensions))
    ("wid-edit.el"
        "Functions for creating and using widgets"
        (extensions))
    ("widget.el"
        "a library of user interface components"
        (help extensions faces hypermedia))
    ("windmove.el"
        "directional window-selection routines"
        (window movement convenience))
    ("window.el"
        "GNU Emacs window commands aside from those written in C"
        (internal))
    ("winner.el"
        "Restore old window configurations"
        (convenience frames))
    ("woman.el"
        "browse UN*X manual pages `wo (without) man'"
        (help unix))
    ("x-dnd.el"
        "drag and drop support for X."
        (window drag drop))
    ("xml.el"
        "XML parser"
        (xml data))
    ("xt-mouse.el"
        "support the mouse when emacs run in an xterm"
        (mouse terminals))
    ("calc-aent.el"
        "algebraic entry functions for Calc"
        nil)
    ("calc-alg.el"
        "algebraic functions for Calc"
        nil)
    ("calc-arith.el"
        "arithmetic functions for Calc"
        nil)
    ("calc-bin.el"
        "binary functions for Calc"
        nil)
    ("calc-comb.el"
        "combinatoric functions for Calc"
        nil)
    ("calc-cplx.el"
        "Complex number functions for Calc"
        nil)
    ("calc-embed.el"
        "embed Calc in a buffer"
        nil)
    ("calc-ext.el"
        "various extension functions for Calc"
        nil)
    ("calc-fin.el"
        "financial functions for Calc"
        nil)
    ("calc-forms.el"
        "data format conversion functions for Calc"
        nil)
    ("calc-frac.el"
        "fraction functions for Calc"
        nil)
    ("calc-funcs.el"
        "well-known functions for Calc"
        nil)
    ("calc-graph.el"
        "graph output functions for Calc"
        nil)
    ("calc-help.el"
        "help display functions for Calc,"
        nil)
    ("calc-incom.el"
        "complex data type input functions for Calc"
        nil)
    ("calc-keypd.el"
        "mouse-capable keypad input for Calc"
        nil)
    ("calc-lang.el"
        "calc language functions"
        nil)
    ("calc-macs.el"
        "important macros for Calc"
        nil)
    ("calc-map.el"
        "higher-order functions for Calc"
        nil)
    ("calc-math.el"
        "mathematical functions for Calc"
        nil)
    ("calc-menu.el"
        "a menu for Calc"
        nil)
    ("calc-misc.el"
        "miscellaneous functions for Calc"
        nil)
    ("calc-mode.el"
        "calculator modes for Calc"
        nil)
    ("calc-mtx.el"
        "matrix functions for Calc"
        nil)
    ("calc-nlfit.el"
        "nonlinear curve fitting for Calc"
        nil)
    ("calc-poly.el"
        "polynomial functions for Calc"
        nil)
    ("calc-prog.el"
        "user programmability functions for Calc"
        nil)
    ("calc-rewr.el"
        "rewriting functions for Calc"
        nil)
    ("calc-rules.el"
        "rules for simplifying algebraic expressions in Calc"
        nil)
    ("calc-sel.el"
        "data selection functions for Calc"
        nil)
    ("calc-stat.el"
        "statistical functions for Calc"
        nil)
    ("calc-store.el"
        "value storage functions for Calc"
        nil)
    ("calc-stuff.el"
        "miscellaneous functions for Calc"
        nil)
    ("calc-trail.el"
        "functions for manipulating the Calc \"trail\""
        nil)
    ("calc-undo.el"
        "undo functions for Calc"
        nil)
    ("calc-units.el"
        "unit conversion functions for Calc"
        nil)
    ("calc-vec.el"
        "vector functions for Calc"
        nil)
    ("calc-yank.el"
        "kill-ring functionality for Calc"
        nil)
    ("calc.el"
        "the GNU Emacs calculator"
        (convenience extensions))
    ("calcalg2.el"
        "more algebraic functions for Calc"
        nil)
    ("calcalg3.el"
        "more algebraic functions for Calc"
        nil)
    ("calccomp.el"
        "composition functions for Calc"
        nil)
    ("calcsel2.el"
        "selection functions for Calc"
        nil)
    ("appt.el"
        "appointment notification functions"
        (calendar))
    ("cal-bahai.el"
        "calendar functions for the Bahá'í calendar."
        (calendar))
    ("cal-china.el"
        "calendar functions for the Chinese calendar"
        (calendar))
    ("cal-coptic.el"
        "calendar functions for the Coptic/Ethiopic calendars"
        (calendar))
    ("cal-dst.el"
        "calendar functions for daylight saving rules"
        (calendar))
    ("cal-french.el"
        "calendar functions for the French Revolutionary calendar"
        (calendar))
    ("cal-hebrew.el"
        "calendar functions for the Hebrew calendar"
        (calendar))
    ("cal-html.el"
        "functions for printing HTML calendars"
        (calendar))
    ("cal-islam.el"
        "calendar functions for the Islamic calendar"
        (calendar))
    ("cal-iso.el"
        "calendar functions for the ISO calendar"
        (calendar))
    ("cal-julian.el"
        "calendar functions for the Julian calendar"
        (calendar))
    ("cal-mayan.el"
        "calendar functions for the Mayan calendars"
        (calendar))
    ("cal-menu.el"
        "calendar functions for menu bar and popup menu support"
        (calendar))
    ("cal-move.el"
        "calendar functions for movement in the calendar"
        (calendar))
    ("cal-persia.el"
        "calendar functions for the Persian calendar"
        (calendar))
    ("cal-tex.el"
        "calendar functions for printing calendars with LaTeX"
        (calendar))
    ("cal-x.el"
        "calendar windows in dedicated frames"
        (calendar))
    ("calendar.el"
        "calendar functions"
        (calendar))
    ("diary-lib.el"
        "diary functions"
        (calendar))
    ("holidays.el"
        "holiday functions for the calendar package"
        (holidays calendar))
    ("icalendar.el"
        "iCalendar implementation"
        (calendar))
    ("lunar.el"
        "calendar functions for phases of the moon"
        (calendar))
    ("parse-time.el"
        "parsing time strings"
        (util))
    ("solar.el"
        "calendar functions for solar events"
        (calendar))
    ("time-date.el"
        "Date and time handling functions"
        (mail news util))
    ("timeclock.el"
        "mode for keeping track of how much you work"
        (calendar data))
    ("todo-mode.el"
        "major mode for editing TODO list files"
        (calendar todo))
    ("advice.el"
        "an overloading mechanism for Emacs Lisp functions"
        (extensions lisp tools))
    ("assoc.el"
        "insert/delete/sort functions on association lists"
        (extensions))
    ("authors.el"
        "utility for maintaining Emacs' AUTHORS file"
        (maint))
    ("autoload.el"
        "maintain autoloads in loaddefs.el"
        (maint))
    ("avl-tree.el"
        "balanced binary trees, AVL-trees"
        (extensions data structures))
    ("backquote.el"
        "implement the ` Lisp construct"
        (extensions internal))
    ("benchmark.el"
        "support for benchmarking code"
        (lisp extensions))
    ("bindat.el"
        "binary data structure packing and unpacking."
        (comm data processes))
    ("byte-opt.el"
        "the optimization passes of the emacs-lisp byte compiler"
        (internal))
    ("byte-run.el"
        "byte-compiler support for inlining"
        (internal))
    ("bytecomp.el"
        "compilation of Lisp code into byte code"
        (lisp))
    ("chart.el"
        "Draw charts (bar charts, etc)"
        (oo chart graph))
    ("check-declare.el"
        "Check declare-function statements"
        (lisp tools maint))
    ("checkdoc.el"
        "check documentation strings for style requirements"
        (docs maint lisp))
    ("cl-compat.el"
        "Common Lisp extensions for GNU Emacs Lisp (compatibility)"
        (extensions))
    ("cl-extra.el"
        "Common Lisp features, part 2"
        (extensions))
    ("cl-indent.el"
        "enhanced lisp-indent mode"
        (lisp tools))
    ("cl-macs.el"
        "Common Lisp macros"
        (extensions))
    ("cl-seq.el"
        "Common Lisp features, part 3"
        (extensions))
    ("cl-specs.el"
        "Edebug specs for cl.el"
        (lisp tools maint))
    ("cl.el"
        "Common Lisp extensions for Emacs"
        (extensions))
    ("copyright.el"
        "update the copyright notice in current buffer"
        (maint tools))
    ("crm.el"
        "read multiple strings with completion"
        (completion minibuffer multiple elements))
    ("cust-print.el"
        "handles print-level and print-circle"
        (extensions))
    ("debug.el"
        "debuggers and related commands for Emacs"
        (lisp tools maint))
    ("derived.el"
        "allow inheritance of major modes"
        (extensions))
    ("disass.el"
        "disassembler for compiled Emacs Lisp code"
        (internal))
    ("easy-mmode.el"
        "easy definition for major and minor modes"
        (extensions lisp))
    ("easymenu.el"
        "support the easymenu interface for defining a menu"
        (emulations))
    ("edebug.el"
        "a source-level debugger for Emacs Lisp"
        (lisp tools maint))
    ("eieio-base.el"
        "Base classes for EIEIO."
        (oo lisp))
    ("eieio-comp.el"
        "eieio routines to help with byte compilation"
        (oop lisp tools))
    ("eieio-custom.el"
        "eieio object customization"
        (oo lisp))
    ("eieio-datadebug.el"
        "EIEIO extensions to the data debugger."
        (oo lisp))
    ("eieio-opt.el"
        "eieio optional functions (debug, printing, speedbar)"
        (oo lisp))
    ("eieio-speedbar.el"
        "Classes for managing speedbar displays."
        (oo tools))
    ("eieio.el"
        "Enhanced Implementation of Emacs Interpreted Objects"
        (oo lisp))
    ("eldoc.el"
        "show function arglist or variable docstring in echo area"
        (extensions))
    ("elint.el"
        "Lint Emacs Lisp"
        (lisp))
    ("elp.el"
        "Emacs Lisp Profiler"
        (debugging lisp tools))
    ("ewoc.el"
        "utility to maintain a view of a list of objects in a buffer"
        (extensions lisp))
    ("find-func.el"
        "find the definition of the Emacs Lisp function near point"
        (emacs-lisp functions variables))
    ("find-gc.el"
        "detect functions that call the garbage collector"
        nil)
    ("float-sup.el"
        "define some constants useful for floating point numbers."
        (internal))
    ("generic.el"
        "defining simple major modes with comment and font-lock"
        (generic comment font-lock))
    ("gulp.el"
        "ask for updates for Lisp packages"
        (maint))
    ("helper.el"
        "utility help package supporting help in electric modes"
        (help))
    ("lisp-mnt.el"
        "utility functions for Emacs Lisp maintainers"
        (docs))
    ("lisp-mode.el"
        "Lisp mode, and its idiosyncratic commands"
        (lisp languages))
    ("lisp.el"
        "Lisp editing commands for Emacs"
        (lisp languages))
    ("lmenu.el"
        "emulate Lucid's menubar support"
        (emulations obsolete))
    ("macroexp.el"
        "Additional macro-expansion support"
        (lisp compiler macros))
    ("map-ynp.el"
        "general-purpose boolean question-asker"
        (lisp extensions))
    ("pp.el"
        "pretty printer for Emacs Lisp"
        (lisp))
    ("re-builder.el"
        "building Regexps with visual feedback"
        (matching lisp tools))
    ("regexp-opt.el"
        "generate efficient regexps to match strings"
        (strings regexps extensions))
    ("regi.el"
        "REGular expression Interpreting engine"
        (extensions matching))
    ("ring.el"
        "handle rings of items"
        (extensions))
    ("rx.el"
        "sexp notation for regular expressions"
        (strings regexps extensions))
    ("shadow.el"
        "locate Emacs Lisp file shadowings"
        (lisp))
    ("sregex.el"
        "symbolic regular expressions"
        (extensions))
    ("syntax.el"
        "helper functions to find syntactic context"
        (internal))
    ("tcover-ses.el"
        "Example use of `testcover' to test \"SES\""
        (spreadsheet lisp utility))
    ("tcover-unsafep.el"
        "Use testcover to test unsafep's code coverage"
        (safety lisp utility))
    ("testcover.el"
        "Visual code-coverage tool"
        (lisp utility))
    ("timer.el"
        "run a function with args at some time in future"
        nil)
    ("tq.el"
        "utility to maintain a transaction queue"
        (extensions))
    ("trace.el"
        "tracing facility for Emacs Lisp functions"
        (tools lisp))
    ("unsafep.el"
        "Determine whether a Lisp form is safe to evaluate"
        (safety lisp utility))
    ("warnings.el"
        "log and display warnings"
        (internal))
    ("crisp.el"
        "CRiSP/Brief Emacs emulator"
        (emulations brief crisp))
    ("cua-base.el"
        "emulate CUA key bindings"
        (keyboard emulation convenience cua))
    ("cua-gmrk.el"
        "CUA unified global mark support"
        (keyboard emulations convenience cua mark))
    ("cua-rect.el"
        "CUA unified rectangle support"
        (keyboard emulations convenience cua))
    ("edt-lk201.el"
        "enhanced EDT keypad mode emulation for LK-201 keyboards"
        (emulations))
    ("edt-mapper.el"
        "create an EDT LK-201 map file for X-Windows Emacs"
        (emulations))
    ("edt-pc.el"
        "enhanced EDT keypad mode emulation for PC 101 keyboards"
        (emulations))
    ("edt-vt100.el"
        "enhanced EDT keypad mode emulation for VT series terminals"
        (emulations))
    ("edt.el"
        "enhanced EDT keypad mode emulation for GNU Emacs 19"
        (emulations))
    ("keypad.el"
        "simplified keypad bindings"
        (keyboard convenience))
    ("pc-mode.el"
        "emulate certain key bindings used on PCs"
        (emulations))
    ("pc-select.el"
        "emulate mark, cut, copy and paste from Motif"
        (convenience emulation))
    ("tpu-edt.el"
        "Emacs emulating TPU emulating EDT"
        (emulations))
    ("tpu-extras.el"
        "scroll margins and free cursor mode for TPU-edt"
        (emulations))
    ("tpu-mapper.el"
        "create a TPU-edt X-windows keymap file"
        (emulations))
    ("vi.el"
        "major mode for emulating \"vi\" editor under GNU Emacs"
        (emulations))
    ("vip.el"
        "a VI Package for GNU Emacs"
        (emulations))
    ("viper-cmd.el"
        "Vi command support for Viper"
        nil)
    ("viper-ex.el"
        "functions implementing the Ex commands for Viper"
        nil)
    ("viper-init.el"
        "some common definitions for Viper"
        nil)
    ("viper-keym.el"
        "Viper keymaps"
        nil)
    ("viper-macs.el"
        "functions implementing keyboard macros for Viper"
        nil)
    ("viper-mous.el"
        "mouse support for Viper"
        nil)
    ("viper-util.el"
        "Utilities used by viper.el"
        nil)
    ("viper.el"
        "A full-featured Vi emulator for GNU Emacs and XEmacs,"
        (emulations))
    ("ws-mode.el"
        "WordStar emulation mode for GNU Emacs"
        (emulations))
    ("erc-autoaway.el"
        "Provides autoaway for ERC"
        nil)
    ("erc-backend.el"
        "Backend network communication for ERC"
        (irc chat client internet))
    ("erc-button.el"
        "A way of buttonizing certain things in ERC buffers"
        (irc button url regexp))
    ("erc-capab.el"
        "support for dancer-ircd and hyperion's CAPAB"
        nil)
    ("erc-compat.el"
        "ERC compatibility code for XEmacs"
        nil)
    ("erc-dcc.el"
        "CTCP DCC module for ERC"
        (comm processes))
    ("erc-ezbounce.el"
        "Handle EZBounce bouncer commands"
        (comm))
    ("erc-fill.el"
        "Filling IRC messages in various ways"
        nil)
    ("erc-goodies.el"
        "Collection of ERC modules"
        nil)
    ("erc-hecomplete.el"
        "Provides Nick name completion for ERC"
        nil)
    ("erc-ibuffer.el"
        "ibuffer integration with ERC"
        (comm))
    ("erc-identd.el"
        "RFC1413 (identd authentication protocol) server"
        (comm processes))
    ("erc-imenu.el"
        "Imenu support for ERC"
        (comm))
    ("erc-join.el"
        "autojoin channels on connect and reconnects"
        (irc))
    ("erc-lang.el"
        "provide the LANG command to ERC"
        (comm languages processes))
    ("erc-list.el"
        "/list support for ERC"
        (comm))
    ("erc-log.el"
        "Logging facilities for ERC."
        (irc chat client internet logging))
    ("erc-match.el"
        "Highlight messages matching certain regexps"
        (comm faces))
    ("erc-menu.el"
        "Menu-bar definitions for ERC"
        (comm processes menu))
    ("erc-netsplit.el"
        "Reduce JOIN/QUIT messages on netsplits"
        (comm))
    ("erc-networks.el"
        "IRC networks"
        (comm))
    ("erc-notify.el"
        "Online status change notification"
        (comm))
    ("erc-page.el"
        nil
        nil)
    ("erc-pcomplete.el"
        "Provides programmable completion for ERC"
        (comm convenience))
    ("erc-replace.el"
        "wash and massage messages inserted into the buffer"
        (irc client internet))
    ("erc-ring.el"
        "Command history handling for erc using ring.el"
        (comm))
    ("erc-services.el"
        "Identify to NickServ"
        nil)
    ("erc-sound.el"
        "CTCP SOUND support for ERC"
        nil)
    ("erc-speedbar.el"
        "Speedbar support for ERC"
        nil)
    ("erc-spelling.el"
        "use flyspell in ERC"
        (irc))
    ("erc-stamp.el"
        "Timestamping for ERC messages"
        (comm processes timestamp))
    ("erc-track.el"
        "Track modified channel buffers"
        (comm faces))
    ("erc-truncate.el"
        "Functions for truncating ERC buffers"
        (irc chat client internet logging))
    ("erc-xdcc.el"
        "XDCC file-server support for ERC"
        (comm processes))
    ("erc.el"
        "An Emacs Internet Relay Chat client"
        (irc chat client internet))
    ("em-alias.el"
        "creation and management of command aliases"
        nil)
    ("em-banner.el"
        "sample module that displays a login banner"
        nil)
    ("em-basic.el"
        "basic shell builtin commands"
        nil)
    ("em-cmpl.el"
        "completion using the TAB key"
        nil)
    ("em-dirs.el"
        "directory navigation commands"
        nil)
    ("em-glob.el"
        "extended file name globbing"
        nil)
    ("em-hist.el"
        "history list management"
        nil)
    ("em-ls.el"
        "implementation of ls in Lisp"
        nil)
    ("em-pred.el"
        "argument predicates and modifiers (ala zsh)"
        nil)
    ("em-prompt.el"
        "command prompts"
        nil)
    ("em-rebind.el"
        "rebind keys when point is at current input"
        nil)
    ("em-script.el"
        "Eshell script files"
        nil)
    ("em-smart.el"
        "smart display of output"
        nil)
    ("em-term.el"
        "running visual commands"
        nil)
    ("em-unix.el"
        "UNIX command aliases"
        nil)
    ("em-xtra.el"
        "extra alias functions"
        nil)
    ("esh-arg.el"
        "argument processing"
        nil)
    ("esh-cmd.el"
        "command invocation"
        nil)
    ("esh-ext.el"
        "commands external to Eshell"
        nil)
    ("esh-io.el"
        "I/O management"
        nil)
    ("esh-mode.el"
        "user interface"
        nil)
    ("esh-module.el"
        "Eshell modules"
        (processes))
    ("esh-opt.el"
        "command options processing"
        nil)
    ("esh-proc.el"
        "process management"
        nil)
    ("esh-test.el"
        "Eshell test suite"
        nil)
    ("esh-util.el"
        "general utilities"
        nil)
    ("esh-var.el"
        "handling of variables"
        nil)
    ("eshell.el"
        "the Emacs command shell"
        (processes))
    ("auth-source.el"
        "authentication sources for Gnus and Emacs"
        (news))
    ("canlock.el"
        "functions for Cancel-Lock feature"
        (news cancel-lock hmac sha1 rfc2104))
    ("compface.el"
        "functions for converting X-Face headers"
        (news))
    ("deuglify.el"
        "deuglify broken Outlook (Express) articles"
        (mail news))
    ("earcon.el"
        "Sound effects for messages"
        nil)
    ("ecomplete.el"
        "electric completion of addresses and the like"
        (mail))
    ("flow-fill.el"
        "interpret RFC2646 \"flowed\" text"
        (mail))
    ("gmm-utils.el"
        "Utility functions for Gnus, Message and MML"
        (news))
    ("gnus-agent.el"
        "unplugged support for Gnus"
        nil)
    ("gnus-art.el"
        "article mode commands for Gnus"
        (news))
    ("gnus-async.el"
        "asynchronous support for Gnus"
        (news))
    ("gnus-audio.el"
        "Sound effects for Gnus"
        (news mail multimedia))
    ("gnus-bcklg.el"
        "backlog functions for Gnus"
        (news))
    ("gnus-bookmark.el"
        "Bookmarks in Gnus"
        (news))
    ("gnus-cache.el"
        "cache interface for Gnus"
        (news))
    ("gnus-cite.el"
        "parse citations in articles for Gnus"
        nil)
    ("gnus-cus.el"
        "customization commands for Gnus"
        (news))
    ("gnus-delay.el"
        "Delayed posting of articles"
        (mail news extensions))
    ("gnus-demon.el"
        "daemonic Gnus behavior"
        (news))
    ("gnus-diary.el"
        "Wrapper around the NNDiary Gnus back end"
        (calendar mail news))
    ("gnus-dired.el"
        "utility functions where gnus and dired meet"
        (mail news extensions))
    ("gnus-draft.el"
        "draft message support for Gnus"
        (news))
    ("gnus-dup.el"
        "suppression of duplicate articles in Gnus"
        (news))
    ("gnus-eform.el"
        "a mode for editing forms for Gnus"
        (news))
    ("gnus-ems.el"
        "functions for making Gnus work under different Emacsen"
        (news))
    ("gnus-fun.el"
        "various frivolous extension functions to Gnus"
        (news))
    ("gnus-group.el"
        "group mode commands for Gnus"
        (news))
    ("gnus-int.el"
        "backend interface functions for Gnus"
        (news))
    ("gnus-kill.el"
        "kill commands for Gnus"
        (news))
    ("gnus-logic.el"
        "advanced scoring code for Gnus"
        (news))
    ("gnus-mh.el"
        "mh-e interface for Gnus"
        (news))
    ("gnus-ml.el"
        "Mailing list minor mode for Gnus"
        (news mail))
    ("gnus-mlspl.el"
        "a group params-based mail splitting mechanism"
        (news mail))
    ("gnus-move.el"
        "commands for moving Gnus from one server to another"
        (news))
    ("gnus-msg.el"
        "mail and post interface for Gnus"
        (news))
    ("gnus-nocem.el"
        "NoCeM pseudo-cancellation treatment"
        (news))
    ("gnus-picon.el"
        "displaying pretty icons in Gnus"
        (news xpm annotation glyph faces))
    ("gnus-range.el"
        "range and sequence functions for Gnus"
        (news))
    ("gnus-registry.el"
        "article registry for Gnus"
        (news registry))
    ("gnus-salt.el"
        "alternate summary mode interfaces for Gnus"
        (news))
    ("gnus-score.el"
        "scoring code for Gnus"
        (news))
    ("gnus-setup.el"
        "Initialization & Setup for Gnus 5"
        (news))
    ("gnus-sieve.el"
        "Utilities to manage sieve scripts for Gnus"
        nil)
    ("gnus-soup.el"
        "SOUP packet writing support for Gnus"
        (news mail))
    ("gnus-spec.el"
        "format spec functions for Gnus"
        (news))
    ("gnus-srvr.el"
        "virtual server support for Gnus"
        (news))
    ("gnus-start.el"
        "startup functions for Gnus"
        (news))
    ("gnus-sum.el"
        "summary mode commands for Gnus"
        (news))
    ("gnus-topic.el"
        "a folding minor mode for Gnus group buffers"
        (news))
    ("gnus-undo.el"
        "minor mode for undoing in Gnus"
        (news))
    ("gnus-util.el"
        "utility functions for Gnus"
        (news))
    ("gnus-uu.el"
        "extract (uu)encoded files in Gnus"
        nil)
    ("gnus-vm.el"
        "vm interface for Gnus"
        (news mail))
    ("gnus-win.el"
        "window configuration functions for Gnus"
        (news))
    ("gnus.el"
        "a newsreader for GNU Emacs"
        (news mail))
    ("html2text.el"
        "a simple html to plain text converter"
        nil)
    ("ietf-drums.el"
        "Functions for parsing RFC822bis headers"
        nil)
    ("legacy-gnus-agent.el"
        "Legacy unplugged support for Gnus"
        (news))
    ("mail-parse.el"
        "Interface functions for parsing mail"
        nil)
    ("mail-prsvr.el"
        "Interface variables for parsing mail"
        nil)
    ("mail-source.el"
        "functions for fetching mail"
        (news mail))
    ("mailcap.el"
        "MIME media types configuration"
        (news mail multimedia))
    ("message.el"
        "composing mail and news messages"
        (mail news))
    ("messcompat.el"
        "making message mode compatible with mail mode"
        (mail news))
    ("mm-bodies.el"
        "Functions for decoding MIME things"
        nil)
    ("mm-decode.el"
        "Functions for decoding MIME things"
        nil)
    ("mm-encode.el"
        "Functions for encoding MIME things"
        nil)
    ("mm-extern.el"
        "showing message/external-body"
        (message external-body))
    ("mm-partial.el"
        "showing message/partial"
        (message partial))
    ("mm-url.el"
        "a wrapper of url functions/commands for Gnus"
        nil)
    ("mm-util.el"
        "Utility functions for Mule and low level things"
        nil)
    ("mm-uu.el"
        "Return uu stuff as mm handles"
        (postscript uudecode binhex shar forward gnatsweb pgp))
    ("mm-view.el"
        "functions for viewing MIME objects"
        nil)
    ("mml-sec.el"
        "A package with security functions for MML documents"
        nil)
    ("mml-smime.el"
        "S/MIME support for MML"
        (gnus mime s/mime mml))
    ("mml.el"
        "A package for parsing and validating MML documents"
        nil)
    ("mml1991.el"
        "Old PGP message format (RFC 1991) support for MML"
        nil)
    ("mml2015.el"
        "MIME Security with Pretty Good Privacy (PGP)"
        (pgp mime mml))
    ("nnagent.el"
        "offline backend for Gnus"
        (news mail))
    ("nnbabyl.el"
        "rmail mbox access for Gnus"
        (news mail))
    ("nndb.el"
        "nndb access for Gnus"
        (news))
    ("nndiary.el"
        "A diary back end for Gnus"
        (calendar mail news))
    ("nndir.el"
        "single directory newsgroup access for Gnus"
        (news))
    ("nndoc.el"
        "single file access for Gnus"
        (news))
    ("nndraft.el"
        "draft article access for Gnus"
        (news))
    ("nneething.el"
        "arbitrary file access for Gnus"
        (news mail))
    ("nnfolder.el"
        "mail folder access for Gnus"
        (mail))
    ("nngateway.el"
        "posting news via mail gateways"
        (news mail))
    ("nnheader.el"
        "header access macros for Gnus and its backends"
        (news))
    ("nnimap.el"
        "imap backend for Gnus"
        (mail))
    ("nnir.el"
        "search mail with various search engines"
        (news mail searching ir))
    ("nnkiboze.el"
        "select virtual news access for Gnus"
        (news))
    ("nnlistserv.el"
        "retrieving articles via web mailing list archives"
        (news mail))
    ("nnmail.el"
        "mail support functions for the Gnus mail backends"
        (news mail))
    ("nnmaildir.el"
        "maildir backend for Gnus"
        nil)
    ("nnmairix.el"
        "Mairix back end for Gnus, the Emacs newsreader"
        (mail searching))
    ("nnmbox.el"
        "mail mbox access for Gnus"
        (news mail))
    ("nnmh.el"
        "mhspool access for Gnus"
        (news mail))
    ("nnml.el"
        "mail spool access for Gnus"
        (news mail))
    ("nnnil.el"
        "empty backend for Gnus"
        nil)
    ("nnoo.el"
        "OO Gnus Backends"
        (news))
    ("nnrss.el"
        "interfacing with RSS"
        (rss))
    ("nnslashdot.el"
        "interfacing with Slashdot"
        (news))
    ("nnsoup.el"
        "SOUP access for Gnus"
        (news mail))
    ("nnspool.el"
        "spool access for GNU Emacs"
        (news))
    ("nntp.el"
        "nntp access for Gnus"
        (news))
    ("nnultimate.el"
        "interfacing with the Ultimate Bulletin Board system"
        (news))
    ("nnvirtual.el"
        "virtual newsgroups access for Gnus"
        (news))
    ("nnwarchive.el"
        "interfacing with web archives"
        (news egroups mail-archive))
    ("nnweb.el"
        "retrieving articles via web search engines"
        (news))
    ("nnwfm.el"
        "interfacing with a web forum"
        (news))
    ("pop3.el"
        "Post Office Protocol (RFC 1460) interface"
        (mail))
    ("qp.el"
        "Quoted-Printable functions"
        (mail extensions))
    ("rfc1843.el"
        "HZ (rfc1843) decoding"
        (news hz hz+ mail i18n))
    ("rfc2045.el"
        "Functions for decoding rfc2045 headers"
        nil)
    ("rfc2047.el"
        "functions for encoding and decoding rfc2047 messages"
        nil)
    ("rfc2104.el"
        "RFC2104 Hashed Message Authentication Codes"
        (mail))
    ("rfc2231.el"
        "Functions for decoding rfc2231 headers"
        nil)
    ("score-mode.el"
        "mode for editing Gnus score files"
        (news mail))
    ("sieve-manage.el"
        "Implementation of the managesive protocol in elisp"
        nil)
    ("sieve-mode.el"
        "Sieve code editing commands for Emacs"
        nil)
    ("sieve.el"
        "Utilities to manage sieve scripts"
        nil)
    ("smiley.el"
        "displaying smiley faces"
        (news mail multimedia))
    ("smime.el"
        "S/MIME support library"
        (smime x.509 pem openssl))
    ("spam-report.el"
        "Reporting spam"
        (network spam mail gmane report))
    ("spam-stat.el"
        "detecting spam based on statistics"
        (network))
    ("spam-wash.el"
        "wash spam before analysis"
        (mail))
    ("spam.el"
        "Identifying spam"
        (network spam mail bogofilter bbdb dspam dig whitelist blacklist gmane hashcash spamassassin bsfilter ifile stat crm114 spamoracle))
    ("starttls.el"
        "STARTTLS functions"
        (tls ssl openssl gnutls mail news))
    ("utf7.el"
        "UTF-7 encoding/decoding for Emacs"
        (mail))
    ("webmail.el"
        "interface of web mail"
        (hotmail netaddress my-deja netscape))
    ("yenc.el"
        "elisp native yenc decoder"
        (yenc news))
    ("ccl.el"
        "CCL (Code Conversion Language) compiler"
        (ccl mule multilingual character set coding-system))
    ("characters.el"
        "set syntax and category for multibyte characters"
        (multibyte character character set syntax category))
    ("charprop.el"
        nil
        nil)
    ("cp51932.el"
        "translation table for CP51932."
        nil)
    ("eucjp-ms.el"
        "translation table for eucJP-ms."
        nil)
    ("fontset.el"
        "commands for handling fontset"
        (mule i18n fontset))
    ("isearch-x.el"
        "extended isearch handling commands"
        (i18n multilingual isearch))
    ("iso-ascii.el"
        "set up char tables for ISO 8859/1 on ASCII terminals"
        (i18n))
    ("iso-cvt.el"
        "translate ISO 8859-1 from/to various encodings"
        (tex iso latin i18n))
    ("iso-transl.el"
        "keyboard input definitions for ISO 8859-1"
        (i18n))
    ("ja-dic-cnv.el"
        "convert a Japanese dictionary (SKK-JISYO.L) to Emacs Lisp"
        (i18n mule multilingual japanese))
    ("ja-dic-utl.el"
        "utilities for handling Japanese dictionary (SKK-JISYO.L)"
        (i18n mule multilingual japanese))
    ("kinsoku.el"
        "`Kinsoku' processing funcs"
        (mule kinsoku))
    ("kkc.el"
        "Kana Kanji converter"
        (i18n mule multilingual japanese))
    ("latexenc.el"
        "guess correct coding system in LaTeX files"
        (mule coding system latex))
    ("latin1-disp.el"
        "display tables for other ISO 8859 on Latin-1 terminals"
        (i18n))
    ("mule-cmds.el"
        "commands for multilingual environment"
        (mule i18n))
    ("mule-conf.el"
        "configure multilingual environment"
        (i18n mule multilingual character set coding system))
    ("mule-diag.el"
        "show diagnosis of multilingual environment (Mule)"
        (multilingual charset coding system fontset diagnosis i18n))
    ("mule-util.el"
        "utility functions for mulitilingual environment (mule)"
        (mule multilingual))
    ("mule.el"
        "basic commands for multilingual environment"
        (mule multilingual character set coding system))
    ("ogonek.el"
        "change the encoding of Polish diacritics"
        (i18n))
    ("quail.el"
        "provides simple input method for multilingual text"
        (mule multilingual input method i18n))
    ("robin.el"
        "yet another input method (smaller than quail)"
        (mule multilingual input method i18n))
    ("titdic-cnv.el"
        "convert cxterm dictionary (TIT format) to Quail package"
        (quail tit cxterm))
    ("ucs-normalize.el"
        "Unicode normalization NFC/NFD/NFKD/NFKC"
        (unicode normalization))
    ("uni-bidi.el"
        nil
        nil)
    ("uni-category.el"
        nil
        nil)
    ("uni-combining.el"
        nil
        nil)
    ("uni-comment.el"
        nil
        nil)
    ("uni-decimal.el"
        nil
        nil)
    ("uni-decomposition.el"
        nil
        nil)
    ("uni-digit.el"
        nil
        nil)
    ("uni-lowercase.el"
        nil
        nil)
    ("uni-mirrored.el"
        nil
        nil)
    ("uni-name.el"
        nil
        nil)
    ("uni-numeric.el"
        nil
        nil)
    ("uni-old-name.el"
        nil
        nil)
    ("uni-titlecase.el"
        nil
        nil)
    ("uni-uppercase.el"
        nil
        nil)
    ("utf-7.el"
        "utf-7 coding system"
        (i18n mail))
    ("burmese.el"
        "support for Burmese"
        (multilingual burma i18n))
    ("cham.el"
        "support for Cham"
        (multilingual cham i18n))
    ("china-util.el"
        "utilities for Chinese"
        (mule multilingual chinese))
    ("chinese.el"
        "support for Chinese"
        (multilingual chinese))
    ("cyril-util.el"
        "utilities for Cyrillic scripts"
        (mule multilingual cyrillic))
    ("cyrillic.el"
        "support for Cyrillic"
        (multilingual cyrillic i18n))
    ("czech.el"
        "support for Czech"
        (multilingual czech))
    ("english.el"
        "support for English"
        (multibyte character character set syntax category))
    ("ethio-util.el"
        "utilities for Ethiopic"
        (mule multilingual ethiopic))
    ("ethiopic.el"
        "support for Ethiopic"
        (multilingual ethiopic))
    ("european.el"
        "support for European languages"
        (multilingual european))
    ("georgian.el"
        "language support for Georgian"
        (i18n))
    ("greek.el"
        "support for Greek"
        (multilingual greek))
    ("hanja-util.el"
        "Korean Hanja util module"
        (multilingual input method korean hanja))
    ("hebrew.el"
        "support for Hebrew"
        (multilingual hebrew))
    ("ind-util.el"
        "Transliteration and Misc. Tools for Indian Languages"
        (multilingual indian devanagari))
    ("indian.el"
        "Indian languages support"
        (multilingual i18n indian))
    ("japan-util.el"
        "utilities for Japanese"
        (mule multilingual japanese))
    ("japanese.el"
        "support for Japanese"
        (multilingual japanese))
    ("khmer.el"
        "support for Khmer"
        (multilingual khmer i18n))
    ("korea-util.el"
        "utilities for Korean"
        (mule multilingual korean))
    ("korean.el"
        "support for Korean"
        (multilingual korean))
    ("lao-util.el"
        "utilities for Lao"
        (multilingual lao i18n))
    ("lao.el"
        "support for Lao"
        (multilingual lao))
    ("misc-lang.el"
        "support for miscellaneous languages (characters)"
        (multilingual character set coding system))
    ("romanian.el"
        "support for Romanian"
        (multilingual romanian i18n))
    ("sinhala.el"
        "support for Sinhala"
        (multilingual sinhala i18n))
    ("slovak.el"
        "support for Slovak"
        (multilingual slovak))
    ("tai-viet.el"
        "support for Tai Viet"
        (multilingual tai viet i18n))
    ("thai-util.el"
        "utilities for Thai"
        (mule multilingual thai i18n))
    ("thai-word.el"
        "find Thai word boundaries"
        (thai word break emacs))
    ("thai.el"
        "support for Thai"
        (multilingual thai i18n))
    ("tibet-util.el"
        "utilities for Tibetan"
        (multilingual tibetan))
    ("tibetan.el"
        "support for Tibetan language"
        (multilingual tibetan i18n))
    ("tv-util.el"
        "support for Tai Viet"
        (multilingual tai viet i18n))
    ("utf-8-lang.el"
        "generic UTF-8 language environment"
        (i18n))
    ("viet-util.el"
        "utilities for Vietnamese"
        (mule multilingual vietnamese))
    ("vietnamese.el"
        "support for Vietnamese"
        (multilingual vietnamese i18n))
    ("binhex.el"
        "elisp native binhex decode"
        (binhex news))
    ("blessmail.el"
        "decide whether movemail needs special privileges"
        (internal))
    ("emacsbug.el"
        "command to report Emacs bugs to appropriate mailing list"
        (maint mail))
    ("feedmail.el"
        "assist other email packages to massage outgoing messages"
        (email queue mail sendmail message spray smtp draft))
    ("footnote.el"
        "footnote support for message mode"
        (mail news))
    ("hashcash.el"
        "Add hashcash payments to email"
        (mail hashcash))
    ("mail-extr.el"
        "extract full name and address from RFC 822 mail header"
        (mail))
    ("mail-hist.el"
        "headers and message body history for outgoing mail"
        (mail history))
    ("mail-utils.el"
        "utility functions used both by rmail and rnews"
        (mail news))
    ("mailabbrev.el"
        "abbrev-expansion of mail aliases"
        (mail))
    ("mailalias.el"
        "expand and complete mailing address aliases"
        (mail))
    ("mailclient.el"
        "mail sending via system's mail client."
        (mail))
    ("mailheader.el"
        "mail header parsing, merging, formatting"
        (tools mail news))
    ("mailpost.el"
        "RMAIL coupler to /usr/uci/post mailer"
        (mail))
    ("metamail.el"
        "Metamail interface for GNU Emacs"
        (mail news mime multimedia))
    ("mspools.el"
        "show mail spools waiting to be read"
        (mail))
    ("reporter.el"
        "customizable bug reporting of lisp programs"
        (maint mail tools))
    ("rfc2368.el"
        "support for rfc2368"
        (mail))
    ("rfc822.el"
        "hairy rfc822 parser for mail and news and suchlike"
        (mail))
    ("rmail-spam-filter.el"
        "spam filter for Rmail, the Emacs mail reader"
        (email spam filter rmail))
    ("rmail.el"
        "main code of \"RMAIL\" mail reader for Emacs"
        (mail))
    ("rmailedit.el"
        "\"RMAIL edit mode\"  Edit the current message"
        (mail))
    ("rmailkwd.el"
        "part of the \"RMAIL\" mail reader for Emacs"
        (mail))
    ("rmailmm.el"
        "MIME decoding and display stuff for RMAIL"
        (mail))
    ("rmailmsc.el"
        "miscellaneous support functions for the RMAIL mail reader"
        (mail))
    ("rmailout.el"
        "\"RMAIL\" mail reader for Emacs: output message to a file"
        (mail))
    ("rmailsort.el"
        "Rmail: sort messages"
        (mail))
    ("rmailsum.el"
        "make summary buffers for the mail reader"
        (mail))
    ("sendmail.el"
        "mail sending commands for Emacs."
        (mail))
    ("smtpmail.el"
        "simple SMTP protocol (RFC 821) for sending mail"
        (mail))
    ("supercite.el"
        "minor mode for citing mail and news replies"
        (mail news))
    ("uce.el"
        "facilitate reply to unsolicited commercial email"
        (mail uce unsolicited commercial email))
    ("undigest.el"
        "digest-cracking support for the RMAIL mail reader"
        (mail))
    ("unrmail.el"
        "convert Rmail Babyl files to mailbox files"
        (mail))
    ("uudecode.el"
        "elisp native uudecode"
        (uudecode news))
    ("mh-acros.el"
        "macros used in MH-E"
        (mail))
    ("mh-alias.el"
        "MH-E mail alias completion and expansion"
        (mail))
    ("mh-buffers.el"
        "MH-E buffer constants and utilities"
        (mail))
    ("mh-comp.el"
        "MH-E functions for composing and sending messages"
        (mail))
    ("mh-compat.el"
        "make MH-E compatibile with various versions of Emacs"
        (mail))
    ("mh-e.el"
        "GNU Emacs interface to the MH mail system"
        (mail))
    ("mh-folder.el"
        "MH-Folder mode"
        (mail))
    ("mh-funcs.el"
        "MH-E functions not everyone will use right away"
        (mail))
    ("mh-gnus.el"
        "make MH-E compatible with various versions of Gnus"
        (mail))
    ("mh-identity.el"
        "multiple identify support for MH-E"
        (mail))
    ("mh-inc.el"
        "MH-E \"inc\" and separate mail spool handling"
        (mail))
    ("mh-junk.el"
        "MH-E interface to anti-spam measures"
        (mail spam))
    ("mh-letter.el"
        "MH-Letter mode"
        (mail))
    ("mh-limit.el"
        "MH-E display limits"
        (mail))
    ("mh-mime.el"
        "MH-E MIME support"
        (mail))
    ("mh-print.el"
        "MH-E printing support"
        (mail))
    ("mh-scan.el"
        "MH-E scan line constants and utilities"
        (mail))
    ("mh-search.el"
        "MH-Search mode"
        (mail))
    ("mh-seq.el"
        "MH-E sequences support"
        (mail))
    ("mh-show.el"
        "MH-Show mode"
        (mail))
    ("mh-speed.el"
        "MH-E speedbar support"
        (mail))
    ("mh-thread.el"
        "MH-E threading support"
        (mail))
    ("mh-tool-bar.el"
        "MH-E tool bar support"
        (mail))
    ("mh-utils.el"
        "MH-E general utilities"
        (mail))
    ("mh-xface.el"
        "MH-E X-Face and Face header field display"
        (mail))
    ("ange-ftp.el"
        "transparent FTP support for GNU Emacs"
        (comm))
    ("browse-url.el"
        "pass a URL to a WWW browser"
        (hypertext hypermedia mouse))
    ("dbus.el"
        "Elisp bindings for D-Bus."
        (comm hardware))
    ("dig.el"
        "Domain Name System dig interface"
        (dns bind dig comm))
    ("dns.el"
        "Domain Name Service lookups"
        (network comm))
    ("eudc-bob.el"
        "Binary Objects Support for EUDC"
        (comm))
    ("eudc-export.el"
        "functions to export EUDC query results"
        (comm))
    ("eudc-hotlist.el"
        "hotlist management for EUDC"
        (comm))
    ("eudc-vars.el"
        "Emacs Unified Directory Client"
        (comm))
    ("eudc.el"
        "Emacs Unified Directory Client"
        (comm))
    ("eudcb-bbdb.el"
        "Emacs Unified Directory Client - BBDB Backend"
        (comm))
    ("eudcb-ldap.el"
        "Emacs Unified Directory Client - LDAP Backend"
        (comm))
    ("eudcb-mab.el"
        "Emacs Unified Directory Client - AddressBook backend"
        (comm))
    ("eudcb-ph.el"
        "Emacs Unified Directory Client - CCSO PH/QI Backend"
        (comm))
    ("goto-addr.el"
        "click to browse URL or to send to e-mail address"
        (mh-e www mouse mail))
    ("hmac-def.el"
        "A macro for defining HMAC functions."
        (hmac rfc-2104))
    ("hmac-md5.el"
        "Compute HMAC-MD5."
        (hmac rfc-2104 hmac-md5 md5 keyed-md5 cram-md5))
    ("imap-hash.el"
        "Hashtable-like interface to an IMAP mailbox"
        (mail))
    ("imap.el"
        "imap library"
        (mail))
    ("ldap.el"
        "client interface to LDAP for Emacs"
        (comm))
    ("mairix.el"
        "Mairix interface for Emacs"
        (mail searching))
    ("net-utils.el"
        "network functions"
        (network comm))
    ("netrc.el"
        ".netrc parsing functionality"
        (news modularized by ted zlatanov <tzz@lifelogs.com> when it was part of gnus.))
    ("newst-backend.el"
        "Retrieval backend for newsticker."
        (news rss atom))
    ("newst-plainview.el"
        "Single buffer frontend for newsticker."
        nil)
    ("newst-reader.el"
        "Generic RSS reader functions."
        nil)
    ("newst-ticker.el"
        "modeline ticker for newsticker."
        (news rss atom))
    ("newst-treeview.el"
        "Treeview frontend for newsticker."
        (news rss atom))
    ("newsticker.el"
        "A Newsticker for Emacs."
        (news rss atom))
    ("ntlm.el"
        "NTLM (NT LanManager) authentication support"
        (ntlm sasl))
    ("quickurl.el"
        "insert an URL based on text at point in buffer"
        (hypermedia))
    ("rcirc.el"
        "default, simple IRC client."
        (comm))
    ("rcompile.el"
        "run a compilation on a remote machine"
        (tools processes))
    ("rlogin.el"
        "remote login interface"
        (unix comm))
    ("sasl-cram.el"
        "CRAM-MD5 module for the SASL client framework"
        (sasl cram-md5))
    ("sasl-digest.el"
        "DIGEST-MD5 module for the SASL client framework"
        (sasl digest-md5))
    ("sasl-ntlm.el"
        "NTLM (NT Lan Manager) module for the SASL client framework"
        (sasl ntlm))
    ("sasl.el"
        "SASL client framework"
        (sasl))
    ("snmp-mode.el"
        "SNMP & SNMPv2 MIB major mode"
        (data))
    ("socks.el"
        "A Socks v5 Client for Emacs"
        (comm firewalls))
    ("telnet.el"
        "run a telnet session from within an Emacs buffer"
        (unix comm))
    ("tls.el"
        "TLS/SSL support via wrapper around GnuTLS"
        (comm tls gnutls ssl))
    ("tramp-cache.el"
        "file information caching for Tramp"
        (comm processes))
    ("tramp-cmds.el"
        "Interactive commands for Tramp"
        (comm processes))
    ("tramp-compat.el"
        "Tramp compatibility functions"
        (comm processes))
    ("tramp-fish.el"
        "Tramp access functions for FISH protocol"
        (comm processes))
    ("tramp-ftp.el"
        "Tramp convenience functions for Ange-FTP"
        (comm processes))
    ("tramp-gvfs.el"
        "Tramp access functions for GVFS daemon"
        (comm processes))
    ("tramp-gw.el"
        "Tramp utility functions for HTTP tunnels and SOCKS gateways"
        (comm processes))
    ("tramp-imap.el"
        "Tramp interface to IMAP through imap.el"
        (mail comm))
    ("tramp-smb.el"
        "Tramp access functions for SMB servers"
        (comm processes))
    ("tramp-uu.el"
        "uuencode in Lisp"
        (comm terminals))
    ("tramp.el"
        "Transparent Remote Access, Multiple Protocol"
        (comm processes))
    ("trampver.el"
        "Transparent Remote Access, Multiple Protocol"
        (comm processes))
    ("webjump.el"
        "programmable Web hotlist"
        (comm www))
    ("xesam.el"
        "Xesam interface to search engines."
        (tools hypermedia))
    ("zeroconf.el"
        "Service browser using Avahi."
        (comm hardware))
    ("nxml-enc.el"
        "XML encoding auto-detection"
        (xml))
    ("nxml-glyph.el"
        "glyph-handling for nxml-mode"
        (xml))
    ("nxml-maint.el"
        "commands for maintainers of nxml-*.el"
        (xml))
    ("nxml-mode.el"
        "a new XML mode"
        (xml))
    ("nxml-ns.el"
        "XML namespace processing"
        (xml))
    ("nxml-outln.el"
        "outline support for nXML mode"
        (xml))
    ("nxml-parse.el"
        "XML parser, sharing infrastructure with nxml-mode"
        (xml))
    ("nxml-rap.el"
        "low-level support for random access parsing for nXML mode"
        (xml))
    ("nxml-uchnm.el"
        "support for Unicode standard cha names in nxml-mode"
        (xml))
    ("nxml-util.el"
        "utility functions for nxml-*.el"
        (xml))
    ("rng-cmpct.el"
        "parsing of RELAX NG Compact Syntax schemas"
        (xml relaxng))
    ("rng-dt.el"
        "datatype library interface for RELAX NG"
        (xml relaxng))
    ("rng-loc.el"
        "locate the schema to use for validation"
        (xml relaxng))
    ("rng-maint.el"
        "commands for RELAX NG maintainers"
        (xml relaxng))
    ("rng-match.el"
        "matching of RELAX NG patterns against XML events"
        (xml relaxng))
    ("rng-nxml.el"
        "make nxml-mode take advantage of rng-validate-mode"
        (xml relaxng))
    ("rng-parse.el"
        "parse an XML file and validate it against a schema"
        (xml relaxng))
    ("rng-pttrn.el"
        "RELAX NG patterns"
        (xml relaxng))
    ("rng-uri.el"
        "URI parsing and manipulation"
        (xml))
    ("rng-util.el"
        "utility functions for RELAX NG library"
        (xml relaxng))
    ("rng-valid.el"
        "real-time validation of XML using RELAX NG"
        (xml relaxng))
    ("rng-xsd.el"
        "W3C XML Schema datatypes library for RELAX NG"
        (xml relaxng))
    ("xmltok.el"
        "XML tokenization"
        (xml))
    ("xsd-regexp.el"
        "translate W3C XML Schema regexps to Emacs regexps"
        (xml regexp))
    ("org-agenda.el"
        "Dynamic task and appointment lists for Org"
        (outlines hypermedia calendar wp))
    ("org-archive.el"
        "Archiving for Org-mode"
        (outlines hypermedia calendar wp))
    ("org-ascii.el"
        "ASCII export for Org-mode"
        (outlines hypermedia calendar wp))
    ("org-attach.el"
        "Manage file attachments to org-mode tasks"
        (org data task))
    ("org-bbdb.el"
        "Support for links to BBDB entries from within Org-mode"
        (outlines hypermedia calendar wp))
    ("org-bibtex.el"
        "Org links to BibTeX entries"
        (org wp remember))
    ("org-clock.el"
        "The time clocking code for Org-mode"
        (outlines hypermedia calendar wp))
    ("org-colview.el"
        "Column View in Org-mode"
        (outlines hypermedia calendar wp))
    ("org-compat.el"
        "Compatibility code for Org-mode"
        (outlines hypermedia calendar wp))
    ("org-crypt.el"
        "Public key encryption for org-mode entries"
        (org-mode))
    ("org-datetree.el"
        "Create date entries in a tree"
        (outlines hypermedia calendar wp))
    ("org-docbook.el"
        "DocBook exporter for org-mode"
        (org wp docbook))
    ("org-exp-blocks.el"
        "pre-process blocks when exporting org files"
        nil)
    ("org-exp.el"
        "ASCII, HTML, XOXO and iCalendar export for Org-mode"
        (outlines hypermedia calendar wp))
    ("org-faces.el"
        "Face definitions for Org-mode."
        (outlines hypermedia calendar wp))
    ("org-feed.el"
        "Add RSS feed items to Org files"
        (outlines hypermedia calendar wp))
    ("org-footnote.el"
        "Footnote support in Org and elsewhere"
        (outlines hypermedia calendar wp))
    ("org-freemind.el"
        "Export Org files to freemind"
        (outlines hypermedia calendar wp))
    ("org-gnus.el"
        "Support for links to Gnus groups and messages from within Org-mode"
        (outlines hypermedia calendar wp))
    ("org-habit.el"
        "The habit tracking code for Org-mode"
        (outlines hypermedia calendar wp))
    ("org-html.el"
        "HTML export for Org-mode"
        (outlines hypermedia calendar wp))
    ("org-icalendar.el"
        "iCalendar export for Org-mode"
        (outlines hypermedia calendar wp))
    ("org-id.el"
        "Global identifiers for Org-mode entries"
        (outlines hypermedia calendar wp))
    ("org-indent.el"
        "Dynamic indentation for  Org-mode"
        (outlines hypermedia calendar wp))
    ("org-info.el"
        "Support for links to Info nodes from within Org-Mode"
        (outlines hypermedia calendar wp))
    ("org-inlinetask.el"
        "Tasks independent of outline hierarchy"
        (outlines hypermedia calendar wp))
    ("org-install.el"
        "Outline-based notes management and organizer"
        (outlines hypermedia calendar wp))
    ("org-irc.el"
        "Store links to IRC sessions"
        (erc irc link org))
    ("org-jsinfo.el"
        "Support for org-info.js Javascript in Org HTML export"
        (outlines hypermedia calendar wp))
    ("org-latex.el"
        "LaTeX exporter for org-mode"
        (org wp tex))
    ("org-list.el"
        "Plain lists for Org-mode"
        (outlines hypermedia calendar wp))
    ("org-mac-message.el"
        "Links to Apple Mail.app messages from within Org-mode"
        (outlines hypermedia calendar wp))
    ("org-macs.el"
        "Top-level definitions for Org-mode"
        (outlines hypermedia calendar wp))
    ("org-mew.el"
        "Support for links to Mew messages from within Org-mode"
        (outlines hypermedia calendar wp))
    ("org-mhe.el"
        "Support for links to MH-E messages from within Org-mode"
        (outlines hypermedia calendar wp))
    ("org-mobile.el"
        "Code for asymmetric sync with a mobile device"
        (outlines hypermedia calendar wp))
    ("org-mouse.el"
        "Better mouse support for org-mode"
        nil)
    ("org-plot.el"
        "Support for plotting from Org-mode"
        (tables plotting))
    ("org-protocol.el"
        "Intercept calls from emacsclient to trigger custom actions."
        (org emacsclient wp))
    ("org-publish.el"
        "publish related org-mode files as a website"
        (hypermedia outlines wp))
    ("org-remember.el"
        "Fast note taking in Org-mode"
        (outlines hypermedia calendar wp))
    ("org-rmail.el"
        "Support for links to Rmail messages from within Org-mode"
        (outlines hypermedia calendar wp))
    ("org-src.el"
        "Source code examples in Org"
        (outlines hypermedia calendar wp))
    ("org-table.el"
        "The table editor for Org-mode"
        (outlines hypermedia calendar wp))
    ("org-timer.el"
        "The relative timer code for Org-mode"
        (outlines hypermedia calendar wp))
    ("org-vm.el"
        "Support for links to VM messages from within Org-mode"
        (outlines hypermedia calendar wp))
    ("org-w3m.el"
        "Support from copy and paste from w3m to Org-mode"
        (outlines hypermedia calendar wp))
    ("org-wl.el"
        "Support for links to Wanderlust messages from within Org-mode"
        (outlines hypermedia calendar wp))
    ("org-xoxo.el"
        "XOXO export for Org-mode"
        (outlines hypermedia calendar wp))
    ("org.el"
        "Outline-based notes management and organizer"
        (outlines hypermedia calendar wp))
    ("5x5.el"
        "simple little puzzle game"
        (games puzzles))
    ("animate.el"
        "make text dance"
        (games))
    ("blackbox.el"
        "blackbox game in Emacs Lisp"
        (games))
    ("bruce.el"
        "bruce phrase utility for overloading the Communications"
        (games))
    ("bubbles.el"
        "Puzzle game for Emacs."
        (games))
    ("cookie1.el"
        "retrieve random phrases from fortune cookie files"
        (games extensions))
    ("decipher.el"
        "cryptanalyze monoalphabetic substitution ciphers"
        (games))
    ("dissociate.el"
        "scramble text amusingly for Emacs"
        (games))
    ("doctor.el"
        "psychological help for frustrated users"
        (games))
    ("dunnet.el"
        "text adventure for Emacs"
        (games))
    ("fortune.el"
        "use fortune to create signatures"
        (games utils mail))
    ("gamegrid.el"
        "library for implementing grid-based games on Emacs"
        (games))
    ("gametree.el"
        "manage game analysis trees in Emacs"
        (games))
    ("gomoku.el"
        "Gomoku game between you and Emacs"
        (games))
    ("handwrite.el"
        "turns your emacs buffer into a handwritten document"
        (wp print postscript cursive writing))
    ("hanoi.el"
        "towers of hanoi in Emacs"
        (games))
    ("landmark.el"
        "neural-network robot that learns landmarks"
        (games gomoku neural network adaptive search chemotaxis))
    ("life.el"
        "John Horton Conway's `Life' game for GNU Emacs"
        (games))
    ("meese.el"
        "protect the impressionable young minds of America"
        (games))
    ("morse.el"
        "convert text to morse code and back"
        (games))
    ("mpuz.el"
        "multiplication puzzle for GNU Emacs"
        (games))
    ("pong.el"
        "classical implementation of pong"
        (games))
    ("snake.el"
        "implementation of Snake for Emacs"
        (games))
    ("solitaire.el"
        "game of solitaire in Emacs Lisp"
        (games))
    ("spook.el"
        "spook phrase utility for overloading the NSA line eater"
        (games))
    ("studly.el"
        "StudlyCaps (tm)(r)(c)(xxx)"
        (games))
    ("tetris.el"
        "implementation of Tetris for Emacs"
        (games))
    ("yow.el"
        "quote random zippyisms"
        (games))
    ("zone.el"
        "idle display hacks"
        (games))
    ("ada-mode.el"
        "major-mode for editing Ada sources"
        (languages ada))
    ("ada-prj.el"
        "GUI editing of project files for the ada-mode"
        (languages ada project file))
    ("ada-stmt.el"
        "an extension to Ada mode for inserting statement templates"
        (languages ada))
    ("ada-xref.el"
        "for lookup and completion in Ada mode"
        (languages ada xref))
    ("antlr-mode.el"
        "major mode for ANTLR grammar files"
        (languages antlr code generator))
    ("asm-mode.el"
        "mode for editing assembler code"
        (tools languages))
    ("autoconf.el"
        "mode for editing Autoconf configure.in files"
        (languages))
    ("bug-reference.el"
        "buttonize bug references"
        (tools))
    ("cap-words.el"
        "minor mode for motion in CapitalizedWordIdentifiers"
        (languages))
    ("cc-align.el"
        "custom indentation functions for CC Mode"
        (c languages oop))
    ("cc-awk.el"
        "AWK specific code within cc-mode."
        (awk cc-mode unix languages))
    ("cc-bytecomp.el"
        "compile time setup for proper compilation"
        (c languages oop))
    ("cc-cmds.el"
        "user level commands for CC Mode"
        (c languages oop))
    ("cc-compat.el"
        "cc-mode compatibility with c-mode.el confusion"
        (c languages oop))
    ("cc-defs.el"
        "compile time definitions for CC Mode"
        (c languages oop))
    ("cc-engine.el"
        "core syntax guessing engine for CC mode"
        (c languages oop))
    ("cc-fonts.el"
        "font lock support for CC Mode"
        (c languages oop))
    ("cc-langs.el"
        "language specific settings for CC Mode"
        (c languages oop))
    ("cc-menus.el"
        "imenu support for CC Mode"
        (c languages oop))
    ("cc-mode.el"
        "major mode for editing C and similar languages"
        (c languages oop))
    ("cc-styles.el"
        "support for styles in CC Mode"
        (c languages oop))
    ("cc-vars.el"
        "user customization variables for CC Mode"
        (c languages oop))
    ("cfengine.el"
        "mode for editing Cfengine files"
        (languages))
    ("cmacexp.el"
        "expand C macros in a region"
        (c))
    ("compile.el"
        "run compiler as inferior of Emacs, parse error messages"
        (tools processes))
    ("cperl-mode.el"
        "Perl code editing commands for Emacs"
        (languages perl))
    ("cpp.el"
        "highlight or hide text according to cpp conditionals"
        (c faces tools))
    ("cwarn.el"
        "highlight suspicious C and C++ constructions"
        (c languages faces))
    ("dcl-mode.el"
        "major mode for editing DCL command files"
        (dcl editing major-mode languages))
    ("delphi.el"
        "major mode for editing Delphi source (Object Pascal) in Emacs"
        (languages))
    ("ebnf-abn.el"
        "parser for ABNF (Augmented BNF)"
        (wp ebnf postscript))
    ("ebnf-bnf.el"
        "parser for EBNF"
        (wp ebnf postscript))
    ("ebnf-dtd.el"
        "parser for DTD (Data Type Description for XML)"
        (wp ebnf postscript))
    ("ebnf-ebx.el"
        "parser for EBNF used to specify XML (EBNFX)"
        (wp ebnf postscript))
    ("ebnf-iso.el"
        "parser for ISO EBNF"
        (wp ebnf postscript))
    ("ebnf-otz.el"
        "syntactic chart OpTimiZer"
        (wp ebnf postscript))
    ("ebnf-yac.el"
        "parser for Yacc/Bison"
        (wp ebnf postscript))
    ("ebnf2ps.el"
        "translate an EBNF to a syntactic chart on PostScript"
        (wp ebnf postscript))
    ("ebrowse.el"
        "Emacs C++ class browser & tags facility"
        (c++ tags tools))
    ("etags.el"
        "etags facility for Emacs"
        (tools))
    ("executable.el"
        "base functionality for executable interpreter scripts"
        (languages unix))
    ("f90.el"
        "Fortran-90 mode (free format)"
        (fortran f90 languages))
    ("flymake.el"
        "a universal on-the-fly syntax checker"
        (c languages tools))
    ("fortran.el"
        "Fortran mode for GNU Emacs"
        (fortran languages))
    ("gdb-ui.el"
        "User Interface for running GDB"
        (unix tools))
    ("glasses.el"
        "make cantReadThis readable"
        (tools))
    ("grep.el"
        "run Grep as inferior of Emacs, parse match messages"
        (tools processes))
    ("gud.el"
        "Grand Unified Debugger mode for running GDB and other debuggers"
        (unix tools))
    ("hideif.el"
        "hides selected code within ifdef"
        (c outlines))
    ("hideshow.el"
        "minor mode cmds to selectively display code/comment blocks"
        (c c++ java lisp tools editing comments blocks hiding outlines))
    ("icon.el"
        "mode for editing Icon code"
        (languages))
    ("idlw-complete-structtag.el"
        "Completion of structure tags."
        (languages))
    ("idlw-help.el"
        "HTML Help code for IDLWAVE"
        nil)
    ("idlw-shell.el"
        "run IDL as an inferior process of Emacs."
        (processes))
    ("idlw-toolbar.el"
        "a debugging toolbar for IDLWAVE"
        (processes))
    ("idlwave.el"
        "IDL editing mode for GNU Emacs"
        (languages))
    ("inf-lisp.el"
        "an inferior-lisp mode"
        (processes lisp))
    ("js.el"
        "Major mode for editing JavaScript"
        (languages oop javascript))
    ("ld-script.el"
        "GNU linker script editing mode for Emacs"
        (languages faces))
    ("m4-mode.el"
        "m4 code editing commands for Emacs"
        (languages faces))
    ("make-mode.el"
        "makefile editing commands for Emacs"
        (unix tools))
    ("mantemp.el"
        "create manual template instantiations from g++ 2.7.2 output"
        (g++ templates))
    ("meta-mode.el"
        "major mode for editing Metafont or MetaPost sources"
        (metafont metapost tex languages))
    ("mixal-mode.el"
        "Major mode for the mix asm language."
        (languages knuth mix mixal asm mixvm "the art of computer programming"))
    ("modula2.el"
        "Modula-2 editing support package"
        (languages))
    ("octave-inf.el"
        "running Octave as an inferior Emacs process"
        (languages))
    ("octave-mod.el"
        "editing Octave source files under Emacs"
        (languages))
    ("pascal.el"
        "major mode for editing pascal source in Emacs"
        (languages))
    ("perl-mode.el"
        "Perl code editing commands for GNU Emacs"
        (languages))
    ("prolog.el"
        "major mode for editing and running Prolog under Emacs"
        (languages))
    ("ps-mode.el"
        "PostScript mode for GNU Emacs"
        (postscript languages))
    ("python.el"
        "silly walks for Python"
        (languages))
    ("ruby-mode.el"
        "Major mode for editing Ruby files"
        (languages ruby))
    ("scheme.el"
        "Scheme (and DSSSL) editing mode"
        (languages lisp))
    ("sh-script.el"
        "shell-script editing commands for Emacs"
        (languages unix))
    ("simula.el"
        "SIMULA 87 code editing commands for Emacs"
        (languages))
    ("sql.el"
        "specialized comint.el for SQL interpreters"
        (comm languages processes))
    ("subword.el"
        "Handling capitalized subwords in a nomenclature"
        nil)
    ("tcl.el"
        "Tcl code editing commands for Emacs"
        (languages tcl modes))
    ("vera-mode.el"
        "major mode for editing Vera files."
        (languages vera))
    ("verilog-mode.el"
        "major mode for editing verilog source in Emacs"
        (languages))
    ("vhdl-mode.el"
        "major mode for editing VHDL code"
        (languages vhdl))
    ("which-func.el"
        "print current function in mode line"
        (mode-line imenu tools))
    ("xscheme.el"
        "run MIT Scheme under Emacs"
        (languages lisp))
    ("artist.el"
        "draw ascii graphics with your mouse"
        (mouse))
    ("bib-mode.el"
        "major mode for editing bib files"
        (bib))
    ("bibtex-style.el"
        "Major mode for BibTeX Style files"
        (tex))
    ("bibtex.el"
        "BibTeX mode for GNU Emacs"
        (bibtex latex tex))
    ("conf-mode.el"
        "Simple major mode for editing conf/ini/properties files"
        (conf ini windows java))
    ("css-mode.el"
        "Major mode to edit CSS files"
        (hypermedia))
    ("dns-mode.el"
        "a mode for viewing/editing Domain Name System master files"
        (dns master zone file soa comm))
    ("enriched.el"
        "read and save files in text/enriched format"
        (wp faces))
    ("fill.el"
        "fill commands for Emacs"
        (wp))
    ("flyspell.el"
        "on-the-fly spell checker"
        (convenience))
    ("ispell.el"
        "interface to International Ispell Versions 3.1 and 3.2"
        (unix wp))
    ("makeinfo.el"
        "run makeinfo conveniently"
        (docs convenience))
    ("nroff-mode.el"
        "GNU Emacs major mode for editing nroff source"
        (wp))
    ("page-ext.el"
        "extended page handling commands"
        (wp data))
    ("page.el"
        "page motion commands for Emacs"
        (wp convenience))
    ("paragraphs.el"
        "paragraph and sentence parsing"
        (wp))
    ("picture.el"
        "\"Picture mode\" -- editing using quarter-plane screen model"
        (convenience wp))
    ("po.el"
        "basic support of PO translation files"
        (i18n files))
    ("refbib.el"
        "convert refer-style references to ones usable by Latex bib"
        (bib tex))
    ("refer.el"
        "look up references in bibliography files"
        (bib))
    ("refill.el"
        "`auto-fill' by refilling paragraphs on changes"
        (wp))
    ("reftex-auc.el"
        "RefTeX's interface to AUCTeX"
        nil)
    ("reftex-cite.el"
        "creating citations with RefTeX"
        nil)
    ("reftex-dcr.el"
        "viewing cross references and citations with RefTeX"
        nil)
    ("reftex-global.el"
        "operations on entire documents with RefTeX"
        nil)
    ("reftex-index.el"
        "index support with RefTeX"
        nil)
    ("reftex-parse.el"
        "parser functions for RefTeX"
        nil)
    ("reftex-ref.el"
        "code to create labels and references with RefTeX"
        nil)
    ("reftex-sel.el"
        "the selection modes for RefTeX"
        nil)
    ("reftex-toc.el"
        "RefTeX's table of contents mode"
        nil)
    ("reftex-vars.el"
        "configuration variables for RefTeX"
        nil)
    ("reftex.el"
        "minor mode for doing \\label, \\ref, \\cite, \\index in LaTeX"
        (tex))
    ("remember.el"
        "a mode for quickly jotting down things to remember"
        (data memory todo pim))
    ("rst.el"
        "Mode for viewing and editing reStructuredText-documents."
        nil)
    ("sgml-mode.el"
        "SGML- and HTML-editing modes"
        (wp hypermedia comm languages))
    ("spell.el"
        "spelling correction interface for Emacs"
        (wp unix))
    ("table.el"
        "create and edit WYSIWYG text based embedded tables"
        (wp convenience))
    ("tex-mode.el"
        "TeX, LaTeX, and SliTeX mode commands"
        (tex))
    ("texinfmt.el"
        "format Texinfo files into Info files"
        (maint tex docs))
    ("texinfo.el"
        "major mode for editing Texinfo files"
        (maint tex docs))
    ("texnfo-upd.el"
        "utilities for updating nodes and menus in Texinfo files"
        (maint tex docs))
    ("text-mode.el"
        "text mode, and its idiosyncratic commands"
        (wp))
    ("tildify.el"
        "adding hard spaces into texts"
        (text tex sgml wp))
    ("two-column.el"
        "minor mode for editing of two-column text"
        (wp))
    ("underline.el"
        "insert/remove underlining (done by overstriking) in Emacs"
        (wp))
    ("url-about.el"
        "Show internal URLs"
        (comm data processes hypermedia))
    ("url-auth.el"
        "Uniform Resource Locator authorization modules"
        (comm data processes hypermedia))
    ("url-cache.el"
        "Uniform Resource Locator retrieval tool"
        (comm data processes hypermedia))
    ("url-cid.el"
        "Content-ID URL loader"
        (comm data processes))
    ("url-cookie.el"
        "Netscape Cookie support"
        (comm data processes hypermedia))
    ("url-dav.el"
        "WebDAV support"
        (url vc))
    ("url-dired.el"
        "URL Dired minor mode"
        (comm files))
    ("url-expand.el"
        "expand-file-name for URLs"
        (comm data processes))
    ("url-file.el"
        "File retrieval code"
        (comm data processes))
    ("url-ftp.el"
        "FTP wrapper"
        (comm data processes))
    ("url-gw.el"
        "Gateway munging for URL loading"
        (comm data processes))
    ("url-handlers.el"
        "file-name-handler stuff for URL loading"
        (comm data processes hypermedia))
    ("url-history.el"
        "Global history tracking for URL package"
        (comm data processes hypermedia))
    ("url-http.el"
        "HTTP retrieval routines"
        (comm data processes))
    ("url-imap.el"
        "IMAP retrieval routines"
        (comm data processes))
    ("url-irc.el"
        "IRC URL interface"
        (comm data processes))
    ("url-ldap.el"
        "LDAP Uniform Resource Locator retrieval code"
        (comm data processes))
    ("url-mailto.el"
        "Mail Uniform Resource Locator retrieval code"
        (comm data processes))
    ("url-methods.el"
        "Load URL schemes as needed"
        (comm data processes hypermedia))
    ("url-misc.el"
        "Misc Uniform Resource Locator retrieval code"
        (comm data processes))
    ("url-news.el"
        "News Uniform Resource Locator retrieval code"
        (comm data processes))
    ("url-nfs.el"
        "NFS URL interface"
        (comm data processes))
    ("url-ns.el"
        "Various netscape-ish functions for proxy definitions"
        (comm data processes hypermedia))
    ("url-parse.el"
        "Uniform Resource Locator parser"
        (comm data processes))
    ("url-privacy.el"
        "Global history tracking for URL package"
        (comm data processes hypermedia))
    ("url-proxy.el"
        "Proxy server support"
        (comm data processes hypermedia))
    ("url-util.el"
        "Miscellaneous helper routines for URL library"
        (comm data processes))
    ("url-vars.el"
        "Variables for Uniform Resource Locator tool"
        (comm data processes hypermedia))
    ("url.el"
        "Uniform Resource Locator retrieval tool"
        (comm data processes hypermedia))
    ("cedet-cscope.el"
        "CScope support for CEDET"
        nil)
    ("cedet-files.el"
        "Common routines dealing with file names."
        nil)
    ("cedet-global.el"
        "GNU Global support for CEDET."
        nil)
    ("cedet-idutils.el"
        "ID Utils support for CEDET."
        (oo lisp))
    ("cedet.el"
        "Setup CEDET environment"
        (oo lisp))
    ("data-debug.el"
        "Datastructure Debugger"
        (oo lisp))
    ("ede.el"
        "Emacs Development Environment gloss"
        (project make))
    ("inversion.el"
        "When you need something in version XX.XX"
        (oo lisp))
    ("mode-local.el"
        "Support for mode local facilities"
        (syntax))
    ("pulse.el"
        "Pulsing Overlays"
        nil)
    ("semantic.el"
        "Semantic buffer evaluator."
        (syntax))
    ("srecode.el"
        "Semantic buffer evaluator."
        (codegeneration))
    ("autoconf-edit.el"
        "Keymap for autoconf"
        (project))
    ("cpp-root.el"
        "A simple way to wrap a C++ project with a single root"
        nil)
    ("emacs.el"
        "Special project for Emacs"
        nil)
    ("linux.el"
        "Special project for Linux"
        nil)
    ("make.el"
        "General information about \"make\""
        nil)
    ("makefile-edit.el"
        "Makefile editing/scanning commands."
        nil)
    ("pconf.el"
        "configure.ac maintenance for EDE"
        (project))
    ("pmake.el"
        "EDE Generic Project Makefile code generator."
        (project make))
    ("proj-archive.el"
        "EDE Generic Project archive support"
        (project make))
    ("proj-aux.el"
        "EDE Generic Project auxiliary file support"
        (project make))
    ("proj-comp.el"
        "EDE Generic Project compiler/rule driver"
        (project make))
    ("proj-elisp.el"
        "EDE Generic Project Emacs Lisp support"
        (project make))
    ("proj-info.el"
        "EDE Generic Project texinfo support"
        (project make))
    ("proj-misc.el"
        "EDE Generic Project Emacs Lisp support"
        (project make))
    ("proj-obj.el"
        "EDE Generic Project Object code generation support"
        (project make))
    ("proj-prog.el"
        "EDE Generic Project program support"
        (project make))
    ("proj-scheme.el"
        "EDE Generic Project scheme (guile) support"
        (project make scheme))
    ("proj-shared.el"
        "EDE Generic Project shared library support"
        (project make))
    ("proj.el"
        "EDE Generic Project file driver"
        (project make))
    ("project-am.el"
        "A project management scheme based on automake files."
        (project make))
    ("source.el"
        "EDE source code object"
        (project make))
    ("system.el"
        "EDE working with the system (VC, FTP, ETC)"
        (project make vc))
    ("util.el"
        "EDE utilities"
        (project make))
    ("analyze.el"
        "Analyze semantic tags against local context"
        nil)
    ("bovine.el"
        "LL Parser/Analyzer core."
        nil)
    ("ctxt.el"
        "Context calculations for Semantic tools."
        (syntax))
    ("db-debug.el"
        "Extra level debugging routines for Semantic"
        nil)
    ("db-ebrowse.el"
        "Semanticdb backend using ebrowse."
        (tags))
    ("db-el.el"
        "Semantic database extensions for Emacs Lisp"
        (tags))
    ("db-file.el"
        "Save a semanticdb to a cache file."
        (tags))
    ("db-find.el"
        "Searching through semantic databases."
        (tags))
    ("db-global.el"
        "Semantic database extensions for GLOBAL"
        (tags))
    ("db-javascript.el"
        "Semantic database extensions for javascript"
        nil)
    ("db-mode.el"
        "Semanticdb Minor Mode"
        nil)
    ("db-ref.el"
        "Handle cross-db file references"
        nil)
    ("db-typecache.el"
        "Manage Datatypes"
        nil)
    ("db.el"
        "Semantic tag database manager"
        (tags))
    ("decorate.el"
        "Utilities for decorating/highlighting tokens."
        (syntax))
    ("dep.el"
        "Methods for tracking dependencies (include files)"
        (syntax))
    ("doc.el"
        "Routines for documentation strings"
        (syntax))
    ("ede-grammar.el"
        "EDE support for Semantic Grammar Files"
        (project make))
    ("edit.el"
        "Edit Management for Semantic"
        nil)
    ("find.el"
        "Search routines for Semantic"
        (syntax))
    ("fw.el"
        "Framework for Semantic"
        nil)
    ("grammar-wy.el"
        "Generated parser support file"
        (syntax))
    ("grammar.el"
        "Major mode framework for Semantic grammars"
        nil)
    ("html.el"
        "Semantic details for html files"
        nil)
    ("ia-sb.el"
        "Speedbar analysis display interactor"
        (syntax))
    ("ia.el"
        "Interactive Analysis functions"
        (syntax))
    ("idle.el"
        "Schedule parsing tasks in idle time"
        (syntax))
    ("java.el"
        "Semantic functions for Java"
        nil)
    ("lex-spp.el"
        "Semantic Lexical Pre-processor"
        nil)
    ("lex.el"
        "Lexical Analyzer builder"
        nil)
    ("mru-bookmark.el"
        "Automatic bookmark tracking"
        nil)
    ("sb.el"
        "Semantic tag display for speedbar"
        (syntax))
    ("scope.el"
        "Analyzer Scope Calculations"
        nil)
    ("senator.el"
        "SEmantic NAvigaTOR"
        (syntax))
    ("symref.el"
        "Symbol Reference API"
        nil)
    ("tag-file.el"
        "Routines that find files based on tags."
        (syntax))
    ("tag-ls.el"
        "Language Specific override functions for tags"
        nil)
    ("tag-write.el"
        "Write tags to a text stream"
        nil)
    ("tag.el"
        "tag creation and access"
        nil)
    ("texi.el"
        "Semantic details for Texinfo files"
        nil)
    ("util-modes.el"
        "Semantic minor modes"
        (syntax))
    ("wisent.el"
        "Wisent - Semantic gateway"
        (syntax))
    ("fcn.el"
        "Analyzer support functions."
        nil)
    ("refs.el"
        "Analysis of the references between tags."
        nil)
    ("c-by.el"
        "Generated parser support file"
        nil)
    ("c.el"
        "Semantic details for C"
        nil)
    ("el.el"
        "Semantic details for Emacs Lisp"
        nil)
    ("gcc.el"
        "gcc querying special code for the C parser"
        nil)
    ("make-by.el"
        "Generated parser support file"
        nil)
    ("scm-by.el"
        "Generated parser support file"
        nil)
    ("scm.el"
        "Semantic details for Scheme (guile)"
        nil)
    ("include.el"
        "Decoration modes for include statements"
        nil)
    ("mode.el"
        "Minor mode for decorating tags"
        (syntax))
    ("cscope.el"
        "Semantic-symref support via cscope."
        nil)
    ("filter.el"
        "Filter symbol reference hits for accuracy."
        nil)
    ("global.el"
        "Use GNU Global for symbol references"
        nil)
    ("idutils.el"
        "Symref implementation for idutils"
        nil)
    ("list.el"
        "Symref Output List UI."
        nil)
    ("comp.el"
        "GNU Bison for Emacs - Grammar compiler"
        (syntax))
    ("java-tags.el"
        "Java LALR parser for Emacs"
        (syntax))
    ("javascript.el"
        "javascript parser support"
        (syntax))
    ("javat-wy.el"
        "Generated parser support file"
        nil)
    ("js-wy.el"
        "Generated parser support file"
        nil)
    ("python-wy.el"
        "Generated parser support file"
        nil)
    ("args.el"
        "Provide some simple template arguments"
        nil)
    ("dictionary.el"
        "Dictionary code for the semantic recoder."
        nil)
    ("document.el"
        "Documentation (comment) generation"
        nil)
    ("expandproto.el"
        "Expanding prototypes."
        nil)
    ("extract.el"
        "Extract content from previously inserted macro."
        nil)
    ("fields.el"
        "Handling type-in fields in a buffer."
        nil)
    ("filters.el"
        "Filters for use in template variables."
        nil)
    ("getset.el"
        "Package for inserting new get/set methods."
        nil)
    ("insert.el"
        "Insert srecode templates to an output stream."
        nil)
    ("map.el"
        "Manage a template file map"
        nil)
    ("srt-mode.el"
        "Major mode for writing screcode macros"
        nil)
    ("srt-wy.el"
        "Generated parser support file"
        nil)
    ("srt.el"
        "argument handlers for SRT files"
        nil)
    ("template.el"
        "SRecoder template language parser support."
        nil)
    ))

(provide 'finder-inf)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; finder-inf.el ends here
