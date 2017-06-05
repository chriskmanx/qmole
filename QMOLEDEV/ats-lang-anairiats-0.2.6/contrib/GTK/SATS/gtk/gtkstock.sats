(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the  terms of the  GNU General Public License as published by the Free
** Software Foundation; either version 2.1, or (at your option) any later
** version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: April, 2010
//
(* ****** ****** *)

(*
#define GTK_STOCK_DIALOG_AUTHENTICATION "gtk-dialog-authentication"
#define GTK_STOCK_DIALOG_INFO      "gtk-dialog-info"
#define GTK_STOCK_DIALOG_WARNING   "gtk-dialog-warning"
#define GTK_STOCK_DIALOG_ERROR     "gtk-dialog-error"
#define GTK_STOCK_DIALOG_QUESTION  "gtk-dialog-question"
*)

macdef GTK_STOCK_DIALOG_AUTHENTICATION =
  $extval (string, "GTK_STOCK_DIALOG_AUTHENTICATION")
macdef GTK_STOCK_DIALOG_INFO = $extval (string, "GTK_STOCK_DIALOG_INFO")
macdef GTK_STOCK_DIALOG_WARNING = $extval (string, "GTK_STOCK_DIALOG_WARNING")
macdef GTK_STOCK_DIALOG_ERROR = $extval (string, "GTK_STOCK_DIALOG_ERROR")
macdef GTK_STOCK_DIALOG_QUESTION = $extval (string, "GTK_STOCK_DIALOG_QUESTION")

(* ****** ****** *)

(*
#define GTK_STOCK_DND              "gtk-dnd"
#define GTK_STOCK_DND_MULTIPLE     "gtk-dnd-multiple"
*)

macdef GTK_STOCK_DND = $extval (string, "GTK_STOCK_DND")
macdef GTK_STOCK_DND_MULTIPLE = $extval (string, "GTK_STOCK_DND_MULTIPLE")

(* ****** ****** *)

(*
#define GTK_STOCK_ABOUT            "gtk-about"
#define GTK_STOCK_ADD              "gtk-add"
#define GTK_STOCK_APPLY            "gtk-apply"
#define GTK_STOCK_BOLD             "gtk-bold"
#define GTK_STOCK_CANCEL           "gtk-cancel"
#define GTK_STOCK_CAPS_LOCK_WARNING "gtk-caps-lock-warning"
#define GTK_STOCK_CDROM            "gtk-cdrom"
#define GTK_STOCK_CLEAR            "gtk-clear"
#define GTK_STOCK_CLOSE            "gtk-close"
#define GTK_STOCK_COLOR_PICKER     "gtk-color-picker"
#define GTK_STOCK_CONVERT          "gtk-convert"
#define GTK_STOCK_CONNECT          "gtk-connect"
#define GTK_STOCK_COPY             "gtk-copy"
#define GTK_STOCK_CUT              "gtk-cut"
#define GTK_STOCK_DELETE           "gtk-delete"
#define GTK_STOCK_DIRECTORY        "gtk-directory"
#define GTK_STOCK_DISCARD          "gtk-discard"
#define GTK_STOCK_DISCONNECT       "gtk-disconnect"
#define GTK_STOCK_EDIT             "gtk-edit"
#define GTK_STOCK_EXECUTE          "gtk-execute"
#define GTK_STOCK_FILE             "gtk-file"
#define GTK_STOCK_FIND             "gtk-find"
#define GTK_STOCK_FIND_AND_REPLACE "gtk-find-and-replace"
#define GTK_STOCK_FLOPPY           "gtk-floppy"
#define GTK_STOCK_FULLSCREEN       "gtk-fullscreen"
#define GTK_STOCK_GOTO_BOTTOM      "gtk-goto-bottom"
#define GTK_STOCK_GOTO_FIRST       "gtk-goto-first"
#define GTK_STOCK_GOTO_LAST        "gtk-goto-last"
#define GTK_STOCK_GOTO_TOP         "gtk-goto-top"
#define GTK_STOCK_GO_BACK          "gtk-go-back"
#define GTK_STOCK_GO_DOWN          "gtk-go-down"
#define GTK_STOCK_GO_FORWARD       "gtk-go-forward"
#define GTK_STOCK_GO_UP            "gtk-go-up"
#define GTK_STOCK_HARDDISK         "gtk-harddisk"
#define GTK_STOCK_HELP             "gtk-help"
#define GTK_STOCK_HOME             "gtk-home"
#define GTK_STOCK_INDEX            "gtk-index"
#define GTK_STOCK_INDENT           "gtk-indent"
#define GTK_STOCK_INFO             "gtk-info"
#define GTK_STOCK_UNINDENT         "gtk-unindent"
#define GTK_STOCK_ITALIC           "gtk-italic"
#define GTK_STOCK_JUMP_TO          "gtk-jump-to"
#define GTK_STOCK_JUSTIFY_CENTER   "gtk-justify-center"
#define GTK_STOCK_JUSTIFY_FILL     "gtk-justify-fill"
#define GTK_STOCK_JUSTIFY_LEFT     "gtk-justify-left"
#define GTK_STOCK_JUSTIFY_RIGHT    "gtk-justify-right"
#define GTK_STOCK_LEAVE_FULLSCREEN "gtk-leave-fullscreen"
#define GTK_STOCK_MISSING_IMAGE    "gtk-missing-image"
#define GTK_STOCK_MEDIA_FORWARD    "gtk-media-forward"
#define GTK_STOCK_MEDIA_NEXT       "gtk-media-next"
#define GTK_STOCK_MEDIA_PAUSE      "gtk-media-pause"
#define GTK_STOCK_MEDIA_PLAY       "gtk-media-play"
#define GTK_STOCK_MEDIA_PREVIOUS   "gtk-media-previous"
#define GTK_STOCK_MEDIA_RECORD     "gtk-media-record"
#define GTK_STOCK_MEDIA_REWIND     "gtk-media-rewind"
#define GTK_STOCK_MEDIA_STOP       "gtk-media-stop"
#define GTK_STOCK_NETWORK          "gtk-network"
#define GTK_STOCK_NEW              "gtk-new"
#define GTK_STOCK_NO               "gtk-no"
#define GTK_STOCK_OK               "gtk-ok"
#define GTK_STOCK_OPEN             "gtk-open"
#define GTK_STOCK_ORIENTATION_PORTRAIT "gtk-orientation-portrait"
#define GTK_STOCK_ORIENTATION_LANDSCAPE "gtk-orientation-landscape"
#define GTK_STOCK_ORIENTATION_REVERSE_LANDSCAPE "gtk-orientation-reverse-landscape"
#define GTK_STOCK_ORIENTATION_REVERSE_PORTRAIT "gtk-orientation-reverse-portrait"
#define GTK_STOCK_PAGE_SETUP       "gtk-page-setup"
#define GTK_STOCK_PASTE            "gtk-paste"
#define GTK_STOCK_PREFERENCES      "gtk-preferences"
#define GTK_STOCK_PRINT            "gtk-print"
#define GTK_STOCK_PRINT_ERROR      "gtk-print-error"
#define GTK_STOCK_PRINT_PAUSED     "gtk-print-paused"
#define GTK_STOCK_PRINT_PREVIEW    "gtk-print-preview"
#define GTK_STOCK_PRINT_REPORT     "gtk-print-report"
#define GTK_STOCK_PRINT_WARNING    "gtk-print-warning"
#define GTK_STOCK_PROPERTIES       "gtk-properties"
#define GTK_STOCK_QUIT             "gtk-quit"
#define GTK_STOCK_REDO             "gtk-redo"
#define GTK_STOCK_REFRESH          "gtk-refresh"
#define GTK_STOCK_REMOVE           "gtk-remove"
#define GTK_STOCK_REVERT_TO_SAVED  "gtk-revert-to-saved"
#define GTK_STOCK_SAVE             "gtk-save"
#define GTK_STOCK_SAVE_AS          "gtk-save-as"
#define GTK_STOCK_SELECT_ALL       "gtk-select-all"
#define GTK_STOCK_SELECT_COLOR     "gtk-select-color"
#define GTK_STOCK_SELECT_FONT      "gtk-select-font"
#define GTK_STOCK_SORT_ASCENDING   "gtk-sort-ascending"
#define GTK_STOCK_SORT_DESCENDING  "gtk-sort-descending"
#define GTK_STOCK_SPELL_CHECK      "gtk-spell-check"
#define GTK_STOCK_STOP             "gtk-stop"
#define GTK_STOCK_STRIKETHROUGH    "gtk-strikethrough"
#define GTK_STOCK_UNDELETE         "gtk-undelete"
#define GTK_STOCK_UNDERLINE        "gtk-underline"
#define GTK_STOCK_UNDO             "gtk-undo"
#define GTK_STOCK_YES              "gtk-yes"
#define GTK_STOCK_ZOOM_100         "gtk-zoom-100"
#define GTK_STOCK_ZOOM_FIT         "gtk-zoom-fit"
#define GTK_STOCK_ZOOM_IN          "gtk-zoom-in"
#define GTK_STOCK_ZOOM_OUT         "gtk-zoom-out"
*)

macdef GTK_STOCK_ABOUT = $extval (string, "GTK_STOCK_ABOUT")
macdef GTK_STOCK_ADD = $extval (string, "GTK_STOCK_ADD")
macdef GTK_STOCK_APPLY = $extval (string, "GTK_STOCK_APPLY")
macdef GTK_STOCK_BOLD = $extval (string, "GTK_STOCK_BOLD")
macdef GTK_STOCK_CANCEL = $extval (string, "GTK_STOCK_CANCEL")
macdef GTK_STOCK_CAPS_LOCK_WARNING =
  $extval (string, "GTK_STOCK_CAPS_LOCK_WARNING")
macdef GTK_STOCK_CDROM = $extval (string, "GTK_STOCK_CDROM")
macdef GTK_STOCK_CLEAR = $extval (string, "GTK_STOCK_CLEAR")
macdef GTK_STOCK_CLOSE = $extval (string, "GTK_STOCK_CLOSE")
macdef GTK_STOCK_COLOR_PICKER = $extval (string, "GTK_STOCK_COLOR_PICKER")
macdef GTK_STOCK_CONVERT = $extval (string, "GTK_STOCK_CONVERT")
macdef GTK_STOCK_CONNECT = $extval (string, "GTK_STOCK_CONNECT")
macdef GTK_STOCK_COPY = $extval (string, "GTK_STOCK_COPY")
macdef GTK_STOCK_CUT = $extval (string, "GTK_STOCK_CUT")
macdef GTK_STOCK_DELETE = $extval (string, "GTK_STOCK_DELETE")
macdef GTK_STOCK_DIRECTORY = $extval (string, "GTK_STOCK_DIRECTORY")
macdef GTK_STOCK_DISCARD = $extval (string, "GTK_STOCK_DISCARD")
macdef GTK_STOCK_DISCONNECT = $extval (string, "GTK_STOCK_DISCONNECT")
macdef GTK_STOCK_EDIT = $extval (string, "GTK_STOCK_EDIT")
macdef GTK_STOCK_EXECUTE = $extval (string, "GTK_STOCK_EXECUTE")
macdef GTK_STOCK_FILE = $extval (string, "GTK_STOCK_FILE")
macdef GTK_STOCK_FIND = $extval (string, "GTK_STOCK_FIND")
macdef GTK_STOCK_FIND_AND_REPLACE =
  $extval (string, "GTK_STOCK_FIND_AND_REPLACE")
macdef GTK_STOCK_FLOPPY = $extval (string, "GTK_STOCK_FLOPPY")
macdef GTK_STOCK_FULLSCREEN = $extval (string, "GTK_STOCK_FULLSCREEN")
macdef GTK_STOCK_GOTO_BOTTOM = $extval (string, "GTK_STOCK_GOTO_BOTTOM")
macdef GTK_STOCK_GOTO_FIRST = $extval (string, "GTK_STOCK_GOTO_FIRST")
macdef GTK_STOCK_GOTO_LAST = $extval (string, "GTK_STOCK_GOTO_LAST")
macdef GTK_STOCK_GOTO_TOP = $extval (string, "GTK_STOCK_GOTO_TOP")
macdef GTK_STOCK_GO_BACK = $extval (string, "GTK_STOCK_GO_BACK")
macdef GTK_STOCK_GO_DOWN = $extval (string, "GTK_STOCK_GO_DOWN")
macdef GTK_STOCK_GO_FORWARD = $extval (string, "GTK_STOCK_GO_FORWARD")
macdef GTK_STOCK_GO_UP = $extval (string, "GTK_STOCK_GO_UP")
macdef GTK_STOCK_HARDDISK = $extval (string, "GTK_STOCK_HARDDISK")
macdef GTK_STOCK_HELP = $extval (string, "GTK_STOCK_HELP")
macdef GTK_STOCK_HOME = $extval (string, "GTK_STOCK_HOME")
macdef GTK_STOCK_INDEX = $extval (string, "GTK_STOCK_INDEX")
macdef GTK_STOCK_INDENT = $extval (string, "GTK_STOCK_INDENT")
macdef GTK_STOCK_INFO = $extval (string, "GTK_STOCK_INFO")
macdef GTK_STOCK_UNINDENT = $extval (string, "GTK_STOCK_UNINDENT")
macdef GTK_STOCK_ITALIC = $extval (string, "GTK_STOCK_ITALIC")
macdef GTK_STOCK_JUMP_TO = $extval (string, "GTK_STOCK_JUMP_TO")
macdef GTK_STOCK_JUSTIFY_CENTER = $extval (string, "GTK_STOCK_JUSTIFY_CENTER")
macdef GTK_STOCK_JUSTIFY_FILL = $extval (string, "GTK_STOCK_JUSTIFY_FILL")
macdef GTK_STOCK_JUSTIFY_LEFT = $extval (string, "GTK_STOCK_JUSTIFY_LEFT")
macdef GTK_STOCK_JUSTIFY_RIGHT = $extval (string, "GTK_STOCK_JUSTIFY_RIGHT")
macdef GTK_STOCK_LEAVE_FULLSCREEN = $extval (string, "GTK_STOCK_LEAVE_FULLSCREEN")
macdef GTK_STOCK_MISSING_IMAGE = $extval (string, "GTK_STOCK_MISSING_IMAGE")
macdef GTK_STOCK_MEDIA_FORWARD = $extval (string, "GTK_STOCK_MEDIA_FORWARD")
macdef GTK_STOCK_MEDIA_NEXT = $extval (string, "GTK_STOCK_MEDIA_NEXT")
macdef GTK_STOCK_MEDIA_PAUSE = $extval (string, "GTK_STOCK_MEDIA_PAUSE")
macdef GTK_STOCK_MEDIA_PLAY = $extval (string, "GTK_STOCK_MEDIA_PLAY")
macdef GTK_STOCK_MEDIA_PREVIOUS = $extval (string, "GTK_STOCK_MEDIA_PREVIOUS")
macdef GTK_STOCK_MEDIA_RECORD = $extval (string, "GTK_STOCK_MEDIA_RECORD")
macdef GTK_STOCK_MEDIA_REWIND = $extval (string, "GTK_STOCK_MEDIA_REWIND")
macdef GTK_STOCK_MEDIA_STOP = $extval (string, "GTK_STOCK_MEDIA_STOP")
macdef GTK_STOCK_NETWORK = $extval (string, "GTK_STOCK_NETWORK")
macdef GTK_STOCK_NEW = $extval (string, "GTK_STOCK_NEW")
macdef GTK_STOCK_NO = $extval (string, "GTK_STOCK_NO")
macdef GTK_STOCK_OK = $extval (string, "GTK_STOCK_OK")
macdef GTK_STOCK_OPEN = $extval (string, "GTK_STOCK_OPEN")
macdef GTK_STOCK_ORIENTATION_PORTRAIT =
  $extval (string, "GTK_STOCK_ORIENTATION_PORTRAIT")
macdef GTK_STOCK_ORIENTATION_LANDSCAPE =
  $extval (string, "GTK_STOCK_ORIENTATION_LANDSCAPE")
macdef GTK_STOCK_ORIENTATION_REVERSE_LANDSCAPE =
  $extval (string, "GTK_STOCK_ORIENTATION_REVERSE_LANDSCAPE")
macdef GTK_STOCK_ORIENTATION_REVERSE_PORTRAIT =
  $extval (string, "GTK_STOCK_ORIENTATION_REVERSE_PORTRAIT")
macdef GTK_STOCK_PAGE_SETUP = $extval (string, "GTK_STOCK_PAGE_SETUP")
macdef GTK_STOCK_PASTE = $extval (string, "GTK_STOCK_PASTE")
macdef GTK_STOCK_PREFERENCES = $extval (string, "GTK_STOCK_PREFERENCES")
macdef GTK_STOCK_PRINT = $extval (string, "GTK_STOCK_PRINT")
macdef GTK_STOCK_PRINT_ERROR = $extval (string, "GTK_STOCK_PRINT_ERROR")
macdef GTK_STOCK_PRINT_PAUSED = $extval (string, "GTK_STOCK_PRINT_PAUSED")
macdef GTK_STOCK_PRINT_PREVIEW = $extval (string, "GTK_STOCK_PRINT_PREVIEW")
macdef GTK_STOCK_PRINT_REPORT = $extval (string, "GTK_STOCK_PRINT_REPORT")
macdef GTK_STOCK_PRINT_WARNING = $extval (string, "GTK_STOCK_PRINT_WARNING")
macdef GTK_STOCK_PROPERTIES = $extval (string, "GTK_STOCK_PROPERTIES")
macdef GTK_STOCK_QUIT = $extval (string, "GTK_STOCK_QUIT")
macdef GTK_STOCK_REDO = $extval (string, "GTK_STOCK_REDO")
macdef GTK_STOCK_REFRESH = $extval (string, "GTK_STOCK_REFRESH")
macdef GTK_STOCK_REMOVE = $extval (string, "GTK_STOCK_REMOVE")
macdef GTK_STOCK_REVERT_TO_SAVED = $extval (string, "GTK_STOCK_REVERT_TO_SAVED")
macdef GTK_STOCK_SAVE = $extval (string, "GTK_STOCK_SAVE")
macdef GTK_STOCK_SAVE_AS = $extval (string, "GTK_STOCK_SAVE_AS")
macdef GTK_STOCK_SELECT_ALL = $extval (string, "GTK_STOCK_SELECT_ALL")
macdef GTK_STOCK_SELECT_COLOR = $extval (string, "GTK_STOCK_SELECT_COLOR")
macdef GTK_STOCK_SELECT_FONT = $extval (string, "GTK_STOCK_SELECT_FONT")
macdef GTK_STOCK_SORT_ASCENDING = $extval (string, "GTK_STOCK_SORT_ASCENDING")
macdef GTK_STOCK_SORT_DESCENDING = $extval (string, "GTK_STOCK_SORT_DESCENDING")
macdef GTK_STOCK_SPELL_CHECK = $extval (string, "GTK_STOCK_SPELL_CHECK")
macdef GTK_STOCK_STOP = $extval (string, "GTK_STOCK_STOP")
macdef GTK_STOCK_STRIKETHROUGH = $extval (string, "GTK_STOCK_STRIKETHROUGH")
macdef GTK_STOCK_UNDELETE = $extval (string, "GTK_STOCK_UNDELETE")
macdef GTK_STOCK_UNDERLINE = $extval (string, "GTK_STOCK_UNDERLINE")
macdef GTK_STOCK_UNDO = $extval (string, "GTK_STOCK_UNDO")
macdef GTK_STOCK_YES = $extval (string, "GTK_STOCK_YES")
macdef GTK_STOCK_ZOOM_100 = $extval (string, "GTK_STOCK_ZOOM_100")
macdef GTK_STOCK_ZOOM_FIT = $extval (string, "GTK_STOCK_ZOOM_FIT")
macdef GTK_STOCK_ZOOM_IN = $extval (string, "GTK_STOCK_ZOOM_IN")
macdef GTK_STOCK_ZOOM_OUT = $extval (string, "GTK_STOCK_ZOOM_OUT")

(* ****** ****** *)

(* end of [gtkstock.sats] *)
