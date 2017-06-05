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
//
// HX:
//
// Some naming conventions used in this API for GTK
//
// get0 : get out a value that must be returned
// get1 : get out a value by increasing its reference count
// getref : get out a reference that must be returned
//
// set0 : consume-set; the original value is freed if necessary
// set1 : preserve-set; the original value is freed if necessary
//
// takeout : this is the same as get0
//
(* ****** ****** *)

%{#
#include "contrib/GTK/CATS/gtk.cats"
%} // end of [%{#]

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no need for staloading at run-time

(* ****** ****** *)

staload GLIB =
  "contrib/glib/SATS/glib.sats"
//
stadef gboolean = $GLIB.gboolean
//
stadef gchar = $GLIB.gchar
//
stadef gint (i:int) = $GLIB.gint (i)
stadef gint = $GLIB.gint
//
stadef guint (i:int) = $GLIB.guint (i)
stadef guint = $GLIB.guint
//
stadef gfloat = $GLIB.gfloat
stadef gdouble = $GLIB.gdouble
//
stadef gpointer = $GLIB.gpointer
//
stadef gstring = $GLIB.gstring
stadef gstring0 = $GLIB.gstring0
stadef gstring1 = $GLIB.gstring1
//
stadef GSList_ptr = $GLIB.GSList_ptr
stadef GSList_ptr0 = $GLIB.GSList_ptr0
stadef GSList_ptr1 = $GLIB.GSList_ptr1
//
(* ****** ****** *)

staload GOBJ =
  "contrib/glib/SATS/glib-object.sats"
//
stadef gobjref = $GOBJ.gobjref
stadef gsignal = $GOBJ.gsignal
//
(* ****** ****** *)

staload "contrib/pango/SATS/pango.sats"

(* ****** ****** *)

staload "contrib/GTK/SATS/gdk.sats"

(* ****** ****** *)

staload "contrib/GTK/SATS/gdkclassdec.sats"
staload "contrib/GTK/SATS/gtkclassdec.sats"

(* ****** ****** *)

viewtypedef GtkAdjustment_ref (l:addr) = gobjref (GtkAdjustment, l)
viewtypedef GtkAdjustment_ref0 = [l:agez] GtkAdjustment_ref l
viewtypedef GtkAdjustment_ref1 = [l:addr | l > null] GtkAdjustment_ref l

viewtypedef GtkAlignment_ref (l:addr) = gobjref (GtkAlignment, l)
viewtypedef GtkAlignment_ref0 = [l:agez] GtkAlignment_ref l
viewtypedef GtkAlignment_ref1 = [l:addr | l > null] GtkAlignment_ref l

viewtypedef GtkArrow_ref (l:addr) = gobjref (GtkArrow, l)
viewtypedef GtkArrow_ref0 = [l:agez] GtkArrow_ref l
viewtypedef GtkArrow_ref1 = [l:addr | l > null] GtkArrow_ref l

viewtypedef GtkButton_ref (l:addr) = gobjref (GtkButton, l)
viewtypedef GtkButton_ref0 = [l:agez] GtkButton_ref l
viewtypedef GtkButton_ref1 = [l:addr | l > null] GtkButton_ref l

viewtypedef GtkCheckButton_ref (l:addr) = gobjref (GtkCheckButton, l)
viewtypedef GtkCheckButton_ref0 = [l:agez] GtkCheckButton_ref l
viewtypedef GtkCheckButton_ref1 = [l:addr | l > null] GtkCheckButton_ref l

viewtypedef GtkCheckMenuItem_ref (l:addr) = gobjref (GtkCheckMenuItem, l)
viewtypedef GtkCheckMenuItem_ref0 = [l:agez] GtkCheckMenuItem_ref l
viewtypedef GtkCheckMenuItem_ref1 = [l:addr | l > null] GtkCheckMenuItem_ref l

viewtypedef GtkClipBoard_ref (l:addr) = gobjref (GtkClipBoard, l)
viewtypedef GtkClipBoard_ref0 = [l:agez] GtkClipBoard_ref l
viewtypedef GtkClipBoard_ref1 = [l:addr | l > null] GtkClipBoard_ref l

viewtypedef GtkColorSelection_ref (l:addr) = gobjref (GtkColorSelection, l)
viewtypedef GtkColorSelection_ref0 = [l:agez] GtkColorSelection_ref l
viewtypedef GtkColorSelection_ref1 = [l:addr | l > null] GtkColorSelection_ref l

viewtypedef GtkColorSelectionDialog_ref (l:addr) = gobjref (GtkColorSelectionDialog, l)
viewtypedef GtkColorSelectionDialog_ref0 = [l:agez] GtkColorSelectionDialog_ref l
viewtypedef GtkColorSelectionDialog_ref1 = [l:addr | l > null] GtkColorSelectionDialog_ref l

viewtypedef GtkDialog_ref (l:addr) = gobjref (GtkDialog, l)
viewtypedef GtkDialog_ref0 = [l:agez] GtkDialog_ref l
viewtypedef GtkDialog_ref1 = [l:addr | l > null] GtkDialog_ref l

viewtypedef GtkDrawingArea_ref (l:addr) = gobjref (GtkDrawingArea, l)
viewtypedef GtkDrawingArea_ref0 = [l:agez] GtkDrawingArea_ref l
viewtypedef GtkDrawingArea_ref1 = [l:addr | l > null] GtkDrawingArea_ref l

viewtypedef GtkEntry_ref (l:addr) = gobjref (GtkEntry, l)
viewtypedef GtkEntry_ref0 = [l:agez] GtkEntry_ref l
viewtypedef GtkEntry_ref1 = [l:addr | l > null] GtkEntry_ref l

viewtypedef GtkFileSelection_ref (l:addr) = gobjref (GtkFileSelection, l)
viewtypedef GtkFileSelection_ref0 = [l:agez] GtkFileSelection_ref l
viewtypedef GtkFileSelection_ref1 = [l:addr | l > null] GtkFileSelection_ref l

viewtypedef GtkFileChooserDialog_ref (l:addr) = gobjref (GtkFileChooserDialog, l)
viewtypedef GtkFileChooserDialog_ref0 = [l:agez] GtkFileChooserDialog_ref l
viewtypedef GtkFileChooserDialog_ref1 = [l:addr | l > null] GtkFileChooserDialog_ref l

viewtypedef GtkFontSelection_ref (l:addr) = gobjref (GtkFontSelection, l)
viewtypedef GtkFontSelection_ref0 = [l:agez] GtkFontSelection_ref l
viewtypedef GtkFontSelection_ref1 = [l:addr | l > null] GtkFontSelection_ref l

viewtypedef GtkFontSelectionDialog_ref (l:addr) = gobjref (GtkFontSelectionDialog, l)
viewtypedef GtkFontSelectionDialog_ref0 = [l:agez] GtkFontSelectionDialog_ref l
viewtypedef GtkFontSelectionDialog_ref1 = [l:addr | l > null] GtkFontSelectionDialog_ref l

viewtypedef GtkFrame_ref (l:addr) = gobjref (GtkFrame, l)
viewtypedef GtkFrame_ref0 = [l:agez] GtkFrame_ref l
viewtypedef GtkFrame_ref1 = [l:addr | l > null] GtkFrame_ref l

viewtypedef GtkHBox_ref (l:addr) = gobjref (GtkHBox, l)
viewtypedef GtkHBox_ref0 = [l:agez] GtkHBox_ref l
viewtypedef GtkHBox_ref1 = [l:addr | l > null] GtkHBox_ref l

viewtypedef GtkHPaned_ref (l:addr) = gobjref (GtkHPaned, l)
viewtypedef GtkHPaned_ref0 = [l:agez] GtkHPaned_ref l
viewtypedef GtkHPaned_ref1 = [l:addr | l > null] GtkHPaned_ref l

viewtypedef GtkHRuler_ref (l:addr) = gobjref (GtkHRuler, l)
viewtypedef GtkHRuler_ref0 = [l:agez] GtkHRuler_ref l
viewtypedef GtkHRuler_ref1 = [l:addr | l > null] GtkHRuler_ref l

viewtypedef GtkHScale_ref (l:addr) = gobjref (GtkHScale, l)
viewtypedef GtkHScale_ref0 = [l:agez] GtkHScale_ref l
viewtypedef GtkHScale_ref1 = [l:addr | l > null] GtkHScale_ref l

viewtypedef GtkHScrollbar_ref (l:addr) = gobjref (GtkHScrollbar, l)
viewtypedef GtkHScrollbar_ref0 = [l:agez] GtkHScrollbar_ref l
viewtypedef GtkHScrollbar_ref1 = [l:addr | l > null] GtkHScrollbar_ref l

viewtypedef GtkHSeparator_ref (l:addr) = gobjref (GtkHSeparator, l)
viewtypedef GtkHSeparator_ref0 = [l:agez] GtkHSeparator_ref l
viewtypedef GtkHSeparator_ref1 = [l:addr | l > null] GtkHSeparator_ref l

viewtypedef GtkImageMenuItem_ref (l:addr) = gobjref (GtkImageMenuItem, l)
viewtypedef GtkImageMenuItem_ref0 = [l:agez] GtkImageMenuItem_ref l
viewtypedef GtkImageMenuItem_ref1 = [l:addr | l > null] GtkImageMenuItem_ref l

viewtypedef GtkLabel_ref (l:addr) = gobjref (GtkLabel, l)
viewtypedef GtkLabel_ref0 = [l:agez] GtkLabel_ref l
viewtypedef GtkLabel_ref1 = [l:addr | l > null] GtkLabel_ref l

viewtypedef GtkMenu_ref (l:addr) = gobjref (GtkMenu, l)
viewtypedef GtkMenu_ref0 = [l:agez] GtkMenu_ref l
viewtypedef GtkMenu_ref1 = [l:addr | l > null] GtkMenu_ref l

viewtypedef GtkMenuBar_ref (l:addr) = gobjref (GtkMenuBar, l)
viewtypedef GtkMenuBar_ref0 = [l:agez] GtkMenuBar_ref l
viewtypedef GtkMenuBar_ref1 = [l:addr | l > null] GtkMenuBar_ref l

viewtypedef GtkMenuItem_ref (l:addr) = gobjref (GtkMenuItem, l)
viewtypedef GtkMenuItem_ref0 = [l:agez] GtkMenuItem_ref l
viewtypedef GtkMenuItem_ref1 = [l:addr | l > null] GtkMenuItem_ref l

viewtypedef GtkMenuShell_ref (l:addr) = gobjref (GtkMenuShell, l)
viewtypedef GtkMenuShell_ref0 = [l:agez] GtkMenuShell_ref l
viewtypedef GtkMenuShell_ref1 = [l:addr | l > null] GtkMenuShell_ref l

viewtypedef GtkMessageDialog_ref (l:addr) = gobjref (GtkMessageDialog, l)
viewtypedef GtkMessageDialog_ref0 = [l:agez] GtkMessageDialog_ref l
viewtypedef GtkMessageDialog_ref1 = [l:addr | l > null] GtkMessageDialog_ref l

viewtypedef GtkOptionMenu_ref (l:addr) = gobjref (GtkOptionMenu, l)
viewtypedef GtkOptionMenu_ref0 = [l:agez] GtkOptionMenu_ref l
viewtypedef GtkOptionMenu_ref1 = [l:addr | l > null] GtkOptionMenu_ref l

viewtypedef GtkProgressBar_ref (l:addr) = gobjref (GtkProgressBar, l)
viewtypedef GtkProgressBar_ref0 = [l:agez] GtkProgressBar_ref l
viewtypedef GtkProgressBar_ref1 = [l:addr | l > null] GtkProgressBar_ref l

viewtypedef GtkRadioButton_ref (l:addr) = gobjref (GtkRadioButton, l)
viewtypedef GtkRadioButton_ref0 = [l:agez] GtkRadioButton_ref l
viewtypedef GtkRadioButton_ref1 = [l:addr | l > null] GtkRadioButton_ref l

viewtypedef GtkScrolledWindow_ref (l:addr) = gobjref (GtkScrolledWindow, l)
viewtypedef GtkScrolledWindow_ref0 = [l:agez] GtkScrolledWindow_ref l
viewtypedef GtkScrolledWindow_ref1 = [l:addr | l > null] GtkScrolledWindow_ref l

viewtypedef GtkSeparatorMenuItem_ref (l:addr) = gobjref (GtkSeparatorMenuItem, l)
viewtypedef GtkSeparatorMenuItem_ref0 = [l:agez] GtkSeparatorMenuItem_ref l
viewtypedef GtkSeparatorMenuItem_ref1 = [l:addr | l > null] GtkSeparatorMenuItem_ref l

viewtypedef GtkSeparatorToolItem_ref (l:addr) = gobjref (GtkSeparatorToolItem, l)
viewtypedef GtkSeparatorToolItem_ref0 = [l:agez] GtkSeparatorToolItem_ref l
viewtypedef GtkSeparatorToolItem_ref1 = [l:addr | l > null] GtkSeparatorToolItem_ref l

viewtypedef GtkSpinButton_ref (l:addr) = gobjref (GtkSpinButton, l)
viewtypedef GtkSpinButton_ref0 = [l:agez] GtkSpinButton_ref l
viewtypedef GtkSpinButton_ref1 = [l:addr | l > null] GtkSpinButton_ref l

viewtypedef GtkStatusbar_ref (l:addr) = gobjref (GtkStatusbar, l)
viewtypedef GtkStatusbar_ref0 = [l:agez] GtkStatusbar_ref l
viewtypedef GtkStatusbar_ref1 = [l:addr | l > null] GtkStatusbar_ref l

viewtypedef GtkTable_ref (l:addr) = gobjref (GtkTable, l)
viewtypedef GtkTable_ref0 = [l:agez] GtkTable_ref l
viewtypedef GtkTable_ref1 = [l:addr | l > null] GtkTable_ref l

viewtypedef GtkTextView_ref (l:addr) = gobjref (GtkTextView, l)
viewtypedef GtkTextView_ref0 = [l:agez] GtkTextView_ref l
viewtypedef GtkTextView_ref1 = [l:addr | l > null] GtkTextView_ref l

viewtypedef GtkToggleButton_ref (l:addr) = gobjref (GtkToggleButton, l)
viewtypedef GtkToggleButton_ref0 = [l:agez] GtkToggleButton_ref l
viewtypedef GtkToggleButton_ref1 = [l:addr | l > null] GtkToggleButton_ref l

viewtypedef GtkToolbar_ref (l:addr) = gobjref (GtkToolbar, l)
viewtypedef GtkToolbar_ref0 = [l:agez] GtkToolbar_ref l
viewtypedef GtkToolbar_ref1 = [l:addr | l > null] GtkToolbar_ref l

viewtypedef GtkToolButton_ref (l:addr) = gobjref (GtkToolButton, l)
viewtypedef GtkToolButton_ref0 = [l:agez] GtkToolButton_ref l
viewtypedef GtkToolButton_ref1 = [l:addr | l > null] GtkToolButton_ref l

viewtypedef GtkVBox_ref (l:addr) = gobjref (GtkVBox, l)
viewtypedef GtkVBox_ref0 = [l:agez] GtkVBox_ref l
viewtypedef GtkVBox_ref1 = [l:addr | l > null] GtkVBox_ref l

viewtypedef GtkVPaned_ref (l:addr) = gobjref (GtkVPaned, l)
viewtypedef GtkVPaned_ref0 = [l:agez] GtkVPaned_ref l
viewtypedef GtkVPaned_ref1 = [l:addr | l > null] GtkVPaned_ref l

viewtypedef GtkVRuler_ref (l:addr) = gobjref (GtkVRuler, l)
viewtypedef GtkVRuler_ref0 = [l:agez] GtkVRuler_ref l
viewtypedef GtkVRuler_ref1 = [l:addr | l > null] GtkVRuler_ref l

viewtypedef GtkVScale_ref (l:addr) = gobjref (GtkVScale, l)
viewtypedef GtkVScale_ref0 = [l:agez] GtkVScale_ref l
viewtypedef GtkVScale_ref1 = [l:addr | l > null] GtkVScale_ref l

viewtypedef GtkVScrollbar_ref (l:addr) = gobjref (GtkVScrollbar, l)
viewtypedef GtkVScrollbar_ref0 = [l:agez] GtkVScrollbar_ref l
viewtypedef GtkVScrollbar_ref1 = [l:addr | l > null] GtkVScrollbar_ref l

viewtypedef GtkVSeparator_ref (l:addr) = gobjref (GtkVSeparator, l)
viewtypedef GtkVSeparator_ref0 = [l:agez] GtkVSeparator_ref l
viewtypedef GtkVSeparator_ref1 = [l:addr | l > null] GtkVSeparator_ref l

//
// viewtypedef GtkWidget_ref (l:addr) = gobjref (GtkWidget, l)
//

viewtypedef GtkWindow_ref (l:addr) = gobjref (GtkWindow, l)
viewtypedef GtkWindow_ref0 = [l:agez] GtkWindow_ref l
viewtypedef GtkWindow_ref1 = [l:addr | l > null] GtkWindow_ref l

(* ****** ****** *)

viewtypedef GtkAccelGroup_ref (l:addr) = gobjref (GtkAccelGroup, l)
viewtypedef GtkAccelGroup_ref0 = [l:agez] GtkAccelGroup_ref l
viewtypedef GtkAccelGroup_ref1 = [l:addr | l > null] GtkAccelGroup_ref l

(* ****** ****** *)

viewtypedef GtkFileChooser_ref (l:addr) = gobjref (GtkFileChooser, l)
viewtypedef GtkFileChooser_ref0 = [l:agez] GtkFileChooser_ref l
viewtypedef GtkFileChooser_ref1 = [l:addr | l > null] GtkFileChooser_ref l

(* ****** ****** *)

viewtypedef GtkStyle_ref (l:addr) = gobjref (GtkStyle, l)
viewtypedef GtkStyle_ref0 = [l:agez] GtkStyle_ref l
viewtypedef GtkStyle_ref1 = [l:addr | l > null] GtkStyle_ref l

(* ****** ****** *)

viewtypedef GtkTextBuffer_ref (l:addr) = gobjref (GtkTextBuffer, l)
viewtypedef GtkTextBuffer_ref0 = [l:agez] GtkTextBuffer_ref l
viewtypedef GtkTextBuffer_ref1 = [l:addr | l > null] GtkTextBuffer_ref l

viewtypedef GtkTextTag_ref (l:addr) = gobjref (GtkTextTag, l)
viewtypedef GtkTextTag_ref0 = [l:agez] GtkTextTag_ref l
viewtypedef GtkTextTag_ref1 = [l:addr | l > null] GtkTextTag_ref l

viewtypedef GtkTextTagTable_ref (l:addr) = gobjref (GtkTextTagTable, l)
viewtypedef GtkTextTagTable_ref0 = [l:agez] GtkTextTagTable_ref l
viewtypedef GtkTextTagTable_ref1 = [l:addr | l > null] GtkTextTagTable_ref l

abst@ype GtkTextIter = $extype"GtkTextIter" // opaque

viewtypedef GtkTextMark_ref (l:addr) = gobjref (GtkTextMark, l)
viewtypedef GtkTextMark_ref0 = [l:agez] GtkTextMark_ref l
viewtypedef GtkTextMark_ref1 = [l:addr | l > null] GtkTextMark_ref l

(* ****** ****** *)

#include "contrib/GTK/SATS/gtk/gtkenums.sats"
#include "contrib/GTK/SATS/gtk/gtkstock.sats"
#include "contrib/GTK/SATS/gtk/gtktypeutils.sats"

(* ****** ****** *)

#include "contrib/GTK/SATS/gtk/gtkaccelgroup.sats"

(* ****** ****** *)

#include "contrib/GTK/SATS/gtk/gtkstyle.sats"

(* ****** ****** *)

#include "contrib/GTK/SATS/gtk/gtktextbuffer.sats"
#include "contrib/GTK/SATS/gtk/gtktextiter.sats"
#include "contrib/GTK/SATS/gtk/gtktextmark.sats"
#include "contrib/GTK/SATS/gtk/gtktexttag.sats"
#include "contrib/GTK/SATS/gtk/gtktexttagtable.sats"

(* ****** ****** *)

#include "contrib/GTK/SATS/gtk/gtkfilechooser.sats" // GInterface

(* ****** ****** *)

#include "contrib/GTK/SATS/gtk/gtkadjustment.sats"
#include "contrib/GTK/SATS/gtk/gtkalignment.sats"
#include "contrib/GTK/SATS/gtk/gtkarrow.sats"
#include "contrib/GTK/SATS/gtk/gtkbox.sats"
#include "contrib/GTK/SATS/gtk/gtkbutton.sats"
#include "contrib/GTK/SATS/gtk/gtkcheckbutton.sats"
#include "contrib/GTK/SATS/gtk/gtkcheckmenuitem.sats"
#include "contrib/GTK/SATS/gtk/gtkclipboard.sats"
#include "contrib/GTK/SATS/gtk/gtkcontainer.sats"
#include "contrib/GTK/SATS/gtk/gtkcolorsel.sats"
#include "contrib/GTK/SATS/gtk/gtkcolorseldialog.sats"
#include "contrib/GTK/SATS/gtk/gtkdialog.sats"
#include "contrib/GTK/SATS/gtk/gtkdrawingarea.sats"
#include "contrib/GTK/SATS/gtk/gtkentry.sats"
#include "contrib/GTK/SATS/gtk/gtkfilesel.sats" // DEPRECATED!!!
#include "contrib/GTK/SATS/gtk/gtkfilechooserdialog.sats"
#include "contrib/GTK/SATS/gtk/gtkfontsel.sats"
#include "contrib/GTK/SATS/gtk/gtkframe.sats"
#include "contrib/GTK/SATS/gtk/gtkhbox.sats"
#include "contrib/GTK/SATS/gtk/gtkhpaned.sats"
#include "contrib/GTK/SATS/gtk/gtkhruler.sats"
#include "contrib/GTK/SATS/gtk/gtkhscale.sats"
#include "contrib/GTK/SATS/gtk/gtkhscrollbar.sats"
#include "contrib/GTK/SATS/gtk/gtkhseparator.sats"
#include "contrib/GTK/SATS/gtk/gtkimagemenuitem.sats"
#include "contrib/GTK/SATS/gtk/gtklabel.sats"
#include "contrib/GTK/SATS/gtk/gtkmenu.sats"
#include "contrib/GTK/SATS/gtk/gtkmenubar.sats"
#include "contrib/GTK/SATS/gtk/gtkmenuitem.sats"
#include "contrib/GTK/SATS/gtk/gtkmenushell.sats"
#include "contrib/GTK/SATS/gtk/gtkmessagedialog.sats"
#include "contrib/GTK/SATS/gtk/gtkmisc.sats"
#include "contrib/GTK/SATS/gtk/gtkoptionmenu.sats"
#include "contrib/GTK/SATS/gtk/gtkpaned.sats"
#include "contrib/GTK/SATS/gtk/gtkprogress.sats" // DEPRECATED!!!
#include "contrib/GTK/SATS/gtk/gtkprogressbar.sats"
#include "contrib/GTK/SATS/gtk/gtkradiobutton.sats"
#include "contrib/GTK/SATS/gtk/gtkrange.sats"
#include "contrib/GTK/SATS/gtk/gtkruler.sats"
#include "contrib/GTK/SATS/gtk/gtkscale.sats"
#include "contrib/GTK/SATS/gtk/gtkscrollbar.sats"
#include "contrib/GTK/SATS/gtk/gtkscrolledwindow.sats"
#include "contrib/GTK/SATS/gtk/gtkseparator.sats"
#include "contrib/GTK/SATS/gtk/gtkseparatormenuitem.sats" // <= menuitem
#include "contrib/GTK/SATS/gtk/gtkseparatortoolitem.sats"
#include "contrib/GTK/SATS/gtk/gtkspinbutton.sats" // <= entry
#include "contrib/GTK/SATS/gtk/gtkstatusbar.sats"
#include "contrib/GTK/SATS/gtk/gtktable.sats" // <= container
#include "contrib/GTK/SATS/gtk/gtktextview.sats" // <= container
#include "contrib/GTK/SATS/gtk/gtktogglebutton.sats"
#include "contrib/GTK/SATS/gtk/gtktoolbar.sats"
#include "contrib/GTK/SATS/gtk/gtktoolbutton.sats"
#include "contrib/GTK/SATS/gtk/gtkvbox.sats"
#include "contrib/GTK/SATS/gtk/gtkvpaned.sats"
#include "contrib/GTK/SATS/gtk/gtkvruler.sats"
#include "contrib/GTK/SATS/gtk/gtkvscale.sats"
#include "contrib/GTK/SATS/gtk/gtkvscrollbar.sats"
#include "contrib/GTK/SATS/gtk/gtkvseparator.sats"
#include "contrib/GTK/SATS/gtk/gtkwidget.sats"
#include "contrib/GTK/SATS/gtk/gtkwindow.sats"

(* ****** ****** *)

#include "contrib/GTK/SATS/gtk/gtkmain.sats"

(* ****** ****** *)

(* end of [gtk.sats] *)
