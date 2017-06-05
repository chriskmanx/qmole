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
// Start Time: April, 2010
//
(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no need for staloading at run-time

(* ****** ****** *)

staload GOBJ = "contrib/glib/SATS/glib-object.sats"
stadef GObject = $GOBJ.GObject
stadef GInitiallyUnowned = $GOBJ.GInitiallyUnowned
stadef GInterface = $GOBJ.GInterface

(* ****** ****** *)

classdec GtkAccelGroup : GObject
classdec GtkClipBoard : GObject
classdec GtkFileChooser : GInterface

(* ****** ****** *)
//
// class hierarchy for GTK
//
classdec GtkObject : GInitiallyUnowned
  classdec GtkWidget : GtkObject
    classdec GtkMisc : GtkWidget
      classdec GtkLabel : GtkMisc
      classdec GtkArrow : GtkMisc
      classdec GtkImage : GtkMisc
      classdec GtkPixmap : GtkMisc
    // end of [GtkMisc]
    classdec GtkContainer : GtkWidget
      classdec GtkBin : GtkContainer
        classdec GtkAlignment : GtkBin
        classdec GtkFrame : GtkBin
          classdec GtkAspectFrame : GtkFrame
        // end of [GtkGrame]
        classdec GtkButton : GtkBin
          classdec GtkToggleButton : GtkButton
            classdec GtkCheckButton : GtkToggleButton
              classdec GtkRadioButton : GtkCheckButton
            // end of [GtkCheckButton]
          // end of [GtkToggleButton]
          classdec GtkOptionMenu : GtkButton
        // end of [GtkButton]
        classdec GtkItem : GtkBin
          classdec GtkMenuItem : GtkItem
            classdec GtkCheckMenuItem : GtkMenuItem
              classdec GtkRadioMenuItem : GtkCheckMenuItem
            // end of [GtkCheckMenuItem]
            classdec GtkImageMenuItem : GtkMenuItem
            classdec GtkSeparatorMenuItem : GtkMenuItem
            classdec GtkTearoffMenuItem : GtkMenuItem
          // end of [GtkMenuItem]
          // classdec GtkListItem : GtkItem // deprecated since GTK+-2.0
          // classdec GtkTreeItem : GtkItem // deprecated since GTK+-2.0
        // end of [GtkItem]
        classdec GtkToolItem : GtkBin
          classdec GtkToolButton : GtkToolItem
          classdec GtkSeparatorToolItem : GtkToolItem
        // end of [GtkToolItem]
        classdec GtkWindow : GtkBin
          classdec GtkDialog : GtkWindow
            classdec GtkColorSelectionDialog : GtkDialog
            classdec GtkFileSelection : GtkDialog // DEPRECATED!
            classdec GtkFileChooserDialog : GtkDialog
            classdec GtkFontSelectionDialog : GtkDialog
            classdec GtkInputDialog : GtkDialog
            classdec GtkMessageDialog : GtkDialog
          // end of [GtkDialog]
          classdec GtkPlug : GtkWindow
        // end of [GtkWindow]
        classdec GtkEventBox : GtkBin
        classdec GtkHandleBox : GtkBin
        classdec GtkScrolledWindow : GtkBin
        classdec GtkViewport : GtkBin
      // end of [GtkBin]
      classdec GtkBox : GtkContainer
        classdec GtkBottonBox : GtkBox
          classdec GtkBottonHBox : GtkBottonBox
          classdec GtkBottonVBox : GtkBottonBox
        // end of [GtkBottonBox]
        classdec GtkVBox : GtkBox
          classdec GtkColorSelection : GtkVBox
          classdec GtkFontSelection : GtkVBox
          classdec GtkGammarCurve : GtkVBox
        // end of [GtkVBox]
        classdec GtkHBox : GtkBox
          classdec GtkCombo : GtkHBox
          classdec GtkStatusbar : GtkHBox
        // end of [GtkHBox]
      // end of [GtkBox]
      classdec GtkFixed : GtkContainer
      classdec GtkPaned : GtkContainer
        classdec GtkHPaned : GtkPaned
        classdec GtkVPaned : GtkPaned
      // end of [GtkPaned]
      classdec GtkLayout : GtkContainer
      classdec GtkMenuShell : GtkContainer
        classdec GtkMenuBar : GtkMenuShell
        classdec GtkMenu : GtkMenuShell
      // end of [GtkMenuShell]
      classdec GtkNotebook : GtkContainer
      classdec GtkSocket : GtkContainer
      classdec GtkTable : GtkContainer
      classdec GtkTextView : GtkContainer
      classdec GtkToolbar : GtkContainer
      classdec GtkTreeView : GtkContainer
    // end of [GtkContainer]
    classdec GtkCalendar : GtkWidget
    classdec GtkDrawingArea : GtkWidget
      classdec GtkCurve : GtkDrawingArea
    // end of [GtkDrawingArea]
    classdec GtkEditable : GtkWidget
      classdec GtkEntry : GtkEditable
        classdec GtkSpinButton : GtkEntry
      // end of [GtkEntry]
    // end of [GtkEditable]
    classdec GtkRuler : GtkWidget
      classdec GtkHRuler : GtkRuler
      classdec GtkVRuler : GtkRuler
    // end of [GtkScale]
    classdec GtkRange : GtkWidget
      classdec GtkScale : GtkRange
        classdec GtkHScale : GtkScale
        classdec GtkVScale : GtkScale
      // end of [GtkScale]
      classdec GtkScrollbar : GtkRange
        classdec GtkHScrollbar : GtkScrollbar
        classdec GtkVScrollbar : GtkScrollbar
      // end of [GtkScrollbar]
    // end of [GtkRange]
    classdec GtkSeparator : GtkWidget
      classdec GtkHSeparator : GtkSeparator
      classdec GtkVSeparator : GtkSeparator
    // end of [GtkSeparator]
    classdec GtkInvisible : GtkWidget
    classdec GtkPreview : GtkWidget
    classdec GtkProgress : GtkWidget // DEPRECATED!!!
      classdec GtkProgressBar : GtkProgress
    // end of [GtkProgress]
  // end of [GtkWidget]
  classdec GtkAdjustment : GtkObject
  classdec GtkCellRenderer : GtkObject
    classdec GtkCellRendererPixbuf : GtkCellRenderer
    classdec GtkCellRendererText : GtkCellRenderer
    classdec GtkCellRendererToggle : GtkCellRenderer
  // end of [GtkCellRenderer]
  classdec GtkItemFactory : GtkObject
  classdec GtkTooltips : GtkObject
  classdec GtkTreeViewColumn : GtkObject
// end of [GtkObject]

(* ****** ****** *)

classdec GtkStyle : GObject

(* ****** ****** *)

classdec GtkTextBuffer : GObject
classdec GtkTextTag : GObject
classdec GtkTextTagTable : GObject
classdec GtkTextMark : GObject

(* ****** ****** *)

(* end of [gtkclassdec.sats] *)
