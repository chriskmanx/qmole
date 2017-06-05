/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/TranslNew.c,v 1.1 2004/08/28 19:22:46 dannybackx Exp $
 *
 * Copyright (C) 1997 Free Software Foundation, Inc.
 * Copyright © 1997, 1998, 1999, 2000, 2001 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/TranslNew.c,v 1.1 2004/08/28 19:22:46 dannybackx Exp $";

#include <LTconfig.h>

#include <Xm/XmP.h>
#include <Xm/TransltnsP.h> /* we have different headers for each version !!! */

/* Translation & accelerator tables */


#if XmVersion == 2000
XmConst char _XmCSText_EventBindings1[] =
    "<Unmap>:			unmap()\n"
    "<EnterWindow>:		enter()\n"
    "<LeaveWindow>:		leave()\n"
    "<FocusIn>:			focusIn()\n"
    "<FocusOut>:		focusOut()\n"
    "~c s ~m ~a <Btn1Down>:	process-bselect(extend-start)\n"
    "c ~s ~m ~a <Btn1Down>:	process-bselect(move-destination)\n"
    "~c ~s ~m ~a <Btn1Down>:	process-bselect(grab-focus)\n"
    "m ~a <Btn1Down>:		process-bselect()\n"
    "~m a <Btn1Down>:		process-bselect()\n"
    "~c ~m ~a <Btn1Motion>:	process-bselect(extend-adjust)\n"
    "~c ~m ~a <Btn1Up>:		process-bselect(extend-end)\n"
    "~m ~a <Btn1Up>:	        process-bselect()\n"
    "<Btn2Down>:		process-bdrag-event(extend-start, process-bdrag)\n"
    "m ~a <Btn2Motion>:		process-bdrag-event(extend-adjust,secondary-adjust)\n"
    "~m a <Btn2Motion>:		process-bdrag-event(extend-adjust,secondary-adjust)\n"
    "<Btn2Motion>:	        process-bdrag-event(extend-adjust)\n"
    "s c <Btn2Up>:         	process-bdrag-event(extend-end, link-to)\n"
    "~s <Btn2Up>:		process-bdrag-event(extend-end, copy-to)\n"
    "~c <Btn2Up>:		process-bdrag-event(extend-end, move-to)\n"
    ":m <Key>osfPrimaryPaste:	cut-primary()\n"
    ":a <Key>osfPrimaryPaste:	cut-primary()\n"
    ":<Key>osfPrimaryPaste:	copy-primary()\n"
    ":m <Key>osfCut:		cut-primary()\n"
    ":a <Key>osfCut:		cut-primary()\n"
    ":<Key>osfCut:		cut-clipboard()\n"
    ":<Key>osfPaste:		paste-clipboard()\n"
    ":m <Key>osfCopy:		copy-primary()\n"
    ":a <Key>osfCopy:		copy-primary()\n"
    ":<Key>osfCopy:		copy-clipboard()\n"
    ":s c <Key>osfBeginLine:	beginning-of-file(extend)\n"
    ":c <Key>osfBeginLine:	beginning-of-file()\n"
    ":s <Key>osfBeginLine:	beginning-of-line(extend)\n"
    ":<Key>osfBeginLine:	beginning-of-line()\n"
    ":s c <Key>osfEndLine:	end-of-file(extend)\n"
    ":c <Key>osfEndLine:	end-of-file()\n"
    ":s <Key>osfEndLine:	end-of-line(extend)\n"
    ":<Key>osfEndLine:		end-of-line()\n"
    ":s <Key>osfPageLeft:	left-page(extend)\n"
    ":<Key>osfPageLeft:		left-page()\n"
    ":s c <Key>osfPageUp:	left-page(extend)\n"
    ":c <Key>osfPageUp:		left-page()\n"
    ":s ~c<Key>osfPageUp:	previous-page(extend)\n"
    ":~c<Key>osfPageUp:		previous-page()\n"
    ":s <Key>osfPageRight:	right-page(extend)\n"
    ":<Key>osfPageRight:	right-page()\n"
    "";

XmConst char _XmCSText_EventBindings2[] =
    ":s c <Key>osfPageDown:	right-page(extend)\n"
    ":c <Key>osfPageDown:	right-page()\n"
    ":s ~c<Key>osfPageDown:	next-page(extend)\n"
    ":~c<Key>osfPageDown:	next-page()\n"
    ":<Key>osfClear:		clear-selection()\n"
    ":<Key>osfBackSpace:	delete-previous-character()\n"
    ":s m <Key>osfDelete:	cut-primary()\n"
    ":s a <Key>osfDelete:	cut-primary()\n"
    ":s <Key>osfDelete:		cut-clipboard()\n"
    ":c <Key>osfDelete:		delete-to-end-of-line()\n"
    ":<Key>osfDelete:		delete-next-character()\n"
    ":c m <Key>osfInsert:	copy-primary()\n"
    ":c a <Key>osfInsert:	copy-primary()\n"
    ":s <Key>osfInsert:		paste-clipboard()\n"
    ":c <Key>osfInsert:		copy-clipboard()\n"
    ":<Key>osfInsert:		toggle-overstrike()\n"
    ":s <Key>osfSelect:		key-select()\n"
    ":<Key>osfSelect:		set-anchor()\n"
    ":<Key>osfSelectAll:	select-all()\n"
    ":<Key>osfDeselectAll:	deselect-all()\n"
    ":<Key>osfActivate:		activate()\n"
    ":<Key>osfAddMode:		toggle-add-mode()\n"
    ":<Key>osfHelp:		Help()\n"
    ":<Key>osfCancel:		process-cancel()\n"
    ":s c <Key>osfLeft:		left-word(extend)\n"
    ":c <Key>osfLeft:		left-word()\n"
    ":s <Key>osfLeft:		key-select(left)\n"
    ":<Key>osfLeft:		left-character()\n"
    ":s c <Key>osfRight:	right-word(extend)\n"
    ":c <Key>osfRight:		right-word()\n"
    ":s <Key>osfRight:		key-select(right)\n"
    ":<Key>osfRight:		right-character()\n"
    ":s c <Key>osfUp:		previous-paragraph(extend)\n"
    ":c <Key>osfUp:		previous-paragraph()\n"
    ":s <Key>osfUp:		process-shift-up()\n"
    ":<Key>osfUp:		process-up()\n"
    ":s c <Key>osfDown:		next-paragraph(extend)\n"
    ":c <Key>osfDown:		next-paragraph()\n"
    ":s <Key>osfDown:		process-shift-down()\n"
    ":<Key>osfDown:		process-down()\n"
    "";

XmConst char _XmCSText_EventBindings3[] =
    "c ~m ~a <Key>slash:		select-all()\n"
    "c ~m ~a <Key>backslash:		deselect-all()\n"
    " s  c ~m ~a <Key>Tab:		previous-tab-group()\n"
    "~s  c ~m ~a <Key>Tab:		next-tab-group()\n"
    " s ~c ~m ~a <Key>Tab:		process-tab(Prev)\n"
    "~s ~c ~m ~a <Key>Tab:		process-tab(Next)\n"
    "c ~s ~m ~a <Key>Return:		activate()\n"
    "~c ~s ~m ~a <Key>Return:		process-return()\n"
    "c ~s ~m ~a <Key>space:		set-anchor()\n"
    "c s ~m ~a <Key>space:		key-select()\n"
    "s ~c ~m ~a <Key>space:		insert-self()\n"
    "<Key>:				insert-self()\n"
    "";
#endif /* XmVersion = 2000 */


XmConst char _XmComboBox_defaultTranslations[] =
    "<Btn1Down>:		CBArmAndDropDownList()\n"
    "<Btn1Up>:		CBDisarm()";

XmConst char _XmComboBox_defaultAccelerators[] =
    "#override\n"
    ":c <Key>osfUp:		CBDropDownList()\n"
    ":<Key>osfUp:		CBListAction(Up)\n"
    ":c <Key>osfDown:		CBDropDownList()\n"
    ":<Key>osfDown:		CBListAction(Down)\n"
    ":c <Key>osfBeginLine:	CBListAction(ListBeginData)\n"
    ":c <Key>osfEndLine:	CBListAction(ListEndData)\n"
    ":<Key>osfPageUp:		CBListAction(ListPrevPage)\n"
    ":<Key>osfPageDown:		CBListAction(ListNextPage)\n"
    "";

XmConst char _XmComboBox_dropDownComboBoxAccelerators[] =
    "#override\n"
    ":m <Key>osfPrimaryPaste:	cut-primary()\n"
    ":a <Key>osfPrimaryPaste:	cut-primary()\n"
    ":<Key>osfPrimaryPaste:	copy-primary()\n"
    ":m <Key>osfCut:		cut-primary()\n"
    ":a <Key>osfCut:		cut-primary()\n"
    ":<Key>osfCut:		cut-clipboard()\n"
    ":<Key>osfPaste:		paste-clipboard()\n"
    ":m <Key>osfCopy:		copy-primary()\n"
    ":a <Key>osfCopy:		copy-primary()\n"
    ":<Key>osfCopy:		copy-clipboard()\n"
    ":s <Key>osfBeginLine:	beginning-of-line(extend)\n"
    ":<Key>osfBeginLine:	beginning-of-line()\n"
    ":s <Key>osfEndLine:	end-of-line(extend)\n"
    ":<Key>osfEndLine:		end-of-line()\n"
    ":s <Key>osfPageLeft:	page-left(extend)\n"
    ":<Key>osfPageLeft:		page-left()\n"
    ":s <Key>osfPageRight:	page-right(extend)\n"
    ":<Key>osfPageRight:	page-right()\n"
    ":<Key>osfClear:		clear-selection()\n"
    ":<Key>osfBackSpace:	delete-previous-character()\n"
    ":s m <Key>osfDelete:	cut-primary()\n"
    ":s a <Key>osfDelete:	cut-primary()\n"
    ":s <Key>osfDelete:		cut-clipboard()\n"
    ":c <Key>osfDelete:		delete-to-end-of-line()\n"
    ":<Key>osfDelete:		delete-next-character()\n"
    ":c m <Key>osfInsert:	copy-primary()\n"
    ":c a <Key>osfInsert:	copy-primary()\n"
    ":s <Key>osfInsert:		paste-clipboard()\n"
    ":c <Key>osfInsert:		copy-clipboard()\n"
    ":s <Key>osfSelect:		key-select()\n"
    ":<Key>osfSelect:		set-anchor()\n"
    ":<Key>osfSelectAll:	select-all()\n"
    ":<Key>osfDeselectAll:	deselect-all()\n"
    ":<Key>osfActivate:		activate()\n"
    ":<Key>osfAddMode:		toggle-add-mode()\n"
    ":<Key>osfHelp:		Help()\n"
    ":<Key>osfCancel:		process-cancel()\n"
    ":s c <Key>osfLeft:		backward-word(extend)\n"
    ":c <Key>osfLeft:		backward-word()\n"
    ":s <Key>osfLeft:		key-select(left)\n"
    ":<Key>osfLeft:		backward-character()\n"
    ":s c <Key>osfRight:	forward-word(extend)\n"
    ":c <Key>osfRight:		forward-word()\n"
    ":s <Key>osfRight:		key-select(right)\n"
    ":<Key>osfRight:		forward-character()\n"
    "c ~m ~a <Key>slash:	select-all()\n"
    "c ~m ~a <Key>backslash:	deselect-all()\n"
    "s ~m ~a <Key>Tab:		prev-tab-group()\n"
    "~m ~a <Key>Tab:		next-tab-group()\n"
    "~s ~m ~a <Key>Return:	activate()\n"
    "c ~s ~m ~a <Key>space:	set-anchor()\n"
    "c s ~m ~a <Key>space:	key-select()\n"
    "s ~c ~m ~a <Key>space:	self-insert()\n"
    "<Key>:			self-insert()";

XmConst char _XmComboBox_dropDownListTranslations[] =
    "#override\n"
    ":c <Key>osfDown:		CBDropDownList()\n"
    ":c <Key>osfUp:		CBDropDownList()\n"
    ":<Key>osfCancel:		CBCancel()\n"
    ":<Key>osfActivate:		CBActivate()\n"
    "~s ~m ~a<Key>Return:	CBActivate()";

XmConst char _XmComboBox_textFocusTranslations[] =
    "#override\n"
    "<FocusOut>:		CBTextFocusOut()";

XmConst char _XmContainer_defaultTranslations[] =
    "~c ~s ~m ~a <Btn1Down>:		ContainerHandleBtn1Down(ContainerBeginSelect,Copy)\n"
    " c ~s ~m ~a <Btn1Down>:		ContainerHandleBtn1Down(ContainerBeginToggle,Copy)\n"
    " c  s ~m ~a <Btn1Down>:		ContainerHandleBtn1Down(ContainerNoop,Link)\n"
    "~c  s ~m ~a <Btn1Down>:		ContainerHandleBtn1Down(ContainerBeginExtend,Move)\n"
    "<Btn1Motion>:			ContainerHandleBtn1Motion(ContainerButtonMotion)\n"
    "~c ~s ~m ~a <Btn1Up>:		ContainerHandleBtn1Up(ContainerEndSelect)\n"
    " c ~s ~m ~a <Btn1Up>:		ContainerHandleBtn1Up(ContainerEndToggle)\n"
    "~c  s ~m ~a <Btn1Up>:		ContainerHandleBtn1Up(ContainerEndExtend)\n"
    " c  s ~m ~a <Btn1Down>:		ContainerHandleBtn1Down(ContainerBeginExtend)\n"
    " c  s ~m ~a <Btn1Up>:		ContainerHandleBtn1Up(ContainerEndExtend)\n"
    "~c ~s ~m ~a <Btn2Down>:		ContainerHandleBtn2Down(ContainerStartTransfer,Copy)\n"
    " c  s ~m ~a <Btn2Down>:		ContainerHandleBtn2Down(ContainerStartTransfer,Link)\n"
    "~c  s ~m ~a <Btn2Down>:		ContainerHandleBtn2Down(ContainerStartTransfer,Move)\n"
    "<Btn2Motion>:			ContainerHandleBtn2Motion(ContainerButtonMotion)\n"
    "~m ~a <Btn2Up>:			ContainerHandleBtn2Up(ContainerEndTransfer)\n"
    ":c  s  a <Key>osfInsert:		ContainerPrimaryLink()\n"
    ":c  s  m <Key>osfInsert:		ContainerPrimaryLink()\n"
    ":a <Key>osfInsert:			ContainerPrimaryCopy()\n"
    ":m <Key>osfInsert:			ContainerPrimaryCopy()\n"
    ":s  a <Key>osfDelete:		ContainerPrimaryMove()\n"
    ":s  m <Key>osfDelete:		ContainerPrimaryMove()\n"
    ":<Key>osfCancel:			ContainerCancel()\n"
    ":s <Key>osfSelect:			ContainerExtend()\n"
    ":<Key>osfSelect:			ContainerSelect()\n"
    ":<Key>osfSelectAll:		ContainerSelectAll()\n"
    ":<Key>osfDeselectAll:		ContainerDeselectAll()\n"
    ":<Key>osfAddMode:			ContainerToggleMode()\n"
    ":<Key>osfActivate:			ContainerActivate()\n"
    " s ~c ~m ~a <Key>space:		ContainerExtend()\n"
    "~s ~c ~m ~a <Key>space:		ContainerSelect()\n"
    "~s ~c ~m ~a <Key>Return:		ContainerActivate()\n"
    "~s  c ~m ~a <Key>slash:		ContainerSelectAll()\n"
    "~s  c ~m ~a <Key>backslash:	ContainerDeselectAll()";

XmConst char _XmContainer_traversalTranslations[] =
    "<FocusOut>:		ManagerFocusOut()\n"
    "<FocusIn>:			ManagerFocusIn()\n"
    ":c s <Key>osfBeginLine:	ContainerExtendCursor(First)\n"
    ":c s <Key>osfEndLine:	ContainerExtendCursor(Last)\n"
    ":c <Key>osfBeginLine:	ContainerMoveCursor(First)\n"
    ":c <Key>osfEndLine:	ContainerMoveCursor(Last)\n"
    ":c <Key>osfLeft:		ContainerExpandOrCollapse(Left)\n"
    ":c <Key>osfRight:		ContainerExpandOrCollapse(Right)\n"
    ":s <Key>osfUp:		ContainerExtendCursor(Up)\n"
    ":s <Key>osfDown:		ContainerExtendCursor(Down)\n"
    ":s <Key>osfLeft:		ContainerExtendCursor(Left)\n"
    ":s <Key>osfRight:		ContainerExtendCursor(Right)\n"
    ":<Key>osfUp:		ContainerMoveCursor(Up)\n"
    ":<Key>osfDown:		ContainerMoveCursor(Down)\n"
    ":<Key>osfLeft:		ContainerMoveCursor(Left)\n"
    ":<Key>osfRight:		ContainerMoveCursor(Right)\n"
    " s ~m ~a <Key>Tab:		ManagerGadgetPrevTabGroup()\n"
    "~s ~m ~a <Key>Tab:		ManagerGadgetNextTabGroup()";

XmConst char _XmGrabShell_translations[] =
    "<BtnUp>:		GrabShellBtnUp()\n"
    "<BtnDown>:		GrabShellBtnDown()";

XmConst char _XmNotebook_manager_translations[] =
    ":c <Key>osfBeginLine:           TraverseTab(Home)\n"
    ":<Key>osfBeginLine:             TraverseTab(Home)\n"
    ":c <Key>osfEndLine:             TraverseTab(End)\n"
    ":<Key>osfEndLine:               TraverseTab(End)\n"
    ":<Key>osfUp:                    TraverseTab(Previous)\n"
    ":<Key>osfDown:                  TraverseTab(Next)\n"
    ":<Key>osfLeft:                  TraverseTab(Previous)\n"
    ":<Key>osfRight:                 TraverseTab(Next)\n"
    ":s <Key>Tab:                    ManagerGadgetPrevTabGroup()\n"
    ":<Key>Tab:                      ManagerGadgetNextTabGroup()\n"
    "<EnterWindow>:                  ManagerEnter()\n"
    "<LeaveWindow>:                  ManagerLeave()\n"
    "<FocusOut>:                     ManagerFocusOut()\n"
    "<FocusIn>:                      ManagerFocusIn()\n"
    "";

XmConst char _XmNotebook_TabAccelerators[] =
    "#override\n"
    ":c <Key>osfBeginLine:           TraverseTab(Home)\n"
    ":<Key>osfBeginLine:             TraverseTab(Home)\n"
    ":c <Key>osfEndLine:             TraverseTab(End)\n"
    ":<Key>osfEndLine:               TraverseTab(End)\n"
    ":<Key>osfUp:                    TraverseTab(Previous)\n"
    ":<Key>osfDown:                  TraverseTab(Next)\n"
    ":<Key>osfLeft:                  TraverseTab(Previous)\n"
    ":<Key>osfRight:                 TraverseTab(Next)\n"
    "";

XmConst char _XmSpinB_defaultTranslations[] =
    "<Btn1Down>:		SpinBArm()\n"
    "<Btn1Up>:			SpinBDisarm()\n"
    "<EnterWindow>:		SpinBEnter()\n"
    "<LeaveWindow>:		SpinBLeave()\n"
    ":<Key>osfUp:		SpinBPrior()\n"
    ":<Key>osfDown:		SpinBNext()\n"
    ":<Key>osfLeft:		SpinBLeft()\n"
    ":<Key>osfRight:		SpinBRight()\n"
    ":<Key>osfBeginLine:	SpinBFirst()\n"
    ":<Key>osfEndLine:		SpinBLast()";

XmConst char _XmSpinB_defaultAccelerators[] =
    "#override\n"
    "<Key> osfUp:SpinBPrior()\n"
    "<Key> osfDown:SpinBNext()\n"
    "<KeyUp> osfUp:SpinBDisarm()\n"
    "<KeyUp> osfDown:SpinBDisarm()\n"
    "<Key> osfLeft:SpinBLeft()\n"
    "<Key> osfRight:SpinBRight()\n"
    "<KeyUp> osfLeft:SpinBDisarm()\n"
    "<KeyUp> osfRight:SpinBDisarm()\n"
    "<Key> osfBeginLine:SpinBFirst()\n"
    "<Key> osfEndLine:SpinBLast()";

XmConst char _XmClipWindowTranslationTable[] = "\
    :c <Key>osfBeginLine:     ActionGrab(SWTopLine)\n\
    :<Key>osfBeginLine:       ActionGrab(SWBeginLine)\n\
    :c <Key>osfEndLine:       ActionGrab(SWBottomLine)\n\
    :<Key>osfEndLine:         ActionGrab(SWEndLine)\n\
    :<Key>osfPageLeft:        ActionGrab(SWLeftPage)\n\
    :c <Key>osfPageUp:        ActionGrab(SWLeftPage)\n\
    :<Key>osfPageUp:          ActionGrab(SWUpPage)\n\
    :<Key>osfPageRight:       ActionGrab(SWRightPage)\n\
    :c <Key>osfPageDown:      ActionGrab(SWRightPage)\n\
    :<Key>osfPageDown:        ActionGrab(SWDownPage)";

XmConst char _XmDragC_defaultTranslations[] = "\
    Button1<Enter>:           DragMotion()\n\
    Button1<Leave>:           DragMotion()\n\
    Button1<Motion>:          DragMotion()\n\
    Button2<Enter>:           DragMotion()\n\
    Button2<Leave>:           DragMotion()\n\
    Button2<Motion>:          DragMotion()\n\
    <Btn2Up>:                 FinishDrag()\n\
    <Btn1Up>:                 FinishDrag()\n\
    <BtnDown>:                IgnoreButtons()\n\
    <BtnUp>:                  IgnoreButtons()\n\
    <Key>Return:              FinishDrag()\n\
    :<Key>osfActivate:        FinishDrag()\n\
    :<Key>osfCancel:          CancelDrag()\n\
    :<Key>osfHelp:            HelpDrag()\n\
    :<Key>osfUp:              DragKey(Up)\n\
    :<Key>osfDown:            DragKey(Down)\n\
    :<Key>osfLeft:            DragKey(Left)\n\
    :<Key>osfRight:           DragKey(Right)\n\
    :<KeyUp>:                 DragKey(Update)\n\
    :<KeyDown>:               DragKey(Update)";


#if XmVersion >= 2001
/* Hmm, is there something ... ?! */
#endif
