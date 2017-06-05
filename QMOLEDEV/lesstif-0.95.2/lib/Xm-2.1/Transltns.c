/*
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Transltns.c,v 1.4 2006/12/31 17:01:51 dannybackx Exp $
 *
 * This file was originally machine generated.

 * Copyright (C) 1995-2001 LessTif Development Team
 */

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Transltns.c,v 1.4 2006/12/31 17:01:51 dannybackx Exp $";

#include <LTconfig.h>

#include <Xm/XmP.h>
#include <Xm/TransltnsP.h> /* we have different headers for each version !!! */


XmConst char _XmArrowB_defaultTranslations[] = "\
    <EnterWindow>:            Enter()\n\
    <LeaveWindow>:            Leave()\n\
    <Btn1Down>:               Arm()\n\
    <Btn1Down>,<Btn1Up>:      Activate() Disarm()\n\
    <Btn1Down>(2+):           MultiArm()\n\
    <Btn1Up>(2+):             MultiActivate()\n\
    <Btn1Up>:                 Activate() Disarm()\n\
    :<Key>osfActivate:        PrimitiveParentActivate()\n\
    :<Key>osfCancel:          PrimitiveParentCancel()\n\
    :<Key>osfSelect:          ArmAndActivate()\n\
    :<Key>osfHelp:            Help()\n\
    ~s ~m ~a <Key>Return:     PrimitiveParentActivate()\n\
    ~s ~m ~a <Key>space:      ArmAndActivate()";

XmConst char _XmBulletinB_defaultTranslations[] = "\
    <BtnMotion>:              ManagerGadgetButtonMotion()\n\
    <Btn1Down>:               ManagerGadgetArm()\n\
    <Btn1Down>,<Btn1Up>:      ManagerGadgetActivate()\n\
    <Btn1Up>:                 ManagerGadgetActivate()\n\
    <Btn1Down>(2+):           ManagerGadgetMultiArm()\n\
    <Btn1Up>(2+):             ManagerGadgetMultiActivate()\n\
    <Btn2Down>:               ManagerGadgetDrag()\n\
    :<Key>osfHelp:            ManagerGadgetHelp()\n\
    :<Key>osfActivate:        ManagerParentActivate()\n\
    :<Key>osfCancel:          ManagerParentCancel()\n\
    :<Key>osfSelect:          ManagerGadgetSelect()\n\
    <Key>space:               ManagerGadgetSelect()\n\
    <Key>Return:              ManagerParentActivate()\n\
    <Key>:                    ManagerGadgetKeyInput()";

XmConst char _XmCascadeB_menubar_events[] = "\
    <EnterWindow>Normal:      MenuBarEnter()\n\
    <LeaveWindow>Normal:      MenuBarLeave()\n\
    <BtnDown>:                MenuBarSelect()\n\
    <BtnUp>:                  DoSelect()\n\
    :<Key>osfSelect:          KeySelect()\n\
    :<Key>osfActivate:        KeySelect()\n\
    :<Key>osfHelp:            Help()\n\
    :<Key>osfCancel:          CleanupMenuBar()\n\
    ~s<Key>Return:            KeySelect()\n\
    ~s<Key>space:             KeySelect()";

XmConst char _XmCascadeB_p_events[] = "\
    <EnterWindow>:            DelayedArm()\n\
    <LeaveWindow>:            CheckDisarm()\n\
    <BtnDown>:                StartDrag()\n\
    <BtnUp>:                  DoSelect()\n\
    :<Key>osfSelect:          KeySelect()\n\
    :<Key>osfActivate:        KeySelect()\n\
    :<Key>osfHelp:            Help()\n\
    :<Key>osfCancel:          CleanupMenuBar()\n\
    ~s<Key>Return:            KeySelect()\n\
    ~s<Key>space:             KeySelect()";

XmConst char _XmDrawingA_defaultTranslations[] = "\
    <BtnMotion>:              ManagerGadgetButtonMotion()\n\
    <Btn1Down>:               DrawingAreaInput() ManagerGadgetArm()\n\
    <Btn1Down>,<Btn1Up>:      DrawingAreaInput() ManagerGadgetActivate()\n\
    <Btn1Up>:                 DrawingAreaInput() ManagerGadgetActivate()\n\
    <Btn1Down>(2+):           DrawingAreaInput() ManagerGadgetMultiArm()\n\
    <Btn1Up>(2+):             DrawingAreaInput() ManagerGadgetMultiActivate()\n\
    <Btn2Down>:               DrawingAreaInput() ManagerGadgetDrag()\n\
    <BtnDown>:                DrawingAreaInput()\n\
    <BtnUp>:                  DrawingAreaInput()\n\
    :<Key>osfActivate:        DrawingAreaInput() ManagerParentActivate()\n\
    :<Key>osfCancel:          DrawingAreaInput() ManagerParentCancel()\n\
    :<Key>osfHelp:            DrawingAreaInput() ManagerGadgetHelp()\n\
    :<Key>osfSelect:          DrawingAreaInput() ManagerGadgetSelect()\n\
    ~s ~m ~a <Key>Return:     DrawingAreaInput() ManagerParentActivate()\n\
    <Key>Return:              DrawingAreaInput() ManagerGadgetSelect()\n\
    <Key>space:               DrawingAreaInput() ManagerGadgetSelect()\n\
    <KeyDown>:                DrawingAreaInput() ManagerGadgetKeyInput()\n\
    <KeyUp>:                  DrawingAreaInput()";

XmConst char _XmDrawingA_traversalTranslations[] = "\
    <EnterWindow>:            ManagerEnter()\n\
    <LeaveWindow>:            ManagerLeave()\n\
    <FocusOut>:               ManagerFocusOut()\n\
    <FocusIn>:                ManagerFocusIn()\n\
    :<Key>osfUp:              DrawingAreaInput() ManagerGadgetTraverseUp()\n\
    :<Key>osfDown:            DrawingAreaInput() ManagerGadgetTraverseDown()\n\
    :<Key>osfLeft:            DrawingAreaInput() ManagerGadgetTraverseLeft()\n\
    :<Key>osfRight:           DrawingAreaInput() ManagerGadgetTraverseRight()\n\
    :<Key>osfBeginLine:       DrawingAreaInput() ManagerGadgetTraverseHome()\n\
    s<Key>Tab:                DrawingAreaInput() ManagerGadgetPrevTabGroup()\n\
    ~s<Key>Tab:               DrawingAreaInput() ManagerGadgetNextTabGroup()";

XmConst char _XmDrawnB_defaultTranslations[] = "\
    <EnterWindow>:            Enter()\n\
    <LeaveWindow>:            Leave()\n\
    <Btn1Down>:               Arm()\n\
    <Btn1Down>,<Btn1Up>:      Activate() Disarm()\n\
    <Btn1Down>(2+):           MultiArm()\n\
    <Btn1Up>(2+):             MultiActivate()\n\
    <Btn1Up>:                 Activate() Disarm()\n\
    :<Key>osfActivate:        PrimitiveParentActivate()\n\
    :<Key>osfCancel:          PrimitiveParentCancel()\n\
    :<Key>osfSelect:          ArmAndActivate()\n\
    :<Key>osfHelp:            Help()\n\
    ~s ~m ~a <Key>Return:     PrimitiveParentActivate()\n\
    ~s ~m ~a <Key>space:      ArmAndActivate()";

XmConst char _XmFrame_defaultTranslations[] = "\
    <EnterWindow>:            Enter()\n\
    <FocusIn>:                FocusIn()\n\
    <Btn1Down>:               Arm()\n\
    <Btn1Up>:                 Activate()\n\
    <Btn2Down>:               ManagerGadgetDrag()\n\
    :<Key>osfActivate:        ManagerParentActivate()\n\
    :<Key>osfCancel:          ManagerParentCancel()\n\
    ~s ~m ~a <Key>Return:     ManagerParentActivate()";

XmConst char _XmLabel_defaultTranslations[] = "\
    <EnterWindow>:            Enter()\n\
    <LeaveWindow>:            Leave()\n\
    <Btn2Down>:               ProcessDrag()\n\
    :<Key>osfActivate:        PrimitiveParentActivate()\n\
    :<Key>osfCancel:          PrimitiveParentCancel()\n\
    :<Key>osfHelp:            Help()\n\
    ~s ~m ~a <Key>Return:     PrimitiveParentActivate()";

XmConst char _XmLabel_menuTranslations[] = "\
    <EnterWindow>:            Enter()\n\
    <LeaveWindow>:            Leave()\n\
    :<Key>osfHelp:            Help()";

XmConst char _XmLabel_menu_traversal_events[] = "\
    <Unmap>:                  Unmap()\n\
    <FocusOut>:               FocusOut()\n\
    <FocusIn>:                FocusIn()\n\
    :<Key>osfCancel:          MenuEscape()\n\
    :<Key>osfLeft:            MenuTraverseLeft()\n\
    :<Key>osfRight:           MenuTraverseRight()\n\
    :<Key>osfUp:              MenuTraverseUp()\n\
    :<Key>osfDown:            MenuTraverseDown()";

XmConst char _XmList_ListXlations1[] = "\
    s c <Key>osfBeginLine:    ListBeginDataExtend()\n\
    :c <Key>osfBeginLine:     ListBeginData()\n\
    :<Key>osfBeginLine:       ListBeginLine()\n\
    s c <Key>osfEndLine:      ListEndDataExtend()\n\
    :c <Key>osfEndLine:       ListEndData()\n\
    :<Key>osfEndLine:         ListEndLine()\n\
    :<Key>osfPageLeft:        ListLeftPage()\n\
    :c <Key>osfPageUp:        ListLeftPage()\n\
    :<Key>osfPageUp:          ListPrevPage()\n\
    :<Key>osfPageRight:       ListRightPage()\n\
    :c <Key>osfPageDown:      ListRightPage()\n\
    :<Key>osfPageDown:        ListNextPage()\n\
    :s <KeyDown>osfSelect:    ListKbdBeginExtend()\n\
    :s <KeyUp>osfSelect:      ListKbdEndExtend()\n\
    :<KeyDown>osfSelect:      ListKbdBeginSelect()\n\
    :<KeyUp>osfSelect:        ListKbdEndSelect()\n\
    :<Key>osfActivate:        ListKbdActivate()\n\
    :<Key>osfAddMode:         ListAddMode()\n\
    :<Key>osfHelp:            PrimitiveHelp()\n\
    :<Key>osfCancel:          ListKbdCancel()\n\
    ";

XmConst char _XmList_ListXlations2[] = "\
    <Unmap>:                  PrimitiveUnmap()\n\
    <Enter>:                  ListEnter()\n\
    <Leave>:                  ListLeave()\n\
    <FocusIn>:                ListFocusIn()\n\
    <FocusOut>:               ListFocusOut()\n\
    Button1<Motion>:          ListButtonMotion()\n\
    s ~m ~a <Btn1Down>:       ListBeginExtend()\n\
    s ~m ~a <Btn1Up>:         ListEndExtend()\n\
    c ~s ~m ~a <Btn1Down>:    ListBeginToggle()\n\
    c ~s ~m ~a <Btn1Up>:      ListEndToggle()\n\
    ~s ~c ~m ~a <Btn1Down>:   ListBeginSelect()\n\
    ~s ~c ~m ~a <Btn1Up>:     ListEndSelect()\n\
    <Btn2Down>:               ListProcessDrag()\n\
    :c <Key>osfLeft:          ListLeftPage()\n\
    :<Key>osfLeft:            ListLeftChar()\n\
    :c <Key>osfRight:         ListRightPage()\n\
    :<Key>osfRight:           ListRightChar()\n\
    s <Key>osfUp:             ListExtendPrevItem()\n\
    :<Key>osfUp:              ListPrevItem()\n\
    s <Key>osfDown:           ListExtendNextItem()\n\
    :<Key>osfDown:            ListNextItem()\n\
    :c <Key>osfInsert:        ListCopyToClipboard()\n\
    :<Key>osfCopy:            ListCopyToClipboard()\n\
    ~s c ~m ~a <Key>slash:    ListKbdSelectAll()\n\
    ~s c ~m ~a <Key>backslash:ListKbdDeSelectAll()\n\
    s ~m ~a <Key>Tab:         PrimitivePrevTabGroup()\n\
    ~m ~a <Key>Tab:           PrimitiveNextTabGroup()\n\
    ~s ~m ~a <Key>Return:     ListKbdActivate()\n\
    ~s ~m ~a <KeyDown>space:  ListKbdBeginSelect()\n\
    ~s ~m ~a <KeyUp>space:    ListKbdEndSelect()\n\
    s ~m ~a <KeyDown>space:   ListKbdBeginExtend()\n\
    s ~m ~a <KeyUp>space:     ListKbdEndExtend()"
#if 1
    /* Scroll wheel support */
    "\n<Btn5Down>: ListNextPage()"
    "\n<Btn4Down>: ListPrevPage()"
#endif
    ;

XmConst char _XmManager_managerTraversalTranslations[] = "\
    <EnterWindow>:            ManagerEnter()\n\
    <LeaveWindow>:            ManagerLeave()\n\
    <FocusOut>:               ManagerFocusOut()\n\
    <FocusIn>:                ManagerFocusIn()\n\
    :<Key>osfBeginLine:       ManagerGadgetTraverseHome()\n\
    :<Key>osfUp:              ManagerGadgetTraverseUp()\n\
    :<Key>osfDown:            ManagerGadgetTraverseDown()\n\
    :<Key>osfLeft:            ManagerGadgetTraverseLeft()\n\
    :<Key>osfRight:           ManagerGadgetTraverseRight()\n\
    s ~m ~a <Key>Tab:         ManagerGadgetPrevTabGroup()\n\
    ~m ~a <Key>Tab:           ManagerGadgetNextTabGroup()";

XmConst char _XmManager_defaultTranslations[] = "\
    <BtnMotion>:              ManagerGadgetButtonMotion()\n\
    <Btn1Down>:               ManagerGadgetArm()\n\
    <Btn1Down>,<Btn1Up>:      ManagerGadgetActivate()\n\
    <Btn1Up>:                 ManagerGadgetActivate()\n\
    <Btn1Down>(2+):           ManagerGadgetMultiArm()\n\
    <Btn1Up>(2+):             ManagerGadgetMultiActivate()\n\
    <Btn2Down>:               ManagerGadgetDrag()\n\
    :<Key>osfActivate:        ManagerParentActivate()\n\
    :<Key>osfCancel:          ManagerParentCancel()\n\
    :<Key>osfSelect:          ManagerGadgetSelect()\n\
    :<Key>osfHelp:            ManagerGadgetHelp()\n\
    ~s ~m ~a <Key>Return:     ManagerParentActivate()\n\
    ~s ~m ~a <Key>space:      ManagerGadgetSelect()\n\
    <Key>:                    ManagerGadgetKeyInput()";

XmConst char _XmMenuShell_translations[] = "\
    <BtnDown>:                ClearTraversal()\n\
    <BtnUp>:                  MenuShellPopdownDone()";

XmConst char _XmPrimitive_defaultTranslations[] = "\
    <Unmap>:                  PrimitiveUnmap()\n\
    <FocusIn>:                PrimitiveFocusIn()\n\
    <FocusOut>:               PrimitiveFocusOut()\n\
    :<Key>osfActivate:        PrimitiveParentActivate()\n\
    :<Key>osfCancel:          PrimitiveParentCancel()\n\
    :<Key>osfBeginLine:       PrimitiveTraverseHome()\n\
    :<Key>osfHelp:            PrimitiveHelp()\n\
    :<Key>osfUp:              PrimitiveTraverseUp()\n\
    :<Key>osfDown:            PrimitiveTraverseDown()\n\
    :<Key>osfLeft:            PrimitiveTraverseLeft()\n\
    :<Key>osfRight:           PrimitiveTraverseRight()\n\
    ~s ~m ~a <Key>Return:     PrimitiveParentActivate()\n\
    s ~m ~a <Key>Tab:         PrimitivePrevTabGroup()\n\
    ~m ~a <Key>Tab:           PrimitiveNextTabGroup()";

XmConst char _XmPushB_defaultTranslations[] = "\
    <EnterWindow>:            Enter()\n\
    <LeaveWindow>:            Leave()\n\
    <Btn1Down>:               Arm()\n\
    <Btn1Down>,<Btn1Up>:      Activate() Disarm()\n\
    <Btn1Down>(2+):           MultiArm()\n\
    <Btn1Up>(2+):             MultiActivate()\n\
    <Btn1Up>:                 Activate() Disarm()\n"
#if 0
    /*
     * This is a temporary addition to make the JDK people happy.
     * It should be fixed somewhere else 'cause this doesn't belong here.
     */
    "<Btn3Up>:                 Activate() Disarm()\n"
#endif
    "<Btn2Down>:               ProcessDrag()\n\
    :<Key>osfActivate:        PrimitiveParentActivate()\n\
    :<Key>osfCancel:          PrimitiveParentCancel()\n\
    :<Key>osfSelect:          ArmAndActivate()\n\
    :<Key>osfHelp:            Help()\n\
    ~s ~m ~a <Key>Return:     PrimitiveParentActivate()\n\
    ~s ~m ~a <Key>space:      ArmAndActivate()";

XmConst char _XmPushB_menuTranslations[] = "\
    <EnterWindow>:            Enter()\n\
    <LeaveWindow>:            Leave()\n\
    <BtnDown>:                BtnDown()\n\
    <BtnUp>:                  BtnUp()\n\
    :<Key>osfSelect:          ArmAndActivate()\n\
    :<Key>osfActivate:        ArmAndActivate()\n\
    :<Key>osfCancel:          MenuEscape()\n\
    :<Key>osfHelp:            Help()\n\
    ~s ~m ~a <Key>Return:     ArmAndActivate()\n\
    ~s ~m ~a <Key>space:      ArmAndActivate()";

XmConst char _XmRowColumn_menu_traversal_table[] = "\
    <Unmap>:                  MenuUnmap()\n\
    <EnterWindow>Normal:      MenuEnter()\n\
    <FocusIn>:                MenuFocusIn()\n\
    <FocusOut>:               MenuFocusOut()\n\
    :<Key>osfHelp:            MenuHelp()\n\
    :<Key>osfLeft:            MenuGadgetTraverseLeft()\n\
    :<Key>osfRight:           MenuGadgetTraverseRight()\n\
    :<Key>osfUp:              MenuGadgetTraverseUp()\n\
    :<Key>osfDown:            MenuGadgetTraverseDown()";

XmConst char _XmRowColumn_bar_table[] = "\
    <BtnDown>:                MenuBtnDown()\n\
    <BtnUp>:                  MenuBtnUp()\n\
    :<Key>osfSelect:          MenuBarGadgetSelect()\n\
    :<Key>osfActivate:        MenuBarGadgetSelect()\n\
    :<Key>osfHelp:            MenuHelp()\n\
    :<Key>osfCancel:          MenuGadgetEscape()\n\
    ~s ~m ~a <Key>Return:     MenuBarGadgetSelect()\n\
    ~s ~m ~a <Key>space:      MenuBarGadgetSelect()";

XmConst char _XmRowColumn_option_table[] = "\
    <BtnDown>:                MenuBtnDown()\n\
    <BtnUp>:                  MenuBtnUp()\n\
    :<Key>osfActivate:        ManagerParentActivate()\n\
    :<Key>osfCancel:          ManagerParentCancel()\n\
    :<Key>osfSelect:          ManagerGadgetSelect()\n\
    :<Key>osfHelp:            MenuHelp()\n\
    ~s ~m ~a <Key>Return:     ManagerParentActivate()\n\
    ~s ~m ~a <Key>space:      ManagerGadgetSelect()";

XmConst char _XmRowColumn_menu_table[] = "\
    <BtnDown>:                MenuBtnDown()\n\
    <BtnUp>:                  MenuBtnUp()\n\
    :<Key>osfSelect:          ManagerGadgetSelect()\n\
    :<Key>osfActivate:        ManagerGadgetSelect()\n\
    :<Key>osfHelp:            MenuHelp()\n\
    :<Key>osfCancel:          MenuGadgetEscape()\n\
    ~s ~m ~a <Key>Return:     ManagerGadgetSelect()\n\
    ~s ~m ~a <Key>space:      ManagerGadgetSelect()";

XmConst char _XmSash_defTranslations[] = "\
    <Unmap>:                  PrimitiveUnmap()\n\
    <EnterWindow>:            enter()\n\
    <LeaveWindow>:            leave()\n\
    <FocusIn>:                SashFocusIn()\n\
    <FocusOut>:               SashFocusOut()\n\
    ~c ~s ~m ~a <Btn1Down>:   SashAction(Start)\n\
    ~c ~s ~m ~a <Btn1Motion>: SashAction(Move)\n\
    ~c ~s ~m ~a <Btn1Up>:     SashAction(Commit)\n\
    ~c ~s ~m ~a <Btn2Down>:   SashAction(Start)\n\
    ~c ~s ~m ~a <Btn2Motion>: SashAction(Move)\n\
    ~c ~s ~m ~a <Btn2Up>:     SashAction(Commit)\n\
    :<Key>osfActivate:        PrimitiveParentActivate()\n\
    :<Key>osfCancel:          PrimitiveParentCancel()\n\
    :<Key>osfHelp:            Help()\n\
    :c <Key>osfUp:            SashAction(Key,LargeIncr,Up)\n\
    :<Key>osfUp:              SashAction(Key,DefaultIncr,Up)\n\
    :c <Key>osfDown:          SashAction(Key,LargeIncr,Down)\n\
    :<Key>osfDown:            SashAction(Key,DefaultIncr,Down)\n\
    ~s ~m ~a <Key>Return:     PrimitiveParentActivate()\n\
    s ~m ~a <Key>Tab:         PrevTabGroup()\n\
    ~m ~a <Key>Tab:           NextTabGroup()";

XmConst char _XmScrollBar_defaultTranslations[] = "\
    <Unmap>:                  PrimitiveUnmap()\n\
    <Enter>:                  PrimitiveEnter()\n\
    <Leave>:                  PrimitiveLeave()\n\
    <FocusIn>:                PrimitiveFocusIn()\n\
    <FocusOut>:               PrimitiveFocusOut()\n\
    ~s ~c ~m ~a <Btn1Down>:   Select()\n\
    ~s ~c ~m ~a <Btn1Up>:     Release()\n\
    ~s ~c ~m ~a Button1<PtrMoved>:Moved()\n\
    ~s ~c ~m ~a <Btn2Down>:   Select()\n\
    ~s ~c ~m ~a <Btn2Up>:     Release()\n\
    ~s ~c ~m ~a Button2<PtrMoved>:Moved()\n\
    ~s c ~m ~a <Btn1Down>:    TopOrBottom()\n\
    ~s c ~m ~a <Btn1Up>:      Release()\n\
    :<Key>osfActivate:        PrimitiveParentActivate()\n\
    :<Key>osfCancel:          CancelDrag()\n\
    :<Key>osfBeginLine:       TopOrBottom()\n\
    :<Key>osfEndLine:         TopOrBottom()\n\
    :<Key>osfPageLeft:        PageUpOrLeft(1)\n\
    :c <Key>osfPageUp:        PageUpOrLeft(1)\n\
    :<Key>osfPageUp:          PageUpOrLeft(0)\n\
    :<Key>osfPageRight:       PageDownOrRight(1)\n\
    :c <Key>osfPageDown:      PageDownOrRight(1)\n\
    :<Key>osfPageDown:        PageDownOrRight(0)\n\
    :<Key>osfHelp:            PrimitiveHelp()\n\
    :c <Key>osfUp:            PageUpOrLeft(0)\n\
    :c <Key>osfDown:          PageDownOrRight(0)\n\
    :c <Key>osfLeft:          PageUpOrLeft(1)\n\
    :c <Key>osfRight:         PageDownOrRight(1)\n\
    :<Key>osfUp:              IncrementUpOrLeft(0)\n\
    :<Key>osfDown:            IncrementDownOrRight(0)\n\
    :<Key>osfLeft:            IncrementUpOrLeft(1)\n\
    :<Key>osfRight:           IncrementDownOrRight(1)\n\
    ~s ~m ~a <Key>Return:     PrimitiveParentActivate()\n\
    s ~m ~a <Key>Tab:         PrimitivePrevTabGroup()\n\
    ~m ~a <Key>Tab:           PrimitiveNextTabGroup()"
#if 1
    /* Scroll wheel support */
    "\n<Btn4Down>: IncrementUpOrLeft(0) IncrementUpOrLeft(1)"
    "\n<Btn5Down>: IncrementDownOrRight(0) IncrementDownOrRight(1)"
#endif
    ;

XmConst char _XmScrolledW_ScrolledWindowXlations[] = "\
    <EnterWindow>:            ManagerEnter()\n\
    <FocusOut>:               ManagerFocusOut()\n\
    <FocusIn>:                ManagerFocusIn()\n\
    <Btn2Down>:               ManagerGadgetDrag()\n\
    :<Key>osfActivate:        ManagerParentActivate()\n\
    :<Key>osfCancel:          ManagerParentCancel()\n\
    :c <Key>osfBeginLine:     SWTopLine()\n\
    :<Key>osfBeginLine:       SWBeginLine()\n\
    :c <Key>osfEndLine:       SWBottomLine()\n\
    :<Key>osfEndLine:         SWEndLine()\n\
    :<Key>osfPageLeft:        SWLeftPage()\n\
    :c <Key>osfPageUp:        SWLeftPage()\n\
    :<Key>osfPageUp:          SWUpPage()\n\
    :<Key>osfPageRight:       SWRightPage()\n\
    :c <Key>osfPageDown:      SWRightPage()\n\
    :<Key>osfPageDown:        SWDownPage()\n\
    :<Key>osfHelp:            ManagerGadgetHelp()\n\
    :<Key>osfUp:              ManagerGadgetTraverseUp()\n\
    :<Key>osfDown:            ManagerGadgetTraverseDown()\n\
    :<Key>osfLeft:            ManagerGadgetTraverseLeft()\n\
    :<Key>osfRight:           ManagerGadgetTraverseRight()\n\
    ~s ~m ~a <Key>Return:     ManagerParentActivate()\n\
    s ~m ~a <Key>Tab:         ManagerGadgetPrevTabGroup()\n\
    ~m ~a <Key>Tab:           ManagerGadgetNextTabGroup()"
#if 1
    /* Scroll wheel support */
    "\n<Btn4Down>:		SWUpPage()"
    "\n<Btn5Down>:		SWDownPage()"
#endif
    ;

/* The following two entries are not exported in Motif 2.x.
   However we need them for internal usage */

XmConst char _XmScrolledW_ClipWindowTranslationTable[] = "\
    <MapNotify>:              SWNoop()\n\
    :c <Key>osfBeginLine:     SWTopLineGrab()\n\
    :<Key>osfBeginLine:       SWBeginLineGrab()\n\
    :c <Key>osfEndLine:       SWBottomLineGrab()\n\
    :<Key>osfEndLine:         SWEndLineGrab()\n\
    :<Key>osfPageLeft:        SWLeftPageGrab()\n\
    :c <Key>osfPageUp:        SWLeftPageGrab()\n\
    :<Key>osfPageUp:          SWUpPageGrab()\n\
    :<Key>osfPageRight:       SWRightPageGrab()\n\
    :c <Key>osfPageDown:      SWRightPageGrab()\n\
    :<Key>osfPageDown:        SWDownPageGrab()"
#if 1
    /* Scroll wheel support */
    "\n<Btn4Down>:		SWUpPage()"
    "\n<Btn5Down>:		SWDownPage()"
#endif
    ;

XmConst char _XmScrolledW_WorkWindowTranslationTable[] = "\
    :c <Key>osfBeginLine:     SWTopLineWork()\n\
    :<Key>osfBeginLine:       SWBeginLineWork()\n\
    :c <Key>osfEndLine:       SWBottomLineWork()\n\
    :<Key>osfEndLine:         SWEndLineWork()"
#if 1
    /* Scroll wheel support */
    "\n<Btn4Down>:		SWUpPage()"
    "\n<Btn5Down>:		SWDownPage()"
#endif
    ;

XmConst char _XmSelectioB_defaultTextAccelerators[] = "\
    #override\n\
    :<Key>osfUp:              SelectionBoxUpOrDown(0)\n\
    :<Key>osfDown:            SelectionBoxUpOrDown(1)\n\
    :<Key>osfBeginLine:       SelectionBoxUpOrDown(2)\n\
    :<Key>osfEndLine:         SelectionBoxUpOrDown(3)\n\
    s c ~m ~a <Key>space:     SelectionBoxRestore()";

XmConst char _XmTearOffB_overrideTranslations[] = "\
    <Btn2Down>:               BDrag()\n\
    <BtnUp>:                  BActivate()\n\
    :<Key>osfSelect:          KActivate()\n\
    :<Key>osfActivate:        KActivate()\n\
    ~s ~m ~a <Key>Return:     KActivate()\n\
    ~s ~m ~a <Key>space:      KActivate()";

XmConst char _XmTextF_EventBindings1[] = "\
    :m <Key>osfPrimaryPaste:  cut-primary()\n\
    :a <Key>osfPrimaryPaste:  cut-primary()\n\
    :<Key>osfPrimaryPaste:    copy-primary()\n\
    :m <Key>osfCut:           cut-primary()\n\
    :a <Key>osfCut:           cut-primary()\n\
    :<Key>osfCut:             cut-clipboard()\n\
    :<Key>osfPaste:           paste-clipboard()\n\
    :m <Key>osfCopy:          copy-primary()\n\
    :a <Key>osfCopy:          copy-primary()\n\
    :<Key>osfCopy:            copy-clipboard()\n\
    s <Key>osfBeginLine:      beginning-of-line(extend)\n\
    :<Key>osfBeginLine:       beginning-of-line()\n\
    s <Key>osfEndLine:        end-of-line(extend)\n\
    :<Key>osfEndLine:         end-of-line()\n\
    s <Key>osfPageLeft:       page-left(extend)\n\
    :<Key>osfPageLeft:        page-left()\n\
    s c<Key>osfPageUp:        page-left(extend)\n\
    :c <Key>osfPageUp:        page-left()\n\
    s <Key>osfPageRight:      page-right(extend)\n\
    :<Key>osfPageRight:       page-right()\n\
    s c <Key>osfPageDown:     page-right(extend)\n\
    :c <Key>osfPageDown:      page-right()\n\
    :<Key>osfClear:           clear-selection()\n\
    :<Key>osfBackSpace:       delete-previous-character()\n\
    s m <Key>osfDelete:       cut-primary()\n\
    s a <Key>osfDelete:       cut-primary()\n\
    s <Key>osfDelete:         cut-clipboard()\n\
    :c <Key>osfDelete:        delete-to-end-of-line()\n\
    :<Key>osfDelete:          delete-next-character()\n\
    ";

XmConst char _XmTextF_EventBindings2[] = "\
    :c m <Key>osfInsert:      copy-primary()\n\
    :c a <Key>osfInsert:      copy-primary()\n\
    s <Key>osfInsert:         paste-clipboard()\n\
    :c <Key>osfInsert:        copy-clipboard()\n\
    :s <Key>osfSelect:        key-select()\n\
    :<Key>osfSelect:          set-anchor()\n\
    :<Key>osfActivate:        activate()\n\
    :<Key>osfAddMode:         toggle-add-mode()\n\
    :<Key>osfHelp:            Help()\n\
    :<Key>osfCancel:          process-cancel()\n\
    s c <Key>osfLeft:         backward-word(extend)\n\
    :c <Key>osfLeft:          backward-word()\n\
    s <Key>osfLeft:           key-select(left)\n\
    :<Key>osfLeft:            backward-character()\n\
    s c <Key>osfRight:        forward-word(extend)\n\
    :c <Key>osfRight:         forward-word()\n\
    s <Key>osfRight:          key-select(right)\n\
    :<Key>osfRight:           forward-character()\n\
    :<Key>osfUp:              traverse-prev()\n\
    :<Key>osfDown:            traverse-next()\n\
    c ~m ~a <Key>slash:       select-all()\n\
    c ~m ~a <Key>backslash:   deselect-all()\n\
    s ~m ~a <Key>Tab:         prev-tab-group()\n\
    ~m ~a <Key>Tab:           next-tab-group()\n\
    ~s ~m ~a <Key>Return:     activate()\n\
    c ~s ~m ~a <Key>space:    set-anchor()\n\
    c s ~m ~a <Key>space:     key-select()\n\
    s ~c ~m ~a <Key>space:    self-insert()\n\
    <Key>:                    self-insert()\n\
    ";

XmConst char _XmTextF_EventBindings3[] = "\
    <Unmap>:                  unmap()\n\
    <Enter>:                  enter()\n\
    <Leave>:                  leave()\n\
    <FocusIn>:                focusIn()\n\
    <FocusOut>:               focusOut()\n\
    ~c s ~m ~a <Btn1Down>:    extend-start()\n\
    c ~s ~m ~a <Btn1Down>:    move-destination()\n\
    ~c ~s ~m ~a <Btn1Down>:   grab-focus()\n\
    ~c ~m ~a <Btn1Motion>:    extend-adjust()\n\
    ~c ~m ~a <Btn1Up>:        extend-end()\n\
    <Btn2Down>:               process-bdrag()\n\
    m ~a <Btn2Motion>:        secondary-adjust()\n\
    ~m a <Btn2Motion>:        secondary-adjust()\n\
    ~s <Btn2Up>:              copy-to()\n\
    ~c <Btn2Up>:              move-to()";

XmConst char _XmTextIn_XmTextEventBindings1[] = "\
    :m <Key>osfPrimaryPaste:  cut-primary()\n\
    :a <Key>osfPrimaryPaste:  cut-primary()\n\
    :<Key>osfPrimaryPaste:    copy-primary()\n\
    :m <Key>osfCut:           cut-primary()\n\
    :a <Key>osfCut:           cut-primary()\n\
    :<Key>osfCut:             cut-clipboard()\n\
    :<Key>osfPaste:           paste-clipboard()\n\
    :m <Key>osfCopy:          copy-primary()\n\
    :a <Key>osfCopy:          copy-primary()\n\
    :<Key>osfCopy:            copy-clipboard()\n\
    s c <Key>osfBeginLine:    beginning-of-file(extend)\n\
    :c <Key>osfBeginLine:     beginning-of-file()\n\
    s <Key>osfBeginLine:      beginning-of-line(extend)\n\
    :<Key>osfBeginLine:       beginning-of-line()\n\
    s c <Key>osfEndLine:      end-of-file(extend)\n\
    :c <Key>osfEndLine:       end-of-file()\n\
    s <Key>osfEndLine:        end-of-line(extend)\n\
    :<Key>osfEndLine:         end-of-line()\n\
    s <Key>osfPageLeft:       page-left(extend)\n\
    :<Key>osfPageLeft:        page-left()\n\
    s c <Key>osfPageUp:       page-left(extend)\n\
    :c <Key>osfPageUp:        page-left()\n\
    s <Key>osfPageUp:         previous-page(extend)\n\
    :<Key>osfPageUp:          previous-page()\n\
    s <Key>osfPageRight:      page-right(extend)\n\
    :<Key>osfPageRight:       page-right()\n\
    s c <Key>osfPageDown:     page-right(extend)\n\
    :c <Key>osfPageDown:      page-right()\n\
    s <Key>osfPageDown:       next-page(extend)\n\
    :<Key>osfPageDown:        next-page()\n\
    :<Key>osfClear:           clear-selection()\n\
    :<Key>osfBackSpace:       delete-previous-character()\n\
    s m <Key>osfDelete:       cut-primary()\n\
    s a <Key>osfDelete:       cut-primary()\n\
    s <Key>osfDelete:         cut-clipboard()\n\
    :c <Key>osfDelete:        delete-to-end-of-line()\n\
    :<Key>osfDelete:          delete-next-character()\n\
    :<Key>osfInsert:          toggle-overstrike()\n\
    ";

XmConst char _XmTextIn_XmTextEventBindings2[] = "\
    :c m <Key>osfInsert:      copy-primary()\n\
    :c a <Key>osfInsert:      copy-primary()\n\
    s <Key>osfInsert:         paste-clipboard()\n\
    :c <Key>osfInsert:        copy-clipboard()\n\
    :s <Key>osfSelect:        key-select()\n\
    :<Key>osfSelect:          set-anchor()\n\
    :<Key>osfActivate:        activate()\n\
    :<Key>osfAddMode:         toggle-add-mode()\n\
    :<Key>osfHelp:            Help()\n\
    :<Key>osfCancel:          process-cancel()\n\
    s c <Key>osfLeft:         backward-word(extend)\n\
    :c <Key>osfLeft:          backward-word()\n\
    s <Key>osfLeft:           key-select(left)\n\
    :<Key>osfLeft:            backward-character()\n\
    s c <Key>osfRight:        forward-word(extend)\n\
    :c <Key>osfRight:         forward-word()\n\
    s <Key>osfRight:          key-select(right)\n\
    :<Key>osfRight:           forward-character()\n\
    s c <Key>osfUp:           backward-paragraph(extend)\n\
    :c <Key>osfUp:            backward-paragraph()\n\
    s <Key>osfUp:             process-shift-up()\n\
    :<Key>osfUp:              process-up()\n\
    s c <Key>osfDown:         forward-paragraph(extend)\n\
    :c <Key>osfDown:          forward-paragraph()\n\
    s <Key>osfDown:           process-shift-down()\n\
    :<Key>osfDown:            process-down()\n\
    c ~m ~a <Key>slash:       select-all()\n\
    c ~m ~a <Key>backslash:   deselect-all()\n\
    s ~m ~a <Key>Tab:         prev-tab-group()\n\
    c ~m ~a <Key>Tab:         next-tab-group()\n\
    ~m ~a <Key>Tab:           process-tab()\n\
    c ~s ~m ~a <Key>Return:   activate()\n\
    ~c ~s ~m ~a <Key>Return:  process-return()\n\
    c ~s ~m ~a <Key>space:    set-anchor()\n\
    c s ~m ~a <Key>space:     key-select()\n\
    s ~c ~m ~a <Key>space:    self-insert()\n\
    c ~m a <Key>f:            find-word()\n\
    <Key>:                    self-insert()\n\
    ";

XmConst char _XmTextIn_XmTextEventBindings3[] = "\
    <Unmap>:                  unmap()\n\
    <EnterWindow>:            enter()\n\
    <LeaveWindow>:            leave()\n\
    <FocusIn>:                focusIn()\n\
    <FocusOut>:               focusOut()\n\
    ~c s ~m ~a <Btn1Down>:    extend-start()\n\
    c ~s ~m ~a <Btn1Down>:    move-destination()\n\
    ~c ~s ~m ~a <Btn1Down>:   grab-focus()\n\
    ~c ~m ~a <Btn1Motion>:    extend-adjust()\n\
    ~c ~m ~a <Btn1Up>:        extend-end()\n\
    <Btn2Down>:               process-bdrag()\n\
    m ~a <Btn2Motion>:        secondary-adjust()\n\
    ~m a <Btn2Motion>:        secondary-adjust()\n\
    ~s <Btn2Up>:              copy-to()\n\
    ~c <Btn2Up>:              move-to()"
#if 1
    /* Scroll wheel */
    "\nShift<Btn4Down>:		page-left()"
    "\nShift<Btn5Down>:		page-right()"
    "\n<Btn4Down>:		scroll-one-line-up()"
    "\n<Btn5Down>:		scroll-one-line-down()"
#endif
    ;

XmConst char _XmToggleB_defaultTranslations[] = "\
    <EnterWindow>:            Enter()\n\
    <LeaveWindow>:            Leave()\n\
    <Btn1Down>:               Arm()\n\
    <Btn1Up>:                 Select() Disarm()\n\
    <Btn2Down>:               ProcessDrag()\n\
    :<Key>osfActivate:        PrimitiveParentActivate()\n\
    :<Key>osfCancel:          PrimitiveParentCancel()\n\
    :<Key>osfSelect:          ArmAndActivate()\n\
    :<Key>osfHelp:            Help()\n\
    ~s ~m ~a <Key>Return:     PrimitiveParentActivate()\n\
    ~s ~m ~a <Key>space:      ArmAndActivate()";

XmConst char _XmToggleB_menuTranslations[] = "\
    <EnterWindow>:            Enter()\n\
    <LeaveWindow>:            Leave()\n\
    <BtnDown>:                BtnDown()\n\
    <BtnUp>:                  BtnUp()\n\
    :<Key>osfSelect:          ArmAndActivate()\n\
    :<Key>osfActivate:        ArmAndActivate()\n\
    :<Key>osfHelp:            Help()\n\
    :<Key>osfCancel:          MenuEscape()\n\
    ~s ~m ~a <Key>Return:     ArmAndActivate()\n\
    ~s ~m ~a <Key>space:      ArmAndActivate()";

XmConst char _XmVirtKeys_fallbackBindingString[] = "\
    osfCancel:                <Key>Escape\n\
    osfLeft:                  <Key>Left\n\
    osfUp:                    <Key>Up\n\
    osfRight:                 <Key>Right\n\
    osfDown:                  <Key>Down\n\
    osfEndLine:               <Key>End\n\
    osfBeginLine:             <Key>Home\n\
    osfPageUp:                <Key>Prior\n\
    osfPageDown:              <Key>Next\n\
    osfBackSpace:             <Key>BackSpace\n\
    osfDelete:                <Key>Delete\n\
    osfInsert:                <Key>Insert\n\
    osfAddMode:               Shift<Key>F8\n\
    osfHelp:                  <Key>F1\n\
    osfMenu:                  Shift<Key>F10\n\
    osfMenuBar:               <Key>F10\n\
    osfSelect:                <Key>Select\n\
    osfActivate:              <Key>KP_Enter\n\
    osfClear:                 <Key>Clear\n\
    osfUndo:                  <Key>Undo";

XmConst char _XmVirtKeys_acornFallbackBindingString[] = "\
    osfCancel:                <Key>Escape\n\
    osfLeft:                  <Key>Left\n\
    osfUp:                    <Key>Up\n\
    osfRight:                 <Key>Right\n\
    osfDown:                  <Key>Down\n\
    osfEndLine:               Alt <Key>Right\n\
    osfBeginLine:             Alt <Key>Left\n\
    osfPageUp:                <Key>Prior\n\
    osfPageDown:              <Key>Next\n\
    osfBackSpace:             <Key>BackSpace\n\
    osfDelete:                <Key>Delete\n\
    osfInsert:                <Key>Insert\n\
    osfAddMode:               Shift <Key>F8\n\
    osfHelp:                  <Key>F1\n\
    osfMenu:                  Shift<Key>F10\n\
    osfMenuBar:               <Key>F10\n\
    osfActivate:              <Key>KP_Enter\n\
    osfCopy:                  <Key>Select";

XmConst char _XmVirtKeys_apolloFallbackBindingString[] = "\
    osfCancel:                <Key>Escape\n\
    osfLeft:                  <Key>Left\n\
    osfUp:                    <Key>Up\n\
    osfRight:                 <Key>Right\n\
    osfDown:                  <Key>Down\n\
    osfEndLine:               <Key>apRightBar\n\
    osfBeginLine:             <Key>apLeftBar\n\
    osfPageLeft:              <Key>apLeftBox\n\
    osfPageRight:             <Key>apRightBox\n\
    osfPageUp:                <Key>apUpBox\n\
    osfPageDown:              <Key>apDownBox\n\
    osfBackSpace:             <Key>BackSpace\n\
    osfDelete:                <Key>apCharDel\n\
    osfInsert:                <Key>Select\n\
    osfAddMode:               Shift<Key>F8\n\
    osfHelp:                  <Key>Help\n\
    osfMenu:                  Shift<Key>F10\n\
    osfMenuBar:               <Key>F10\n\
    osfCopy:                  <Key>apCopy\n\
    osfCut:                   <Key>apCut\n\
    osfPaste:                 <Key>apPaste\n\
    osfUndo:                  <Key>Undo";
    
XmConst char _XmVirtKeys_dgFallbackBindingString[] = "\
    osfCancel:                <Key>Escape\n\
    osfLeft:                  <Key>Left\n\
    osfUp:                    <Key>Up\n\
    osfRight:                 <Key>Right\n\
    osfDown:                  <Key>Down\n\
    osfEndLine:               <Key>End\n\
    osfBeginLine:             <Key>Home\n\
    osfPageUp:                <Key>Prior\n\
    osfPageDown:              <Key>Next\n\
    osfBackSpace:             <Key>BackSpace\n\
    osfDelete:                <Key>Delete\n\
    osfInsert:                <Key>Insert\n\
    osfAddMode:               Shift <Key>F8\n\
    osfHelp:                  <Key>F1\n\
    osfMenu:                  Shift<Key>F10\n\
    osfMenuBar:               <Key>F10 ";
    
XmConst char _XmVirtKeys_decFallbackBindingString[] = "\
    osfCancel:                <Key>Escape\n\
    osfLeft:                  <Key>Left\n\
    osfUp:                    <Key>Up\n\
    osfRight:                 <Key>Right\n\
    osfDown:                  <Key>Down\n\
    osfEndLine:               Alt<Key>Right\n\
    osfBeginLine:             Alt<Key>Left\n\
    osfPageUp:                <Key>Prior\n\
    osfPageDown:              <Key>Next\n\
    osfBackSpace:             <Key>Delete\n\
    osfDelete:                <Key>DRemove\n\
    osfInsert:                <Key>Insert\n\
    osfAddMode:               Shift<Key>F8\n\
    osfHelp:                  <Key>Help\n\
    osfMenu:                  Shift<Key>F10\n\
    osfMenuBar:               <Key>F10\n\
    osfSelect:                <Key>Select\n\
    osfActivate:              <Key>KP_Enter";

XmConst char _XmVirtKeys_dblclkFallbackBindingString[] = "\
    osfCancel:                <Key>Escape\n\
    osfLeft:                  <Key>Left\n\
    osfUp:                    <Key>Up\n\
    osfRight:                 <Key>Right\n\
    osfDown:                  <Key>Down\n\
    osfEndLine:               <Key>End\n\
    osfBeginLine:             <Key>Home\n\
    osfPageUp:                <Key>Prior\n\
    osfPageDown:              <Key>Next\n\
    osfBackSpace:             <Key>BackSpace\n\
    osfDelete:                <Key>Delete\n\
    osfInsert:                <Key>Insert\n\
    osfAddMode:               Shift <Key>F8\n\
    osfHelp:                  <Key>F1\n\
    osfMenu:                  Shift<Key>F10\n\
    osfMenuBar:               <Key>F10";
    
XmConst char _XmVirtKeys_ingrFallbackBindingString[] = "\
    osfCancel:                <Key>Escape\n\
    osfLeft:                  <Key>Left\n\
    osfUp:                    <Key>Up\n\
    osfRight:                 <Key>Right\n\
    osfDown:                  <Key>Down\n\
    osfEndLine:               Alt<Key>Right\n\
    osfBeginLine:             Alt<Key>Left\n\
    osfPageUp:                <Key>Prior\n\
    osfPageDown:              <Key>Next\n\
    osfBackSpace:             <Key>BackSpace\n\
    osfDelete:                <Key>Delete\n\
    osfInsert:                <Key>Insert\n\
    osfAddMode:               Shift<Key>F8\n\
    osfHelp:                  <Key>Help\n\
    osfMenu:                  Shift<Key>F10\n\
    osfMenuBar:               <Key>F10";
    
XmConst char _XmVirtKeys_megatekFallbackBindingString[] = "\
    osfCancel:                <Key>Escape\n\
    osfLeft:                  <Key>Left\n\
    osfUp:                    <Key>Up\n\
    osfRight:                 <Key>Right\n\
    osfDown:                  <Key>Down\n\
    osfEndLine:               <Key>R13\n\
    osfBeginLine:             <Key>F27\n\
    osfPageUp:                <Key>F29\n\
    osfPageDown:              <Key>F35\n\
    osfBackSpace:             <Key>BackSpace\n\
    osfDelete:                <Key>Delete\n\
    osfInsert:                <Key>Insert\n\
    osfAddMode:               Shift <Key>F8\n\
    osfHelp:                  <Key>Help\n\
    osfMenu:                  Shift<Key>F10\n\
    osfMenuBar:               <Key>F10\n\
    osfCopy:                  <Key>F16\n\
    osfCut:                   <Key>F20\n\
    osfPaste:                 <Key>F18\n\
    osfUndo:                  <Key>F14";
    
XmConst char _XmVirtKeys_motorolaFallbackBindingString[] = "\
    osfCancel:                <Key>Escape\n\
    osfLeft:                  <Key>Left\n\
    osfUp:                    <Key>Up\n\
    osfRight:                 <Key>Right\n\
    osfDown:                  <Key>Down\n\
    osfEndLine:               <Key>End\n\
    osfBeginLine:             <Key>Home\n\
    osfPageUp:                <Key>Prior\n\
    osfPageDown:              <Key>Next\n\
    osfBackSpace:             <Key>BackSpace\n\
    osfDelete:                <Key>Delete\n\
    osfInsert:                <Key>Insert\n\
    osfAddMode:               Shift <Key>F8\n\
    osfHelp:                  <Key>F1\n\
    osfMenu:                  Shift<Key>F10\n\
    osfMenuBar:               <Key>F10";
    
XmConst char _XmVirtKeys_siemensWx200FallbackBindingString[] = "\
    osfCancel:                <Key>Escape\n\
    osfLeft:                  <Key>Left\n\
    osfUp:                    <Key>Up\n\
    osfRight:                 <Key>Right\n\
    osfDown:                  <Key>Down\n\
    osfEndLine:               <Key>Cancel\n\
    osfBeginLine:             <Key>Home\n\
    osfPageUp:                <Key>Prior\n\
    osfPageDown:              <Key>Next\n\
    osfBackSpace:             <Key>BackSpace\n\
    osfDelete:                <Key>Delete\n\
    osfInsert:                <Key>Insert\n\
    osfAddMode:               Shift <Key>F8\n\
    osfHelp:                  <Key>Help\n\
    osfActivate:              <Key>KP_Enter\n\
    osfMenu:                  <Key>Menu\n\
    osfMenuBar:               <Key>F10";
    
XmConst char _XmVirtKeys_siemens9733FallbackBindingString[] = "\
    osfCancel:                <Key>Escape\n\
    osfLeft:                  <Key>Left\n\
    osfUp:                    <Key>Up\n\
    osfRight:                 <Key>Right\n\
    osfDown:                  <Key>Down\n\
    osfEndLine:               <Key>End\n\
    osfBeginLine:             <Key>Home\n\
    osfPageUp:                <Key>Prior\n\
    osfPageDown:              <Key>Next\n\
    osfBackSpace:             <Key>BackSpace\n\
    osfDelete:                <Key>Delete_char\n\
    osfInsert:                <Key>Insert_char\n\
    osfAddMode:               Shift <Key>F8\n\
    osfHelp:                  <Key>Help\n\
    osfMenu:                  <Key>Linefeed\n\
    osfMenuBar:               <Key>F10";
    
XmConst char _XmVirtKeys_tekFallbackBindingString[] = "\
    osfCancel:                <Key>Escape\n\
    osfLeft:                  <Key>Left\n\
    osfUp:                    <Key>Up\n\
    osfRight:                 <Key>Right\n\
    osfDown:                  <Key>Down\n\
    osfEndLine:               <Key>End\n\
    osfBeginLine:             <Key>Home\n\
    osfPageUp:                <Key>Prior\n\
    osfPageDown:              <Key>Next\n\
    osfBackSpace:             <Key>BackSpace\n\
    osfDelete:                <Key>Delete\n\
    osfInsert:                <Key>Insert\n\
    osfAddMode:               Shift <Key>F8\n\
    osfHelp:                  <Key>F1\n\
    osfMenu:                  Shift<Key>F10\n\
    osfMenuBar:               <Key>F10";
