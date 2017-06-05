/********************************************************************************
*                                                                               *
*                     T h e   A d i e   T e x t   E d i t o r                   *
*                                                                               *
*********************************************************************************
* Copyright (C) 1998,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* This program is free software; you can redistribute it and/or modify          *
* it under the terms of the GNU General Public License as published by          *
* the Free Software Foundation; either version 2 of the License, or             *
* (at your option) any later version.                                           *
*                                                                               *
* This program is distributed in the hope that it will be useful,               *
* but WITHOUT ANY WARRANTY; without even the implied warranty of                *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                 *
* GNU General Public License for more details.                                  *
*                                                                               *
* You should have received a copy of the GNU General Public License             *
* along with this program; if not, write to the Free Software                   *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.    *
*********************************************************************************
* $Id: TextWindow.cpp,v 1.125.2.3 2008/03/11 02:19:42 fox Exp $                     *
********************************************************************************/
#include "fx.h"
#include "fxkeys.h"
#include <new>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <signal.h>
#ifndef WIN32
#include <unistd.h>
#endif
#include <ctype.h>
#include "FXRex.h"
#include "FXArray.h"
#include "HelpWindow.h"
#include "Preferences.h"
#include "Commands.h"
#include "Hilite.h"
#include "TextWindow.h"
#include "Adie.h"
#include "icons.h"


/*
  Note:
  - Commenting/uncommenting source code (various programming languages, of course).
  - Block select (and block operations).
  - Better icons.
  - Syntax highlighting (programmable).
  - Shell commands.
  - C tags support.
  - Each style has optional parent; colors with value FXRGBA(0,0,0,0) are
    inherited from the parent; this way, sub-styles are possible.
  - Bookmarks should be adjusted when text is added or removed:- bookmarks
    inside a deleted area should be removed.
  - If there is text selected, ctrl-H goes to the next occurrence.  If there
    is nothing selected, ctrl-H would go to the previous search pattern, similar
    to the way ctrl-G works in nedit.
  - If there is a number selected, ctrl-L goes to that line number.  If there
    is nothing selected, ctrl-L pops the goto dialog.
  - Have an option to beep when the search wraps back to the top of the file.
  - When entire lines are highlighted through triple click and then dragged, a
    drop at the start of the destination line would seem more natural.
  - A more serious problem occurs when you undo a drag and drop.  If you undo,
    (using ctrl-Z for example), the paste is reversed, but not the cut.  You have
    to hit undo again to reverse the cut.  It would be far more natural to have
    the "combination" type operations, such as a move, (cut then paste), be
    somehow recorded as a single operation in the undo system.
  - Ctrl-B does not seem to be used for anything now.  Could we use this for the
    block select.  The shift-alt-{ does not flow from my fingers easily.  Just a
    preference...
  - The C++ comment/uncomment of selected lines would be very useful.  I didn't
    realize how much I used it until it wasn't there.
  - When the auto indent is turned on, and you press return, the start of the
    next line is the same as the previous line, which is good.  If the next key
    that's pressed is the backspace, it would be nice if the caret could back up
    a full indention level.  (e.g. We use two spaces for emulated tabs.  The
    backspace would back up two spaces...)
  - Would be nice if we could remember not only bookmarks, but also window
    size/position based on file name.
  - Add option to preferences for text widget non-tracking sliders, i.e. use
    jump-scrolling for slow machines/network connections.
  - Close last window just saves text, then starts new document in last window;
    in other words, only quit will terminate app.
  - Maybe FXText should have its own accelerator table so that key bindings
    may be changed.
  - Would be nice to save as HTML.
*/

#define CLOCKTIMER      500
#define FILETIMER       1000
#define RESTYLEJUMP     80

/*******************************************************************************/

// Map
FXDEFMAP(TextWindow) TextWindowMap[]={
  FXMAPFUNC(SEL_UPDATE,            0,                              TextWindow::onUpdate),
  FXMAPFUNC(SEL_FOCUSIN,           0,                              TextWindow::onFocusIn),
  FXMAPFUNC(SEL_TIMEOUT,           TextWindow::ID_CLOCKTIME,       TextWindow::onClock),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_ABOUT,           TextWindow::onCmdAbout),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_HELP,            TextWindow::onCmdHelp),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_NEW,             TextWindow::onCmdNew),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_OPEN,            TextWindow::onCmdOpen),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_OPEN_SELECTED,   TextWindow::onCmdOpenSelected),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_OPEN_TREE,       TextWindow::onCmdOpenTree),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_OPEN_RECENT,     TextWindow::onCmdOpenRecent),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_REOPEN,          TextWindow::onCmdReopen),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_REOPEN,          TextWindow::onUpdReopen),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_SAVE,            TextWindow::onCmdSave),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_SAVE,            TextWindow::onUpdSave),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_SAVEAS,          TextWindow::onCmdSaveAs),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_FONT,            TextWindow::onCmdFont),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_PRINT,           TextWindow::onCmdPrint),

  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_TEXT_BACK,       TextWindow::onCmdTextBackColor),
  FXMAPFUNC(SEL_CHANGED,           TextWindow::ID_TEXT_BACK,       TextWindow::onCmdTextBackColor),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_TEXT_BACK,       TextWindow::onUpdTextBackColor),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_TEXT_FORE,       TextWindow::onCmdTextForeColor),
  FXMAPFUNC(SEL_CHANGED,           TextWindow::ID_TEXT_FORE,       TextWindow::onCmdTextForeColor),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_TEXT_FORE,       TextWindow::onUpdTextForeColor),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_TEXT_SELBACK,    TextWindow::onCmdTextSelBackColor),
  FXMAPFUNC(SEL_CHANGED,           TextWindow::ID_TEXT_SELBACK,    TextWindow::onCmdTextSelBackColor),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_TEXT_SELBACK,    TextWindow::onUpdTextSelBackColor),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_TEXT_SELFORE,    TextWindow::onCmdTextSelForeColor),
  FXMAPFUNC(SEL_CHANGED,           TextWindow::ID_TEXT_SELFORE,    TextWindow::onCmdTextSelForeColor),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_TEXT_SELFORE,    TextWindow::onUpdTextSelForeColor),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_TEXT_HILITEBACK, TextWindow::onCmdTextHiliteBackColor),
  FXMAPFUNC(SEL_CHANGED,           TextWindow::ID_TEXT_HILITEBACK, TextWindow::onCmdTextHiliteBackColor),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_TEXT_HILITEBACK, TextWindow::onUpdTextHiliteBackColor),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_TEXT_HILITEFORE, TextWindow::onCmdTextHiliteForeColor),
  FXMAPFUNC(SEL_CHANGED,           TextWindow::ID_TEXT_HILITEFORE, TextWindow::onCmdTextHiliteForeColor),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_TEXT_HILITEFORE, TextWindow::onUpdTextHiliteForeColor),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_TEXT_CURSOR,     TextWindow::onCmdTextCursorColor),
  FXMAPFUNC(SEL_CHANGED,           TextWindow::ID_TEXT_CURSOR,     TextWindow::onCmdTextCursorColor),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_TEXT_CURSOR,     TextWindow::onUpdTextCursorColor),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_TEXT_ACTIVEBACK, TextWindow::onCmdTextActBackColor),
  FXMAPFUNC(SEL_CHANGED,           TextWindow::ID_TEXT_ACTIVEBACK, TextWindow::onCmdTextActBackColor),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_TEXT_ACTIVEBACK, TextWindow::onUpdTextActBackColor),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_TEXT_NUMBACK,    TextWindow::onCmdTextBarColor),
  FXMAPFUNC(SEL_CHANGED,           TextWindow::ID_TEXT_NUMBACK,    TextWindow::onCmdTextBarColor),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_TEXT_NUMBACK,    TextWindow::onUpdTextBarColor),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_TEXT_NUMFORE,    TextWindow::onCmdTextNumberColor),
  FXMAPFUNC(SEL_CHANGED,           TextWindow::ID_TEXT_NUMFORE,    TextWindow::onCmdTextNumberColor),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_TEXT_NUMFORE,    TextWindow::onUpdTextNumberColor),

  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_DIR_BACK,        TextWindow::onCmdDirBackColor),
  FXMAPFUNC(SEL_CHANGED,           TextWindow::ID_DIR_BACK,        TextWindow::onCmdDirBackColor),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_DIR_BACK,        TextWindow::onUpdDirBackColor),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_DIR_FORE,        TextWindow::onCmdDirForeColor),
  FXMAPFUNC(SEL_CHANGED,           TextWindow::ID_DIR_FORE,        TextWindow::onCmdDirForeColor),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_DIR_FORE,        TextWindow::onUpdDirForeColor),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_DIR_SELBACK,     TextWindow::onCmdDirSelBackColor),
  FXMAPFUNC(SEL_CHANGED,           TextWindow::ID_DIR_SELBACK,     TextWindow::onCmdDirSelBackColor),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_DIR_SELBACK,     TextWindow::onUpdDirSelBackColor),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_DIR_SELFORE,     TextWindow::onCmdDirSelForeColor),
  FXMAPFUNC(SEL_CHANGED,           TextWindow::ID_DIR_SELFORE,     TextWindow::onCmdDirSelForeColor),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_DIR_SELFORE,     TextWindow::onUpdDirSelForeColor),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_DIR_LINES,       TextWindow::onCmdDirLineColor),
  FXMAPFUNC(SEL_CHANGED,           TextWindow::ID_DIR_LINES,       TextWindow::onCmdDirLineColor),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_DIR_LINES,       TextWindow::onUpdDirLineColor),

  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_TOGGLE_WRAP,     TextWindow::onUpdWrap),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_TOGGLE_WRAP,     TextWindow::onCmdWrap),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_SAVE_SETTINGS,   TextWindow::onCmdSaveSettings),
  FXMAPFUNC(SEL_CHANGED,           TextWindow::ID_TEXT,            TextWindow::onTextChanged),
  FXMAPFUNC(SEL_INSERTED,          TextWindow::ID_TEXT,            TextWindow::onTextInserted),
  FXMAPFUNC(SEL_REPLACED,          TextWindow::ID_TEXT,            TextWindow::onTextReplaced),
  FXMAPFUNC(SEL_DELETED,           TextWindow::ID_TEXT,            TextWindow::onTextDeleted),
  FXMAPFUNC(SEL_RIGHTBUTTONRELEASE,TextWindow::ID_TEXT,            TextWindow::onTextRightMouse),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_FIXED_WRAP,      TextWindow::onUpdWrapFixed),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_FIXED_WRAP,      TextWindow::onCmdWrapFixed),
  FXMAPFUNC(SEL_DND_MOTION,        TextWindow::ID_TEXT,            TextWindow::onEditDNDMotion),
  FXMAPFUNC(SEL_DND_DROP,          TextWindow::ID_TEXT,            TextWindow::onEditDNDDrop),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_STRIP_CR,        TextWindow::onUpdStripReturns),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_STRIP_CR,        TextWindow::onCmdStripReturns),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_STRIP_SP,        TextWindow::onUpdStripSpaces),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_STRIP_SP,        TextWindow::onCmdStripSpaces),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_APPEND_NL,       TextWindow::onUpdAppendNewline),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_APPEND_NL,       TextWindow::onCmdAppendNewline),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_INCLUDE_PATH,    TextWindow::onCmdIncludePaths),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_FILEFILTER,      TextWindow::onCmdFilter),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_OVERSTRIKE,      TextWindow::onUpdOverstrike),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_READONLY,        TextWindow::onUpdReadOnly),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_NUM_ROWS,        TextWindow::onUpdNumRows),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_PREFERENCES,     TextWindow::onCmdPreferences),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_TABCOLUMNS,      TextWindow::onCmdTabColumns),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_TABCOLUMNS,      TextWindow::onUpdTabColumns),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_DELIMITERS,      TextWindow::onCmdDelimiters),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_DELIMITERS,      TextWindow::onUpdDelimiters),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_WRAPCOLUMNS,     TextWindow::onCmdWrapColumns),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_WRAPCOLUMNS,     TextWindow::onUpdWrapColumns),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_AUTOINDENT,      TextWindow::onCmdAutoIndent),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_AUTOINDENT,      TextWindow::onUpdAutoIndent),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_INSERTTABS,      TextWindow::onCmdInsertTabs),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_INSERTTABS,      TextWindow::onUpdInsertTabs),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_BRACEMATCH,      TextWindow::onCmdBraceMatch),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_BRACEMATCH,      TextWindow::onUpdBraceMatch),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_INSERT_FILE,     TextWindow::onUpdInsertFile),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_INSERT_FILE,     TextWindow::onCmdInsertFile),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_EXTRACT_FILE,    TextWindow::onUpdExtractFile),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_EXTRACT_FILE,    TextWindow::onCmdExtractFile),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_WHEELADJUST,     TextWindow::onUpdWheelAdjust),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_WHEELADJUST,     TextWindow::onCmdWheelAdjust),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_NEXT_MARK,       TextWindow::onUpdNextMark),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_NEXT_MARK,       TextWindow::onCmdNextMark),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_PREV_MARK,       TextWindow::onUpdPrevMark),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_PREV_MARK,       TextWindow::onCmdPrevMark),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_SET_MARK,        TextWindow::onUpdSetMark),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_SET_MARK,        TextWindow::onCmdSetMark),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_CLEAR_MARKS,     TextWindow::onCmdClearMarks),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_SAVEMARKS,       TextWindow::onUpdSaveMarks),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_SAVEMARKS,       TextWindow::onCmdSaveMarks),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_SAVEVIEWS,       TextWindow::onUpdSaveViews),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_SAVEVIEWS,       TextWindow::onCmdSaveViews),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_SHOWACTIVE,      TextWindow::onUpdShowActive),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_SHOWACTIVE,      TextWindow::onCmdShowActive),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_TEXT_LINENUMS,   TextWindow::onUpdLineNumbers),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_TEXT_LINENUMS,   TextWindow::onCmdLineNumbers),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_WARNCHANGED,     TextWindow::onUpdWarnChanged),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_WARNCHANGED,     TextWindow::onCmdWarnChanged),

  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_SYNTAX,          TextWindow::onUpdSyntax),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_SYNTAX,          TextWindow::onCmdSyntax),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_RESTYLE,         TextWindow::onUpdRestyle),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_RESTYLE,         TextWindow::onCmdRestyle),
  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_JUMPSCROLL,      TextWindow::onUpdJumpScroll),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_JUMPSCROLL,      TextWindow::onCmdJumpScroll),
  FXMAPFUNCS(SEL_UPDATE,           TextWindow::ID_WINDOW_1,        TextWindow::ID_WINDOW_10,    TextWindow::onUpdWindow),
  FXMAPFUNCS(SEL_COMMAND,          TextWindow::ID_WINDOW_1,        TextWindow::ID_WINDOW_10,    TextWindow::onCmdWindow),
  FXMAPFUNCS(SEL_UPDATE,           TextWindow::ID_SYNTAX_FIRST,    TextWindow::ID_SYNTAX_LAST,  TextWindow::onUpdSyntaxSwitch),
  FXMAPFUNCS(SEL_COMMAND,          TextWindow::ID_SYNTAX_FIRST,    TextWindow::ID_SYNTAX_LAST,  TextWindow::onCmdSyntaxSwitch),

  FXMAPFUNC(SEL_UPDATE,            TextWindow::ID_STYLE_INDEX,     TextWindow::onUpdStyleIndex),
  FXMAPFUNC(SEL_COMMAND,           TextWindow::ID_STYLE_INDEX,     TextWindow::onCmdStyleIndex),
  FXMAPFUNCS(SEL_UPDATE,           TextWindow::ID_STYLE_NORMAL_FG, TextWindow::ID_STYLE_ACTIVE_BG, TextWindow::onUpdStyleColor),
  FXMAPFUNCS(SEL_CHANGED,          TextWindow::ID_STYLE_NORMAL_FG, TextWindow::ID_STYLE_ACTIVE_BG, TextWindow::onCmdStyleColor),
  FXMAPFUNCS(SEL_COMMAND,          TextWindow::ID_STYLE_NORMAL_FG, TextWindow::ID_STYLE_ACTIVE_BG, TextWindow::onCmdStyleColor),
  FXMAPFUNCS(SEL_UPDATE,           TextWindow::ID_STYLE_UNDERLINE, TextWindow::ID_STYLE_BOLD,      TextWindow::onUpdStyleFlags),
  FXMAPFUNCS(SEL_COMMAND,          TextWindow::ID_STYLE_UNDERLINE, TextWindow::ID_STYLE_BOLD,      TextWindow::onCmdStyleFlags),
  };


// Object implementation
FXIMPLEMENT(TextWindow,FXMainWindow,TextWindowMap,ARRAYNUMBER(TextWindowMap))


/*******************************************************************************/

// Make some windows
TextWindow::TextWindow(Adie* a,const FXString& file):FXMainWindow(a,"Adie",NULL,NULL,DECOR_ALL,0,0,850,600,0,0),mrufiles(a){

  // Add to list of windows
  getApp()->windowlist.append(this);

  // Default font
  font=NULL;

  // Application icons
  setIcon(getApp()->bigicon);
  setMiniIcon(getApp()->smallicon);

  // Status bar
  statusbar=new FXStatusBar(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|STATUSBAR_WITH_DRAGCORNER|FRAME_RAISED);

  // Sites where to dock
  topdock=new FXDockSite(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X);
  bottomdock=new FXDockSite(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X);
  leftdock=new FXDockSite(this,LAYOUT_SIDE_LEFT|LAYOUT_FILL_Y);
  rightdock=new FXDockSite(this,LAYOUT_SIDE_RIGHT|LAYOUT_FILL_Y);

  // Make menu bar
  dragshell1=new FXToolBarShell(this,FRAME_RAISED);
  menubar=new FXMenuBar(topdock,dragshell1,LAYOUT_DOCK_NEXT|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|FRAME_RAISED);
  new FXToolBarGrip(menubar,menubar,FXMenuBar::ID_TOOLBARGRIP,TOOLBARGRIP_DOUBLE);

  // Tool bar
  dragshell2=new FXToolBarShell(this,FRAME_RAISED);
  toolbar=new FXToolBar(topdock,dragshell2,LAYOUT_DOCK_NEXT|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|FRAME_RAISED);
  new FXToolBarGrip(toolbar,toolbar,FXToolBar::ID_TOOLBARGRIP,TOOLBARGRIP_DOUBLE);

  // Info about the editor
  new FXButton(statusbar,tr("\tAbout Adie\tAbout the Adie text editor."),getApp()->smallicon,this,ID_ABOUT,LAYOUT_FILL_Y|LAYOUT_RIGHT);

  // File menu
  filemenu=new FXMenuPane(this);
  new FXMenuTitle(menubar,tr("&File"),NULL,filemenu);

  // Edit Menu
  editmenu=new FXMenuPane(this);
  new FXMenuTitle(menubar,tr("&Edit"),NULL,editmenu);

  // Goto Menu
  gotomenu=new FXMenuPane(this);
  new FXMenuTitle(menubar,tr("&Goto"),NULL,gotomenu);

  // Search Menu
  searchmenu=new FXMenuPane(this);
  new FXMenuTitle(menubar,tr("&Search"),NULL,searchmenu);

  // Options Menu
  optionmenu=new FXMenuPane(this);
  new FXMenuTitle(menubar,tr("&Options"),NULL,optionmenu);

  // View menu
  viewmenu=new FXMenuPane(this);
  new FXMenuTitle(menubar,tr("&View"),NULL,viewmenu);

  // Window menu
  windowmenu=new FXMenuPane(this);
  new FXMenuTitle(menubar,tr("&Window"),NULL,windowmenu);

  // Help menu
  helpmenu=new FXMenuPane(this);
  new FXMenuTitle(menubar,tr("&Help"),NULL,helpmenu,LAYOUT_RIGHT);

  // Splitter
  FXSplitter* splitter=new FXSplitter(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y|SPLITTER_TRACKING);

  // Sunken border for tree
  treebox=new FXVerticalFrame(splitter,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0);

  // Make tree
  FXHorizontalFrame* treeframe=new FXHorizontalFrame(treebox,FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0);
  dirlist=new FXDirList(treeframe,this,ID_OPEN_TREE,DIRLIST_SHOWFILES|DIRLIST_NO_OWN_ASSOC|TREELIST_BROWSESELECT|TREELIST_SHOWS_LINES|TREELIST_SHOWS_BOXES|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  dirlist->setAssociations(getApp()->associations);
  FXHorizontalFrame* filterframe=new FXHorizontalFrame(treebox,LAYOUT_FILL_X,0,0,0,0, 4,0,0,4);
  new FXLabel(filterframe,tr("Filter:"),NULL,LAYOUT_CENTER_Y);
  filter=new FXComboBox(filterframe,25,this,ID_FILEFILTER,COMBOBOX_STATIC|LAYOUT_FILL_X|FRAME_SUNKEN|FRAME_THICK);
  filter->setNumVisible(4);

  // Sunken border for text widget
  FXHorizontalFrame *textbox=new FXHorizontalFrame(splitter,FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0);

  // Make editor window
  editor=new FXText(textbox,this,ID_TEXT,LAYOUT_FILL_X|LAYOUT_FILL_Y|TEXT_SHOWACTIVE);
  editor->setHiliteMatchTime(2000);
  editor->setBarColumns(6);

  // Show clock on status bar
  clock=new FXTextField(statusbar,8,NULL,0,FRAME_SUNKEN|JUSTIFY_RIGHT|LAYOUT_RIGHT|LAYOUT_CENTER_Y|TEXTFIELD_READONLY,0,0,0,0,2,2,1,1);
  clock->setBackColor(statusbar->getBackColor());

  // Undo/redo block
  undoredoblock=new FXHorizontalFrame(statusbar,LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0, 0,0,0,0);
  new FXLabel(undoredoblock,tr("  Undo:"),NULL,LAYOUT_CENTER_Y);
  FXTextField* undocount=new FXTextField(undoredoblock,6,&undolist,FXUndoList::ID_UNDO_COUNT,TEXTFIELD_READONLY|FRAME_SUNKEN|JUSTIFY_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,2,2,1,1);
  undocount->setBackColor(statusbar->getBackColor());
  new FXLabel(undoredoblock,tr("  Redo:"),NULL,LAYOUT_CENTER_Y);
  FXTextField* redocount=new FXTextField(undoredoblock,6,&undolist,FXUndoList::ID_REDO_COUNT,TEXTFIELD_READONLY|FRAME_SUNKEN|JUSTIFY_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,2,2,1,1);
  redocount->setBackColor(statusbar->getBackColor());

  // Show readonly state in status bar
  FXLabel* readonly=new FXLabel(statusbar,FXString::null,NULL,FRAME_SUNKEN|JUSTIFY_RIGHT|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,2,2,1,1);
  readonly->setTarget(this);
  readonly->setSelector(ID_READONLY);

  // Show insert mode in status bar
  FXLabel* overstrike=new FXLabel(statusbar,FXString::null,NULL,FRAME_SUNKEN|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,2,2,1,1);
  overstrike->setTarget(this);
  overstrike->setSelector(ID_OVERSTRIKE);

  // Show size of text in status bar
  FXTextField* numchars=new FXTextField(statusbar,7,this,ID_NUM_ROWS,TEXTFIELD_READONLY|FRAME_SUNKEN|JUSTIFY_RIGHT|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,2,2,1,1);
  numchars->setBackColor(statusbar->getBackColor());

  // Caption before number
  new FXLabel(statusbar,tr("  Lines:"),NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y);

  // Show column number in status bar
  FXTextField* columnno=new FXTextField(statusbar,7,editor,FXText::ID_CURSOR_COLUMN,FRAME_SUNKEN|JUSTIFY_RIGHT|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,2,2,1,1);
  columnno->setBackColor(statusbar->getBackColor());

  // Caption before number
  new FXLabel(statusbar,tr("  Col:"),NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y);

  // Show line number in status bar
  FXTextField* rowno=new FXTextField(statusbar,7,editor,FXText::ID_CURSOR_ROW,FRAME_SUNKEN|JUSTIFY_RIGHT|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,2,2,1,1);
  rowno->setBackColor(statusbar->getBackColor());

  // Caption before number
  new FXLabel(statusbar,tr("  Line:"),NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y);

  // Toobar buttons: File manipulation
  new FXButton(toolbar,tr("\tNew\tCreate new document."),getApp()->newicon,this,ID_NEW,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXButton(toolbar,tr("\tOpen\tOpen document file."),getApp()->openicon,this,ID_OPEN,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXButton(toolbar,tr("\tSave\tSave document."),getApp()->saveicon,this,ID_SAVE,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXButton(toolbar,tr("\tSave As\tSave document to another file."),getApp()->saveasicon,this,ID_SAVEAS,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

  // Spacer
  new FXSeparator(toolbar,SEPARATOR_GROOVE);

  // Toobar buttons: Print
  new FXButton(toolbar,"\tPrint\tPrint document.",getApp()->printicon,this,ID_PRINT,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

  // Spacer
  new FXSeparator(toolbar,SEPARATOR_GROOVE);

  // Toobar buttons: Editing
  new FXButton(toolbar,tr("\tCut\tCut selection to clipboard."),getApp()->cuticon,editor,FXText::ID_CUT_SEL,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXButton(toolbar,tr("\tCopy\tCopy selection to clipboard."),getApp()->copyicon,editor,FXText::ID_COPY_SEL,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXButton(toolbar,tr("\tPaste\tPaste clipboard."),getApp()->pasteicon,editor,FXText::ID_PASTE_SEL,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXButton(toolbar,tr("\tDelete\t\tDelete selection."),getApp()->deleteicon,editor,FXText::ID_DELETE_SEL,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

  // Spacer
  new FXSeparator(toolbar,SEPARATOR_GROOVE);

  // Undo/redo
  new FXButton(toolbar,tr("\tUndo\tUndo last change."),getApp()->undoicon,&undolist,FXUndoList::ID_UNDO,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXButton(toolbar,tr("\tRedo\tRedo last undo."),getApp()->redoicon,&undolist,FXUndoList::ID_REDO,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

  // Spacer
  new FXSeparator(toolbar,SEPARATOR_GROOVE);

  // Search
  new FXButton(toolbar,tr("\tSearch\tSearch text."),getApp()->searchicon,editor,FXText::ID_SEARCH,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXButton(toolbar,tr("\tSearch Previous Selected\tSearch previous occurrence of selected text."),getApp()->searchprevicon,editor,FXText::ID_SEARCH_BACK_SEL,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXButton(toolbar,tr("\tSearch Next Selected\tSearch next occurrence of selected text."),getApp()->searchnexticon,editor,FXText::ID_SEARCH_FORW_SEL,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

  // Spacer
  new FXSeparator(toolbar,SEPARATOR_GROOVE);

  // Bookmarks
  new FXButton(toolbar,tr("\tBookmark\tSet bookmark."),getApp()->bookseticon,this,ID_SET_MARK,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXButton(toolbar,tr("\tPrevious Bookmark\tGoto previous bookmark."),getApp()->bookprevicon,this,ID_PREV_MARK,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXButton(toolbar,tr("\tNext Bookmark\tGoto next bookmark."),getApp()->booknexticon,this,ID_NEXT_MARK,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXButton(toolbar,tr("\tDelete Bookmarks\tDelete all bookmarks."),getApp()->bookdelicon,this,ID_CLEAR_MARKS,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

  // Spacer
  new FXSeparator(toolbar,SEPARATOR_GROOVE);

  new FXButton(toolbar,tr("\tShift left\tShift text left by one."),getApp()->shiftlefticon,editor,FXText::ID_SHIFT_LEFT,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXButton(toolbar,tr("\tShift right\tShift text right by one."),getApp()->shiftrighticon,editor,FXText::ID_SHIFT_RIGHT,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

  // Spacer
  new FXSeparator(toolbar,SEPARATOR_GROOVE);

  // Color
  new FXButton(toolbar,tr("\tFonts\tDisplay font dialog."),getApp()->fontsicon,this,ID_FONT,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

  // Help
  new FXButton(toolbar,tr("\tDisplay help\tDisplay online help information."),getApp()->helpicon,this,ID_HELP,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_RIGHT);

  // File Menu entries
  new FXMenuCommand(filemenu,tr("&New...\tCtl-N\tCreate new document."),getApp()->newicon,this,ID_NEW);
  new FXMenuCommand(filemenu,tr("&Open...\tCtl-O\tOpen document file."),getApp()->openicon,this,ID_OPEN);
  new FXMenuCommand(filemenu,tr("Open Selected...  \tCtl-E\tOpen highlighted document file."),NULL,this,ID_OPEN_SELECTED);
  new FXMenuCommand(filemenu,tr("&Reopen...\t\tReopen file."),getApp()->reloadicon,this,ID_REOPEN);
  new FXMenuCommand(filemenu,tr("&Save\tCtl-S\tSave changes to file."),getApp()->saveicon,this,ID_SAVE);
  new FXMenuCommand(filemenu,tr("Save &As...\t\tSave document to another file."),getApp()->saveasicon,this,ID_SAVEAS);
  new FXMenuCommand(filemenu,tr("&Close\tCtl-W\tClose document."),NULL,this,ID_CLOSE);
  new FXMenuSeparator(filemenu);
  new FXMenuCommand(filemenu,tr("Insert from file...\t\tInsert text from file."),NULL,this,ID_INSERT_FILE);
  new FXMenuCommand(filemenu,tr("Extract to file...\t\tExtract text to file."),NULL,this,ID_EXTRACT_FILE);
  new FXMenuCommand(filemenu,tr("&Print...\tCtl-P\tPrint document."),getApp()->printicon,this,ID_PRINT);
  new FXMenuCheck(filemenu,tr("&Editable\t\tDocument editable."),editor,FXText::ID_TOGGLE_EDITABLE);

  // Recent file menu; this automatically hides if there are no files
  FXMenuSeparator* sep1=new FXMenuSeparator(filemenu);
  sep1->setTarget(&mrufiles);
  sep1->setSelector(FXRecentFiles::ID_ANYFILES);
  new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_1);
  new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_2);
  new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_3);
  new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_4);
  new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_5);
  new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_6);
  new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_7);
  new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_8);
  new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_9);
  new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_10);
  new FXMenuCommand(filemenu,tr("&Clear Recent Files"),NULL,&mrufiles,FXRecentFiles::ID_CLEAR);
  FXMenuSeparator* sep2=new FXMenuSeparator(filemenu);
  sep2->setTarget(&mrufiles);
  sep2->setSelector(FXRecentFiles::ID_ANYFILES);
  new FXMenuCommand(filemenu,tr("&Quit\tCtl-Q"),getApp()->quiticon,getApp(),Adie::ID_CLOSEALL);

  // Edit Menu entries
  new FXMenuCommand(editmenu,tr("&Undo\tCtl-Z\tUndo last change."),getApp()->undoicon,&undolist,FXUndoList::ID_UNDO);
  new FXMenuCommand(editmenu,tr("&Redo\tCtl-Y\tRedo last undo."),getApp()->redoicon,&undolist,FXUndoList::ID_REDO);
  new FXMenuCommand(editmenu,tr("&Undo all\t\tUndo all."),NULL,&undolist,FXUndoList::ID_UNDO_ALL);
  new FXMenuCommand(editmenu,tr("&Redo all\t\tRedo all."),NULL,&undolist,FXUndoList::ID_REDO_ALL);
  new FXMenuCommand(editmenu,tr("&Revert to saved\t\tRevert to saved."),NULL,&undolist,FXUndoList::ID_REVERT);
  new FXMenuSeparator(editmenu);
  new FXMenuCommand(editmenu,tr("&Copy\tCtl-C\tCopy selection to clipboard."),getApp()->copyicon,editor,FXText::ID_COPY_SEL);
  new FXMenuCommand(editmenu,tr("Cu&t\tCtl-X\tCut selection to clipboard."),getApp()->cuticon,editor,FXText::ID_CUT_SEL);
  new FXMenuCommand(editmenu,tr("&Paste\tCtl-V\tPaste from clipboard."),getApp()->pasteicon,editor,FXText::ID_PASTE_SEL);
  new FXMenuCommand(editmenu,tr("&Delete\t\tDelete selection."),getApp()->deleteicon,editor,FXText::ID_DELETE_SEL);
  new FXMenuSeparator(editmenu);
  new FXMenuCommand(editmenu,tr("Lo&wer-case\tCtl-U\tChange to lower case."),NULL,editor,FXText::ID_LOWER_CASE);
  new FXMenuCommand(editmenu,tr("Upp&er-case\tCtl-Shift-U\tChange to upper case."),NULL,editor,FXText::ID_UPPER_CASE);
  new FXMenuCommand(editmenu,tr("Clean indent\t\tClean indentation to either all tabs or all spaces."),NULL,editor,FXText::ID_CLEAN_INDENT);
  new FXMenuCommand(editmenu,tr("Shift left\tCtl-[\tShift text left."),getApp()->shiftlefticon,editor,FXText::ID_SHIFT_LEFT);
  new FXMenuCommand(editmenu,tr("Shift right\tCtl-]\tShift text right."),getApp()->shiftrighticon,editor,FXText::ID_SHIFT_RIGHT);
  new FXMenuCommand(editmenu,tr("Shift tab left\tAlt-[\tShift text left one tab position."),getApp()->shiftlefticon,editor,FXText::ID_SHIFT_TABLEFT);
  new FXMenuCommand(editmenu,tr("Shift tab right\tAlt-]\tShift text right one tab position."),getApp()->shiftrighticon,editor,FXText::ID_SHIFT_TABRIGHT);

  // Right mouse popup
  popupmenu=new FXMenuPane(this);
  new FXMenuCommand(popupmenu,tr("Undo"),getApp()->undoicon,&undolist,FXUndoList::ID_UNDO);
  new FXMenuCommand(popupmenu,tr("Redo"),getApp()->redoicon,&undolist,FXUndoList::ID_REDO);
  new FXMenuSeparator(popupmenu);
  new FXMenuCommand(popupmenu,tr("Cut"),getApp()->cuticon,editor,FXText::ID_CUT_SEL);
  new FXMenuCommand(popupmenu,tr("Copy"),getApp()->copyicon,editor,FXText::ID_COPY_SEL);
  new FXMenuCommand(popupmenu,tr("Paste"),getApp()->pasteicon,editor,FXText::ID_PASTE_SEL);
  new FXMenuCommand(popupmenu,tr("Select All"),NULL,editor,FXText::ID_SELECT_ALL);
  new FXMenuSeparator(popupmenu);
  new FXMenuCommand(popupmenu,tr("Set bookmark"),getApp()->bookseticon,this,ID_SET_MARK);
  new FXMenuCommand(popupmenu,tr("Next bookmark"),getApp()->booknexticon,this,ID_NEXT_MARK);
  new FXMenuCommand(popupmenu,tr("Previous bookmark"),getApp()->bookprevicon,this,ID_PREV_MARK);
  new FXMenuCommand(popupmenu,tr("Clear bookmarks"),getApp()->bookdelicon,this,ID_CLEAR_MARKS);

  // Goto Menu entries
  new FXMenuCommand(gotomenu,tr("&Goto...\tCtl-G\tGoto line number."),NULL,editor,FXText::ID_GOTO_LINE);
  new FXMenuCommand(gotomenu,tr("Goto selected...\tCtl-L\tGoto selected line number."),NULL,editor,FXText::ID_GOTO_SELECTED);
  new FXMenuSeparator(gotomenu);
  new FXMenuCommand(gotomenu,tr("Goto {..\tShift-Ctl-{\tGoto start of enclosing block."),NULL,editor,FXText::ID_LEFT_BRACE);
  new FXMenuCommand(gotomenu,tr("Goto ..}\tShift-Ctl-}\tGoto end of enclosing block."),NULL,editor,FXText::ID_RIGHT_BRACE);
  new FXMenuCommand(gotomenu,tr("Goto (..\tShift-Ctl-(\tGoto start of enclosing expression."),NULL,editor,FXText::ID_LEFT_PAREN);
  new FXMenuCommand(gotomenu,tr("Goto ..)\tShift-Ctl-)\tGoto end of enclosing expression."),NULL,editor,FXText::ID_RIGHT_PAREN);
  new FXMenuSeparator(gotomenu);
  new FXMenuCommand(gotomenu,tr("Goto matching      (..)\tCtl-M\tGoto matching brace or parenthesis."),NULL,editor,FXText::ID_GOTO_MATCHING);
  new FXMenuSeparator(gotomenu);
  new FXMenuCommand(gotomenu,tr("&Set bookmark\tAlt-B"),getApp()->bookseticon,this,ID_SET_MARK);
  new FXMenuCommand(gotomenu,tr("&Next bookmark\tAlt-N"),getApp()->booknexticon,this,ID_NEXT_MARK);
  new FXMenuCommand(gotomenu,tr("&Previous bookmark\tAlt-P"),getApp()->bookprevicon,this,ID_PREV_MARK);
  new FXMenuCommand(gotomenu,tr("&Clear bookmarks\tAlt-C"),getApp()->bookdelicon,this,ID_CLEAR_MARKS);

  // Search Menu entries
  new FXMenuCommand(searchmenu,tr("Select matching (..)\tShift-Ctl-M\tSelect matching brace or parenthesis."),NULL,editor,FXText::ID_SELECT_MATCHING);
  new FXMenuCommand(searchmenu,tr("Select block {..}\tShift-Alt-{\tSelect enclosing block."),NULL,editor,FXText::ID_SELECT_BRACE);
  new FXMenuCommand(searchmenu,tr("Select block {..}\tShift-Alt-}\tSelect enclosing block."),NULL,editor,FXText::ID_SELECT_BRACE);
  new FXMenuCommand(searchmenu,tr("Select expression (..)\tShift-Alt-(\tSelect enclosing parentheses."),NULL,editor,FXText::ID_SELECT_PAREN);
  new FXMenuCommand(searchmenu,tr("Select expression (..)\tShift-Alt-)\tSelect enclosing parentheses."),NULL,editor,FXText::ID_SELECT_PAREN);
  new FXMenuSeparator(searchmenu);
  new FXMenuCommand(searchmenu,tr("&Search sel. fwd\tCtl-H\tSearch for selection."),getApp()->searchnexticon,editor,FXText::ID_SEARCH_FORW_SEL);
  new FXMenuCommand(searchmenu,tr("&Search sel. bck\tShift-Ctl-H\tSearch for selection."),getApp()->searchprevicon,editor,FXText::ID_SEARCH_BACK_SEL);
  //new FXMenuCommand(searchmenu,tr("&Search next fwd\tCtl-G\tSearch forward for next occurrence."),getApp()->searchnexticon,editor,FXText::ID_SEARCH_FORW);
  //new FXMenuCommand(searchmenu,tr("&Search next bck\tShift-Ctl-G\tSearch backward for next occurrence."),getApp()->searchprevicon,editor,FXText::ID_SEARCH_BACK);
  new FXMenuCommand(searchmenu,tr("&Search...\tCtl-F\tSearch for a string."),getApp()->searchicon,editor,FXText::ID_SEARCH);
  new FXMenuCommand(searchmenu,tr("R&eplace...\tCtl-R\tSearch for a string."),NULL,editor,FXText::ID_REPLACE);

  // Syntax menu
  syntaxmenu=new FXMenuPane(this);
  new FXMenuRadio(syntaxmenu,tr("Plain"),this,ID_SYNTAX_FIRST);
  for(int syn=0; syn<getApp()->syntaxes.no(); syn++){
    new FXMenuRadio(syntaxmenu,getApp()->syntaxes[syn]->getName(),this,ID_SYNTAX_FIRST+1+syn);
    }

  // Options menu
  new FXMenuCommand(optionmenu,tr("Preferences...\t\tChange preferences."),NULL,this,ID_PREFERENCES);
  new FXMenuCommand(optionmenu,tr("Font...\t\tChange text font."),getApp()->fontsicon,this,ID_FONT);
  new FXMenuCheck(optionmenu,tr("Word wrap\tCtl-K\tToggle word wrap mode."),this,ID_TOGGLE_WRAP);
  new FXMenuCheck(optionmenu,tr("Overstrike\t\tToggle overstrike mode."),editor,FXText::ID_TOGGLE_OVERSTRIKE);
  new FXMenuCheck(optionmenu,tr("Syntax coloring\t\tToggle syntax coloring."),this,ID_SYNTAX);
  new FXMenuCheck(optionmenu,tr("&Jump Scrolling\t\tToggle jump scrolling mode."),this,ID_JUMPSCROLL);
  new FXMenuCommand(optionmenu,tr("Res&tyle\tCtl-T\tToggle syntax coloring."),NULL,this,ID_RESTYLE);
  new FXMenuCommand(optionmenu,tr("Include path...\t\tDirectories to search for include files."),NULL,this,ID_INCLUDE_PATH);
  new FXMenuCascade(optionmenu,tr("&Syntax"),NULL,syntaxmenu);
  new FXMenuSeparator(optionmenu);
  new FXMenuCommand(optionmenu,tr("Save Settings\t\tSave settings now."),NULL,this,ID_SAVE_SETTINGS);

  // View Menu entries
  new FXMenuCheck(viewmenu,tr("Hidden Files\t\tShow hidden files and directories."),dirlist,FXDirList::ID_TOGGLE_HIDDEN);
  new FXMenuCheck(viewmenu,tr("File Browser\t\tDisplay file list."),treebox,FXWindow::ID_TOGGLESHOWN);
  new FXMenuCheck(viewmenu,tr("Toolbar\t\tDisplay toolbar."),toolbar,FXWindow::ID_TOGGLESHOWN);
  new FXMenuCheck(viewmenu,tr("Status line\t\tDisplay status line."),statusbar,FXWindow::ID_TOGGLESHOWN);
  new FXMenuCheck(viewmenu,tr("Undo Counters\t\tShow undo/redo counters on status line."),undoredoblock,FXWindow::ID_TOGGLESHOWN);
  new FXMenuCheck(viewmenu,tr("Clock\t\tShow clock on status line."),clock,FXWindow::ID_TOGGLESHOWN);

  // Window menu
  new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_1);
  new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_2);
  new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_3);
  new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_4);
  new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_5);
  new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_6);
  new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_7);
  new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_8);
  new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_9);
  new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_10);

  // Help Menu entries
  new FXMenuCommand(helpmenu,tr("&Help...\t\tDisplay help information."),getApp()->helpicon,this,ID_HELP,0);
  new FXMenuSeparator(helpmenu);
  new FXMenuCommand(helpmenu,tr("&About Adie...\t\tDisplay about panel."),getApp()->smallicon,this,ID_ABOUT,0);

  // Add some alternative accelerators
  if(getAccelTable()){
    getAccelTable()->addAccel(MKUINT(KEY_Z,CONTROLMASK|SHIFTMASK),&undolist,FXSEL(SEL_COMMAND,FXUndoList::ID_REDO));

    // These were the old bindings; keeping them for now...
    getAccelTable()->addAccel(MKUINT(KEY_9,CONTROLMASK),editor,FXSEL(SEL_COMMAND,FXText::ID_SHIFT_LEFT));
    getAccelTable()->addAccel(MKUINT(KEY_0,CONTROLMASK),editor,FXSEL(SEL_COMMAND,FXText::ID_SHIFT_RIGHT));
    }

  // Initialize bookmarks
  clearBookmarks();

  // Initial setting
  syntax=NULL;

  // Recent files
  mrufiles.setTarget(this);
  mrufiles.setSelector(ID_OPEN_RECENT);

  // Initialize file name
  filename=file;
  filenameset=FALSE;
  filetime=0;

  // Initialize other stuff
  searchpath="/usr/include";
  setPatterns(tr("All Files (*)"));
  setCurrentPattern(0);
  currentstyle=-1;
  colorize=FALSE;
  stripcr=TRUE;
  stripsp=FALSE;
  appendnl=TRUE;
  saveviews=FALSE;
  savemarks=FALSE;
  warnchanged=FALSE;
  undolist.mark();
  }


// Create and show window
void TextWindow::create(){
  readRegistry();
  FXMainWindow::create();
  dragshell1->create();
  dragshell2->create();
  filemenu->create();
  editmenu->create();
  gotomenu->create();
  searchmenu->create();
  optionmenu->create();
  viewmenu->create();
  windowmenu->create();
  helpmenu->create();
  popupmenu->create();
  if(!urilistType){urilistType=getApp()->registerDragType(urilistTypeName);}
  getApp()->addTimeout(this,ID_CLOCKTIME,1);
  show(PLACEMENT_DEFAULT);
  editor->setFocus();

  // Jump to current directory
  dirlist->setCurrentFile(FXSystem::getCurrentDirectory());
  }


// Detach window
void TextWindow::detach(){
  FXMainWindow::detach();
  dragshell1->detach();
  dragshell2->detach();
  urilistType=0;
  }


// Redraw when style changed
void TextWindow::redraw(){
  editor->update();
  }


// Clean up the mess
TextWindow::~TextWindow(){
  getApp()->windowlist.remove(this);
  getApp()->removeTimeout(this,ID_CLOCKTIME);
  delete font;
  delete dragshell1;
  delete dragshell2;
  delete filemenu;
  delete editmenu;
  delete gotomenu;
  delete searchmenu;
  delete optionmenu;
  delete viewmenu;
  delete windowmenu;
  delete helpmenu;
  delete popupmenu;
  delete syntaxmenu;
  }


/*******************************************************************************/

// Is it modified
FXbool TextWindow::isModified() const {
  return !undolist.marked();
  }

// Set editable flag
void TextWindow::setEditable(FXbool edit){
  editor->setEditable(edit);
  }

// Is it editable
FXbool TextWindow::isEditable() const {
  return editor->isEditable();
  }


// Load file
FXbool TextWindow::loadFile(const FXString& file){
  FXFile textfile(file,FXFile::Reading);
  FXint size,n,i,j,k,c;
  FXchar *text;

  FXTRACE((100,"loadFile(%s)\n",file.text()));

  // Opened file?
  if(!textfile.isOpen()){
    FXMessageBox::error(this,MBOX_OK,tr("Error Loading File"),tr("Unable to open file: %s"),file.text());
    return FALSE;
    }

  // Get file size
  size=textfile.size();

  // Make buffer to load file
  if(!FXMALLOC(&text,FXchar,size)){
    FXMessageBox::error(this,MBOX_OK,tr("Error Loading File"),tr("File is too big: %s (%d bytes)"),file.text(),size);
    return FALSE;
    }

  // Set wait cursor
  getApp()->beginWaitCursor();

  // Read the file
  n=textfile.readBlock(text,size);
  if(n<0){
    FXFREE(&text);
    FXMessageBox::error(this,MBOX_OK,tr("Error Loading File"),tr("Unable to read file: %s"),file.text());
    return FALSE;
    }

  // Strip carriage returns
  if(stripcr){
    for(i=j=0; j<n; j++){
      c=text[j];
      if(c!='\r'){
        text[i++]=c;
        }
      }
    n=i;
    }

  // Strip trailing spaces
  if(stripsp){
    for(i=j=k=0; j<n; i++,j++){
      c=text[j];
      if(c=='\n'){
        i=k;
        k++;
        }
      else if(!Ascii::isSpace(c)){
        k=i+1;
        }
      text[i]=c;
      }
    n=i;
    }

  // Set text
  editor->setText(text,n);
  FXFREE(&text);

  // Kill wait cursor
  getApp()->endWaitCursor();

  // Set stuff
  setEditable(FXStat::isWritable(file));
  dirlist->setCurrentFile(file);
  mrufiles.appendFile(file);
  filetime=FXStat::modified(file);
  filenameset=TRUE;
  filename=file;

  // Determine language
  determineSyntax();

  // Clear undo records
  undolist.clear();

  // Mark undo state as clean (saved)
  undolist.mark();

  return TRUE;
  }


// Insert file
FXbool TextWindow::insertFile(const FXString& file){
  FXint size,n,i,j,k,c;
  FXchar *text;
  FILE *fp;

  FXTRACE((100,"insertFile(%s)\n",file.text()));

  // Open file
  fp=fopen(file.text(),"r");
  if(!fp){
    FXMessageBox::error(this,MBOX_OK,tr("Error Inserting File"),tr("Unable to open file: %s"),file.text());
    return FALSE;
    }

  // Get file size
  size=FXStat::size(file);

  // Make buffer to load file
  if(!FXMALLOC(&text,FXchar,size)){
    FXMessageBox::error(this,MBOX_OK,tr("Error Inserting File"),tr("File is too big: %s (%d bytes)"),file.text(),size);
    fclose(fp);
    return FALSE;
    }

  // Set wait cursor
  getApp()->beginWaitCursor();

  // Read the file
  n=fread(text,1,size,fp);

  // Strip carriage returns
  if(stripcr){
    for(i=j=0; j<n; j++){
      c=text[j];
      if(c!='\r'){
        text[i++]=c;
        }
      }
    n=i;
    }

  // Strip trailing spaces
  if(stripsp){
    for(i=j=k=0; j<n; i++,j++){
      c=text[j];
      if(c=='\n'){
        i=k;
        k++;
        }
      else if(!isspace(c)){
        k=i+1;
        }
      text[i]=c;
      }
    n=i;
    }

  // Set text
  editor->insertText(editor->getCursorPos(),text,n,TRUE);
  FXFREE(&text);

  // Kill wait cursor
  getApp()->endWaitCursor();

  // Close file
  fclose(fp);

  // Set stuff
  return TRUE;
  }


// Save file
FXbool TextWindow::saveFile(const FXString& file){
  FXFile textfile(file,FXFile::Writing);
  FXint size,n;
  FXchar *text;

  FXTRACE((100,"saveFile(%s)\n",file.text()));

  // Opened file?
  if(!textfile.isOpen()){
    FXMessageBox::error(this,MBOX_OK,tr("Error Saving File"),tr("Unable to open file: %s"),file.text());
    return FALSE;
    }

  // Get size
  size=editor->getLength();

  // Alloc buffer
  if(!FXMALLOC(&text,FXchar,size+1)){
    FXMessageBox::error(this,MBOX_OK,tr("Error Saving File"),tr("File is too big: %s"),file.text());
    return FALSE;
    }

  // Set wait cursor
  getApp()->beginWaitCursor();

  // Get text from editor
  editor->getText(text,size);

  // Append newline?
  if(appendnl && (0<size) && (text[size-1]!='\n')){
    text[size++]='\n';
    }

  // Translate newlines
#ifdef WIN32
  fxtoDOS(text,size);
#endif

  // Write the file
  n=textfile.writeBlock(text,size);

  // Ditch buffer
  FXFREE(&text);

  // Kill wait cursor
  getApp()->endWaitCursor();

  // Were we able to write it all?
  if(n!=size){
    FXMessageBox::error(this,MBOX_OK,tr("Error Saving File"),tr("File: %s truncated."),file.text());
    return FALSE;
    }

  // Set stuff
  setEditable(TRUE);
  dirlist->setCurrentFile(file);
  mrufiles.appendFile(file);
  filetime=FXStat::modified(file);
  filenameset=TRUE;
  filename=file;
  undolist.mark();
  return TRUE;
  }


// Extract file
FXbool TextWindow::extractFile(const FXString& file){
  FXFile textfile(file,FXFile::Writing);
  FXint size,n;
  FXchar *text;

  FXTRACE((100,"extractFile(%s)\n",file.text()));

  // Opened file?
  if(!textfile.isOpen()){
    FXMessageBox::error(this,MBOX_OK,tr("Error Extracting File"),tr("Unable to open file: %s"),file.text());
    return FALSE;
    }

  // Get size
  size=editor->getSelEndPos()-editor->getSelStartPos();

  // Alloc buffer
  if(!FXMALLOC(&text,FXchar,size+1)){
    FXMessageBox::error(this,MBOX_OK,tr("Error Extracting File"),tr("File is too big: %s"),file.text());
    return FALSE;
    }

  // Set wait cursor
  getApp()->beginWaitCursor();

  // Get text from editor
  editor->extractText(text,editor->getSelStartPos(),size);

  // Translate newlines
#ifdef WIN32
  fxtoDOS(text,size);
#endif

  // Write the file
  n=textfile.writeBlock(text,size);

  // Ditch buffer
  FXFREE(&text);

  // Kill wait cursor
  getApp()->endWaitCursor();

  // Were we able to write it all?
  if(n!=size){
    FXMessageBox::error(this,MBOX_OK,tr("Error Extracting File"),tr("File: %s truncated."),file.text());
    return FALSE;
    }

  return TRUE;
  }


// Generate unique name for a new window
FXString TextWindow::unique() const {
  FXString name="untitled";
  for(FXint i=1; i<2147483647; i++){
    if(!findWindow(name)) break;
    name.format("untitled%d",i);
    }
  return name;
  }


// Find an as yet untitled, unedited window
TextWindow *TextWindow::findUnused() const {
  for(FXint w=0; w<getApp()->windowlist.no(); w++){
    if(!getApp()->windowlist[w]->isFilenameSet() && !getApp()->windowlist[w]->isModified()){
      return getApp()->windowlist[w];
      }
    }
  return NULL;
  }


// Find window, if any, currently editing the given file
TextWindow *TextWindow::findWindow(const FXString& file) const {
  for(FXint w=0; w<getApp()->windowlist.no(); w++){
    if(getApp()->windowlist[w]->getFilename()==file) return getApp()->windowlist[w];
    }
  return NULL;
  }


// Visit given line
void TextWindow::visitLine(FXint line){
  FXint pos=editor->nextLine(0,line-1);
  editor->setCursorPos(pos);
  editor->setCenterLine(pos);
  }


// Change patterns, each pattern separated by newline
void TextWindow::setPatterns(const FXString& patterns){
  FXString pat; FXint i;
  filter->clearItems();
  for(i=0; !(pat=patterns.section('\n',i)).empty(); i++){
    filter->appendItem(pat);
    }
  if(!filter->getNumItems()) filter->appendItem(tr("All Files (*)"));
  setCurrentPattern(0);
  }


// Return list of patterns
FXString TextWindow::getPatterns() const {
  FXString pat; FXint i;
  for(i=0; i<filter->getNumItems(); i++){
    if(!pat.empty()) pat+='\n';
    pat+=filter->getItemText(i);
    }
  return pat;
  }


// Set current pattern
void TextWindow::setCurrentPattern(FXint n){
  n=FXCLAMP(0,n,filter->getNumItems()-1);
  filter->setCurrentItem(n);
  dirlist->setPattern(FXFileSelector::patternFromText(filter->getItemText(n)));
  }


// Return current pattern
FXint TextWindow::getCurrentPattern() const {
  return filter->getCurrentItem();
  }


/*******************************************************************************/

// Read settings from registry
void TextWindow::readRegistry(){
  FXColor textback,textfore,textselback,textselfore,textcursor,texthilitefore,texthiliteback;
  FXColor dirback,dirfore,dirselback,dirselfore,dirlines,textactiveback,textbar,textnumber;
  FXint ww,hh,xx,yy,treewidth,hidetree,hiddenfiles,wrapping,wrapcols,fixedwrap,tabcols,showactive,hideundo;
  FXint autoindent,hardtabs,hideclock,hidestatus,hidetoolbar,hilitematchtime,barcols,jumpscroll;
  FXString fontspec;

  // Text colors
  textback=getApp()->reg().readColorEntry("SETTINGS","textbackground",editor->getBackColor());
  textfore=getApp()->reg().readColorEntry("SETTINGS","textforeground",editor->getTextColor());
  textselback=getApp()->reg().readColorEntry("SETTINGS","textselbackground",editor->getSelBackColor());
  textselfore=getApp()->reg().readColorEntry("SETTINGS","textselforeground",editor->getSelTextColor());
  textcursor=getApp()->reg().readColorEntry("SETTINGS","textcursor",editor->getCursorColor());
  texthiliteback=getApp()->reg().readColorEntry("SETTINGS","texthilitebackground",editor->getHiliteBackColor());
  texthilitefore=getApp()->reg().readColorEntry("SETTINGS","texthiliteforeground",editor->getHiliteTextColor());
  textactiveback=getApp()->reg().readColorEntry("SETTINGS","textactivebackground",editor->getActiveBackColor());
  textbar=getApp()->reg().readColorEntry("SETTINGS","textnumberbackground",editor->getBarColor());
  textnumber=getApp()->reg().readColorEntry("SETTINGS","textnumberforeground",editor->getNumberColor());

  // Directory colors
  dirback=getApp()->reg().readColorEntry("SETTINGS","browserbackground",dirlist->getBackColor());
  dirfore=getApp()->reg().readColorEntry("SETTINGS","browserforeground",dirlist->getTextColor());
  dirselback=getApp()->reg().readColorEntry("SETTINGS","browserselbackground",dirlist->getSelBackColor());
  dirselfore=getApp()->reg().readColorEntry("SETTINGS","browserselforeground",dirlist->getSelTextColor());
  dirlines=getApp()->reg().readColorEntry("SETTINGS","browserlines",dirlist->getLineColor());

  // Delimiters
  delimiters=getApp()->reg().readStringEntry("SETTINGS","delimiters","~.,/\\`'!@#$%^&*()-=+{}|[]\":;<>?");

  // Font
  fontspec=getApp()->reg().readStringEntry("SETTINGS","textfont","");
  if(!fontspec.empty()){
    font=new FXFont(getApp(),fontspec);
    editor->setFont(font);
    }

  // Get size
  xx=getApp()->reg().readIntEntry("SETTINGS","x",5);
  yy=getApp()->reg().readIntEntry("SETTINGS","y",5);
  ww=getApp()->reg().readIntEntry("SETTINGS","width",600);
  hh=getApp()->reg().readIntEntry("SETTINGS","height",400);


  // Hidden files shown
  hiddenfiles=getApp()->reg().readIntEntry("SETTINGS","showhiddenfiles",FALSE);
  dirlist->showHiddenFiles(hiddenfiles);

  // Showing undo counters?
  hideundo=getApp()->reg().readIntEntry("SETTINGS","hideundo",TRUE);

  // Showing the tree?
  hidetree=getApp()->reg().readIntEntry("SETTINGS","hidetree",TRUE);

  // Showing the clock?
  hideclock=getApp()->reg().readIntEntry("SETTINGS","hideclock",FALSE);

  // Showing the status line?
  hidestatus=getApp()->reg().readIntEntry("SETTINGS","hidestatus",FALSE);

  // Showing the tool bar?
  hidetoolbar=getApp()->reg().readIntEntry("SETTINGS","hidetoolbar",FALSE);

  // Highlight match time
  hilitematchtime=getApp()->reg().readIntEntry("SETTINGS","hilitematchtime",3000);

  // Width of tree
  treewidth=getApp()->reg().readIntEntry("SETTINGS","treewidth",100);

  // Active Background
  showactive=getApp()->reg().readIntEntry("SETTINGS","showactive",FALSE);

  // Word wrapping
  wrapping=getApp()->reg().readIntEntry("SETTINGS","wordwrap",0);
  wrapcols=getApp()->reg().readIntEntry("SETTINGS","wrapcols",80);
  fixedwrap=getApp()->reg().readIntEntry("SETTINGS","fixedwrap",1);

  // Tab settings, autoindent
  autoindent=getApp()->reg().readIntEntry("SETTINGS","autoindent",0);
  hardtabs=getApp()->reg().readIntEntry("SETTINGS","hardtabs",1);
  tabcols=getApp()->reg().readIntEntry("SETTINGS","tabcols",8);

  // Space for line numbers
  barcols=getApp()->reg().readIntEntry("SETTINGS","barcols",0);

  // Various flags
  stripcr=getApp()->reg().readIntEntry("SETTINGS","stripreturn",FALSE);
  stripsp=getApp()->reg().readIntEntry("SETTINGS","stripspaces",FALSE);
  appendnl=getApp()->reg().readIntEntry("SETTINGS","appendnewline",TRUE);
  saveviews=getApp()->reg().readIntEntry("SETTINGS","saveviews",FALSE);
  savemarks=getApp()->reg().readIntEntry("SETTINGS","savebookmarks",FALSE);
  warnchanged=getApp()->reg().readIntEntry("SETTINGS","warnchanged",TRUE);
  colorize=getApp()->reg().readIntEntry("SETTINGS","colorize",FALSE);
  jumpscroll=getApp()->reg().readIntEntry("SETTINGS","jumpscroll",FALSE);

  // File patterns
  setPatterns(getApp()->reg().readStringEntry("SETTINGS","filepatterns","All Files (*)"));
  setCurrentPattern(getApp()->reg().readIntEntry("SETTINGS","filepatternno",0));

  // Search path
  searchpath=getApp()->reg().readStringEntry("SETTINGS","searchpath","/usr/include");

  // Change the colors
  editor->setTextColor(textfore);
  editor->setBackColor(textback);
  editor->setSelBackColor(textselback);
  editor->setSelTextColor(textselfore);
  editor->setCursorColor(textcursor);
  editor->setHiliteBackColor(texthiliteback);
  editor->setHiliteTextColor(texthilitefore);
  editor->setActiveBackColor(textactiveback);
  editor->setBarColor(textbar);
  editor->setNumberColor(textnumber);

  dirlist->setTextColor(dirfore);
  dirlist->setBackColor(dirback);
  dirlist->setSelBackColor(dirselback);
  dirlist->setSelTextColor(dirselfore);
  dirlist->setLineColor(dirlines);

  // Change delimiters
  editor->setDelimiters(delimiters.text());

  // Hide tree if asked for
  if(hidetree) treebox->hide();

  // Hide clock
  if(hideclock) clock->hide();

  // Hide statusline
  if(hidestatus) statusbar->hide();

  // Hide toolbar
  if(hidetoolbar) toolbar->hide();

  // Hide undo counters
  if(hideundo) undoredoblock->hide();

  // Set tree width
  treebox->setWidth(treewidth);

  // Open toward file
//  dirlist->setCurrentFile(filename);

  // Wrap mode
  if(wrapping)
    editor->setTextStyle(editor->getTextStyle()|TEXT_WORDWRAP);
  else
    editor->setTextStyle(editor->getTextStyle()&~TEXT_WORDWRAP);

  // Active line color being used
  if(showactive)
    editor->setTextStyle(editor->getTextStyle()|TEXT_SHOWACTIVE);
  else
    editor->setTextStyle(editor->getTextStyle()&~TEXT_SHOWACTIVE);

  // Wrap fixed mode
  if(fixedwrap)
    editor->setTextStyle(editor->getTextStyle()|TEXT_FIXEDWRAP);
  else
    editor->setTextStyle(editor->getTextStyle()&~TEXT_FIXEDWRAP);

  // Autoindent
  if(autoindent)
    editor->setTextStyle(editor->getTextStyle()|TEXT_AUTOINDENT);
  else
    editor->setTextStyle(editor->getTextStyle()&~TEXT_AUTOINDENT);

  // Hard tabs
  if(hardtabs)
    editor->setTextStyle(editor->getTextStyle()&~TEXT_NO_TABS);
  else
    editor->setTextStyle(editor->getTextStyle()|TEXT_NO_TABS);

  // Jump Scroll
  if(jumpscroll)
    editor->setScrollStyle(editor->getScrollStyle()|SCROLLERS_DONT_TRACK);
  else
    editor->setScrollStyle(editor->getScrollStyle()&~SCROLLERS_DONT_TRACK);

  // Wrap and tab columns
  editor->setWrapColumns(wrapcols);
  editor->setTabColumns(tabcols);
  editor->setBarColumns(barcols);

  // Highlight match time
  editor->setHiliteMatchTime(hilitematchtime);

  // Reposition window
  position(xx,yy,ww,hh);
  }


/*******************************************************************************/


// Save settings to registry
void TextWindow::writeRegistry(){
  FXString fontspec;

  // Colors of text
  getApp()->reg().writeColorEntry("SETTINGS","textbackground",editor->getBackColor());
  getApp()->reg().writeColorEntry("SETTINGS","textforeground",editor->getTextColor());
  getApp()->reg().writeColorEntry("SETTINGS","textselbackground",editor->getSelBackColor());
  getApp()->reg().writeColorEntry("SETTINGS","textselforeground",editor->getSelTextColor());
  getApp()->reg().writeColorEntry("SETTINGS","textcursor",editor->getCursorColor());
  getApp()->reg().writeColorEntry("SETTINGS","texthilitebackground",editor->getHiliteBackColor());
  getApp()->reg().writeColorEntry("SETTINGS","texthiliteforeground",editor->getHiliteTextColor());
  getApp()->reg().writeColorEntry("SETTINGS","textactivebackground",editor->getActiveBackColor());
  getApp()->reg().writeColorEntry("SETTINGS","textnumberbackground",editor->getBarColor());
  getApp()->reg().writeColorEntry("SETTINGS","textnumberforeground",editor->getNumberColor());

  // Colors of directory
  getApp()->reg().writeColorEntry("SETTINGS","browserbackground",dirlist->getBackColor());
  getApp()->reg().writeColorEntry("SETTINGS","browserforeground",dirlist->getTextColor());
  getApp()->reg().writeColorEntry("SETTINGS","browserselbackground",dirlist->getSelBackColor());
  getApp()->reg().writeColorEntry("SETTINGS","browserselforeground",dirlist->getSelTextColor());
  getApp()->reg().writeColorEntry("SETTINGS","browserlines",dirlist->getLineColor());

  // Delimiters
  getApp()->reg().writeStringEntry("SETTINGS","delimiters",delimiters.text());

  // Write new window size back to registry
  getApp()->reg().writeIntEntry("SETTINGS","x",getX());
  getApp()->reg().writeIntEntry("SETTINGS","y",getY());
  getApp()->reg().writeIntEntry("SETTINGS","width",getWidth());
  getApp()->reg().writeIntEntry("SETTINGS","height",getHeight());

  // Were showing hidden files
  getApp()->reg().writeIntEntry("SETTINGS","showhiddenfiles",dirlist->showHiddenFiles());

  // Was tree shown
  getApp()->reg().writeIntEntry("SETTINGS","hidetree",!treebox->shown());

  // Was status line shown
  getApp()->reg().writeIntEntry("SETTINGS","hidestatus",!statusbar->shown());

  // Was clock shown
  getApp()->reg().writeIntEntry("SETTINGS","hideclock",!clock->shown());

  // Was toolbar shown
  getApp()->reg().writeIntEntry("SETTINGS","hidetoolbar",!toolbar->shown());

  // Were undo counters shown
  getApp()->reg().writeIntEntry("SETTINGS","hideundo",!undoredoblock->shown());

  // Highlight match time
  getApp()->reg().writeIntEntry("SETTINGS","hilitematchtime",editor->getHiliteMatchTime());

  // Width of tree
  getApp()->reg().writeIntEntry("SETTINGS","treewidth",treebox->getWidth());

  // Wrap mode
  getApp()->reg().writeIntEntry("SETTINGS","wordwrap",(editor->getTextStyle()&TEXT_WORDWRAP)!=0);
  getApp()->reg().writeIntEntry("SETTINGS","fixedwrap",(editor->getTextStyle()&TEXT_FIXEDWRAP)!=0);
  getApp()->reg().writeIntEntry("SETTINGS","wrapcols",editor->getWrapColumns());

  // Active background
  getApp()->reg().writeIntEntry("SETTINGS","showactive",(editor->getTextStyle()&TEXT_SHOWACTIVE)!=0);


  // Bar columns
  getApp()->reg().writeIntEntry("SETTINGS","barcols",editor->getBarColumns());

  // Tab settings, autoindent
  getApp()->reg().writeIntEntry("SETTINGS","autoindent",(editor->getTextStyle()&TEXT_AUTOINDENT)!=0);
  getApp()->reg().writeIntEntry("SETTINGS","hardtabs",(editor->getTextStyle()&TEXT_NO_TABS)==0);
  getApp()->reg().writeIntEntry("SETTINGS","tabcols",editor->getTabColumns());

  // Strip returns
  getApp()->reg().writeIntEntry("SETTINGS","stripreturn",stripcr);
  getApp()->reg().writeIntEntry("SETTINGS","stripspaces",stripsp);
  getApp()->reg().writeIntEntry("SETTINGS","appendnewline",appendnl);
  getApp()->reg().writeIntEntry("SETTINGS","saveviews",saveviews);
  getApp()->reg().writeIntEntry("SETTINGS","savebookmarks",savemarks);
  getApp()->reg().writeIntEntry("SETTINGS","warnchanged",warnchanged);
  getApp()->reg().writeIntEntry("SETTINGS","colorize",colorize);
  getApp()->reg().writeIntEntry("SETTINGS","jumpscroll",(editor->getScrollStyle()&SCROLLERS_DONT_TRACK)!=0);

  // File patterns
  getApp()->reg().writeIntEntry("SETTINGS","filepatternno",getCurrentPattern());
  getApp()->reg().writeStringEntry("SETTINGS","filepatterns",getPatterns().text());

  // Search path
  getApp()->reg().writeStringEntry("SETTINGS","searchpath",searchpath.text());

  // Font
  fontspec=editor->getFont()->getFont();
  getApp()->reg().writeStringEntry("SETTINGS","textfont",fontspec.text());
  }


/*******************************************************************************/


// About box
long TextWindow::onCmdAbout(FXObject*,FXSelector,void*){
  FXDialogBox about(this,tr("About Adie"),DECOR_TITLE|DECOR_BORDER,0,0,0,0, 0,0,0,0, 0,0);
  FXGIFIcon picture(getApp(),adie_gif);
  new FXLabel(&about,FXString::null,&picture,FRAME_GROOVE|LAYOUT_SIDE_LEFT|LAYOUT_CENTER_Y|JUSTIFY_CENTER_X|JUSTIFY_CENTER_Y,0,0,0,0, 0,0,0,0);
  FXVerticalFrame* side=new FXVerticalFrame(&about,LAYOUT_SIDE_RIGHT|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 10,10,10,10, 0,0);
  new FXLabel(side,"A . d . i . e",NULL,JUSTIFY_LEFT|ICON_BEFORE_TEXT|LAYOUT_FILL_X);
  new FXHorizontalSeparator(side,SEPARATOR_LINE|LAYOUT_FILL_X);
  new FXLabel(side,FXStringFormat(tr("\nThe Adie ADvanced Interactive Editor, version %d.%d.%d.\n\nAdie is a fast and convenient programming text editor and text\nfile viewer with an integrated file browser.\nAdie uses the FOX Toolkit version %d.%d.%d.\nCopyright (C) 2000,2006 Jeroen van der Zijp (jeroen@fox-toolkit.org).\n "),VERSION_MAJOR,VERSION_MINOR,VERSION_PATCH,FOX_MAJOR,FOX_MINOR,FOX_LEVEL),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  FXButton *button=new FXButton(side,tr("&OK"),NULL,&about,FXDialogBox::ID_ACCEPT,BUTTON_INITIAL|BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT,0,0,0,0,32,32,2,2);
  button->setFocus();
  about.execute(PLACEMENT_OWNER);
  return 1;
  }


// Show help window, create it on-the-fly
long TextWindow::onCmdHelp(FXObject*,FXSelector,void*){
  HelpWindow *helpwindow=new HelpWindow(getApp());
  helpwindow->create();
  helpwindow->show(PLACEMENT_CURSOR);
  return 1;
  }



// Show preferences dialog
long TextWindow::onCmdPreferences(FXObject*,FXSelector,void*){
  Preferences preferences(this);
  preferences.setPatterns(getPatterns());
  preferences.setSyntax(getSyntax());
  if(preferences.execute()){
    setPatterns(preferences.getPatterns());
    }
  return 1;
  }


// Change font
long TextWindow::onCmdFont(FXObject*,FXSelector,void*){
  FXFontDialog fontdlg(this,tr("Change Font"),DECOR_BORDER|DECOR_TITLE);
  FXFontDesc fontdesc;
  editor->getFont()->getFontDesc(fontdesc);
  fontdlg.setFontSelection(fontdesc);
  if(fontdlg.execute()){
    FXFont *oldfont=font;
    fontdlg.getFontSelection(fontdesc);
    font=new FXFont(getApp(),fontdesc);
    font->create();
    editor->setFont(font);
    delete oldfont;
    }
  return 1;
  }


/*******************************************************************************/


// Reopen file
long TextWindow::onCmdReopen(FXObject*,FXSelector,void*){
  if(isModified()){
    if(FXMessageBox::question(this,MBOX_YES_NO,tr("Document was changed"),tr("Discard changes to this document?"))==MBOX_CLICKED_NO) return 1;
    }
  loadFile(filename);
  return 1;
  }


// Update reopen file
long TextWindow::onUpdReopen(FXObject* sender,FXSelector,void* ptr){
  sender->handle(this,isFilenameSet()?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),ptr);
  return 1;
  }


// New
long TextWindow::onCmdNew(FXObject*,FXSelector,void*){
  TextWindow *window=new TextWindow(getApp(),unique());
  window->create();
  window->raise();
  window->setFocus();
  return 1;
  }


// Open
long TextWindow::onCmdOpen(FXObject*,FXSelector,void*){
  FXFileDialog opendialog(this,tr("Open Document"));
  opendialog.setSelectMode(SELECTFILE_EXISTING);
  opendialog.setPatternList(getPatterns());
  opendialog.setCurrentPattern(getCurrentPattern());
  opendialog.setDirectory(FXPath::directory(filename));
  if(opendialog.execute()){
    setCurrentPattern(opendialog.getCurrentPattern());
    FXString file=opendialog.getFilename();
    TextWindow *window=findWindow(file);
    if(!window){
      window=findUnused();
      if(!window){
        window=new TextWindow(getApp(),unique());
        window->create();
        }
      window->loadFile(file);
      window->readBookmarks(file);
      window->readView(file);
      }
    window->raise();
    window->setFocus();
    }
  return 1;
  }


// Open Selected
long TextWindow::onCmdOpenSelected(FXObject*,FXSelector,void*){
  FXchar name[1024],*data; FXuint len; FXint lineno=0;

  // Get selection
  if(getDNDData(FROM_SELECTION,stringType,(FXuchar*&)data,len)){
    FXString string(data,len);
    FXFREE(&data);

    // Its too big, most likely not a file name
    if(len<1024){
      FXString file=FXString::null;
      FXString dir=FXSystem::getCurrentDirectory();

      // Base off currently loaded file
      if(!filename.empty()) dir=FXPath::directory(filename);

      // Strip leading/trailing space
      string.trim();

      // Extract name from #include syntax
      if(sscanf(string.text(),"#include \"%[^\"]\"",name)==1){
        file=FXPath::absolute(dir,name);
        if(!FXStat::exists(file)){
          file=FXPath::search(searchpath,name);
          }
        }
      else if(sscanf(string.text(),"#include <%[^>]>",name)==1){
        file=FXPath::absolute(dir,name);
        if(!FXStat::exists(file)){
          file=FXPath::search(searchpath,name);
          }
        }

      // Compiler output in the form <filename>:<number>: Error message
      else if(sscanf(string.text(),"%[^:]:%d:",name,&lineno)==2){
        file=FXPath::absolute(dir,name);
        if(!FXStat::exists(file)){
          file=FXPath::absolute(dir,string);
          }
        }

      // Compiler output in the form <filename>(<number>) : Error message
      else if(sscanf(string.text(),"%[^(](%d)",name,&lineno)==2){
        file=FXPath::absolute(dir,name);
        if(!FXStat::exists(file)){
          file=FXPath::absolute(dir,string);
          }
        }

      // Compiler output in the form "<filename>", line <number>: Error message
      else if(sscanf(string.text(),"\"%[^\"]\", line %d",name,&lineno)==2){
        file=FXPath::absolute(dir,name);
        if(!FXStat::exists(file)){
          file=FXPath::absolute(dir,string);
          }
        }

      // Compiler output in the form ... File = <filename>, Line = <number>
      else if(sscanf(string.text(),"%*[^:]: %*s File = %[^,], Line = %d",name,&lineno)==2){
        file=FXPath::absolute(dir,name);
        if(!FXStat::exists(file)){
          file=FXPath::absolute(dir,string);
          }
        }

      // Compiler output in the form filename: Other stuff
      else if(sscanf(string.text(),"%[^:]:",name)==1){
        file=FXPath::absolute(dir,name);
        if(!FXStat::exists(file)){
          file=FXPath::absolute(dir,string);
          }
        }

      // Try whole selection
      else{
        file=FXPath::absolute(dir,string);
        }

      // Not a file name
      if(FXStat::exists(file)){

        // File loaded already?
        TextWindow *window=findWindow(file);
        if(!window){
          window=findUnused();
          if(!window){
            window=new TextWindow(getApp(),unique());
            window->create();
            }
          window->loadFile(file);
          window->readBookmarks(file);
          }

        // Switch line number only
        if(lineno){
          window->visitLine(lineno);
          }

        // Bring up the window
        window->raise();
        window->setFocus();
        return 1;
        }
      }
    getApp()->beep();
    }
  return 1;
  }


// Open recent file
long TextWindow::onCmdOpenRecent(FXObject*,FXSelector,void* ptr){
  FXString file=(const char*)ptr;
  TextWindow *window=findWindow(file);
  if(!window){
    window=findUnused();
    if(!window){
      window=new TextWindow(getApp(),unique());
      window->create();
      }
    window->loadFile(file);
    window->readBookmarks(file);
    window->readView(file);
    }
  window->raise();
  window->setFocus();
  return 1;
  }


// Command from the tree list
long TextWindow::onCmdOpenTree(FXObject*,FXSelector,void* ptr){
  FXTreeItem *item=(FXTreeItem*)ptr;
  FXString file;
  if(!item || !dirlist->isItemFile(item)) return 1;
  if(!saveChanges()) return 1;
  file=dirlist->getItemPathname(item);
  loadFile(file);
  readBookmarks(file);
  readView(file);
  return 1;
  }


// See if we can get it as a filename
long TextWindow::onEditDNDDrop(FXObject*,FXSelector,void*){
  FXchar *data; FXuint len;
  if(getDNDData(FROM_DRAGNDROP,urilistType,(FXuchar*&)data,len)){
    FXString urilist(data,len);
    FXString file=FXURL::fileFromURL(urilist.before('\r'));
    FXFREE(&data);
    if(file.empty()) return 1;
    if(!saveChanges()) return 1;
    loadFile(file);
    readBookmarks(file);
    readView(file);
    return 1;
    }
  return 0;
  }


// See if a filename is being dragged over the window
long TextWindow::onEditDNDMotion(FXObject*,FXSelector,void*){
  if(offeredDNDType(FROM_DRAGNDROP,urilistType)){
    acceptDrop(DRAG_COPY);
    return 1;
    }
  return 0;
  }


// Insert file into buffer
long TextWindow::onCmdInsertFile(FXObject*,FXSelector,void*){
  FXString file;
  FXFileDialog opendialog(this,tr("Open Document"));
  opendialog.setSelectMode(SELECTFILE_EXISTING);
  opendialog.setPatternList(getPatterns());
  opendialog.setCurrentPattern(getCurrentPattern());
  opendialog.setDirectory(FXPath::directory(filename));
  if(opendialog.execute()){
    setCurrentPattern(opendialog.getCurrentPattern());
    file=opendialog.getFilename();
    insertFile(file);
    }
  return 1;
  }


// Update insert file
long TextWindow::onUpdInsertFile(FXObject* sender,FXSelector,void*){
  sender->handle(this,editor->isEditable()?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Extract selection to file
long TextWindow::onCmdExtractFile(FXObject*,FXSelector,void*){
  FXFileDialog savedialog(this,tr("Save Document"));
  FXString file="untitled";
  savedialog.setSelectMode(SELECTFILE_ANY);
  savedialog.setPatternList(getPatterns());
  savedialog.setCurrentPattern(getCurrentPattern());
  savedialog.setDirectory(FXPath::directory(filename));
  savedialog.setFilename(file);
  if(savedialog.execute()){
    setCurrentPattern(savedialog.getCurrentPattern());
    file=savedialog.getFilename();
    if(FXStat::exists(file)){
      if(MBOX_CLICKED_NO==FXMessageBox::question(this,MBOX_YES_NO,tr("Overwrite Document"),tr("Overwrite existing document: %s?"),file.text())) return 1;
      }
    extractFile(file);
    }
  return 1;
  }


// Update extract file
long TextWindow::onUpdExtractFile(FXObject* sender,FXSelector,void*){
  sender->handle(this,editor->hasSelection()?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Save changes, prompt for new filename
FXbool TextWindow::saveChanges(){
  FXuint answer;
  FXString file;
  writeBookmarks(filename);
  writeView(filename);
  if(isModified()){
    answer=FXMessageBox::question(this,MBOX_YES_NO_CANCEL,tr("Unsaved Document"),tr("Save %s to file?"),FXPath::name(filename).text());
    if(answer==MBOX_CLICKED_CANCEL) return FALSE;
    if(answer==MBOX_CLICKED_YES){
      file=filename;
      if(!filenameset){
        FXFileDialog savedialog(this,tr("Save Document"));
        savedialog.setSelectMode(SELECTFILE_ANY);
        savedialog.setPatternList(getPatterns());
        savedialog.setCurrentPattern(getCurrentPattern());
        savedialog.setFilename(file);
        if(!savedialog.execute()) return FALSE;
        setCurrentPattern(savedialog.getCurrentPattern());
        file=savedialog.getFilename();
        if(FXStat::exists(file)){
          if(MBOX_CLICKED_NO==FXMessageBox::question(this,MBOX_YES_NO,tr("Overwrite Document"),tr("Overwrite existing document: %s?"),file.text())) return FALSE;
          }
        }
      saveFile(file);
      }
    }
  return TRUE;
  }


// Save
long TextWindow::onCmdSave(FXObject* sender,FXSelector sel,void* ptr){
  if(!filenameset) return onCmdSaveAs(sender,sel,ptr);
  saveFile(filename);
  return 1;
  }


// Save Update
long TextWindow::onUpdSave(FXObject* sender,FXSelector,void*){
  sender->handle(this,isModified()?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Save As
long TextWindow::onCmdSaveAs(FXObject*,FXSelector,void*){
  FXFileDialog savedialog(this,tr("Save Document"));
  FXString file=filename;
  savedialog.setSelectMode(SELECTFILE_ANY);
  savedialog.setPatternList(getPatterns());
  savedialog.setCurrentPattern(getCurrentPattern());
  savedialog.setFilename(file);
  if(savedialog.execute()){
    setCurrentPattern(savedialog.getCurrentPattern());
    file=savedialog.getFilename();
    if(FXStat::exists(file)){
      if(MBOX_CLICKED_NO==FXMessageBox::question(this,MBOX_YES_NO,tr("Overwrite Document"),tr("Overwrite existing document: %s?"),file.text())) return 1;
      }
    saveFile(file);
    }
  return 1;
  }


// Close window
FXbool TextWindow::close(FXbool notify){

  // Prompt to save changes
  if(!saveChanges()) return FALSE;

  // Save settings
  writeRegistry();

  // Perform normal close stuff
  return FXMainWindow::close(notify);
  }


// User clicks on one of the window menus
long TextWindow::onCmdWindow(FXObject*,FXSelector sel,void*){
  FXint which=FXSELID(sel)-ID_WINDOW_1;
  if(which<getApp()->windowlist.no()){
    getApp()->windowlist[which]->raise();
    getApp()->windowlist[which]->setFocus();
    }
  return 1;
  }


// Update handler for window menus
long TextWindow::onUpdWindow(FXObject *sender,FXSelector sel,void*){
  FXint which=FXSELID(sel)-ID_WINDOW_1;
  if(which<getApp()->windowlist.no()){
    TextWindow *window=getApp()->windowlist[which];
    FXString string;
    if(which<9)
      string.format("&%d %s",which+1,window->getTitle().text());
    else
      string.format("1&0 %s",window->getTitle().text());
    sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETSTRINGVALUE),(void*)&string);
    if(window==getApp()->getActiveWindow())
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_CHECK),NULL);
    else
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_UNCHECK),NULL);
    sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SHOW),NULL);
    }
  else{
    sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_HIDE),NULL);
    }
  return 1;
  }


// Update title from current filename
long TextWindow::onUpdate(FXObject* sender,FXSelector sel,void* ptr){
  FXMainWindow::onUpdate(sender,sel,ptr);
  FXString title=FXPath::name(getFilename());
  if(isModified()) title+=tr(" (changed)");
  FXString directory=FXPath::directory(getFilename());
  if(!directory.empty()) title+=" - " + directory;
  setTitle(title);
  return 1;
  }


// Print the text
long TextWindow::onCmdPrint(FXObject*,FXSelector,void*){
  FXPrintDialog dlg(this,tr("Print File"));
  FXPrinter printer;
  if(dlg.execute()){
    dlg.getPrinter(printer);
    FXTRACE((100,"Printer = %s\n",printer.name.text()));
    }
  return 1;
  }


/*******************************************************************************/

// Save settings
long TextWindow::onCmdSaveSettings(FXObject*,FXSelector,void*){
  writeRegistry();
  getApp()->reg().write();
  return 1;
  }


// Toggle wrap mode
long TextWindow::onCmdWrap(FXObject*,FXSelector,void*){
  editor->setTextStyle(editor->getTextStyle()^TEXT_WORDWRAP);
  return 1;
  }


// Update toggle wrap mode
long TextWindow::onUpdWrap(FXObject* sender,FXSelector,void*){
  if(editor->getTextStyle()&TEXT_WORDWRAP)
    sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
  else
    sender->handle(this,FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Toggle fixed wrap mode
long TextWindow::onCmdWrapFixed(FXObject*,FXSelector,void*){
  editor->setTextStyle(editor->getTextStyle()^TEXT_FIXEDWRAP);
  return 1;
  }


// Update toggle fixed wrap mode
long TextWindow::onUpdWrapFixed(FXObject* sender,FXSelector,void*){
  if(editor->getTextStyle()&TEXT_FIXEDWRAP)
    sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
  else
    sender->handle(this,FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }

// Toggle show active background mode
long TextWindow::onCmdShowActive(FXObject*,FXSelector,void*){
  editor->setTextStyle(editor->getTextStyle()^TEXT_SHOWACTIVE);
  return 1;
  }


// Update show active background mode
long TextWindow::onUpdShowActive(FXObject* sender,FXSelector,void*){
  if(editor->getTextStyle()&TEXT_SHOWACTIVE)
    sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
  else
    sender->handle(this,FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }

// Toggle strip returns mode
long TextWindow::onCmdStripReturns(FXObject*,FXSelector,void* ptr){
  stripcr=(FXbool)(FXuval)ptr;
  return 1;
  }


// Update toggle strip returns mode
long TextWindow::onUpdStripReturns(FXObject* sender,FXSelector,void*){
  if(stripcr)
    sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
  else
    sender->handle(this,FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Enable warning if file changed externally
long TextWindow::onCmdWarnChanged(FXObject*,FXSelector,void* ptr){
  warnchanged=(FXbool)(FXuval)ptr;
  return 1;
  }


// Update check button for warning
long TextWindow::onUpdWarnChanged(FXObject* sender,FXSelector,void*){
  if(warnchanged)
    sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
  else
    sender->handle(this,FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Toggle strip spaces mode
long TextWindow::onCmdStripSpaces(FXObject*,FXSelector,void* ptr){
  stripsp=(FXbool)(FXuval)ptr;
  return 1;
  }


// Update toggle strip spaces mode
long TextWindow::onUpdStripSpaces(FXObject* sender,FXSelector,void*){
  if(stripsp)
    sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
  else
    sender->handle(this,FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Toggle append newline mode
long TextWindow::onCmdAppendNewline(FXObject*,FXSelector,void* ptr){
  appendnl=(FXbool)(FXuval)ptr;
  return 1;
  }


// Update toggle append newline mode
long TextWindow::onUpdAppendNewline(FXObject* sender,FXSelector,void*){
  if(appendnl)
    sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
  else
    sender->handle(this,FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }

// Change tab columns
long TextWindow::onCmdTabColumns(FXObject* sender,FXSelector,void*){
  FXint tabs;
  sender->handle(this,FXSEL(SEL_COMMAND,ID_GETINTVALUE),(void*)&tabs);
  editor->setTabColumns(tabs);
  return 1;
  }


// Update tab columns
long TextWindow::onUpdTabColumns(FXObject* sender,FXSelector,void*){
  FXint tabs=editor->getTabColumns();
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&tabs);
  return 1;
  }


// Change wrap columns
long TextWindow::onCmdWrapColumns(FXObject* sender,FXSelector,void*){
  FXint wrap;
  sender->handle(this,FXSEL(SEL_COMMAND,ID_GETINTVALUE),(void*)&wrap);
  editor->setWrapColumns(wrap);
  return 1;
  }


// Update wrap columns
long TextWindow::onUpdWrapColumns(FXObject* sender,FXSelector,void*){
  FXint wrap=editor->getWrapColumns();
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&wrap);
  return 1;
  }


// Change line number columna
long TextWindow::onCmdLineNumbers(FXObject* sender,FXSelector,void*){
  FXint cols;
  sender->handle(this,FXSEL(SEL_COMMAND,ID_GETINTVALUE),(void*)&cols);
  editor->setBarColumns(cols);
  return 1;
  }


// Update line number columna
long TextWindow::onUpdLineNumbers(FXObject* sender,FXSelector,void*){
  FXint cols=editor->getBarColumns();
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&cols);
  return 1;
  }


// Toggle insertion of tabs
long TextWindow::onCmdInsertTabs(FXObject*,FXSelector,void*){
  editor->setTextStyle(editor->getTextStyle()^TEXT_NO_TABS);
  return 1;
  }


// Update insertion of tabs
long TextWindow::onUpdInsertTabs(FXObject* sender,FXSelector,void*){
  sender->handle(this,(editor->getTextStyle()&TEXT_NO_TABS)?FXSEL(SEL_COMMAND,ID_UNCHECK):FXSEL(SEL_COMMAND,ID_CHECK),NULL);
  return 1;
  }


// Toggle autoindent
long TextWindow::onCmdAutoIndent(FXObject*,FXSelector,void*){
  editor->setTextStyle(editor->getTextStyle()^TEXT_AUTOINDENT);
  return 1;
  }


// Update autoindent
long TextWindow::onUpdAutoIndent(FXObject* sender,FXSelector,void*){
  sender->handle(this,(editor->getTextStyle()&TEXT_AUTOINDENT)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Set brace match time
long TextWindow::onCmdBraceMatch(FXObject* sender,FXSelector,void*){
  FXuint value;
  sender->handle(this,FXSEL(SEL_COMMAND,ID_GETINTVALUE),(void*)&value);
  editor->setHiliteMatchTime(value);
  return 1;
  }


// Update brace match time
long TextWindow::onUpdBraceMatch(FXObject* sender,FXSelector,void*){
  FXuint value=editor->getHiliteMatchTime();
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&value);
  return 1;
  }


// Change word delimiters
long TextWindow::onCmdDelimiters(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,ID_GETSTRINGVALUE),(void*)&delimiters);
  editor->setDelimiters(delimiters.text());
  return 1;
  }


// Update word delimiters
long TextWindow::onUpdDelimiters(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&delimiters);
  return 1;
  }


// Update box for overstrike mode display
long TextWindow::onUpdOverstrike(FXObject* sender,FXSelector,void*){
  FXString mode((editor->getTextStyle()&TEXT_OVERSTRIKE)?"OVR":"INS");
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&mode);
  return 1;
  }


// Update box for readonly display
long TextWindow::onUpdReadOnly(FXObject* sender,FXSelector,void*){
  FXString rw((editor->getTextStyle()&TEXT_READONLY)?"RO":"RW");
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&rw);
  return 1;
  }


// Update box for size display
long TextWindow::onUpdNumRows(FXObject* sender,FXSelector,void*){
  FXuint size=editor->getNumRows();
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&size);
  return 1;
  }


// Set TextWindow path
long TextWindow::onCmdIncludePaths(FXObject*,FXSelector,void*){
  FXInputDialog::getString(searchpath,this,tr("Change include file search path"),tr("Specify a list of directories separated by a `" PATHLISTSEPSTRING "' where include files are to be found.\nFor example:\n\n  /usr/include" PATHLISTSEPSTRING "/usr/local/include\n\nThis list will be used to locate the selected file name."));
  return 1;
  }


// Set scroll wheel lines (Mathew Robertson <mathew@optushome.com.au>)
long TextWindow::onCmdWheelAdjust(FXObject* sender,FXSelector,void*){
  FXuint value;
  sender->handle(this,FXSEL(SEL_COMMAND,ID_GETINTVALUE),(void*)&value);
  getApp()->setWheelLines(value);
  return 1;
  }


// Update brace match time
long TextWindow::onUpdWheelAdjust(FXObject* sender,FXSelector,void*){
  FXuint value=getApp()->getWheelLines();
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&value);
  return 1;
  }


/*******************************************************************************/


// Change text color
long TextWindow::onCmdTextForeColor(FXObject*,FXSelector,void* ptr){
  editor->setTextColor((FXColor)(FXuval)ptr);
  return 1;
  }

// Update text color
long TextWindow::onUpdTextForeColor(FXObject* sender,FXSelector,void*){
  FXColor color=editor->getTextColor();
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
  return 1;
  }


// Change text background color
long TextWindow::onCmdTextBackColor(FXObject*,FXSelector,void* ptr){
  editor->setBackColor((FXColor)(FXuval)ptr);
  return 1;
  }

// Update background color
long TextWindow::onUpdTextBackColor(FXObject* sender,FXSelector,void*){
  FXColor color=editor->getBackColor();
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
  return 1;
  }


// Change selected text foreground color
long TextWindow::onCmdTextSelForeColor(FXObject*,FXSelector,void* ptr){
  editor->setSelTextColor((FXColor)(FXuval)ptr);
  return 1;
  }


// Update selected text foregoround color
long TextWindow::onUpdTextSelForeColor(FXObject* sender,FXSelector,void*){
  FXColor color=editor->getSelTextColor();
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
  return 1;
  }


// Change selected text background color
long TextWindow::onCmdTextSelBackColor(FXObject*,FXSelector,void* ptr){
  editor->setSelBackColor((FXColor)(FXuval)ptr);
  return 1;
  }

// Update selected text background color
long TextWindow::onUpdTextSelBackColor(FXObject* sender,FXSelector,void*){
  FXColor color=editor->getSelBackColor();
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
  return 1;
  }


// Change hilight text color
long TextWindow::onCmdTextHiliteForeColor(FXObject*,FXSelector,void* ptr){
  editor->setHiliteTextColor((FXColor)(FXuval)ptr);
  return 1;
  }

// Update hilight text color
long TextWindow::onUpdTextHiliteForeColor(FXObject* sender,FXSelector,void*){
  FXColor color=editor->getHiliteTextColor();
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
  return 1;
  }


// Change hilight text background color
long TextWindow::onCmdTextHiliteBackColor(FXObject*,FXSelector,void* ptr){
  editor->setHiliteBackColor((FXColor)(FXuval)ptr);
  return 1;
  }

// Update hilight text background color
long TextWindow::onUpdTextHiliteBackColor(FXObject* sender,FXSelector,void*){
  FXColor color=editor->getHiliteBackColor();
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
  return 1;
  }


// Change active text background color
long TextWindow::onCmdTextActBackColor(FXObject*,FXSelector,void* ptr){
  editor->setActiveBackColor((FXColor)(FXuval)ptr);
  return 1;
  }

// Update active text background color
long TextWindow::onUpdTextActBackColor(FXObject* sender,FXSelector,void*){
  FXColor color=editor->getActiveBackColor();
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
  return 1;
  }


// Change cursor color
long TextWindow::onCmdTextCursorColor(FXObject*,FXSelector,void* ptr){
  editor->setCursorColor((FXColor)(FXuval)ptr);
  return 1;
  }

// Update cursor color
long TextWindow::onUpdTextCursorColor(FXObject* sender,FXSelector,void*){
  FXColor color=editor->getCursorColor();
  sender->handle(sender,FXSEL(SEL_COMMAND,FXWindow::ID_SETINTVALUE),(void*)&color);
  return 1;
  }


// Change line numbers background color
long TextWindow::onCmdTextBarColor(FXObject*,FXSelector,void* ptr){
  editor->setBarColor((FXColor)(FXuval)ptr);
  return 1;
  }


// Update line numbers background color
long TextWindow::onUpdTextBarColor(FXObject* sender,FXSelector,void*){
  FXColor color=editor->getBarColor();
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
  return 1;
  }

// Change line numbers color
long TextWindow::onCmdTextNumberColor(FXObject*,FXSelector,void* ptr){
  editor->setNumberColor((FXColor)(FXuval)ptr);
  return 1;
  }


// Update line numbers color
long TextWindow::onUpdTextNumberColor(FXObject* sender,FXSelector,void*){
  FXColor color=editor->getNumberColor();
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
  return 1;
  }


// Change both tree background color
long TextWindow::onCmdDirBackColor(FXObject*,FXSelector,void* ptr){
  dirlist->setBackColor((FXColor)(FXuval)ptr);
  return 1;
  }


// Update background color
long TextWindow::onUpdDirBackColor(FXObject* sender,FXSelector,void*){
  FXColor color=dirlist->getBackColor();
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
  return 1;
  }


// Change both text and tree selected background color
long TextWindow::onCmdDirSelBackColor(FXObject*,FXSelector,void* ptr){
  dirlist->setSelBackColor((FXColor)(FXuval)ptr);
  return 1;
  }

// Update selected background color
long TextWindow::onUpdDirSelBackColor(FXObject* sender,FXSelector,void*){
  FXColor color=dirlist->getSelBackColor();
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
  return 1;
  }


// Change both text and tree text color
long TextWindow::onCmdDirForeColor(FXObject*,FXSelector,void* ptr){
  dirlist->setTextColor((FXColor)(FXuval)ptr);
  return 1;
  }

// Forward GUI update to text widget
long TextWindow::onUpdDirForeColor(FXObject* sender,FXSelector,void*){
  FXColor color=dirlist->getTextColor();
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
  return 1;
  }


// Change both text and tree
long TextWindow::onCmdDirSelForeColor(FXObject*,FXSelector,void* ptr){
  dirlist->setSelTextColor((FXColor)(FXuval)ptr);
  return 1;
  }


// Forward GUI update to text widget
long TextWindow::onUpdDirSelForeColor(FXObject* sender,FXSelector,void*){
  FXColor color=dirlist->getSelTextColor();
  sender->handle(sender,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
  return 1;
  }


// Change both text and tree
long TextWindow::onCmdDirLineColor(FXObject*,FXSelector,void* ptr){
  dirlist->setLineColor((FXColor)(FXuval)ptr);
  return 1;
  }


// Forward GUI update to text widget
long TextWindow::onUpdDirLineColor(FXObject* sender,FXSelector,void*){
  FXColor color=dirlist->getLineColor();
  sender->handle(sender,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
  return 1;
  }


// Change jump scrolling mode
long TextWindow::onCmdJumpScroll(FXObject*,FXSelector,void*){
  editor->setScrollStyle(editor->getScrollStyle()^SCROLLERS_DONT_TRACK);
  return 1;
  }


// Update change jump scrolling mode
long TextWindow::onUpdJumpScroll(FXObject* sender,FXSelector,void*){
  sender->handle(this,(editor->getScrollStyle()&SCROLLERS_DONT_TRACK)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Change the pattern
long TextWindow::onCmdFilter(FXObject*,FXSelector,void* ptr){
  dirlist->setPattern(FXFileSelector::patternFromText((FXchar*)ptr));
  dirlist->makeItemVisible(dirlist->getCurrentItem());
  return 1;
  }


/*******************************************************************************/


// Text inserted; callback has [pos nins]
long TextWindow::onTextInserted(FXObject*,FXSelector,void* ptr){
  const FXTextChange *change=(const FXTextChange*)ptr;

  FXTRACE((140,"Inserted: pos=%d ndel=%d nins=%d\n",change->pos,change->ndel,change->nins));

  // Log undo record
  if(!undolist.busy()){
    undolist.add(new FXTextInsert(editor,change->pos,change->nins,change->ins));
    if(undolist.size()>MAXUNDOSIZE) undolist.trimSize(KEEPUNDOSIZE);
    }

  // Update bookmark locations
  updateBookmarks(change->pos,change->ndel,change->nins);

  // Restyle text
  restyleText(change->pos,change->ndel,change->nins);

  return 1;
  }


// Text deleted; callback has [pos ndel]
long TextWindow::onTextDeleted(FXObject*,FXSelector,void* ptr){
  const FXTextChange *change=(const FXTextChange*)ptr;

  FXTRACE((140,"Deleted: pos=%d ndel=%d nins=%d\n",change->pos,change->ndel,change->nins));

  // Log undo record
  if(!undolist.busy()){
    undolist.add(new FXTextDelete(editor,change->pos,change->ndel,change->del));
    if(undolist.size()>MAXUNDOSIZE) undolist.trimSize(KEEPUNDOSIZE);
    }

  // Update bookmark locations
  updateBookmarks(change->pos,change->ndel,change->nins);

  // Restyle text
  restyleText(change->pos,change->ndel,change->nins);

  return 1;
  }


// Text replaced; callback has [pos ndel nins]
long TextWindow::onTextReplaced(FXObject*,FXSelector,void* ptr){
  const FXTextChange *change=(const FXTextChange*)ptr;

  FXTRACE((140,"Replaced: pos=%d ndel=%d nins=%d\n",change->pos,change->ndel,change->nins));

  // Log undo record
  if(!undolist.busy()){
    undolist.add(new FXTextReplace(editor,change->pos,change->ndel,change->nins,change->del,change->ins));
    if(undolist.size()>MAXUNDOSIZE) undolist.trimSize(KEEPUNDOSIZE);
    }

  // Update bookmark locations
  updateBookmarks(change->pos,change->ndel,change->nins);

  // Restyle text
  restyleText(change->pos,change->ndel,change->nins);

  return 1;
  }


// Text cursor pos changed
long TextWindow::onTextChanged(FXObject*,FXSelector,void* ptr){
  FXTRACE((1,"onTextChanged cursorpos=%d\n",(FXint)(FXival)ptr));
  return 1;
  }


// Released right button
long TextWindow::onTextRightMouse(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(!event->moved){
    popupmenu->popup(NULL,event->root_x,event->root_y);
    getApp()->runModalWhileShown(popupmenu);
    }
  return 1;
  }


/*******************************************************************************/


// Check file when focus moves in
long TextWindow::onFocusIn(FXObject* sender,FXSelector sel,void* ptr){
  register FXTime t;
  FXMainWindow::onFocusIn(sender,sel,ptr);
  if(warnchanged && filetime!=0){
    t=FXStat::modified(filename);
    if(t && t!=filetime){
      filetime=t;
      if(MBOX_CLICKED_OK==FXMessageBox::warning(this,MBOX_OK_CANCEL,tr("File Was Changed"),tr("%s\nwas changed by another program. Reload this file from disk?"),filename.text())){
        FXint top=editor->getTopLine();
        FXint pos=editor->getCursorPos();
        loadFile(filename);
        editor->setTopLine(top);
        editor->setCursorPos(pos);
        }
      }
    }
  return 1;
  }


// Update clock
long TextWindow::onClock(FXObject*,FXSelector,void*){
  FXTime current=FXSystem::now();
  clock->setText(FXSystem::time("%H:%M:%S",current));
  clock->setTipText(FXSystem::time("%A %B %d %Y",current));
  getApp()->addTimeout(this,ID_CLOCKTIME,CLOCKTIMER);
  return 0;
  }


/*******************************************************************************/


// Next bookmarked place
long TextWindow::onCmdNextMark(FXObject*,FXSelector,void*){
  register FXint b;
  if(bookmark[0]){
    FXint pos=editor->getCursorPos();
    for(b=0; b<=9; b++){
      if(bookmark[b]==0) break;
      if(bookmark[b]>pos){ gotoBookmark(b); break; }
      }
    }
  return 1;
  }


// Sensitize if bookmark beyond cursor pos
long TextWindow::onUpdNextMark(FXObject* sender,FXSelector,void*){
  register FXint b;
  if(bookmark[0]){
    FXint pos=editor->getCursorPos();
    for(b=0; b<=9; b++){
      if(bookmark[b]==0) break;
      if(bookmark[b]>pos){ sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL); return 1; }
      }
    }
  sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Previous bookmarked place
long TextWindow::onCmdPrevMark(FXObject*,FXSelector,void*){
  register FXint b;
  if(bookmark[0]){
    FXint pos=editor->getCursorPos();
    for(b=9; 0<=b; b--){
      if(bookmark[b]==0) continue;
      if(bookmark[b]<pos){ gotoBookmark(b); break; }
      }
    }
  return 1;
  }


// Sensitize if bookmark before cursor pos
long TextWindow::onUpdPrevMark(FXObject* sender,FXSelector,void*){
  register FXint b;
  if(bookmark[0]){
    FXint pos=editor->getCursorPos();
    for(b=9; 0<=b; b--){
      if(bookmark[b]==0) continue;
      if(bookmark[b]<pos){ sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL); return 1; }
      }
    }
  sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Set bookmark
long TextWindow::onCmdSetMark(FXObject*,FXSelector,void*){
  setBookmark(editor->getCursorPos());
  return 1;
  }


// Update set bookmark
long TextWindow::onUpdSetMark(FXObject* sender,FXSelector,void*){
  sender->handle(this,(bookmark[9]==0)?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Clear bookmarks
long TextWindow::onCmdClearMarks(FXObject*,FXSelector,void*){
  clearBookmarks();
  return 1;
  }


// Add bookmark at current cursor position; we force the cursor
// position to be somewhere in the currently visible text.
void TextWindow::setBookmark(FXint pos){
  register FXint b;
  if(!bookmark[9]){
    for(b=9; 0<b && (bookmark[b-1]==0 || bookmark[b-1]>pos); b--){
      bookmark[b]=bookmark[b-1];
      }
    bookmark[b]=pos;
    }
  }


// Update bookmarks upon a text mutation, deleting the bookmark
// if it was inside the deleted text and moving its position otherwise
void TextWindow::updateBookmarks(FXint pos,FXint nd,FXint ni){
  register FXint delta=ni-nd,i,j,p;
  for(i=j=0; j<=9; j++){
    p=bookmark[j];
    bookmark[j]=0;
    if(pos+nd<=p)
      bookmark[i++]=p+delta;
    else if(p<=pos)
      bookmark[i++]=p;
    }
  }


// Goto bookmark
void TextWindow::gotoBookmark(FXint b){
  register FXint p=bookmark[b];
  if(p){
    if(!editor->isPosVisible(p)){
      editor->setCenterLine(p);
      }
    editor->setCursorPos(p);
    }
  }


// Clear bookmarks
void TextWindow::clearBookmarks(){
  memset(bookmark,0,sizeof(bookmark));
  }


// Read bookmarks associated with file
void TextWindow::readBookmarks(const FXString& file){
  const FXchar *marks=getApp()->reg().readStringEntry("BOOKMARKS",FXPath::name(file).text(),"0,0,0,0,0,0,0,0,0,0");
  sscanf(marks,"%d,%d,%d,%d,%d,%d,%d,%d,%d,%d",&bookmark[0],&bookmark[1],&bookmark[2],&bookmark[3],&bookmark[4],&bookmark[5],&bookmark[6],&bookmark[7],&bookmark[8],&bookmark[9]);
  }


// Write bookmarks associated with file, if any were set
void TextWindow::writeBookmarks(const FXString& file){
  if(savemarks){
    if(bookmark[0] || bookmark[1] || bookmark[2] || bookmark[3] || bookmark[4] || bookmark[5] || bookmark[6] || bookmark[7] || bookmark[8] || bookmark[9]){
      FXchar marks[1000];
      sprintf(marks,"%d,%d,%d,%d,%d,%d,%d,%d,%d,%d",bookmark[0],bookmark[1],bookmark[2],bookmark[3],bookmark[4],bookmark[5],bookmark[6],bookmark[7],bookmark[8],bookmark[9]);
      getApp()->reg().writeStringEntry("BOOKMARKS",FXPath::name(file).text(),marks);
      }
    else{
      getApp()->reg().deleteEntry("BOOKMARKS",FXPath::name(file).text());
      }
    }
  }


// Toggle saving of bookmarks
long TextWindow::onCmdSaveMarks(FXObject*,FXSelector,void*){
  savemarks=!savemarks;
  if(!savemarks) getApp()->reg().deleteSection("BOOKMARKS");
  return 1;
  }


// Update saving bookmarks
long TextWindow::onUpdSaveMarks(FXObject* sender,FXSelector,void*){
  sender->handle(this,savemarks?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Toggle saving of views
long TextWindow::onCmdSaveViews(FXObject*,FXSelector,void*){
  saveviews=!saveviews;
  if(!saveviews) getApp()->reg().deleteSection("VIEW");
  return 1;
  }


// Update saving views
long TextWindow::onUpdSaveViews(FXObject* sender,FXSelector,void*){
  sender->handle(this,saveviews?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Read view of the file
void TextWindow::readView(const FXString& file){
  editor->setTopLine(getApp()->reg().readIntEntry("VIEW",FXPath::name(file).text(),0));
  }


// Write current view of the file
void TextWindow::writeView(const FXString& file){
  if(saveviews){
    if(editor->getTopLine()){
      getApp()->reg().writeIntEntry("VIEW",FXPath::name(file).text(),editor->getTopLine());
      }
    else{
      getApp()->reg().deleteEntry("VIEW",FXPath::name(file).text());
      }
    }
  }


/*******************************************************************************/

// Determine language
void TextWindow::determineSyntax(){
  register FXint syn;

  FXTRACE((1,"Determine Language for window %s\n",filename.text()));

  // Get filename part of pathname
  FXString file=FXPath::name(filename);

  // First, try match based on file extension
  for(syn=0; syn<getApp()->syntaxes.no(); syn++){
    if(getApp()->syntaxes[syn]->matchFilename(file)){
      FXTRACE((1,"language %s matched wildcard\n",getApp()->syntaxes[syn]->getName().text()));
      setSyntax(getApp()->syntaxes[syn]);
      return;
      }
    }

  // Get the first part of contents
  FXString text('\0',FXMIN(editor->getLength(),512));
  editor->extractText(&text[0],0,text.length());

  // And try to match based on contents
  for(syn=0; syn<getApp()->syntaxes.no(); syn++){
    if(getApp()->syntaxes[syn]->matchContents(text)){
      FXTRACE((1,"language %s matched contents\n",getApp()->syntaxes[syn]->getName().text()));
      setSyntax(getApp()->syntaxes[syn]);
      return;
      }
    }

  // Back to plain text mode
  setSyntax(NULL);
  }


// Set syntax by name
FXbool TextWindow::forceSyntax(const FXString& language){
  register FXint syn;
  for(syn=0; syn<getApp()->syntaxes.no(); syn++){
    if(getApp()->syntaxes[syn]->getName()==language){
      setSyntax(getApp()->syntaxes[syn]);
      return TRUE;
      }
    }
  return FALSE;
  }


// Change style colors
void TextWindow::setStyleColors(FXint index,const FXHiliteStyle& style){
  styles[index]=style;
  editor->update();
  }


// Switch syntax
long TextWindow::onCmdSyntaxSwitch(FXObject*,FXSelector sel,void*){
  FXint syn=FXSELID(sel)-ID_SYNTAX_FIRST;
  FXASSERT(0<=syn && syn<=getApp()->syntaxes.no());
  setSyntax(syn?getApp()->syntaxes[syn-1]:NULL);
  return 1;
  }


// Switch syntax
long TextWindow::onUpdSyntaxSwitch(FXObject* sender,FXSelector sel,void*){
  FXint syn=FXSELID(sel)-ID_SYNTAX_FIRST;
  FXASSERT(0<=syn && syn<=getApp()->syntaxes.no());
  FXSyntax *sntx=syn?getApp()->syntaxes[syn-1]:NULL;
  sender->handle(this,(sntx==syntax)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Change style index
long TextWindow::onCmdStyleIndex(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,ID_GETINTVALUE),(void*)&currentstyle);
  return 1;
  }


// Update style index
long TextWindow::onUpdStyleIndex(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&currentstyle);
  return 1;
  }


// Change style flags
long TextWindow::onCmdStyleFlags(FXObject*,FXSelector sel,void*){
  if(0<=currentstyle && currentstyle<styles.no()){
    switch(FXSELID(sel)){
      case ID_STYLE_UNDERLINE: styles[currentstyle].style^=FXText::STYLE_UNDERLINE; break;
      case ID_STYLE_STRIKEOUT: styles[currentstyle].style^=FXText::STYLE_STRIKEOUT; break;
      case ID_STYLE_BOLD:      styles[currentstyle].style^=FXText::STYLE_BOLD; break;
      }
    setStyleColors(currentstyle,styles[currentstyle]);
    }
  return 1;
  }


// Update style flags
long TextWindow::onUpdStyleFlags(FXObject* sender,FXSelector sel,void*){
  FXuint bit=0;
  if(0<=currentstyle && currentstyle<styles.no()){
    switch(FXSELID(sel)){
      case ID_STYLE_UNDERLINE: bit=styles[currentstyle].style&FXText::STYLE_UNDERLINE; break;
      case ID_STYLE_STRIKEOUT: bit=styles[currentstyle].style&FXText::STYLE_STRIKEOUT; break;
      case ID_STYLE_BOLD:      bit=styles[currentstyle].style&FXText::STYLE_BOLD; break;
      }
    sender->handle(this,bit?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
    sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
    }
  else{
    sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
    }
  return 1;
  }


// Change style color
long TextWindow::onCmdStyleColor(FXObject*,FXSelector sel,void* ptr){
  if(0<=currentstyle && currentstyle<styles.no()){
    switch(FXSELID(sel)){
      case ID_STYLE_NORMAL_FG: styles[currentstyle].normalForeColor=(FXColor)(FXuval)ptr; break;
      case ID_STYLE_NORMAL_BG: styles[currentstyle].normalBackColor=(FXColor)(FXuval)ptr; break;
      case ID_STYLE_SELECT_FG: styles[currentstyle].selectForeColor=(FXColor)(FXuval)ptr; break;
      case ID_STYLE_SELECT_BG: styles[currentstyle].selectBackColor=(FXColor)(FXuval)ptr; break;
      case ID_STYLE_HILITE_FG: styles[currentstyle].hiliteForeColor=(FXColor)(FXuval)ptr; break;
      case ID_STYLE_HILITE_BG: styles[currentstyle].hiliteBackColor=(FXColor)(FXuval)ptr; break;
      case ID_STYLE_ACTIVE_BG: styles[currentstyle].activeBackColor=(FXColor)(FXuval)ptr; break;
      }
    writeStyleForRule(syntax->getRule(currentstyle+1)->getName(),styles[currentstyle]);
    setStyleColors(currentstyle,styles[currentstyle]);
    }
  return 1;
  }


// Update style color
long TextWindow::onUpdStyleColor(FXObject* sender,FXSelector sel,void*){
  FXColor color=0;
  if(0<=currentstyle && currentstyle<styles.no()){
    switch(FXSELID(sel)){
      case ID_STYLE_NORMAL_FG: color=styles[currentstyle].normalForeColor; break;
      case ID_STYLE_NORMAL_BG: color=styles[currentstyle].normalBackColor; break;
      case ID_STYLE_SELECT_FG: color=styles[currentstyle].selectForeColor; break;
      case ID_STYLE_SELECT_BG: color=styles[currentstyle].selectBackColor; break;
      case ID_STYLE_HILITE_FG: color=styles[currentstyle].hiliteForeColor; break;
      case ID_STYLE_HILITE_BG: color=styles[currentstyle].hiliteBackColor; break;
      case ID_STYLE_ACTIVE_BG: color=styles[currentstyle].activeBackColor; break;
      }
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
    sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
    }
  else{
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
    sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
    }
  return 1;
  }


// Set language
void TextWindow::setSyntax(FXSyntax* syn){
  register FXint rule;
  syntax=syn;
  if(syntax){
    editor->setDelimiters(syntax->getDelimiters().text());
    styles.no(syntax->getNumRules()-1);
    for(rule=1; rule<syntax->getNumRules(); rule++){
      styles[rule-1]=readStyleForRule(syntax->getRule(rule)->getName());
      }
    editor->setHiliteStyles(styles.data());
    editor->setStyled(colorize);
    restyleText();
    currentstyle=0;
    }
  else{
    editor->setDelimiters(FXText::textDelimiters);
    editor->setHiliteStyles(NULL);
    editor->setStyled(FALSE);
    currentstyle=-1;
    }
  }


// Restyle entire text
void TextWindow::restyleText(){
  FXchar *text,*style;
  FXint head,tail,len;
  if(colorize && syntax){
    len=editor->getLength();
    FXMALLOC(&text,FXchar,len+len);
    style=text+len;
    editor->extractText(text,0,len);
    syntax->getRule(0)->stylize(text,style,0,len,head,tail);
    editor->changeStyle(0,style,len);
    FXFREE(&text);
    }
  }


// Read style
FXHiliteStyle TextWindow::readStyleForRule(const FXString& name){
  FXchar nfg[100],nbg[100],sfg[100],sbg[100],hfg[100],hbg[100],abg[100]; FXint sty;
  FXHiliteStyle style={0,0,0,0,0,0,0,0};
  if(getApp()->reg().readFormatEntry("STYLE",name.text(),"%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%d",nfg,nbg,sfg,sbg,hfg,hbg,abg,&sty)==8){
    style.normalForeColor=fxcolorfromname(nfg);
    style.normalBackColor=fxcolorfromname(nbg);
    style.selectForeColor=fxcolorfromname(sfg);
    style.selectBackColor=fxcolorfromname(sbg);
    style.hiliteForeColor=fxcolorfromname(hfg);
    style.hiliteBackColor=fxcolorfromname(hbg);
    style.activeBackColor=fxcolorfromname(abg);
    style.style=sty;
    }
  return style;
  }


// Write style
void TextWindow::writeStyleForRule(const FXString& name,const FXHiliteStyle& style){
  FXchar nfg[100],nbg[100],sfg[100],sbg[100],hfg[100],hbg[100],abg[100];
  fxnamefromcolor(nfg,style.normalForeColor);
  fxnamefromcolor(nbg,style.normalBackColor);
  fxnamefromcolor(sfg,style.selectForeColor);
  fxnamefromcolor(sbg,style.selectBackColor);
  fxnamefromcolor(hfg,style.hiliteForeColor);
  fxnamefromcolor(hbg,style.hiliteBackColor);
  fxnamefromcolor(abg,style.activeBackColor);
  getApp()->reg().writeFormatEntry("STYLE",name.text(),"%s,%s,%s,%s,%s,%s,%s,%d",nfg,nbg,sfg,sbg,hfg,hbg,abg,style.style);
  }

/*

// Read styles
void Adie::readStyles(){
  FXchar nfg[100],nbg[100],sfg[100],sbg[100],hfg[100],hbg[100],abg[100],index[10],name[200];
  FXint  sty,i;
  nstyles=0;
  for(i=0; i<MAXSTYLES; i++){
    sprintf(index,"%d",i+1);
    if(reg().readFormatEntry("STYLES",index,"%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%d",name,nfg,nbg,sfg,sbg,hfg,hbg,abg,&sty)!=9) break;
    FXTRACE((1,"name=\"%s\" nfg=%s nbg=%s sfg=%s sbg=%s hfg=%s hbg=%s abg=%s sty=%d\n",name,nfg,nbg,sfg,sbg,hfg,hbg,abg,sty));
    stylename[i]=name;
    stylecolor[i].normalForeColor=fxcolorfromname(nfg);
    stylecolor[i].normalBackColor=fxcolorfromname(nbg);
    stylecolor[i].selectForeColor=fxcolorfromname(sfg);
    stylecolor[i].selectBackColor=fxcolorfromname(sbg);
    stylecolor[i].hiliteForeColor=fxcolorfromname(hfg);
    stylecolor[i].hiliteBackColor=fxcolorfromname(hbg);
    stylecolor[i].activeBackColor=fxcolorfromname(abg);
    stylecolor[i].style=sty;
    nstyles++;
    }
  }


// Write styles
void Adie::writeStyles(){
  FXchar nfg[100],nbg[100],sfg[100],sbg[100],hfg[100],hbg[100],abg[100],name[10];
  FXint  i;
  reg().deleteSection("STYLES");
  for(i=0; i<nstyles; i++){
    fxnamefromcolor(nfg,stylecolor[i].normalForeColor);
    fxnamefromcolor(nbg,stylecolor[i].normalBackColor);
    fxnamefromcolor(sfg,stylecolor[i].selectForeColor);
    fxnamefromcolor(sbg,stylecolor[i].selectBackColor);
    fxnamefromcolor(hfg,stylecolor[i].hiliteForeColor);
    fxnamefromcolor(hbg,stylecolor[i].hiliteBackColor);
    fxnamefromcolor(abg,stylecolor[i].activeBackColor);
    sprintf(name,"%d",i+1);
    reg().writeFormatEntry("STYLES",name,"%s,%s,%s,%s,%s,%s,%s,%s,%d",stylename[i].text(),nfg,nbg,sfg,sbg,hfg,hbg,abg,stylecolor[i].style);
    }
  }
  // Get style pointer
  FXHiliteStyle* getStyles() const { return styles.data(); }

  // Get particular style
  const FXHiliteStyle& getStyle(FXint index) const { return styles[index]; }

  // Change particular style
  void setStyle(FXint index,const FXHiliteStyle& s){ styles[index]=s; }

*/

// Scan backward by context amount
FXint TextWindow::backwardByContext(FXint pos) const {
  register FXint nlines=syntax->getContextLines();
  register FXint nchars=syntax->getContextChars();
  register FXint result=pos;
  if(1<nlines){
    result=editor->prevLine(pos,nlines-1);
    }
  else if(nlines==1){
    result=editor->lineStart(pos);
    }
  if(pos-nchars<result) result=pos-nchars;
  if(result<0) result=0;
  return result;
  }


// Scan forward by context amount
FXint TextWindow::forwardByContext(FXint pos) const {
  register FXint nlines=syntax->getContextLines();
  register FXint nchars=syntax->getContextChars();
  register FXint result=pos;
  result=editor->nextLine(pos,nlines);
  if(pos+nchars>result) result=pos+nchars;
  if(result>editor->getLength()) result=editor->getLength();
  return result;
  }


// Find restyle point
FXint TextWindow::findRestylePoint(FXint pos,FXint& style) const {
  register FXint probepos,safepos,beforesafepos,runstyle,s;

  // Return 0 for style unless we found something else
  style=0;

  // Scan back by a certain amount of match context
  probepos=backwardByContext(pos);
  if(probepos==0) return 0;

  // Get style here
  runstyle=editor->getStyle(probepos);
  if(runstyle==0) return probepos;

  // Scan back one more context and one before that
  safepos=backwardByContext(probepos);
  beforesafepos=backwardByContext(safepos);

  // Scan back
  for(--probepos; 0<probepos; --probepos){

    // Same style; continue scanning backwards
    s=editor->getStyle(probepos);
    if(s==runstyle){

      // Went back pretty far, return running style
      if(probepos<=beforesafepos){
        style=runstyle;
        return safepos;
        }

      // Continue scanning backwards till we see a style change
      continue;
      }

    // At beginning of child-pattern, return parent style
    if(syntax->isAncestor(s,runstyle)){
      style=s;
      return probepos+1;
      }

    // Before end of child-pattern, return running style
    if(syntax->isAncestor(runstyle,s)){
      style=runstyle;
      return probepos+1;
      }

    // Sibling styles with common ancestor, return ancestor style
    if(syntax->getRule(s)->getParent()==syntax->getRule(runstyle)->getParent()){
      style=syntax->getRule(s)->getParent();
      return probepos+1;
      }

    // Unrelated styles, return with root style
    return probepos+1;
    }
  return 0;
  }



// Restyle range; returns affected style end, i.e. one beyond
// the last position where the style changed from the original style
FXint TextWindow::restyleRange(FXint beg,FXint end,FXint& head,FXint& tail,FXint rule){
  FXchar *text,*newstyle,*oldstyle;
  register FXint len=end-beg;
  register FXint delta=len;
  FXASSERT(0<=rule);
  FXASSERT(0<=beg && beg<=end && end<=editor->getLength());
  FXMALLOC(&text,FXchar,len+len+len);
  newstyle=text+len;
  oldstyle=text+len+len;
  editor->extractText(text,beg,len);
  editor->extractStyle(oldstyle,beg,len);
  syntax->getRule(rule)->stylizeBody(text,newstyle,0,len,head,tail);
  editor->changeStyle(beg,newstyle,len);
  while(0<delta && oldstyle[delta-1]==newstyle[delta-1]) --delta;
  FXFREE(&text);
  head+=beg;
  tail+=beg;
  delta+=beg;
  FXTRACE((1,"changed head=%d tail=%d same till delta=%d\n",head,tail,delta));
  return delta;
  }


// Restyle text after change in buffer [fm,to]
void TextWindow::restyleText(FXint pos,FXint del,FXint ins){
  FXint head,tail,changed,affected,beg,end,len,rule,restylejump;
  if(colorize && syntax){

    // Length of text
    len=editor->getLength();

    // End of buffer modification
    changed=pos+ins;

    // Scan back to a place where the style changed, return
    // the style rule in effect at that location
    beg=findRestylePoint(pos,rule);

    // Scan forward by one context
    end=forwardByContext(changed);

    FXTRACE((1,"restyleText: pos=%d del=%d ins=%d beg=%d end=%d rule=%d\n",pos,del,ins,beg,end,rule));

    FXASSERT(0<=rule);

    // Restyle until we fully enclose the style change
    restylejump=RESTYLEJUMP;
    while(1){

      // Restyle [beg,end> using rule, return matched range in [head,tail>
      affected=restyleRange(beg,end,head,tail,rule);

      // Not all colored yet, continue coloring with parent rule
      if(tail<end){
        beg=tail;
        if(rule==0){ fxwarning("Top level patterns did not color everything.\n"); return; }
        rule=syntax->getRule(rule)->getParent();
        continue;
        }

      // Style changed in unchanged text
      if(affected>changed){
        restylejump<<=1;
	changed=affected;
    	end=changed+restylejump;
    	if(end>len) end=len;
        continue;
        }

      // Everything was recolored and style didn't change anymore
      return;
      }
    }
  }


// Toggle syntax coloring
long TextWindow::onCmdSyntax(FXObject*,FXSelector,void* ptr){
  colorize=(FXbool)(FXuval)ptr;
  if(syntax && colorize){
    editor->setStyled(TRUE);
    restyleText();
    }
  else{
    editor->setStyled(FALSE);
    }
  return 1;
  }


// Update syntax coloring
long TextWindow::onUpdSyntax(FXObject* sender,FXSelector,void*){
  sender->handle(this,colorize?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Restyle text
long TextWindow::onCmdRestyle(FXObject*,FXSelector,void*){
  restyleText();
  return 1;
  }


// Update restyle text
long TextWindow::onUpdRestyle(FXObject* sender,FXSelector,void*){
  sender->handle(this,editor->isStyled()?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }



