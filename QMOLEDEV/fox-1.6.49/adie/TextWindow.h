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
* $Id: TextWindow.h,v 1.38 2006/02/06 03:03:40 fox Exp $                        *
********************************************************************************/
#ifndef TEXTWINDOW_H
#define TEXTWINDOW_H



class HelpWindow;
class Preferences;
class Adie;
class FXSyntax;

// Array of styles
typedef FXArray<FXHiliteStyle> FXHiliteArray;


// Editor main window
class TextWindow : public FXMainWindow {
  FXDECLARE(TextWindow)
protected:
  FXToolBarShell      *dragshell1;              // Shell for floating menubar
  FXToolBarShell      *dragshell2;              // Shell for floating toolbar
  FXDockSite          *topdock;                 // Topmost dock area
  FXDockSite          *bottomdock;              // Bottom dock area
  FXDockSite          *leftdock;                // Left dock area
  FXDockSite          *rightdock;               // Bottom dock area
  FXMenuPane          *filemenu;                // File menu
  FXMenuPane          *editmenu;                // Edit menu
  FXMenuPane          *gotomenu;                // Goto menu
  FXMenuPane          *searchmenu;              // Search menu
  FXMenuPane          *optionmenu;              // Option menu
  FXMenuPane          *viewmenu;                // View menu
  FXMenuPane          *windowmenu;              // Window menu
  FXMenuPane          *helpmenu;                // Help menu
  FXMenuPane          *popupmenu;               // Popup menu
  FXMenuPane          *syntaxmenu;              // Syntax menu
  FXVerticalFrame     *treebox;                 // Tree box containing directories/files
  FXHorizontalFrame   *undoredoblock;           // Undo/redo block on status line
  FXText              *editor;                  // Multiline text widget
  FXDirList           *dirlist;                 // Directory view
  FXComboBox          *filter;                  // Combobox for pattern list
  FXTextField         *clock;                   // Time
  FXMenuBar           *menubar;                 // Menu bar
  FXToolBar           *toolbar;                 // Tool bar
  FXStatusBar         *statusbar;               // Status bar
  FXFont              *font;                    // Text window font
  FXint                bookmark[10];            // Book marks
  FXSyntax            *syntax;                  // Syntax highlighter
  FXUndoList           undolist;                // Undo list
  FXRecentFiles        mrufiles;                // Recent files list
  FXString             filename;                // File being edited
  FXTime               filetime;                // Original modtime of file
  FXbool               filenameset;             // Filename is set
  FXString             delimiters;              // Text delimiters
  FXString             searchpath;              // To search for files
  FXHiliteArray        styles;                  // Highlight styles
  FXint                currentstyle;            // Style being changed
  FXbool               colorize;                // Syntax coloring on if possible
  FXbool               stripcr;                 // Strip carriage returns
  FXbool               stripsp;                 // Strip trailing spaces
  FXbool               appendnl;                // Append missing newline at end of text
  FXbool               saveviews;               // Save views of files
  FXbool               savemarks;               // Save bookmarks of files
  FXbool               warnchanged;             // Warn if changed by other program
protected:
  void readRegistry();
  void writeRegistry();
  FXString unique() const;
  TextWindow *findUnused() const;
  TextWindow *findWindow(const FXString& file) const;
  FXint findRestylePoint(FXint pos,FXint& style) const;
  FXint backwardByContext(FXint pos) const;
  FXint forwardByContext(FXint pos) const;
  void restyleText();
  void restyleText(FXint pos,FXint del,FXint ins);
  FXint restyleRange(FXint beg,FXint end,FXint& head,FXint& tail,FXint rule);
  FXHiliteStyle readStyleForRule(const FXString& name);
  void writeStyleForRule(const FXString& name,const FXHiliteStyle& style);
protected:
  enum{
    MAXUNDOSIZE    = 1000000,               // Don't let the undo buffer get out of hand
    KEEPUNDOSIZE   = 500000                 // When MAXUNDOSIZE was exceeded, trim down to this size
    };
private:
  TextWindow(){}
  TextWindow(const TextWindow&);
  TextWindow& operator=(const TextWindow&);
public:
  long onUpdate(FXObject*,FXSelector,void*);
  long onFocusIn(FXObject*,FXSelector,void*);
  long onCmdAbout(FXObject*,FXSelector,void*);
  long onCmdHelp(FXObject*,FXSelector,void*);

  // File management
  long onCmdNew(FXObject*,FXSelector,void*);
  long onCmdOpen(FXObject*,FXSelector,void*);
  long onCmdOpenRecent(FXObject*,FXSelector,void*);
  long onCmdOpenTree(FXObject*,FXSelector,void*);
  long onCmdOpenSelected(FXObject*,FXSelector,void*);
  long onCmdReopen(FXObject*,FXSelector,void*);
  long onUpdReopen(FXObject*,FXSelector,void*);
  long onCmdSave(FXObject*,FXSelector,void*);
  long onUpdSave(FXObject*,FXSelector,void*);
  long onCmdSaveAs(FXObject*,FXSelector,void*);
  long onCmdFont(FXObject*,FXSelector,void*);
  long onCmdPrint(FXObject*,FXSelector,void*);
  long onCmdSaveSettings(FXObject*,FXSelector,void*);

  // Text display
  long onCmdTextBackColor(FXObject*,FXSelector,void*);
  long onUpdTextBackColor(FXObject*,FXSelector,void*);
  long onCmdTextForeColor(FXObject*,FXSelector,void*);
  long onUpdTextForeColor(FXObject*,FXSelector,void*);
  long onCmdTextSelBackColor(FXObject*,FXSelector,void*);
  long onUpdTextSelBackColor(FXObject*,FXSelector,void*);
  long onCmdTextSelForeColor(FXObject*,FXSelector,void*);
  long onUpdTextSelForeColor(FXObject*,FXSelector,void*);
  long onCmdTextHiliteBackColor(FXObject*,FXSelector,void*);
  long onUpdTextHiliteBackColor(FXObject*,FXSelector,void*);
  long onCmdTextHiliteForeColor(FXObject*,FXSelector,void*);
  long onUpdTextHiliteForeColor(FXObject*,FXSelector,void*);
  long onCmdTextCursorColor(FXObject*,FXSelector,void*);
  long onUpdTextCursorColor(FXObject*,FXSelector,void*);
  long onCmdTextActBackColor(FXObject*,FXSelector,void*);
  long onUpdTextActBackColor(FXObject*,FXSelector,void*);
  long onCmdTextBarColor(FXObject*,FXSelector,void*);
  long onUpdTextBarColor(FXObject*,FXSelector,void*);
  long onCmdTextNumberColor(FXObject*,FXSelector,void*);
  long onUpdTextNumberColor(FXObject*,FXSelector,void*);
  long onCmdDirBackColor(FXObject*,FXSelector,void*);
  long onUpdDirBackColor(FXObject*,FXSelector,void*);
  long onCmdDirForeColor(FXObject*,FXSelector,void*);
  long onUpdDirForeColor(FXObject*,FXSelector,void*);
  long onCmdDirSelBackColor(FXObject*,FXSelector,void*);
  long onUpdDirSelBackColor(FXObject*,FXSelector,void*);
  long onCmdDirSelForeColor(FXObject*,FXSelector,void*);
  long onUpdDirSelForeColor(FXObject*,FXSelector,void*);
  long onCmdDirLineColor(FXObject*,FXSelector,void*);
  long onUpdDirLineColor(FXObject*,FXSelector,void*);
  long onCmdShowActive(FXObject*,FXSelector,void*);
  long onUpdShowActive(FXObject*,FXSelector,void*);
  long onCmdLineNumbers(FXObject*,FXSelector,void*);
  long onUpdLineNumbers(FXObject*,FXSelector,void*);
  long onCmdTabColumns(FXObject*,FXSelector,void*);
  long onUpdTabColumns(FXObject*,FXSelector,void*);
  long onCmdBraceMatch(FXObject*,FXSelector,void*);
  long onUpdBraceMatch(FXObject*,FXSelector,void*);
  long onCmdAutoIndent(FXObject*,FXSelector,void*);
  long onUpdAutoIndent(FXObject*,FXSelector,void*);
  long onCmdInsertTabs(FXObject*,FXSelector,void*);
  long onUpdInsertTabs(FXObject*,FXSelector,void*);
  long onCmdWrapColumns(FXObject*,FXSelector,void*);
  long onUpdWrapColumns(FXObject*,FXSelector,void*);
  long onCmdWrapFixed(FXObject*,FXSelector,void*);
  long onUpdWrapFixed(FXObject*,FXSelector,void*);
  long onCmdWrap(FXObject*,FXSelector,void*);
  long onUpdWrap(FXObject*,FXSelector,void*);

  // Text changes
  long onTextInserted(FXObject*,FXSelector,void*);
  long onTextReplaced(FXObject*,FXSelector,void*);
  long onTextDeleted(FXObject*,FXSelector,void*);
  long onTextRightMouse(FXObject*,FXSelector,void*);
  long onTextChanged(FXObject*,FXSelector,void*);
  long onEditDNDMotion(FXObject*,FXSelector,void*);
  long onEditDNDDrop(FXObject*,FXSelector,void*);

  // Miscellaneous
  long onCmdStripReturns(FXObject*,FXSelector,void*);
  long onUpdStripReturns(FXObject*,FXSelector,void*);
  long onCmdStripSpaces(FXObject*,FXSelector,void*);
  long onUpdStripSpaces(FXObject*,FXSelector,void*);
  long onCmdAppendNewline(FXObject*,FXSelector,void*);
  long onUpdAppendNewline(FXObject*,FXSelector,void*);
  long onCmdIncludePaths(FXObject*,FXSelector,void*);
  long onCmdFilter(FXObject*,FXSelector,void*);
  long onUpdOverstrike(FXObject*,FXSelector,void*);
  long onUpdReadOnly(FXObject*,FXSelector,void*);
  long onUpdNumRows(FXObject*,FXSelector,void*);
  long onClock(FXObject*,FXSelector,void*);
  long onCmdPreferences(FXObject*,FXSelector,void*);
  long onCmdDelimiters(FXObject*,FXSelector,void*);
  long onUpdDelimiters(FXObject*,FXSelector,void*);
  long onCmdInsertFile(FXObject*,FXSelector,void*);
  long onUpdInsertFile(FXObject*,FXSelector,void*);
  long onCmdExtractFile(FXObject*,FXSelector,void*);
  long onUpdExtractFile(FXObject*,FXSelector,void*);
  long onCmdWheelAdjust(FXObject*,FXSelector,void*);
  long onUpdWheelAdjust(FXObject*,FXSelector,void*);
  long onCmdNextMark(FXObject*,FXSelector,void*);
  long onUpdNextMark(FXObject*,FXSelector,void*);
  long onCmdPrevMark(FXObject*,FXSelector,void*);
  long onUpdPrevMark(FXObject*,FXSelector,void*);
  long onCmdSetMark(FXObject*,FXSelector,void*);
  long onUpdSetMark(FXObject*,FXSelector,void*);
  long onCmdClearMarks(FXObject*,FXSelector,void*);
  long onCmdSaveMarks(FXObject*,FXSelector,void*);
  long onUpdSaveMarks(FXObject*,FXSelector,void*);
  long onCmdSaveViews(FXObject*,FXSelector,void*);
  long onUpdSaveViews(FXObject*,FXSelector,void*);
  long onCmdWarnChanged(FXObject*,FXSelector,void*);
  long onUpdWarnChanged(FXObject*,FXSelector,void*);
  long onCmdSyntax(FXObject*,FXSelector,void*);
  long onUpdSyntax(FXObject*,FXSelector,void*);
  long onCmdRestyle(FXObject*,FXSelector,void*);
  long onUpdRestyle(FXObject*,FXSelector,void*);
  long onCmdJumpScroll(FXObject*,FXSelector,void*);
  long onUpdJumpScroll(FXObject*,FXSelector,void*);
  long onCmdWindow(FXObject*,FXSelector,void*);
  long onUpdWindow(FXObject*,FXSelector,void*);
  long onCmdSyntaxSwitch(FXObject*,FXSelector,void*);
  long onUpdSyntaxSwitch(FXObject*,FXSelector,void*);
  long onCmdStyleFlags(FXObject*,FXSelector,void*);
  long onUpdStyleFlags(FXObject*,FXSelector,void*);
  long onCmdStyleColor(FXObject*,FXSelector,void*);
  long onUpdStyleColor(FXObject*,FXSelector,void*);
  long onCmdStyleIndex(FXObject*,FXSelector,void*);
  long onUpdStyleIndex(FXObject*,FXSelector,void*);
public:
  enum{
    ID_ABOUT=FXMainWindow::ID_LAST,
    ID_FILEFILTER,
    ID_NEW,
    ID_OPEN,
    ID_OPEN_TREE,
    ID_OPEN_SELECTED,
    ID_OPEN_RECENT,
    ID_REOPEN,
    ID_SAVE,
    ID_SAVEAS,
    ID_FONT,
    ID_HELP,
    ID_WINDOW,
    ID_PRINT,
    ID_TEXT_BACK,
    ID_TEXT_FORE,
    ID_TEXT_SELBACK,
    ID_TEXT_SELFORE,
    ID_TEXT_HILITEBACK,
    ID_TEXT_HILITEFORE,
    ID_TEXT_ACTIVEBACK,
    ID_TEXT_CURSOR,
    ID_TEXT_NUMBACK,
    ID_TEXT_NUMFORE,
    ID_TEXT_LINENUMS,
    ID_DIR_BACK,
    ID_DIR_FORE,
    ID_DIR_SELBACK,
    ID_DIR_SELFORE,
    ID_DIR_LINES,
    ID_TOGGLE_WRAP,
    ID_FIXED_WRAP,
    ID_SAVE_SETTINGS,
    ID_TEXT,
    ID_STRIP_CR,
    ID_STRIP_SP,
    ID_APPEND_NL,
    ID_INCLUDE_PATH,
    ID_OVERSTRIKE,
    ID_READONLY,
    ID_CLOCKTIME,
    ID_PREFERENCES,
    ID_TABCOLUMNS,
    ID_WRAPCOLUMNS,
    ID_DELIMITERS,
    ID_INSERTTABS,
    ID_AUTOINDENT,
    ID_BRACEMATCH,
    ID_NUM_ROWS,
    ID_INSERT_FILE,
    ID_EXTRACT_FILE,
    ID_WHEELADJUST,
    ID_SET_MARK,
    ID_NEXT_MARK,
    ID_PREV_MARK,
    ID_CLEAR_MARKS,
    ID_SAVEMARKS,
    ID_SAVEVIEWS,
    ID_SHOWACTIVE,
    ID_WARNCHANGED,
    ID_SYNTAX,
    ID_RESTYLE,
    ID_JUMPSCROLL,
    ID_WINDOW_1,
    ID_WINDOW_2,
    ID_WINDOW_3,
    ID_WINDOW_4,
    ID_WINDOW_5,
    ID_WINDOW_6,
    ID_WINDOW_7,
    ID_WINDOW_8,
    ID_WINDOW_9,
    ID_WINDOW_10,
    ID_SYNTAX_FIRST,
    ID_SYNTAX_LAST=ID_SYNTAX_FIRST+100,
    ID_STYLE_INDEX,
    ID_STYLE_NORMAL_FG,
    ID_STYLE_NORMAL_BG,
    ID_STYLE_SELECT_FG,
    ID_STYLE_SELECT_BG,
    ID_STYLE_HILITE_FG,
    ID_STYLE_HILITE_BG,
    ID_STYLE_ACTIVE_BG,
    ID_STYLE_UNDERLINE,
    ID_STYLE_STRIKEOUT,
    ID_STYLE_BOLD,
    ID_LAST
    };
public:

  // Create new text window
  TextWindow(Adie* a,const FXString& file);

  // Create window
  virtual void create();

  // Detach window
  virtual void detach();

  // Close the window, return TRUE if actually closed
  virtual FXbool close(FXbool notify=FALSE);

  // Return Adie application
  Adie* getApp() const { return (Adie*)FXMainWindow::getApp(); }

  // Return this window's filename
  const FXString& getFilename() const { return filename; }

  // Change this window's filename
  void setFilename(const FXString& file){ filename=file; }

  // Has a filename been set or is it a new window
  FXbool isFilenameSet() const { return filenameset; }

  // Is it modified
  FXbool isModified() const;

  // Set editable flag
  void setEditable(FXbool edit=TRUE);

  // Is it editable
  FXbool isEditable() const;

  // Load text from file
  FXbool loadFile(const FXString& file);

  // Save text to file
  FXbool saveFile(const FXString& file);

  // Insert file at cursor
  FXbool insertFile(const FXString& file);

  // Extract selection to file
  FXbool extractFile(const FXString& file);

  // Return TRUE if changes have been saved
  FXbool saveChanges();

  // Change pattern list
  void setPatterns(const FXString& patterns);

  // Get pattern list
  FXString getPatterns() const;

  // Change current file pattern
  void setCurrentPattern(FXint n);

  // Return current file pattern
  FXint getCurrentPattern() const;

  // Add bookmark at current cursor position
  void setBookmark(FXint pos);

  // Update bookmarks upon a text mutation
  void updateBookmarks(FXint pos,FXint nd,FXint ni);

  // Goto bookmark
  void gotoBookmark(FXint b);

  // Clear bookmarks
  void clearBookmarks();

  // Read/write bookmarks
  void readBookmarks(const FXString& file);
  void writeBookmarks(const FXString& file);

  // Visit given line
  void visitLine(FXint line);

  // Read/write view
  void readView(const FXString& file);
  void writeView(const FXString& file);

  // Set syntax
  void setSyntax(FXSyntax* syn);

  // Get syntax
  FXSyntax* getSyntax() const { return syntax; }

  // Determine syntax
  void determineSyntax();

  // Set syntax by name
  FXbool forceSyntax(const FXString& language);

  // Change style colors
  void setStyleColors(FXint index,const FXHiliteStyle& style);

  // Get style colors
  const FXHiliteStyle& getStyleColors(FXint index) const { return styles[index]; }

  // Redraw when style changed
  void redraw();

  // Delete text window
  virtual ~TextWindow();
  };


typedef FXObjectListOf<TextWindow> TextWindowList;

#endif

