/********************************************************************************
*                                                                               *
*                        P r e f e r e n c e s   D i a l o g                    *
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
* $Id: Preferences.cpp,v 1.68 2006/01/22 18:01:10 fox Exp $                     *
********************************************************************************/
#include "fx.h"
#include "icons.h"
#include "help.h"
#include <stdio.h>
#include <stdlib.h>
#include "FXRex.h"
#include "FXArray.h"
#include "Preferences.h"
#include "Hilite.h"
#include "TextWindow.h"
#include "Adie.h"
#include "icons.h"


/*******************************************************************************/


// Object implementation
FXIMPLEMENT(Preferences,FXDialogBox,NULL,0)



// Construct
Preferences::Preferences(TextWindow *owner):FXDialogBox(owner,"Adie Preferences",DECOR_TITLE|DECOR_BORDER|DECOR_RESIZE,0,0,0,0, 0,0,0,0, 4,4){

  // Set title
  setTitle(tr("Adie Preferences"));

  // Interior
  FXVerticalFrame *vertical=new FXVerticalFrame(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  FXHorizontalFrame *horizontal=new FXHorizontalFrame(vertical,LAYOUT_FILL_X|LAYOUT_FILL_Y);
  FXVerticalFrame *buttons=new FXVerticalFrame(horizontal,LAYOUT_LEFT|LAYOUT_FILL_Y|FRAME_SUNKEN|PACK_UNIFORM_WIDTH|PACK_UNIFORM_HEIGHT,0,0,0,0, 0,0,0,0, 0,0);
  FXSwitcher *switcher=new FXSwitcher(horizontal,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0);

  // Icons
  pal=new FXGIFIcon(getApp(),palette_gif);
  ind=new FXGIFIcon(getApp(),indent_gif);
  pat=new FXGIFIcon(getApp(),pattern_gif);
  sty=new FXGIFIcon(getApp(),styles_gif);

  /////////////////////////  Color settings pane  ///////////////////////////////
  FXVerticalFrame* colorspane=new FXVerticalFrame(switcher,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0, 0,0);
  new FXLabel(colorspane,tr("Color settings"),NULL,LAYOUT_LEFT);
  new FXHorizontalSeparator(colorspane,SEPARATOR_LINE|LAYOUT_FILL_X);
  FXMatrix *matrix1=new FXMatrix(colorspane,8,MATRIX_BY_ROWS|PACK_UNIFORM_HEIGHT|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 4,4,4,4, 4, 4);

  new FXLabel(matrix1,tr("Background:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix1,tr("Text:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix1,tr("Sel. text background:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix1,tr("Sel. text:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix1,tr("Hilite text background:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix1,tr("Hilite text:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix1,tr("Act. text background:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix1,tr("Numbers background:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);

  new FXColorWell(matrix1,FXRGB(0,0,0),owner,TextWindow::ID_TEXT_BACK,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,TextWindow::ID_TEXT_FORE,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,TextWindow::ID_TEXT_SELBACK,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,TextWindow::ID_TEXT_SELFORE,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,TextWindow::ID_TEXT_HILITEBACK,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,TextWindow::ID_TEXT_HILITEFORE,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,TextWindow::ID_TEXT_ACTIVEBACK,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,TextWindow::ID_TEXT_NUMBACK,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);

  new FXFrame(matrix1,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix1,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix1,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix1,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix1,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix1,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix1,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix1,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);

  new FXLabel(matrix1,tr("Files background:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix1,tr("Files:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix1,tr("Sel. files background:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix1,tr("Sel. files:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix1,tr("Lines:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix1,tr("Cursor:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix1,tr("Active background:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix1,tr("Numbers:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);

  new FXColorWell(matrix1,FXRGB(0,0,0),owner,TextWindow::ID_DIR_BACK,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,TextWindow::ID_DIR_FORE,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,TextWindow::ID_DIR_SELBACK,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,TextWindow::ID_DIR_SELFORE,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,TextWindow::ID_DIR_LINES,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,TextWindow::ID_TEXT_CURSOR,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);
  new FXCheckButton(matrix1,FXString::null,owner,TextWindow::ID_SHOWACTIVE,LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW,0,0,0,0, 5,5,0,0);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,TextWindow::ID_TEXT_NUMFORE,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);

  //// Color settings button
  new FXButton(buttons,tr("Colors\tChange Colors\tChange text colors."),pal,switcher,FXSwitcher::ID_OPEN_FIRST,FRAME_RAISED|ICON_ABOVE_TEXT|LAYOUT_FILL_Y);


  ///////////////////////////  Editor settings pane  ////////////////////////////
  FXVerticalFrame* editorpane=new FXVerticalFrame(switcher,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0, 0,0);
  new FXLabel(editorpane,tr("Editor settings"),NULL,LAYOUT_LEFT);
  new FXHorizontalSeparator(editorpane,SEPARATOR_LINE|LAYOUT_FILL_X);
  FXMatrix *matrix2=new FXMatrix(editorpane,8,MATRIX_BY_ROWS|PACK_UNIFORM_HEIGHT|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 4,4,4,4, 4, 2);

  new FXLabel(matrix2,tr("Word wrapping:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix2,tr("Auto indent:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix2,tr("Fixed wrap margin:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix2,tr("Strip carriage returns:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix2,tr("Strip trailing spaces:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix2,tr("Append newline at end of file:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix2,tr("Insert tab characters:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix2,tr("Warn if changed externally:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);

  new FXCheckButton(matrix2,FXString::null,owner,TextWindow::ID_TOGGLE_WRAP,LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW,0,0,0,0, 0,0,0,0);
  new FXCheckButton(matrix2,FXString::null,owner,TextWindow::ID_AUTOINDENT,LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW,0,0,0,0, 0,0,0,0);
  new FXCheckButton(matrix2,FXString::null,owner,TextWindow::ID_FIXED_WRAP,LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW,0,0,0,0, 0,0,0,0);
  new FXCheckButton(matrix2,FXString::null,owner,TextWindow::ID_STRIP_CR,LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW,0,0,0,0, 0,0,0,0);
  new FXCheckButton(matrix2,FXString::null,owner,TextWindow::ID_STRIP_SP,LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW,0,0,0,0, 0,0,0,0);
  new FXCheckButton(matrix2,FXString::null,owner,TextWindow::ID_APPEND_NL,LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW,0,0,0,0, 0,0,0,0);
  new FXCheckButton(matrix2,FXString::null,owner,TextWindow::ID_INSERTTABS,LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW,0,0,0,0, 0,0,0,0);
  new FXCheckButton(matrix2,FXString::null,owner,TextWindow::ID_WARNCHANGED,LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW,0,0,0,0, 0,0,0,0);

  new FXFrame(matrix2,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix2,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix2,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix2,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix2,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix2,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix2,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXFrame(matrix2,LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);

  new FXLabel(matrix2,tr("Wrap margin:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix2,tr("Tab columns:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix2,tr("Brace match time (ms):"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix2,tr("Mouse wheel lines:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix2,tr("Line number space:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix2,tr("Save view of file:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix2,tr("Save bookmarks:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXFrame(matrix2,LAYOUT_FILL_ROW);

  new FXTextField(matrix2,6,owner,TextWindow::ID_WRAPCOLUMNS,JUSTIFY_RIGHT|FRAME_SUNKEN|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW,0,0,0,0, 2,2,1,1);
  new FXTextField(matrix2,6,owner,TextWindow::ID_TABCOLUMNS,JUSTIFY_RIGHT|FRAME_SUNKEN|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW,0,0,0,0, 2,2,1,1);
  new FXTextField(matrix2,6,owner,TextWindow::ID_BRACEMATCH,JUSTIFY_RIGHT|FRAME_SUNKEN|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW,0,0,0,0, 2,2,1,1);
  FXSpinner* spinner=new FXSpinner(matrix2,3,owner,TextWindow::ID_WHEELADJUST,JUSTIFY_RIGHT|FRAME_SUNKEN|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW,0,0,0,0, 2,2,1,1);
  spinner->setRange(1,100);
  FXSpinner* spinner1=new FXSpinner(matrix2,3,owner,TextWindow::ID_TEXT_LINENUMS,JUSTIFY_RIGHT|FRAME_SUNKEN|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW,0,0,0,0, 2,2,1,1);
  spinner1->setRange(0,8);
  new FXCheckButton(matrix2,FXString::null,owner,TextWindow::ID_SAVEVIEWS,LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW,0,0,0,0, 0,0,0,0);
  new FXCheckButton(matrix2,FXString::null,owner,TextWindow::ID_SAVEMARKS,LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW,0,0,0,0, 0,0,0,0);
  FXHorizontalFrame *worddelims=new FXHorizontalFrame(editorpane,LAYOUT_FILL_Y|LAYOUT_FILL_X,0,0,0,0, 4,4,4,4, 4, 4);
  new FXLabel(worddelims,tr("Word delimiters:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y);
  new FXTextField(worddelims,10,owner,TextWindow::ID_DELIMITERS,FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X,0,0,0,0, 2,2,1,1);

  //// Editor settings button
  new FXButton(buttons,tr("Editor\tEditor settings\tChange editor settings and other things."),ind,switcher,FXSwitcher::ID_OPEN_SECOND,FRAME_RAISED|ICON_ABOVE_TEXT|LAYOUT_FILL_Y);


  ///////////////////////  File pattern settings pane  //////////////////////////
  FXVerticalFrame* filepatpane=new FXVerticalFrame(switcher,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0, 0,0);
  new FXLabel(filepatpane,tr("Filename patterns"),NULL,LAYOUT_LEFT);
  new FXHorizontalSeparator(filepatpane,SEPARATOR_LINE|LAYOUT_FILL_X);
  FXVerticalFrame *sub3=new FXVerticalFrame(filepatpane,LAYOUT_FILL_Y|LAYOUT_FILL_X,0,0,0,0, 0,0,10,0, 0,0);
  new FXLabel(sub3,tr("Filename patterns, one per line:"),NULL,JUSTIFY_LEFT);
  FXVerticalFrame* textwell=new FXVerticalFrame(sub3,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_SUNKEN|FRAME_THICK,0,0,0,0, 0,0,0,0);
  filepattext=new FXText(textwell,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y);

  //// File pattern settings button
  new FXButton(buttons,tr("Patterns\tFilename patterns\tChange wildcard patterns for filenames."),pat,switcher,FXSwitcher::ID_OPEN_THIRD,FRAME_RAISED|ICON_ABOVE_TEXT|LAYOUT_FILL_Y);


  /////////////////////  Highlight style settings pane  /////////////////////////
  FXVerticalFrame* highlightpane=new FXVerticalFrame(switcher,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0, 0,0);
  new FXLabel(highlightpane,tr("Highlight styles"),NULL,LAYOUT_LEFT);
  new FXHorizontalSeparator(highlightpane,SEPARATOR_LINE|LAYOUT_FILL_X);
  FXHorizontalFrame *sub5=new FXHorizontalFrame(highlightpane,LAYOUT_FILL_Y|LAYOUT_FILL_X,0,0,0,0, 0,0,10,0);

  FXGroupBox* colorgroup=new FXGroupBox(sub5,tr("Style of item"),FRAME_GROOVE|LAYOUT_RIGHT|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 4,4,4,4, 4,4);
  FXMatrix *colormatrix=new FXMatrix(colorgroup,3,MATRIX_BY_COLUMNS|PACK_UNIFORM_HEIGHT|LAYOUT_SIDE_TOP|LAYOUT_FILL_X,0,0,0,0, 0,0,0,0, 4, 4);
  new FXLabel(colormatrix,tr("Normal text color fg/bg:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW|LAYOUT_FILL_COLUMN);
  new FXColorWell(colormatrix,FXRGB(0,0,0),owner,TextWindow::ID_STYLE_NORMAL_FG,FRAME_SUNKEN|FRAME_THICK|LAYOUT_CENTER_X|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);
  new FXColorWell(colormatrix,FXRGB(0,0,0),owner,TextWindow::ID_STYLE_NORMAL_BG,FRAME_SUNKEN|FRAME_THICK|LAYOUT_CENTER_X|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);
  new FXLabel(colormatrix,tr("Selected text color fg/bg:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW|LAYOUT_FILL_COLUMN);
  new FXColorWell(colormatrix,FXRGB(0,0,0),owner,TextWindow::ID_STYLE_SELECT_FG,FRAME_SUNKEN|FRAME_THICK|LAYOUT_CENTER_X|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);
  new FXColorWell(colormatrix,FXRGB(0,0,0),owner,TextWindow::ID_STYLE_SELECT_BG,FRAME_SUNKEN|FRAME_THICK|LAYOUT_CENTER_X|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);
  new FXLabel(colormatrix,tr("Highlighted text color fg/bg:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW|LAYOUT_FILL_COLUMN);
  new FXColorWell(colormatrix,FXRGB(0,0,0),owner,TextWindow::ID_STYLE_HILITE_FG,FRAME_SUNKEN|FRAME_THICK|LAYOUT_CENTER_X|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);
  new FXColorWell(colormatrix,FXRGB(0,0,0),owner,TextWindow::ID_STYLE_HILITE_BG,FRAME_SUNKEN|FRAME_THICK|LAYOUT_CENTER_X|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);
  new FXLabel(colormatrix,tr("Active line background color:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW|LAYOUT_FILL_COLUMN);
  new FXColorWell(colormatrix,FXRGB(0,0,0),owner,TextWindow::ID_STYLE_ACTIVE_BG,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);
  new FXFrame(colormatrix,LAYOUT_FILL_ROW);

  new FXLabel(colormatrix,tr("Underline text:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW|LAYOUT_FILL_COLUMN);
  new FXCheckButton(colormatrix,FXString::null,owner,TextWindow::ID_STYLE_UNDERLINE,LAYOUT_CENTER_X|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW,0,0,0,0, 0,0,0,0);
  new FXFrame(colormatrix,LAYOUT_FILL_ROW);
  new FXLabel(colormatrix,tr("Strikeout text:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW|LAYOUT_FILL_COLUMN);
  new FXCheckButton(colormatrix,FXString::null,owner,TextWindow::ID_STYLE_STRIKEOUT,LAYOUT_CENTER_X|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW,0,0,0,0, 0,0,0,0);
  new FXFrame(colormatrix,LAYOUT_FILL_ROW);
  new FXLabel(colormatrix,tr("Bold:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW|LAYOUT_FILL_COLUMN);
  new FXCheckButton(colormatrix,FXString::null,owner,TextWindow::ID_STYLE_BOLD,LAYOUT_CENTER_X|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW,0,0,0,0, 0,0,0,0);
  new FXFrame(colormatrix,LAYOUT_FILL_ROW);
//   new FXLabel(colormatrix,"Italic:",NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW|LAYOUT_FILL_COLUMN);
//   new FXCheckButton(colormatrix,NULL,NULL,0,LAYOUT_CENTER_X|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW,0,0,0,0, 0,0,0,0);
//   new FXFrame(colormatrix,LAYOUT_FILL_ROW);

  FXGroupBox* stylegroup=new FXGroupBox(sub5,tr("Item name"),FRAME_GROOVE|LAYOUT_FILL_Y,0,0,0,0, 4,4,4,4, 4,4);
  FXVerticalFrame* listframe=new FXVerticalFrame(stylegroup,FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0);
  stylelist=new FXList(listframe,owner,TextWindow::ID_STYLE_INDEX,LIST_BROWSESELECT|HSCROLLER_NEVER|LAYOUT_FIX_WIDTH|LAYOUT_FILL_Y, 0,0,110,0);

  //// Highlight style settings button
  new FXButton(buttons,tr("Styles\tHighlight styles\tChange highlight styles for syntax coloring."),sty,switcher,FXSwitcher::ID_OPEN_FOURTH,FRAME_RAISED|ICON_ABOVE_TEXT|LAYOUT_FILL_Y);


  // Bottom part
  new FXHorizontalSeparator(vertical,SEPARATOR_RIDGE|LAYOUT_FILL_X);
  FXHorizontalFrame *closebox=new FXHorizontalFrame(vertical,LAYOUT_BOTTOM|LAYOUT_FILL_X|PACK_UNIFORM_WIDTH);
  new FXButton(closebox,tr("&Accept"),NULL,this,FXDialogBox::ID_ACCEPT,BUTTON_INITIAL|BUTTON_DEFAULT|LAYOUT_RIGHT|FRAME_RAISED|FRAME_THICK,0,0,0,0, 20,20);
  new FXButton(closebox,tr("&Cancel"),NULL,this,FXDialogBox::ID_CANCEL,BUTTON_DEFAULT|LAYOUT_RIGHT|FRAME_RAISED|FRAME_THICK,0,0,0,0, 20,20);
  }



/*******************************************************************************/


// Change patterns, each pattern separated by newline
void Preferences::setPatterns(const FXString& patterns){
  filepattext->setText(patterns);
  }


// Return list of patterns
FXString Preferences::getPatterns() const {
  return filepattext->getText();
  }


// Set language syntax
void Preferences::setSyntax(FXSyntax* syn){
  stylelist->clearItems(TRUE);
  if(syn){
    for(FXint i=1; i<syn->getNumRules(); i++){
      stylelist->appendItem(syn->getRule(i)->getName(),NULL,NULL,TRUE);
      }
    }
  }


// Clean up
Preferences::~Preferences(){
  delete pal;
  delete ind;
  delete pat;
  delete sty;
  }
