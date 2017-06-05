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
* $Id: Preferences.cpp,v 1.17 2006/01/22 18:01:13 fox Exp $                     *
********************************************************************************/
#include "fx.h"
#include "icons.h"
#include "Preferences.h"
#include "Calculator.h"


/*******************************************************************************/


FXIMPLEMENT(Preferences,FXDialogBox,NULL,0)


// Construct
Preferences::Preferences(Calculator *owner):
  FXDialogBox(owner,"Calculator Preferences",DECOR_TITLE|DECOR_BORDER|DECOR_RESIZE,0,0,0,0, 10,10,10,10, 4,4){

  setTitle(tr("Calculator Preferences"));

  // Icons
  palette=new FXGIFIcon(getApp(),colors);
  calculator=new FXGIFIcon(getApp(),bigcalc);
  info=new FXGIFIcon(getApp(),information,0,IMAGE_ALPHAGUESS);

  // Bottom part
  FXHorizontalFrame *closebox=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|PACK_UNIFORM_WIDTH);
  FXButton* close=new FXButton(closebox,tr("&Close"),NULL,this,FXDialogBox::ID_ACCEPT,BUTTON_INITIAL|BUTTON_DEFAULT|LAYOUT_RIGHT|FRAME_RAISED|FRAME_THICK,0,0,0,0, 20,20);


  new FXHorizontalSeparator(this,LAYOUT_SIDE_BOTTOM|SEPARATOR_RIDGE|LAYOUT_FILL_X);

  FXHorizontalFrame *horizontal=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0);
  FXVerticalFrame *buttons=new FXVerticalFrame(horizontal,LAYOUT_LEFT|LAYOUT_FILL_Y|FRAME_SUNKEN|PACK_UNIFORM_WIDTH|PACK_UNIFORM_HEIGHT,0,0,0,0, 0,0,0,0, 0,0);
  FXSwitcher *switcher=new FXSwitcher(horizontal,LAYOUT_FILL_X|LAYOUT_FILL_Y);

  // Pane 2
  FXVerticalFrame* pane2=new FXVerticalFrame(switcher,LAYOUT_FILL_X|LAYOUT_FILL_Y);
  new FXLabel(pane2,tr("Calculator settings"),NULL,LAYOUT_LEFT);
  new FXHorizontalSeparator(pane2,SEPARATOR_LINE|LAYOUT_FILL_X);
  FXMatrix *matrix2=new FXMatrix(pane2,5,MATRIX_BY_ROWS|PACK_UNIFORM_HEIGHT|LAYOUT_FILL_X|LAYOUT_FILL_Y);

  new FXLabel(matrix2,tr("Display Font:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix2,tr("Always show exponent:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix2,tr("Never show exponent:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix2,tr("Precision:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXLabel(matrix2,tr("Beep on error:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);

  new FXButton(matrix2,tr("Set..."),NULL,owner,Calculator::ID_FONT,FRAME_RAISED|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  new FXCheckButton(matrix2,FXString::null,owner,Calculator::ID_EXPONENT_ALWAYS,LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW,0,0,0,0, 0,0,0,0);
  new FXCheckButton(matrix2,FXString::null,owner,Calculator::ID_EXPONENT_NEVER,LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW,0,0,0,0, 0,0,0,0);
  FXSpinner* spinner=new FXSpinner(matrix2,2,owner,Calculator::ID_PRECISION,JUSTIFY_RIGHT|FRAME_SUNKEN|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  spinner->setRange(1,30);
  new FXCheckButton(matrix2,FXString::null,owner,Calculator::ID_BEEP,LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW,0,0,0,0, 0,0,0,0);

  // Button 2
  new FXButton(buttons,tr("Calculator\tCalculator settings\tChange calculator settings."),calculator,switcher,FXSwitcher::ID_OPEN_FIRST,FRAME_RAISED|ICON_ABOVE_TEXT|LAYOUT_FILL_Y);

  // Pane 1
  FXVerticalFrame* pane1=new FXVerticalFrame(switcher,LAYOUT_FILL_X|LAYOUT_FILL_Y);
  new FXLabel(pane1,tr("Color settings"),NULL,LAYOUT_LEFT);
  new FXHorizontalSeparator(pane1,SEPARATOR_LINE|LAYOUT_FILL_X);
  FXMatrix *matrix1=new FXMatrix(pane1,6,MATRIX_BY_COLUMNS|PACK_UNIFORM_HEIGHT|PACK_UNIFORM_WIDTH|LAYOUT_FILL_X|LAYOUT_FILL_Y);

  new FXLabel(matrix1,tr("Display:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,Calculator::ID_COLOR_DISPLAY,COLORWELL_OPAQUEONLY|FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW,0,0,40,24);

  new FXLabel(matrix1,tr("Display Number:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,Calculator::ID_COLOR_DISPLAYNUMBER,COLORWELL_OPAQUEONLY|FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW,0,0,40,24);

  new FXLabel(matrix1,tr("Digits:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,Calculator::ID_COLOR_DIGITS,COLORWELL_OPAQUEONLY|FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW,0,0,40,24);

  new FXLabel(matrix1,tr("Hex Digits:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,Calculator::ID_COLOR_HEXDIGITS,COLORWELL_OPAQUEONLY|FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW,0,0,40,24);

  new FXLabel(matrix1,tr("Operators:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,Calculator::ID_COLOR_OPERATORS,COLORWELL_OPAQUEONLY|FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW,0,0,40,24);

  new FXLabel(matrix1,tr("Functions:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,Calculator::ID_COLOR_FUNCTIONS,COLORWELL_OPAQUEONLY|FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW,0,0,40,24);

  new FXLabel(matrix1,tr("Memory:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,Calculator::ID_COLOR_MEMORY,COLORWELL_OPAQUEONLY|FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW,0,0,40,24);

  new FXLabel(matrix1,tr("Number Base:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,Calculator::ID_COLOR_BASE,COLORWELL_OPAQUEONLY|FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW,0,0,40,24);

  new FXLabel(matrix1,tr("Angle Mode:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,Calculator::ID_COLOR_ANGLES,COLORWELL_OPAQUEONLY|FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW,0,0,40,24);

  new FXLabel(matrix1,tr("Invert:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,Calculator::ID_COLOR_INVERT,COLORWELL_OPAQUEONLY|FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW,0,0,40,24);

  new FXLabel(matrix1,tr("Hyper:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,Calculator::ID_COLOR_HYPER,COLORWELL_OPAQUEONLY|FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW,0,0,40,24);

  new FXLabel(matrix1,tr("Clear All:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,Calculator::ID_COLOR_CLEARALL,COLORWELL_OPAQUEONLY|FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW,0,0,40,24);

  new FXLabel(matrix1,tr("Clear:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW);
  new FXColorWell(matrix1,FXRGB(0,0,0),owner,Calculator::ID_COLOR_CLEAR,COLORWELL_OPAQUEONLY|FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW,0,0,40,24);

  // Button 1
  new FXButton(buttons,tr("Colors\tChange Colors\tChange text colors."),palette,switcher,FXSwitcher::ID_OPEN_SECOND,FRAME_RAISED|ICON_ABOVE_TEXT|LAYOUT_FILL_Y);

  // Pane 3
  FXVerticalFrame* pane3=new FXVerticalFrame(switcher,LAYOUT_FILL_X|LAYOUT_FILL_Y);
  new FXLabel(pane3,tr("About FOX Calculator"),NULL,LAYOUT_LEFT);
  new FXHorizontalSeparator(pane3,SEPARATOR_LINE|LAYOUT_FILL_X);
  FXHorizontalFrame *sub3=new FXHorizontalFrame(pane3,LAYOUT_FILL_Y|LAYOUT_FILL_X);
  new FXLabel(sub3,FXString::null,calculator,LAYOUT_CENTER_Y,0,0,0,0,20,20,20,20);
  new FXLabel(sub3,FXStringFormat(tr("The FOX Calculator\n\nA Programmer's Desktop Calculator version 2.1.0.\nFOX library version %d.%d.%d.\nHome Page: http://www.fox-toolkit.org\nFTP Site: ftp://ftp.fox-toolkit.org\n\nCopyright (C) 2001,2005 Jeroen van der Zijp."),FOX_MAJOR,FOX_MINOR,FOX_LEVEL),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y);

  // Button 3
  new FXButton(buttons,tr("About\tAbout FOX Calculator\tAbout the FOX Calculator."),info,switcher,FXSwitcher::ID_OPEN_THIRD,FRAME_RAISED|ICON_ABOVE_TEXT|LAYOUT_FILL_Y);

  // Focus on close button
  close->setFocus();
  }


// Clean up
Preferences::~Preferences(){
  delete palette;
  delete calculator;
  palette=(FXIcon*)-1;
  calculator=(FXIcon*)-1;
  }
