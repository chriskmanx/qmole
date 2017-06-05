/********************************************************************************
*                                                                               *
*                            H e l p   W i n d o w                              *
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
* $Id: HelpWindow.cpp,v 1.10 2006/01/22 18:01:13 fox Exp $                      *
********************************************************************************/
#include "fx.h"
#include "HelpWindow.h"


/*******************************************************************************/


FXIMPLEMENT(HelpWindow,FXDialogBox,NULL,0)


// Construct help dialog box
HelpWindow::HelpWindow(FXWindow *owner,const FXString& title):
  FXDialogBox(owner,title,DECOR_TITLE|DECOR_BORDER|DECOR_RESIZE|DECOR_CLOSE,0,0,0,0, 6,6,6,6, 4,4){

  // Bottom part
  FXHorizontalFrame *closebox=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|PACK_UNIFORM_WIDTH);
  FXButton *button=new FXButton(closebox,tr("&Close"),NULL,this,FXDialogBox::ID_ACCEPT,BUTTON_INITIAL|BUTTON_DEFAULT|LAYOUT_RIGHT|FRAME_RAISED|FRAME_THICK,0,0,0,0, 20,20,5,5);

  // Text part
  FXHorizontalFrame *editbox=new FXHorizontalFrame(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_SUNKEN|FRAME_THICK,0,0,0,0, 0,0,0,0);
  helptext=new FXText(editbox,NULL,0,TEXT_READONLY|TEXT_WORDWRAP|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  helptext->setVisibleRows(50);
  helptext->setVisibleColumns(90);
  button->setFocus();
  }


// Set help text
void HelpWindow::setHelp(const FXString& help){
  helptext->setText(help);
  }


// Obtain help text
FXString HelpWindow::getHelp() const {
  return helptext->getText();
  }


// Clean up
HelpWindow::~HelpWindow(){
  helptext=(FXText*)-1;
  }
