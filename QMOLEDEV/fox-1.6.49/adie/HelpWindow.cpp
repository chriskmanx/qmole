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
* $Id: HelpWindow.cpp,v 1.12 2006/01/22 18:01:10 fox Exp $                      *
********************************************************************************/
#include "fx.h"
#include "icons.h"
#include "help.h"
#include <stdio.h>
#include <stdlib.h>
#include "Preferences.h"
#include "Commands.h"
#include "FXRex.h"
#include "FXArray.h"
#include "Hilite.h"
#include "TextWindow.h"
#include "Adie.h"
#include "HelpWindow.h"


/*******************************************************************************/


FXIMPLEMENT(HelpWindow,FXDialogBox,NULL,0)


// Construct help dialog box
HelpWindow::HelpWindow(Adie *a):
  FXDialogBox(a,"Help on Adie",DECOR_TITLE|DECOR_BORDER|DECOR_RESIZE,0,0,0,0, 6,6,6,6, 4,4){

  // Set title
  setTitle(tr("Help on Adie"));

  // Bottom part
  FXHorizontalFrame *closebox=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|PACK_UNIFORM_WIDTH);

  // Destroy window when closed
  FXButton *button=new FXButton(closebox,tr("&Close"),NULL,this,FXDialogBox::ID_CLOSE,BUTTON_INITIAL|BUTTON_DEFAULT|LAYOUT_RIGHT|FRAME_RAISED|FRAME_THICK,0,0,0,0, 20,20,5,5);

  // Editor part
  FXHorizontalFrame *editbox=new FXHorizontalFrame(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_SUNKEN|FRAME_THICK,0,0,0,0, 0,0,0,0);
  helptext=new FXText(editbox,NULL,0,TEXT_READONLY|TEXT_WORDWRAP|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  helptext->setVisibleRows(50);
  helptext->setVisibleColumns(90);

  // Fill with help
  helptext->setText(help,strlen(help));
  helptext->setTabColumns(35);
  button->setFocus();
  }


// Clean up
HelpWindow::~HelpWindow(){
  helptext=(FXText*)-1;
  }
