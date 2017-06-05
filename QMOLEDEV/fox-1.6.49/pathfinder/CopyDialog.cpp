/********************************************************************************
*                                                                               *
*                       F i l e   C o p y   D i a l o g                         *
*                                                                               *
*********************************************************************************
* Copyright (C) 2005,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: CopyDialog.cpp,v 1.4 2006/01/22 17:58:15 fox Exp $                       *
********************************************************************************/
#include "xincs.h"
#include "fx.h"
#include "CopyDialog.h"


/*
  Notes:
  - Make this look better
*/


/**********************************  Preferences  ******************************/

FXIMPLEMENT(CopyDialog,FXDialogBox,NULL,0)


// Create properties dialog
CopyDialog::CopyDialog(FXWindow *owner,const FXString& name):FXDialogBox(owner,name,DECOR_TITLE|DECOR_BORDER|DECOR_RESIZE,0,0,0,0, 4,4,4,4, 4,4){
  FXHorizontalFrame* buttons=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|PACK_UNIFORM_WIDTH,0,0,0,0, 0,0,0,0);
  new FXButton(buttons,tr("&Cancel"),NULL,this,FXDialogBox::ID_CANCEL,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_X|LAYOUT_CENTER_Y,0,0,0,0,20,20);
  new FXButton(buttons,tr("&OK"),NULL,this,FXDialogBox::ID_ACCEPT,BUTTON_INITIAL|BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_X|LAYOUT_CENTER_Y);
  new FXHorizontalSeparator(this,SEPARATOR_GROOVE|LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X);
  FXMatrix *matrix=new FXMatrix(this,2,MATRIX_BY_COLUMNS|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0);
  new FXLabel(matrix,tr("&Old name:"),NULL,LAYOUT_FILL_X|JUSTIFY_LEFT);
  oldname=new FXTextField(matrix,50,NULL,0,TEXTFIELD_ENTER_ONLY|FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X|LAYOUT_FILL_COLUMN);
  oldname->setEditable(FALSE);
  new FXLabel(matrix,tr("&New name:"),NULL,LAYOUT_FILL_X|JUSTIFY_LEFT);
  newname=new FXTextField(matrix,50,this,FXDialogBox::ID_ACCEPT,TEXTFIELD_ENTER_ONLY|FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X|LAYOUT_FILL_COLUMN);
  }


// Destroy
CopyDialog::~CopyDialog(){
  oldname=(FXTextField*)-1L;
  newname=(FXTextField*)-1L;
  }
