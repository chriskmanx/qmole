/********************************************************************************
*                                                                               *
*                  F i l e   P r o p e r t i e s   D i a l o g                  *
*                                                                               *
*********************************************************************************
* Copyright (C) 2000,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: PropertyDialog.cpp,v 1.24 2006/01/22 17:58:15 fox Exp $                  *
********************************************************************************/
#include "xincs.h"
#include "fx.h"
#include "PropertyDialog.h"
#include "PathFinder.h"
#include "icons.h"
#include <stdio.h>
#include <stdlib.h>

/*
  Notes:
  - Refer to RFC 2045, 2046, 2047, 2048, and 2077.
    The Internet media type registry is at:
    ftp://ftp.iana.org/in-notes/iana/assignments/media-types/
*/

/**********************************  Preferences  ******************************/



FXIMPLEMENT(PropertyDialog,FXDialogBox,NULL,0)


// Create properties dialog
PropertyDialog::PropertyDialog(FXWindow *owner):
  FXDialogBox(owner,"Properties",DECOR_TITLE|DECOR_BORDER|DECOR_RESIZE,0,0,0,0, 4,4,4,4, 4,4){

  setTitle(tr("Properties"));

  // Close button
  FXHorizontalFrame *closebox=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|PACK_UNIFORM_WIDTH);
  new FXButton(closebox,tr("&OK"),NULL,this,FXDialogBox::ID_ACCEPT,BUTTON_INITIAL|BUTTON_DEFAULT|LAYOUT_RIGHT|FRAME_RAISED|FRAME_THICK,0,0,0,0, 20,20,5,5);
  new FXButton(closebox,tr("&Cancel"),NULL,this,FXDialogBox::ID_CANCEL,BUTTON_DEFAULT|LAYOUT_RIGHT|FRAME_RAISED|FRAME_THICK,0,0,0,0, 20,20,5,5);

  // Separator
  new FXHorizontalSeparator(this,SEPARATOR_GROOVE|LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X);

  FXTabBook* tabbook=new FXTabBook(this,NULL,0,LAYOUT_SIDE_TOP|TABBOOK_TOPTABS|LAYOUT_FILL_X|LAYOUT_FILL_Y);

  // ===== General Info =====
  new FXTabItem(tabbook,tr("&General\tGeneral Information\tGeneral information about the item."),NULL,TAB_TOP|ICON_BEFORE_TEXT);
  FXVerticalFrame *generalFrame=new FXVerticalFrame(tabbook,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_RAISED|FRAME_THICK);

  FXGroupBox *namegroup=new FXGroupBox(generalFrame,tr("Description"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X);
  filename=new FXLabel(namegroup,FXString::null,NULL,LAYOUT_CENTER_Y|LAYOUT_SIDE_LEFT|ICON_BEFORE_TEXT|LAYOUT_FIX_HEIGHT,0,0,0,40);
  filename->setTarget(owner);
  filename->setSelector(PathFinderMain::ID_FILE_DESC);

  FXGroupBox *attrgroup=new FXGroupBox(generalFrame,tr("Attributes"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X);
  FXMatrix *attrmatrix=new FXMatrix(attrgroup,2,MATRIX_BY_COLUMNS|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  new FXLabel(attrmatrix,tr("Type:"),NULL,LAYOUT_RIGHT);
  filetype=new FXLabel(attrmatrix,FXString::null,NULL,LAYOUT_LEFT|LAYOUT_FILL_COLUMN);
  filetype->setTarget(owner);
  filetype->setSelector(PathFinderMain::ID_FILE_TYPE);
  new FXLabel(attrmatrix,tr("Location:"),NULL,LAYOUT_RIGHT);
  directory=new FXLabel(attrmatrix,FXString::null,NULL,LAYOUT_LEFT|LAYOUT_FILL_COLUMN);
  directory->setTarget(owner);
  directory->setSelector(PathFinderMain::ID_FILE_LOCATION);
  new FXLabel(attrmatrix,tr("Size:"),NULL,LAYOUT_RIGHT);
  filesize=new FXLabel(attrmatrix,FXString::null,NULL,LAYOUT_LEFT|LAYOUT_FILL_COLUMN);
  filesize->setTarget(owner);
  filesize->setSelector(PathFinderMain::ID_FILE_SIZE);

  FXGroupBox *timegroup=new FXGroupBox(generalFrame,tr("File Time"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X);
  FXMatrix *timematrix=new FXMatrix(timegroup,2,MATRIX_BY_COLUMNS|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  new FXLabel(timematrix,tr("Created:"),NULL,LAYOUT_RIGHT);
  createtime=new FXLabel(timematrix,FXString::null,NULL,LAYOUT_LEFT|LAYOUT_FILL_COLUMN);
  createtime->setTarget(owner);
  createtime->setSelector(PathFinderMain::ID_FILE_CREATED);
  new FXLabel(timematrix,tr("Modified:"),NULL,LAYOUT_RIGHT);
  modifytime=new FXLabel(timematrix,FXString::null,NULL,LAYOUT_LEFT|LAYOUT_FILL_COLUMN);
  modifytime->setTarget(owner);
  modifytime->setSelector(PathFinderMain::ID_FILE_MODIFIED);
  new FXLabel(timematrix,tr("Accessed:"),NULL,LAYOUT_RIGHT);
  accesstime=new FXLabel(timematrix,FXString::null,NULL,LAYOUT_LEFT|LAYOUT_FILL_COLUMN);
  accesstime->setTarget(owner);
  accesstime->setSelector(PathFinderMain::ID_FILE_ACCESSED);

  // ===== Permissions =====
  new FXTabItem(tabbook,tr("&Permissions\tAccess Permissions\tAccess permissions for this item."),NULL,TAB_TOP|ICON_BEFORE_TEXT);
  FXVerticalFrame *permFrame=new FXVerticalFrame(tabbook,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_RAISED|FRAME_THICK);
  FXGroupBox *accessgroup=new FXGroupBox(permFrame,tr("Access Permissions"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X);
  FXMatrix *accessmatrix=new FXMatrix(accessgroup,6,MATRIX_BY_COLUMNS|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  new FXFrame(accessmatrix,LAYOUT_FILL_COLUMN);
  new FXLabel(accessmatrix,tr("Read"),NULL,LAYOUT_CENTER_X);
  new FXLabel(accessmatrix,tr("Write"),NULL,LAYOUT_CENTER_X);
  new FXLabel(accessmatrix,tr("Exec"),NULL,LAYOUT_CENTER_X);
  new FXFrame(accessmatrix,LAYOUT_FILL_COLUMN);
  new FXLabel(accessmatrix,tr("Special"),NULL,LAYOUT_CENTER_X);

  new FXLabel(accessmatrix,tr("&User"),NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN);
  new FXCheckButton(accessmatrix,FXString::null,owner,PathFinderMain::ID_RUSR,LAYOUT_CENTER_X|LAYOUT_CENTER_Y);
  new FXCheckButton(accessmatrix,FXString::null,owner,PathFinderMain::ID_WUSR,LAYOUT_CENTER_X|LAYOUT_CENTER_Y);
  new FXCheckButton(accessmatrix,FXString::null,owner,PathFinderMain::ID_XUSR,LAYOUT_CENTER_X|LAYOUT_CENTER_Y);
  new FXLabel(accessmatrix,tr("Set UID"),NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN);
  new FXCheckButton(accessmatrix,FXString::null,owner,PathFinderMain::ID_SUID,LAYOUT_CENTER_X|LAYOUT_CENTER_Y);

  new FXLabel(accessmatrix,tr("&Group"),NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN);
  new FXCheckButton(accessmatrix,FXString::null,owner,PathFinderMain::ID_RGRP,LAYOUT_CENTER_X|LAYOUT_CENTER_Y);
  new FXCheckButton(accessmatrix,FXString::null,owner,PathFinderMain::ID_WGRP,LAYOUT_CENTER_X|LAYOUT_CENTER_Y);
  new FXCheckButton(accessmatrix,FXString::null,owner,PathFinderMain::ID_XGRP,LAYOUT_CENTER_X|LAYOUT_CENTER_Y);
  new FXLabel(accessmatrix,tr("Set GID"),NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN);
  new FXCheckButton(accessmatrix,FXString::null,owner,PathFinderMain::ID_SGID,LAYOUT_CENTER_X|LAYOUT_CENTER_Y);

  new FXLabel(accessmatrix,tr("&Other"),NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN);
  new FXCheckButton(accessmatrix,FXString::null,owner,PathFinderMain::ID_ROTH,LAYOUT_CENTER_X|LAYOUT_CENTER_Y);
  new FXCheckButton(accessmatrix,FXString::null,owner,PathFinderMain::ID_WOTH,LAYOUT_CENTER_X|LAYOUT_CENTER_Y);
  new FXCheckButton(accessmatrix,FXString::null,owner,PathFinderMain::ID_XOTH,LAYOUT_CENTER_X|LAYOUT_CENTER_Y);
  new FXLabel(accessmatrix,tr("Sticky"),NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN);
  new FXCheckButton(accessmatrix,FXString::null,owner,PathFinderMain::ID_SVTX,LAYOUT_CENTER_X|LAYOUT_CENTER_Y);

  FXGroupBox *ownergroup=new FXGroupBox(permFrame,tr("Ownership"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X);
  FXMatrix *ownermatrix=new FXMatrix(ownergroup,2,MATRIX_BY_COLUMNS|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  new FXLabel(ownermatrix,tr("O&wner"),NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN);
  fileowner=new FXTextField(ownermatrix,20,owner,PathFinderMain::ID_OWNER,LAYOUT_FILL_X|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN|FRAME_SUNKEN|FRAME_THICK);
  new FXLabel(ownermatrix,tr("Grou&p"),NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN);
  filegroup=new FXTextField(ownermatrix,20,owner,PathFinderMain::ID_GROUP,LAYOUT_FILL_X|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN|FRAME_SUNKEN|FRAME_THICK);
  }


// Destroy
PropertyDialog::~PropertyDialog(){
  }
