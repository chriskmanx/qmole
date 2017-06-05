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
* $Id: CopyDialog.h,v 1.4 2006/01/22 17:58:15 fox Exp $                         *
********************************************************************************/
#ifndef COPYDIALOG_H
#define COPYDIALOG_H


// Copy/Move/Link/Rename dialog
class CopyDialog : public FXDialogBox {
  FXDECLARE(CopyDialog)
protected:
  FXTextField *oldname;
  FXTextField *newname;
private:
  CopyDialog(){}
  CopyDialog(const CopyDialog&);
public:

  // Construct copy/move/link/rename dialog
  CopyDialog(FXWindow *owner,const FXString& name);

  // Get/set old name
  void setOldName(const FXString& nm){ oldname->setText(nm); }
  FXString getOldName() const { return oldname->getText(); }

  // Get/set new name
  void setNewName(const FXString& nm){ newname->setText(nm); }
  FXString getNewName() const { return newname->getText(); }

  // Destroy
  virtual ~CopyDialog();
  };

#endif
