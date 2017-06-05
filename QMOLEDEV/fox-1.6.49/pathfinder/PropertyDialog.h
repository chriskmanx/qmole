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
* $Id: PropertyDialog.h,v 1.15 2006/01/22 17:58:15 fox Exp $                    *
********************************************************************************/
#ifndef PROPERTYDIALOG_H
#define PROPERTYDIALOG_H


// Property dialog
class PropertyDialog : public FXDialogBox {
  FXDECLARE(PropertyDialog)
protected:
  FXLabel       *filename;
  FXLabel       *filetype;
  FXLabel       *filesize;
  FXLabel       *directory;
  FXLabel       *createtime;
  FXLabel       *modifytime;
  FXLabel       *accesstime;
  FXTextField   *fileowner;
  FXTextField   *filegroup;
private:
  PropertyDialog(){}
  PropertyDialog(const PropertyDialog&);
public:
  PropertyDialog(FXWindow *owner);
  virtual ~PropertyDialog();
  };

#endif
