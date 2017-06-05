/********************************************************************************
*                                                                               *
*                        P r e f e r e n c e s   D i a l o g                    *
*                                                                               *
*********************************************************************************
* Copyright (C) 2001,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: Preferences.h,v 1.31 2006/01/22 18:01:11 fox Exp $                       *
********************************************************************************/
#ifndef PREFERENCES_H
#define PREFERENCES_H


//////////////////////////////  UNDER DEVELOPMENT  //////////////////////////////


class TextWindow;
class Adie;
class FXSyntax;


class Preferences : public FXDialogBox {
  FXDECLARE(Preferences)
protected:
  FXText          *filepattext;
  FXList          *stylelist;
  FXTextField     *stylename;
  FXIcon          *pal;
  FXIcon          *ind;
  FXIcon          *pat;
  FXIcon          *sty;
private:
  Preferences(){}
  Preferences(const Preferences&);
  Preferences& operator=(const Preferences&);
public:

  // Create preferences dialog
  Preferences(TextWindow *owner);

  // Owner is text window
  Adie* getApp() const { return (Adie*)FXDialogBox::getApp(); }

  // Set filename patterns
  void setPatterns(const FXString& patterns);

  // Get filename patterns
  FXString getPatterns() const;

  // Set language syntax
  void setSyntax(FXSyntax* syn);

    // Clean up
  virtual ~Preferences();
  };

#endif
