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
* $Id: Adie.h,v 1.54.2.1 2006/03/21 07:29:06 fox Exp $                              *
********************************************************************************/
#ifndef ADIE_H
#define ADIE_H


// Version
#define VERSION_MAJOR 3
#define VERSION_MINOR 0
#define VERSION_PATCH 0

class HelpWindow;
class Preferences;
class TextWindow;
struct ParseInfo;


// Main Application class
class Adie : public FXApp {
  FXDECLARE(Adie)
  friend class TextWindow;
protected:
  TextWindowList  windowlist;                   // Window list
  FXFileDict     *associations;                 // File association table
  FXSyntaxList    syntaxes;                     // List of syntax patterns
  FXIcon         *bigicon;                      // Big application icon
  FXIcon         *smallicon;                    // Small application icon
  FXIcon         *newicon;
  FXIcon         *reloadicon;
  FXIcon         *openicon;
  FXIcon         *saveicon;
  FXIcon         *saveasicon;
  FXIcon         *printicon;
  FXIcon         *cuticon;
  FXIcon         *copyicon;
  FXIcon         *pasteicon;
  FXIcon         *deleteicon;
  FXIcon         *undoicon;
  FXIcon         *redoicon;
  FXIcon         *fontsicon;
  FXIcon         *helpicon;
  FXIcon         *quiticon;
  FXIcon         *searchicon;
  FXIcon         *searchnexticon;
  FXIcon         *searchprevicon;
  FXIcon         *bookseticon;
  FXIcon         *booknexticon;
  FXIcon         *bookprevicon;
  FXIcon         *bookdelicon;
  FXIcon         *shiftlefticon;
  FXIcon         *shiftrighticon;
private:
  Adie(){}
  Adie(const Adie&);
  Adie& operator=(const Adie&);
  FXbool loadSyntaxFile(const FXString& file);
public:
  enum{
    ID_CLOSEALL=FXApp::ID_LAST,
    ID_LAST
    };
public:
  long onCmdCloseAll(FXObject*,FXSelector,void*);
public:

  // Construct application object
  Adie(const FXString& name);

  // Initialize application
  virtual void init(int& argc,char** argv,bool connect=TRUE);

  // Exit application
  virtual void exit(FXint code=0);

  // Delete application object
  virtual ~Adie();
  };

#endif

