/********************************************************************************
*                                                                               *
*                     U n d o a b l e   C o m m a n d s                         *
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
* $Id: Commands.h,v 1.16 2006/01/22 18:01:10 fox Exp $                          *
********************************************************************************/
#ifndef COMMANDS_H
#define COMMANDS_H



// Undo record for text fragment
class FXTextCommand : public FXCommand {
  FXDECLARE_ABSTRACT(FXTextCommand)
protected:
  FXText *text;     // Text widget
  FXchar *buffer;   // Character buffer
  FXint   pos;      // Character position
  FXint   ndel;     // Deleted characters
  FXint   nins;     // Inserted characters
public:
  FXTextCommand(FXText* txt,FXint p,FXint nd,FXint ni):text(txt),buffer(NULL),pos(p),ndel(nd),nins(ni){}
  virtual FXuint size() const;
  virtual ~FXTextCommand(){FXFREE(&buffer);}
  };


// Insert command
class FXTextInsert : public FXTextCommand {
  FXDECLARE_ABSTRACT(FXTextInsert)
public:
  FXTextInsert(FXText* txt,FXint p,FXint ni,const FXchar* ins);
  virtual FXString undoName() const { return "Undo insert"; }
  virtual FXString redoName() const { return "Redo insert"; }
  virtual void undo();
  virtual void redo();
  };


// Delete command
class FXTextDelete : public FXTextCommand {
  FXDECLARE_ABSTRACT(FXTextDelete)
public:
  FXTextDelete(FXText* txt,FXint p,FXint nd,const FXchar* del);
  virtual FXString undoName() const { return "Undo delete"; }
  virtual FXString redoName() const { return "Redo delete"; }
  virtual void undo();
  virtual void redo();
  };


// Replace command
class FXTextReplace : public FXTextCommand {
  FXDECLARE_ABSTRACT(FXTextReplace)
public:
  FXTextReplace(FXText* txt,FXint p,FXint nd,FXint ni,const FXchar* del,const FXchar* ins);
  virtual FXString undoName() const { return "Undo replace"; }
  virtual FXString redoName() const { return "Redo replace"; }
  virtual void undo();
  virtual void redo();
  };

#endif

