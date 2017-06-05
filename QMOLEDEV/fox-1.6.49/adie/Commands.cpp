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
* $Id: Commands.cpp,v 1.18 2006/01/22 18:01:10 fox Exp $                        *
********************************************************************************/
#include "fx.h"
#include "Commands.h"
#include "FXRex.h"
#include "FXArray.h"
#include "Hilite.h"
#include "TextWindow.h"
#include "Adie.h"


/*
  Notes:

  - When manipulating text widget, we generate callbacks even when changing
    the text via undo or redo.
  - During the execution of an undo or redo, FXUndoList is marked as busy;
    thus, the busy state may be checked to determine if one is in the middle
    of an undo or redo.
*/


/*******************************************************************************/

FXIMPLEMENT_ABSTRACT(FXTextCommand,FXCommand,NULL,0)


// Return size of record plus any data kept here
FXuint FXTextCommand::size() const {
  return sizeof(FXTextCommand)+ndel;
  }


FXIMPLEMENT_ABSTRACT(FXTextInsert,FXTextCommand,NULL,0)

// Insert command
FXTextInsert::FXTextInsert(FXText* txt,FXint p,FXint ni,const FXchar* ins):FXTextCommand(txt,p,0,ni){
  FXMALLOC(&buffer,FXchar,ni);
  memcpy(buffer,ins,ni);
  }


// Undo an insert removes the inserted text
void FXTextInsert::undo(){
  text->removeText(pos,nins,TRUE);
  text->setCursorPos(pos);
  text->makePositionVisible(pos);
  }


// Redo an insert inserts the same old text again
void FXTextInsert::redo(){
  text->insertText(pos,buffer,nins,TRUE);
  text->setCursorPos(pos+nins);
  text->makePositionVisible(pos+nins);
  }


FXIMPLEMENT_ABSTRACT(FXTextDelete,FXTextCommand,NULL,0)

// Delete command
FXTextDelete::FXTextDelete(FXText* txt,FXint p,FXint nd,const FXchar* del):FXTextCommand(txt,p,nd,0){
  FXMALLOC(&buffer,FXchar,nd);
  memcpy(buffer,del,nd);
  }


// Undo a delete reinserts the old text
void FXTextDelete::undo(){
  text->insertText(pos,buffer,ndel,TRUE);
  text->setCursorPos(pos+ndel);
  text->makePositionVisible(pos+ndel);
  }


// Redo a delete removes it again
void FXTextDelete::redo(){
  text->removeText(pos,ndel,TRUE);
  text->setCursorPos(pos);
  text->makePositionVisible(pos);
  }


FXIMPLEMENT_ABSTRACT(FXTextReplace,FXTextCommand,NULL,0)

// Replace command
FXTextReplace::FXTextReplace(FXText* txt,FXint p,FXint nd,FXint ni,const FXchar* del,const FXchar* ins):FXTextCommand(txt,p,nd,ni){
  FXMALLOC(&buffer,FXchar,nd+ni);
  memcpy(buffer,del,nd);
  memcpy(buffer+nd,ins,ni);
  }


// Undo a replace reinserts the old text
void FXTextReplace::undo(){
  text->replaceText(pos,nins,buffer,ndel,TRUE);
  text->setCursorPos(pos+ndel);
  text->makePositionVisible(pos+ndel);
  }


// Redo a replace reinserts the new text
void FXTextReplace::redo(){
  text->replaceText(pos,ndel,buffer+ndel,nins,TRUE);
  text->setCursorPos(pos+nins);
  text->makePositionVisible(pos+nins);
  }

