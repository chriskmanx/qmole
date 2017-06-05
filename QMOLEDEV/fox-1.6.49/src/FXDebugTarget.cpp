/********************************************************************************
*                                                                               *
*                      D e b u g - T a r g e t   O b j e c t                    *
*                                                                               *
*********************************************************************************
* Copyright (C) 1997,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* This library is free software; you can redistribute it and/or                 *
* modify it under the terms of the GNU Lesser General Public                    *
* License as published by the Free Software Foundation; either                  *
* version 2.1 of the License, or (at your option) any later version.            *
*                                                                               *
* This library is distributed in the hope that it will be useful,               *
* but WITHOUT ANY WARRANTY; without even the implied warranty of                *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU             *
* Lesser General Public License for more details.                               *
*                                                                               *
* You should have received a copy of the GNU Lesser General Public              *
* License along with this library; if not, write to the Free Software           *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.    *
*********************************************************************************
* $Id: FXDebugTarget.cpp,v 1.33 2006/01/22 17:58:22 fox Exp $                   *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXDebugTarget.h"

/*

  Notes:
  - The point of this object is simply to accept all messages,
    and print out where they came from, which type they were, etc.
  - So purely for debugging purposes.
*/

using namespace FX;

/*******************************************************************************/

namespace FX {


// Table of message type names
const char *const FXDebugTarget::messageTypeName[]={
  "SEL_NONE",
  "SEL_KEYPRESS",
  "SEL_KEYRELEASE",
  "SEL_LEFTBUTTONPRESS",
  "SEL_LEFTBUTTONRELEASE",
  "SEL_MIDDLEBUTTONPRESS",
  "SEL_MIDDLEBUTTONRELEASE",
  "SEL_RIGHTBUTTONPRESS",
  "SEL_RIGHTBUTTONRELEASE",
  "SEL_MOTION",
  "SEL_ENTER",
  "SEL_LEAVE",
  "SEL_FOCUSIN",
  "SEL_FOCUSOUT",
  "SEL_KEYMAP",
  "SEL_UNGRABBED",
  "SEL_PAINT",
  "SEL_CREATE",
  "SEL_DESTROY",
  "SEL_UNMAP",
  "SEL_MAP",
  "SEL_CONFIGURE",
  "SEL_SELECTION_LOST",
  "SEL_SELECTION_GAINED",
  "SEL_SELECTION_REQUEST",
  "SEL_RAISED",
  "SEL_LOWERED",
  "SEL_CLOSE",
  "SEL_DELETE",
  "SEL_MINIMIZE",
  "SEL_RESTORE",
  "SEL_MAXIMIZE",
  "SEL_UPDATE",
  "SEL_COMMAND",
  "SEL_CLICKED",
  "SEL_DOUBLECLICKED",
  "SEL_TRIPLECLICKED",
  "SEL_MOUSEWHEEL",
  "SEL_CHANGED",
  "SEL_VERIFY",
  "SEL_DESELECTED",
  "SEL_SELECTED",
  "SEL_INSERTED",
  "SEL_REPLACED",
  "SEL_DELETED",
  "SEL_OPENED",
  "SEL_CLOSED",
  "SEL_EXPANDED",
  "SEL_COLLAPSED",
  "SEL_BEGINDRAG",
  "SEL_ENDDRAG",
  "SEL_DRAGGED",
  "SEL_LASSOED",
  "SEL_TIMEOUT",
  "SEL_SIGNAL",
  "SEL_CLIPBOARD_LOST",
  "SEL_CLIPBOARD_GAINED",
  "SEL_CLIPBOARD_REQUEST",
  "SEL_CHORE",
  "SEL_FOCUS_SELF",
  "SEL_FOCUS_RIGHT",
  "SEL_FOCUS_LEFT",
  "SEL_FOCUS_DOWN",
  "SEL_FOCUS_UP",
  "SEL_FOCUS_NEXT",
  "SEL_FOCUS_PREV",
  "SEL_DND_ENTER",
  "SEL_DND_LEAVE",
  "SEL_DND_DROP",
  "SEL_DND_MOTION",
  "SEL_DND_REQUEST",
  "SEL_IO_READ",
  "SEL_IO_WRITE",
  "SEL_IO_EXCEPT",
  "SEL_PICKED",
  "SEL_QUERY_TIP",
  "SEL_QUERY_HELP",
  "SEL_DOCKED",
  "SEL_FLOATED",
  "SEL_SESSION_NOTIFY",
  "SEL_SESSION_CLOSED"
  };


// Map
FXDEFMAP(FXDebugTarget) FXDebugTargetMap[]={
  FXMAPTYPES(SEL_KEYPRESS,SEL_LAST,FXDebugTarget::onMessage),
  };


// Object implementation
FXIMPLEMENT(FXDebugTarget,FXObject,FXDebugTargetMap,ARRAYNUMBER(FXDebugTargetMap))


// Init
FXDebugTarget::FXDebugTarget(){
  lastsender=NULL;
  lastsel=0;
  count=0;
  }


// Got one
long FXDebugTarget::onMessage(FXObject* sender,FXSelector sel,void* ptr){
  FXuint type=FXSELTYPE(sel);
  FXuint msid=FXSELID(sel);
  FXASSERT(ARRAYNUMBER(messageTypeName)==SEL_LAST);
  if(sender!=lastsender || sel!=lastsel){
    fxmessage("\nTYPE:%-23s ID:%-5d SENDER: %-15s PTR: 0x%08p #%-4d",type<SEL_LAST?messageTypeName[type]:"ILLEGAL",msid,sender?sender->getClassName():"NULL",ptr,1);
    lastsender=sender;
    lastsel=sel;
    count=1;
    }
  else{
    count++;
    fxmessage("\b\b\b\b%-4d",count);
    }
  return 0;
  }

}
