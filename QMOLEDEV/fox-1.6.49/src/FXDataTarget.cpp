/********************************************************************************
*                                                                               *
*                              D a t a   T a r g e t                            *
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
* $Id: FXDataTarget.cpp,v 1.36 2006/01/22 17:58:22 fox Exp $                    *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxkeys.h"
#include "FXHash.h"
#include "FXThread.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXApp.h"
#include "FXWindow.h"
#include "FXDataTarget.h"


/*
  Notes:
  - DataTarget connects GUI to basic values such as flags (FXbool), integral
    or real numbers, and strings (FXString).
  - Values in the application program may get updated from the GUI, and
    vice-versa GUI gets updated when the program has changed a value as well.
  - Would be nice to set value from message ID also...
  - When the sender of onCmdValue does not understand the ID_GETXXXXVALUE message,
    the data target keeps the same value as before.
  - Catch SEL_CHANGED when we have expunged this from FXTextField.
  - DT_VOID, i.e. unconnected FXDataTarget maybe it should grey out corresponding
    widgets.
  - Need to add ID_GETLONGVALUE/ID_SETLONGVALUE message handlers some day.
*/

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXDataTarget) FXDataTargetMap[]={
  FXMAPFUNC(SEL_COMMAND,FXDataTarget::ID_VALUE,FXDataTarget::onCmdValue),
  FXMAPFUNC(SEL_CHANGED,FXDataTarget::ID_VALUE,FXDataTarget::onCmdValue),
  FXMAPFUNC(SEL_UPDATE,FXDataTarget::ID_VALUE,FXDataTarget::onUpdValue),
  FXMAPFUNCS(SEL_COMMAND,FXDataTarget::ID_OPTION-10001,FXDataTarget::ID_OPTION+10000,FXDataTarget::onCmdOption),
  FXMAPFUNCS(SEL_UPDATE,FXDataTarget::ID_OPTION-10001,FXDataTarget::ID_OPTION+10000,FXDataTarget::onUpdOption),
  };


// Object implementation
FXIMPLEMENT(FXDataTarget,FXObject,FXDataTargetMap,ARRAYNUMBER(FXDataTargetMap))


// Value changed from widget
long FXDataTarget::onCmdValue(FXObject* sender,FXSelector sel,void*){
  FXdouble d;
  FXint    i;
  switch(type){
    case DT_CHAR:
      i=*((FXchar*)data);
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_GETINTVALUE),(void*)&i);
      *((FXchar*)data)=i;
      break;
    case DT_UCHAR:
      i=*((FXuchar*)data);
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_GETINTVALUE),(void*)&i);
      *((FXuchar*)data)=i;
      break;
    case DT_SHORT:
      i=*((FXshort*)data);
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_GETINTVALUE),(void*)&i);
      *((FXshort*)data)=i;
      break;
    case DT_USHORT:
      i=*((FXushort*)data);
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_GETINTVALUE),(void*)&i);
      *((FXushort*)data)=i;
      break;
    case DT_INT:
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_GETINTVALUE),data);
      break;
    case DT_UINT:
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_GETINTVALUE),data);
      break;
    case DT_LONG:
      i=(FXint) *((FXlong*)data);
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_GETINTVALUE),(void*)&i);
      *((FXlong*)data)=i;
      break;
    case DT_ULONG:
      i=(FXint) *((FXulong*)data);
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_GETINTVALUE),(void*)&i);
      *((FXulong*)data)=i;
      break;
    case DT_FLOAT:
      d=*((FXfloat*)data);
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_GETREALVALUE),(void*)&d);
      *((FXfloat*)data)=(FXfloat)d;
      break;
    case DT_DOUBLE:
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_GETREALVALUE),data);
      break;
    case DT_STRING:
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_GETSTRINGVALUE),data);
      break;
    }
  if(target){
    target->handle(this,FXSEL(FXSELTYPE(sel),message),data);
    }
  return 1;
  }


// Widget changed from value
long FXDataTarget::onUpdValue(FXObject* sender,FXSelector,void*){
  FXdouble d;
  FXint    i;
  switch(type){
    case DT_CHAR:
      i=*((FXchar*)data);
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETINTVALUE),(void*)&i);
      break;
    case DT_UCHAR:
      i=*((FXuchar*)data);
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETINTVALUE),(void*)&i);
      break;
    case DT_SHORT:
      i=*((FXshort*)data);
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETINTVALUE),(void*)&i);
      break;
    case DT_USHORT:
      i=*((FXushort*)data);
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETINTVALUE),(void*)&i);
      break;
    case DT_INT:
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETINTVALUE),data);
      break;
    case DT_UINT:
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETINTVALUE),data);
      break;
    case DT_LONG:
      i=(FXint) *((FXlong*)data);
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETINTVALUE),(void*)&i);
      break;
    case DT_ULONG:
      i=(FXint) *((FXulong*)data);
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETINTVALUE),(void*)&i);
      break;
    case DT_FLOAT:
      d=*((FXfloat*)data);
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETREALVALUE),(void*)&d);
      break;
    case DT_DOUBLE:
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETREALVALUE),data);
      break;
    case DT_STRING:
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETSTRINGVALUE),data);
      break;
    }
  return 1;
  }


// Value set from message id
long FXDataTarget::onCmdOption(FXObject*,FXSelector sel,void*){
  FXint num=((FXint)FXSELID(sel))-ID_OPTION;
  switch(type){
    case DT_CHAR:
      *((FXchar*)data)=num;
      break;
    case DT_UCHAR:
      *((FXuchar*)data)=num;
      break;
    case DT_SHORT:
      *((FXshort*)data)=num;
      break;
    case DT_USHORT:
      *((FXushort*)data)=num;
      break;
    case DT_INT:
      *((FXint*)data)=num;
      break;
    case DT_UINT:
      *((FXuint*)data)=num;
      break;
    case DT_LONG:
      *((FXlong*)data)=num;
      break;
    case DT_ULONG:
      *((FXulong*)data)=num;
      break;
    case DT_FLOAT:
      *((FXfloat*)data)=(FXfloat)num;
      break;
    case DT_DOUBLE:
      *((FXdouble*)data)=num;
      break;
    }
  if(target){
    target->handle(this,FXSEL(FXSELTYPE(sel),message),data);
    }
  return 1;
  }


// Check widget whose message id matches
long FXDataTarget::onUpdOption(FXObject* sender,FXSelector sel,void*){
  FXint num=((FXint)FXSELID(sel))-ID_OPTION;
  FXint i=0;
  switch(type){
    case DT_CHAR:
      i=*((FXchar*)data);
      break;
    case DT_UCHAR:
      i=*((FXuchar*)data);
      break;
    case DT_SHORT:
      i=*((FXshort*)data);
      break;
    case DT_USHORT:
      i=*((FXushort*)data);
      break;
    case DT_INT:
      i=*((FXint*)data);
      break;
    case DT_UINT:
      i=*((FXuint*)data);
      break;
    case DT_LONG:
      i=(FXint) *((FXlong*)data);
      break;
    case DT_ULONG:
      i=(FXint) *((FXulong*)data);
      break;
    case DT_FLOAT:
      i=(FXint) *((FXfloat*)data);
      break;
    case DT_DOUBLE:
      i=(FXint) *((FXdouble*)data);
      break;
    }
  if(i==num){
    sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_CHECK),NULL);
    }
  else{
    sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_UNCHECK),NULL);
    }
  return 1;
  }


/// Destroy
FXDataTarget::~FXDataTarget(){
  target=(FXObject*)-1L;
  data=(void*)-1L;
  }

}

