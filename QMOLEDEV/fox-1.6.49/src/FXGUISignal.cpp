/********************************************************************************
*                                                                               *
*                        S i g n a l   G U I   T h r e a d                      *
*                                                                               *
*********************************************************************************
* Copyright (C) 2005,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXGUISignal.cpp,v 1.5.2.1 2006/08/13 15:15:35 fox Exp $                      *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXException.h"
#include "FXHash.h"
#include "FXThread.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXRegistry.h"
#include "FXApp.h"
#ifndef WIN32
#include <pthread.h>
#include <semaphore.h>
#ifdef __CYGWIN__
#include <asm/socket.h>         /* For FIONREAD */
#endif
#else
#include <process.h>
#endif
#include "FXGUISignal.h"

/*
  Notes:
  - On UNIX, pipe will be closed on exec of child process; we don't want
    to get parent/child pipes crossed.
  - On Windows, its a manual reset event object.
  - This class is inspired by Daniel Gehriger's original FXExThreadEvent.
*/


using namespace FX;


/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXGUISignal) FXGUISignalMap[]={
  FXMAPFUNC(SEL_IO_READ,FXGUISignal::ID_IO_READ,FXGUISignal::onSignal)
  };


// Object implementation
FXIMPLEMENT(FXGUISignal,FXObject,FXGUISignalMap,ARRAYNUMBER(FXGUISignalMap));


// Initialize
FXGUISignal::FXGUISignal():app((FXApp*)-1L),target(NULL),data(NULL),message(0){
#ifndef WIN32
  fd[0]=fd[1]=-1;
#else
  event=NULL;
#endif
  }


// Add handler to application
FXGUISignal::FXGUISignal(FXApp* a,FXObject* tgt,FXSelector sel,void* ptr):app(a),target(tgt),data(ptr),message(sel){
#ifndef WIN32
  int res=pipe(fd);
  if(res!=0){ throw FXResourceException("unable to create pipe."); }
  // Change both ends of the pipe to close on exec since
  // we don't want to cross pipes with the child process.
  fcntl(fd[0],F_SETFD,FD_CLOEXEC);
  fcntl(fd[1],F_SETFD,FD_CLOEXEC);
  app->addInput(fd[0],INPUT_READ,this,ID_IO_READ);
#else
  // Create event object for waking up GUI; its manual reset type
  // event because if the main user interface thread is awake already
  // we want it to go through the event loop once again before blocking.
  event=CreateEvent(NULL,TRUE,FALSE,NULL);
  if(event==NULL){ throw FXResourceException("unable to create event."); }
  app->addInput(event,INPUT_READ,this,ID_IO_READ);
#endif
  }


// Called by worker thread to wake GUI thread
void FXGUISignal::signal(){
#ifndef WIN32
  size_t n=0;
  if(0<=ioctl(fd[0],FIONREAD,(char*)&n) && n==0){ write(fd[1],"!",1); }
#else
  SetEvent(event);
#endif
  }


// Fire signal message to target
long FXGUISignal::onSignal(FXObject*,FXSelector,void*){
#ifndef WIN32
  FXuchar stuff[1];
  read(fd[0],stuff,1);
#else
  ResetEvent(event);
#endif
  return target && target->handle(this,FXSEL(SEL_IO_READ,message),data);
  }


// Remove handler from application
FXGUISignal::~FXGUISignal(){
#ifndef WIN32
  app->removeInput(fd[0],INPUT_READ);
  close(fd[0]);
  close(fd[1]);
#else
  app->removeInput(event,INPUT_READ);
  CloseHandle(event);
#endif
  app=(FXApp*)-1L;
  target=(FXObject*)-1L;
  data=(void*)-1L;
  }

}
