/********************************************************************************
*                                                                               *
*                     M a i n   W i n d o w   O b j e c t                       *
*                                                                               *
*********************************************************************************
* Copyright (C) 1998,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXMainWindow.cpp,v 1.31 2006/01/22 17:58:35 fox Exp $                    *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXThread.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXRegistry.h"
#include "FXAccelTable.h"
#include "FXApp.h"
#include "FXCursor.h"
#include "FXRootWindow.h"
#include "FXMainWindow.h"

/*
  Notes:
  - allow resize option..
  - Iconified/normal.
  - Want unlimited number of main windows.
  - Don't call X11/WIN32 unless xid and application is initialized.
*/


#define DISPLAY(app) ((Display*)((app)->display))


using namespace FX;

/*******************************************************************************/

namespace FX {


// Object implementation
FXIMPLEMENT(FXMainWindow,FXTopWindow,NULL,0)


// Make main window
FXMainWindow::FXMainWindow(FXApp* a,const FXString& name,FXIcon *ic,FXIcon *mi,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb,FXint hs,FXint vs):
  FXTopWindow(a,name,ic,mi,opts,x,y,w,h,pl,pr,pt,pb,hs,vs){
  }


// Create server-side resources
void FXMainWindow::create(){
  FXTopWindow::create();
  if(xid){
    if(getApp()->isInitialized()){
#ifndef WIN32
      // Set the WM_COMMAND hint on non-owned toplevel windows
      XSetCommand(DISPLAY(getApp()),xid,(char**)getApp()->getArgv(),getApp()->getArgc());
#endif
      }
    }
  }


// Destroy
FXMainWindow::~FXMainWindow(){
  }

}
