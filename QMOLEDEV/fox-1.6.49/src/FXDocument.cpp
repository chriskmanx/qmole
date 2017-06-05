/********************************************************************************
*                                                                               *
*                         D o c u m e n t   O b j e c t                         *
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
* $Id: FXDocument.cpp,v 1.24 2006/01/22 17:58:24 fox Exp $                      *
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
#include "FXPath.h"
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXAccelTable.h"
#include "FXObjectList.h"
#include "FXApp.h"
#include "FXCursor.h"
#include "FXFrame.h"
#include "FXRootWindow.h"
#include "FXShell.h"
#include "FXDocument.h"


/*
  Notes:

  - How to use:

    1) You construct a FXDocument class, e.g. when your app loads a file.

    2) FXDocument constructs one or more FXMDIChild widgets, and makes itself the target
       of the each of them so that it will be notified when the FXMDIChild minimizes,
       maximizes, or gets closed etc.

    3) MenuCommands, Buttons and what have you which need to send messages to FXDocument
       will do so through delegation via FXMDIClient, and FXMDIChild.

       Thus, the only places pointing to a specific FXDocument are FXMDIChild windows,
       and their sub-widgets (such as for example the FXGLViewer).

    4) When the user interacts with one of the FXMDIChild's controls, data in the
       FXDocument is changed.  If any other FXMDIChild windows are used this same
       data, they may need to be updated.

       FXDocument can do this through message ``broadcast'' from the FXMDIClient
       to its list FXMDIChild windows:


       mdiclient->forallWindows(sender,sel,ptr)


       sends a message to ALL FXMDIChild windows under mdiclient. Also:


       mdiclient->forallDocWindows(document,sender,sel,ptr)

       sends a message to all FXMDIChild windows whose target is document.

       A new message, understood by ALL FXWindows, has been added if you want to
       simply force a repaint; so in order to just repaint everything:

        long MyDocument::onCmdChangedSomething(FXObject* sender, FXSelector sel,void* ptr){
          forallWindows(sender,FXSEL(SEL_COMMAND,ID_UPDATE),ptr);
          return 1;
          }


    5) Thus,

         - There is only ONE place that keeps track of all the FXMDIChild windows
           pertaining to a certain document.

           Your application may want to cycle through the FXMDIChild list itself;
           this can be done by:

             mdichild = mdiclient->getMDIChildFirst();

             mdichild = mdichild->getMDINext();

             and so on.

           The FXMDIChild windows are linked into a doubly-linked list.

         - After a change, FXMDIChild window contents may be updated by a simple
           message broadcast to all FXMDIChild windows; in many cases, the
           original FXMDIChild with which the user interacted should be skipped.
           This can be accomplished by simply passing the same sender which
           sent the change to the FXDocument back to the broadcast message,
           so if a message is received by the FXMDIChild it could simply test
           if it itself was the originator.

         - Applications which don't ``buy into'' this whole multi-document business
           but still want to have multiple viewer windows won't have to
           bother with FXDocuments at all!

           Just create FXMDIChild widgets and make them send the messages to the
           right place...


    6) FXMainWindow and FXMDIChild can ask their target to supply the title to be
       displayed in the titlebar; this is done through GUI Updating.

    7) New FXMDIChildren should be created by FXDocument; when an FXMDIChild is closed,
       the FXDocument should be asked first; if its the last window, FXDocument
       should prompt to save the data if modified. After data has been saved, the
       FXDocument returns 1 in the SEL_CLOSE handler which causes the FXMDIChild
       to delete itself.

    8) A message ID_CLOSE_DOCUMENT to FXDocument (via FXMDIClient and FXMDIChild) checks
       if document needs to be saved; once saved, the document can delete all child
       windows.  It could do this via:

         mdiclient->forallDocWindows(this,this,FXSEL(SEL_COMMAND,FXWindow::ID_DELETE),NULL);

       forAllfirst sends a SEL_CLOSEALL
       to each document; if this returns 1, FXMDIClient
       proceeds to send ID_MDI_CLOSE to each FXMDIChild belonging to the same document.
       Each FXMDIChild will then ask the FXDocument whether its OK to close this
       FXMDIChild; since the FXDocument was already saved in response to the SEL_CLOSEALL,
       the FXDocument is no longer dirty and the answer will be OK to close for all of
       them [except perhaps in unusual circumstances].


*/

using namespace FX;

/*******************************************************************************/

namespace FX {


// Map
FXDEFMAP(FXDocument) FXDocumentMap[]={
  FXMAPFUNC(SEL_UPDATE,FXDocument::ID_TITLE,FXDocument::onUpdTitle),
  FXMAPFUNC(SEL_UPDATE,FXDocument::ID_FILENAME,FXDocument::onUpdFilename),
  };


// Object implementation
FXIMPLEMENT(FXDocument,FXObject,FXDocumentMap,ARRAYNUMBER(FXDocumentMap))


// Construct
FXDocument::FXDocument(){
  modified=FALSE;
  }


// Change title
void FXDocument::setTitle(const FXString& name){
  title=name;
  }


// Set file name, and the title also
void FXDocument::setFilename(const FXString& path){
  filename=FXPath::absolute(path);
  title=FXPath::title(filename);
  }


// Update document title
long FXDocument::onUpdTitle(FXObject* sender,FXSelector,void*){
  FXString string=title;
  if(modified) string+="*";
  sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETSTRINGVALUE),(void*)&string);
  return 1;
  }


// Update document filename
long FXDocument::onUpdFilename(FXObject* sender,FXSelector,void*){
  FXString string=filename;
  if(modified) string+="*";
  sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETSTRINGVALUE),(void*)&string);
  return 1;
  }


// Save object to stream
void FXDocument::save(FXStream& store) const {
  FXObject::save(store);
  }


// Load object from stream
void FXDocument::load(FXStream& store){
  FXObject::load(store);
  }


// Destruct
FXDocument::~FXDocument(){
  }

}
