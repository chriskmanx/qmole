/********************************************************************************
*                                                                               *
*          M u l t i p l e   D o c u m e n t   C l i e n t   W i n d o w        *
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
* $Id: FXMDIClient.cpp,v 1.62.2.2 2007/10/25 15:08:51 fox Exp $                     *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXThread.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXObjectList.h"
#include "FXRegistry.h"
#include "FXAccelTable.h"
#include "FXApp.h"
#include "FXId.h"
#include "FXDrawable.h"
#include "FXImage.h"
#include "FXIcon.h"
#include "FXWindow.h"
#include "FXFrame.h"
#include "FXLabel.h"
#include "FXButton.h"
#include "FXMenuButton.h"
#include "FXComposite.h"
#include "FXShell.h"
#include "FXPopup.h"
#include "FXMenuPane.h"
#include "FXMDIButton.h"
#include "FXPacker.h"
#include "FXHorizontalFrame.h"
#include "FXToolBar.h"
#include "FXMenuBar.h"
#include "FXMDIChild.h"
#include "FXMDIClient.h"
#include "FXDialogBox.h"
#include "FXVerticalFrame.h"
#include "FXList.h"


/*
  Notes:
  - This brings up the question of the cascade design.  Do you want the windows
    to be cascaded/tiled in the order that they were created, or do you want them
    to be cascaded in some sort of focus order, with the one that had the focus
    to be on top,  keeping its focus.
    In the test app, if you click within any of the child windows, it pops to
    the top with the focus, with the exception of "TEST3", which has a button.
    If this button is clicked, that window dos not pop to the top when it
    gets the focus.  Seems like it should...
  - Minor problems with mdi.  When a window is maximized and then
    deleted, the next window that is created is created in "normal" mode.
    When any adjustment is made to the size of the new window, it pops to
    "almost" maximized mode.  (The child frame is visible, but as large as
    possible).  Seems like the child windows should be created maximized if
    the mdi is in maximized mode.  Also, if there are two windows and one
    gets maximized and then deleted, the second window pops to "almost"
    maximixed mode.
  - We make MDIClient get a first crack at the messages, so that the MDIChild
    can not shadow any messages really directed at the MDIClient.
  - Need ``arrange icons'' feature.
  - When switching active MDIChild windows, we pass the old to the new and vice
    versa; this allows the MDIChild's target to determine if we switched windows
    only, or if we switched between one document and another at the same time
*/

#define HORZ_PAD      12
#define VERT_PAD      2

#define CASCADE_XOFF  24
#define CASCADE_YOFF  24

#define CLIENT_MIN_WIDTH  16
#define CLIENT_MIN_HEIGHT 16


using namespace FX;

/*******************************************************************************/

namespace FX {


FXDEFMAP(FXMDIClient) FXMDIClientMap[]={
  FXMAPFUNC(SEL_UPDATE,FXWindow::ID_MDI_NEXT,FXMDIClient::onUpdActivateNext),
  FXMAPFUNC(SEL_UPDATE,FXWindow::ID_MDI_PREV,FXMDIClient::onUpdActivatePrev),
  FXMAPFUNC(SEL_UPDATE,FXWindow::ID_MDI_TILEHORIZONTAL,FXMDIClient::onUpdTileHorizontal),
  FXMAPFUNC(SEL_UPDATE,FXWindow::ID_MDI_TILEVERTICAL,FXMDIClient::onUpdTileVertical),
  FXMAPFUNC(SEL_UPDATE,FXWindow::ID_MDI_CASCADE,FXMDIClient::onUpdCascade),
  FXMAPFUNC(SEL_UPDATE,FXWindow::ID_MDI_CLOSE,FXMDIClient::onUpdClose),
  FXMAPFUNC(SEL_UPDATE,FXWindow::ID_MDI_MINIMIZE,FXMDIClient::onUpdMinimize),
  FXMAPFUNC(SEL_UPDATE,FXWindow::ID_MDI_RESTORE,FXMDIClient::onUpdRestore),
  FXMAPFUNC(SEL_UPDATE,FXWindow::ID_MDI_MAXIMIZE,FXMDIClient::onUpdMaximize),
  FXMAPFUNC(SEL_UPDATE,FXWindow::ID_MDI_MENURESTORE,FXMDIClient::onUpdMenuRestore),
  FXMAPFUNC(SEL_UPDATE,FXWindow::ID_MDI_MENUCLOSE,FXMDIClient::onUpdMenuClose),
  FXMAPFUNC(SEL_UPDATE,FXWindow::ID_MDI_MENUMINIMIZE,FXMDIClient::onUpdMenuMinimize),
  FXMAPFUNC(SEL_UPDATE,FXWindow::ID_MDI_MENUWINDOW,FXMDIClient::onUpdMenuWindow),
  FXMAPFUNC(SEL_UPDATE,FXMDIClient::ID_MDI_ANY,FXMDIClient::onUpdAnyWindows),
  FXMAPFUNCS(SEL_UPDATE,FXMDIClient::ID_MDI_1,FXMDIClient::ID_MDI_10,FXMDIClient::onUpdWindowSelect),
  FXMAPFUNCS(SEL_UPDATE,FXMDIClient::ID_MDI_OVER_1,FXMDIClient::ID_MDI_OVER_10,FXMDIClient::onUpdOthersWindows),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_MDI_NEXT,FXMDIClient::onCmdActivateNext),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_MDI_PREV,FXMDIClient::onCmdActivatePrev),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_MDI_TILEHORIZONTAL,FXMDIClient::onCmdTileHorizontal),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_MDI_TILEVERTICAL,FXMDIClient::onCmdTileVertical),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_MDI_CASCADE,FXMDIClient::onCmdCascade),
  FXMAPFUNCS(SEL_COMMAND,FXMDIClient::ID_MDI_1,FXMDIClient::ID_MDI_10,FXMDIClient::onCmdWindowSelect),
  FXMAPFUNCS(SEL_COMMAND,FXMDIClient::ID_MDI_OVER_1,FXMDIClient::ID_MDI_OVER_10,FXMDIClient::onCmdOthersWindows),
  };


// Object implementation
FXIMPLEMENT(FXMDIClient,FXComposite,FXMDIClientMap,ARRAYNUMBER(FXMDIClientMap))


// Construct and init
FXMDIClient::FXMDIClient(){
  flags|=FLAG_SHOWN;
  active=NULL;
  cascadex=CASCADE_XOFF;
  cascadey=CASCADE_YOFF;
  }


// Construct and init
FXMDIClient::FXMDIClient(FXComposite* p,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXComposite(p,opts,x,y,w,h){
  flags|=FLAG_SHOWN;
  backColor=getApp()->getShadowColor();
  active=NULL;
  cascadex=CASCADE_XOFF;
  cascadey=CASCADE_YOFF;
  }


// Get width
FXint FXMDIClient::getDefaultWidth(){
  return CLIENT_MIN_WIDTH;
  }


// Get height
FXint FXMDIClient::getDefaultHeight(){
  return CLIENT_MIN_HEIGHT;
  }


// Recalculate layout
void FXMDIClient::layout(){
  register FXMDIChild* child;
  register FXint xx,yy,ww,hh;

  // Place children
  for(child=(FXMDIChild*)getFirst(); child; child=(FXMDIChild*)child->getNext()){
    if(child->shown()){
      if(child->isMaximized()){
        xx=0;
        yy=0;
        ww=width;
        hh=height;
        }
      else if(child->isMinimized()){
        xx=child->getX();
        yy=child->getY();
        ww=child->getDefaultWidth();
        hh=child->getDefaultHeight();
        }
      else{
        xx=child->getX();
        yy=child->getY();
        ww=child->getWidth();
        hh=child->getHeight();
        }
      child->position(xx,yy,ww,hh);
      }
    }

  // Raise active child
  if(active && active->shown()) active->raise();

  // No more dirty
  flags&=~FLAG_DIRTY;
  }


// Cascade windows
void FXMDIClient::cascade(FXbool notify){
  register FXMDIChild* child;
  FXint childx,childy,childw,childh;
  childx=5;
  childy=5;
  childw=(2*width)/3;
  childh=(2*height)/3;
  for(child=(FXMDIChild*)getFirst(); child; child=(FXMDIChild*)child->getNext()){
    if(child==active) continue;
    if(child->shown() && !child->isMinimized()){
      child->restore(notify);
      child->position(childx,childy,childw,childh);
      child->raise();
      childx+=cascadex;
      childy+=cascadey;
      if(childx+child->getWidth()>width){ childx=5; childy=5; }
      if(childy+child->getHeight()>height){ childy=5; }
      }
    }
  if(active && active->shown() && !active->isMinimized()){
    active->restore(notify);
    active->position(childx,childy,childw,childh);
    active->raise();
    }
  }


// Layout horizontally
void FXMDIClient::horizontal(FXbool notify){
  register FXMDIChild* child;
  register FXint n,nr,nc,hroom,vroom,r,c;
  for(n=0,child=(FXMDIChild*)getFirst(); child; child=(FXMDIChild*)child->getNext()){
    if(child->shown() && !child->isMinimized()) n++;
    }
  nr=n;
  nc=1;
  if(n>3){
    nc=(int)sqrt((double)n);
    nr=(n+nc-1)/nc;
    }
  hroom=0;
  vroom=0;
  if(nc>0) hroom=width/nc;
  if(nr>0) vroom=height/nr;
  for(child=(FXMDIChild*)getFirst(),n=0; child; child=(FXMDIChild*)child->getNext()){
    if(child->shown() && !child->isMinimized()){
      r=n/nc;
      c=n%nc;
      child->restore(notify);
      child->position(c*hroom,r*vroom,hroom,vroom);
      n++;
      }
    }
  if(active && active->shown()) active->raise();
  }


// Layout vertically
void FXMDIClient::vertical(FXbool notify){
  register FXMDIChild* child;
  register FXint n,nr,nc,hroom,vroom,r,c;
  for(n=0,child=(FXMDIChild*)getFirst(); child; child=(FXMDIChild*)child->getNext()){
    if(child->shown() && !child->isMinimized()) n++;
    }
  nc=n;
  nr=1;
  if(n>3){
    nr=(int)sqrt((double)n);
    nc=(n+nr-1)/nr;
    }
  hroom=0;
  vroom=0;
  if(nc>0) hroom=width/nc;
  if(nr>0) vroom=height/nr;
  for(child=(FXMDIChild*)getFirst(),n=0; child; child=(FXMDIChild*)child->getNext()){
    if(child->shown() && !child->isMinimized()){
      r=n/nc;
      c=n%nc;
      child->restore(notify);
      child->position(c*hroom,r*vroom,hroom,vroom);
      n++;
      }
    }
  if(active && active->shown()) active->raise();
  }


// User clicks on one of the window menus
long FXMDIClient::onCmdWindowSelect(FXObject*,FXSelector sel,void*){
  setActiveChild((FXMDIChild*)childAtIndex(FXSELID(sel)-ID_MDI_1),TRUE);
  return 1;
  }


// Update handler for window menus
long FXMDIClient::onUpdWindowSelect(FXObject *sender,FXSelector sel,void*){
  FXint which=FXSELID(sel)-ID_MDI_1;
  FXMDIChild *child=(FXMDIChild*)childAtIndex(which);
  if(child){
    FXString string;
    if(which<9)
      string.format("&%d %s",which+1,child->getTitle().text());
    else
      string.format("1&0 %s",child->getTitle().text());
    sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETSTRINGVALUE),(void*)&string);
    sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SHOW),NULL);
    if(child==active)
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_CHECK),NULL);
    else
      sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_UNCHECK),NULL);
    }
  else{
    sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_HIDE),NULL);
    }
  return 1;
  }



// Show a menu of other MDI child windows
long FXMDIClient::onCmdOthersWindows(FXObject*,FXSelector,void*){
  FXDialogBox choose(this,tr("Select Window"),DECOR_TITLE|DECOR_BORDER|DECOR_RESIZE,0,0,300,200,10,10,10,10, 10,10);
  FXHorizontalFrame* buttons=new FXHorizontalFrame(&choose,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|PACK_UNIFORM_WIDTH|PACK_UNIFORM_HEIGHT,0,0,0,0,0,0,0,0);
  new FXButton(buttons,tr("&OK"),NULL,&choose,FXDialogBox::ID_ACCEPT,BUTTON_INITIAL|BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
  new FXButton(buttons,tr("&Cancel"),NULL,&choose,FXDialogBox::ID_CANCEL,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
  FXVerticalFrame* mdilistframe=new FXVerticalFrame(&choose,FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0,0,0,0,0,0,0);
  FXList* mdilist=new FXList(mdilistframe,NULL,0,LIST_BROWSESELECT|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  mdilist->setNumVisible(10);
  for(FXMDIChild *child=(FXMDIChild*)getFirst(); child; child=(FXMDIChild*)child->getNext()){
    mdilist->appendItem(child->getTitle(),child->getIcon(),child);
    if(child==active) mdilist->setCurrentItem(mdilist->getNumItems()-1);
    }
  if(choose.execute(PLACEMENT_OWNER)){
    FXASSERT(mdilist->getCurrentItem()>=0);
    setActiveChild((FXMDIChild*)mdilist->getItemData(mdilist->getCurrentItem()));
    }
  return 1;
  }


// Update button to show menu of other MDI child windows when more than N windows
long FXMDIClient::onUpdOthersWindows(FXObject *sender,FXSelector sel,void*){
  sender->handle(this,((FXSELID(sel)-ID_MDI_OVER_1)<numChildren())?FXSEL(SEL_COMMAND,FXWindow::ID_SHOW):FXSEL(SEL_COMMAND,FXWindow::ID_HIDE),NULL);
  return 1;
  }


// Show or hide depending on whether there are any windows
long FXMDIClient::onUpdAnyWindows(FXObject *sender,FXSelector,void*){
  sender->handle(this,getFirst()?FXSEL(SEL_COMMAND,ID_SHOW):FXSEL(SEL_COMMAND,ID_HIDE),NULL);
  return 1;
  }


// Update restore; gray if no active
long FXMDIClient::onUpdRestore(FXObject* sender,FXSelector sel,void* ptr){
  if(active) return active->handle(sender,sel,ptr);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Update maximized; gray if no active
long FXMDIClient::onUpdMaximize(FXObject* sender,FXSelector sel,void* ptr){
  if(active) return active->handle(sender,sel,ptr);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Update minimized
long FXMDIClient::onUpdMinimize(FXObject* sender,FXSelector sel,void* ptr){
  if(active) return active->handle(sender,sel,ptr);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Update close active child
long FXMDIClient::onUpdClose(FXObject* sender,FXSelector sel,void* ptr){
  if(active) return active->handle(sender,sel,ptr);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Update menu's restore button
long FXMDIClient::onUpdMenuWindow(FXObject* sender,FXSelector sel,void* ptr){
  if(active) return active->handle(sender,sel,ptr);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_HIDE),NULL);
  return 1;
  }


// Update menu's restore button
long FXMDIClient::onUpdMenuRestore(FXObject* sender,FXSelector sel,void* ptr){
  if(active) return active->handle(sender,sel,ptr);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_HIDE),NULL);
  return 1;
  }


// Update menu's minimized button
long FXMDIClient::onUpdMenuMinimize(FXObject* sender,FXSelector sel,void* ptr){
  if(active) return active->handle(sender,sel,ptr);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_HIDE),NULL);
  return 1;
  }


// Update menu's close button
long FXMDIClient::onUpdMenuClose(FXObject* sender,FXSelector sel,void* ptr){
  if(active) return active->handle(sender,sel,ptr);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_HIDE),NULL);
  return 1;
  }


// Set the active child
FXbool FXMDIClient::setActiveChild(FXMDIChild* child,FXbool notify){
  FXbool wasmax=FALSE;
  if(active!=child){

    if(active){

      // Was it maximized?
      wasmax=active->isMaximized();

      // Deactivate old MDIChild
      active->handle(this,FXSEL(SEL_DESELECTED,0),(void*)child);     // FIXME should call member function

      // Restore to normal size if it was maximized
      if(wasmax) active->restore(notify);
      }

    if(child){

      // Activate new MDIChild
      child->handle(this,FXSEL(SEL_SELECTED,0),(void*)active);     // FIXME should call member function

      // Maximize because the old MDIChild was maximized
      if(wasmax) child->maximize(notify);

      // Raise it
      child->raise();
      }

    active=child;

    // Need layout
    recalc();

    // GUI update will be needed
    getApp()->refresh();

    // Notify target
    if(notify && target){ target->tryHandle(this,FXSEL(SEL_CHANGED,message),child); }

    return TRUE;
    }
  return FALSE;
  }


// Tile horizontally (actually, prefer wider windows)
long FXMDIClient::onCmdTileHorizontal(FXObject*,FXSelector,void*){
  horizontal(TRUE);
  return 1;
  }


// Update tile horizontally
long FXMDIClient::onUpdTileHorizontal(FXObject* sender,FXSelector,void*){
  sender->handle(this,getFirst()?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Tile vertically (actually, prefer taller windows)
long FXMDIClient::onCmdTileVertical(FXObject*,FXSelector,void*){
  vertical(TRUE);
  return 1;
  }


// Update tile vertically
long FXMDIClient::onUpdTileVertical(FXObject* sender,FXSelector,void*){
  sender->handle(this,getFirst()?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Cascade windows
long FXMDIClient::onCmdCascade(FXObject*,FXSelector,void*){
  cascade(TRUE);
  return 1;
  }


// Update cascade
long FXMDIClient::onUpdCascade(FXObject* sender,FXSelector,void*){
  sender->handle(this,getFirst()?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Pass message to all MDI windows; the crufty loop is because
// it is possible for the child receiving the message to be deleted
long FXMDIClient::forallWindows(FXObject* sender,FXSelector sel,void* ptr){
  register FXWindow *child,*nextchild;
  for(child=getFirst(); child; child=nextchild){
    nextchild=child->getNext();
    if(!child->handle(sender,sel,ptr)) return 0;
    }
  return 1;
  }


// Pass message to all different documents; here the complication
// is that the whole group of child windows sharing the same
// document may be deleted; also, we want to send only ONE of the
// document-sharing children a message.
long FXMDIClient::forallDocuments(FXObject* sender,FXSelector sel,void* ptr){
  register FXWindow *child,*nextchild,*ch;
  for(child=getFirst(); child; child=nextchild){
    nextchild=child->getNext();
x:  if(nextchild && nextchild->getTarget()){
      for(ch=child; ch; ch=ch->getPrev()){
        if(ch->getTarget()==nextchild->getTarget()){
          nextchild=nextchild->getNext();
          goto x;
          }
        }
      }
    if(!child->handle(sender,sel,ptr)) return 0;
    }
  return 1;
  }


// Pass message to all MDI windows whose target is document;
// note that the child may be deleted as a result of the message.
long FXMDIClient::forallDocWindows(FXObject* document,FXObject* sender,FXSelector sel,void* ptr){
  register FXWindow *child,*nextchild;
  for(child=getFirst(); child; child=nextchild){
    nextchild=child->getNext();
    if(child->getTarget()==document){
      if(!child->handle(sender,sel,ptr)) return 0;
      }
    }
  return 1;
  }


// Activate next child
long FXMDIClient::onCmdActivateNext(FXObject*,FXSelector,void*){
  if(active && active->getNext()) setActiveChild((FXMDIChild*)active->getNext(),TRUE);
  return 1;
  }


// Activate next child
long FXMDIClient::onUpdActivateNext(FXObject* sender,FXSelector,void*){
  sender->handle(this,(active && active->getNext())?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Activate previous child
long FXMDIClient::onCmdActivatePrev(FXObject*,FXSelector,void*){
  if(active && active->getPrev()) setActiveChild((FXMDIChild*)active->getPrev(),TRUE);
  return 1;
  }


// Activate previous child
long FXMDIClient::onUpdActivatePrev(FXObject* sender,FXSelector,void*){
  sender->handle(this,(active && active->getPrev())?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Delegate all other messages to active child; we can't block any
// messages here as some are meant fot the FXMDIChild and some for
// the FXMDIChild's content or target.
long FXMDIClient::onDefault(FXObject* sender,FXSelector sel,void* ptr){
  return active && active->handle(sender,sel,ptr);
  }


// Save object to stream
void FXMDIClient::save(FXStream& store) const {
  FXComposite::save(store);
  store << active;
  store << cascadex;
  store << cascadey;
  }


// Load object from stream
void FXMDIClient::load(FXStream& store){
  FXComposite::load(store);
  store >> active;
  store >> cascadex;
  store >> cascadey;
  }


// Destruct thrashes object
FXMDIClient::~FXMDIClient(){
  active=(FXMDIChild*)-1L;
  }

}
