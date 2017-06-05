/********************************************************************************
*                                                                               *
*                     D i r e c t o r y   L i s t   O b j e c t                 *
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
* $Id: FXDirList.cpp,v 1.160.2.3 2006/11/22 15:40:27 fox Exp $                      *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxascii.h"
#include "fxkeys.h"
#include "FXHash.h"
#include "FXThread.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXPath.h"
#include "FXStat.h"
#include "FXFile.h"
#include "FXDir.h"
#include "FXURL.h"
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXApp.h"
#include "FXFont.h"
#include "FXIcon.h"
#include "FXBMPIcon.h"
#include "FXGIFIcon.h"
#include "FXScrollBar.h"
#include "FXDirList.h"
#include "FXFileDict.h"
#ifdef WIN32
#include <shellapi.h>
#endif
#include "icons.h"

/*
  Notes:
  - One can never create items in constructor:- createItem() may be overloaded!
  - Instead of FXTreeItems, callbacks should pass pointer to directory?
  - Clipboard of a filenames.
  - Should do drag and drop and such.
  - Clipboard, DND, etc. support.
  - We should NOT assume the root's name is just '/'.  It could be C:\ etc.
  - Try read icons from <path>/.dir.gif and <path>/.opendir.gif!
  - Special icon for root.
  - We should generate SEL_INSERTED, SEL_DELETED, SEL_REPLACED, SEL_CHANGED
    messages as the FXDirList updates itself from the file system.
  - Under MS-Windows, always pass FILEMATCH_NOESCAPE setting match mode!
  - Every once in a while, even if checking doesn't reveal a change,
    refresh the entire list anyway!
*/


#define REFRESHINTERVAL     1000        // Interval between refreshes
#define REFRESHFREQUENCY    30          // File systems not supporting mod-time, refresh every nth time

using namespace FX;

/*******************************************************************************/

namespace FX {


// Object implementation
FXIMPLEMENT(FXDirItem,FXTreeItem,NULL,0)


// Map
FXDEFMAP(FXDirList) FXDirListMap[]={
  FXMAPFUNC(SEL_DRAGGED,0,FXDirList::onDragged),
  FXMAPFUNC(SEL_TIMEOUT,FXDirList::ID_REFRESHTIMER,FXDirList::onRefreshTimer),
  FXMAPFUNC(SEL_DND_ENTER,0,FXDirList::onDNDEnter),
  FXMAPFUNC(SEL_DND_LEAVE,0,FXDirList::onDNDLeave),
  FXMAPFUNC(SEL_DND_DROP,0,FXDirList::onDNDDrop),
  FXMAPFUNC(SEL_DND_MOTION,0,FXDirList::onDNDMotion),
  FXMAPFUNC(SEL_DND_REQUEST,0,FXDirList::onDNDRequest),
  FXMAPFUNC(SEL_BEGINDRAG,0,FXDirList::onBeginDrag),
  FXMAPFUNC(SEL_ENDDRAG,0,FXDirList::onEndDrag),
  FXMAPFUNC(SEL_UPDATE,FXDirList::ID_SHOW_HIDDEN,FXDirList::onUpdShowHidden),
  FXMAPFUNC(SEL_UPDATE,FXDirList::ID_HIDE_HIDDEN,FXDirList::onUpdHideHidden),
  FXMAPFUNC(SEL_UPDATE,FXDirList::ID_TOGGLE_HIDDEN,FXDirList::onUpdToggleHidden),
  FXMAPFUNC(SEL_UPDATE,FXDirList::ID_SHOW_FILES,FXDirList::onUpdShowFiles),
  FXMAPFUNC(SEL_UPDATE,FXDirList::ID_HIDE_FILES,FXDirList::onUpdHideFiles),
  FXMAPFUNC(SEL_UPDATE,FXDirList::ID_TOGGLE_FILES,FXDirList::onUpdToggleFiles),
  FXMAPFUNC(SEL_UPDATE,FXDirList::ID_SET_PATTERN,FXDirList::onUpdSetPattern),
  FXMAPFUNC(SEL_UPDATE,FXDirList::ID_SORT_REVERSE,FXDirList::onUpdSortReverse),
  FXMAPFUNC(SEL_UPDATE,FXDirList::ID_SORT_CASE,FXDirList::onUpdSortCase),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_SETVALUE,FXDirList::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_SETSTRINGVALUE,FXDirList::onCmdSetStringValue),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_GETSTRINGVALUE,FXDirList::onCmdGetStringValue),
  FXMAPFUNC(SEL_COMMAND,FXDirList::ID_SHOW_HIDDEN,FXDirList::onCmdShowHidden),
  FXMAPFUNC(SEL_COMMAND,FXDirList::ID_HIDE_HIDDEN,FXDirList::onCmdHideHidden),
  FXMAPFUNC(SEL_COMMAND,FXDirList::ID_TOGGLE_HIDDEN,FXDirList::onCmdToggleHidden),
  FXMAPFUNC(SEL_COMMAND,FXDirList::ID_SHOW_FILES,FXDirList::onCmdShowFiles),
  FXMAPFUNC(SEL_COMMAND,FXDirList::ID_HIDE_FILES,FXDirList::onCmdHideFiles),
  FXMAPFUNC(SEL_COMMAND,FXDirList::ID_TOGGLE_FILES,FXDirList::onCmdToggleFiles),
  FXMAPFUNC(SEL_COMMAND,FXDirList::ID_SET_PATTERN,FXDirList::onCmdSetPattern),
  FXMAPFUNC(SEL_COMMAND,FXDirList::ID_SORT_REVERSE,FXDirList::onCmdSortReverse),
  FXMAPFUNC(SEL_COMMAND,FXDirList::ID_SORT_CASE,FXDirList::onCmdSortCase),
  FXMAPFUNC(SEL_COMMAND,FXDirList::ID_REFRESH,FXDirList::onCmdRefresh),
  };


// Object implementation
FXIMPLEMENT(FXDirList,FXTreeList,FXDirListMap,ARRAYNUMBER(FXDirListMap))


// For serialization
FXDirList::FXDirList(){
  dropEnable();
  sortfunc=ascendingCase;
  associations=NULL;
  list=NULL;
  matchmode=0;
  counter=0;
  dropaction=DRAG_MOVE;
  open_folder=NULL;
  closed_folder=NULL;
  mini_doc=NULL;
  mini_app=NULL;
  cdromicon=NULL;
  harddiskicon=NULL;
  networkicon=NULL;
  floppyicon=NULL;
  zipdiskicon=NULL;
  }


// Directory List Widget
FXDirList::FXDirList(FXComposite *p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXTreeList(p,tgt,sel,opts,x,y,w,h),pattern("*"){
  dropEnable();
  open_folder=new FXGIFIcon(getApp(),minifolderopen);
  closed_folder=new FXGIFIcon(getApp(),minifolder);
  mini_doc=new FXGIFIcon(getApp(),minidoc);
  mini_app=new FXGIFIcon(getApp(),miniapp);
  cdromicon=new FXGIFIcon(getApp(),minicdrom);
  harddiskicon=new FXGIFIcon(getApp(),miniharddisk);
  networkicon=new FXGIFIcon(getApp(),mininetdrive);
  floppyicon=new FXGIFIcon(getApp(),minifloppy);
  zipdiskicon=new FXGIFIcon(getApp(),minizipdrive);
#ifdef WIN32
  matchmode=FILEMATCH_FILE_NAME|FILEMATCH_NOESCAPE|FILEMATCH_CASEFOLD;
#else
  matchmode=FILEMATCH_FILE_NAME|FILEMATCH_NOESCAPE;
#endif
  associations=NULL;
  if(!(options&DIRLIST_NO_OWN_ASSOC)) associations=new FXFileDict(getApp());
  list=NULL;
  sortfunc=ascendingCase;
  dropaction=DRAG_MOVE;
  counter=0;
  }


// Create X window
void FXDirList::create(){
  if(!id()) getApp()->addTimeout(this,ID_REFRESHTIMER,REFRESHINTERVAL);
  FXTreeList::create();
  if(!deleteType){deleteType=getApp()->registerDragType(deleteTypeName);}
  if(!urilistType){urilistType=getApp()->registerDragType(urilistTypeName);}
  open_folder->create();
  closed_folder->create();
  mini_doc->create();
  mini_app->create();
  cdromicon->create();
  harddiskicon->create();
  networkicon->create();
  floppyicon->create();
  zipdiskicon->create();
  scan(FALSE);
  }


// Detach disconnects the icons
void FXDirList::detach(){
  if(id()) getApp()->removeTimeout(this,ID_REFRESHTIMER);
  FXTreeList::detach();
  open_folder->detach();
  closed_folder->detach();
  mini_doc->detach();
  mini_app->detach();
  cdromicon->detach();
  harddiskicon->detach();
  networkicon->detach();
  floppyicon->detach();
  zipdiskicon->detach();
  deleteType=0;
  urilistType=0;
  }


// Destroy zaps the icons
void FXDirList::destroy(){
  if(id()) getApp()->removeTimeout(this,ID_REFRESHTIMER);
  FXTreeList::destroy();
  open_folder->destroy();
  closed_folder->destroy();
  mini_doc->destroy();
  mini_app->destroy();
  }


// Create item
FXTreeItem* FXDirList::createItem(const FXString& text,FXIcon* oi,FXIcon* ci,void* ptr){
  return (FXTreeItem*) new FXDirItem(text,oi,ci,ptr);
  }


// Sort ascending order, keeping directories first
FXint FXDirList::ascending(const FXTreeItem* pa,const FXTreeItem* pb){
  register const FXDirItem *a=(FXDirItem*)pa;
  register const FXDirItem *b=(FXDirItem*)pb;
  register FXint diff=(FXint)b->isDirectory() - (FXint)a->isDirectory();
  return diff ? diff : compare(a->label,b->label);
  }


// Sort descending order, keeping directories first
FXint FXDirList::descending(const FXTreeItem* pa,const FXTreeItem* pb){
  register const FXDirItem *a=(FXDirItem*)pa;
  register const FXDirItem *b=(FXDirItem*)pb;
  register FXint diff=(FXint)b->isDirectory() - (FXint)a->isDirectory();
  return diff ? diff : compare(b->label,a->label);
  }


// Sort ascending order, case insensitive, keeping directories first
FXint FXDirList::ascendingCase(const FXTreeItem* pa,const FXTreeItem* pb){
  register const FXDirItem *a=(FXDirItem*)pa;
  register const FXDirItem *b=(FXDirItem*)pb;
  register FXint diff=(FXint)b->isDirectory() - (FXint)a->isDirectory();
  return diff ? diff : comparecase(a->label,b->label);
  }


// Sort descending order, case insensitive, keeping directories first
FXint FXDirList::descendingCase(const FXTreeItem* pa,const FXTreeItem* pb){
  register const FXDirItem *a=(FXDirItem*)pa;
  register const FXDirItem *b=(FXDirItem*)pb;
  register FXint diff=(FXint)b->isDirectory() - (FXint)a->isDirectory();
  return diff ? diff : comparecase(b->label,a->label);
  }

/*******************************************************************************/


// Handle drag-and-drop enter
long FXDirList::onDNDEnter(FXObject* sender,FXSelector sel,void* ptr){
  FXTreeList::onDNDEnter(sender,sel,ptr);
  return 1;
  }


// Handle drag-and-drop leave
long FXDirList::onDNDLeave(FXObject* sender,FXSelector sel,void* ptr){
  stopAutoScroll();
  FXTreeList::onDNDLeave(sender,sel,ptr);
  return 1;
  }


// Handle drag-and-drop motion
long FXDirList::onDNDMotion(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent *event=(FXEvent*)ptr;
  FXTreeItem *item;

  // Start autoscrolling
  if(startAutoScroll(event,FALSE)) return 1;

  // Give base class a shot
  if(FXTreeList::onDNDMotion(sender,sel,ptr)) return 1;

  // Dropping list of filenames
  if(offeredDNDType(FROM_DRAGNDROP,urilistType)){

    // Locate drop place
    item=getItemAt(event->win_x,event->win_y);

    // We can drop in a directory
    if(item && isItemDirectory(item)){

      // Get drop directory
      dropdirectory=getItemPathname(item);

      // What is being done (move,copy,link)
      dropaction=inquireDNDAction();

      // See if this is writable
      if(FXStat::isWritable(dropdirectory)){
        FXTRACE((100,"accepting drop on %s\n",dropdirectory.text()));
        acceptDrop(DRAG_ACCEPT);
        }
      }
    return 1;
    }
  return 0;
  }


// Handle drag-and-drop drop
long FXDirList::onDNDDrop(FXObject* sender,FXSelector sel,void* ptr){
  FXString dropfiles,filesrc,filedst,url;
  FXint beg,end;

  // Stop scrolling
  stopAutoScroll();

  // Perhaps target wants to deal with it
  if(FXTreeList::onDNDDrop(sender,sel,ptr)) return 1;

  // Get uri-list of files being dropped
  if(getDNDData(FROM_DRAGNDROP,urilistType,dropfiles)){

    // Tell drag source we got it
    dropFinished(DRAG_ACCEPT);

    // Loop over urls
    for(beg=0; beg<dropfiles.length(); beg=end+2){
      if((end=dropfiles.find_first_of("\r\n",beg))<0) end=dropfiles.length();

      // File url
      url=dropfiles.mid(beg,end-beg);

      // Source filename
      filesrc=FXURL::decode(FXURL::fileFromURL(url));

      // Destination filename
      filedst=dropdirectory+PATHSEPSTRING+FXPath::name(filesrc);
      // Move, Copy, or Link as appropriate
      if(dropaction==DRAG_MOVE){
        FXTRACE((100,"Moving file: %s to %s\n",filesrc.text(),filedst.text()));
        if(!FXFile::moveFiles(filesrc,filedst)) getApp()->beep();
        }
      else if(dropaction==DRAG_COPY){
        FXTRACE((100,"Copying file: %s to %s\n",filesrc.text(),filedst.text()));
        if(!FXFile::copyFiles(filesrc,filedst)) getApp()->beep();
        }
      else if(dropaction==DRAG_LINK){
        FXTRACE((100,"Linking file: %s to %s\n",filesrc.text(),filedst.text()));
        if(!FXFile::symlink(filesrc,filedst)) getApp()->beep();
        }
      }
    return 1;
    }

  return 0;
  }


// Somebody wants our dragged data
long FXDirList::onDNDRequest(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent *event=(FXEvent*)ptr;

  // Perhaps the target wants to supply its own data
  if(FXTreeList::onDNDRequest(sender,sel,ptr)) return 1;

  // Return list of filenames as a uri-list
  if(event->target==urilistType){
    setDNDData(FROM_DRAGNDROP,event->target,dragfiles);
    return 1;
    }

  // Delete selected files
  if(event->target==deleteType){
    FXTRACE((100,"Delete files not yet implemented\n"));
    return 1;
    }

  return 0;
  }


// Start a drag operation
long FXDirList::onBeginDrag(FXObject* sender,FXSelector sel,void* ptr){
  register FXTreeItem *item;
  if(FXTreeList::onBeginDrag(sender,sel,ptr)) return 1;
  if(beginDrag(&urilistType,1)){
    dragfiles=FXString::null;
    item=firstitem;
    while(item){
      if(item->isSelected()){
        if(!dragfiles.empty()) dragfiles+="\r\n";
        dragfiles+=FXURL::encode(FXURL::fileToURL(getItemPathname(item)));
        }
      if(item->first){
        item=item->first;
        }
      else{
        while(!item->next && item->parent) item=item->parent;
        item=item->next;
        }
      }
    return 1;
    }
  return 0;
  }


// End drag operation
long FXDirList::onEndDrag(FXObject* sender,FXSelector sel,void* ptr){
  if(FXTreeList::onEndDrag(sender,sel,ptr)) return 1;
  endDrag((didAccept()!=DRAG_REJECT));
  setDragCursor(getDefaultCursor());
  return 1;
  }


// Dragged stuff around
long FXDirList::onDragged(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXDragAction action;
  if(FXTreeList::onDragged(sender,sel,ptr)) return 1;
  action=DRAG_MOVE;
  if(event->state&CONTROLMASK) action=DRAG_COPY;
  if(event->state&SHIFTMASK) action=DRAG_MOVE;
  if(event->state&ALTMASK) action=DRAG_LINK;
  handleDrag(event->root_x,event->root_y,action);
  if(didAccept()!=DRAG_REJECT){
    if(action==DRAG_MOVE)
      setDragCursor(getApp()->getDefaultCursor(DEF_DNDMOVE_CURSOR));
    else if(action==DRAG_LINK)
      setDragCursor(getApp()->getDefaultCursor(DEF_DNDLINK_CURSOR));
    else
      setDragCursor(getApp()->getDefaultCursor(DEF_DNDCOPY_CURSOR));
    }
  else{
    setDragCursor(getApp()->getDefaultCursor(DEF_DNDSTOP_CURSOR));
    }
  return 1;
  }


/*******************************************************************************/


// Open up the path down to the given string
long FXDirList::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setCurrentFile((const FXchar*)ptr);
  return 1;
  }


// Open up the path down to the given string
long FXDirList::onCmdSetStringValue(FXObject*,FXSelector,void* ptr){
  setCurrentFile(*((FXString*)ptr));
  return 1;
  }


// Obtain value of the current item
long FXDirList::onCmdGetStringValue(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getCurrentFile();
  return 1;
  }


// Toggle hidden files
long FXDirList::onCmdToggleHidden(FXObject*,FXSelector,void*){
  showHiddenFiles(!showHiddenFiles());
  return 1;
  }


// Update toggle hidden files widget
long FXDirList::onUpdToggleHidden(FXObject* sender,FXSelector,void*){
  sender->handle(this,showHiddenFiles()?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Show hidden files
long FXDirList::onCmdShowHidden(FXObject*,FXSelector,void*){
  showHiddenFiles(TRUE);
  return 1;
  }


// Update show hidden files widget
long FXDirList::onUpdShowHidden(FXObject* sender,FXSelector,void*){
  sender->handle(this,showHiddenFiles()?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Hide hidden files
long FXDirList::onCmdHideHidden(FXObject*,FXSelector,void*){
  showHiddenFiles(FALSE);
  return 1;
  }


// Update hide hidden files widget
long FXDirList::onUpdHideHidden(FXObject* sender,FXSelector,void*){
  sender->handle(this,showHiddenFiles()?FXSEL(SEL_COMMAND,ID_UNCHECK):FXSEL(SEL_COMMAND,ID_CHECK),NULL);
  return 1;
  }


// Toggle files display
long FXDirList::onCmdToggleFiles(FXObject*,FXSelector,void*){
  showFiles(!showFiles());
  return 1;
  }


// Update toggle files widget
long FXDirList::onUpdToggleFiles(FXObject* sender,FXSelector,void*){
  sender->handle(this,showFiles()?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Show files
long FXDirList::onCmdShowFiles(FXObject*,FXSelector,void*){
  showFiles(TRUE);
  return 1;
  }


// Update show files widget
long FXDirList::onUpdShowFiles(FXObject* sender,FXSelector,void*){
  sender->handle(this,showFiles()?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Hide files
long FXDirList::onCmdHideFiles(FXObject*,FXSelector,void*){
  showFiles(FALSE);
  return 1;
  }


// Update hide files widget
long FXDirList::onUpdHideFiles(FXObject* sender,FXSelector,void*){
  sender->handle(this,showFiles()?FXSEL(SEL_COMMAND,ID_UNCHECK):FXSEL(SEL_COMMAND,ID_CHECK),NULL);
  return 1;
  }


// Change pattern
long FXDirList::onCmdSetPattern(FXObject*,FXSelector,void* ptr){
  setPattern((const char*)ptr);
  return 1;
  }


// Update pattern
long FXDirList::onUpdSetPattern(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETVALUE),(void*)pattern.text());
  return 1;
  }


// Reverse sort order
long FXDirList::onCmdSortReverse(FXObject*,FXSelector,void*){
  if(sortfunc==ascending) sortfunc=descending;
  else if(sortfunc==descending) sortfunc=ascending;
  else if(sortfunc==ascendingCase) sortfunc=descendingCase;
  else if(sortfunc==descendingCase) sortfunc=ascendingCase;
  scan(TRUE);
  return 1;
  }


// Update sender
long FXDirList::onUpdSortReverse(FXObject* sender,FXSelector,void* ptr){
  sender->handle(this,(sortfunc==descending || sortfunc==descendingCase) ? FXSEL(SEL_COMMAND,ID_CHECK) : FXSEL(SEL_COMMAND,ID_UNCHECK),ptr);
  return 1;
  }


// Toggle case sensitivity
long FXDirList::onCmdSortCase(FXObject*,FXSelector,void*){
  if(sortfunc==ascending) sortfunc=ascendingCase;
  else if(sortfunc==descending) sortfunc=descendingCase;
  else if(sortfunc==ascendingCase) sortfunc=ascending;
  else if(sortfunc==descendingCase) sortfunc=descending;
  scan(TRUE);
  return 1;
  }


// Check if case sensitive
long FXDirList::onUpdSortCase(FXObject* sender,FXSelector,void* ptr){
  sender->handle(this,(sortfunc==ascendingCase || sortfunc==descendingCase) ? FXSEL(SEL_COMMAND,ID_CHECK) : FXSEL(SEL_COMMAND,ID_UNCHECK),ptr);
  return 1;
  }


// Expand tree
FXbool FXDirList::expandTree(FXTreeItem* tree,FXbool notify){
  if(FXTreeList::expandTree(tree,notify)){
    if(isItemDirectory(tree)){
      listChildItems((FXDirItem*)tree);
      sortChildItems(tree);
      }
    return TRUE;
    }
  return FALSE;
  }


// Collapse tree
FXbool FXDirList::collapseTree(FXTreeItem* tree,FXbool notify){
  if(FXTreeList::collapseTree(tree,notify)){
    if(isItemDirectory(tree)){
      // As a memory saving feature, all knowledge below this item
      // is deleted; we'll just recreate it when its reexpanded!
      //removeItems(item->first,item->last);
      //recalc();
      }
    return TRUE;
    }
  return FALSE;
  }


// Refresh; don't update if user is interacting with the list
long FXDirList::onRefreshTimer(FXObject*,FXSelector,void*){
  if(flags&FLAG_UPDATE){
    scan(FALSE);
    counter=(counter+1)%REFRESHFREQUENCY;
    }
  getApp()->addTimeout(this,ID_REFRESHTIMER,REFRESHINTERVAL);
  return 0;
  }


// Force an immediate update of the list
long FXDirList::onCmdRefresh(FXObject*,FXSelector,void*){
  scan(TRUE);
  return 1;
  }


// Scan items to see if listing is necessary
void FXDirList::scan(FXbool force){
  FXString   pathname;
  FXDirItem *item;
  FXStat     info;

  // Do root first time
  if(!firstitem || force){
    listRootItems();
    sortRootItems();
    }

  // Check all items
  item=(FXDirItem*)firstitem;
  while(item){

    // Is expanded directory?
    if(item->isDirectory() && item->isExpanded()){

      // Get the full path of the item
      pathname=getItemPathname(item);

      // Stat this directory; should not fail as parent has been scanned already
      FXStat::statFile(pathname,info);

      // Get the mod date of the item
      FXTime newdate=info.touched();

      // Forced, date was changed, or failed to get proper date and counter expired
      if(force || (item->date!=newdate) || (counter==0)){

        // And do the refresh
        listChildItems(item);
        sortChildItems(item);

        // Remember when we did this
        item->date=newdate;
        }

      // Go deeper
      if(item->first){
        item=(FXDirItem*)item->first;
        continue;
        }
      }

    // Go up
    while(!item->next && item->parent){
      item=(FXDirItem*)item->parent;
      }

    // Go to next
    item=(FXDirItem*)item->next;
    }
  }


#ifndef WIN32           // UNIX flavor

// List root directories
void FXDirList::listRootItems(){
  FXDirItem   *item=(FXDirItem*)firstitem;
  FXIcon      *openicon;
  FXIcon      *closedicon;
  FXFileAssoc *fileassoc;

  // First time, make root node
  if(!item) item=list=(FXDirItem*)appendItem(NULL,PATHSEPSTRING,harddiskicon,harddiskicon,NULL,TRUE);

  // Root is a directory, has items under it, and is searchable
  item->state|=FXDirItem::FOLDER|FXDirItem::HASITEMS;
  item->state&=~(FXDirItem::CHARDEV|FXDirItem::BLOCKDEV|FXDirItem::FIFO|FXDirItem::SOCK|FXDirItem::SYMLINK|FXDirItem::EXECUTABLE);

  // Determine associations, icons and type
  fileassoc=NULL;
  openicon=harddiskicon;
  closedicon=harddiskicon;
  if(associations) fileassoc=associations->findDirBinding(PATHSEPSTRING);

  // If association is found, use it
  if(fileassoc){
    if(fileassoc->miniicon) closedicon=fileassoc->miniicon;
    if(fileassoc->miniiconopen) openicon=fileassoc->miniiconopen;
    }

  // Update item information
  item->openIcon=openicon;
  item->closedIcon=closedicon;
  item->size=0L;
  item->assoc=fileassoc;
  item->date=0;

  // Create item
  if(id()) item->create();

  // Need to layout
  recalc();
  }



#else                   // Windows flavor

// List root directories
void FXDirList::listRootItems(){
  FXDirItem      *oldlist,*newlist,**po,**pp,**pn,*item,*link;
  FXIcon         *openicon;
  FXIcon         *closedicon;
  FXFileAssoc    *fileassoc;
  FXString        name;
  DWORD           mask;
  UINT            drivetype;

  // Build new insert-order list
  oldlist=list;
  newlist=NULL;

  // Assemble lists
  po=&oldlist;
  pn=&newlist;

  // Loop over drive letters
  for(mask=GetLogicalDrives(),name="A:\\"; mask; mask>>=1,name[0]++){

    // Skip unavailable drives
    if(!(mask&1)) continue;

    // Find it, and take it out from the old list if found
    for(pp=po; (item=*pp)!=NULL; pp=&item->link){
      if(comparecase(item->label,name)==0){
        *pp=item->link;
        item->link=NULL;
        po=pp;
        goto fnd;
        }
      }

    // Not found; prepend before list
    item=(FXDirItem*)appendItem(NULL,name,open_folder,closed_folder,NULL,TRUE);

    // Next gets hung after this one
fnd:*pn=item;
    pn=&item->link;

    // Its a folder
    item->state=FXDirItem::FOLDER|FXDirItem::HASITEMS;

    // Assume no associations
    fileassoc=NULL;
    drivetype=GetDriveTypeA(name.text());
    switch(drivetype){
      case DRIVE_REMOVABLE:
        if(name[0]=='A' || name[0]=='B'){
          openicon=floppyicon;
          closedicon=floppyicon;
          }
        else{
          openicon=zipdiskicon;
          closedicon=zipdiskicon;
          }
        break;
      case DRIVE_REMOTE:
        openicon=networkicon;
        closedicon=networkicon;
        break;
      case DRIVE_CDROM:
        openicon=cdromicon;
        closedicon=cdromicon;
        break;
      case DRIVE_RAMDISK:
        openicon=open_folder;
        closedicon=closed_folder;
        break;
      case DRIVE_FIXED:
        openicon=harddiskicon;
        closedicon=harddiskicon;
        break;
      case DRIVE_UNKNOWN:
      case DRIVE_NO_ROOT_DIR:
      default:
        openicon=open_folder;
        closedicon=closed_folder;
        break;
      }
    if(associations) fileassoc=associations->findDirBinding(name.text());

    // If association is found, use it
    if(fileassoc){
      if(fileassoc->miniicon) closedicon=fileassoc->miniicon;
      if(fileassoc->miniiconopen) openicon=fileassoc->miniiconopen;
      }

    // Update item information
    item->openIcon=openicon;
    item->closedIcon=closedicon;
    item->size=0L;
    item->assoc=fileassoc;
    item->date=0;

    // Create item
    if(id()) item->create();
    }

  // Wipe items remaining in list:- they have disappeared!!
  for(item=oldlist; item; item=link){
    link=item->link;
    removeItem(item,TRUE);
    }

  // Remember new list
  list=newlist;
  }

#endif


/*******************************************************************************/

// List child items
void FXDirList::listChildItems(FXDirItem *par){
  FXDirItem   *oldlist,*newlist,**po,**pp,**pn,*item,*link;
  FXIcon      *openicon;
  FXIcon      *closedicon;
  FXFileAssoc *fileassoc;
  FXString     pathname;
  FXString     directory;
  FXString     name;
  FXStat       info;
  FXint        islink;
  FXDir        dir;

  // Path to parent node
  directory=getItemPathname(par);

  // Build new insert-order list
  oldlist=par->list;
  newlist=NULL;

  // Assemble lists
  po=&oldlist;
  pn=&newlist;

  // Assume not a link
  islink=FALSE;

  // Managed to open directory
  if(dir.open(directory)){

    // Process directory entries
    while(dir.next()){

      // Get name of entry
      name=dir.name();

      // A dot special file?
      if(name[0]=='.' && (name[1]==0 || (name[1]=='.' && name[2]==0))) continue;

      // Hidden file or directory normally not shown
      if(name[0]=='.' && !(options&DIRLIST_SHOWHIDDEN)) continue;

      // Build full pathname of entry
      pathname=directory;
      if(!ISPATHSEP(pathname[pathname.length()-1])) pathname+=PATHSEPSTRING;
      pathname+=name;

#ifndef WIN32

      // Get file/link info
      if(!FXStat::statLink(pathname,info)) continue;

      // If its a link, get the info on file itself
      islink=info.isLink();
      if(islink && !FXStat::statFile(pathname,info)) continue;

#else

      // Get file/link info
      if(!FXStat::statFile(pathname,info)) continue;

      // Hidden file or directory normally not shown
      if(info.isHidden() && !(options&DIRLIST_SHOWHIDDEN)) continue;

#endif

      // If it is not a directory, and not showing files and matching pattern skip it
      if(!info.isDirectory() && !((options&DIRLIST_SHOWFILES) && FXPath::match(pattern,name,matchmode))) continue;

      // Find it, and take it out from the old list if found
      for(pp=po; (item=*pp)!=NULL; pp=&item->link){
        if(compare(item->label,name)==0){
          *pp=item->link;
          item->link=NULL;
          po=pp;
          goto fnd;
          }
        }

      // Not found; prepend before list
      item=(FXDirItem*)appendItem(par,name,open_folder,closed_folder,NULL,TRUE);

      // Next gets hung after this one
fnd:  *pn=item;
      pn=&item->link;

      // Item flags
      if(info.isExecutable()){item->state|=FXDirItem::EXECUTABLE;}else{item->state&=~FXDirItem::EXECUTABLE;}
      if(info.isDirectory()){item->state|=FXDirItem::FOLDER;item->state&=~FXDirItem::EXECUTABLE;}else{item->state&=~(FXDirItem::FOLDER|FXDirItem::HASITEMS);}
      if(info.isCharacter()){item->state|=FXDirItem::CHARDEV;item->state&=~FXDirItem::EXECUTABLE;}else{item->state&=~FXDirItem::CHARDEV;}
      if(info.isBlock()){item->state|=FXDirItem::BLOCKDEV;item->state&=~FXDirItem::EXECUTABLE;}else{item->state&=~FXDirItem::BLOCKDEV;}
      if(info.isFifo()){item->state|=FXDirItem::FIFO;item->state&=~FXDirItem::EXECUTABLE;}else{item->state&=~FXDirItem::FIFO;}
      if(info.isSocket()){item->state|=FXDirItem::SOCK;item->state&=~FXDirItem::EXECUTABLE;}else{item->state&=~FXDirItem::SOCK;}
      if(islink){item->state|=FXDirItem::SYMLINK;}else{item->state&=~FXDirItem::SYMLINK;}

      // We can drag items only if we can drop them to
      item->setDraggable(TRUE);

      // Assume no associations
      fileassoc=NULL;

      // Determine icons and type
      if(item->isDirectory()){
        openicon=open_folder;
        closedicon=closed_folder;
        if(associations) fileassoc=associations->findDirBinding(pathname.text());
        }
      else if(item->isExecutable()){
        openicon=mini_app;
        closedicon=mini_app;
        if(associations) fileassoc=associations->findExecBinding(pathname.text());
        }
      else{
        openicon=mini_doc;
        closedicon=mini_doc;
        if(associations) fileassoc=associations->findFileBinding(pathname.text());
        }

      // If association is found, use it
      if(fileassoc){
        if(fileassoc->miniicon) closedicon=fileassoc->miniicon;
        if(fileassoc->miniiconopen) openicon=fileassoc->miniiconopen;
        }

      // Update item information
      item->openIcon=openicon;
      item->closedIcon=closedicon;
      item->size=info.size();
      item->assoc=fileassoc;
      item->date=info.modified();

      // Create item
      if(id()) item->create();
      }

    // Close it
    dir.close();
    }

  // Wipe items remaining in list:- they have disappeared!!
  for(item=oldlist; item; item=link){
    link=item->link;
    removeItem(item,TRUE);
    }

  // Now we know for sure whether we really have subitems or not
  if(par->first)
    par->state|=FXDirItem::HASITEMS;
  else
    par->state&=~FXDirItem::HASITEMS;

  // Remember new list
  par->list=newlist;

  // Need to layout
  recalc();
  }


/*******************************************************************************/

// Is directory
FXbool FXDirList::isItemDirectory(const FXTreeItem* item) const {
  return item && (item->state&FXDirItem::FOLDER);
  }


// Is file
FXbool FXDirList::isItemFile(const FXTreeItem* item) const {
  return item && !(item->state&(FXDirItem::FOLDER|FXDirItem::CHARDEV|FXDirItem::BLOCKDEV|FXDirItem::FIFO|FXDirItem::SOCK));
  }


// Is executable
FXbool FXDirList::isItemExecutable(const FXTreeItem* item) const {
  return item && (item->state&FXDirItem::EXECUTABLE);
  }


// Return absolute pathname of item
FXString FXDirList::getItemPathname(const FXTreeItem* item) const {
  FXString pathname;
  if(item){
    while(1){
      pathname.prepend(item->getText());
      item=item->parent;
      if(!item) break;
      if(item->parent) pathname.prepend(PATHSEP);
      }
    }
  return pathname;
  }


// Return the item from the absolute pathname
FXTreeItem* FXDirList::getPathnameItem(const FXString& path){
  register FXTreeItem *item,*it;
  register FXint beg=0,end=0;
  FXString name;
  if(!path.empty()){
#ifndef WIN32
    if(ISPATHSEP(path[0])) end++;
#else
    if(ISPATHSEP(path[0])){
      end++;
      if(ISPATHSEP(path[1])) end++;
      }
    else if(Ascii::isLetter((FXuchar)path[0]) && path[1]==':'){
      end+=2;
      if(ISPATHSEP(path[2])) end++;
      }
#endif
    if(beg<end){
      name=path.mid(beg,end-beg);
      for(it=firstitem; it; it=it->next){
#ifndef WIN32
        if(compare(name,it->getText())==0) goto x;
#else
        if(comparecase(name,it->getText())==0) goto x;
#endif
        }
      listRootItems();
      sortRootItems();
      for(it=firstitem; it; it=it->next){
#ifndef WIN32
        if(compare(name,it->getText())==0) goto x;
#else
        if(comparecase(name,it->getText())==0) goto x;
#endif
        }
      return NULL;
x:    item=it;
      FXASSERT(item);
      while(end<path.length()){
        beg=end;
        while(end<path.length() && !ISPATHSEP(path[end])) end++;
        name=path.mid(beg,end-beg);
        for(it=item->first; it; it=it->next){
#ifndef WIN32
          if(compare(name,it->getText())==0) goto y;
#else
          if(comparecase(name,it->getText())==0) goto y;
#endif
          }
        listChildItems((FXDirItem*)item);
        sortChildItems(item);
        for(it=item->first; it; it=it->next){
#ifndef WIN32
          if(compare(name,it->getText())==0) goto y;
#else
          if(comparecase(name,it->getText())==0) goto y;
#endif
          }
        return item;
y:      item=it;
        FXASSERT(item);
        if(end<path.length() && ISPATHSEP(path[end])) end++;
        }
      FXASSERT(item);
      return item;
      }
    }
  return NULL;
  }


// Open all intermediate directories down toward given one
void FXDirList::setDirectory(const FXString& pathname,FXbool notify){
  FXTRACE((100,"%s::setDirectory(%s)\n",getClassName(),pathname.text()));
  if(!pathname.empty()){
    FXString path=FXPath::absolute(getItemPathname(currentitem),pathname);
    while(!FXPath::isTopDirectory(path) && !FXStat::isDirectory(path)){
      path=FXPath::upLevel(path);
      }
    FXTreeItem *item=getPathnameItem(path);
    if(id()) layout();
    makeItemVisible(item);
    setCurrentItem(item,notify);
    }
  }


// Return directory part of path to current item
FXString FXDirList::getDirectory() const {
  const FXTreeItem* item=currentitem;
  while(item){
    if(item->state&FXDirItem::FOLDER) return getItemPathname(item);
    item=item->parent;
    }
  return "";
  }


// Set current (dir/file) name path
void FXDirList::setCurrentFile(const FXString& pathname,FXbool notify){
  FXTRACE((100,"%s::setCurrentFile(%s)\n",getClassName(),pathname.text()));
  if(!pathname.empty()){
    FXString path=FXPath::absolute(getItemPathname(currentitem),pathname);
    while(!FXPath::isTopDirectory(path) && !FXStat::exists(path)){
      path=FXPath::upLevel(path);
      }
    FXTreeItem *item=getPathnameItem(path);
    makeItemVisible(item);
    setAnchorItem(item);
    setCurrentItem(item,notify);
    if(item){
      selectItem(item);
      }
    }
  }


// Get current (dir/file) name path
FXString FXDirList::getCurrentFile() const {
  return getItemPathname(currentitem);
  }



// Get list style
FXbool FXDirList::showFiles() const {
  return (options&DIRLIST_SHOWFILES)!=0;
  }


// Change list style
void FXDirList::showFiles(FXbool showing){
  FXuint opts=options;
  if(showing) opts|=DIRLIST_SHOWFILES; else opts&=~DIRLIST_SHOWFILES;
  if(options!=opts){
    options=opts;
    scan(TRUE);
    }
  }


// Return TRUE if showing hidden files
FXbool FXDirList::showHiddenFiles() const {
  return (options&DIRLIST_SHOWHIDDEN)!=0;
  }


// Change show hidden files mode
void FXDirList::showHiddenFiles(FXbool showing){
  FXuint opts=options;
  if(showing) opts|=DIRLIST_SHOWHIDDEN; else opts&=~DIRLIST_SHOWHIDDEN;
  if(opts!=options){
    options=opts;
    scan(TRUE);
    }
  }


// Change associations table; force a rescan so as to
// update the bindings in each item to the new associations
void FXDirList::setAssociations(FXFileDict* assocs){
  if(associations!=assocs){
    associations=assocs;
    scan(TRUE);
    }
  }


// Set the pattern to filter
void FXDirList::setPattern(const FXString& ptrn){
  if(ptrn.empty()) return;
  if(pattern!=ptrn){
    pattern=ptrn;
    scan(TRUE);
    }
  }


// Change file match mode
void FXDirList::setMatchMode(FXuint mode){
  if(matchmode!=mode){
    matchmode=mode;
    scan(TRUE);
    }
  }


// Save data
void FXDirList::save(FXStream& store) const {
  FXTreeList::save(store);
  store << associations;
  store << pattern;
  store << matchmode;
  store << closed_folder;
  store << open_folder;
  store << mini_doc;
  store << mini_app;
  }


// Load data
void FXDirList::load(FXStream& store){
  FXTreeList::load(store);
  store >> associations;
  store >> pattern;
  store >> matchmode;
  store >> closed_folder;
  store >> open_folder;
  store >> mini_doc;
  store >> mini_app;
  }


// Cleanup
FXDirList::~FXDirList(){
  clearItems();
  getApp()->removeTimeout(this,ID_REFRESHTIMER);
  if(!(options&DIRLIST_NO_OWN_ASSOC)) delete associations;
  delete closed_folder;
  delete open_folder;
  delete mini_doc;
  delete mini_app;
  delete cdromicon;
  delete harddiskicon;
  delete networkicon;
  delete floppyicon;
  delete zipdiskicon;
  associations=(FXFileDict*)-1L;
  closed_folder=(FXGIFIcon*)-1L;
  open_folder=(FXGIFIcon*)-1L;
  mini_doc=(FXGIFIcon*)-1L;
  mini_app=(FXGIFIcon*)-1L;
  cdromicon=(FXIcon*)-1L;
  harddiskicon=(FXIcon*)-1L;
  networkicon=(FXIcon*)-1L;
  floppyicon=(FXIcon*)-1L;
  zipdiskicon=(FXIcon*)-1L;
  }

}

