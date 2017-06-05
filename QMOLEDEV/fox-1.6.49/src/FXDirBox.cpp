/********************************************************************************
*                                                                               *
*                     D i r e c t o r y   B o x   O b j e c t                   *
*                                                                               *
*********************************************************************************
* Copyright (C) 1999,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXDirBox.cpp,v 1.65 2006/01/22 17:58:22 fox Exp $                        *
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
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXAccelTable.h"
#include "FXObjectList.h"
#include "FXApp.h"
#include "FXImage.h"
#include "FXIcon.h"
#include "FXGIFIcon.h"
#include "FXBMPIcon.h"
#include "FXFont.h"
#include "FXDC.h"
#include "FXWindow.h"
#include "FXFrame.h"
#include "FXLabel.h"
#include "FXTextField.h"
#include "FXButton.h"
#include "FXMenuButton.h"
#include "FXComposite.h"
#include "FXPacker.h"
#include "FXShell.h"
#include "FXPopup.h"
#include "FXScrollBar.h"
#include "FXScrollArea.h"
#include "FXTreeList.h"
#include "FXTreeListBox.h"
#include "FXDirBox.h"
#include "FXFileDict.h"
#include "icons.h"

/*
  Notes:
  - When setting path, it adds all directories from the top down to
    the lowest directory.
  - Share icons with other widgets; upgrade icons to some nicer ones.
  - Should some of these icons move to FXFileDict?
  - Need to support ":" directory list separator so we can path not just
    a single path but a list of paths.
*/

using namespace FX;

/*******************************************************************************/

namespace FX {


// Map
FXDEFMAP(FXDirBox) FXDirBoxMap[]={
  FXMAPFUNC(SEL_CHANGED,FXDirBox::ID_TREE,FXDirBox::onTreeChanged),
  FXMAPFUNC(SEL_CLICKED,FXDirBox::ID_TREE,FXDirBox::onTreeClicked),
  FXMAPFUNC(SEL_COMMAND,FXDirBox::ID_SETVALUE,FXDirBox::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXDirBox::ID_SETSTRINGVALUE,FXDirBox::onCmdSetStringValue),
  FXMAPFUNC(SEL_COMMAND,FXDirBox::ID_GETSTRINGVALUE,FXDirBox::onCmdGetStringValue),
  };


// Implementation
FXIMPLEMENT(FXDirBox,FXTreeListBox,FXDirBoxMap,ARRAYNUMBER(FXDirBoxMap))


// Directory box
FXDirBox::FXDirBox(FXComposite *p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXTreeListBox(p,tgt,sel,opts,x,y,w,h, pl,pr,pt,pb){
  associations=NULL;
  if(!(options&DIRBOX_NO_OWN_ASSOC)) associations=new FXFileDict(getApp());
  foldericon=new FXGIFIcon(getApp(),minifolder);
  cdromicon=new FXGIFIcon(getApp(),minicdrom);
  harddiskicon=new FXGIFIcon(getApp(),miniharddisk);
  netdriveicon=new FXGIFIcon(getApp(),mininetdrive);
  floppyicon=new FXGIFIcon(getApp(),minifloppy);
  nethoodicon=new FXGIFIcon(getApp(),mininethood);
  zipdiskicon=new FXGIFIcon(getApp(),minizipdrive);
  setDirectory(PATHSEPSTRING);
  }


// Create
void FXDirBox::create(){
  FXTreeListBox::create();
  foldericon->create();
  cdromicon->create();
  harddiskicon->create();
  netdriveicon->create();
  floppyicon->create();
  nethoodicon->create();
  zipdiskicon->create();
  }


// Detach disconnects the icons
void FXDirBox::detach(){
  foldericon->detach();
  cdromicon->detach();
  harddiskicon->detach();
  netdriveicon->detach();
  floppyicon->detach();
  nethoodicon->detach();
  zipdiskicon->detach();
  FXTreeListBox::detach();
  }


// Destroy zaps the icons
void FXDirBox::destroy(){
  foldericon->destroy();
  cdromicon->destroy();
  harddiskicon->destroy();
  netdriveicon->destroy();
  floppyicon->destroy();
  nethoodicon->destroy();
  zipdiskicon->destroy();
  FXTreeListBox::destroy();
  }


// Set the current item's text from the message
long FXDirBox::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setDirectory((char*)ptr);
  return 1;
  }


// Change value
long FXDirBox::onCmdSetStringValue(FXObject*,FXSelector,void* ptr){
  setDirectory(*((FXString*)ptr));
  return 1;
  }


// Obtain value
long FXDirBox::onCmdGetStringValue(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getDirectory();
  return 1;
  }


// Return item path
FXString FXDirBox::getItemPathname(FXTreeItem *item) const {
  FXString string;
  if(item){
    while(item->getParent()){
      string.prepend(getItemText(item));
      item=item->getParent();
      if(item->getParent()) string.prepend(PATHSEPSTRING);
      }
    string.prepend(getItemText(item));
    }
  return string;
  }


#ifndef WIN32           // UNIX flavor

// Return the item from the absolute pathname
FXTreeItem* FXDirBox::getPathnameItem(const FXString& path){
  register FXFileAssoc *fileassoc;
  register FXTreeItem *item;
  register FXIcon *icon;
  register FXint beg=0;
  register FXint end=0;

  // Remove old items first
  clearItems();

  // Parse past root
  if(ISPATHSEP(path[0])) end++;

  // Absolute path?
  if(beg<end){

    // Determine associations, icons and type
    icon=foldericon;
    if(associations){
      fileassoc=associations->findDirBinding("/");
      if(fileassoc && fileassoc->miniicon) icon=fileassoc->miniicon;
      }

    // Create item
    if(id()) icon->create();

    // Add root
    item=appendItem(NULL,"/",icon,icon);

    // Add the rest
    while(end<path.length()){

      // Begin of path component
      beg=end;

      // Find next path separator
      while(end<path.length() && !ISPATHSEP(path[end])) end++;

      // Determine associations, icons and type
      icon=foldericon;
      if(associations){
        fileassoc=associations->findDirBinding(path.left(end).text());
        if(fileassoc && fileassoc->miniicon) icon=fileassoc->miniicon;
        }

      // Add next item under last
      item=appendItem(item,path.mid(beg,end-beg),icon,icon);

      // Create item
      if(id()) icon->create();

      // Skip over path separator
      if(end<path.length() && ISPATHSEP(path[end])) end++;
      }

    // Return leaf item
    return item;
    }
  return NULL;
  }

#else                   // Windows flavor

// Return the item from the absolute pathname
FXTreeItem* FXDirBox::getPathnameItem(const FXString& path){
  register FXFileAssoc *fileassoc;
  register FXTreeItem *item,*it;
  register FXIcon *icon;
  register FXint beg=0;
  register FXint end=0;
  FXchar drivename[10];
//  FXchar volumename[256];
//  FXchar filesystem[100];
//  FXchar fullname[266];
//  DWORD  MaximumComponentLength;
//  DWORD  FileSystemFlags;
  FXuint drivemask;

  // Remove old items first
  clearItems();

  // Parse past root
  if(ISPATHSEP(path[0])){
    end++;
    if(ISPATHSEP(path[1])) end++;
    }
  else if(Ascii::isLetter(path[0]) && path[1]==':'){
    end+=2;
    if(ISPATHSEP(path[2])) end++;
    }

  // Absolute path?
  if(beg<end){

    // Add all roots
    item=NULL;
    drivemask=GetLogicalDrives();
    drivename[1]=':';
    drivename[2]=PATHSEP;
    drivename[3]='\0';

    // Loop over drive letters
    for(drivename[0]='A'; drivename[0]<='Z'; drivename[0]++){
      if(drivemask&1){

        // Find volume label; unfortunately, we can't use this
        // name as-is since when we're retrieving the item name
        // we're expecting a legal drive letter sans volume label
// 		if('B'<drivename[0] && GetVolumeInformationA(drivename,volumename,sizeof(volumename),NULL,&MaximumComponentLength,&FileSystemFlags,filesystem,sizeof(filesystem))!=0){
//          sprintf(fullname,"%s (%s)",volumename,drivename);
//          }
//        else{
//          sprintf(fullname,"Drive (%s)",drivename);
//          }

        // Default icon based on hardware type
        switch(GetDriveTypeA(drivename)){
          case DRIVE_REMOVABLE: icon=(drivename[0]<='B') ? floppyicon : zipdiskicon; break;
          case DRIVE_FIXED: icon=harddiskicon; break;
          case DRIVE_REMOTE: icon=netdriveicon; break;
          case DRIVE_CDROM: icon=cdromicon; break;
          case DRIVE_RAMDISK: icon=harddiskicon; break;
          case DRIVE_UNKNOWN: icon=foldericon; break;
          case DRIVE_NO_ROOT_DIR: icon=foldericon; break;
          default: icon=foldericon; break;
          }

        // Maybe override from associations
        if(associations){
          fileassoc=associations->findDirBinding(drivename);
          if(fileassoc && fileassoc->miniicon) icon=fileassoc->miniicon;
          }

        // Create item
        if(id()) icon->create();

        // Add another root item
        it=appendItem(NULL,drivename,icon,icon);

        // Rest of path under this root
        if(comparecase(path,drivename,end)==0) item=it;
        }
      drivemask>>=1;
      }
/*
    // Network neighborhood
    icon=nethoodicon;

    // Maybe override from associations
    if(associations){
      fileassoc=associations->findDirBinding("\\\\");
      if(fileassoc && fileassoc->miniicon) icon=fileassoc->miniicon;
      }

    // Create item
    if(id()) icon->create();

    // Add netword neighborhood item
    it=appendItem(NULL,"\\\\",icon,icon);

    // Rest of path under this root maybe
    if(comparecase(path,"\\\\",end)==0) item=it;
*/
    // Got root?
    if(item){

      // Add the rest
      while(end<path.length()){

        // Begin of path component
        beg=end;

        // Find next path separator
        while(end<path.length() && !ISPATHSEP(path[end])) end++;

        // Determine associations, icons and type
        icon=foldericon;
        if(associations){
          fileassoc=associations->findDirBinding(path.left(end).text());
          if(fileassoc && fileassoc->miniicon) icon=fileassoc->miniicon;
          }

        // Create item
        if(id()) icon->create();

        // Add next item under last
        item=appendItem(item,path.mid(beg,end-beg),icon,icon);

        // Skip over path separator
        if(end<path.length() && ISPATHSEP(path[end])) end++;
        }

      // Return leaf item
      return item;
      }
    }
  return NULL;
  }

#endif


// Forward clicked message from list to target
long FXDirBox::onTreeClicked(FXObject*,FXSelector,void* ptr){
  FXString string=getItemPathname((FXTreeItem*)ptr);
  button->handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);    // Unpost the list
  if(ptr){
    field->setText(tree->getItemText((FXTreeItem*)ptr));
    field->setIcon(tree->getItemClosedIcon((FXTreeItem*)ptr));
    removeItem(((FXTreeItem*)ptr)->getFirst());
    if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)string.text());
    }
  return 1;
  }


// Forward changed message from list to target
long FXDirBox::onTreeChanged(FXObject*,FXSelector,void* ptr){
  FXString string=getItemPathname((FXTreeItem*)ptr);
  if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)string.text());
  return 1;
  }


// Set directory
void FXDirBox::setDirectory(const FXString& pathname){
  setCurrentItem(getPathnameItem(FXPath::absolute(pathname)));
  }


// Return current directory
FXString FXDirBox::getDirectory() const {
  return getItemPathname(getCurrentItem());
  }


// Change associations table; force regeneration of the items
// in the tree list so all the new bindings take effect
void FXDirBox::setAssociations(FXFileDict* assocs){
  if(associations!=assocs){
    associations=assocs;
    setDirectory(getDirectory());
    }
  }


// Save object to stream
void FXDirBox::save(FXStream& store) const {
  FXTreeListBox::save(store);
  store << associations;
  store << foldericon;
  store << cdromicon;
  store << harddiskicon;
  store << netdriveicon;
  store << floppyicon;
  store << nethoodicon;
  store << zipdiskicon;
  }


// Load object from stream
void FXDirBox::load(FXStream& store){
  FXTreeListBox::load(store);
  store >> associations;
  store >> foldericon;
  store >> cdromicon;
  store >> harddiskicon;
  store >> netdriveicon;
  store >> floppyicon;
  store >> nethoodicon;
  store >> zipdiskicon;
  }


// Delete it
FXDirBox::~FXDirBox(){
  clearItems();
  if(!(options&DIRBOX_NO_OWN_ASSOC)) delete associations;
  delete foldericon;
  delete cdromicon;
  delete harddiskicon;
  delete netdriveicon;
  delete floppyicon;
  delete nethoodicon;
  delete zipdiskicon;
  associations=(FXFileDict*)-1L;
  foldericon=(FXIcon*)-1L;
  cdromicon=(FXIcon*)-1L;
  harddiskicon=(FXIcon*)-1L;
  netdriveicon=(FXIcon*)-1L;
  floppyicon=(FXIcon*)-1L;
  nethoodicon=(FXIcon*)-1L;
  zipdiskicon=(FXIcon*)-1L;
  }

}

