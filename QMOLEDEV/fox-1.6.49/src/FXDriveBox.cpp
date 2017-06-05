/********************************************************************************
*                                                                               *
*                         D r i v e   B o x   O b j e c t                       *
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
* $Id: FXDriveBox.cpp,v 1.35 2006/01/22 17:58:25 fox Exp $                      *
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
#include "FXAccelTable.h"
#include "FXObjectList.h"
#include "FXApp.h"
#include "FXId.h"
#include "FXPath.h"
#include "FXSystem.h"
#include "FXDrawable.h"
#include "FXImage.h"
#include "FXIcon.h"
#include "FXGIFIcon.h"
#include "FXBMPIcon.h"
#include "FXFont.h"
#include "FXDC.h"
#include "FXWindow.h"
#include "FXComposite.h"
#include "FXShell.h"
#include "FXTopWindow.h"
#include "FXDialogBox.h"
#include "FXMessageBox.h"
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
#include "FXList.h"
#include "FXListBox.h"
#include "FXDriveBox.h"
#include "FXFileDict.h"
#include "icons.h"


/*
  Notes:
  - When setting path, it adds all directories from the top down to
    the lowest directory.
  - It also adds common places in the file system.
  - Add API's to set current path [from root down to whereever]
  - Add code to read path of selected item
  - Add some better icons
*/

using namespace FX;

/*******************************************************************************/

namespace FX {


// Map
FXDEFMAP(FXDriveBox) FXDriveBoxMap[]={
  FXMAPFUNC(SEL_CHANGED,FXDriveBox::ID_LIST,FXDriveBox::onListChanged),
  FXMAPFUNC(SEL_CLICKED,FXDriveBox::ID_LIST,FXDriveBox::onListClicked),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_SETVALUE,FXDriveBox::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_SETSTRINGVALUE,FXDriveBox::onCmdSetStringValue),
  FXMAPFUNC(SEL_COMMAND,FXWindow::ID_GETSTRINGVALUE,FXDriveBox::onCmdGetStringValue),
  };


// Implementation
FXIMPLEMENT(FXDriveBox,FXListBox,FXDriveBoxMap,ARRAYNUMBER(FXDriveBoxMap))


// Directory box
FXDriveBox::FXDriveBox(FXComposite *p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXListBox(p,tgt,sel,opts,x,y,w,h, pl,pr,pt,pb){
  associations=NULL;
  if(!(options&DRIVEBOX_NO_OWN_ASSOC)) associations=new FXFileDict(getApp());
  foldericon=new FXGIFIcon(getApp(),minifolder);
  cdromicon=new FXGIFIcon(getApp(),minicdrom);
  harddiskicon=new FXGIFIcon(getApp(),miniharddisk);
  netdriveicon=new FXGIFIcon(getApp(),mininetdrive);
  floppyicon=new FXGIFIcon(getApp(),minifloppy);
  nethoodicon=new FXGIFIcon(getApp(),mininethood);
  zipdiskicon=new FXGIFIcon(getApp(),minizipdrive);
  setDrive(FXSystem::getCurrentDrive());
  }


// Create
void FXDriveBox::create(){
  FXListBox::create();
  foldericon->create();
  cdromicon->create();
  harddiskicon->create();
  netdriveicon->create();
  floppyicon->create();
  nethoodicon->create();
  zipdiskicon->create();
  }


// Detach disconnects the icons
void FXDriveBox::detach(){
  FXListBox::detach();
  foldericon->detach();
  cdromicon->detach();
  harddiskicon->detach();
  netdriveicon->detach();
  floppyicon->detach();
  nethoodicon->detach();
  zipdiskicon->detach();
  }


// Destroy zaps the icons
void FXDriveBox::destroy(){
  FXListBox::destroy();
  foldericon->destroy();
  cdromicon->destroy();
  harddiskicon->destroy();
  netdriveicon->destroy();
  floppyicon->destroy();
  nethoodicon->destroy();
  zipdiskicon->destroy();
  }


// Set the current item's text from the message
long FXDriveBox::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setDrive((char*)ptr);
  return 1;
  }


// Change value
long FXDriveBox::onCmdSetStringValue(FXObject*,FXSelector,void* ptr){
  setDrive(*((FXString*)ptr));
  return 1;
  }


// Obtain value
long FXDriveBox::onCmdGetStringValue(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getDrive();
  return 1;
  }

#ifndef WIN32           // UNIX flavor

// Fill list with names of available drives
void FXDriveBox::listDrives(){
  register FXFileAssoc *fileassoc;
  register FXIcon *icon;

  // Remove old items first
  clearItems();

  // Determine associations, icons and type
  icon=foldericon;
  if(associations){
    fileassoc=associations->findDirBinding("/");
    if(fileassoc && fileassoc->miniicon) icon=fileassoc->miniicon;
    }

  // Create icon
  if(id()) icon->create();

  // Add item
  appendItem("/",icon);
  }

#else                   // Windows flavor

// Fill list with names of available drives
void FXDriveBox::listDrives(){
  register FXFileAssoc *fileassoc;
  register FXIcon *icon;
  FXchar drivename[10];
  FXuint drivemask;

  // Remove old drives
  clearItems();

  // Add all drives
  drivemask=GetLogicalDrives();
  drivename[1]=':';
  drivename[2]=PATHSEP;
  drivename[3]='\0';

  // Loop over drive letters
  for(drivename[0]='A'; drivename[0]<='Z'; drivename[0]++){
    if(drivemask&1){

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

      // Add another item
      appendItem(drivename,icon);
      }
    drivemask>>=1;
    }
  }

#endif


// Forward clicked message from list to target
long FXDriveBox::onListClicked(FXObject*,FXSelector,void* ptr){
  button->handle(this,FXSEL(SEL_COMMAND,ID_UNPOST),NULL);    // Unpost the list
  if(0<=((FXint)(FXival)ptr)){
    field->setText(getItemText((FXival)ptr));
    field->setIcon(getItemIcon((FXival)ptr));
    if(target){target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)getItemText((FXival)ptr).text());}
    }
  return 1;
  }


// List has changed
long FXDriveBox::onListChanged(FXObject*,FXSelector,void* ptr){
  return target && target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)getItemText((FXival)ptr).text());
  }


// Set directory
FXbool FXDriveBox::setDrive(const FXString& drive){
  listDrives();
  setCurrentItem(findItem(FXPath::drive(FXPath::absolute(drive))));
  return TRUE;
  }


// Return current drive
FXString FXDriveBox::getDrive() const {
  return getItemText(getCurrentItem());
  }


// Change associations table; force regeneration of the items
// in the tree list so all the new bindings take effect
void FXDriveBox::setAssociations(FXFileDict* assocs){
  if(associations!=assocs){
    associations=assocs;
    listDrives();
    }
  }


// Save object to stream
void FXDriveBox::save(FXStream& store) const {
  FXListBox::save(store);
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
void FXDriveBox::load(FXStream& store){
  FXListBox::load(store);
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
FXDriveBox::~FXDriveBox(){
  clearItems();
  if(!(options&DRIVEBOX_NO_OWN_ASSOC)) delete associations;
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
