/********************************************************************************
*                                                                               *
*                     R e c e n t   F i l e s   L i s t                         *
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
* $Id: FXRecentFiles.cpp,v 1.36 2006/01/22 17:58:39 fox Exp $                   *
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
#include "FXRegistry.h"
#include "FXApp.h"
#include "FXWindow.h"
#include "FXRecentFiles.h"



/*
  Notes:
  - Use the auto-hide or auto-gray feature to hide menus which are connected
    to the FXRecentFiles class.
  - Default constructor should probably be deprecated in applications.
*/

using namespace FX;

/*******************************************************************************/

namespace FX {

// Message map
FXDEFMAP(FXRecentFiles) FXRecentFilesMap[] = {
  FXMAPFUNC(SEL_UPDATE,FXRecentFiles::ID_ANYFILES,FXRecentFiles::onUpdAnyFiles),
  FXMAPFUNC(SEL_UPDATE,FXRecentFiles::ID_CLEAR,FXRecentFiles::onUpdAnyFiles),
  FXMAPFUNC(SEL_COMMAND,FXRecentFiles::ID_CLEAR,FXRecentFiles::onCmdClear),
  FXMAPFUNCS(SEL_COMMAND,FXRecentFiles::ID_FILE_1,FXRecentFiles::ID_FILE_10,FXRecentFiles::onCmdFile),
  FXMAPFUNCS(SEL_UPDATE,FXRecentFiles::ID_FILE_1,FXRecentFiles::ID_FILE_10,FXRecentFiles::onUpdFile),
  };


// Class implementation
FXIMPLEMENT(FXRecentFiles,FXObject,FXRecentFilesMap,ARRAYNUMBER(FXRecentFilesMap))


// Serialization
FXRecentFiles::FXRecentFiles():app(FXApp::instance()),target(NULL),message(0),group("Recent Files"),maxfiles(10){
  }


// Make new Recent Files group with default group
FXRecentFiles::FXRecentFiles(FXApp* a):app(a),target(NULL),message(0),group("Recent Files"),maxfiles(10){
  }


// Make new Recent Files group
FXRecentFiles::FXRecentFiles(FXApp* a,const FXString& gp,FXObject *tgt,FXSelector sel):app(a),target(tgt),message(sel),group(gp),maxfiles(10){
  }


// Obtain the filename at index
FXString FXRecentFiles::getFile(FXint index) const {
  FXchar key[20];
  sprintf(key,"FILE%d",index);
  return app->reg().readStringEntry(group.text(),key,FXString::null);
  }


// Change the filename at index
void FXRecentFiles::setFile(FXint index,const FXString& filename){
  FXchar key[20];
  sprintf(key,"FILE%d",index);
  app->reg().writeStringEntry(group.text(),key,filename.text());
  }


// Append a file; its added to the top of the list, and everything else
// is moved down the list one notch; the last one is dropped from the list.
void FXRecentFiles::appendFile(const FXString& filename){
  FXString newname=filename;
  FXString oldname;
  FXchar key[20];
  FXint i=1,j=1;
  do{
    do{
      sprintf(key,"FILE%d",j++);
      oldname=app->reg().readStringEntry(group.text(),key,NULL);
      }
    while(oldname==filename);
    sprintf(key,"FILE%d",i++);
    app->reg().writeStringEntry(group.text(),key,newname.text());
    newname=oldname;
    }
  while(!oldname.empty() && i<=maxfiles);
  }


// Remove a file
void FXRecentFiles::removeFile(const FXString& filename){
  FXchar key[20];
  FXString name;
  FXint i=1,j=1;
  do{
    sprintf(key,"FILE%d",i++);
    name=app->reg().readStringEntry(group.text(),key,NULL);
    app->reg().deleteEntry(group.text(),key);
    if(name.empty()) break;
    if(name!=filename){
      sprintf(key,"FILE%d",j++);
      app->reg().writeStringEntry(group.text(),key,name.text());
      }
    }
  while(i<=maxfiles);
  }


// Remove all files from the list
void FXRecentFiles::clear(){
  app->reg().deleteSection(group.text());
  }


// Clear the files list
long FXRecentFiles::onCmdClear(FXObject*,FXSelector,void*){
  clear();
  return 1;
  }


// User clicks on one of the file names
long FXRecentFiles::onCmdFile(FXObject*,FXSelector sel,void*){
  const FXchar *filename;
  FXchar key[20];
  if(target){
    sprintf(key,"FILE%d",(FXSELID(sel)-ID_FILE_1+1));
    filename=app->reg().readStringEntry(group.text(),key,NULL);
    if(filename){
      target->handle(this,FXSEL(SEL_COMMAND,message),(void*)filename);
      }
    }
  return 1;
  }


// Update handler for same
long FXRecentFiles::onUpdFile(FXObject *sender,FXSelector sel,void*){
  FXint which=FXSELID(sel)-ID_FILE_1+1;
  const FXchar *filename;
  FXString string;
  FXchar key[20];
  sprintf(key,"FILE%d",which);
  filename=app->reg().readStringEntry(group.text(),key,NULL);
  if(filename){
    FXString string;
    if(which<10)
      string.format("&%d %s",which,filename);
    else
      string.format("1&0 %s",filename);
    sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETSTRINGVALUE),(void*)&string);
    sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SHOW),NULL);
    }
  else{
    sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_HIDE),NULL);
    }
  return 1;
  }


// Show or hide depending on whether there are any files
long FXRecentFiles::onUpdAnyFiles(FXObject *sender,FXSelector,void*){
  if(app->reg().readStringEntry(group.text(),"FILE1",NULL))
    sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SHOW),NULL);
  else
    sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_HIDE),NULL);
  return 1;
  }


// Save data
void FXRecentFiles::save(FXStream& store) const {
  FXObject::save(store);
  store << app;
  store << target;
  store << message;
  store << group;
  store << maxfiles;
  }


// Load data
void FXRecentFiles::load(FXStream& store){
  FXObject::load(store);
  store >> app;
  store >> target;
  store >> message;
  store >> group;
  store >> maxfiles;
  }


// Destructor
FXRecentFiles::~FXRecentFiles(){
  target=(FXObject*)-1L;
  }

}
