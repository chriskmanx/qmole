/********************************************************************************
*                                                                               *
*                D i r e c t o r y   S e l e c t i o n   D i a l o g            *
*                                                                               *
*********************************************************************************
* Copyright (C) 2000,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXDirDialog.cpp,v 1.27 2006/01/22 17:58:22 fox Exp $                     *
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
#include "FXStat.h"
#include "FXFile.h"
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXApp.h"
#include "FXRecentFiles.h"
#include "FXId.h"
#include "FXFont.h"
#include "FXDrawable.h"
#include "FXWindow.h"
#include "FXFrame.h"
#include "FXLabel.h"
#include "FXButton.h"
#include "FXComposite.h"
#include "FXTreeList.h"
#include "FXDirList.h"
#include "FXPacker.h"
#include "FXShell.h"
#include "FXTopWindow.h"
#include "FXDialogBox.h"
#include "FXDirSelector.h"
#include "FXDirDialog.h"



/*
  To do:
*/

using namespace FX;

/*******************************************************************************/

namespace FX {

// Object implementation
FXIMPLEMENT(FXDirDialog,FXDialogBox,NULL,0)


// Construct directory dialog box
FXDirDialog::FXDirDialog(FXWindow* owner,const FXString& name,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXDialogBox(owner,name,opts|DECOR_TITLE|DECOR_BORDER|DECOR_RESIZE|DECOR_CLOSE,x,y,w,h,0,0,0,0,4,4){
  initdialog();
  }


// Construct directory dialog box
FXDirDialog::FXDirDialog(FXApp* a,const FXString& name,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXDialogBox(a,name,opts|DECOR_TITLE|DECOR_BORDER|DECOR_RESIZE|DECOR_CLOSE,x,y,w,h,0,0,0,0,4,4){
  initdialog();
  }


// Initialize dialog and load settings
void FXDirDialog::initdialog(){
  dirbox=new FXDirSelector(this,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y);
  dirbox->acceptButton()->setTarget(this);
  dirbox->acceptButton()->setSelector(FXDialogBox::ID_ACCEPT);
  dirbox->cancelButton()->setTarget(this);
  dirbox->cancelButton()->setSelector(FXDialogBox::ID_CANCEL);
  setWidth(getApp()->reg().readIntEntry("Directory Dialog","width",getWidth()));
  setHeight(getApp()->reg().readIntEntry("Directory Dialog","height",getHeight()));
  }


// Hide window and save settings
void FXDirDialog::hide(){
  FXDialogBox::hide();
  getApp()->reg().writeIntEntry("Directory Dialog","width",getWidth());
  getApp()->reg().writeIntEntry("Directory Dialog","height",getHeight());
  }


// Set directory
void FXDirDialog::setDirectory(const FXString& path){
  dirbox->setDirectory(path);
  }


// Get directory
FXString FXDirDialog::getDirectory() const {
  return dirbox->getDirectory();
  }


// Return TRUE if showing files as well as directories
FXbool FXDirDialog::showFiles() const {
  return dirbox->showFiles();
  }


// Show or hide normal files
void FXDirDialog::showFiles(FXbool showing){
  dirbox->showFiles(showing);
  }


// Return TRUE if showing hidden files
FXbool FXDirDialog::showHiddenFiles() const {
  return dirbox->showHiddenFiles();
  }


// Show or hide hidden files
void FXDirDialog::showHiddenFiles(FXbool showing){
  dirbox->showHiddenFiles(showing);
  }


// Change wildcard matching mode
void FXDirDialog::setMatchMode(FXuint mode){
  dirbox->setMatchMode(mode);
  }


// Return wildcard matching mode
FXuint FXDirDialog::getMatchMode() const {
  return dirbox->getMatchMode();
  }


// Change Directory List style
void FXDirDialog::setDirBoxStyle(FXuint style){
  dirbox->setDirBoxStyle(style);
  }


// Return Directory List style
FXuint FXDirDialog::getDirBoxStyle() const {
  return dirbox->getDirBoxStyle();
  }


// Save data
void FXDirDialog::save(FXStream& store) const {
  FXDialogBox::save(store);
  store << dirbox;
  }


// Load data
void FXDirDialog::load(FXStream& store){
  FXDialogBox::load(store);
  store >> dirbox;
  }


// Cleanup
FXDirDialog::~FXDirDialog(){
  dirbox=(FXDirSelector*)-1L;
  }


// Open existing directory name
FXString FXDirDialog::getOpenDirectory(FXWindow* owner,const FXString& caption,const FXString& path){
  FXDirDialog dirdialog(owner,caption);
  FXString dirname;
  dirdialog.setDirectory(path);
  if(dirdialog.execute()){
    dirname=dirdialog.getDirectory();
    if(FXStat::isDirectory(dirname)) return dirname;
    }
  return FXString::null;
  }

}
