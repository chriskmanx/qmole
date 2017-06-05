/********************************************************************************
*                                                                               *
*                        P r e f e r e n c e s   D i a l o g                    *
*                                                                               *
*********************************************************************************
* Copyright (C) 2003,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* This program is free software; you can redistribute it and/or modify          *
* it under the terms of the GNU General Public License as published by          *
* the Free Software Foundation; either version 2 of the License, or             *
* (at your option) any later version.                                           *
*                                                                               *
* This program is distributed in the hope that it will be useful,               *
* but WITHOUT ANY WARRANTY; without even the implied warranty of                *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                 *
* GNU General Public License for more details.                                  *
*                                                                               *
* You should have received a copy of the GNU General Public License             *
* along with this program; if not, write to the Free Software                   *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.    *
*********************************************************************************
* $Id: Preferences.cpp,v 1.17 2006/01/22 17:58:15 fox Exp $                     *
********************************************************************************/
#include "fx.h"
#include "PathFinder.h"
#include "Preferences.h"
#include "icons.h"


/*******************************************************************************/

// Map
FXDEFMAP(Preferences) PreferencesMap[]={
  FXMAPFUNC(SEL_COMMAND,Preferences::ID_BROWSE_EDITOR,Preferences::onCmdBrowseEditor),
  FXMAPFUNC(SEL_COMMAND,Preferences::ID_BROWSE_TERMINAL,Preferences::onCmdBrowseTerminal),
  FXMAPFUNC(SEL_COMMAND,Preferences::ID_BROWSE_COMMAND,Preferences::onCmdBrowseCommand),

  FXMAPFUNC(SEL_COMMAND,Preferences::ID_COMMAND,Preferences::onCmdCommand),
  FXMAPFUNC(SEL_COMMAND,Preferences::ID_MIMETYPE,Preferences::onCmdMimeType),
  FXMAPFUNC(SEL_COMMAND,Preferences::ID_DESCRIPTION,Preferences::onCmdDescription),

  FXMAPFUNC(SEL_CHANGED,Preferences::ID_SELECT_EXTENSION,Preferences::onCmdSelectExtension),
  FXMAPFUNC(SEL_COMMAND,Preferences::ID_APPEND_EXTENSION,Preferences::onCmdAppendExtension),
  FXMAPFUNC(SEL_COMMAND,Preferences::ID_REMOVE_EXTENSION,Preferences::onCmdRemoveExtension),
  FXMAPFUNC(SEL_UPDATE,Preferences::ID_REMOVE_EXTENSION,Preferences::onUpdSelectExtension),

  FXMAPFUNCS(SEL_COMMAND,Preferences::ID_BROWSE_BIGICON,Preferences::ID_BROWSE_SMALLICONOPEN,Preferences::onCmdBrowseIcon),
  FXMAPFUNCS(SEL_UPDATE,Preferences::ID_BROWSE_BIGICON,Preferences::ID_BROWSE_SMALLICONOPEN,Preferences::onUpdSelectExtension),
  };


// Object implementation
FXIMPLEMENT(Preferences,FXDialogBox,PreferencesMap,ARRAYNUMBER(PreferencesMap))


// Construct
Preferences::Preferences(PathFinderMain *owner):
  FXDialogBox(owner,"PathFinder Preferences",DECOR_TITLE|DECOR_BORDER|DECOR_RESIZE,0,0,0,0, 0,0,0,0, 4,4){

  FXVerticalFrame *vertical=new FXVerticalFrame(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  FXHorizontalFrame *horizontal=new FXHorizontalFrame(vertical,LAYOUT_FILL_X|LAYOUT_FILL_Y);
  FXVerticalFrame *buttons=new FXVerticalFrame(horizontal,LAYOUT_LEFT|LAYOUT_FILL_Y|FRAME_SUNKEN|PACK_UNIFORM_WIDTH|PACK_UNIFORM_HEIGHT,0,0,0,0, 0,0,0,0, 0,0);
  FXSwitcher *switcher=new FXSwitcher(horizontal,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0);

  // Icons
  pat=new FXGIFIcon(getApp(),pattern_gif);
  brw=new FXGIFIcon(getApp(),file_gif);
  icp=new FXGIFIcon(getApp(),iconpath);
  mim=new FXGIFIcon(getApp(),mimetype);
  dir=new FXGIFIcon(getApp(),setdir);

  ///////////////////////////  Browser settings pane  ////////////////////////////
  FXVerticalFrame* browserpane=new FXVerticalFrame(switcher,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0, 0,0);
  new FXLabel(browserpane,tr("PathFinder settings"),NULL,LAYOUT_LEFT);
  new FXHorizontalSeparator(browserpane,SEPARATOR_LINE|LAYOUT_FILL_X);
  FXMatrix *matrix2=new FXMatrix(browserpane,3,MATRIX_BY_COLUMNS|PACK_UNIFORM_HEIGHT|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,10,0, 6, 6);

  new FXLabel(matrix2,tr("Editor command:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X);
  editor=new FXTextField(matrix2,6,NULL,0,FRAME_SUNKEN|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_COLUMN,0,0,0,0, 2,2,2,2);
  new FXButton(matrix2,tr("\tBrowse..."),dir,this,ID_BROWSE_EDITOR,FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_Y);

  new FXLabel(matrix2,tr("Terminal command:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X);
  terminal=new FXTextField(matrix2,6,NULL,0,FRAME_SUNKEN|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_COLUMN,0,0,0,0, 2,2,2,2);
  new FXButton(matrix2,tr("\tBrowse..."),dir,this,ID_BROWSE_TERMINAL,FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_Y);

  new FXLabel(matrix2,tr("Preview images:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X);
  preview=new FXCheckButton(matrix2,FXString::null,NULL,0,LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN,0,0,0,0, 0,0,0,0);
  new FXFrame(matrix2,0);

  new FXLabel(matrix2,tr("Image blending:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_X);
  blending=new FXCheckButton(matrix2,FXString::null,NULL,0,LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN,0,0,0,0, 0,0,0,0);
  new FXFrame(matrix2,0);

  //// Browser settings button
  new FXButton(buttons,tr("Browser\tFile browser settings\tChange browser settings and other things."),brw,switcher,FXSwitcher::ID_OPEN_FIRST,FRAME_RAISED|ICON_ABOVE_TEXT|LAYOUT_FILL_Y);


  ///////////////////////  File pattern settings pane  //////////////////////////
  FXVerticalFrame* filepatpane=new FXVerticalFrame(switcher,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0, 0,0);
  new FXLabel(filepatpane,tr("Filename patterns"),NULL,LAYOUT_LEFT);
  new FXHorizontalSeparator(filepatpane,SEPARATOR_LINE|LAYOUT_FILL_X);
  FXVerticalFrame *sub3=new FXVerticalFrame(filepatpane,LAYOUT_FILL_Y|LAYOUT_FILL_X,0,0,0,0, 0,0,10,0, 0,0);
  new FXLabel(sub3,tr("Filename patterns, one per line:"),NULL,JUSTIFY_LEFT);
  FXVerticalFrame* textwell=new FXVerticalFrame(sub3,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_SUNKEN|FRAME_THICK,0,0,0,0, 0,0,0,0);
  pattern=new FXText(textwell,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y);

  //// File pattern settings button
  new FXButton(buttons,tr("Patterns\tFilename patterns\tChange wildcard patterns for filenames."),pat,switcher,FXSwitcher::ID_OPEN_SECOND,FRAME_RAISED|ICON_ABOVE_TEXT|LAYOUT_FILL_Y);


  ///////////////////////  Icon path settings pane  //////////////////////////
  FXVerticalFrame* iconpathpane=new FXVerticalFrame(switcher,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0, 0,0);
  new FXLabel(iconpathpane,tr("Icon search path"),NULL,LAYOUT_LEFT);
  new FXHorizontalSeparator(iconpathpane,SEPARATOR_LINE|LAYOUT_FILL_X);
  FXVerticalFrame *sub4=new FXVerticalFrame(iconpathpane,LAYOUT_FILL_Y|LAYOUT_FILL_X,0,0,0,0, 0,0,10,0, 0,0);
  new FXLabel(sub4,tr("Icon search folders, separated by '" PATHLISTSEPSTRING "', on one line:"),NULL,JUSTIFY_LEFT);
  FXVerticalFrame* textbox=new FXVerticalFrame(sub4,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_SUNKEN|FRAME_THICK,0,0,0,0, 0,0,0,0);
  icondirs=new FXText(textbox,NULL,0,TEXT_WORDWRAP|LAYOUT_FILL_X|LAYOUT_FILL_Y);

  //// File pattern settings button
  new FXButton(buttons,tr("Icon Path\tIcon search path\tChange folders searched for icons."),icp,switcher,FXSwitcher::ID_OPEN_THIRD,FRAME_RAISED|ICON_ABOVE_TEXT|LAYOUT_FILL_Y);


  ///////////////////////  Mime type settings pane  //////////////////////////
  FXPacker* mimetypepane=new FXPacker(switcher,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0);
  new FXLabel(mimetypepane,tr("Mime Type Setup"),NULL,LAYOUT_LEFT|LAYOUT_SIDE_TOP);
  new FXHorizontalSeparator(mimetypepane,SEPARATOR_LINE|LAYOUT_FILL_X|LAYOUT_SIDE_TOP);

  // List of possible extensions of this file type
  FXGroupBox *extensiongroup=new FXGroupBox(mimetypepane,tr("File Extensions"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_Y|LAYOUT_SIDE_LEFT);
  FXHorizontalFrame *filetypebuttons=new FXHorizontalFrame(extensiongroup,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|PACK_UNIFORM_WIDTH,0,0,0,0, 0,0,0,0);
  new FXButton(filetypebuttons,tr("Add..."),NULL,this,ID_APPEND_EXTENSION,FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_CENTER_X);
  new FXButton(filetypebuttons,tr("Remove"),NULL,this,ID_REMOVE_EXTENSION,FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_CENTER_X);
  FXVerticalFrame *extensionFrame=new FXVerticalFrame(extensiongroup,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_SUNKEN|FRAME_THICK,0,0,0,0, 0,0,0,0);
  extensions=new FXList(extensionFrame,this,ID_SELECT_EXTENSION,LIST_BROWSESELECT|LAYOUT_FILL_X|LAYOUT_FILL_Y|HSCROLLER_NEVER);
  extensions->setSortFunc(FXList::ascendingCase);
  extensions->setNumVisible(4);

  // Various icons for this extension
  FXGroupBox *iconsgroup=new FXGroupBox(mimetypepane,tr("Icons"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X|LAYOUT_SIDE_TOP);
  FXMatrix *iconsmatrix=new FXMatrix(iconsgroup,8,MATRIX_BY_COLUMNS|LAYOUT_FILL_X|LAYOUT_FILL_Y|PACK_UNIFORM_WIDTH,0,0,0,0, 0,0,0,0);

  new FXLabel(iconsmatrix,tr("Big\nNormal"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_Y|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  bigclosed=new FXButton(iconsmatrix,tr("\tChange icon"),NULL,this,ID_BROWSE_BIGICON,FRAME_RAISED|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|JUSTIFY_CENTER_X|JUSTIFY_CENTER_Y|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW,0,0,40,40, 1,1,1,1);

  new FXLabel(iconsmatrix,tr("Small\nNormal"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_Y|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  smallclosed=new FXButton(iconsmatrix,tr("\tChange icon"),NULL,this,ID_BROWSE_SMALLICON,FRAME_RAISED|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|JUSTIFY_CENTER_X|JUSTIFY_CENTER_Y|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW,0,0,40,40, 1,1,1,1);

  new FXLabel(iconsmatrix,tr("Big\nOpen"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_Y|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  bigopen=new FXButton(iconsmatrix,tr("\tChange icon"),NULL,this,ID_BROWSE_BIGICONOPEN,FRAME_RAISED|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|JUSTIFY_CENTER_X|JUSTIFY_CENTER_Y|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW,0,0,40,40, 1,1,1,1);

  new FXLabel(iconsmatrix,tr("Small\nOpen"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_Y|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
  smallopen=new FXButton(iconsmatrix,tr("\tChange icon"),NULL,this,ID_BROWSE_SMALLICONOPEN,FRAME_RAISED|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|JUSTIFY_CENTER_X|JUSTIFY_CENTER_Y|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW,0,0,40,40, 1,1,1,1);

  // Description
  FXGroupBox *descgroup=new FXGroupBox(mimetypepane,tr("Description of extension"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X|LAYOUT_SIDE_TOP);
  description=new FXTextField(descgroup,30,this,ID_DESCRIPTION,LAYOUT_FILL_X|LAYOUT_CENTER_Y|FRAME_SUNKEN|FRAME_THICK);

  // Mime type
  FXGroupBox *mimegroup=new FXGroupBox(mimetypepane,tr("Mime Types"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X|LAYOUT_SIDE_TOP);
  FXVerticalFrame *mimeFrame=new FXVerticalFrame(mimegroup,LAYOUT_SIDE_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_SUNKEN|FRAME_THICK,0,0,0,0, 0,0,0,0);
  mimetypes=new FXComboBox(mimeFrame,10,this,ID_MIMETYPE,COMBOBOX_NO_REPLACE|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  mimetypes->setSortFunc(FXList::ascendingCase);
  mimetypes->setNumVisible(10);

  // Command
  FXGroupBox *commandgroup=new FXGroupBox(mimetypepane,tr("Command"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X|LAYOUT_SIDE_TOP);
  FXHorizontalFrame *commandset=new FXHorizontalFrame(commandgroup,LAYOUT_SIDE_TOP|LAYOUT_FILL_X);
  new FXButton(commandset,tr("\tBrowse..."),dir,this,ID_BROWSE_COMMAND,LAYOUT_RIGHT|LAYOUT_CENTER_Y|FRAME_RAISED|FRAME_THICK);
  command=new FXTextField(commandset,40,this,ID_COMMAND,LAYOUT_FILL_X|LAYOUT_CENTER_Y|FRAME_SUNKEN|FRAME_THICK);
  runinterminal=new FXCheckButton(commandgroup,tr("Run in terminal"),NULL,0,ICON_BEFORE_TEXT|LAYOUT_SIDE_LEFT);
  changedirectory=new FXCheckButton(commandgroup,tr("Change directory"),NULL,0,ICON_BEFORE_TEXT|LAYOUT_SIDE_LEFT);
  runinterminal->disable();
  changedirectory->disable();


  //// Mime type settings button
  new FXButton(buttons,tr("Mime Types\tMime type setup\tChange mime types for files."),mim,switcher,FXSwitcher::ID_OPEN_FOURTH,FRAME_RAISED|ICON_ABOVE_TEXT|LAYOUT_FILL_Y);

  // Bottom part
  new FXHorizontalSeparator(vertical,SEPARATOR_RIDGE|LAYOUT_FILL_X);
  FXHorizontalFrame *closebox=new FXHorizontalFrame(vertical,LAYOUT_BOTTOM|LAYOUT_FILL_X|PACK_UNIFORM_WIDTH);
  new FXButton(closebox,tr("&Accept"),NULL,this,FXDialogBox::ID_ACCEPT,BUTTON_INITIAL|BUTTON_DEFAULT|LAYOUT_RIGHT|FRAME_RAISED|FRAME_THICK,0,0,0,0, 20,20);
  new FXButton(closebox,tr("&Cancel"),NULL,this,FXDialogBox::ID_CANCEL,BUTTON_DEFAULT|LAYOUT_RIGHT|FRAME_RAISED|FRAME_THICK,0,0,0,0, 20,20);
  }


// Select command
long Preferences::onCmdSelectExtension(FXObject*,FXSelector,void*){
  return 1;
  }


// Gray out things if no extension selected
long Preferences::onUpdSelectExtension(FXObject* sender,FXSelector,void*){
  sender->handle(this,(0<=extensions->getCurrentItem())?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Append new extension binding
long Preferences::onCmdAppendExtension(FXObject*,FXSelector,void*){
  return 1;
  }


// Remove extension binding
long Preferences::onCmdRemoveExtension(FXObject*,FXSelector,void*){
  return 1;
  }


// Change command
long Preferences::onCmdCommand(FXObject*,FXSelector,void* ptr){
  return 1;
  }


// Change description
long Preferences::onCmdDescription(FXObject*,FXSelector,void* ptr){
  return 1;
  }


// Change mime type
long Preferences::onCmdMimeType(FXObject*,FXSelector,void* ptr){
  return 1;
  }


// Set icon path
long Preferences::onCmdBrowseIcon(FXObject*,FXSelector sel,void*){
  return 1;
  }


// Select command
long Preferences::onCmdBrowseCommand(FXObject*,FXSelector,void*){
  return 1;
  }


// Set browser editor
long Preferences::onCmdBrowseEditor(FXObject*,FXSelector,void*){
  FXString neweditor=editor->getText();
  neweditor=FXFileDialog::getOpenFilename(this,tr("Editor Program"),neweditor);
  if(!neweditor.empty()) editor->setText(neweditor);
  return 1;
  }


// Set terminal
long Preferences::onCmdBrowseTerminal(FXObject*,FXSelector,void*){
  FXString newterminal=terminal->getText();
  newterminal=FXFileDialog::getOpenFilename(this,tr("Terminal Program"),newterminal);
  if(!newterminal.empty()) terminal->setText(newterminal);
  return 1;
  }


// Clean up
Preferences::~Preferences(){
  delete pat;
  delete brw;
  delete icp;
  delete mim;
  delete dir;
  }
