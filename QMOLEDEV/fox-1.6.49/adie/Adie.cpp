/********************************************************************************
*                                                                               *
*                     T h e   A d i e   T e x t   E d i t o r                   *
*                                                                               *
*********************************************************************************
* Copyright (C) 1998,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: Adie.cpp,v 1.110.2.1 2006/03/21 07:29:06 fox Exp $                           *
********************************************************************************/
#include "fx.h"
#include "fxkeys.h"
#include <new>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <signal.h>
#ifndef WIN32
#include <unistd.h>
#endif
#include <ctype.h>
#include "FXRex.h"
#include "FXArray.h"
#include "HelpWindow.h"
#include "Preferences.h"
#include "Commands.h"
#include "Hilite.h"
#include "TextWindow.h"
#include "Adie.h"
#include "icons.h"

/*
  Notes:
  - One single collection of icons.
  - Manage list of open windows.
*/


/*******************************************************************************/

// Map
FXDEFMAP(Adie) AdieMap[]={
  FXMAPFUNC(SEL_SIGNAL,Adie::ID_CLOSEALL,Adie::onCmdCloseAll),
  FXMAPFUNC(SEL_COMMAND,Adie::ID_CLOSEALL,Adie::onCmdCloseAll),
  };


// Object implementation
FXIMPLEMENT(Adie,FXApp,AdieMap,ARRAYNUMBER(AdieMap))


// Make some windows
Adie::Adie(const FXString& name):FXApp(name,FXString::null){

  // Make some icons; these are shared between all text windows
  bigicon=new FXGIFIcon(this,big_gif);
  smallicon=new FXGIFIcon(this,small_gif);
  newicon=new FXGIFIcon(this,new_gif,0,IMAGE_ALPHAGUESS);
  reloadicon=new FXGIFIcon(this,reload_gif);
  openicon=new FXGIFIcon(this,open_gif);
  saveicon=new FXGIFIcon(this,save_gif);
  saveasicon=new FXGIFIcon(this,saveas_gif,0,IMAGE_ALPHAGUESS);
  printicon=new FXGIFIcon(this,print_gif);
  cuticon=new FXGIFIcon(this,cut_gif);
  copyicon=new FXGIFIcon(this,copy_gif);
  pasteicon=new FXGIFIcon(this,paste_gif);
  deleteicon=new FXGIFIcon(this,delete_gif);
  undoicon=new FXGIFIcon(this,undo_gif);
  redoicon=new FXGIFIcon(this,redo_gif);
  fontsicon=new FXGIFIcon(this,fonts_gif);
  helpicon=new FXGIFIcon(this,help_gif);
  quiticon=new FXGIFIcon(this,quit_gif);
  shiftlefticon=new FXGIFIcon(this,shiftleft_gif);
  shiftrighticon=new FXGIFIcon(this,shiftright_gif);

  searchicon=new FXGIFIcon(this,search_gif,0,IMAGE_ALPHAGUESS);
  searchnexticon=new FXGIFIcon(this,searchnext_gif,0,IMAGE_ALPHAGUESS);
  searchprevicon=new FXGIFIcon(this,searchprev_gif,0,IMAGE_ALPHAGUESS);
  bookseticon=new FXGIFIcon(this,bookset_gif);
  booknexticon=new FXGIFIcon(this,booknext_gif);
  bookprevicon=new FXGIFIcon(this,bookprev_gif);
  bookdelicon=new FXGIFIcon(this,bookdel_gif);

#ifndef DEBUG
  // If interrupt happens, quit gracefully; we may want to
  // save edit buffer contents w/o asking if display gets
  // disconnected or if hangup signal is received.
  addSignal(SIGINT,this,ID_CLOSEALL);
#ifndef WIN32
  addSignal(SIGQUIT,this,ID_CLOSEALL);
  addSignal(SIGHUP,this,ID_CLOSEALL);
  addSignal(SIGPIPE,this,ID_CLOSEALL);
#endif
#endif

  // File associations
  associations=new FXFileDict(this);
  }


// Initialize application
void Adie::init(int& argc,char** argv,bool connect){
  FXString syntaxfile;

  // After init, the registry has been loaded
  FXApp::init(argc,argv,connect);

  // Now we know the icon search path
  associations->setIconPath(reg().readStringEntry("SETTINGS","iconpath",FXIconDict::defaultIconPath));

  // Hunt for the syntax file
  syntaxfile=FXPath::search(FXSystem::getExecPath(),"Adie.stx");

  // Load syntax file
  if(!syntaxfile.empty()){
    loadSyntaxFile(syntaxfile);
    }
  }


// Exit application
void Adie::exit(FXint code){

  // Writes registry, and quits
  FXApp::exit(code);
  }



// Close all windows
long Adie::onCmdCloseAll(FXObject*,FXSelector,void*){
  while(0<windowlist.no() && windowlist[0]->close(TRUE));
  return 1;
  }


// Clean up the mess
Adie::~Adie(){
  for(int i=0; i<syntaxes.no(); i++) delete syntaxes[i];
  FXASSERT(windowlist.no()==0);
  delete associations;
  delete bigicon;
  delete smallicon;
  delete newicon;
  delete reloadicon;
  delete openicon;
  delete saveicon;
  delete saveasicon;
  delete printicon;
  delete cuticon;
  delete copyicon;
  delete pasteicon;
  delete deleteicon;
  delete undoicon;
  delete redoicon;
  delete fontsicon;
  delete helpicon;
  delete quiticon;
  delete shiftlefticon;
  delete shiftrighticon;
  delete searchicon;
  delete searchnexticon;
  delete searchprevicon;
  delete bookseticon;
  delete booknexticon;
  delete bookprevicon;
  delete bookdelicon;
  }

