/********************************************************************************
*                                                                               *
*                     T h e   A d i e   T e x t   E d i t o r                   *
*                                                                               *
*********************************************************************************
* Copyright (C) 1998,2005 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: main.cpp,v 1.19 2005/11/23 07:07:06 fox Exp $                            *
********************************************************************************/
#include "fx.h"
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
#include "Hilite.h"
#include "TextWindow.h"
#include "Adie.h"
#include "icons.h"

/*
  Note:
*/


/*******************************************************************************/

// Print command line help
void printusage(){
  fprintf(stderr,"Usage: adie [options] files...\n");
  fprintf(stderr,"  options:\n");
  fprintf(stderr,"  -h, --help                          Print help.\n");
  fprintf(stderr,"  -V, --version                       Print version number.\n");
  fprintf(stderr,"  -v, --view                          View, don't edit file.\n");
  fprintf(stderr,"  -e, --edit                          Edit file.\n");
  fprintf(stderr,"  -g GEOMETRY, --geometry GEOMETRY    Specify window size.\n");
  fprintf(stderr,"  -l NUM, --line NUM                  Jump to line number.\n");
  fprintf(stderr,"  -m LANGUAGE, --mode LANGUAGE        Force language mode.\n");
  fprintf(stderr,"  -t TAGS, --tags TAGS                Load tags file.\n");
  }


// Start the whole thing
int main(int argc,char *argv[]){
  TextWindow *window=NULL;
  FXchar     *language=NULL;
  FXchar     *tags=NULL;
  FXbool      edit=TRUE;
  FXint       line=0;
  FXString    file;
  FXint       arg;
  FXint       x,y,w,h,g;

  // Make application
  Adie application("Adie");

  // Open display
  application.init(argc,argv);

  // Make a tool tip
  new FXToolTip(&application,0);

  // Create it
  application.create();

  // Loop over args
  for(arg=1; arg<argc; ++arg){

    // Parse a few options
    if(compare(argv[arg],"-v")==0 || compare(argv[arg],"--view")==0){
      edit=FALSE;
      }
    else if(compare(argv[arg],"-e")==0 || compare(argv[arg],"--edit")==0){
      edit=TRUE;
      }
    else if(compare(argv[arg],"-h")==0 || compare(argv[arg],"--help")==0){
      printusage();
      exit(0);
      }
    else if(compare(argv[arg],"-l")==0 || compare(argv[arg],"--line")==0){
      if(++arg>=argc){ fprintf(stderr,"Adie: missing line number argument.\n"); exit(1); }
      sscanf(argv[arg],"%d",&line);
      }
    else if(compare(argv[arg],"-t")==0 || compare(argv[arg],"--tags")==0){
      if(++arg>=argc){ fprintf(stderr,"Adie: missing tags file argument.\n"); exit(1); }
      tags=argv[arg];
      }
    else if(compare(argv[arg],"-m")==0 || compare(argv[arg],"--mode")==0){
      if(++arg>=argc){ fprintf(stderr,"Adie: missing language mode argument.\n"); exit(1); }
      language=argv[arg];
      }
    else if(compare(argv[arg],"-g")==0 || compare(argv[arg],"--geometry")==0){
      if(++arg>=argc){ fprintf(stderr,"Adie: missing geometry argument.\n"); exit(1); }
      g=fxparsegeometry(argv[arg],x,y,w,h);
      }
    else if(compare(argv[arg],"-V")==0 || compare(argv[arg],"--version")==0){
      fprintf(stdout,"Adie - ADvanced Interactive Editor %d.%d.%d.\nCopyright (C) 1998,2003 Jeroen van der Zijp.\n\n",VERSION_MAJOR,VERSION_MINOR,VERSION_PATCH);
      exit(0);
      }

    // Load the file
    else{
      file=FXPath::absolute(argv[arg]);
      window=new TextWindow(&application,"untitled");
      window->create();
      window->loadFile(file);
      window->readBookmarks(file);
      window->readView(file);
      window->setEditable(edit);
      if(language) window->forceSyntax(language);
      if(line) window->visitLine(line);
      }
    }

  // Make window, if none opened yet
  if(!window){
    window=new TextWindow(&application,"untitled");
    window->create();
    if(language) window->forceSyntax(language);
    }

  // Run
  return application.run();
  }


