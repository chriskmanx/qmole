/********************************************************************************
*                                                                               *
*                         I c o n   D i c t i o n a r y                         *
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
* $Id: FXIconDict.cpp,v 1.12 2006/01/22 17:58:31 fox Exp $                      *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXThread.h"
#include "FXFile.h"
#include "FXStream.h"
#include "FXFileStream.h"
#include "FXString.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXPath.h"
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXApp.h"
#include "FXIcon.h"
#include "FXIconDict.h"
#include "FXIconSource.h"


/*
  Notes:
  - This class loads an icon from a collection found off the
    icon search path.
  - FXIconDict owns the icons being loaded; other classes like
    FXFileDict and FXFileList merely reference them.
*/


// You can override the default icon locations to search for your
// particular platform by specifying -DDEFAULTICONPATH="path" on
// the command line.
#ifndef DEFAULTICONPATH
#define DEFAULTICONPATH   "~/.foxicons:/usr/local/share/icons:/usr/share/icons"
#endif

using namespace FX;

/*******************************************************************************/

namespace FX {


// Object implementation
FXIMPLEMENT(FXIconDict,FXDict,NULL,0)


// Default icon path
const FXchar FXIconDict::defaultIconPath[]=DEFAULTICONPATH;


// Build icon table
FXIconDict::FXIconDict(FXApp* app,const FXString& p):path(p){
  FXTRACE((100,"FXIconDict::FXIconDict\n"));
  source=new FXIconSource(app);
  }


// Search for the icon name along the search path, and try to load it
void *FXIconDict::createData(const void* ptr){
  return source->loadIconFile(FXPath::search(path,(const char*)ptr));
  }


// Delete the icon
void FXIconDict::deleteData(void* ptr){
  delete ((FXIcon*)ptr);
  }


// Save data
void FXIconDict::save(FXStream& store) const {
  FXDict::save(store);
  store << source;
  store << path;
  }


// Load data
void FXIconDict::load(FXStream& store){
  FXDict::load(store);
  store >> source;
  store >> path;
  }


// Destructor
FXIconDict::~FXIconDict(){
  FXTRACE((100,"FXIconDict::~FXIconDict\n"));
  delete source;
  source=(FXIconSource*)-1L;
  clear();
  }


}
