/********************************************************************************
*                                                                               *
*                           S n a p p e r   W i d g e t                         *
*                                                                               *
*********************************************************************************
* Copyright (C) 2003,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: Snapper.cpp,v 1.6 2006/01/22 17:58:15 fox Exp $                          *
********************************************************************************/
#include "fx.h"
#include "Snapper.h"

/*
  Notes:
*/

/*******************************************************************************/



// Object implementation
FXIMPLEMENT(Snapper,FXShell,NULL,0)


// Deserialization
Snapper::Snapper(){
  flags|=FLAG_ENABLED;
  }


// Create a toplevel window
Snapper::Snapper(FXApp* a,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXShell(a,opts,x,y,w,h){
  backColor=FXRGB(255,128,128);
  flags|=FLAG_ENABLED;
  target=tgt;
  message=sel;
  }

// Get default width
FXint Snapper::getDefaultWidth(){ return 1; }


// Get default height
FXint Snapper::getDefaultHeight(){ return 1; }


// Snapper does override-redirect
bool Snapper::doesOverrideRedirect() const { return true; }


// Snapper does save-unders
bool Snapper::doesSaveUnder() const { return false; }


