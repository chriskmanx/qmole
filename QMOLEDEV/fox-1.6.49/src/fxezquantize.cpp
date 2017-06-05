/********************************************************************************
*                                                                               *
*                   E Z   C o l o r   Q u a n t i z a t i o n                   *
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
* $Id: fxezquantize.cpp,v 1.5 2006/01/22 17:58:52 fox Exp $                     *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxpriv.h"


/*
  Notes:

  - Use fxezquantize for a quick test to see if the image contains
    less than 256 colors; for example loading then saving back out
    an 8-bit GIF image.  This ensures that the original set of
    colors is maintained.
*/


using namespace FX;


/*******************************************************************************/

namespace FX {


// EZ quantization may be used if w*h<=maxcolors, or if the actual colors
// used is less than maxcolors; using fxezquantize assures that no
// loss of data occurs repeatedly loading and saving the same file!
FXbool fxezquantize(FXuchar* dst,const FXColor* src,FXColor* colormap,FXint& actualcolors,FXint w,FXint h,FXint maxcolors){
  register FXint   npixels=w*h;
  register FXint   ncolors=0;
  register FXColor color;
  register FXint   i,p,x;
  FXColor  colortable[337];             // Colors encountered in image
  FXushort mapindex[337];               // Map index assigned to color

  FXASSERT(maxcolors<=256);

  // Clear map index
  memset(mapindex,0xff,sizeof(mapindex));

  // Hash all colors from image
  for(i=0; i<npixels; i++){

    // Get pixel
    color=src[i];

    // Find position in table
    p=color%337;
    x=color%331+1;
    while(mapindex[p]!=0xffff){         // Empty slot
      if(colortable[p]==color) goto nxt;
      p=(p+x)%337;
      }

    // If no more room in colormap, we failed
    if(ncolors>=maxcolors) return FALSE;

    // Add new color
    colortable[p]=color;                // Add color to color hash table
    colormap[ncolors]=color;            // Add color to color map
    mapindex[p]=ncolors;                // Remember map index of this color
    ncolors++;

    // Next pixel
nxt:continue;
    }

  // Now loop through image, assigning map indices; all colors
  // must be in the map, so each lookup will be successful.
  for(i=0; i<npixels; i++){

    // Get pixel
    color=src[i];

    // Find position in table
    p=color%337;
    x=color%331+1;
    while(colortable[p]!=color){
      p=(p+x)%337;
      }

    // Output map index
    dst[i]=(FXuchar)mapindex[p];
    }

  // Actual number of colors used
  actualcolors=ncolors;

  return TRUE;
  }

}
