/********************************************************************************
*                                                                               *
*                     F S   C o l o r   Q u a n t i z a t i o n                 *
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
* $Id: fxfsquantize.cpp,v 1.5 2006/01/22 17:58:52 fox Exp $                     *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxpriv.h"


/*
  Notes:

  - The fxfsquantize is a floyd-steinberg dither.  It is quite fast
    but not as good as Heckbert's or Wu's.
*/


using namespace FX;


// Up to 256 colors: 3 bits R, 3 bits G, 2 bits B  (RRRGGGBB)
#define REDMASK          0xe0
#define REDSHIFT         0
#define GREENMASK        0xe0
#define GREENSHIFT       3
#define BLUEMASK         0xc0
#define BLUESHIFT        6

/*******************************************************************************/

namespace FX {


// Floyd-Steinberg quantization full 32 bpp to less than or equal to maxcolors
FXbool fxfsquantize(FXuchar* dst,const FXColor* src,FXColor* colormap,FXint& actualcolors,FXint w,FXint h,FXint){
  register FXint i,j,val,r1,g1,b1,*cr,*cg,*cb,*nr,*ng,*nb,*p;
  FXint *begin;

  // Fill colormap
  for(r1=i=0; r1<8; r1++){
    for(g1=0; g1<8; g1++){
      for(b1=0; b1<4; b1++){
        ((FXuchar*)(colormap+i))[0]=(r1*255+3)/7;
        ((FXuchar*)(colormap+i))[1]=(g1*255+3)/7;
        ((FXuchar*)(colormap+i))[2]=(b1*255+1)/3;
        ((FXuchar*)(colormap+i))[3]=255;
        i++;
        }
      }
    }

  // Temporary storage
  if(!FXMALLOC(&begin,FXint,w*2*3)) return FALSE;
  cr=begin;
  cg=cr+w;
  cb=cg+w;
  nr=cb+w;
  ng=nr+w;
  nb=ng+w;

  // Get first line of picture
  for(j=0; j<w; j++){
    nr[j]=((const FXuchar*)(src+j))[0];
    ng[j]=((const FXuchar*)(src+j))[1];
    nb[j]=((const FXuchar*)(src+j))[2];
    }
  src+=w;

  // Dither loop
  for(i=0; i<h; i++){

    // Swap lines
    FXSWAP(cr,nr,p);
    FXSWAP(cg,ng,p);
    FXSWAP(cb,nb,p);

    // Get next line
    if(i!=h-1){
      for(j=0; j<w; j++){
        nr[j]=((const FXuchar*)(src+j))[0];
        ng[j]=((const FXuchar*)(src+j))[1];
        nb[j]=((const FXuchar*)(src+j))[2];
        }
      src+=w;
      }

    // Dither
    for(j=0; j<w; j++){
      r1=cr[j]; r1=FXCLAMP(0,r1,255);
      g1=cg[j]; g1=FXCLAMP(0,g1,255);
      b1=cb[j]; b1=FXCLAMP(0,b1,255);

      // choose actual pixel value
      val=(((r1&REDMASK)>>REDSHIFT)|((g1&GREENMASK)>>GREENSHIFT)|((b1&BLUEMASK)>>BLUESHIFT));
      *dst++=val;

      // compute color errors
      r1-=((FXuchar*)(colormap+val))[0];
      g1-=((FXuchar*)(colormap+val))[1];
      b1-=((FXuchar*)(colormap+val))[2];

      // Add fractions of errors to adjacent pixels
      if(j!=w-1){                       // Adjust RIGHT pixel
        cr[j+1]+=(r1*7)/16;
        cg[j+1]+=(g1*7)/16;
        cb[j+1]+=(b1*7)/16;
        }
      if(i!=h-1){                       // do BOTTOM pixel
        nr[j]+=(r1*5)/16;
        ng[j]+=(g1*5)/16;
        nb[j]+=(b1*5)/16;
        if(j>0){                        // do BOTTOM LEFT pixel
          nr[j-1]+=(r1*3)/16;
          ng[j-1]+=(g1*3)/16;
          nb[j-1]+=(b1*3)/16;
          }
        if(j!=w-1){                     // do BOTTOM RIGHT pixel
          nr[j+1]+=(r1)/16;
          ng[j+1]+=(g1)/16;
          nb[j+1]+=(b1)/16;
          }
        }
      }
    }
  FXFREE(&begin);
  actualcolors=256;
  return TRUE;
  }


}
