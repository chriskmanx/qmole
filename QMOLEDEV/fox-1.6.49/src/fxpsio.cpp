/********************************************************************************
*                                                                               *
*                        P o s t S c r i p t   O u t p u t                      *
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
* $Id: fxpsio.cpp,v 1.11.2.1 2006/04/14 01:21:01 fox Exp $                          *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"



/*
  Notes:
  - Generates EPS file to a stream; you can pass in the paper
    dimensions in points.
  - There is no PostScript input.
*/

using namespace FX;

/*******************************************************************************/

namespace FX {


extern FXAPI bool fxsavePS(FXStream& store,const FXColor *data,FXint width,FXint height,FXint paperw=612,FXint paperh=792,FXint margin=35,bool color=true);


// Spit output to stream
static void output(FXStream& store,const char* fmt,...){
  FXchar buffer[1024]; FXint len;
  va_list args;
  va_start(args,fmt);
#if defined(WIN32) || defined(HAVE_VSNPRINTF)
  len=vsnprintf(buffer,sizeof(buffer),fmt,args);
#else
  len=vsprintf(buffer,fmt,args);
#endif
  va_end(args);
  store.save(buffer,len);
  }


// Save image to PostScript file
bool fxsavePS(FXStream& store,const FXColor* data,FXint width,FXint height,FXint paperw,FXint paperh,FXint margin,bool color){
  register FXint bx,by,bxx,byy,x,y;
  register FXuchar *p;

  // Must make sense
  if(!data || width<=0 || height<=0 || paperh<=0 || paperw<=0 || margin<=0) return false;

  // Figure out scale; maximize for the paper size
  bxx=paperw-margin*2;
  byy=(height*bxx)/width;
  if((paperh-margin*2)<byy){
    byy=paperh-margin*2;
    bxx=(width*byy)/height;
    }

  bx=margin+(paperw-margin*2-bxx)/2;
  by=margin+(paperh-margin*2-byy)/2;

  // Output header
  output(store,"%%!PS-Adobe-2.0 EPSF-2.0\n");
  output(store,"%%%%Title: Image\n");
  output(store,"%%%%Creator: FOX Toolkit\n");
  output(store,"%%%%BoundingBox: %i %i %i %i\n",bx,by,bx+bxx,by+byy);
  output(store,"%%%%Pages: 1\n");
  output(store,"%%%%DocumentFonts:\n");
  output(store,"%%%%EndComments\n");
  output(store,"%%%%EndProlog\n");
  output(store,"%%%%Page: 1 1\n");
  output(store,"/origstate save def\n");
  output(store,"20 dict begin\n");

  // Color
  if(color){

    // When colorimage not available, use image instead
    output(store,"/bwproc\n");
    output(store," {  rgbproc\n");
    output(store,"    dup length 3 idiv string 0 3 0\n");
    output(store,"    5 -1 roll\n");
    output(store,"    { add 2 1 roll 1 sub dup 0 eq\n");
    output(store,"      { pop 3 idiv 3 -1 roll dup 4 -1 roll dup\n");
    output(store,"        3 1 roll 5 -1 roll put 1 add 3 0 }\n");
    output(store,"      { 2 1 roll } ifelse\n");
    output(store,"    } forall\n");
    output(store,"    pop pop pop\n");
    output(store,"} def\n");
    output(store,"systemdict /colorimage known not\n");
    output(store," { /colorimage\n");
    output(store,"     { pop pop /rgbproc exch def\n");
    output(store,"     { bwproc } image\n");
    output(store," } def\n");
    output(store,"} if\n");

    // Set up color image operator
    output(store,"/pix %i string def\n",width*3);
    output(store,"%i %i translate\n",bx,by);
    output(store,"%i %i scale\n",bxx,byy);
    output(store,"%i %i 8\n",width,height);
    output(store,"[%i 0 0 -%i 0 %i]\n",width,height,height);
    output(store,"{currentfile pix readhexstring pop}\n");
    output(store,"false 3 colorimage\n");
    output(store,"\n");

    // Output pixels
    p=(FXuchar*)data;
    for(y=0; y<height; y++){
      for(x=0; x<width; x++,p+=4){
        output(store,"%02x",p[0]);
        output(store,"%02x",p[1]);
        output(store,"%02x",p[2]);
        }
      output(store, "\n");
      }
    }

  // Gray scale
  else{

    // Set up grayscale image operator
    output(store,"/pix %i string def\n",width);
    output(store,"%i %i translate\n",bx,by);
    output(store,"%i %i scale\n",bxx,byy);
    output(store,"%i %i 8\n",width,height);
    output(store,"[%i 0 0 -%i 0 %i]\n",width,height,height);
    output(store,"{currentfile pix readhexstring pop}\n");
    output(store,"image\n");
    output(store,"\n");

    // Output pixels
    p=(FXuchar*)data;
    for(y=0; y<height; y++){
      for(x=0; x<width; x++,p+=4){
        output(store,"%02x",(77*(FXuint)p[0]+151*(FXuint)p[1]+28*(FXuint)p[2])/256);
        }
      output(store,"\n");
      }
    }

  // Output trailer
  output(store,"\n");
  output(store,"showpage\n");
  output(store,"end\n");
  output(store,"origstate restore\n");
  output(store,"%%%%Trailer\n");
  return true;
  }

}
