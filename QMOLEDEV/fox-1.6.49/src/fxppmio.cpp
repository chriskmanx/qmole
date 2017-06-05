/********************************************************************************
*                                                                               *
*                          P P M   I n p u t / O u t p u t                      *
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
* $Id: fxppmio.cpp,v 1.13.2.2 2009/01/16 01:20:37 fox Exp $                         *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"



/*
  Notes:
  - Definitely a 'no-frills' format.
  - Certainly not optimized for speed; but it works.
  - No support for values greater than 255.
*/

using namespace FX;

/*******************************************************************************/

namespace FX {


extern FXAPI bool fxcheckPPM(FXStream& store);
extern FXAPI bool fxloadPPM(FXStream& store,FXColor*& data,FXint& width,FXint& height);
extern FXAPI bool fxsavePPM(FXStream& store,const FXColor *data,FXint width,FXint height);


// Read one integer
static FXint getint(FXStream& store){
  register FXint num=0;
  FXuchar c;
  while(!store.eof()){
    store >> c;
    if('0'<=c && c<='9') break;
    if(c=='#'){
      while(!store.eof()){
        store >> c;
        if(c=='\n') break;
        }
      }
    }
  while(!store.eof()){
    num=num*10+c-'0';
    store >> c;
    if(c<'0' || c>'9') break;
    }
  return num;
  }


// Check if stream contains a PPM
bool fxcheckPPM(FXStream& store){
  FXuchar signature[2];
  store.load(signature,2);
  store.position(-2,FXFromCurrent);
  return signature[0]=='P' && '1'<=signature[1] && signature[1]<='6';
  }


// Load image from stream
bool fxloadPPM(FXStream& store,FXColor*& data,FXint& width,FXint& height){
  register FXint npixels,i,j,maxvalue=1;
  register FXuchar *pp;
  FXuchar magic,format,byte,r,g,b;

  // Null out
  data=NULL;
  width=0;
  height=0;

  // Check magic byte
  store >> magic;
  if(magic!='P') return false;

  // Check format
  // "P1" = ascii bitmap, "P2" = ascii greymap, "P3" = ascii pixmap,
  // "P4" = raw bitmap, "P5" = raw greymap, "P6" = raw pixmap
  store >> format;
  if(format<'1' || format>'6') return false;

  // Get size
  width=getint(store);
  height=getint(store);
  if(width<1 || height<1) return false;
  npixels=width*height;

  // Get maximum value
  if(format!='1' && format!='4'){
    maxvalue=getint(store);
    if(maxvalue<=0 || maxvalue>=256) return false;
    }

  //FXTRACE((1,"fxloadPPM: width=%d height=%d type=%c \n",width,height,format));

  // Allocate buffer
  if(!FXCALLOC(&data,FXColor,npixels)) return false;

  // Read it
  pp=(FXuchar*)data;
  switch(format){
    case '1':   // ascii bitmap
      for(i=0; i<height; i++){
        for(j=0; j<width; j++,byte<<=1,pp+=4){
          byte=getint(store);
          g=byte?255:0;
          pp[0]=g;
          pp[1]=g;
          pp[2]=g;
          pp[3]=255;
          }
        }
      break;
    case '2':   // ascii greymap
      for(i=0; i<height; i++){
        for(j=0; j<width; j++,pp+=4){
          g=getint(store);
          pp[0]=g;
          pp[1]=g;
          pp[2]=g;
          pp[3]=255;
          }
        }
      break;
    case '3':   // ascii pixmap
      for(i=0; i<height; i++){
        for(j=0; j<width; j++,pp+=4){
          r=getint(store);
          g=getint(store);
          b=getint(store);
          pp[0]=r;
          pp[1]=g;
          pp[2]=b;
          pp[3]=255;
          }
        }
      break;
    case '4':   // binary bitmap
      for(i=0; i<height; i++){
        for(j=0; j<width; j++,byte<<=1,pp+=4){
          if((j&7)==0){ store >> byte; }
          g=(byte&0x80)?255:0;
          pp[0]=g;
          pp[1]=g;
          pp[2]=g;
          pp[3]=255;
          }
        }
      break;
    case '5':   // binary greymap
      for(i=0; i<height; i++){
        for(j=0; j<width; j++,pp+=4){
          store >> g;
          pp[0]=g;
          pp[1]=g;
          pp[2]=g;
          pp[3]=255;
          }
        }
      break;
    case '6':   // binary pixmap
      for(i=0; i<height; i++){
        for(j=0; j<width; j++,pp+=4){
          store >> r;
          store >> g;
          store >> b;
          pp[0]=r;
          pp[1]=g;
          pp[2]=b;
          pp[3]=255;
          }
        }
      break;
    }

  return true;
  }


/*******************************************************************************/


// Save a bmp file to a stream
bool fxsavePPM(FXStream& store,const FXColor *data,FXint width,FXint height){
  register const FXuchar *pp=(const FXuchar*)data;
  register FXint i,j,nsize;
  FXchar size[20];

  // Must make sense
  if(!pp || width<=0 || height<=0) return false;

  // Save header
  store.save("P6\n",3);
  nsize=sprintf(size,"%d %d\n",width,height);
  store.save(size,nsize);
  store.save("255\n",4);

  // 24-bit/pixel
  for(i=0; i<height; i++){
    for(j=0; j<width; j++){
      store << *pp++;
      store << *pp++;
      store << *pp++;
      pp++;
      }
    }
  return true;
  }

}
