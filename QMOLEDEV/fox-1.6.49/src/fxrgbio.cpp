/********************************************************************************
*                                                                               *
*                    I R I S   R G B   I n p u t / O u t p u t                  *
*                                                                               *
*********************************************************************************
* Copyright (C) 2002,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: fxrgbio.cpp,v 1.28 2006/03/18 04:57:03 fox Exp $                         *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"



/*
  Notes:
  - Need to implement RLE compression some time.
  - Bad data may core reader.
*/

using namespace FX;

/*******************************************************************************/

namespace FX {


extern FXAPI bool fxcheckRGB(FXStream& store);
extern FXAPI bool fxloadRGB(FXStream& store,FXColor*& data,FXint& width,FXint& height);
extern FXAPI bool fxsaveRGB(FXStream& store,const FXColor *data,FXint width,FXint height);


// RLE decompress
static void expandrow(FXuchar* optr,FXuchar *iptr){   // FIXME bad data could blow past array!!
  unsigned char pixel, count;
  while(1){
    pixel=*iptr++;
    count=pixel&0x7f;
    if(count==0) return;
    if(pixel&0x80){   // Literal bytes
      while(count--){
	*optr=*iptr++;
	optr+=4;
        }
      }
    else{             // Repeated bytes
      pixel=*iptr++;
      while(count--){
	*optr=pixel;
	optr+=4;
        }
      }
    }
  }


// Check if stream contains a RGB
bool fxcheckRGB(FXStream& store){
  FXuchar signature[2];
  store.load(signature,2);
  store.position(-2,FXFromCurrent);
  return signature[0]==0x01 && signature[1]==0xDA;
  }


// Load image from stream
bool fxloadRGB(FXStream& store,FXColor*& data,FXint& width,FXint& height){
  FXint i,j,c,tablen,sub,t,total;
  FXuchar temp[4096],*array,storage,bpc,swap;
  FXuint *starttab,*lengthtab;
  FXushort magic,dimension,nchannels,w,h;
  FXlong base,start;

  // Null out
  data=NULL;
  width=0;
  height=0;

  // Remember swap state
  swap=store.swapBytes();
  store.setBigEndian(TRUE);

  // Where the image format starts
  base=store.position();

  // Load header
  store >> magic;       // MAGIC (2)
  store >> storage;     // STORAGE (1)
  store >> bpc;         // BPC (1)
  store >> dimension;   // DIMENSION (2)
  store >> w;           // XSIZE (2)
  store >> h;           // YSIZE (2)
  store >> nchannels;   // ZSIZE (2)

  FXTRACE((50,"fxloadRGB: magic=%d width=%d height=%d nchannels=%d dimension=%d storage=%d bpc=%d\n",magic,w,h,nchannels,dimension,storage,bpc));

  // Check magic number and other parameters
  if(magic==474 && nchannels==3 && bpc==1 && w>0 && h>0){

    // Make room for image
    if(FXMALLOC(&data,FXColor,w*h)){

      // Clear
      memset(data,0xff,sizeof(FXColor)*w*h);

      // Skip stuff
      store.position(500,FXFromCurrent);

      // RLE compressed
      if(storage){
        tablen=h*3;

        // Allocate line tables
        if(FXMALLOC(&starttab,FXuint,tablen*2)){
          lengthtab=&starttab[tablen];

          // Read line tables
          store.load(starttab,tablen);
          store.load(lengthtab,tablen);

          // Where the RLE chunks start
          start=store.position();

          // Substract this amount to get offset from chunk start
          sub=start-base;

          total=0;

          // Fix up the line table & figure space for RLE chunks
          // Intelligent RGB writers (not ours ;-)) may re-use RLE
          // chunks for more than 1 line...
          for(i=0; i<tablen; i++){
            starttab[i]-=sub;
            t=starttab[i]+lengthtab[i];
            if(t>total) total=t;
            }

          // Make room for the compressed lines
          if(FXMALLOC(&array,FXuchar,total)){

            // Load all RLE chunks
            store.load(array,total);
            for(c=0; c<3; c++){
              for(j=h-1; j>=0; j--){
                expandrow(((FXuchar*)(data+j*w))+c,&array[starttab[h-1-j+c*h]]);
                }
              }

            // Free RLE chunks
            FXFREE(&array);
            }

          // Free line tables
          FXFREE(&starttab);
          }
        }

      // NON compressed
      else{
        for(c=0; c<3; c++){
          for(j=h-1; j>=0; j--){
            store.load(temp,w);
            for(i=0; i<w; i++) ((FXuchar*)(data+j*w+i))[c]=temp[i];
            }
          }
        }

      // Set width and height
      width=w;
      height=h;

      // Reset swap status
      store.swapBytes(swap);
      return TRUE;
      }
    }

  // Reset swap status
  store.swapBytes(swap);
  return false;
  }


/*******************************************************************************/


// Save a bmp file to a stream
bool fxsaveRGB(FXStream& store,const FXColor *data,FXint width,FXint height){
  const FXushort dimension=3;
  const FXushort nchannels=3;
  const FXushort magic=474;
  const FXuint maxpix=255;
  const FXuint minpix=0;
  const FXuint dummy=0;
  const FXuchar storage=0;
  const FXuchar bpc=1;
  FXuchar temp[4096],swap;
  FXushort w=width;
  FXushort h=height;
  FXint i,j,c;

  // Must make sense
  if(data && 0<width && 0<height){

    // Remember swap state
    swap=store.swapBytes();
    store.setBigEndian(TRUE);

    // Save header
    store << magic;             // MAGIC (2)
    store << storage;           // STORAGE (1)
    store << bpc;               // BPC (1)
    store << dimension;         // DIMENSION (2)
    store << w;                 // XSIZE (2)
    store << h;                 // YSIZE (2)
    store << nchannels;         // ZSIZE (2)
    store << minpix;            // PIXMIN (4)
    store << maxpix;            // PIXMAX (4)
    store << dummy;             // DUMMY (4)
    memset(temp,0,80);          // Clean it
    memcpy(temp,"IRIS RGB",8);  // Write name
    store.save(temp,80);        // IMAGENAME (80)
    store << dummy;             // COLORMAP (4)
    memset(temp,0,404);         // Clean it
    store.save(temp,404);       // DUMMY (404)

    // Write pixels
    for(c=0; c<3; c++){
      for(j=height-1; j>=0; j--){
        for(i=0; i<width; i++) temp[i]=((FXuchar*)(data+j*width+i))[c];
        store.save(temp,width);
        }
      }

    // Reset swap status
    store.swapBytes(swap);
    return true;
    }
  return false;
  }

}
