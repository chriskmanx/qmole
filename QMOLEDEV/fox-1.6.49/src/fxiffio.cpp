/********************************************************************************
*                                                                               *
*                          I F F   I n p u t / O u t p u t                      *
*                                                                               *
*********************************************************************************
* Copyright (C) 2004,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: fxiffio.cpp,v 1.14.2.2 2006/08/02 01:31:11 fox Exp $                         *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"


/*
  Notes:
  - EA IFF 85 Electronic Arts' standard for interchange format files.
  - Based on "EA IFF 85" Standard for Interchange Format Files, Jerry Morrison,
    January 14, 1985.
  - And on "ILBM" IFF Interleaved Bitmap specification, Jerry Morrison,
    January 17, 1986.
  - Some day, when I have some sample files, I may support Maya IFF also.

*/

using namespace FX;

/*******************************************************************************/

namespace FX {


extern FXAPI bool fxcheckIFF(FXStream& store);
extern FXAPI bool fxloadIFF(FXStream& store,FXColor*& data,FXint& width,FXint& height);


// Tags
enum{
  FORM = 0x464F524D,
  FOR1 = 0x464F5231,
  FOR2 = 0x464F5232,
  FOR3 = 0x464F5233,
  FOR4 = 0x464F5234,
  ILBM = 0x494C424D,
  BMHD = 0x424D4844,
  CMAP = 0x434D4150,
  CAMG = 0x43414D47,
  BODY = 0x424F4459,
  FLDS = 0x464C4453,
  FMT1 = 0x464D5401,
  TBMP = 0x54424D50,
  CIMG = 0x43494D47,
  TBHD = 0x54424844,
  RGBA = 0x52474241,
  CLPZ = 0x434C505A,
  ESXY = 0x45535859,
  ZBUF = 0x5A425546,
  BLUR = 0x424C5552,
  BLRT = 0x424C5254,
  HIST = 0x48495354,
  CAT  = 0x43415420,
  GRAB = 0x47524142,
  PBM  = 0x50424D20
  };


// Format
enum {
 ILBM_NORMAL,
 ILBM_EHB,
 ILBM_HAM,
 ILBM_HAM8,
 ILBM_24BIT
 };


// Read short
static inline FXuint read16(FXStream& store){
  FXuchar c1,c2;
  store >> c1 >> c2;
  return ((FXuint)c2) | (((FXuint)c1)<<8);
  }


// Read int
static inline FXuint read32(FXStream& store){
  FXuchar c1,c2,c3,c4;
  store >> c1 >> c2 >> c3 >> c4;
  return ((FXuint)c4) | (((FXuint)c3)<<8) | (((FXuint)c2)<<16) | (((FXuint)c1)<<24);
  }


// Check if stream contains a IFF
bool fxcheckIFF(FXStream& store){
  FXuint signature;
  signature=read32(store);
  store.position(-4,FXFromCurrent);
  return signature==FORM || signature==FOR1 || signature==FOR2 || signature==FOR3 || signature==FOR4;
  }


// Load IFF image from stream
bool fxloadIFF(FXStream& store,FXColor*& data,FXint& width,FXint& height){
  FXuint pixels,bit,view,tag,size,type,colors,i,bytesperline,value,plane,remainingbytes,fmt;
  FXuchar *buffer,*ptr,planes,masking,compress,padding,c1,c2,c3,color,count;
  FXColor colormap[256],*dest,pixelcolor;
  FXlong pos,end;
  FXint x,y;

  // Init
  data=NULL;
  buffer=NULL;
  width=0;
  height=0;
  pixels=0;
  view=0;
  colors=0;
  bytesperline=0;
  pixelcolor=0;
  fmt=ILBM_NORMAL;

  // Read tag
  tag=read32(store);

  //FXTRACE((1,"fxloadIFF tag=%c%c%c%c\n",(tag>>24)&255,(tag>>16)&255,(tag>>8)&255,tag&255));

  // Check for FORM tag
  if(tag!=FORM && tag!=FOR1 && tag!=FOR2 && tag!=FOR3 && tag!=FOR4) return false;

  // Read size
  size=read32(store);

  // Calculate end of data
  pos=store.position();
  end=pos+size;

  // Read type
  type=read32(store);
  if(type!=ILBM) return false;

  // Read stuff
  while(1){

    // Bail if at the end
    if(store.position()+8>end) return false;

    // Read next chunk header
    tag=read32(store);
    size=read32(store);
    pos=store.position();

    // Empty block is a problem too
    if(size==0) return false;

    //FXTRACE((1,"CHUNK %c%c%c%c POS=%d SIZE=%d\n",(tag>>24)&255,(tag>>16)&255,(tag>>8)&255,tag&255,pos,size));

    // Bitmap header
    if(tag==BMHD){
      width=read16(store);
      height=read16(store);
      store.position(4,FXFromCurrent);
      store >> planes;
      store >> masking;
      store >> compress;
      store >> padding;
      store.position(8,FXFromCurrent);
      pixels=width*height;                      // Total image size
      bytesperline=((width+15)>>4)<<1;          // Bytes per line
      }

    // Colormap
    else if(tag==CMAP){
      colors=size/3;
      if(colors<1) return false;
      if(colors>256) return false;
      memset(colormap,0,sizeof(colormap));
      for(i=0; i<colors; i++){                  // Load colors
        store >> c1 >> c2 >> c3;
        colormap[i]=FXRGB(c1,c2,c3);
        }
      }

    // Commodore AMiGa
    else if(tag==CAMG){
      view=read32(store);
      //FXTRACE((1,"view=%04x\n",view));
      }

    // Body
    else if(tag==BODY){
      break;
      }

    // Next chunk
    store.position(pos+size+(size&1));
    }

  // Wat voor vlees in de kuip?
  //FXTRACE((1,"fxloadIFF: width=%d height=%d planes=%d masking=%d compress=%d padding=%d\n",width,height,planes,masking,compress,padding));

  // Determine format
  if(planes==24){
    fmt=ILBM_24BIT;
    }
  else if(planes==8){
    if(view&0x800){
      fmt=ILBM_HAM8;
      }
    }
  else if(planes>5){
    if(view&0x80){
      fmt=ILBM_EHB;
      }
    else if(view&0x800){
      fmt=ILBM_HAM;
      }
    }

  // Tweak colormap
  if((fmt==ILBM_NORMAL) || (fmt==ILBM_EHB) || (fmt==ILBM_HAM)){

    // Colors based on HAM mode
    switch(fmt) {
      case ILBM_NORMAL: colors=1<<planes; break;
      case ILBM_EHB: colors=32*2; break;
      case ILBM_HAM: colors=16; break;
      }

    // Divide colors
    for(i=0; i<colors; i++){
      ((FXuchar*)&colormap[i])[0]=(((FXuchar*)&colormap[i])[0]>>4)*17;
      ((FXuchar*)&colormap[i])[1]=(((FXuchar*)&colormap[i])[1]>>4)*17;
      ((FXuchar*)&colormap[i])[2]=(((FXuchar*)&colormap[i])[2]>>4)*17;
      }

    // Extended half-bright mode
    if(fmt==ILBM_EHB){
      for(i=0; i<32; i++) {
        ((FXuchar*)&colormap[colors+i])[0]=((FXuchar*)&colormap[i])[0]>>1;
        ((FXuchar*)&colormap[colors+i])[1]=((FXuchar*)&colormap[i])[1]>>1;
        ((FXuchar*)&colormap[colors+i])[2]=((FXuchar*)&colormap[i])[2]>>1;
        }
      }
    }

  // Try allocate image
  if(!FXMALLOC(&data,FXColor,pixels)) return false;

  // Temp buffer
  if(!FXMALLOC(&buffer,FXuchar,bytesperline*planes)){ FXFREE(&data); return false; }

  dest=data;

  // Loop over y
  for(y=0; y<height; y++){

    // Uncompress data of each planes
    for(plane=0; plane<planes; plane++){
      ptr=buffer+bytesperline*plane;
      remainingbytes=bytesperline;
      if(compress==1){	        // Compressed
        do{
          store >> count;
          if(count&0x80){
            count^=0xFF;
            count+=2;
            store >> color;
            if(count>remainingbytes) count=remainingbytes;
            memset(ptr,color,count);
            }
          else{
            count+=1;
            if(count>remainingbytes) count=remainingbytes;
            store.load(ptr,count);
            }
          ptr+=count;
          remainingbytes-=count;
          }
        while(remainingbytes>0);
        }
      else{                     // Not compressed
        store.load(ptr,bytesperline);
        }
      }

/*
    FXTRACE((100,"y: %d\n",y));
    for(plane=0; plane<planes; plane++){
      FXTRACE((100,"P %2d: ",plane));
      for(i=0; i<bytesperline; i++){
        FXTRACE((100,"%02x ",buffer[plane*bytesperline+i]));
        }
      FXTRACE((100,"\n"));
      }
*/

    // Loop over x
    for(x=0; x<width; x++){

      // Build value from the planes
      for(plane=value=0; plane<planes; plane++){
        bit=(buffer[bytesperline*plane+(x>>3)]>>(7-(x&7)))&1;
        value|=bit<<plane;
        }

      // Determine what to do based on mode
      switch(fmt){
        case ILBM_NORMAL:
        case ILBM_EHB:
          pixelcolor=colormap[value&255];
          break;
        case ILBM_24BIT:
          pixelcolor=value;
          break;
        case ILBM_HAM:
          switch(value&0x30) {
            case 0x00: pixelcolor=colormap[value&15]; break;
            case 0x10: pixelcolor=(pixelcolor&FXRGB(255,255,0))|FXRGB(0,0,(value&15)*17); break;
            case 0x20: pixelcolor=(pixelcolor&FXRGB(0,255,255))|FXRGB((value&15)*17,0,0); break;
            case 0x30: pixelcolor=(pixelcolor&FXRGB(255,0,255))|FXRGB(0,(value&15)*17,0); break;
            }
          break;
        case ILBM_HAM8:
          switch(value&0xc0) {
            case 0x00: pixelcolor=colormap[value&0x3f]; break;
            case 0x40: pixelcolor=(pixelcolor&FXRGB(255,255,0))|FXRGB(0,0,(value&0x3f)<<2); break;
            case 0x80: pixelcolor=(pixelcolor&FXRGB(0,255,255))|FXRGB((value&0x3f)<<2,0,0); break;
            case 0xc0: pixelcolor=(pixelcolor&FXRGB(255,0,255))|FXRGB(0,(value&0x3f)<<2,0); break;
            }
          break;
        }
      *dest++=pixelcolor;
      }
    }

  // Release buffer
  FXFREE(&buffer);

  return true;
  }



}
