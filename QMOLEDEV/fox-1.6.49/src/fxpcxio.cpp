/********************************************************************************
*                                                                               *
*                          P C X   I n p u t / O u t p u t                      *
*                                                                               *
*********************************************************************************
* Copyright (C) 2001,2006 by Janusz Ganczarski.   All Rights Reserved.          *
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
* $Id: fxpcxio.cpp,v 1.31.2.1 2006/08/01 18:04:46 fox Exp $                         *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"



/*
  Notes:
  - Loading 1-bit/1-plane, 4-bit/1-plane, 8-bit/1-plane and 8-bit/3-plane
    images appears to work.
  - Need to check if fewer colors, if so fall back on lower pixel depth
    mode to save space.
  - Needs to be updated when new stream classes are here (can do byte
    swap on writing).
*/

using namespace FX;

/*******************************************************************************/

namespace FX {


extern FXAPI bool fxcheckPCX(FXStream& store);
extern FXAPI bool fxloadPCX(FXStream& store,FXColor*& data,FXint& width,FXint& height);
extern FXAPI bool fxsavePCX(FXStream& store,const FXColor *data,FXint width,FXint height);


static inline FXuint read16(FXStream& store){
  FXuchar c1,c2;
  store >> c1 >> c2;
  return ((FXuint)c1) | (((FXuint)c2)<<8);
  }


// Check if stream contains a PCX
bool fxcheckPCX(FXStream& store){
  FXuchar signature[4];
  store.load(signature,4);
  store.position(-4,FXFromCurrent);
  return signature[0]==10 && (signature[3]==1 || signature[3]==2 || signature[3]==4 || signature[3]==8);
  }


// Read possibly compressed scanline
static void readscanline(FXStream& store,FXuchar line[],int size,int compressed){
  register FXint count,i=0;
  FXuchar cc;
  if(compressed==1){
    while(i<size){
      count=1;
      store >> cc;
      if(cc>0xc0){
        count=cc-0xc0;
        store >> cc;
        }
      while(count-- && i<size){
        line[i++]=cc;
        }
      }
    }
  else{
    while(i<size){
      store >> cc;
      line[i++]=cc;
      }
    }
  }


// Load PCX image from stream
bool fxloadPCX(FXStream& store,FXColor*& data,FXint& width,FXint& height){
  const FXuchar Mono[2]={0,255};
  FXuchar Colormap[256][3];
  FXuchar Manufacturer;
  FXuchar Version;
  FXuchar Encoding;
  FXuchar BitsPerPixel;
  FXuchar NPlanes;
  FXuchar Reserved;
  FXuchar fill;
  FXuchar *pp;
  FXuchar *line;
  FXint   Xmin;
  FXint   Ymin;
  FXint   Xmax;
  FXint   Ymax;
  FXint   BytesPerLine;
  FXint   NumPixels;
  FXint   totalBytes;
  FXuchar c;
  FXint   i,clr,x,y,index,shift;

  // Null out
  data=NULL;
  width=0;
  height=0;

  // Check Manufacturer
  store >> Manufacturer;
  if(Manufacturer!=10) return false;

  // Get Version
  store >> Version;

  // Get Encoding
  store >> Encoding;

  // Get BitsPerPixel
  store >> BitsPerPixel;

  // One of these four possibilities?
  if(BitsPerPixel!=1 && BitsPerPixel!=2 && BitsPerPixel!=4 && BitsPerPixel!=8) return false;

  // Get Xmin, Ymin, Xmax, Ymax
  Xmin=read16(store);
  Ymin=read16(store);
  Xmax=read16(store);
  Ymax=read16(store);

  // Calculate Width and Height
  width=Xmax-Xmin+1;
  height=Ymax-Ymin+1;

  // Total number of pixels
  NumPixels=width*height;

  // HDpi, VDpi
  read16(store);
  read16(store);

  // Get EGA/VGA Colormap
  store.load(Colormap[0],48);

  // Get Reserved
  store >> Reserved;

  // Get NPlanes
  store >> NPlanes;

  // Does it make sense?
  if(NPlanes!=1 && NPlanes!=3 && NPlanes!=4) return false;

  // Get BytesPerLine
  BytesPerLine=read16(store);

  // Total bytes for scanline
  totalBytes=BytesPerLine*NPlanes;

  // Read over PaletteInfo
  read16(store);

  // Get 58 bytes, to get to 128 byte header
  for(i=0; i<58; i++) store >> fill;

  //FXTRACE((1,"fxloadPCX: width=%d height=%d Version=%d BitsPerPixel=%d NPlanes=%d BytesPerLine=%d Encoding=%d\n",width,height,Version,BitsPerPixel,NPlanes,BytesPerLine,Encoding));

  // Allocate memory
  if(!FXCALLOC(&data,FXColor,NumPixels)) return false;

  // Scanline buffer
  if(!FXMALLOC(&line,FXuchar,totalBytes)){ FXFREE(&line); return false; }

  // Load 1 bit/pixel
  if(BitsPerPixel==1 && NPlanes==1){
    pp=(FXuchar*)data;
    for(y=0; y<height; y++){
      readscanline(store,line,BytesPerLine,Encoding);
      for(x=0; x<width; x++){
        clr=((FXuchar)(line[x>>3]<<(x&7))>>7);
        *pp++=Mono[clr];
        *pp++=Mono[clr];
        *pp++=Mono[clr];
        *pp++=255;
        }
      }
    }

  // Load 8 bit/pixel
  else if(BitsPerPixel==8 && NPlanes==1){
    pp=(FXuchar*)data;
    for(y=0; y<height; y++){
      readscanline(store,line,BytesPerLine,Encoding);
      for(x=0; x<width; x++,pp+=4){
        *pp=line[x];
        }
      }
    store >> c;                   // Get VGApaletteID
    if(c!=12) return FALSE;       // Check VGApaletteID
    store.load(Colormap[0],768);
    pp=(FXuchar*)data;
    for(i=0; i<NumPixels; i++){   // Apply colormap
      clr=pp[0];
      *pp++=Colormap[clr][0];
      *pp++=Colormap[clr][1];
      *pp++=Colormap[clr][2];
      *pp++=255;
      }
    }

  // Load 24 bits/pixel
  else if(BitsPerPixel==8 && NPlanes==3){
    pp=(FXuchar*)data;
    for(y=0; y<height; y++){
      readscanline(store,line,totalBytes,Encoding);
      for(x=0; x<width; x++){
        *pp++=line[x];
        *pp++=line[BytesPerLine+x];
        *pp++=line[(BytesPerLine<<1)+x];
        *pp++=0xFF;
        }
      }
    }

  // Load 4 bit/pixel
  else if((BitsPerPixel==4) || (BitsPerPixel==1 && NPlanes==4)){
    pp=(FXuchar*)data;
    for(y=0; y<height; y++){
      readscanline(store,line,BytesPerLine*4,Encoding);
      for(x=0; x<width; x++){
        clr=0;
        index=x>>3;
        shift=7-(x&7);
        clr|=0x01&((line[index]>>shift));
        clr|=0x02&((line[index+BytesPerLine]>>shift)<<1);
        clr|=0x04&((line[index+BytesPerLine*2]>>shift)<<2);
        clr|=0x08&((line[index+BytesPerLine*3]>>shift)<<3);
        *pp++=Colormap[clr][0];
        *pp++=Colormap[clr][1];
        *pp++=Colormap[clr][2];
        *pp++=255;
        }
      }
    }

  // Done with that
  FXFREE(&line);

  return true;
  }



/*******************************************************************************/


static inline void write16(FXStream& store,FXuint i){
  FXuchar c1,c2;
  c1=i&0xff;
  c2=(i>>8)&0xff;
  store << c1 << c2;
  }


// Save a PCX file to a stream
bool fxsavePCX(FXStream& store,const FXColor *data,FXint width,FXint height){
  const FXuchar Colormap[16][3]={{0,0,0},{255,255,255},{0,170,0},{0,170,170},{170,0,0},{170,0,170},{170,170,0},{170,170,170},{85,85,85},{85,85,255},{85,255,85},{85,255,255},{255,85,85},{255,85,255},{255,255,85},{255,255,255}};
  const FXuchar Manufacturer=10;
  const FXuchar Version=5;
  const FXuchar Encoding=1;
  const FXuchar BitsPerPixel=8;
  const FXuchar NPlanes=3;
  const FXuchar Reserved=0;
  const FXshort PaletteInfo=1;
  const FXshort HRes=75;
  const FXshort VRes=75;
  const FXuchar fill=0;
  const FXuchar *pp;
  const FXshort Xmin=0;
  const FXshort Ymin=0;
  FXshort       Xmax=width-1;
  FXshort       Ymax=height-1;
  FXint         i,x,y,rgb;
  FXuchar       Current,Last,RLECount,rc;

  // Must make sense
  if(!data || width<=0 || height<=0) return false;

  // Manufacturer, Version, Encoding and BitsPerPixel
  store << Manufacturer;
  store << Version;
  store << Encoding;
  store << BitsPerPixel;

  // Xmin = 0
  write16(store,Xmin);

  // Ymin = 0
  write16(store,Ymin);

  // Xmax = width - 1
  write16(store,Xmax);

  // Ymax = height - 1
  write16(store,Ymax);

  // HDpi = 75
  write16(store,HRes);

  // VDpi = 75
  write16(store,VRes);

  // Colormap
  store.save(Colormap[0],48);

  // Reserved
  store << Reserved;

  // NPlanes
  store << NPlanes;

  // BytesPerLine = width
  write16(store,width);

  // PaletteInfo=1
  write16(store,PaletteInfo);

  // Filler
  for(i=0; i<58; i++) store << fill;

  // Save as 24 bits/pixel
  for(y=0; y<height; y++){
    for(rgb=0; rgb<3; rgb++){
      pp=((FXuchar*)(data+y*width))+rgb;
      Last=*pp;
      pp+=4;
      RLECount=1;
      for(x=1; x<width; x++){
        Current=*pp;
        pp+=4;
        if(Current==Last){
          RLECount++;
          if(RLECount==63){
            rc=0xC0|RLECount;
            store << rc << Last;
            RLECount=0;
            }
          }
        else{
          if(RLECount){
            if((RLECount==1) && (0xC0!=(0xC0&Last))){
              store << Last;
              }
            else{
              rc=0xC0|RLECount;
              store << rc << Last;
              RLECount = 1;
              }
            }
          Last=Current;
          RLECount=1;
          }
        }
      if(RLECount){
        if((RLECount==1) && (0xC0!=(0xC0&Last))){
          store << Last;
          }
        else{
          rc=0xC0|RLECount;
          store << rc << Last;
          }
        }
      }
    }
  return true;
  }

}
