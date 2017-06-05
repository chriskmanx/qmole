/********************************************************************************
*                                                                               *
*                          I C O   I n p u t / O u t p u t                      *
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
* $Id: fxicoio.cpp,v 1.33 2006/01/22 17:58:52 fox Exp $                         *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"

/*
  Notes:
  - When writing icons, maybe round width/height up to the nearest allowed value.
  - Maybe see if image can be writting with fewer bit/pixel.
  - All padding is zero because the width is 16, 32, or 64.
  - Partially transparent pixels are considered opaque, to prevent loss of
    data.
  - There is no OS/2 ICO format AFAIK.
*/


#define BIH_RGB         0       // biCompression values
#define BIH_RLE8        1
#define BIH_RLE4        2
#define BIH_BITFIELDS   3

using namespace FX;

/*******************************************************************************/

namespace FX {

extern FXAPI bool fxcheckICO(FXStream& store);
extern FXAPI bool fxloadICO(FXStream& store,FXColor*& data,FXint& width,FXint& height,FXint& xspot,FXint& yspot);
extern FXAPI bool fxsaveICO(FXStream& store,const FXColor *data,FXint width,FXint height,FXint xspot=-1,FXint yspot=-1);



// Check if stream contains ICO
bool fxcheckICO(FXStream& store){
  bool swap=store.swapBytes();
  FXshort signature[3];
  store.setBigEndian(FALSE);
  store.load(signature,3);
  store.position(-6,FXFromCurrent);
  store.swapBytes(swap);
  return signature[0]==0 && (signature[1]==1 || signature[1]==2) && signature[2]>=1;
  }


// Load ICO image from stream
bool fxloadICO(FXStream& store,FXColor*& data,FXint& width,FXint& height,FXint& xspot,FXint& yspot){
  FXColor  colormap[256],*pp;
  FXlong   base,header;
  FXshort  idReserved;
  FXshort  idType;
  FXshort  idCount;
  FXshort  wXHotspot;
  FXshort  wYHotspot;
  FXuchar  bWidth;
  FXuchar  bHeight;
  FXuchar  bColorCount;
  FXuchar  bReserved;
  FXint    dwBytesInRes;
  FXint    dwImageOffset;
  FXint    biSize;
  FXint    biWidth;
  FXint    biHeight;
  FXint    biCompression;
  FXint    biSizeImage;
  FXint    biXPelsPerMeter;
  FXint    biYPelsPerMeter;
  FXint    biClrUsed;
  FXint    biClrImportant;
  FXshort  biBitCount;
  FXshort  biPlanes;
  FXint    i,j,colormaplen,pad;
  FXuchar  c1;
  FXushort rgb16;
  bool     swap;
  bool     ok=FALSE;

  // Null out
  data=NULL;
  width=0;
  height=0;
  xspot=0;
  yspot=0;

  // Start of icon file header
  base=store.position();

  // Bitmaps are little-endian
  swap=store.swapBytes();
  store.setBigEndian(false);

  // IconHeader
  store >> idReserved;     // Must be 0
  store >> idType;         // ICO=1, CUR=2
  store >> idCount;        // Number of images

//  FXTRACE((1,"fxloadICO: idReserved=%d idType=%d idCount=%d\n",idReserved,idType,idCount));

  // Check
  if(idReserved!=0 || (idType!=1 && idType!=2) || idCount<1) goto x;

  // IconDirectoryEntry
  store >> bWidth;
  store >> bHeight;
  store >> bColorCount;
  store >> bReserved;
  store >> wXHotspot;
  store >> wYHotspot;
  store >> dwBytesInRes;
  store >> dwImageOffset;

//  FXTRACE((1,"fxloadICO: bWidth=%d bHeight=%d bColorCount=%d bReserved=%d xspot=%d yspot=%d dwImageOffset=%d\n",bWidth,bHeight,bColorCount,bReserved,xspot,yspot,dwImageOffset));

  // Only certain color counts allowed; bColorCount=0 means 256 colors supposedly
  if(bColorCount!=0 && bColorCount!=2 && bColorCount!=4 && bColorCount!=8 && bColorCount!=16) goto x;

  // Jump to BitmapInfoHeader
  store.position(base+dwImageOffset);

  // Start of bitmap info header
  header=store.position();

  // BitmapInfoHeader
  store >> biSize;
  store >> biWidth;
  store >> biHeight; biHeight/=2;   // Huh?
  store >> biPlanes;
  store >> biBitCount;
  store >> biCompression;
  store >> biSizeImage;
  store >> biXPelsPerMeter;
  store >> biYPelsPerMeter;
  store >> biClrUsed;
  store >> biClrImportant;

//  FXTRACE((1,"fxloadICO: biSize=%d biWidth=%d biHeight=%d biBitCount=%d biCompression=%d biClrUsed=%d\n",biSize,biWidth,biHeight,biBitCount,biCompression,biClrUsed));

  // Check for supported depths
  if(biBitCount!=1 && biBitCount!=4 && biBitCount!=8 && biBitCount!=16 && biBitCount!=24 && biBitCount!=32) goto x;

  // Check for supported compression methods
  if(biCompression!=BIH_RGB) goto x;

  // Skip ahead to colormap
  store.position(header+biSize);

  // load up colormap, if any
  colormaplen=0;
  if(biBitCount<=8){
    colormaplen = biClrUsed ? biClrUsed : 1 << biBitCount;
    for(i=0; i<colormaplen; i++){
      store >> c1; ((FXuchar*)(colormap+i))[2]=c1;      // Blue
      store >> c1; ((FXuchar*)(colormap+i))[1]=c1;      // Green
      store >> c1; ((FXuchar*)(colormap+i))[0]=c1;      // Red
      store >> c1; ((FXuchar*)(colormap+i))[3]=255;     // Opaque
      }
    }

  // Allocate memory
  if(!FXMALLOC(&data,FXColor,biWidth*biHeight)) goto x;

  // Width and height
  width=biWidth;
  height=biHeight;
  xspot=wXHotspot;
  yspot=wYHotspot;

  switch(biBitCount){
    case 1:             // 1-bit/pixel
      for(i=biHeight-1; i>=0; i--){
        pp=data+i*biWidth;
        for(j=0; j<biWidth; j++){
          if((j&7)==0){ store >> c1; }
          *pp++=colormap[(c1&0x80)>>7];
          c1<<=1;
          }
        }
      break;
    case 4:             // 4-bit/pixel
      for(i=biHeight-1; i>=0; i--){
        pp=data+i*biWidth;
        for(j=0; j<biWidth; j++){
          if((j&1)==0){ store >> c1; }
          *pp++=colormap[(c1&0xf0)>>4];
          c1<<=4;
          }
        }
      break;
    case 8:             // 8-bit/pixel
      for(i=biHeight-1; i>=0; i--){
        pp=data+i*biWidth;
        for(j=0; j<biWidth; j++){
          store >> c1;
          *pp++=colormap[c1];
          }
        }
      break;
     case 16:           // 16-bit/pixel
       pad=(4-((biWidth*2)&3))&3;
       for(i=biHeight-1; i>=0; i--){
         pp=data+i*biWidth;
         for(j=0; j<biWidth; j++,pp++){
           store >> rgb16;
           ((FXuchar*)pp)[0]=((rgb16>>7)&0xf8)+((rgb16>>12)&0x7);  // Red
           ((FXuchar*)pp)[1]=((rgb16>>2)&0xf8)+((rgb16>> 7)&0x7);  // Green
           ((FXuchar*)pp)[2]=((rgb16<<3)&0xf8)+((rgb16>> 2)&0x7);  // Blue
           ((FXuchar*)pp)[3]=255;                                  // Alpha
           }
         store.position(pad,FXFromCurrent);
         }
      break;
    case 24:            // 24-bit/pixel
      pad=(4-((biWidth*3)&3))&3;
      for(i=biHeight-1; i>=0; i--){
        pp=data+i*biWidth;
        for(j=0; j<biWidth; j++,pp++){
          store >> ((FXuchar*)pp)[2];             // Blue
          store >> ((FXuchar*)pp)[1];             // Green
          store >> ((FXuchar*)pp)[0];             // Red
                   ((FXuchar*)pp)[3]=255;         // Alpha
          }
        store.position(pad,FXFromCurrent);
        }
      break;
    case 32:            // 32-bit/pixel
      for(i=biHeight-1; i>=0; i--){
        pp=data+i*biWidth;
        for(j=0; j<biWidth; j++,pp++){
          store >> ((FXuchar*)pp)[2];             // Blue
          store >> ((FXuchar*)pp)[1];             // Green
          store >> ((FXuchar*)pp)[0];             // Red
          store >> ((FXuchar*)pp)[3];             // Alpha
          }
        }
      break;
    }

  // Read 1-bit alpha data if no alpha channel, and skip otherwise.
  // We need to skip instead of just quit reading because the image
  // may be embedded in a larger stream and we need the byte-count to
  // remain correct for the format.
  if(biBitCount!=32){
    pad=(4-((width+7)/8))&3;
    for(i=height-1; i>=0; i--){
      pp=data+i*width;
      for(j=0; j<width; j++,pp++){
        if((j&7)==0){ store >> c1; }
        ((FXuchar*)pp)[3]=~-((c1&0x80)>>7);       // Groovy!
        c1<<=1;
        }
      store.position(pad,FXFromCurrent);
      }
    }
  else{
    store.position(store.position()+height*(width>>3));
    }

  // Done
  ok=true;
x:store.swapBytes(swap);
  return ok;
  }


/*******************************************************************************/


// Save a ICO file to a stream
bool fxsaveICO(FXStream& store,const FXColor *data,FXint width,FXint height,FXint xspot,FXint yspot){
  const FXint    biSize=40;
  const FXshort  biPlanes=1;
  const FXint    biCompression=BIH_RGB;
  const FXint    biXPelsPerMeter=0;
  const FXint    biYPelsPerMeter=0;
  const FXint    biClrUsed=0;
  const FXint    biClrImportant=0;
  const FXshort  idReserved=0;
  const FXshort  idCount=1;
  const FXuchar  bReserved=0;
  const FXuchar  bColorCount=0;
  const FXint    dwImageOffset=22;
  const FXColor *pp;
  const FXuchar  padding[3]={0,0,0};
  FXshort        biBitCount=24;
  FXshort        idType=2;
  FXshort        wXHotspot=xspot;
  FXshort        wYHotspot=yspot;
  FXint          biSizeImage=width*height*3;
  FXint          dwBytesInRes=biSize+biSizeImage+height*(width>>3);
  FXint          iWidth=width;
  FXint          iHeight=height+height;
  FXuchar        bWidth=(FXuchar)width;
  FXuchar        bHeight=(FXuchar)height;
  FXint          i,j,pad;
  FXuchar        c,bit;
  bool           swap;

  // Must make sense
  if(!data || width<=0 || height<=0) return false;

  // Quick pass to see if alpha<255 anywhere
  for(i=width*height-1; i>=0; i--){
    if(((FXuchar*)(data+i))[3]<255){ biBitCount=32; break; }
    }

  // If no hot-spot given, save as an icon instead of a cursor
  if(wXHotspot<0 || wYHotspot<0){
    wXHotspot=wYHotspot=0;
    idType=1;
    }

  // Bitmaps are little-endian
  swap=store.swapBytes();
  store.setBigEndian(false);

  // IconHeader
  store << idReserved;          // Must be zero
  store << idType;              // Must be 1 or 2
  store << idCount;             // Only one icon

  // IconDirectoryEntry
  store << bWidth;
  store << bHeight;
  store << bColorCount;         // 0 for > 8bit/pixel
  store << bReserved;
  store << wXHotspot;
  store << wYHotspot;
  store << dwBytesInRes;        // Total number of bytes in images (including palette data)
  store << dwImageOffset;       // Location of image from the beginning of file

  // BitmapInfoHeader
  store << biSize;
  store << iWidth;
  store << iHeight;
  store << biPlanes;
  store << biBitCount;
  store << biCompression;
  store << biSizeImage;
  store << biXPelsPerMeter;
  store << biYPelsPerMeter;
  store << biClrUsed;
  store << biClrImportant;

  // Write 24-bit rgb data
  if(biBitCount==24){
    pad=(4-((width*3)&3))&3;
    for(i=height-1; i>=0; i--){
      pp=data+i*width;
      for(j=0; j<width; j++){
        store << ((FXuchar*)pp)[2];
        store << ((FXuchar*)pp)[1];
        store << ((FXuchar*)pp)[0];
        pp++;
        }
      store.save(padding,pad);
      }
    }

  // 32-bit/pixel
  else{
    for(i=height-1; i>=0; i--){
      pp=data+i*width;
      for(j=0; j<width; j++){
        store << ((FXuchar*)pp)[2];
        store << ((FXuchar*)pp)[1];
        store << ((FXuchar*)pp)[0];
        store << ((FXuchar*)pp)[3];
        pp++;
        }
      }
    }

  // Write 1-bit alpha data
  pad=(4-((width+7)/8))&3;
  for(i=height-1; i>=0; i--){
    pp=data+i*width;
    for(j=c=0,bit=0x80; j<width; j++){
      if(((FXuchar*)pp)[3]==0) c|=bit;  // Only transparent if FULLY transparent!
      bit>>=1;
      if(bit==0){
        store << c;
        bit=0x80;
        c=0;
        }
      pp++;
      }
    }
  store.save(padding,pad);
  store.swapBytes(swap);
  return true;
  }

}
