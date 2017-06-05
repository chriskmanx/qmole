/********************************************************************************
*                                                                               *
*                          B M P   I n p u t / O u t p u t                      *
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
* $Id: fxbmpio.cpp,v 1.53 2006/01/22 17:58:52 fox Exp $                         *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"



/*
  Notes:
  - Writer should use fxezquantize() and if the number of colors is less than
    256, use 8bpp RLE compressed output; if less that 4, use 4bpp RLE compressed
    output, else if less than 2, use monochrome.
  - Writer should do this only when no loss of fidelity occurs.
  - Find documentation on 32-bpp bitmap.
  - When new FXStream is here, update reader/writer for byte-swapped i/o.
  - Need to have a pass, save as 32-bit when alpha present.
  - Need to have checks in RLE decoder for out-of-bounds checking.
  - To map from 5-bit to 8-bit, we use value*8+floor(value/4) which
    is almost the same as the correct value*8.225806.
*/

#define BIH_RGB         0       // biCompression values
#define BIH_RLE8        1
#define BIH_RLE4        2
#define BIH_BITFIELDS   3

#define OS2_OLD         12      // biSize values
#define WIN_NEW         40
#define OS2_NEW         64


using namespace FX;

/*******************************************************************************/

namespace FX {


extern FXAPI bool fxcheckBMP(FXStream& store);
extern FXAPI bool fxloadBMP(FXStream& store,FXColor*& data,FXint& width,FXint& height);
extern FXAPI bool fxsaveBMP(FXStream& store,const FXColor *data,FXint width,FXint height);


// Check if stream contains a BMP
bool fxcheckBMP(FXStream& store){
  FXuchar signature[2];
  store.load(signature,2);
  store.position(-2,FXFromCurrent);
  return signature[0]=='B' && signature[1]=='M';
  }


// Load image from stream
bool fxloadBMP(FXStream& store,FXColor*& data,FXint& width,FXint& height){
  FXint biXPelsPerMeter,biYPelsPerMeter,biClrUsed,biClrImportant,biCompression,biSize;
  FXint biWidth,biHeight,biSizeImage,bfOffBits,bfSize,i,j,x,y,maxpixels,colormaplen,padw,pad;
  FXushort bfType,bfReserved,biBitCount,biPlanes,rgb16;
  FXColor colormap[256],*pp;
  FXuchar padding[3],c1,c2;
  FXlong base,header;
  bool swap;
  bool ok=false;

  // Null out
  data=NULL;
  width=0;
  height=0;

  // Start of bitmap file header
  base=store.position();

  // Bitmaps are little-endian
  swap=store.swapBytes();
  store.setBigEndian(FALSE);

  // Get size and offset
  store >> bfType;
  store >> bfSize;
  store >> bfReserved;
  store >> bfReserved;
  store >> bfOffBits;

  // Check signature
  if(bfType!=0x4d42) goto x;

  // Start of bitmap info header
  header=store.position();

  // Read bitmap info header
  store >> biSize;
  if(biSize==OS2_OLD){                  // Old format
    store >> bfReserved; biWidth=bfReserved;
    store >> bfReserved; biHeight=bfReserved;
    store >> biPlanes;
    store >> biBitCount;
    biCompression = BIH_RGB;
    biSizeImage = (((biPlanes*biBitCount*biWidth)+31)/32)*4*biHeight;
    biXPelsPerMeter = 0;
    biYPelsPerMeter = 0;
    biClrUsed = 0;
    biClrImportant = 0;
    }
  else{                                 // New format
    store >> biWidth;
    store >> biHeight;
    store >> biPlanes;
    store >> biBitCount;
    store >> biCompression;
    store >> biSizeImage;
    store >> biXPelsPerMeter;
    store >> biYPelsPerMeter;
    store >> biClrUsed;
    store >> biClrImportant;
    }

//  FXTRACE((1,"fxloadBMP: biWidth=%d biHeight=%d biPlanes=%d biBitCount=%d biCompression=%d biClrUsed=%d biClrImportant=%d\n",biWidth,biHeight,biPlanes,biBitCount,biCompression,biClrUsed,biClrImportant));

  // Ought to be 1
  if(biPlanes!=1) goto x;

  // Check for supported depths
  if(biBitCount!=1 && biBitCount!=4 && biBitCount!=8 && biBitCount!=16 && biBitCount!=24 && biBitCount!=32) goto x;

  // Check for supported compression methods
  if(biCompression!=BIH_RGB && biCompression!=BIH_RLE4 && biCompression!=BIH_RLE8 && biCompression!=BIH_BITFIELDS) goto x;

  // Skip ahead to colormap
  store.position(header+biSize);

  // Load up colormap, if needed
  colormaplen=0;
  if(biBitCount<=8){
    colormaplen = biClrUsed ? biClrUsed : 1<<biBitCount;
    FXASSERT(colormaplen<=256);
    if(biSize!=OS2_OLD){
      for(i=0; i<colormaplen; i++){
        store >> c1; ((FXuchar*)(colormap+i))[2]=c1;      // Blue
        store >> c1; ((FXuchar*)(colormap+i))[1]=c1;      // Green
        store >> c1; ((FXuchar*)(colormap+i))[0]=c1;      // Red
        store >> c1; ((FXuchar*)(colormap+i))[3]=255;     // Opaque
        }
      }
    else{
      for(i=0; i<colormaplen; i++){
        store >> c1; ((FXuchar*)(colormap+i))[2]=c1;      // Blue
        store >> c1; ((FXuchar*)(colormap+i))[1]=c1;      // Green
        store >> c1; ((FXuchar*)(colormap+i))[0]=c1;      // Red
                     ((FXuchar*)(colormap+i))[3]=255;     // Opaque
        }
      }
    }

  // Jump to start of actual bitmap data
  if(biSize!=OS2_OLD){
    store.position(base+bfOffBits);
    }

  // Total number of pixels
  maxpixels=biWidth*biHeight;

  // Allocate memory
  if(!FXMALLOC(&data,FXColor,maxpixels)) goto x;

  // Width and height
  width=biWidth;
  height=biHeight;

  // Handle various depths
  switch(biBitCount){
    case 1:             // 1-bit/pixel
      padw=(biWidth+31)&~31;
      for(i=biHeight-1; i>=0; i--){
        pp=data+i*biWidth;
        for(j=0; j<padw; j++){
          if((j&7)==0){ store >> c1; }
          if(j<biWidth){ *pp++=colormap[(c1&0x80)>>7]; c1<<=1; }
          }
        }
      break;
    case 4:             // 4-bit/pixel
      if(biCompression==BIH_RGB){       // Read uncompressed data
        padw=(biWidth+7)&~7;
        for(i=biHeight-1; i>=0; i--){
          pp=data+i*biWidth;
          for(j=0; j<padw; j++){
            if((j&1)==0){ store >> c1; }
            if(j<biWidth){ *pp++=colormap[(c1&0xf0)>>4]; c1<<=4; }
            }
          }
        }
      else{                             // Read RLE4 compressed data
        x=y=0;
        pp=data+(biHeight-1)*biWidth;
        while(y<biHeight){
          store >> c2;
          if(c2){                       // Encoded mode
            store >> c1;
            for(i=0; i<c2; i++,x++){
              *pp++=colormap[(i&1)?(c1&0x0f):((c1>>4)&0x0f)];
              }
            }
          else{                         // Escape codes
            store >> c2;
            if(c2==0){                  // End of line
              x=0;
              y++;
              pp=data+(biHeight-y-1)*biWidth;
              }
            else if(c2==0x01){          // End of pic8
              break;
              }
            else if(c2==0x02){          // Delta
              store >> c2; x+=c2;
              store >> c2; y+=c2;
              pp=data+x+(biHeight-y-1)*biWidth;
              }
            else{                       // Absolute mode
              for(i=0; i<c2; i++,x++){
                if((i&1)==0){ store >> c1; }
                *pp++=colormap[(i&1)?(c1&0x0f):((c1>>4)&0x0f)];
                }
              if(((c2&3)==1) || ((c2&3)==2)) store >> c1;       // Read pad byte
              }
            }
          }
        }
      break;
    case 8:             // 8-bit/pixel
      if(biCompression==BIH_RGB){       // Read uncompressed data
        padw=(biWidth+3)&~3;
        for(i=biHeight-1; i>=0; i--){
          pp=data+i*biWidth;
          for(j=0; j<padw; j++){
            store >> c1;
            if(j<biWidth) *pp++=colormap[c1];
            }
          }
        }
      else{                             // Read RLE8 compressed data
        x=y=0;
        pp=data+(biHeight-1)*biWidth;
        while(y<biHeight){
          store >> c2;
          if(c2){                       // Encoded mode
            store >> c1;
            for(i=0; i<c2; i++,x++){
              *pp++=colormap[c1];
              }
            }
          else{                         // Escape codes
            store >> c2;
            if(c2==0x00){               // End of line
              x=0;
              y++;
              pp=data+(biHeight-y-1)*biWidth;
              }
            else if(c2==0x01){          // End of pic8
              break;
              }
            else if(c2==0x02){          // delta
              store >> c2; x+=c2;
              store >> c2; y+=c2;
              pp=data+x+(biHeight-y-1)*biWidth;
              }
            else{                       // Absolute mode
              for(i=0; i<c2; i++,x++){
                store >> c1;
                *pp++=colormap[c1];
                }
              if(c2&1) store >> c1;     // Odd length run: read an extra pad byte
              }
            }
          }
        }
      break;
    case 16:            // 16-bit/pixel
      pad=(4-((biWidth*2)&3))&3;
      for(i=biHeight-1; i>=0; i--){
        pp=data+i*biWidth;
        for(j=0; j<biWidth; j++,pp++){
          store >> rgb16;
          ((FXuchar*)pp)[0]=((rgb16>>7)&0xf8)+((rgb16>>12)&7);  // Red
          ((FXuchar*)pp)[1]=((rgb16>>2)&0xf8)+((rgb16>> 7)&7);  // Green
          ((FXuchar*)pp)[2]=((rgb16<<3)&0xf8)+((rgb16>> 2)&7);  // Blue
          ((FXuchar*)pp)[3]=255;                                // Alpha
          }
        store.load(padding,pad);
        }
      break;
    case 24:            // 24-bit/pixel
      pad=(4-((biWidth*3)&3))&3;
      for(i=biHeight-1; i>=0; i--){
        pp=data+i*biWidth;
        for(j=0; j<biWidth; j++,pp++){
          store >> ((FXuchar*)pp)[2];           // Blue
          store >> ((FXuchar*)pp)[1];           // Green
          store >> ((FXuchar*)pp)[0];           // Red
                   ((FXuchar*)pp)[3]=255;       // Alpha
          }
        store.load(padding,pad);
        }
      break;
    case 32:            // 32-bit/pixel
      for(i=biHeight-1; i>=0; i--){
        pp=data+i*biWidth;
        for(j=0; j<biWidth; j++,pp++){
          store >> ((FXuchar*)pp)[2];           // Blue
          store >> ((FXuchar*)pp)[1];           // Green
          store >> ((FXuchar*)pp)[0];           // Red
          store >> ((FXuchar*)pp)[3];           // Alpha
          }
        }
      break;
    }

  // Done
  ok=true;
x:store.swapBytes(swap);
  return ok;
  }


/*******************************************************************************/


// Save a bmp file to a stream
bool fxsaveBMP(FXStream& store,const FXColor *data,FXint width,FXint height){
  const FXshort  biPlanes=1;
  const FXshort  bfReserved=0;
  const FXint    biXPelsPerMeter=75*39;
  const FXint    biYPelsPerMeter=75*39;
  const FXint    biClrUsed=0;
  const FXint    biClrImportant=0;
  const FXint    biCompression=BIH_RGB;
  const FXint    biSize=40;
  const FXint    bfHeader=14;
  const FXuchar  padding[3]={0,0,0};
  const FXColor *pp;
  FXint          bperlin,i,j,pad;
  FXint          bfSize;
  FXint          biSizeImage;
  FXint          bfOffBits;
  FXshort        biBitCount=24;
  bool           swap;

  // Must make sense
  if(!data || width<=0 || height<=0) return false;

  // Quick pass to see if alpha<255 anywhere
  for(i=width*height-1; i>=0; i--){
    if(((FXuchar*)(data+i))[3]<255){ biBitCount=32; break; }
    }

  // Number of bytes written per line
  bperlin=((width*biBitCount+31)/32)*4;

  // Size of raw image data
  biSizeImage=bperlin*height;

  // Offset to image data
  bfOffBits=bfHeader+biSize+biClrUsed*4;

  // Compute file size// size of bitmap file
  bfSize=bfHeader+biSize+biClrUsed*4+biSizeImage;

  // Bitmaps are little-endian
  swap=store.swapBytes();
  store.setBigEndian(FALSE);

  // BitmapFileHeader
  store << 'B';                         // Magic number
  store << 'M';
  store << bfSize;                      // bfSize: size of file
  store << bfReserved;                  // bfReserved1
  store << bfReserved;                  // bfReserved2
  store << bfOffBits;                   // bfOffBits

  // BitmapInfoHeader
  store << biSize;                      // biSize: size of bitmap info header
  store << width;                       // biWidth
  store << height;                      // biHeight
  store << biPlanes;                    // biPlanes:  must be '1'
  store << biBitCount;                  // biBitCount (1,4,8,24, or 32)
  store << biCompression;               // biCompression:  BIH_RGB, BIH_RLE8, BIH_RLE4, or BIH_BITFIELDS
  store << biSizeImage;                 // biSizeImage:  size of raw image data
  store << biXPelsPerMeter;             // biXPelsPerMeter: (75dpi * 39" per meter)
  store << biYPelsPerMeter;             // biYPelsPerMeter: (75dpi * 39" per meter)
  store << biClrUsed;                   // biClrUsed
  store << biClrImportant;              // biClrImportant

  // 24-bit/pixel
  if(biBitCount==24){
    pad=(4-((width*3)&3))&3;
    for(i=height-1; i>=0; i--){
      pp=data+i*width;
      for(j=0; j<width; j++,pp++){
        store << ((FXuchar*)pp)[2];
        store << ((FXuchar*)pp)[1];
        store << ((FXuchar*)pp)[0];
        }
      store.save(padding,pad);
      }
    }

  // 32-bit/pixel
  else{
    for(i=height-1; i>=0; i--){
      pp=data+i*width;
      for(j=0; j<width; j++,pp++){
        store << ((FXuchar*)pp)[2];
        store << ((FXuchar*)pp)[1];
        store << ((FXuchar*)pp)[0];
        store << ((FXuchar*)pp)[3];
        }
      }
    }

  store.swapBytes(swap);
  return true;
  }

}
