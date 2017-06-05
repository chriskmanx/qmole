/********************************************************************************
*                                                                               *
*             S U N   R A S T E R   I M A G E   I n p u t / O u t p u t         *
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
* $Id: fxrasio.cpp,v 1.16.2.1 2006/08/01 18:04:47 fox Exp $                         *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"

/*
  Notes:

  - The official SUN Raster Image format specification says:

    A rasterfile is composed of three parts: first, a header containing 8
    integers; second, a (possibly empty) set of colormap values; and third,
    the pixel image, stored a line at a time, in increasing y order.
    The image is layed out in the file as in a memory pixrect.  Each line of
    the image is rounded up to the nearest 16 bits.

    The header is defined by the following structure:

         struct rasterfile {
              int  ras_magic;
              int  ras_width;
              int  ras_height;
              int  ras_depth;
              int  ras_length;
              int  ras_type;
              int  ras_maptype;
              int  ras_maplength;
         };

    The ras_magic field always contains the following constant:

         #define   RAS_MAGIC 0x59a66a95

    The ras_width, ras_height, and ras_depth fields contain  the image's width
    and height in pixels, and its depth in bits per pixel, respectively.
    The depth is either 1 or 8, corresponding to standard frame buffer depths.
    The ras_length field contains the length in bytes of the image data.
    For an unencoded image, this number is computable from the ras_width,
    ras_height, and ras_depth fields, but for an  encoded image it must be
    explicitly stored in order to be available without decoding the image itself.

    Note: the  length of the header and of the (possibly empty) colormap values
    are not included in the value of the ras_length field; it is only the
    image data length.  For historical reasons, files of type RT_OLD will
    usually have a 0 in the ras_length field, and software expecting to
    encounter such files should be prepared to compute the  actual image data
    length if needed.  The ras_maptype and ras_maplength fields contain the
    type and length in bytes of the colormap values, respectively.
    If ras_maptype is not RMT_NONE and the ras_maplength is not 0, then the
    colormap values are the ras_maplength bytes immediately after the header.
    These values are either uninterpreted bytes (usually with the ras_maptype
    set to RMT_RAW) or the equal length red, green and blue vectors, in that
    order (when the ras_maptype is RMT_EQUAL_RGB).
    In the latter case, the ras_maplength must be three times the size in bytes
    of any one of the vectors.


  - A note from Jamie Zawinski says:

    The manpage for rasterfile(5) doesn't say anything about the format of
    byte-encoded images, or  about plane/scanline  ordering in multi-plane
    images.

    The first thing in the file is

            struct rasterfile {
                    int ras_magic;
                    int ras_width;
                    int ras_height;
                    int ras_depth;
                    int ras_length;
                    int ras_type;
                    int ras_maptype;
                    int ras_maplength;
                    };

    The ras_magic field always contains the following constant:

            #define RAS_MAGIC 0x59a66a95

    The ras_length field is the length of the image data (which is the
    length of the file minus the length of the header and colormap).
    Catch: this is sometimes zero instead, so you can't really depend on
    it.

    The ras_type field is ras_old=0, ras_standard=1, ras_byte_encoded=2,
    or ras_experimental=FFFF.  There doesn't seem to be any difference
    between OLD and STANDARD except that the ras_length field is always 0
    in OLD.

    I didn't deal with cmaps, so from the  man page: "The ras_maptype and
    ras_maplength fields contain the type and length in bytes of the
    colormap values, respectively.  If ras_maptype is not RMT_NONE and the
    ras_maplength is not 0, then the colormap values are the ras_maplength
    bytes immediately after the header.   These values are either
    uninterpreted bytes (usually with the ras_maptype set to RMT_RAW) or
    the equal length red, green and blue vectors, in that order (when the
    ras_maptype is RMT_EQUAL_RGB).  In the latter case, the ras_maplength
    must be three times the size in bytes of any one of the vectors."
    Regardless of width, the stored scanlines are rounded up to multiples
    of 16 bits.

    I found the following description of byte-length encoding in Sun-Spots
    Digest, Volume 6, Issue 84:

    > From:    jpm%green@lanl.gov (Pat McGee)
    > Subject: Re: Format for byte encoded rasterfiles (1)
    >
    > The format is composed of many sequences of variable length records.
    > Each record may be 1, 2, or 3 bytes long.
    >
    >  o  If the first byte is not 0x80, the record is one byte long, and
    >     contains a pixel value.  Output 1 pixel of that value.
    >  o  If the first byte is 0x80 and the second byte is zero, the record
    >     is two bytes long.  Output 1 pixel with value 0x80.
    >  o  If the first byte is 0x80, and the second byte is not zero, the
    >     record is three bytes long.  The second byte is a count and the
    >     third byte is a value.  Output (count+1) pixels of that value.
    >
    > A run is not terminated at the end of a scan line.  So, if there are
    > three lines of red in a picture 100 pixels wide, the first run will
    > be 0x80 0xff 0x<red>, and the second will be 0x80 0x2b 0x<red>.
    >
    > 	Pat McGee, jpm@lanl.gov

*/

using namespace FX;

/*******************************************************************************/

namespace FX {


const FXint RAS_MAGIC = 0x59a66a95;     // Magic number

const FXint RT_OLD          = 0;        // Raster types
const FXint RT_STANDARD     = 1;
const FXint RT_BYTE_ENCODED = 2;
const FXint RT_FORMAT_RGB   = 3;        // [X]RGB instead of [X]BGR

const FXint RMT_NONE      = 0;          // Map type
const FXint RMT_EQUAL_RGB = 1;
const FXint RMT_RAW       = 2;



struct HEADER {                         // File header
  FXint magic;
  FXint width;
  FXint height;
  FXint depth;
  FXint length;
  FXint type;
  FXint maptype;
  FXint maplength;
  };


extern FXAPI bool fxcheckRAS(FXStream& store);
extern FXAPI bool fxloadRAS(FXStream& store,FXColor*& data,FXint& width,FXint& height);
extern FXAPI bool fxsaveRAS(FXStream& store,const FXColor *data,FXint width,FXint height);


// Read in MSB order
static inline FXuint read32(FXStream& store){
  FXuchar c1,c2,c3,c4;
  store >> c1 >> c2 >> c3 >> c4;
  return ((FXuint)c4) | (((FXuint)c3)<<8) | (((FXuint)c2)<<16) | (((FXuint)c1)<<24);
  }


// Check if stream contains a RAS
bool fxcheckRAS(FXStream& store){
  FXint signature;
  signature=read32(store);
  store.position(-4,FXFromCurrent);
  return signature==RAS_MAGIC;
  }


// Load SUN raster image file format
bool fxloadRAS(FXStream& store,FXColor*& data,FXint& width,FXint& height){
  FXuchar red[256],green[256],blue[256],*line,count,c,*p,*q,bit;
  register FXint npixels,depth,linesize,x,y,i;
  HEADER header;

  // Null out
  data=NULL;
  line=NULL;
  width=0;
  height=0;

  // Read header
  header.magic=read32(store);
  header.width=read32(store);
  header.height=read32(store);
  header.depth=read32(store);
  header.length=read32(store);
  header.type=read32(store);
  header.maptype=read32(store);
  header.maplength=read32(store);

  //FXTRACE((1,"fxloadRAS: magic=%08x width=%d height=%d depth=%d length=%d type=%d maptype=%d maplength=%d\n",header.magic,header.width,header.height,header.depth,header.length,header.type,header.maptype,header.maplength));

  // Check magic code
  if(header.magic!=RAS_MAGIC) return false;

  // Trivial reject
  if(header.width<1 || header.height<1) return false;

  // Bad colormap size
  if(header.maplength<0 || header.maplength>768) return false;

  // Verify depth options; must be 1,8,24, or 32
  if(header.depth!=1 && header.depth!=8 && header.depth!=24 && header.depth!=32) return false;

  // Verify supported types
  if(header.type!=RT_OLD && header.type!=RT_STANDARD && header.type!=RT_BYTE_ENCODED && header.type!=RT_FORMAT_RGB) return false;

  // Verify map types
  if(header.maptype!=RMT_RAW && header.maptype!=RMT_NONE && header.maptype!=RMT_EQUAL_RGB) return false;

  // Get size
  width=header.width;
  height=header.height;
  depth=header.depth;
  npixels=width*height;
  linesize=((width*depth+15)/16)*2;

  //FXTRACE((1,"fxloadRAS: header.length=%d linesize=%d 4*npixels=%d\n",header.length,linesize,4*npixels));

  // Read in the colormap
  if(header.maptype==RMT_EQUAL_RGB && header.maplength){
    //FXTRACE((1,"fxloadRAS: RMT_EQUAL_RGB\n"));
    store.load(red,header.maplength/3);
    store.load(green,header.maplength/3);
    store.load(blue,header.maplength/3);
    }

  // Skip colormap
  else if(header.maptype==RMT_RAW && header.maplength){
    //FXTRACE((1,"fxloadRAS: RMT_RAW\n"));
    store.position(header.maplength,FXFromCurrent);
    }

  // Black and white
  else if(header.depth==1){
    //FXTRACE((1,"fxloadRAS: 1 bit\n"));
    red[0]=green[0]=blue[0]=0;
    red[1]=green[1]=blue[1]=255;
    }

  // Gray scale
  else if(header.depth==8){
    //FXTRACE((1,"fxloadRAS: 8 bit\n"));
    for(i=0; i<256; i++){
      red[i]=green[i]=blue[i]=i;
      }
    }

  // Allocate pixel data
  if(!FXMALLOC(&data,FXColor,npixels)) return false;

  // Allocate scanline
  if(!FXMALLOC(&line,FXuchar,linesize)){ FXFREE(&data); return false; }

  // Now read the image
  for(y=0,p=(FXuchar*)data,count=c=0; y<height; y++){
    if(header.type!=RT_BYTE_ENCODED){           // Load uncompressed
      store.load(line,linesize);
      }
    else{
      for(i=0; i<linesize; i++){                // Load RLE compressed
        if(count){
          line[i]=c;
          count--;
          }
        else{
          store >> c;
          if(c==0x80){
            store >> count;
            if(count==0){
              line[i]=0x80;
              }
            else{
              store >> c;
              line[i]=c;
              }
            }
          else{
            line[i]=c;
            }
          }
        }
      }
    if(depth==1){                               // 1 bits/pixel
      for(x=0,q=line; x<width; x++,p+=4){
	bit=(line[x>>3]>>(7-(x&7)))&1;
        p[0]=red[bit];
        p[1]=green[bit];
        p[2]=blue[bit];
        p[3]=255;
        }
      }
    else if(depth==8){                          // 8 bits/pixel
      for(x=0,q=line; x<width; x++,p+=4,q+=1){
        p[0]=red[q[0]];
        p[1]=green[q[0]];
        p[2]=blue[q[0]];
        p[3]=255;
	}
      }
    else if(depth==24){                         // 24 bits/pixel
      if(header.type==RT_FORMAT_RGB){
        for(x=0,q=line; x<width; x++,p+=4,q+=3){
          p[0]=q[0];
          p[1]=q[1];
          p[2]=q[2];
          p[3]=255;
          }
        }
      else{
        for(x=0,q=line; x<width; x++,p+=4,q+=3){
          p[0]=q[2];
          p[1]=q[1];
          p[2]=q[0];
          p[3]=255;
          }
        }
      }
    else{                                       // 32 bits/pixel
      if(header.type==RT_FORMAT_RGB){
        for(x=0,q=line; x<width; x++,p+=4,q+=4){
          p[0]=q[0];
          p[1]=q[1];
          p[2]=q[2];
          p[3]=q[3];
          }
        }
      else{
        for(x=0,q=line; x<width; x++,p+=4,q+=4){
          p[0]=q[2];
          p[1]=q[1];
          p[2]=q[0];
          p[3]=q[3];
          }
        }
      }
    }

  // Release temporary stuff
  FXFREE(&line);

  return true;
  }




/*******************************************************************************/


// Write in MSB order
static inline void write32(FXStream& store,FXuint i){
  FXuchar c1,c2,c3,c4;
  c4=i&0xff;
  c3=(i>>8)&0xff;
  c2=(i>>16)&0xff;
  c1=(i>>24)&0xff;
  store << c1 << c2 << c3 << c4;
  }


// Save SUN raster image file format
bool fxsaveRAS(FXStream& store,const FXColor *data,FXint width,FXint height){
  register FXint npixels=width*height;

  // Must make sense
  if(!data || width<=0 || height<=0) return false;

  // Write header
  write32(store,RAS_MAGIC);
  write32(store,width);
  write32(store,height);
  write32(store,32);
  write32(store,4*npixels);
  write32(store,RT_FORMAT_RGB);
  write32(store,RMT_NONE);
  write32(store,0);

  // No RLE, or any other attempt to reduce size; sorry!
  store.save(data,npixels);

  return true;
  }


}
