/********************************************************************************
*                                                                               *
*                      T A R G A   I n p u t / O u t p u t                      *
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
* $Id: fxtargaio.cpp,v 1.30 2006/01/22 17:58:54 fox Exp $                       *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"

/*
  Notes:
  - Need to have checks in RLE decoder for out-of-bounds checking.
  - Need to try save image with fewer bits/pixel if possible.
  - To map from 5-bit to 8-bit, we use value*8+floor(value/4) which
    is almost the same as the correct value*8.225806.
  - Yes, in 16 bit its still 5,5,5 and not 5,6,5.
  - We need to clean this up and simplify a bit some day.
*/


using namespace FX;


/*******************************************************************************/

namespace FX {


extern FXAPI bool fxcheckTGA(FXStream& store);
extern FXAPI bool fxloadTGA(FXStream& store,FXColor*& data,FXint& width,FXint& height);
extern FXAPI bool fxsaveTGA(FXStream& store,const FXColor *data,FXint width,FXint height);


static inline FXuint read16(FXStream& store){
  FXuchar c1,c2;
  store >> c1 >> c2;
  return ((FXuint)c1) | (((FXuint)c2)<<8);
  }



static bool loadTarga32(FXStream& store,FXColor* data,FXint width,FXint height,FXuchar imgdescriptor,FXuchar ImageType){
  register FXuchar *pp;
  register FXint i,j,rc;
  FXuchar R,G,B,A,c;

  // 2 - Uncompressed, RGB images.
  if(ImageType==2){

    // Origin in upper left-hand corner
    if((imgdescriptor&0x20)==0x20){
      pp=(FXuchar*)data;
      for(i=0; i<height; i++){
        for(j=0; j<width; j++){
          store >> pp[2];       // Blue
          store >> pp[1];       // Green
          store >> pp[0];       // Red
          store >> pp[3];       // Alpha
          pp+=4;
          }
        }
      }

    // Origin in lower left-hand corner
    else{
      for(i=height-1; i>=0; i--){
        pp=(FXuchar*)(data+i*width);
        for(j=0; j<width; j++){
          store >> pp[2];       // Blue
          store >> pp[1];       // Green
          store >> pp[0];       // Red
          store >> pp[3];       // Alpha
          pp+=4;
          }
        }
      }
    }

  // 10 - Runlength encoded RGB images.
  else if(ImageType==10){

    // Origin in upper left-hand corner
    if((imgdescriptor&0x20)==0x20){
      pp=(FXuchar*)data;
      for(i=0; i<height; i++){
        j=0;
        while(j<width){

          // read Repetition Count field
          store >> c;

          // check for Run-length Packet
          if(c>127){
            rc=c-127;
            j+=rc;

            // read Pixel Value field - get R, G, B, A values
            store >> B;
            store >> G;
            store >> R;
            store >> A;

            while(rc--){
              *pp++=R; // Red
              *pp++=G; // Green
              *pp++=B; // Blue
              *pp++=A; // Alpha
              }
            }

          // Raw Packet
          else{
            rc=c+1;
            j+=rc;
            while(rc--){
              store >> B;
              store >> G;
              store >> R;
              store >> A;
              *pp++=R; // Red
              *pp++=G; // Green
              *pp++=B; // Blue
              *pp++=A; // Alpha
              }
            }
          }
        }
      }

    // Origin in lower left-hand corner
    else{
      for(i=height-1; i>=0; i--){
        j=0;
        pp=(FXuchar*)(data+i*width);
        while(j<width){

          // read Repetition Count field
          store >> c;

          // check for Run-length Packet
          if(c>127){
            rc=c-127;
            j+=rc;

            // read Pixel Value field - get R,G,B,A values
            store >> B;
            store >> G;
            store >> R;
            store >> A;
            while(rc--){
              *pp++=R; // Red
              *pp++=G; // Green
              *pp++=B; // Blue
              *pp++=A; // Alpha
              }
            }
          // Raw Packet
          else{
            rc=c+1;
            j+=rc;
            while(rc--){
              store >> B;
              store >> G;
              store >> R;
              store >> A;
              *pp++=R; // Red
              *pp++=G; // Green
              *pp++=B; // Blue
              *pp++=A; // Alpha
              }
            }
          }
        }
      }
    }
  return true;
  }


static bool loadTarga24(FXStream& store,FXColor* data,FXint width,FXint height,FXuchar imgdescriptor,FXuchar ImageType){
  register int i,j,rc;
  register FXuchar *pp;
  FXuchar R,G,B,c;

  // 2 - Uncompressed, RGB images.
  if(ImageType == 2){

    // Origin in upper left-hand corner
    if((imgdescriptor&0x20)==0x20){
      pp=(FXuchar*)data;
      for(i=0; i<height; i++){
        for(j=0; j<width; j++){
          store >> pp[2];       // Blue
          store >> pp[1];       // Green
          store >> pp[0];       // Red
          pp[3]=255;            // Alpha
          pp+=4;
          }
        }
      }

    // Origin in lower left-hand corner
    else{
      for(i=height-1; i>=0; i--){
        pp=(FXuchar*)(data+i*width);
        for(j=0; j<width; j++){
          store >> pp[2];       // Blue
          store >> pp[1];       // Green
          store >> pp[0];       // Red
          pp[3]=255;            // Alpha
          pp+=4;
          }
        }
      }
    }

  // 10 - Runlength encoded RGB images.
  else if(ImageType==10){

    // Origin in upper left-hand corner
    if((imgdescriptor&0x20)==0x20){
      pp=(FXuchar*)data;
      for(i=0; i<height; i++){
        j=0;
        while(j<width){

          // read Repetition Count field
          store >> c;

          // check for Run-length Packet
          if(c>127){
            rc=c-127;
            j+=rc;

            // read Pixel Value field - get R, G, B values
            store >> B;
            store >> G;
            store >> R;
            while(rc--){
              *pp++=R;          // Red
              *pp++=G;          // Green
              *pp++=B;          // Blue
              *pp++=255;        // Alpha
              }
            }
          // Raw Packet
          else{
            rc=c+1;
            j+=rc;
            while(rc--){
              store >> B;
              store >> G;
              store >> R;
              *pp++=R;          // Red
              *pp++=G;          // Green
              *pp++=B;          // Blue
              *pp++=255;        // Alpha
              }
            }
          }
        }
      }

    // Origin in lower left-hand corner
    else{
      for(i=height-1; i>=0; i--){
        j=0;
        pp=(FXuchar*)(data+i*width);
        while(j<width){

          // read Repetition Count field
          store >> c;

          // check for Run-length Packet
          if(c>127){
            rc=c-127;
            j+=rc;

            // read Pixel Value field - get R,G,B values
            store >> B;
            store >> G;
            store >> R;
            while(rc--){
              *pp++=R;          // Red
              *pp++=G;          // Green
              *pp++=B;          // Blue
              *pp++=255;        // Alpha
              }
            }
          // Raw Packet
          else{
            rc = c + 1;
            j += rc;
            while(rc--){
              store >> B;
              store >> G;
              store >> R;
              *pp++=R;          // Red
              *pp++=G;          // Green
              *pp++=B;          // Blue
              *pp++=255;        // Alpha
              }
            }
          }
        }
      }
    }
  return true;
  }


static bool loadTarga16(FXStream& store,FXColor* data,FXint width,FXint height,FXuchar imgdescriptor,FXuchar ImageType){
  register FXushort rgb16;
  register FXuchar *pp;
  register int i,j,rc;
  FXuchar R,G,B,c;

  // 2 - Uncompressed, RGB images.
  if(ImageType==2){
    // check Image Descriptor
    // Origin in upper left-hand corner
    if((imgdescriptor&0x20)==0x20){
      pp=(FXuchar*)data;
      for(i=0; i<height; i++){
        for(j=0; j<width; j++){
          rgb16=read16(store);
          *pp++=((rgb16>>7)&0xf8)+((rgb16>>12)&7);      // Red
          *pp++=((rgb16>>2)&0xf8)+((rgb16>>7)&7);       // Green
          *pp++=((rgb16<<3)&0xf8)+((rgb16>>2)&7);       // Blue
          *pp++=255;                                    // Alpha
          }
        }
      }
    else{
      // Origin in lower left-hand corner
      for(i=height-1; i>=0; i--){
        pp=(FXuchar*)(data+i*width);
        for(j=0; j<width; j++){
          rgb16=read16(store);
          *pp++=((rgb16>>7)&0xf8)+((rgb16>>12)&7);      // Red
          *pp++=((rgb16>>2)&0xf8)+((rgb16>>7)&7);       // Green
          *pp++=((rgb16<<3)&0xf8)+((rgb16>>2)&7);       // Blue
          *pp++=255;                                    // Alpha
          }
        }
      }
    }

  // 10 - Runlength encoded RGB images.
  else if(ImageType==10){
    // check Image Descriptor
    // Origin in upper left-hand corner
    if((imgdescriptor&0x20)==0x20){
      pp=(FXuchar*)data;
      for(i=0; i<height; i++){
        j=0;
        while(j<width){

          // read Repetition Count field
          store >> c;

          // check for Run-length Packet
          if(c>127){
            rc=c-127;
            j+=rc;

            // read Pixel Value field -
            rgb16=read16(store);

            // get R, G, B values
            R=((rgb16>>7)&0xf8)+((rgb16>>12)&7);      // Red
            G=((rgb16>>2)&0xf8)+((rgb16>>7)&7);       // Green
            B=((rgb16<<3)&0xf8)+((rgb16>>2)&7);       // Blue
            while(rc--){
              *pp++=R;          // Red
              *pp++=G;          // Green
              *pp++=B;          // Blue
              *pp++=255;        // Alpha
              }
            }

          // Raw Packet
          else{
            rc=c+1;
            j+=rc;
            while(rc--){
              rgb16=read16(store);
              *pp++=((rgb16>>7)&0xf8)+((rgb16>>12)&7);// Red
              *pp++=((rgb16>>2)&0xf8)+((rgb16>>7)&7); // Green
              *pp++=((rgb16<<3)&0xf8)+((rgb16>>2)&7); // Blue
              *pp++=255;                                // Alpha
              }
            }
          }
        }
      }

    // Origin in lower left-hand corner
    else{
      for(i=height-1; i>=0; i--){
        j=0;
        pp=(FXuchar*)(data+i*width);
        while(j<width){

          // read Repetition Count field
          store >> c;

          // check for Run-length Packet
          if(c>127){
            rc=c-127;
            j+=rc;

            // read Pixel Value field
            rgb16=read16(store);

            // get R, G, B values
            R=((rgb16>>7)&0xf8)+((rgb16>>12)&7);      // Red
            G=((rgb16>>2)&0xf8)+((rgb16>>7)&7);       // Green
            B=((rgb16<<3)&0xf8)+((rgb16>>2)&7);       // Blue
            while(rc--){
              *pp++=R;                  // Red
              *pp++=G;                  // Green
              *pp++=B;                  // Blue
              *pp++=255;                // Alpha
              }
            }

          // Raw Packet
          else{
            rc=c+1;
            j+=rc;
            while(rc--){
              rgb16=read16(store);
              *pp++=((rgb16>>7)&0xf8)+((rgb16>>12)&7);// Red
              *pp++=((rgb16>>2)&0xf8)+((rgb16>>7)&7); // Green
              *pp++=((rgb16<<3)&0xf8)+((rgb16>>2)&7); // Blue
              *pp++=255;                                // Alpha
              }
            }
          }
        }
      }
    }
  return true;
  }


static bool loadTarga8(FXStream& store,FXColor* data,FXint width,FXint height,FXuchar colormap[][4],FXuchar imgdescriptor,FXuchar ImageType){
  register FXint i,j,rc;
  register FXuchar *pp;
  FXuchar R,G,B,A,c;

  // 1 - Uncompressed, color-mapped images
  if(ImageType==1){
    // check Image Descriptor
    // Origin in upper left-hand corner
    if((imgdescriptor&0x20)==0x20){
      pp=(FXuchar*)data;
      for(i=0; i<height; i++){
        for(j=0; j<width; j++){
          store >> c;
          *pp++=colormap[c][2];         // Red
          *pp++=colormap[c][1];         // Green
          *pp++=colormap[c][0];         // Blue
          *pp++=colormap[c][3];         // Alpha
          }
        }
      }
    // Origin in lower left-hand corner
    else{
      for(i=height-1; i>=0; i--){
        pp=(FXuchar*)(data+i*width);
        for(j=0; j<width; j++){
          store >> c;
          *pp++=colormap[c][2];         // Red
          *pp++=colormap[c][1];         // Green
          *pp++=colormap[c][0];         // Blue
          *pp++=colormap[c][3];         // Alpha
          }
        }
      }
    }

  // 9 - Runlength encoded color-mapped images
  else if(ImageType==9){
    // check Image Descriptor
    // Origin in upper left-hand corner
    if((imgdescriptor&0x20)==0x20){
      pp=(FXuchar*)data;
      for(i=0; i<height; i++){
        j=0;
        while(j<width){
          // read Repetition Count field
          store >> c;

          // check for Run-length Packet
          if(c>127){
            rc=c-127;
            j+=rc;

            // read Pixel Value field
            store >> c;

            // get R,G,B values
            R=colormap[c][2];
            G=colormap[c][1];
            B=colormap[c][0];
            A=colormap[c][3];
            while(rc--){
              *pp++=R;          // Red
              *pp++=G;          // Green
              *pp++=B;          // Blue
              *pp++=A;          // Alpha
              }
            }

          // Raw Packet
          else{
            rc=c+1;
            j+=rc;
            while(rc--){
              store >> c;
              *pp++=colormap[c][2];     // Red
              *pp++=colormap[c][1];     // Green
              *pp++=colormap[c][0];     // Blue
              *pp++=colormap[c][3];     // Alpha
              }
            }
          }
        }
      }

    // Origin in lower left-hand corner
    else{
      for(i=height-1; i>=0; i--){
        j=0;
        pp=(FXuchar*)(data+i*width);
        while(j<width){

          // read Repetition Count field
          store >> c;

          // check for Run-length Packet
          if(c>127){
            rc=c-127;
            j+=rc;

            // read Pixel Value field
            store >> c;

            // get R,G,B values
            R=colormap[c][2];
            G=colormap[c][1];
            B=colormap[c][0];
            A=colormap[c][3];
            while(rc--){
              *pp++=R;          // Red
              *pp++=G;          // Green
              *pp++=B;          // Blue
              *pp++=A;          // Alpha
              }
            }

          // Raw Packet
          else{
            rc=c+1;
            j+=rc;
            while(rc--){
              store >> c;
              *pp++=colormap[c][2];     // Red
              *pp++=colormap[c][1];     // Green
              *pp++=colormap[c][0];     // Blue
              *pp++=colormap[c][3];     // Alpha
              }
            }
          }
        }
      }
    }
  return true;
  }


static bool loadTargaGray(FXStream& store,FXColor* data,FXint width,FXint height,FXuchar imgdescriptor,FXuchar ImageType){
  register FXint i,j,rc;
  register FXuchar *pp;
  FXuchar c;

  // 3 - Uncompressed, black and white images.
  if(ImageType==3){
    // check Image Descriptor
    // Origin in upper left-hand corner
    if((imgdescriptor&0x20)==0x20){
      pp=(FXuchar*)data;
      for(i=0; i<height; i++){
        for(j=0; j<width; j++){
          store >> c;
          *pp++=c;
          *pp++=c;
          *pp++=c;
          *pp++=255;
          }
        }
      }

    // Origin in lower left-hand corner
    else{
      for(i=height-1; i>=0; i--){
        pp=(FXuchar*)(data+i*width);
        for(j=0; j<width; j++){
          store >> c;
          *pp++=c;
          *pp++=c;
          *pp++=c;
          *pp++=255;
          }
        }
      }
    }

  // 11 - Compressed, black and white images.
  else if(ImageType==11){

    // check Image Descriptor
    // Origin in upper left-hand corner
    if((imgdescriptor&0x20)==0x20){
      pp=(FXuchar*)data;
      for(i=0; i<height; i++){
        j=0;
        while(j<width){

          // read Repetition Count field
          store >> c;

          // check for Run-length Packet
          if(c>127){
            rc=c-127;
            j+=rc;

            // read Pixel Value field
            store >> c;
            while(rc--){
              *pp++=c;
              *pp++=c;
              *pp++=c;
              *pp++=255;
              }
            }

          // Raw Packet
          else{
            rc=c+1;
            j+=rc;
            while(rc--){
              store >> c;
              *pp++=c;
              *pp++=c;
              *pp++=c;
              *pp++=255;
              }
            }
          }
        }
      }

    // Origin in lower left-hand corner
    else{
      for(i=height-1; i>=0; i--){
        j = 0;
        pp=(FXuchar*)(data+i*width);
        while(j<width){

          // read Repetition Count field
          store >> c;

          // check for Run-length Packet
          if(c>127){
            rc=c-127;
            j+=rc;

            // read Pixel Value field
            store >> c;
            while(rc--){
              *pp++=c;
              *pp++=c;
              *pp++=c;
              *pp++=255;
              }
            }

          // Raw Packet
          else{
            rc=c+1;
            j+=rc;
            while(rc--){
              store >> c;
              *pp++=c;
              *pp++=c;
              *pp++=c;
              *pp++=255;
              }
            }
          }
        }
      }
    }
  return true;
  }


// Check if stream contains a TARGA
bool fxcheckTGA(FXStream& store){
  FXuchar signature[3];
  store.load(signature,3);
  store.position(-3,FXFromCurrent);
  return signature[2]==1 || signature[2]==2 || signature[2]==3 || signature[2]==9 || signature[2]==10 || signature[2]==11 || signature[2]==32 || signature[2]==33;
  }


// Load Targa image from stream
bool fxloadTGA(FXStream& store,FXColor*& data,FXint& width,FXint& height){
  FXuchar IDLength,ColorMapType,ImageType,ColorMapEntrySize,PixelDepth,ImageDescriptor;
  FXuchar colormap[256][4];
  FXuint rgb16,ColorMapLength,i;
  FXlong start;

  // Null out
  data=NULL;
  width=0;
  height=0;

  // Remember start
  start=store.position();

  // Length of Image ID Field
  store >> IDLength;

  // Type of color map (if any) included with the image
  // 0 - indicates that no color-map data is included with this image
  // 1 - indicates that a color-map is included with this image
  store >> ColorMapType;

  // Image Type
  //  0 - No image data included.
  //  1 - Uncompressed, color-mapped images.
  //  2 - Uncompressed, RGB images.
  //  3 - Uncompressed, black and white images.
  //  9 - Runlength encoded color-mapped images.
  // 10 - Runlength encoded RGB images.
  // 11 - Compressed, black and white images.
  // 32 - Compressed color-mapped data, using Huffman, Delta, and runlength encoding.
  // 33 - Compressed color-mapped data, using Huffman, Delta, and runlength encoding.
  //      4-pass quadtree-type process.
  store >> ImageType;

//  FXTRACE((1,"fxloadTGA IDLength=%d ColorMapType=%d ImageType=%d\n",IDLength,ColorMapType,ImageType));

  // Check for supported image type
  if(ImageType!=1 && ImageType!=2 && ImageType!=3 && ImageType!=9 && ImageType!=10 && ImageType!=11 && ImageType!=32 && ImageType!=33) return false;

  // Color Map Specification

  // FirstEntryIndex - index of the first color map entry
  read16(store);

  // Color map Length
  ColorMapLength=read16(store);

  // Color map Entry Size
  // Establishes the number of bits per entry.
  // Typically 15, 16, 24 or 32-bit values are used.
  store >> ColorMapEntrySize;

  // Image Specification Field

  // X-origin of Image and Y-origin of Image
  read16(store);
  read16(store);

  // This field specifies the width of the image in pixels
  width=read16(store);

  // This field specifies the height of the image in pixels
  height=read16(store);

  // This field indicates the number of bits per pixel. This number includes
  // the Attribute or Alpha channel bits. Common values are 8, 16, 24 and 32
  // but other pixel depths could be used.
  store >> PixelDepth;

//  FXTRACE((1,"fxloadTGA PixelDepth=%d ColorMapLength=%d ColorMapEntrySize=%d width=%d height=%d\n",PixelDepth,ColorMapLength,ColorMapEntrySize,width,height));

  // Don't load too many colors
  if(ColorMapLength>256) return FALSE;

  // Verify sanity
  if(PixelDepth!=1 && PixelDepth!=8 && PixelDepth!=15 && PixelDepth!=16 && PixelDepth!=24 && PixelDepth!=32) return false;

  // Bits 3-0 - number of attribute bits associated with each pixel
  // Bit 4    - reserved.  Must be set to 0
  // Bit 5    - screen origin bit:
  //            0 = Origin in lower left-hand corner
  //            1 = Origin in upper left-hand corner
  //            Must be 0 for Truevision images
  // Bits 7-6 - Data storage interleaving flag:
  //            00 = non-interleaved
  //            01 = two-way (even/odd) interleaving
  //            10 = four way interleaving
  //            11 = reserved
  store >> ImageDescriptor;

  // skip Image ID Field (18 - standard header length)
  store.position(start+18+IDLength);

  // color map
  if(ColorMapLength>0){
    switch(ColorMapEntrySize){
      case 15:
      case 16:          // Is this also 5:5:5 or is it 5:6:5?
        for(i=0; i<ColorMapLength; i++){
          rgb16=read16(store);
          colormap[i][0]=((rgb16>>7)&0xf8)+((rgb16>>12)&7);     // Red
          colormap[i][1]=((rgb16>>2)&0xf8)+((rgb16>>7)&7);      // Green
          colormap[i][2]=((rgb16<<3)&0xf8)+((rgb16>>2)&7);      // Blue
          colormap[i][3]=255;                                   // Alpha
          }
        break;

      // R,G,B
      case 24:
        for(i=0; i<ColorMapLength; i++){
          store >> colormap[i][0];
          store >> colormap[i][1];
          store >> colormap[i][2];
          colormap[i][3]=255;
          }
        break;

      // R,G,B,A
      case 32:
        for(i=0; i<ColorMapLength; i++){
          store >> colormap[i][0];
          store >> colormap[i][1];
          store >> colormap[i][2];
          store >> colormap[i][3];
          }
        break;

      // Huh?
      default:
        return false;
      }
    }

  FXTRACE((100,"fxloadTARGA: width=%d height=%d IDLength=%d ColorMapType=%d ColorMapLength=%d ColorMapEntrySize=%d ImageType=%d PixelDepth=%d ImageDescriptor=%02x\n",width,height,IDLength,ColorMapType,ColorMapLength,ColorMapEntrySize,ImageType,PixelDepth,ImageDescriptor));

  // Allocate memory
  FXMALLOC(&data,FXColor,width*height);
  if(!data) return false;

  // load up the image
  if(PixelDepth==32 && (ImageType==2 || ImageType==10)){
    return loadTarga32(store,data,width,height,ImageDescriptor,ImageType);
    }

  if(PixelDepth==24 && (ImageType==2 || ImageType==10)){
    return loadTarga24(store,data,width,height,ImageDescriptor,ImageType);
    }

  if(PixelDepth==16 && (ImageType==2 || ImageType==10)){
    return loadTarga16(store,data,width,height,ImageDescriptor,ImageType);
    }

  if(PixelDepth==15 && (ImageType==2 || ImageType==10)){
    return loadTarga16(store,data,width,height,ImageDescriptor,ImageType);
    }

  if(PixelDepth==8 && (ImageType==1 || ImageType==9)){
    return loadTarga8(store,data,width,height,colormap,ImageDescriptor,ImageType);
    }

  if(ImageType==3 || ImageType==11){
    return loadTargaGray(store,data,width,height,ImageDescriptor,ImageType);
    }

  return false;
  }

/*******************************************************************************/

static inline void write16(FXStream& store,FXuint i){
  FXuchar c1,c2;
  c1=i&0xff;
  c2=(i>>8)&0xff;
  store << c1 << c2;
  }


// Save a Targa file to a stream
bool fxsaveTGA(FXStream& store,const FXColor *data,FXint width,FXint height){
  FXuchar IDLength,ColorMapType,ImageType,ColorMapEntrySize,PixelDepth,ImageDescriptor;
  const FXuchar *pp;
  FXint i,j;

  // Must make sense
  if(!data || width<=0 || height<=0) return false;

  IDLength=0;
  ColorMapType=0;
  ImageType=2;
  PixelDepth=32;

  ImageDescriptor=0;
  ColorMapEntrySize=0;

  // length of Image ID Field
  store << IDLength;

  // type of color map (if any) included with the image
  // 0 - indicates that no color-map data is included with this image
  // 1 - indicates that a color-map is included with this image
  store << ColorMapType;

  // Image Type
  //  0 - No image data included.
  //  1 - Uncompressed, color-mapped images.
  //  2 - Uncompressed, RGB images.
  //  3 - Uncompressed, black and white images.
  //  9 - Runlength encoded color-mapped images.
  // 10 - Runlength encoded RGB images.
  // 11 - Compressed, black and white images.
  // 32 - Compressed color-mapped data, using Huffman, Delta, and runlength encoding.
  // 33 - Compressed color-mapped data, using Huffman, Delta, and runlength encoding.
  //      4-pass quadtree-type process.
  store << ImageType;

  // Color Map Specification

  // Index of the first color map entry
  write16(store,0);

  // Color map Length
  write16(store,0);

  // Color map Entry Size
  // Establishes the number of bits per entry.
  // Typically 15, 16, 24 or 32-bit values are used.
  store << ColorMapEntrySize;

  // Image Specification Field

  // X-origin of Image and Y-origin of Image
  write16(store,0);
  write16(store,0);

  // This field specifies the width of the image in pixels
  write16(store,width);

  // This field specifies the height of the image in pixels
  write16(store,height);

  // This field indicates the number of bits per pixel. This number includes
  // the Attribute or Alpha channel bits. Common values are 8, 16, 24 and 32
  // but other pixel depths could be used.
  store << PixelDepth;

  // Bits 3-0 - number of attribute bits associated with each pixel
  // Bit 4    - reserved.  Must be set to 0
  // Bit 5    - screen origin bit:
  //            0 = Origin in lower left-hand corner
  //            1 = Origin in upper left-hand corner
  //            Must be 0 for Truevision images
  // Bits 7-6 - Data storage interleaving flag:
  //            00 = non-interleaved
  //            01 = two-way (even/odd) interleaving
  //            10 = four way interleaving
  //            11 = reserved
  store << ImageDescriptor;

  // Write image
  for(i=height-1; i>=0; i--){
    pp=(FXuchar*)(data+i*width);
    for(j=0; j<width; j++){
      store << pp[2];     // blue
      store << pp[1];     // green
      store << pp[0];     // red
      store << pp[3];     // alpha
      pp+=4;
      }
    }
  return true;
  }

}

