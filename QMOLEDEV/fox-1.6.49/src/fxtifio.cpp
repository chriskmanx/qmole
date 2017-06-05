/********************************************************************************
*                                                                               *
*                          T I F F   I n p u t / O u t p u t                    *
*                                                                               *
*********************************************************************************
* Copyright (C) 2001,2006 Eric Gillet.   All Rights Reserved.                   *
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
* $Id: fxtifio.cpp,v 1.35.2.1 2006/08/01 18:04:47 fox Exp $                         *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"
#ifdef HAVE_TIFF_H
#include <tiffio.h>
#endif

/*
  Notes:
  - Made it thread-safe.
  - Made error and warning handlers call FOX's warning handler.
  - References:
    http://www.libtiff.org/
    ftp://ftp.onshore.com/pub/libtiff/TIFF6.ps.Z
    ftp://ftp.sgi.com/graphics/tiff/TTN2.draft.txt
    http://partners.adobe.com/asn/developer/technotes.html
  - Bugs: libtiff does not gracefully recover from certain errors;
    this causes core dump!
  - FOX keeps order in FXColor to RGBA (i.e. red at lowest memory
    address, alpha at highest). Currently not known if this does
    or does not require swapping when read in.
  - Updated for libtiff 3.6.1.
*/


#define TIFF_SWAP(p) (((p)&0xff)<<24 | ((p)&0xff00)<<8 | ((p)&0xff0000)>>8 | ((p)&0xff000000)>>24)


using namespace FX;

/*******************************************************************************/

namespace FX {


extern FXAPI bool fxcheckTIF(FXStream& store);
extern FXAPI bool fxloadTIF(FXStream& store,FXColor*& data,FXint& width,FXint& height,FXushort& codec);
extern FXAPI bool fxsaveTIF(FXStream& store,const FXColor* data,FXint width,FXint height,FXushort codec);


#ifdef HAVE_TIFF_H


// Stuff being passed around
struct tiff_store_handle {
  FXStream *store;
  FXlong    begin;
  FXlong    end;
  FXbool    error;
  };


// Report an error
static void fxerrorhandler(const char* module,const char* format,va_list args){
  FXchar message[1024];
  vsprintf(message,format,args);
  FXTRACE((100,"error: in %s: %s\n",module,message));
  }


// Report a warning
static void fxwarninghandler(const char* module,const char* format,va_list args){
  FXchar message[1024];
  vsprintf(message,format,args);
  FXTRACE((100,"warning: in %s: %s\n",module,message));
  }


// Read bytes from stream
static tsize_t tif_read_store(thandle_t handle,tdata_t data,tsize_t size){
  tiff_store_handle *h=(tiff_store_handle*)handle;
  h->store->load((FXuchar*)data,size);
  if(h->store->eof()!=FXStreamOK) return 0;
  if(h->store->position() > h->end) h->end=h->store->position();
  return size;
  }


// Dummy read bytes
static tsize_t tif_dummy_read_store(thandle_t,tdata_t,tsize_t){
  return 0;
  }


// Write bytes to stream
static tsize_t tif_write_store(thandle_t handle,tdata_t data,tsize_t size){
  tiff_store_handle *h=(tiff_store_handle*)handle;
  h->store->save((FXuchar*)data,size);
  if(h->store->status()!=FXStreamOK) return 0;
  if(h->store->position()>h->end) h->end=h->store->position();
  return size;
  }


// Seek to a position in the stream
static toff_t tif_seek_store(thandle_t handle,toff_t offset,int whence){
  tiff_store_handle *h=(tiff_store_handle*)handle;
  unsigned long off;
  if(whence==SEEK_SET){
    off=h->begin+offset;
    }
  else if(whence==SEEK_CUR){
    off=h->store->position()+offset;
    }
  else{ // SEEK_END
    off=h->end+offset;
    }
  h->store->position(off);
  return off;
  }


// Dummy close store
static int tif_close_store(thandle_t){
  return 0;
  }


// Dummy map file
static int tif_map_store(thandle_t, tdata_t*, toff_t*){
  return 0;
  }


// Dummy unmap file
static void tif_unmap_store(thandle_t, tdata_t, toff_t){
  }


// Compute size of what's been written
static toff_t tif_size_store(thandle_t handle){
  tiff_store_handle *h=(tiff_store_handle*)handle;
  return (h->end-h->begin);
  }


// Check if stream contains a TIFF
bool fxcheckTIF(FXStream& store){
  FXuchar signature[2];
  store.load(signature,2);
  store.position(-2,FXFromCurrent);
  return (signature[0]==0x4d && signature[1]==0x4d) || (signature[0]==0x49 && signature[1]==0x49);
  }


// Load a TIFF image
bool fxloadTIF(FXStream& store,FXColor*& data,FXint& width,FXint& height,FXushort& codec){
  tiff_store_handle s_handle;
  TIFFRGBAImage img;
  TIFF *image;
  char emsg[1024];
  register FXColor *up,*dn,t;
  register FXint i;
  register long size;

  // Null out
  data=NULL;
  width=0;
  height=0;

  // Set error/warning handlers
  TIFFSetErrorHandler(fxerrorhandler);
  TIFFSetWarningHandler(fxwarninghandler);

  // Initialize
  s_handle.store=&store;
  s_handle.begin=store.position();
  s_handle.end=store.position();
  s_handle.error=FALSE;

  // Open image
  image=TIFFClientOpen("tiff","rm",(thandle_t)&s_handle,tif_read_store,tif_write_store,tif_seek_store,tif_close_store,tif_size_store,tif_map_store,tif_unmap_store);
  if(!image) return false;

  // We try to remember the codec for later when we save the image back out...
  TIFFGetField(image,TIFFTAG_COMPRESSION,&codec);
  //FXTRACE((100,"fxloadTIF: codec=%d\n",codec));

  // FIXME TIFFRGBAImage{Begin,Get,End} is too broken!
  if(TIFFRGBAImageBegin(&img,image,0,emsg)!=1){
    TIFFClose(image);
    return false;
    }

  //FXTRACE((1,"fxloadTIF: width=%u height=%u alpha=%d bitspersample=%u samplesperpixel=%u orientation=%u photometric=%u\n",img.width,img.height,img.alpha,img.bitspersample,img.samplesperpixel,img.orientation,img.photometric));

  // Make room for data
  size=img.width*img.height;
  if(!FXMALLOC(&data,FXColor,size)){
    TIFFClose(image);
    return false;
    }

  // Get the pixels
  if(TIFFRGBAImageGet(&img,(uint32*)data,img.width,img.height)!=1){
    FXFREE(&data);
    TIFFClose(image);
    return false;
    }

  // If we got this far, we have the data;
  // nothing can go wrong from here on.
  width=(FXint)img.width;
  height=(FXint)img.height;

  // Maybe flip image upside down?
  if(img.orientation==ORIENTATION_TOPLEFT){
    for(up=data,dn=data+(height-1)*width; up<dn; up+=width,dn-=width){
      for(i=0; i<width; i++){ t=up[i]; up[i]=dn[i]; dn[i]=t; }
      }
    }

  TIFFRGBAImageEnd(&img);
  TIFFClose(image);
  return true;
  }


/*******************************************************************************/

// Save a TIFF image
bool fxsaveTIF(FXStream& store,const FXColor* data,FXint width,FXint height,FXushort codec){
  tiff_store_handle s_handle;
  long rows_per_strip,line;
  const TIFFCodec* coder;
  TIFF *image;

  // Must make sense
  if(!data || width<=0 || height<=0) return false;

  // Correct for unsupported codecs
  coder=TIFFFindCODEC(codec);
  if(coder==NULL) codec=COMPRESSION_PACKBITS;

  // Due to the infamous UNISYS patent, we can read LZW TIFF's but not
  // write them back as that would require the LZW compression algorithm!
  if(codec==COMPRESSION_LZW) codec=COMPRESSION_PACKBITS;

  //FXTRACE((100,"fxsaveTIF: codec=%d\n",codec));

  // Set error/warning handlers
  TIFFSetErrorHandler(fxerrorhandler);
  TIFFSetWarningHandler(fxwarninghandler);

  // Initialize
  s_handle.store=&store;
  s_handle.begin=store.position();
  s_handle.end=store.position();
  s_handle.error=FALSE;

  // Open image
  image=TIFFClientOpen("tiff","w",(thandle_t)&s_handle,tif_dummy_read_store,tif_write_store,tif_seek_store,tif_close_store,tif_size_store,tif_map_store,tif_unmap_store);
  if(!image) return FALSE;

  // Size of a strip is 16kb
  rows_per_strip=16*1024/width;
  if(rows_per_strip<1) rows_per_strip=1;

  // Set fields
  TIFFSetField(image,TIFFTAG_IMAGEWIDTH,width);
  TIFFSetField(image,TIFFTAG_IMAGELENGTH,height);
  TIFFSetField(image,TIFFTAG_COMPRESSION,codec);
  TIFFSetField(image,TIFFTAG_ORIENTATION,ORIENTATION_TOPLEFT);
  TIFFSetField(image,TIFFTAG_ROWSPERSTRIP,rows_per_strip);
  TIFFSetField(image,TIFFTAG_BITSPERSAMPLE,8);
  TIFFSetField(image,TIFFTAG_SAMPLESPERPIXEL,4);
  TIFFSetField(image,TIFFTAG_PLANARCONFIG,1);
  TIFFSetField(image,TIFFTAG_PHOTOMETRIC,PHOTOMETRIC_RGB);

  // Dump each line
  for(line=0; line<height; line++){
    if(TIFFWriteScanline(image,(void*)data,line,1)!=1 || s_handle.error){
      TIFFClose(image);
      return false;
      }
    data+=width;
    }
  TIFFClose(image);
  return true;
  }


/*******************************************************************************/


#else


// Check if stream contains a TIFF
bool fxcheckTIF(FXStream&){
  return false;
  }


// Stub routine
bool fxloadTIF(FXStream&,FXColor*& data,FXint& width,FXint& height,FXushort& codec){
  static const FXuchar tiff_bits[] = {
   0xff, 0xff, 0xff, 0xff, 0x01, 0x00, 0x00, 0x80, 0xfd, 0xff, 0xff, 0xbf,
   0x05, 0x00, 0x00, 0xa0, 0x05, 0x00, 0x00, 0xa0, 0x05, 0x00, 0x00, 0xa0,
   0x05, 0x00, 0x00, 0xa0, 0x05, 0x00, 0x00, 0xa0, 0x05, 0x00, 0x00, 0xa0,
   0x05, 0x00, 0x00, 0xa0, 0x05, 0x00, 0x00, 0xa0, 0x05, 0x00, 0x00, 0xa0,
   0x05, 0x00, 0x00, 0xa0, 0x05, 0x00, 0x00, 0xa0, 0x05, 0x00, 0x00, 0xa0,
   0x05, 0x00, 0x00, 0xa0, 0x05, 0x00, 0x00, 0xa0, 0xf5, 0x39, 0x9f, 0xaf,
   0x45, 0x10, 0x81, 0xa0, 0x45, 0x10, 0x81, 0xa0, 0x45, 0x10, 0x87, 0xa3,
   0x45, 0x10, 0x81, 0xa0, 0x45, 0x10, 0x81, 0xa0, 0x45, 0x10, 0x81, 0xa0,
   0x45, 0x38, 0x81, 0xa0, 0x05, 0x00, 0x00, 0xa0, 0x05, 0x00, 0x00, 0xa0,
   0x05, 0x00, 0x00, 0xa0, 0x05, 0x00, 0x00, 0xa0, 0xfd, 0xff, 0xff, 0xbf,
   0x01, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff};
  register FXint p;
  FXMALLOC(&data,FXColor,32*32);
  for(p=0; p<32*32; p++){
    data[p]=(tiff_bits[p>>3]&(1<<(p&7))) ? FXRGB(0,0,0) : FXRGB(255,255,255);
    }
  width=32;
  height=32;
  codec=1;
  return true;
  }


// Stub routine
bool fxsaveTIF(FXStream&,const FXColor*,FXint,FXint,FXushort){
  return false;
  }


#endif

}
