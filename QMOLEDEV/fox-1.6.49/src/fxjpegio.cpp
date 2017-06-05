/********************************************************************************
*                                                                               *
*                      J P E G    I n p u t / O u t p u t                       *
*                                                                               *
*********************************************************************************
* Copyright (C) 2000,2006 by David Tyree.   All Rights Reserved.                *
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
* $Id: fxjpegio.cpp,v 1.53 2006/01/22 17:58:53 fox Exp $                        *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"
#ifdef HAVE_JPEG_H
#undef FAR
extern "C" {
/* Theo Veenker <Theo.Veenker@let.uu.nl> says this is needed for CYGWIN */
#if (defined(__CYGWIN__) || defined(__MINGW32__) || defined(_MSC_VER)) && !defined(XMD_H)
#define XMD_H
typedef short INT16;
typedef int INT32;
#include "jpeglib.h"
#undef XMD_H
#elif defined __WINE__
#define XMD_H
#include "jpeglib.h"
#else
#include "jpeglib.h"
#endif
}
#endif

#include <setjmp.h>


/*
  To Do:
  - Add more options for fast jpeg loading.
  - Write a more detailed class that offers more options.
  - Add the ability to load jpegs in the background.
  - We should NOT assume that we can reposition the current stream position;
    for example, with bzip2 or gzip streams this is not possible.
  - References:

      http://www.ijg.org/
      ftp://ftp.uu.net/graphics/jpeg/
      http://the-labs.com
*/

#define JPEG_BUFFER_SIZE 4096


using namespace FX;

/*******************************************************************************/

namespace FX {


extern FXAPI bool fxcheckJPG(FXStream& store);
extern FXAPI bool fxloadJPG(FXStream& store,FXColor*& data,FXint& width,FXint& height,FXint& quality);
extern FXAPI bool fxsaveJPG(FXStream& store,const FXColor* data,FXint width,FXint height,FXint quality);


#ifdef HAVE_JPEG_H

// Source Manager for libjpeg
struct FOX_jpeg_source_mgr {
  struct jpeg_source_mgr pub;
  JOCTET    buffer[JPEG_BUFFER_SIZE];
  FXStream *stream;
  };


// Destination Manager for libjpeg
struct FOX_jpeg_dest_mgr {
  struct jpeg_destination_mgr pub;
  JOCTET    buffer[JPEG_BUFFER_SIZE];
  FXStream *stream;
  };


// For error handler
struct FOX_jpeg_error_mgr {
  struct jpeg_error_mgr error_mgr;
  jmp_buf jmpbuf;
  };


/*******************************************************************************/


// Fatal error use FOX's way of reporing errors
static void fatal_error(j_common_ptr cinfo){
  longjmp(((FOX_jpeg_error_mgr*)cinfo->err)->jmpbuf,1);
  }


// A no-op in our case
static void init_source(j_decompress_ptr){
  }


// Read JPEG_BUFFER_SIZE bytes into the buffer
// NOTE:- we need to read in one byte at a time, so as to make sure that
// data belonging to the objects following this JPEG remain in the stream!
static boolean fill_input_buffer(j_decompress_ptr cinfo){
  FOX_jpeg_source_mgr *src=(FOX_jpeg_source_mgr*)cinfo->src;
/*
src->stream->load(src->buffer,JPEG_BUFFER_SIZE);
src->pub.next_input_byte=src->buffer;
src->pub.bytes_in_buffer=JPEG_BUFFER_SIZE;
*/
  *src->stream >> src->buffer[0];
  if(src->stream->eof()){    // Insert a fake EOI marker
    src->buffer[0]=0xff;
    src->buffer[1]=JPEG_EOI;
    src->pub.next_input_byte=src->buffer;
    src->pub.bytes_in_buffer=2;
    }
  else{
    src->pub.next_input_byte=src->buffer;
    src->pub.bytes_in_buffer=1;
    }
  return TRUE;
  }


// Skip ahead some number of bytes
static void skip_input_data(j_decompress_ptr cinfo,long num_bytes){
  FOX_jpeg_source_mgr *src=(FOX_jpeg_source_mgr*)cinfo->src;
  if(num_bytes>0){
    while(num_bytes>(long)src->pub.bytes_in_buffer){
      num_bytes-=(long)src->pub.bytes_in_buffer;
      fill_input_buffer(cinfo);
      }
    src->pub.next_input_byte+=(size_t) num_bytes;
    src->pub.bytes_in_buffer-=(size_t) num_bytes;
    }
  }


// A no-op in our case
static void term_source(j_decompress_ptr){
  }


// Check if stream contains a JPG
bool fxcheckJPG(FXStream& store){
  FXuchar signature[2];
  store.load(signature,2);
  store.position(-2,FXFromCurrent);
  return signature[0]==0xFF && signature[1]==0xD8;
  }



// Load a JPEG image
bool fxloadJPG(FXStream& store,FXColor*& data,FXint& width,FXint& height,FXint&){
  jpeg_decompress_struct srcinfo;
  FOX_jpeg_error_mgr jerr;
  FOX_jpeg_source_mgr src;
  JSAMPLE *buffer[1];
  register FXColor *pp;
  register JSAMPLE *qq;
  int row_stride;

  // Null out
  data=NULL;
  width=0;
  height=0;

  // No sample buffer
  buffer[0]=NULL;

  // initialize the jpeg data structure;
  memset(&srcinfo,0,sizeof(srcinfo));
  jpeg_create_decompress(&srcinfo);

  // setup the error handler
  srcinfo.err=jpeg_std_error(&jerr.error_mgr);
  jerr.error_mgr.error_exit=fatal_error;

  // Set error handling
  if(setjmp(jerr.jmpbuf)){
    jpeg_destroy_decompress(&srcinfo);
    return FALSE;
    }

  // setup our src manager
  src.pub.init_source=init_source;
  src.pub.fill_input_buffer=fill_input_buffer;
  src.pub.resync_to_restart=jpeg_resync_to_restart;   // Use the default method
  src.pub.skip_input_data=skip_input_data;
  src.pub.term_source=term_source;
  src.pub.bytes_in_buffer=0;
  src.pub.next_input_byte=NULL;
  src.stream=&store;

  // set our src manager
  srcinfo.src=&src.pub;

  // read the header from the jpg;
  jpeg_read_header(&srcinfo,TRUE);

  // make sure the output is RGB
  srcinfo.out_color_space=JCS_RGB;

  jpeg_start_decompress(&srcinfo);

  row_stride=srcinfo.output_width*srcinfo.output_components;

  // Data to receive
  if(!FXMALLOC(&data,FXColor,srcinfo.image_height*srcinfo.image_width)){
    jpeg_destroy_decompress(&srcinfo);
    return false;
    }

  height=srcinfo.image_height;
  width=srcinfo.image_width;

  // Sample buffer
  if(!FXMALLOC(&buffer[0],JSAMPLE,row_stride)){
    FXFREE(&data);
    jpeg_destroy_decompress(&srcinfo);
    return false;
    }

  // Read the jpeg data
  pp=data;
  while(srcinfo.output_scanline<srcinfo.output_height){
    jpeg_read_scanlines(&srcinfo,buffer,1);
    qq=buffer[0];
    for(FXint i=0; i<width; i++,pp++){
      ((FXuchar*)pp)[0]=*qq++;
      ((FXuchar*)pp)[1]=*qq++;
      ((FXuchar*)pp)[2]=*qq++;
      ((FXuchar*)pp)[3]=255;
      }
    }

  // Clean up
  jpeg_finish_decompress(&srcinfo);
  jpeg_destroy_decompress(&srcinfo);
  FXFREE(&buffer[0]);
  return true;
  }


/*******************************************************************************/


// Initialize the buffer
static void init_destination(j_compress_ptr cinfo){
  FOX_jpeg_dest_mgr *dest=(FOX_jpeg_dest_mgr*)cinfo->dest;
  dest->pub.next_output_byte=dest->buffer;
  dest->pub.free_in_buffer=JPEG_BUFFER_SIZE;
  }


// Write the buffer to the stream
static boolean empty_output_buffer(j_compress_ptr cinfo){
  FOX_jpeg_dest_mgr *dest=(FOX_jpeg_dest_mgr*)cinfo->dest;
  dest->stream->save(dest->buffer,JPEG_BUFFER_SIZE);
  dest->pub.free_in_buffer=JPEG_BUFFER_SIZE;
  dest->pub.next_output_byte=dest->buffer;
  return TRUE;
  }


// Write any remaining data in the buffer to the stream
static void term_destination(j_compress_ptr cinfo){
  FOX_jpeg_dest_mgr *dest=(FOX_jpeg_dest_mgr*)cinfo->dest;
  dest->stream->save(dest->buffer,JPEG_BUFFER_SIZE-dest->pub.free_in_buffer);
  }


// Save a JPEG image
bool fxsaveJPG(FXStream& store,const FXColor* data,FXint width,FXint height,FXint quality){
  jpeg_compress_struct dstinfo;
  FOX_jpeg_error_mgr jerr;
  FOX_jpeg_dest_mgr dst;
  JSAMPLE *buffer[1];
  register const FXColor *pp;
  register JSAMPLE *qq;

  // Must make sense
  if(!data || width<=0 || height<=0 || quality<=0 || 100<quality) return false;

  // Row buffer
  if(!FXMALLOC(&buffer,JSAMPLE,width*3)) return false;

  // Specify the error manager
  memset(&dstinfo,0,sizeof(dstinfo));
  dstinfo.err=jpeg_std_error(&jerr.error_mgr);
  jerr.error_mgr.error_exit=fatal_error;

  // Set error handling
  if(setjmp(jerr.jmpbuf)){
    FXFREE(&buffer[0]);
    jpeg_destroy_compress(&dstinfo);
    return false;
    }

  // initialize the structure
  jpeg_create_compress(&dstinfo);

  // Specify the use of our destination manager
  dst.pub.init_destination=init_destination;
  dst.pub.empty_output_buffer=empty_output_buffer;
  dst.pub.term_destination=term_destination;
  dst.pub.free_in_buffer=0;
  dst.pub.next_output_byte=NULL;
  dst.stream=&store;

  // Set up the input parameters for the file
  dstinfo.image_width=width;
  dstinfo.image_height=height;
  dstinfo.input_components=3;
  dstinfo.in_color_space=JCS_RGB;
  dstinfo.dest=&dst.pub;

  jpeg_set_defaults(&dstinfo);
  jpeg_set_quality(&dstinfo,quality,TRUE);
  jpeg_start_compress(&dstinfo,TRUE);

  // Write the jpeg data
  pp=data;
  while(dstinfo.next_scanline<dstinfo.image_height){
    qq=buffer[0];
    for(FXint i=0; i<width; i++,pp++){
      *qq++=((FXuchar*)pp)[0];
      *qq++=((FXuchar*)pp)[1];
      *qq++=((FXuchar*)pp)[2];
      }
    jpeg_write_scanlines(&dstinfo,buffer,1);
    }

  // Clean up
  jpeg_finish_compress(&dstinfo);
  jpeg_destroy_compress(&dstinfo);
  FXFREE(&buffer[0]);
  return true;
  }


/*******************************************************************************/


#else


// Check if stream contains a JPG
bool fxcheckJPG(FXStream&){
  return false;
  }


// Stub routine
bool fxloadJPG(FXStream&,FXColor*& data,FXint& width,FXint& height,FXint& quality){
  static const FXuchar jpeg_bits[] = {
   0xff, 0xff, 0xff, 0xff, 0x01, 0x00, 0x00, 0x80, 0xfd, 0xff, 0xff, 0xbf,
   0x05, 0x00, 0x00, 0xa0, 0x05, 0x00, 0x00, 0xa0, 0x05, 0x00, 0x00, 0xa0,
   0x05, 0x00, 0x00, 0xa0, 0x05, 0x00, 0x00, 0xa0, 0x05, 0x00, 0x00, 0xa0,
   0x05, 0x00, 0x00, 0xa0, 0x05, 0x00, 0x00, 0xa0, 0x05, 0x00, 0x00, 0xa0,
   0x05, 0x00, 0x00, 0xa0, 0x05, 0x00, 0x00, 0xa0, 0x05, 0x00, 0x00, 0xa0,
   0x05, 0x00, 0x00, 0xa0, 0x05, 0x00, 0x00, 0xa0, 0xf5, 0x3d, 0x9f, 0xa3,
   0x05, 0x45, 0x41, 0xa4, 0x05, 0x45, 0x41, 0xa0, 0x05, 0x45, 0x47, 0xa0,
   0x05, 0x3d, 0x41, 0xa6, 0x05, 0x05, 0x41, 0xa4, 0x15, 0x05, 0x41, 0xa4,
   0xe5, 0x04, 0x9f, 0xa3, 0x05, 0x00, 0x00, 0xa0, 0x05, 0x00, 0x00, 0xa0,
   0x05, 0x00, 0x00, 0xa0, 0x05, 0x00, 0x00, 0xa0, 0xfd, 0xff, 0xff, 0xbf,
   0x01, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff};
  register FXint p;
  FXMALLOC(&data,FXColor,32*32);
  for(p=0; p<32*32; p++){
    data[p]=(jpeg_bits[p>>3]&(1<<(p&7))) ? FXRGB(0,0,0) : FXRGB(255,255,255);
    }
  width=32;
  height=32;
  quality=75;
  return true;
  }


// Stub routine
bool fxsaveJPG(FXStream&,const FXColor*,FXint,FXint,FXint){
  return false;
  }


#endif

}
