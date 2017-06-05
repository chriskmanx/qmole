/********************************************************************************
*                                                                               *
*                      B Z F i l e S t r e a m   C l a s s e s                  *
*                                                                               *
*********************************************************************************
* Copyright (C) 1999,2006 by Lyle Johnson. All Rights Reserved.                 *
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
* $Id: FXBZFileStream.cpp,v 1.5.2.2 2007/09/28 16:42:20 fox Exp $                   *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXThread.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXObject.h"
#include "FXFile.h"
#include "FXBZFileStream.h"

#ifdef HAVE_BZ2LIB_H
#include <bzlib.h>

/*
  Notes:
  - Very basic compressed file I/O only.
  - Updated for new stream classes 2003/07/08.
  - Updated for use with FXFile 2005/09/03.
*/


#define BLOCKSIZE100K 1         // Block size x 100,000 bytes
#define VERBOSITY     0         // For tracing in bzip library
#define WORKFACTOR    0         // See bzip2 documentation
#define BUFFERSIZE    8192      // Size of the buffer


/*******************************************************************************/


namespace FX {


// Used during compression
struct BZBlock {
  bz_stream stream;
  char      buffer[BUFFERSIZE];
  };


// Initialize file stream
FXBZFileStream::FXBZFileStream(const FXObject* cont):FXFileStream(cont),bz(NULL),ac(0){
  }


// Save to a file
FXuval FXBZFileStream::writeBuffer(FXuval){
  register FXival m,n; int bzerror;
  if(dir!=FXStreamSave){fxerror("FXBZFileStream::writeBuffer: wrong stream direction.\n");}
  FXASSERT(begptr<=rdptr);
  FXASSERT(rdptr<=wrptr);
  FXASSERT(wrptr<=endptr);
  while(rdptr<wrptr || ac==BZ_FINISH || ac==BZ_FLUSH){
    bz->stream.next_in=(char*)rdptr;
    bz->stream.avail_in=wrptr-rdptr;
    bz->stream.next_out=bz->buffer;
    bz->stream.avail_out=BUFFERSIZE;
    bzerror=BZ2_bzCompress(&bz->stream,ac);
//    if(bzerror!=BZ_OK) break;
    if(bzerror<0) break;  // break on error condition
    m=bz->stream.next_out-bz->buffer;
    n=file.writeBlock(bz->buffer,m);
    if(n<m) break;
    rdptr=(FXuchar*)bz->stream.next_in;
    if(bzerror==BZ_STREAM_END) break;  // break from FINISH
    if(ac==BZ_FLUSH  && bzerror==BZ_RUN_OK) break;  // break from FLUSH
    }
  if(rdptr<wrptr){memmove(begptr,rdptr,wrptr-rdptr);}
  wrptr=begptr+(wrptr-rdptr);
  rdptr=begptr;
  return endptr-wrptr;
  }


// Load from file
FXuval FXBZFileStream::readBuffer(FXuval){
  register FXival n; int bzerror;
  if(dir!=FXStreamLoad){fxerror("FXBZFileStream::readBuffer: wrong stream direction.\n");}
  FXASSERT(begptr<=rdptr);
  FXASSERT(rdptr<=wrptr);
  FXASSERT(wrptr<=endptr);
  if(rdptr<wrptr){memmove(begptr,rdptr,wrptr-rdptr);}
  wrptr=begptr+(wrptr-rdptr);
  rdptr=begptr;
  while(wrptr<endptr){
//    n=file.readBlock(bz->buffer,BUFFERSIZE);
//    if(n<=0) break;
//    bz->stream.next_in=bz->buffer;
//    bz->stream.avail_in=n;
    if(bz->stream.avail_in<=0){ // get more input if buffer is empty
      n=file.readBlock(bz->buffer,BUFFERSIZE);
      if(n<0) break;
      bz->stream.next_in=bz->buffer;
      bz->stream.avail_in=n;
      }
    bz->stream.next_out=(char*)wrptr;
    bz->stream.avail_out=endptr-wrptr;
    bzerror=BZ2_bzDecompress(&bz->stream);
//    if(bzerror!=BZ_OK) break;
    if(bzerror<0) break;  // break on error condition
    wrptr=(FXuchar*)bz->stream.next_out;
    if(bzerror==BZ_STREAM_END) break;
    }
  return wrptr-rdptr;
  }


// Try open file stream
bool FXBZFileStream::open(const FXString& filename,FXStreamDirection save_or_load,FXuval size){
  if(FXFileStream::open(filename,save_or_load,size)){
    if(FXCALLOC(&bz,BZBlock,1)){
      int bzerror;
      bz->stream.next_in=NULL;
      bz->stream.avail_in=0;
      bz->stream.next_out=NULL;
      bz->stream.avail_out=0;
      ac=BZ_RUN;
      if(save_or_load==FXStreamLoad){
        bzerror=BZ2_bzDecompressInit(&bz->stream,VERBOSITY,0);
        if(bzerror==BZ_OK) return true;
        code=FXStreamNoRead;
        }
      else{
        bzerror=BZ2_bzCompressInit(&bz->stream,BLOCKSIZE100K,VERBOSITY,WORKFACTOR);
        if(bzerror==BZ_OK) return true;
        code=FXStreamNoWrite;
        }
      FXFREE(&bz);
      }
    FXFileStream::close();
    }
  return false;
  }


// Flush buffer
bool FXBZFileStream::flush(){
  bool status;
  int action=ac;
  if(ac!=BZ_FINISH) ac=BZ_FLUSH;
  status=FXStream::flush();
  ac=action;
  return status;
  }

// Close file stream
bool FXBZFileStream::close(){
  if(dir){
    if(dir==FXStreamLoad){
      FXFileStream::close();
      BZ2_bzDecompressEnd(&bz->stream);
      }
    else{
      ac=BZ_FINISH;
      FXFileStream::close();
      BZ2_bzCompressEnd(&bz->stream);
      }
    FXFREE(&bz);
    return true;
    }
  return false;
  }


// Destructor
FXBZFileStream::~FXBZFileStream(){
  close();
  }

}

#endif
