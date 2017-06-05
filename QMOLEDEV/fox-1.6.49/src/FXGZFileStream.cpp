/********************************************************************************
*                                                                               *
*                     G Z F i l e S t r e a m   C l a s s e s                   *
*                                                                               *
*********************************************************************************
* Copyright (C) 2002,2006 by Sander Jansen.   All Rights Reserved.              *
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
* $Id: FXGZFileStream.cpp,v 1.5.2.2 2007/09/28 16:42:20 fox Exp $                   *
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
#include "FXGZFileStream.h"

#ifdef HAVE_ZLIB_H
#include "zlib.h"

/*
  Notes:
  - Very basic compressed file I/O only.
  - Updated for new stream classes 2003/07/08.
  - Updated for FXFile 2005/09/03.
*/

#define BUFFERSIZE 8192

/*******************************************************************************/

namespace FX {


// Used during compression
struct ZBlock {
  z_stream stream;
  Bytef    buffer[BUFFERSIZE];
  };


// Initialize file stream
FXGZFileStream::FXGZFileStream(const FXObject* cont):FXFileStream(cont),z(NULL),f(0){
  }


// Save to a file
FXuval FXGZFileStream::writeBuffer(FXuval){
  register FXival m,n; int zerror;
  if(dir!=FXStreamSave){fxerror("FXGZFileStream::writeBuffer: wrong stream direction.\n");}
  FXASSERT(begptr<=rdptr);
  FXASSERT(rdptr<=wrptr);
  FXASSERT(wrptr<=endptr);
  while(rdptr<wrptr || f==Z_FINISH || f==Z_SYNC_FLUSH){
    z->stream.next_in=(Bytef*)rdptr;
    z->stream.avail_in=wrptr-rdptr;
    z->stream.next_out=z->buffer;
    z->stream.avail_out=BUFFERSIZE;
    zerror=deflate(&z->stream,f);
//    if(zerror!=Z_OK) break;
    if(zerror<0) break;  // break on error condition
    m=z->stream.next_out-z->buffer;
    n=file.writeBlock(z->buffer,m);
    if(n<m) break;
    rdptr=(FXuchar*)z->stream.next_in;
    if(zerror==Z_STREAM_END) break;  // break from FINISH/FLUSH
    }
  if(rdptr<wrptr){memmove(begptr,rdptr,wrptr-rdptr);}
  wrptr=begptr+(wrptr-rdptr);
  rdptr=begptr;
  return endptr-wrptr;
  }


// Load from file
FXuval FXGZFileStream::readBuffer(FXuval){
  register FXival n; int zerror;
  if(dir!=FXStreamLoad){fxerror("FXGZFileStream::readBuffer: wrong stream direction.\n");}
  FXASSERT(begptr<=rdptr);
  FXASSERT(rdptr<=wrptr);
  FXASSERT(wrptr<=endptr);
  if(rdptr<wrptr){memmove(begptr,rdptr,wrptr-rdptr);}
  wrptr=begptr+(wrptr-rdptr);
  rdptr=begptr;
  while(wrptr<endptr){
//    n=file.readBlock(z->buffer,BUFFERSIZE);
//    if(n<=0) break;
//    z->stream.next_in=z->buffer;
//    z->stream.avail_in=n;
    if(z->stream.avail_in<=0){  // Get more input if buffer is empty
      n=file.readBlock(z->buffer,BUFFERSIZE);
      if(n<0) break;
      z->stream.next_in=z->buffer;
      z->stream.avail_in=n;
      }
    z->stream.next_out=(Bytef*)wrptr;
    z->stream.avail_out=endptr-wrptr;
    zerror=inflate(&z->stream,Z_NO_FLUSH);
//    if(zerror!=Z_OK) break;
    if(zerror<0) break;  // break on error condition
    wrptr=(FXuchar*)z->stream.next_out;
    if(zerror==Z_STREAM_END) break;
    }
  return wrptr-rdptr;
  }


// Try open file stream
bool FXGZFileStream::open(const FXString& filename,FXStreamDirection save_or_load,FXuval size){
  if(FXFileStream::open(filename,save_or_load,size)){
    if(FXCALLOC(&z,ZBlock,1)){
      int zerror;
      z->stream.next_in=NULL;
      z->stream.avail_in=0;
      z->stream.next_out=NULL;
      z->stream.avail_out=0;
      f=Z_NO_FLUSH;
      if(save_or_load==FXStreamLoad){
        zerror=inflateInit(&z->stream);
        if(zerror==Z_OK) return true;
        code=FXStreamNoRead;
        }
      else{
        zerror=deflateInit(&z->stream,Z_DEFAULT_COMPRESSION);
        if(zerror==Z_OK) return true;
        code=FXStreamNoWrite;
        }
      FXFREE(&z);
      }
    FXFileStream::close();
    }
  return false;
  }


// Flush buffer
bool FXGZFileStream::flush(){
  bool status;
  int flush=f;
  if(f!=Z_FINISH) f=Z_SYNC_FLUSH;
  status=FXStream::flush();
  f=flush;
  return status;
  }

// Close file stream
bool FXGZFileStream::close(){
  if(dir){
    if(dir==FXStreamLoad){
      FXFileStream::close();
      inflateEnd(&z->stream);
      }
    else{
      f=Z_FINISH;
      FXFileStream::close();
      deflateEnd(&z->stream);
      }
    FXFREE(&z);
    return true;
    }
  return false;
  }


// Destructor
FXGZFileStream::~FXGZFileStream(){
  close();
  }

}

#endif
