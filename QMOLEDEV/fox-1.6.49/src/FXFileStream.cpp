/********************************************************************************
*                                                                               *
*                       F i l e   S t r e a m   C l a s s                       *
*                                                                               *
*********************************************************************************
* Copyright (C) 1997,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXFileStream.cpp,v 1.26 2006/01/22 17:58:26 fox Exp $                    *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXString.h"
#include "FXStream.h"
#include "FXObject.h"
#include "FXFile.h"
#include "FXFileStream.h"


/*
  Notes:
  - Future verions will use native system calls under WIN32.
*/

using namespace FX;


/*******************************************************************************/

namespace FX {


// Initialize file stream
FXFileStream::FXFileStream(const FXObject* cont):FXStream(cont){
  }


// Write at least count bytes from the buffer
FXuval FXFileStream::writeBuffer(FXuval){
  register FXival m,n;
  if(dir!=FXStreamSave){fxerror("FXFileStream::writeBuffer: wrong stream direction.\n");}
  FXASSERT(begptr<=rdptr);
  FXASSERT(rdptr<=wrptr);
  FXASSERT(wrptr<=endptr);
  m=wrptr-rdptr;
  n=file.writeBlock(rdptr,m);
  if(0<n){
    m-=n;
    if(m){memmove(begptr,rdptr+n,m);}
    rdptr=begptr;
    wrptr=begptr+m;
    }
  return endptr-wrptr;
  }


// Read at least count bytes into the buffer
FXuval FXFileStream::readBuffer(FXuval){
  register FXival m,n;
  if(dir!=FXStreamLoad){fxerror("FXFileStream::readBuffer: wrong stream direction.\n");}
  FXASSERT(begptr<=rdptr);
  FXASSERT(rdptr<=wrptr);
  FXASSERT(wrptr<=endptr);
  m=wrptr-rdptr;
  if(m){memmove(begptr,rdptr,m);}
  rdptr=begptr;
  wrptr=begptr+m;
  n=file.readBlock(wrptr,endptr-wrptr);
  if(0<n){
    wrptr+=n;
    }
  return wrptr-rdptr;
  }


// Open file stream
bool FXFileStream::open(const FXString& filename,FXStreamDirection save_or_load,FXuval size){
  if(save_or_load!=FXStreamSave && save_or_load!=FXStreamLoad){fxerror("FXFileStream::open: illegal stream direction.\n");}
  if(!dir){
    if(save_or_load==FXStreamLoad){
      if(!file.open(filename,FXIO::Reading)){
        code=FXStreamNoRead;
        return false;
        }
      }
    else if(save_or_load==FXStreamSave){
      if(!file.open(filename,FXIO::Writing)){
        code=FXStreamNoWrite;
        return false;
        }
      }
    return FXStream::open(save_or_load,size);
    }
  return false;
  }


// Close file stream
bool FXFileStream::close(){
  if(dir){
    if(dir==FXStreamSave) flush();
    file.close();
    return FXStream::close();
    }
  return false;
  }


// Move to position
bool FXFileStream::position(FXlong offset,FXWhence whence){
  register FXlong p;
  if(dir==FXStreamDead){ fxerror("FXMemoryStream::position: stream is not open.\n"); }
  if(code==FXStreamOK){
    FXASSERT(FXFromStart==SEEK_SET);
    FXASSERT(FXFromCurrent==SEEK_CUR);
    FXASSERT(FXFromEnd==SEEK_END);
    if(dir==FXStreamSave){

      // Flush unwritten data
      writeBuffer(0);

      // System's view of file pointer lags behind ours
      if(whence==FXFromCurrent) offset+=wrptr-rdptr;

      // Position file
      if((p=file.position(offset,whence))<0){
        code=FXStreamFull;
        return FALSE;
        }

      // Update pointers
      wrptr=begptr;
      rdptr=begptr;
      }
    else{

      // System's view of file pointer ahead of ours
      if(whence==FXFromCurrent) offset-=wrptr-rdptr;

      // Position file
      if((p=file.position(offset,whence))<0){
        code=FXStreamEnd;
        return false;
        }

      // Update pointers
      wrptr=begptr;
      rdptr=begptr;
      }
    pos=p;
    return true;
    }
  return false;
  }


// Close file stream
FXFileStream::~FXFileStream(){
  close();
  }


}
