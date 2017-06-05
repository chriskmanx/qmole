/********************************************************************************
*                                                                               *
*                   M e m o r y   S t r e a m   C l a s s e s                   *
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
* $Id: FXMemoryStream.cpp,v 1.17 2006/01/22 17:58:35 fox Exp $                  *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXObject.h"
#include "FXStream.h"
#include "FXMemoryStream.h"


/*
  Notes:
  - Also need memory mapped file stream.
*/


using namespace FX;


/*******************************************************************************/

namespace FX {


// Initialize memory stream
FXMemoryStream::FXMemoryStream(const FXObject* cont):FXStream(cont){
  }


// Write at least count bytes from the buffer
FXuval FXMemoryStream::writeBuffer(FXuval count){
  if(owns){ setSpace(getSpace()+count); }
  return endptr-wrptr;
  }


// Read at least count bytes into the buffer
FXuval FXMemoryStream::readBuffer(FXuval){
  return wrptr-rdptr;
  }


// Open a stream, possibly with an initial data array
bool FXMemoryStream::open(FXStreamDirection save_or_load,FXuchar* data){
  if(save_or_load!=FXStreamSave && save_or_load!=FXStreamLoad){fxerror("FXMemoryStream::open: illegal stream direction.\n");}
  if(FXStream::open(save_or_load,data?ULONG_MAX:16UL,data)){
    if(save_or_load==FXStreamSave){
      wrptr=begptr;
      rdptr=begptr;
      }
    else{
      wrptr=endptr;
      rdptr=begptr;
      }
    return true;
    }
  return false;
  }


// Open a stream, possibly with initial data array of certain size
bool FXMemoryStream::open(FXStreamDirection save_or_load,FXuval size,FXuchar* data){
  if(save_or_load!=FXStreamSave && save_or_load!=FXStreamLoad){fxerror("FXMemoryStream::open: illegal stream direction.\n");}
  if(FXStream::open(save_or_load,size,data)){
    if(save_or_load==FXStreamSave){
      wrptr=begptr;
      rdptr=begptr;
      }
    else{
      wrptr=endptr;
      rdptr=begptr;
      }
    return true;
    }
  return false;
  }


// Take buffer away from stream
void FXMemoryStream::takeBuffer(FXuchar*& data,FXuval& size){
  data=begptr;
  size=endptr-begptr;
  begptr=NULL;
  wrptr=NULL;
  rdptr=NULL;
  endptr=NULL;
  owns=false;
  }


// Give buffer to stream
void FXMemoryStream::giveBuffer(FXuchar *data,FXuval size){
  if(data==NULL){ fxerror("FXMemoryStream::giveBuffer: NULL buffer argument.\n"); }
  if(owns){FXFREE(&begptr);}
  begptr=data;
  endptr=data+size;
  if(dir==FXStreamSave){
    wrptr=begptr;
    rdptr=begptr;
    }
  else{
    wrptr=endptr;
    rdptr=begptr;
    }
  owns=true;
  }


// Close the stream
bool FXMemoryStream::close(){
  if(dir){
    if(owns){FXFREE(&begptr);}
    begptr=NULL;
    wrptr=NULL;
    rdptr=NULL;
    endptr=NULL;
    owns=false;
    return FXStream::close();
    }
  return false;
  }


// Move to position; if saving and we own the buffer, try to resize
// and 0-fill the space; if loading and not out of range, move the pointer;
// otherwise, return error code.
bool FXMemoryStream::position(FXlong offset,FXWhence whence){
  if(dir==FXStreamDead){ fxerror("FXMemoryStream::position: stream is not open.\n"); }
  if(code==FXStreamOK){
    if(whence==FXFromCurrent) offset=offset+pos;
    else if(whence==FXFromEnd) offset=offset+endptr-begptr;
    if(dir==FXStreamSave){
      if(begptr+offset>=endptr){
        if(!owns){ setError(FXStreamFull); return false; }
        setSpace(offset);
        if(begptr+offset>=endptr) return false;
        }
      wrptr=begptr+offset;
      }
    else{
      if(begptr+offset>=endptr){ setError(FXStreamEnd); return false; }
      rdptr=begptr+offset;
      }
    pos=offset;
    return true;
    }
  return false;
  }


}
