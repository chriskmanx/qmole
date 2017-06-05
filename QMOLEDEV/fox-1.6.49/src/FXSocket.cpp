/********************************************************************************
*                                                                               *
*                           S o c k e t   C l a s s                             *
*                                                                               *
*********************************************************************************
* Copyright (C) 2005,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXSocket.cpp,v 1.6.2.1 2006/11/07 15:58:53 fox Exp $                         *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxascii.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXPath.h"
#include "FXIO.h"
#include "FXSocket.h"



/*
  Notes:

  - Obviously this will get fleshed out some more...
*/


#ifdef WIN32
#define BadHandle INVALID_HANDLE_VALUE
#else
#define BadHandle -1
#endif


using namespace FX;

/*******************************************************************************/

namespace FX {



// Construct file and attach existing handle h
FXSocket::FXSocket(FXInputHandle handle,FXuint mode){
  FXIO::open(handle,mode);
  }


// Open device with access mode and handle
bool FXSocket::open(FXInputHandle handle,FXuint mode){
  return FXIO::open(handle,mode);
  }


// Read block
FXival FXSocket::readBlock(void* data,FXival count){
  FXival nread=-1;
  if(isOpen()){
#ifdef WIN32
    DWORD nr;
    if(::ReadFile(device,data,(DWORD)count,&nr,NULL)!=0){
      nread=(FXival)nr;
      }
#else
    do{
      nread=::read(device,data,count);
      }
    while(nread<0 && errno==EINTR);
#endif
    }
  return nread;
  }


// Write block
FXival FXSocket::writeBlock(const void* data,FXival count){
  FXival nwritten=-1;
  if(isOpen()){
#ifdef WIN32
    DWORD nw;
    if(::WriteFile(device,data,(DWORD)count,&nw,NULL)!=0){
      nwritten=(FXival)nw;
      }
#else
    do{
      nwritten=::write(device,data,count);
      }
    while(nwritten<0 && errno==EINTR);
#endif
    }
  return nwritten;
  }


// Close socket
bool FXSocket::close(){
  if(isOpen()){
    FXInputHandle dev=device;
    device=BadHandle;
#ifdef WIN32
    return ::CloseHandle(dev)!=0;
#else
    return ::close(dev)==0;
#endif
    }
  return false;
  }

// Destroy
FXSocket::~FXSocket(){
  close();
  }


}

