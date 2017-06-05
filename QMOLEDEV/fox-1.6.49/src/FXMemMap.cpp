/********************************************************************************
*                                                                               *
*                      M e m o r y   M a p p e d   F i l e                      *
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
* $Id: FXMemMap.cpp,v 1.21 2006/01/22 17:58:35 fox Exp $                        *
********************************************************************************/
#include "xincs.h"
#include "fxdefs.h"
#include "FXString.h"
#include "FXMemMap.h"

/*
  Notes:
  - A memory mapped region of a file, or anonymous memory map.
  - Maybe use long sz = sysconf(_SC_PAGESIZE);
  - msync().
  - Need to bring in line with FXIO esp. with interpretation of options and so on.
*/


using namespace FX;

/*******************************************************************************/

namespace FX {


// Create new map object
FXMemMap::FXMemMap():mapbase(NULL),maplength(-1),mapoffset(0){
#ifndef WIN32
  handle=-1;
  file=-1;
#else
  handle=INVALID_HANDLE_VALUE;
  file=INVALID_HANDLE_VALUE;
#endif
  }


// Map file
void *FXMemMap::mapFile(const FXString& filename,long off,long len,FXuint access,FXuint share){
#ifndef WIN32
#ifdef HAVE_MMAP
  struct stat info;
  FXint prot,flags;

  // Set access flags
  flags=0;
  if(access&READ){ flags=O_RDONLY; }
  if(access&WRITE){ flags=O_RDWR|O_CREAT; }
  if(access&TRUNC){ flags|=O_TRUNC; }

  // Open file
  file=open(filename.text(),flags,0666);
  if(file!=-1){

    // If length was not passed, obtain the length
    if(fstat(file,&info)==0){

      // Map whole file
      if(len==-1) len=info.st_size-off;

      // Trying to map region larger than the file
      if(info.st_size<off+len){
        if(access&WRITE){
          ftruncate(file,off+len);      // Extend the file if writing
          }
        else{
          len=info.st_size-off;         // Map smaller region when reading
          }
        }

      // Set access flags
      prot=PROT_NONE;
      if(access&READ){ prot|=PROT_READ; }
      if(access&WRITE){ prot|=PROT_WRITE|PROT_READ; }
      if(access&EXEC){ prot|=PROT_EXEC; }

      // Map a view of the file
      flags=MAP_PRIVATE;
      if(share&SHAR){ flags=MAP_SHARED; }

      // Now map it
      maplength=len;
      mapoffset=off;
      mapbase=mmap(NULL,maplength,prot,flags,file,mapoffset);
      if(mapbase != (void*)MAP_FAILED){
        return mapbase;
        }
      }

    // Close it
    close(file);
    }
  handle=-1;
  file=-1;
#endif

#else
  DWORD prot,flags,junk;

  // Set access flags
  prot=0;
  flags=0;
  if(access&READ){ prot=GENERIC_READ; flags=FILE_SHARE_READ; }
  if(access&WRITE){ prot=GENERIC_WRITE|GENERIC_READ; flags=FILE_SHARE_READ|FILE_SHARE_WRITE; }

// Open file
#ifdef UNICODE
  FXnchar unifile[1024];
  utf2ncs(unifile,filename.text(),filename.length()+1);
  file=::CreateFileW(unifile,prot,flags,NULL,OPEN_ALWAYS,FILE_ATTRIBUTE_NORMAL|FILE_FLAG_RANDOM_ACCESS,NULL);
#else
  file=::CreateFileA(filename.text(),prot,flags,NULL,OPEN_ALWAYS,FILE_ATTRIBUTE_NORMAL|FILE_FLAG_RANDOM_ACCESS,NULL);
#endif
  if(file!=INVALID_HANDLE_VALUE){

    prot=0;
    if(access&READ){ prot=PAGE_READONLY; }
    if(access&WRITE){ prot=PAGE_READWRITE; }

    maplength=len;
    if(len<0) maplength=GetFileSize(file,&junk);

    // Create map object
    handle=CreateFileMapping(file,NULL,prot,0,maplength,NULL);
    if(handle!=NULL){

      prot=0;
      if(access&READ){ prot=FILE_MAP_READ; }
      if(access&WRITE){ prot=FILE_MAP_WRITE; }

      mapoffset=off;
      mapbase=MapViewOfFile(handle,prot,0,mapoffset,maplength);
      if(mapbase!=NULL){
        return mapbase;
        }
      CloseHandle(handle);
      }
    CloseHandle(file);
    }
  handle=INVALID_HANDLE_VALUE;
  file=INVALID_HANDLE_VALUE;
#endif
  mapbase=NULL;
  maplength=0;
  mapoffset=0;
  return NULL;
  }


// Unmap the view of the file
void* FXMemMap::unmap(){
#ifndef WIN32
#ifdef HAVE_MMAP
  if(mapbase){
    munmap((char*)mapbase,maplength);
    }
  if(file!=-1){
    close(file);
    }
  handle=-1;
  file=-1;
#endif
#else
  if(mapbase){
    UnmapViewOfFile(mapbase);
    }
  if(handle!=INVALID_HANDLE_VALUE){
    CloseHandle(handle);
    }
  if(file!=INVALID_HANDLE_VALUE){
    CloseHandle(file);
    }
  handle=INVALID_HANDLE_VALUE;
  file=INVALID_HANDLE_VALUE;
#endif
  mapbase=NULL;
  maplength=0;
  mapoffset=0;
  return NULL;
  }


// Synchronize disk
void FXMemMap::sync(){
#ifndef WIN32
#ifdef HAVE_MMAP
  if(mapbase){
    msync((char*)mapbase,(size_t)maplength,MS_SYNC|MS_INVALIDATE);
    }
#endif
#else
  if(mapbase){
    FlushViewOfFile(mapbase,(size_t)maplength);
    }
#endif
  }


// Delete the mapping
FXMemMap::~FXMemMap(){
  unmap();
  }


}

