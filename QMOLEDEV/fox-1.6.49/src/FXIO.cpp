/********************************************************************************
*                                                                               *
*                        I / O   D e v i c e   C l a s s                        *
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
* $Id: FXIO.cpp,v 1.6 2006/01/22 17:58:31 fox Exp $                             *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxascii.h"
#include "FXHash.h"
#include "FXStream.h"
//#include "FXString.h"
//#include "FXPath.h"
#include "FXIO.h"
//#include "FXStat.h"
//#include "FXDir.h"



/*
  Notes:

  - An abstract class for low-level IO.
*/

#ifdef WIN32
#define BadHandle INVALID_HANDLE_VALUE
#else
#define BadHandle -1
#endif

using namespace FX;

/*******************************************************************************/

namespace FX {



// Construct
FXIO::FXIO():device(BadHandle),access(NoAccess){
  }


// Open file
bool FXIO::open(FXInputHandle handle,FXuint mode){
  device=handle;
  access=mode;
  return true;
  }


// Return true if open
bool FXIO::isOpen() const {
  return device!=BadHandle;
  }


// Attach existing file handle
void FXIO::attach(FXInputHandle handle,FXuint mode){
  close();
  device=handle;
  access=mode;
  }


// Detach existing file handle
void FXIO::detach(){
  device=BadHandle;
  access=NoAccess;
  }


// Get position
FXlong FXIO::position() const {
  return -1;
  }


// Move to position
FXlong FXIO::position(FXlong,FXuint){
  return -1;
  }


// Read block
FXival FXIO::readBlock(void*,FXival){
  return 0;
  }


// Write block
FXival FXIO::writeBlock(const void*,FXival count){
  return count;
  }


// Truncate file
FXlong FXIO::truncate(FXlong){
  return -1;
  }


// Synchronize disk with cached data
bool FXIO::flush(){
  return false;
  }


// Test if we're at the end
bool FXIO::eof(){
  return true;
  }


// Return file size
FXlong FXIO::size(){
  return 0;
  }


// Close file
bool FXIO::close(){
  device=BadHandle;
  return true;
  }


// Destroy
FXIO::~FXIO(){
  close();
  }


}

