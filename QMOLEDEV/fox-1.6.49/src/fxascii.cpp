/********************************************************************************
*                                                                               *
*                    A S C I I   C h a r a c t e r   I n f o                    *
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
* $Id: fxascii.cpp,v 1.5.2.2 2006/08/09 20:01:07 fox Exp $                          *
********************************************************************************/
#include "xincs.h"
#include "fxdefs.h"
#include "fxascii.h"

/*
  Notes:

  - We need this to support non-locale sensitive ctype-like API's to operate
    on the lower 128 code points of UTF8-encoded unicode.  In other words, we
    need to work on characters and be secure that the multi-byte encoded UTF8
    will be left uninterpreted regardless of locale.
  - This file is pretty much cast in stone.
*/

/*******************************************************************************/



using namespace FX;

namespace FX {

namespace Ascii {

// Ascii table
const unsigned short ascii_data[256]={
  0x2004,0x0004,0x0004,0x0004,0x0004,0x0004,0x0004,0x0004,
  0x0004,0x0904,0x0104,0x0104,0x0104,0x0104,0x0004,0x0004,
  0x0004,0x0004,0x0004,0x0004,0x0004,0x0004,0x0004,0x0004,
  0x0004,0x0004,0x0004,0x0004,0x0004,0x0004,0x0004,0x0004,
  0x0940,0x00d0,0x00d0,0x00d0,0x20d0,0x00d0,0x00d0,0x00d0,
  0x00d0,0x00d0,0x00d0,0x20d0,0x00d0,0x00d0,0x00d0,0x00d0,
  0x0459,0x0459,0x0459,0x0459,0x0459,0x0459,0x0459,0x0459,
  0x0459,0x0459,0x00d0,0x00d0,0x20d0,0x20d0,0x20d0,0x00d0,
  0x00d0,0x4653,0x4653,0x4653,0x4653,0x4653,0x4653,0x4253,
  0x4253,0x4253,0x4253,0x4253,0x4253,0x4253,0x4253,0x4253,
  0x4253,0x4253,0x4253,0x4253,0x4253,0x4253,0x4253,0x4253,
  0x4253,0x4253,0x4253,0x00d0,0x00d0,0x00d0,0x20d0,0x00d0,
  0x20d0,0x4473,0x4473,0x4473,0x4473,0x4473,0x4473,0x4073,
  0x4073,0x4073,0x4073,0x4073,0x4073,0x4073,0x4073,0x4073,
  0x4073,0x4073,0x4073,0x4073,0x4073,0x4073,0x4073,0x4073,
  0x4073,0x4073,0x4073,0x00d0,0x20d0,0x00d0,0x20d0,0x0004,
  0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,
  0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,
  0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,
  0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,
  0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,
  0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,
  0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,
  0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,
  0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,
  0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,
  0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,
  0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,
  0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,
  0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,
  0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,
  0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,
  };


FXint digitValue(FXchar asc){
  return ('0'<=asc && asc<='9') ? (asc-'0') : ('a'<=asc && asc<='z') ? (asc-'a'+10) : ('A'<=asc && asc<='Z') ? asc-'A'+10 : -1;
  }


bool hasCase(FXchar asc){
  return (ascii_data[(FXuchar)asc]&0x4000)!=0;
  }


bool isUpper(FXchar asc){
  return (ascii_data[(FXuchar)asc]&0x200)!=0;
  }


bool isLower(FXchar asc){
  return (ascii_data[(FXuchar)asc]&0x20)!=0;
  }


bool isTitle(FXchar asc){
  return (ascii_data[(FXuchar)asc]&0x200)!=0;
  }


bool isAscii(FXchar asc){
  return ((FXuchar)asc)<128;
  }


bool isLetter(FXchar asc){
  return (ascii_data[(FXuchar)asc]&0x2)!=0;
  }


bool isDigit(FXchar asc){
  return (ascii_data[(FXuchar)asc]&0x8)!=0;
  }


bool isAlphaNumeric(FXchar asc){
  return (ascii_data[(FXuchar)asc]&0x1)!=0;
  }


bool isControl(FXchar asc){
  return (ascii_data[(FXuchar)asc]&0x4)!=0;
  }


bool isSpace(FXchar asc){
  return (ascii_data[(FXuchar)asc]&0x100)!=0;
  }


bool isBlank(FXchar asc){
  return (ascii_data[(FXuchar)asc]&0x800)!=0;
  }


bool isPunct(FXchar asc){
  return (ascii_data[(FXuchar)asc]&0x80)!=0;
  }


bool isGraph(FXchar asc){
  return (ascii_data[(FXuchar)asc]&0x10)!=0;
  }


bool isPrint(FXchar asc){
  return (ascii_data[(FXuchar)asc]&0x40)!=0;
  }


bool isHexDigit(FXchar asc){
  return (ascii_data[(FXuchar)asc]&0x400)!=0;
  }


bool isSymbol(FXchar asc){
  return (ascii_data[(FXuchar)asc]&0x2000)!=0;
  }


bool isSep(FXchar asc){
  return asc==' ';
  }


FXchar toUpper(FXchar asc){
  return ('a'<=asc && asc<='z') ? (asc-'a'+'A') : asc;
  }


FXchar toLower(FXchar asc){
  return ('A'<=asc && asc<='Z') ? (asc-'A'+'a') : asc;
  }


FXchar toTitle(FXchar asc){
  return ('a'<=asc && asc<='z') ? (asc-'a'+'A') : asc;
  }


}

}
