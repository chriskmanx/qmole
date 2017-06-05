/********************************************************************************
*                                                                               *
*                   U n i c o d e   T e x t   C o d e c                         *
*                                                                               *
*********************************************************************************
* Copyright (C) 2002,2006 by L.Johnson & J.van der Zijp.  All Rights Reserved.  *
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
* $Id: FXTextCodec.cpp,v 1.43 2006/01/22 17:58:47 fox Exp $                     *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXDict.h"
#include "FXString.h"
#include "FXTextCodec.h"


/*
  Notes:

  - IANA defined mime names for character sets are found in:

        http://www.iana.org/assignments/character-sets.

  - Need API to count expected #characters needed for decode.

  - UTF-8 Encoding scheme:

    U-00000000 - U-0000007F 0xxxxxxx
    U-00000080 - U-000007FF 110xxxxx 10xxxxxx
    U-00000800 - U-0000FFFF 1110xxxx 10xxxxxx 10xxxxxx
    U-00010000 - U-001FFFFF 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    U-00200000 - U-03FFFFFF 111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
    U-04000000 - U-7FFFFFFF 1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx

    The last two cases shouldn't occur since all unicode is between 0 and 0x10FFFF.

  - Values to add:

    0xFFFFD080 - 11111111 11111111 11010000 10000000
    0xFFFE0080 - 11111111 11111110 00000000 10000000
    0xFFC00080 - 11111111 11000000 00000000 10000000
    0xF8000080 - 11111000 00000000 00000000 10000000
    0x00000080 - 00000000 00000000 00000000 10000000

  - UTF-16 Encoding scheme:

    W1 = 110110yy yyyyyyyy
    W2 = 110111xx xxxxxxxx

    Leading-surrogates or high-surrogates are from D800 to DBFF, and trailing-surrogates
    or low-surrogates are from DC00 to DFFF.

  - Unicode Transformation Formats information http://czyborra.com/utf.
  - The decoder should replace a malformed sequence with U+FFFD
  - See also: RFC 2279.
  - See RFC-1759 for printer MIB Enums.

  - 0x00000000   00000000
    0x00003080   00110000 1000000
    0x000E2080   00001110 00100000 10000000
    0x03C82080   00111100 11001000 00100000 10000000
    0xFA082080   11111010 00001000 00100000 10000000
    0x82082080   10000010 00001000 00100000 10000000

  - FIXME still unhappy with FXTextCodec's API's.  Change in FOX 1.8.
  - Note also, representation (e.g. 8, 16, 32 bit) should be independent
    of encoding/decoding; need to be able to store ascii in wide characters.
*/




/*******************************************************************************/

namespace FX {


// Base class is not instantiated
FXIMPLEMENT_ABSTRACT(FXTextCodec,FXObject,NULL,0)



/*********  Helpers to convert between unicode transformation formats  *********/


// Number of bytes for wide character
static inline FXint utflen(FXwchar w){
  return (w<0x80 ? 1 : (w<0x800 ? 2 : (w<0x10000 ? 3 : (w<0x200000 ? 4 : (w<0x4000000 ? 5 : 6)))));
  }


// Convert utf8 to wide character
FXint FXTextCodec::utf2wc(FXwchar& wc,const FXchar* src,FXint nsrc){
  register FXwchar c;
  if(nsrc<1) return -1;
  wc=c=(FXuchar)src[0];
  if(0x80<=c){
    if(c<0xC0) return 0;
    if(nsrc<2) return -2;
    if((FXuchar)src[1]<0x80) return 0;
    if((FXuchar)src[1]>0xBF) return 0;
    wc=(wc<<6)^(FXuchar)src[1]^0x0003080;
    if(0xE0<=c){
      if(nsrc<3) return -3;
      if((FXuchar)src[2]<0x80) return 0;
      if((FXuchar)src[2]>0xBF) return 0;
      wc=(wc<<6)^(FXuchar)src[2]^0x0020080;
      if(0xF0<=c){
        if(nsrc<4) return -4;
        if((FXuchar)src[3]<0x80) return 0;
        if((FXuchar)src[3]>0xBF) return 0;
        wc=(wc<<6)^(FXuchar)src[3]^0x0400080;
        if(0xF8<=c){
          if(nsrc<5) return -5;
          if((FXuchar)src[4]<0x80) return 0;
          if((FXuchar)src[4]>0xBF) return 0;
          wc=(wc<<6)^(FXuchar)src[4]^0x8000080;
          if(0xFC<=c){
            if(nsrc<6) return -6;
            if((FXuchar)src[5]<0x80) return 0;
            if((FXuchar)src[5]>0xBF) return 0;
            wc=(wc<<6)^(FXuchar)src[5]^0x0000080;
            return 6;
            }
          return 5;
          }
        return 4;
        }
      return 3;
      }
    return 2;
    }
  return 1;
  }


// Convert utf16 to wide character
FXint FXTextCodec::utf2wc(FXwchar& wc,const FXnchar* src,FXint nsrc){
  const FXint SURROGATE_OFFSET=0x10000-(0xD800<<10)-0xDC00;
  if(nsrc<1) return -1;
  wc=src[0];
  if(0xD800<=wc && wc<0xDC00){
    if(nsrc<2) return -2;
    if(src[1]<0xDC00 || 0xE000<=src[1]) return 0;
    wc=(wc<<10)+src[1]+SURROGATE_OFFSET;
    return 2;
    }
  return 1;
  }


// Convert utf32 to wide character
FXint FXTextCodec::utf2wc(FXwchar& wc,const FXwchar* src,FXint nsrc){
  if(nsrc<1) return -1;
  wc=src[0];
  return 1;
  }


// Convert wide character to utf8
FXint FXTextCodec::wc2utf(FXchar* dst,FXint ndst,FXwchar wc){
  if(ndst<1) return -1;
  if(wc>=0x80){
    if(ndst<2) return -2;
    if(wc>=0x800){
      if(ndst<3) return -3;
      if(wc>=0x10000){
        if(ndst<4) return -4;
        if(wc>=0x200000){
          if(ndst<5) return -5;
          if(wc>=0x4000000){
            if(ndst<6) return -6;
            dst[0]=(wc>>30)|0xFC;
            dst[1]=((wc>>24)&0X3F)|0x80;
            dst[2]=((wc>>18)&0X3F)|0x80;
            dst[3]=((wc>>12)&0X3F)|0x80;
            dst[4]=((wc>>6)&0X3F)|0x80;
            dst[5]=(wc&0X3F)|0x80;
            return 6;
            }
          dst[0]=(wc>>24)|0xF8;
          dst[1]=((wc>>18)&0x3F)|0x80;
          dst[2]=((wc>>12)&0x3F)|0x80;
          dst[3]=((wc>>6)&0x3F)|0x80;
          dst[4]=(wc&0x3F)|0x80;
          return 5;
          }
        dst[0]=(wc>>18)|0xF0;
        dst[1]=((wc>>12)&0x3F)|0x80;
        dst[2]=((wc>>6)&0x3F)|0x80;
        dst[3]=(wc&0x3F)|0x80;
        return 4;
        }
      dst[0]=(wc>>12)|0xE0;
      dst[1]=((wc>>6)&0x3F)|0x80;
      dst[2]=(wc&0x3F)|0x80;
      return 3;
      }
    dst[0]=(wc>>6)|0xC0;
    dst[1]=(wc&0x3F)|0x80;
    return 2;
    }
  dst[0]=wc;
  return 1;
  }


// Convert wide character to utf16
FXint FXTextCodec::wc2utf(FXnchar* dst,FXint ndst,FXwchar wc){
  const FXint LEAD_OFFSET=0xD800-(0x10000>>10);
  if(ndst<1) return -1;
  dst[0]=wc;
  if(0xFFFF<wc){
    if(ndst<2) return -2;
    dst[0]=(wc>>10)+LEAD_OFFSET;
    dst[1]=(wc&0x3FF)+0xDC00;
    return 2;
    }
  return 1;
  }


// Convert wide character to utf32
FXint FXTextCodec::wc2utf(FXwchar* dst,FXint ndst,FXwchar wc){
  if(ndst<1) return -1;
  dst[0]=wc;
  return 1;
  }


/*********  Convert arrays of characters from multi-byte to unicode  ***********/


// Convert multi-byte characters from src to single wide character
FXint FXTextCodec::mb2wc(FXwchar& wc,const FXchar* src,FXint nsrc) const {
  return utf2wc(wc,src,nsrc);
  }


// Count number of utf8 characters needed to convert multi-byte characters from src
FXint FXTextCodec::mb2utflen(const FXchar* src,FXint nsrc) const {
  register FXint nr,len=0;
  FXwchar w;
  if(src && 0<nsrc){
    do{
      nr=mb2wc(w,src,nsrc);
      if(nr<=0) return nr;
      src+=nr;
      nsrc-=nr;
      len+=utflen(w);
      }
    while(0<nsrc);
    }
  return len;
  }


// Count utf8 characters needed to convert multi-byte characters from src
FXint FXTextCodec::mb2utflen(const FXString& src) const {
  return mb2utflen(src.text(),src.length());
  }


// Convert multi-byte characters from src to utf8 characters at dst
FXint FXTextCodec::mb2utf(FXchar* dst,FXint ndst,const FXchar* src,FXint nsrc) const {
  register FXint nr,nw,len=0;
  FXwchar w;
  if(dst && src && 0<nsrc){
    do{
      nr=mb2wc(w,src,nsrc);
      if(nr<=0) return nr;
      src+=nr;
      nsrc-=nr;
      nw=wc2utf(dst,ndst,w);
      if(nw<=0) return nw;
      len+=nw;
      dst+=nw;
      ndst-=nw;
      }
    while(0<nsrc);
    }
  return len;
  }


// Convert multi-byte characters from src to utf8 characters at dst
FXint FXTextCodec::mb2utf(FXchar* dst,FXint ndst,const FXchar* src) const {
  return mb2utf(dst,ndst,src,strlen(src));
  }


// Convert multi-byte characters from src to utf8 characters at dst
FXint FXTextCodec::mb2utf(FXchar* dst,FXint ndst,const FXString& src) const {
  return mb2utf(dst,ndst,src.text(),src.length());
  }


// Convert multi-byte characters from src to utf8 string
FXString FXTextCodec::mb2utf(const FXchar* src,FXint nsrc) const {
  register FXint len;
  if(src && 0<nsrc){
    if((len=mb2utflen(src,nsrc))>0){
      FXString result;
      result.length(len);
      if(mb2utf(&result[0],len,src,nsrc)>0){
        return result;
        }
      }
    }
  return FXString::null;
  }


// Convert multi-byte characters from src to utf8 string
FXString FXTextCodec::mb2utf(const FXchar* src) const {
  return mb2utf(src,strlen(src));
  }


// Convert multi-byte string to utf8 string
FXString FXTextCodec::mb2utf(const FXString& src) const {
  return mb2utf(src.text(),src.length());
  }


/*********  Convert arrays of characters from unicode to multi-byte  ***********/


// Convert single wide character to multi-byte characters at dst
FXint FXTextCodec::wc2mb(FXchar* dst,FXint ndst,FXwchar wc) const {
  return wc2utf(dst,ndst,wc);
  }


// Count multi-byte characters characters needed to convert utf8 from src
FXint FXTextCodec::utf2mblen(const FXchar* src,FXint nsrc) const {
  register FXint nr,len=0;
  FXchar buffer[64];
  FXwchar w;
  if(src && 0<nsrc){
    do{
      nr=utf2wc(w,src,nsrc);
      if(nr<=0) return nr;
      src+=nr;
      nsrc-=nr;
      len+=wc2mb(buffer,sizeof(buffer),w);
      }
    while(0<nsrc);
    }
  return len;
  }


// Count multi-byte characters characters needed to convert utf8 from src
FXint FXTextCodec::utf2mblen(const FXString& src) const {
  return utf2mblen(src.text(),src.length());
  }


// Convert utf8 characters at src to multi-byte characters at dst
FXint FXTextCodec::utf2mb(FXchar* dst,FXint ndst,const FXchar* src,FXint nsrc) const {
  register FXint nr,nw,len=0;
  FXwchar w;
  if(dst && src && 0<nsrc){
    do{
      nr=utf2wc(w,src,nsrc);
      if(nr<=0) return nr;
      src+=nr;
      nsrc-=nr;
      nw=wc2mb(dst,ndst,w);
      if(nw<=0) return nw;
      len+=nw;
      dst+=nw;
      ndst-=nw;
      }
    while(0<nsrc);
    }
  return len;
  }


// Convert utf8 characters at src to multi-byte characters at dst
FXint FXTextCodec::utf2mb(FXchar* dst,FXint ndst,const FXchar* src) const {
  return utf2mb(dst,ndst,src,strlen(src));
  }


// Convert utf8 characters at src to multi-byte characters at dst
FXint FXTextCodec::utf2mb(FXchar* dst,FXint ndst,const FXString& src) const {
  return utf2mb(dst,ndst,src.text(),src.length());
  }


// Convert utf8 characters at src to multi-byte string
FXString FXTextCodec::utf2mb(const FXchar* src,FXint nsrc) const {
  register FXint len;
  if(src && 0<nsrc){
    if((len=utf2mblen(src,nsrc))>0){
      FXString result;
      result.length(len);
      if(utf2mb(&result[0],len,src,nsrc)>0){
        return result;
        }
      }
    }
  return FXString::null;
  }


// Convert utf8 characters at src to multi-byte string
FXString FXTextCodec::utf2mb(const FXchar* src) const {
  return utf2mb(src,strlen(src));
  }


// Convert utf8 string to multi-byte string
FXString FXTextCodec::utf2mb(const FXString& src) const {
  return utf2mb(src.text(),src.length());
  }


}
