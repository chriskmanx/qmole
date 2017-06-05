/********************************************************************************
*                                                                               *
*                           S t r i n g   O b j e c t                           *
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
* $Id: FXString.cpp,v 1.218.2.1 2006/08/15 05:03:16 fox Exp $                       *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxascii.h"
#include "fxunicode.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXString.h"


/*
  Notes:
  - The special pointer-value null represents an empty "" string.
  - Strings are never NULL:- this speeds things up a lot as there is no
    need to check for NULL strings anymore.
  - In the new representation, '\0' is allowed as a character everywhere; but there
    is always an (uncounted) '\0' at the end.
  - The length preceeds the text in the buffer.
*/


// The string buffer is always rounded to a multiple of ROUNDVAL
// which must be 2^n.  Thus, small size changes will not result in any
// actual resizing of the buffer except when ROUNDVAL is exceeded.
#define ROUNDVAL    16

// Round up to nearest ROUNDVAL
#define ROUNDUP(n)  (((n)+ROUNDVAL-1)&-ROUNDVAL)

// This will come in handy
#define EMPTY       ((FXchar*)&emptystring[1])

using namespace FX;

/*******************************************************************************/

namespace FX {


// For conversion from UTF16 to UTF32
const FXint SURROGATE_OFFSET=0x10000-(0xD800<<10)-0xDC00;

// For conversion of UTF32 to UTF16
const FXint LEAD_OFFSET=0xD800-(0x10000>>10);


// Empty string
static const FXint emptystring[2]={0,0};


// Special NULL string
const FXchar FXString::null[4]={0,0,0,0};


// Numbers for hexadecimal
const FXchar FXString::hex[17]={'0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f',0};
const FXchar FXString::HEX[17]={'0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F',0};


// Length of a utf8 character representation
const signed char FXString::utfBytes[256]={
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,6,6,1,1
  };


/*******************************************************************************/

// Length of wide character string
static inline FXint strlen(const FXchar *src){
  return ::strlen(src);
  }

// Length of wide character string
static inline FXint strlen(const FXwchar *src){
  register FXint i=0;
  while(src[i]) i++;
  return i;
  }


// Length of narrow character string
static inline FXint strlen(const FXnchar *src){
  register FXint i=0;
  while(src[i]) i++;
  return i;
  }


/*******************************************************************************/


// Return wide character from utf8 string at ptr
FXwchar wc(const FXchar *ptr){
  register FXwchar w=(FXuchar)ptr[0];
  if(0xC0<=w){ w=(w<<6)^(FXuchar)ptr[1]^0x3080;
  if(0x800<=w){ w=(w<<6)^(FXuchar)ptr[2]^0x20080;
  if(0x10000<=w){ w=(w<<6)^(FXuchar)ptr[3]^0x400080;
  if(0x200000<=w){ w=(w<<6)^(FXuchar)ptr[4]^0x8000080;
  if(0x4000000<=w){ w=(w<<6)^(FXuchar)ptr[5]^0x80; }}}}}
  return w;
  }


// Return wide character from utf16 string at ptr
FXwchar wc(const FXnchar *ptr){
  register FXwchar w=ptr[0];
  if(0xD800<=w && w<0xDC00){ w=(w<<10)+ptr[1]+SURROGATE_OFFSET; }
  return w;
  }


// Return number of FXchar's of wide character at ptr
FXint wclen(const FXchar *ptr){
  return FXString::utfBytes[(FXuchar)ptr[0]];
  }


// Return number of FXnchar's of narrow character at ptr
FXint wclen(const FXnchar *ptr){
  return (0xD800<=ptr[0] && ptr[0]<0xDC00) ? 2 : 1;
  }


// Return start of utf8 character containing position
FXint wcvalidate(const FXchar* string,FXint pos){
  return (pos<=0 || FXISUTF(string[pos]) || --pos<=0 || FXISUTF(string[pos]) || --pos<=0 || FXISUTF(string[pos]) || --pos<=0 || FXISUTF(string[pos]) || --pos<=0 || FXISUTF(string[pos]) || --pos), pos;
  }


// Return start of utf16 character containing position
FXint wcvalidate(const FXnchar *string,FXint pos){
  return (pos<=0 || !(0xDC00<=string[pos] && string[pos]<=0xDFFF) || --pos), pos;
  }



// Advance to next utf8 character start
FXint wcinc(const FXchar* string,FXint pos){
  return (string[pos++]==0 || FXISUTF(string[pos]) || string[pos++]==0 || FXISUTF(string[pos]) || string[pos++]==0 || FXISUTF(string[pos]) || string[pos++]==0 || FXISUTF(string[pos]) || string[pos++]==0 || FXISUTF(string[pos]) || ++pos), pos;
  }


// Advance to next utf16 character start
FXint wcinc(const FXnchar *string,FXint pos){
  return ((0xDC00<=string[++pos] && string[pos]<=0xDFFF) || ++pos),pos;
  }



// Retreat to previous utf8 character start
FXint wcdec(const FXchar* string,FXint pos){
  return (--pos<=0 || FXISUTF(string[pos]) || --pos<=0 || FXISUTF(string[pos]) || --pos<=0 || FXISUTF(string[pos]) || --pos<=0 || FXISUTF(string[pos]) || --pos<=0 || FXISUTF(string[pos]) || --pos), pos;
  }


// Retreat to previous utf16 character start
FXint wcdec(const FXnchar *string,FXint pos){
  return (--pos<=0 || !(0xDC00<=string[pos] && string[pos]<=0xDFFF) || --pos), pos;
  }



// Return true if valid utf8 character sequence
bool isutfvalid(const FXchar* str){
  if((FXuchar)str[0]<0x80) return true;
  if((FXuchar)str[0]<0xC0) return false;
  if((FXuchar)str[1]<0x80) return false;
  if((FXuchar)str[1]>0xBF) return false;
  if((FXuchar)str[0]<0xE0) return true;
  if((FXuchar)str[2]<0x80) return false;
  if((FXuchar)str[2]>0xBF) return false;
  if((FXuchar)str[0]<0xF0) return true;
  if((FXuchar)str[3]<0x80) return false;
  if((FXuchar)str[3]>0xBF) return false;
  if((FXuchar)str[0]<0xF8) return true;
  if((FXuchar)str[4]<0x80) return false;
  if((FXuchar)str[4]>0xBF) return false;
  if((FXuchar)str[0]<0xFC) return true;
  if((FXuchar)str[5]<0x80) return false;
  if((FXuchar)str[5]>0xBF) return false;
  return true;
  }


// Length of utf8 representation of wide characters string str of length n
FXint utfslen(const FXwchar *str,FXint n){
  register FXint len=0;
  register FXint p=0;
  register FXwchar w;
  while(p<n){
    w=str[p++];
    len++;
    if(0x80<=w){ len++;
    if(0x800<=w){ len++;
    if(0x10000<=w){ len++;
    if(0x200000<=w){ len++;
    if(0x4000000<=w){ len++; }}}}}
    }
  return len;
  }


// Length of utf8 representation of wide character string str
FXint utfslen(const FXwchar *str){
  return utfslen(str,strlen(str));
  }


// Length of utf8 representation of narrow characters string str of length n
// Test for surrogates is deferred till code possibly exceeds 0xD800
FXint utfslen(const FXnchar *str,FXint n){
  register FXint len=0;
  register FXint p=0;
  register FXwchar w;
  while(p<n){
    w=str[p++];
    len++;
    if(0x80<=w){ len++;
    if(0x800<=w){ len++; if(0xD800<=w && w<0xDC00 && p<n){ w=(w<<10)+str[p++]+SURROGATE_OFFSET; }
    if(0x10000<=w){ len++;
    if(0x200000<=w){ len++;
    if(0x4000000<=w){ len++; }}}}}
    }
  return len;
  }


// Length of utf8 representation of narrow characters string str
FXint utfslen(const FXnchar *str){
  return utfslen(str,strlen(str));
  }


// Length of wide character representation of utf8 string str of length n
FXint wcslen(const FXchar *str,FXint n){
  register FXint len=0;
  register FXint p=0;
  while(p<n){
    p+=FXString::utfBytes[(FXuchar)str[p]];
    len++;
    }
  return len;
  }


// Length of wide character representation of utf8 string str
FXint wcslen(const FXchar *str){
  return wcslen(str,strlen(str));
  }


// Length of narrow character representation of utf8 string str of length n
// Assume surrogates are needed if utf8 code is more than 16 bits
FXint ncslen(const FXchar *str,FXint n){
  register FXint len=0;
  register FXint p=0;
  register FXwchar c;
  while(p<n){
    c=(FXuchar)str[p++];
    if(0xC0<=c){ p++;
    if(0xE0<=c){ p++;
    if(0xF0<=c){ p++;
    if(0xF8<=c){ p++;
    if(0xFC<=c){ p++; }} len++; }}}
    len++;
    }
  return len;
  }


// Length of narrow character representation of utf8 string str
FXint ncslen(const FXchar *str){
  return ncslen(str,strlen(str));
  }


/*******************************************************************************/


// Copy utf8 string of length n to wide character string dst
FXint utf2wcs(FXwchar *dst,const FXchar *src,FXint n){
  register FXint len=0;
  register FXint p=0;
  register FXwchar w;
  while(p<n){
    w=(FXuchar)src[p++];
    if(0xC0<=w){ w=(w<<6)^(FXuchar)src[p++]^0x3080;
    if(0x800<=w){ w=(w<<6)^(FXuchar)src[p++]^0x20080;
    if(0x10000<=w){ w=(w<<6)^(FXuchar)src[p++]^0x400080;
    if(0x200000<=w){ w=(w<<6)^(FXuchar)src[p++]^0x8000080;
    if(0x4000000<=w){ w=(w<<6)^(FXuchar)src[p++]^0x80; }}}}}
    dst[len++]=w;
    }
  return len;
  }


// Copy utf8 string to wide character string dst
FXint utf2wcs(FXwchar *dst,const FXchar *src){
  return utf2wcs(dst,src,strlen(src)+1);
  }


// Copy utf8 string of length n to narrow character string dst
// Assume surrogates are needed if utf8 code is more than 16 bits
FXint utf2ncs(FXnchar *dst,const FXchar *src,FXint n){
  register FXint len=0;
  register FXint p=0;
  register FXwchar w;
  while(p<n){
    w=(FXuchar)src[p++];
    if(0xC0<=w){ w=(w<<6)^(FXuchar)src[p++]^0x3080;
    if(0x800<=w){ w=(w<<6)^(FXuchar)src[p++]^0x20080;
    if(0x10000<=w){ w=(w<<6)^(FXuchar)src[p++]^0x400080;
    if(0x200000<=w){ w=(w<<6)^(FXuchar)src[p++]^0x8000080;
    if(0x4000000<=w){ w=(w<<6)^(FXuchar)src[p++]^0x80; }} dst[len++]=(w>>10)+LEAD_OFFSET; w=(w&0x3FF)+0xDC00; }}}
    dst[len++]=w;
    }
  return len;
  }


// Copy utf8 string to narrow character string dst
FXint utf2ncs(FXnchar *dst,const FXchar *src){
  return utf2ncs(dst,src,strlen(src)+1);
  }


/*******************************************************************************/

// Copy wide character substring of length n to dst
FXint wc2utfs(FXchar* dst,const FXwchar *src,FXint n){
  register FXint len=0;
  register FXint p=0;
  register FXwchar w;
  while(p<n){
    w=src[p++];
    if(w<0x80){
      dst[len++]=w;
      continue;
      }
    if(w<0x800){
      dst[len++]=(w>>6)|0xC0;
      dst[len++]=(w&0x3F)|0x80;
      continue;
      }
    if(w<0x10000){
      dst[len++]=(w>>12)|0xE0;
      dst[len++]=((w>>6)&0x3F)|0x80;
      dst[len++]=(w&0x3F)|0x80;
      continue;
      }
    if(w<0x200000){
      dst[len++]=(w>>18)|0xF0;
      dst[len++]=((w>>12)&0x3F)|0x80;
      dst[len++]=((w>>6)&0x3F)|0x80;
      dst[len++]=(w&0x3F)|0x80;
      continue;
      }
    if(w<0x4000000){
      dst[len++]=(w>>24)|0xF8;
      dst[len++]=((w>>18)&0x3F)|0x80;
      dst[len++]=((w>>12)&0x3F)|0x80;
      dst[len++]=((w>>6)&0x3F)|0x80;
      dst[len++]=(w&0x3F)|0x80;
      continue;
      }
    dst[len++]=(w>>30)|0xFC;
    dst[len++]=((w>>24)&0X3F)|0x80;
    dst[len++]=((w>>18)&0X3F)|0x80;
    dst[len++]=((w>>12)&0X3F)|0x80;
    dst[len++]=((w>>6)&0X3F)|0x80;
    dst[len++]=(w&0X3F)|0x80;
    }
  return len;
  }


// Copy wide character string to dst
FXint wc2utfs(FXchar* dst,const FXwchar *src){
  return wc2utfs(dst,src,strlen(src)+1);
  }


// Copy narrow character substring of length n to dst
// Test for surrogates is deferred till code possibly exceeds 0xD800
FXint nc2utfs(FXchar* dst,const FXnchar *src,FXint n){
  register FXint len=0;
  register FXint p=0;
  register FXwchar w;
  while(p<n){
    w=src[p++];
    if(w<0x80){
      dst[len++]=w;
      continue;
      }
    if(w<0x800){
      dst[len++]=(w>>6)|0xC0;
      dst[len++]=(w&0x3F)|0x80;
      continue;
      }
    if(0xD800<=w && w<0xDC00 && p<n){ w=(w<<10)+src[p++]+SURROGATE_OFFSET; }
    if(w<0x10000){
      dst[len++]=(w>>12)|0xE0;
      dst[len++]=((w>>6)&0x3F)|0x80;
      dst[len++]=(w&0x3F)|0x80;
      continue;
      }
    if(w<0x200000){
      dst[len++]=(w>>18)|0xF0;
      dst[len++]=((w>>12)&0x3F)|0x80;
      dst[len++]=((w>>6)&0x3F)|0x80;
      dst[len++]=(w&0x3F)|0x80;
      continue;
      }
    if(w<0x4000000){
      dst[len++]=(w>>24)|0xF8;
      dst[len++]=((w>>18)&0x3F)|0x80;
      dst[len++]=((w>>12)&0x3F)|0x80;
      dst[len++]=((w>>6)&0x3F)|0x80;
      dst[len++]=(w&0x3F)|0x80;
      continue;
      }
    dst[len++]=(w>>30)|0xFC;
    dst[len++]=((w>>24)&0X3F)|0x80;
    dst[len++]=((w>>18)&0X3F)|0x80;
    dst[len++]=((w>>12)&0X3F)|0x80;
    dst[len++]=((w>>6)&0X3F)|0x80;
    dst[len++]=(w&0X3F)|0x80;
    }
  return len;
  }


// Copy narrow character string to dst
FXint nc2utfs(FXchar* dst,const FXnchar *src){
  return nc2utfs(dst,src,strlen(src)+1);
  }

/*******************************************************************************/

// Change the length of the string to len
void FXString::length(FXint len){
  if(*(((FXint*)str)-1)!=len){
    if(0<len){
      if(str==EMPTY)
        str=sizeof(FXint)+(FXchar*)malloc(ROUNDUP(1+len)+sizeof(FXint));
      else
        str=sizeof(FXint)+(FXchar*)realloc(str-sizeof(FXint),ROUNDUP(1+len)+sizeof(FXint));
      str[len]=0;
      *(((FXint*)str)-1)=len;
      }
    else if(str!=EMPTY){
      free(str-sizeof(FXint));
      str=EMPTY;
      }
    }
  }


// Return start of utf8 character containing position
FXint FXString::validate(FXint p) const { return wcvalidate(str,p); }


// Advance to next utf8 character start
FXint FXString::inc(FXint p) const { return wcinc(str,p); }


// Retreat to previous utf8 character start
FXint FXString::dec(FXint p) const { return wcdec(str,p); }


// Return wide character starting at offset i
FXwchar FXString::wc(FXint i) const {
  register FXwchar w=(FXuchar)str[i];
  if(0xC0<=w){ w=(w<<6)^(FXuchar)str[i+1]^0x3080;
  if(0x800<=w){ w=(w<<6)^(FXuchar)str[i+2]^0x20080;
  if(0x10000<=w){ w=(w<<6)^(FXuchar)str[i+3]^0x400080;
  if(0x200000<=w){ w=(w<<6)^(FXuchar)str[i+4]^0x8000080;
  if(0x4000000<=w){ w=(w<<6)^(FXuchar)str[i+5]^0x80; }}}}}
  return w;
  }


// Count number of utf8 characters in subrange
FXint FXString::count(FXint pos,FXint len) const {
  register FXint cnt=0;
  while(pos<len){
    pos+=utfBytes[(FXuchar)str[pos]];
    cnt++;
    }
  return cnt;
  }


// Count number of utf8 characters
FXint FXString::count() const {
  return count(0,length());
  }


// Return index of utf8 character at byte offset
FXint FXString::index(FXint offs) const {
  register FXint len=length();
  register FXint i=0;
  register FXint p=0;
  while(p<offs && p<len){
    p+=utfBytes[(FXuchar)str[p]];
    i++;
    }
  return i;
  }


// Return byte offset of utf8 character at index
FXint FXString::offset(FXint indx) const {
  register FXint len=length();
  register FXint i=0;
  register FXint p=0;
  while(i<indx && p<len){
    p+=utfBytes[(FXuchar)str[p]];
    i++;
    }
  return p;
  }


// Simple construct
FXString::FXString():str(EMPTY){
  }


// Copy construct
FXString::FXString(const FXString& s):str(EMPTY){
  register FXint n=s.length();
  if(0<n){
    length(n);
    memcpy(str,s.str,n);
    }
  }


// Construct and init
FXString::FXString(const FXchar* s):str(EMPTY){
  if(s && s[0]){
    register FXint n=strlen(s);
    length(n);
    memcpy(str,s,n);
    }
  }


// Construct and init
FXString::FXString(const FXwchar* s):str(EMPTY){
  if(s && s[0]){
    register FXint n=utfslen(s);
    length(n);
    wc2utfs(str,s);
    }
  }


// Construct and init
FXString::FXString(const FXnchar* s):str(EMPTY){
  if(s && s[0]){
    register FXint n=utfslen(s);
    length(n);
    nc2utfs(str,s);
    }
  }


// Construct and init with substring
FXString::FXString(const FXchar* s,FXint n):str(EMPTY){
  if(s && 0<n){
    length(n);
    memcpy(str,s,n);
    }
  }


// Construct and init with wide character substring
FXString::FXString(const FXwchar* s,FXint m):str(EMPTY){
  if(s && 0<m){
    register FXint n=utfslen(s,m);
    length(n);
    wc2utfs(str,s,m);
    }
  }


// Construct and init with narrow character substring
FXString::FXString(const FXnchar* s,FXint m):str(EMPTY){
  if(s && 0<m){
    register FXint n=utfslen(s,m);
    length(n);
    nc2utfs(str,s,m);
    }
  }


// Construct and fill with constant
FXString::FXString(FXchar c,FXint n):str(EMPTY){
  if(0<n){
    length(n);
    memset(str,c,n);
    }
  }


// Return partition of string separated by delimiter delim
FXString FXString::section(FXchar delim,FXint start,FXint num) const {
  register FXint len=length(),s,e;
  s=0;
  if(0<start){
    while(s<len){
      ++s;
      if(str[s-1]==delim && --start==0) break;
      }
    }
  e=s;
  if(0<num){
    while(e<len){
      if(str[e]==delim && --num==0) break;
      ++e;
      }
    }
  return FXString(str+s,e-s);
  }


// Return partition of string separated by delimiters in delim
FXString FXString::section(const FXchar* delim,FXint n,FXint start,FXint num) const {
  register FXint len=length(),s,e,i;
  register FXchar c;
  s=0;
  if(0<start){
    while(s<len){
      c=str[s++];
      i=n;
      while(--i>=0){
        if(delim[i]==c){
          if(--start==0) goto a;
          break;
          }
        }
      }
    }
a:e=s;
  if(0<num){
    while(e<len){
      c=str[e];
      i=n;
      while(--i>=0){
        if(delim[i]==c){
          if(--num==0) goto b;
          break;
          }
        }
      ++e;
      }
    }
b:return FXString(str+s,e-s);
  }


// Return partition of string separated by delimiters in delim
FXString FXString::section(const FXchar* delim,FXint start,FXint num) const {
  return section(delim,strlen(delim),start,num);
  }


// Return partition of string separated by delimiters in delim
FXString FXString::section(const FXString& delim,FXint start,FXint num) const {
  return section(delim.text(),delim.length(),start,num);
  }


// Adopt string s, leaving s empty
FXString& FXString::adopt(FXString& s){
  if(this!=&s){
    if(str!=EMPTY){ free(str-sizeof(FXint)); }
    str=s.str;
    s.str=EMPTY;
    }
  return *this;
  }


// Assign input character to this string
FXString& FXString::assign(FXchar c){
  length(1);
  str[0]=c;
  return *this;
  }


// Assign input n characters c to this string
FXString& FXString::assign(FXchar c,FXint n){
  length(n);
  memset(str,c,n);
  return *this;
  }


// Assign first n characters of input string to this string
FXString& FXString::assign(const FXchar* s,FXint n){
  if(s && 0<n){
    length(n);
    memmove(str,s,n);
    }
  else{
    length(0);
    }
  return *this;
  }


// Assign first n characters of wide character string s to this string
FXString& FXString::assign(const FXwchar* s,FXint m){
  if(s && 0<m){
    register FXint n=utfslen(s,m);
    length(n);
    wc2utfs(str,s,m);
    }
  else{
    length(0);
    }
  return *this;
  }


// Assign first n characters of narrow character string s to this string
FXString& FXString::assign(const FXnchar* s,FXint m){
  if(s && 0<m){
    register FXint n=utfslen(s,m);
    length(n);
    nc2utfs(str,s,m);
    }
  else{
    length(0);
    }
  return *this;
  }


// Assign input string to this string
FXString& FXString::assign(const FXchar* s){
  if(s && s[0]){
    register FXint n=strlen(s);
    length(n);
    memmove(str,s,n);
    }
  else{
    length(0);
    }
  return *this;
  }


// Assign wide character string s to this string
FXString& FXString::assign(const FXwchar* s){
  if(s && s[0]){
    register FXint n=utfslen(s);
    length(n);
    wc2utfs(str,s);
    }
  else{
    length(0);
    }
  return *this;
  }


// Assign narrow character string s to this string
FXString& FXString::assign(const FXnchar* s){
  if(s && s[0]){
    register FXint n=utfslen(s);
    length(n);
    nc2utfs(str,s);
    }
  else{
    length(0);
    }
  return *this;
  }


// Assign input string to this string
FXString& FXString::assign(const FXString& s){
  if(str!=s.str) assign(s.str,s.length());
  return *this;
  }


// Assign a string
FXString& FXString::operator=(const FXchar* s){
  return assign(s);
  }


// Assign a wide character string to this
FXString& FXString::operator=(const FXwchar* s){
  return assign(s);
  }


// Assign a narrow character string to this
FXString& FXString::operator=(const FXnchar* s){
  return assign(s);
  }


// Assignment
FXString& FXString::operator=(const FXString& s){
  if(str!=s.str) assign(s.str,s.length());
  return *this;
  }


// Insert character at position
FXString& FXString::insert(FXint pos,FXchar c){
  register FXint len=length();
  length(len+1);
  if(pos<=0){
    memmove(str+1,str,len);
    str[0]=c;
    }
  else if(pos>=len){
    str[len]=c;
    }
  else{
    memmove(str+pos+1,str+pos,len-pos);
    str[pos]=c;
    }
  return *this;
  }


// Insert n characters c at specified position
FXString& FXString::insert(FXint pos,FXchar c,FXint n){
  if(0<n){
    register FXint len=length();
    length(len+n);
    if(pos<=0){
      memmove(str+n,str,len);
      memset(str,c,n);
      }
    else if(pos>=len){
      memset(str+len,c,n);
      }
    else{
      memmove(str+pos+n,str+pos,len-pos);
      memset(str+pos,c,n);
      }
    }
  return *this;
  }



// Insert string at position
FXString& FXString::insert(FXint pos,const FXchar* s,FXint n){
  if(s && 0<n){
    register FXint len=length();
    length(len+n);
    if(pos<=0){
      memmove(str+n,str,len);
      memcpy(str,s,n);
      }
    else if(pos>=len){
      memcpy(str+len,s,n);
      }
    else{
      memmove(str+pos+n,str+pos,len-pos);
      memcpy(str+pos,s,n);
      }
    }
  return *this;
  }


// Insert wide character string at position
FXString& FXString::insert(FXint pos,const FXwchar* s,FXint m){
  if(s && 0<m){
    register FXint len=length();
    register FXint n=utfslen(s,m);
    length(len+n);
    if(pos<=0){
      memmove(str+n,str,len);
      wc2utfs(str,s,m);
      }
    else if(pos>=len){
      wc2utfs(str+len,s,m);
      }
    else{
      memmove(str+pos+n,str+pos,len-pos);
      wc2utfs(str+pos,s,m);
      }
    }
  return *this;
  }


// Insert narrow character string at position
FXString& FXString::insert(FXint pos,const FXnchar* s,FXint m){
  if(s && 0<m){
    register FXint len=length();
    register FXint n=utfslen(s,m);
    length(len+n);
    if(pos<=0){
      memmove(str+n,str,len);
      nc2utfs(str,s,m);
      }
    else if(pos>=len){
      nc2utfs(str+len,s,m);
      }
    else{
      memmove(str+pos+n,str+pos,len-pos);
      nc2utfs(str+pos,s,m);
      }
    }
  return *this;
  }


// Insert string at position
FXString& FXString::insert(FXint pos,const FXchar* s){
  if(s && s[0]){
    register FXint len=length();
    register FXint n=strlen(s);
    length(len+n);
    if(pos<=0){
      memmove(str+n,str,len);
      memcpy(str,s,n);
      }
    else if(pos>=len){
      memcpy(str+len,s,n);
      }
    else{
      memmove(str+pos+n,str+pos,len-pos);
      memcpy(str+pos,s,n);
      }
    }
  return *this;
  }


// Insert wide character string at position
FXString& FXString::insert(FXint pos,const FXwchar* s){
  if(s && s[0]){
    register FXint len=length();
    register FXint n=utfslen(s);
    length(len+n);
    if(pos<=0){
      memmove(str+n,str,len);
      wc2utfs(str,s);
      }
    else if(pos>=len){
      wc2utfs(str+len,s);
      }
    else{
      memmove(str+pos+n,str+pos,len-pos);
      wc2utfs(str+pos,s);
      }
    }
  return *this;
  }


// Insert narrow character string at position
FXString& FXString::insert(FXint pos,const FXnchar* s){
  if(s && s[0]){
    register FXint len=length();
    register FXint n=utfslen(s);
    length(len+n);
    if(pos<=0){
      memmove(str+n,str,len);
      nc2utfs(str,s);
      }
    else if(pos>=len){
      nc2utfs(str+len,s);
      }
    else{
      memmove(str+pos+n,str+pos,len-pos);
      nc2utfs(str+pos,s);
      }
    }
  return *this;
  }


// Insert string at position
FXString& FXString::insert(FXint pos,const FXString& s){
  return insert(pos,s.str,s.length());
  }


// Append character c to this string
FXString& FXString::append(FXchar c){
  register FXint len=length();
  length(len+1);
  str[len]=c;
  return *this;
  }


// Append n characters c to this string
FXString& FXString::append(FXchar c,FXint n){
  if(0<n){
    register FXint len=length();
    length(len+n);
    memset(str+len,c,n);
    }
  return *this;
  }


// Append string to this string
FXString& FXString::append(const FXchar* s,FXint n){
  if(s && 0<n){
    register FXint len=length();
    length(len+n);
    memcpy(str+len,s,n);
    }
  return *this;
  }


// Append string to this string
FXString& FXString::append(const FXwchar* s,FXint m){
  if(s && 0<m){
    register FXint len=length();
    register FXint n=utfslen(s,m);
    length(len+n);
    wc2utfs(str+len,s,m);
    }
  return *this;
  }


// Append string to this string
FXString& FXString::append(const FXnchar* s,FXint m){
  if(s && 0<m){
    register FXint len=length();
    register FXint n=utfslen(s,m);
    length(len+n);
    nc2utfs(str+len,s,m);
    }
  return *this;
  }


// Append string to this string
FXString& FXString::append(const FXchar* s){
  if(s && s[0]){
    register FXint len=length();
    register FXint n=strlen(s);
    length(len+n);
    memcpy(str+len,s,n);
    }
  return *this;
  }


// Append string to this string
FXString& FXString::append(const FXwchar* s){
  if(s && s[0]){
    register FXint len=length();
    register FXint n=utfslen(s);
    length(len+n);
    wc2utfs(str+len,s);
    }
  return *this;
  }


// Append string to this string
FXString& FXString::append(const FXnchar* s){
  if(s && s[0]){
    register FXint len=length();
    register FXint n=utfslen(s);
    length(len+n);
    nc2utfs(str+len,s);
    }
  return *this;
  }


// Append string to this string
FXString& FXString::append(const FXString& s){
  return append(s.str,s.length());
  }


// Append character
FXString& FXString::operator+=(FXchar c){
  return append(c);
  }


// Append string
FXString& FXString::operator+=(const FXchar* s){
  return append(s);
  }


// Append string
FXString& FXString::operator+=(const FXwchar* s){
  return append(s);
  }


// Append string
FXString& FXString::operator+=(const FXnchar* s){
  return append(s);
  }


// Append FXString
FXString& FXString::operator+=(const FXString& s){
  return append(s);
  }


// Prepend character
FXString& FXString::prepend(FXchar c){
  register FXint len=length();
  length(len+1);
  memmove(str+1,str,len);
  str[0]=c;
  return *this;
  }


// Prepend string with n characters c
FXString& FXString::prepend(FXchar c,FXint n){
  if(0<n){
    register FXint len=length();
    length(len+n);
    memmove(str+n,str,len);
    memset(str,c,n);
    }
  return *this;
  }


// Prepend string
FXString& FXString::prepend(const FXchar* s,FXint n){
  if(s && 0<n){
    register FXint len=length();
    length(len+n);
    memmove(str+n,str,len);
    memcpy(str,s,n);
    }
  return *this;
  }


// Prepend wide character string
FXString& FXString::prepend(const FXwchar* s,FXint m){
  if(s && 0<m){
    register FXint len=length();
    register FXint n=utfslen(s,m);
    length(len+n);
    memmove(str+n,str,len);
    wc2utfs(str,s,m);
    }
  return *this;
  }


// Prepend narrow character string
FXString& FXString::prepend(const FXnchar* s,FXint m){
  if(s && 0<m){
    register FXint len=length();
    register FXint n=utfslen(s,m);
    length(len+n);
    memmove(str+n,str,len);
    nc2utfs(str,s,m);
    }
  return *this;
  }


// Prepend string
FXString& FXString::prepend(const FXchar* s){
  if(s && s[0]){
    register FXint len=length();
    register FXint n=strlen(s);
    length(len+n);
    memmove(str+n,str,len);
    memcpy(str,s,n);
    }
  return *this;
  }


// Prepend wide character string
FXString& FXString::prepend(const FXwchar* s){
  if(s && s[0]){
    register FXint len=length();
    register FXint n=utfslen(s);
    length(len+n);
    memmove(str+n,str,len);
    wc2utfs(str,s);
    }
  return *this;
  }


// Prepend narrow character string
FXString& FXString::prepend(const FXnchar* s){
  if(s && s[0]){
    register FXint len=length();
    register FXint n=utfslen(s);
    length(len+n);
    memmove(str+n,str,len);
    nc2utfs(str,s);
    }
  return *this;
  }


// Prepend string
FXString& FXString::prepend(const FXString& s){
  return prepend(s.str,s.length());
  }


// Replace character in string
FXString& FXString::replace(FXint pos,FXchar c){
  register FXint len=length();
  if(pos<0){
    length(len+1);
    memmove(str+1,str,len);
    str[0]=c;
    }
  else if(pos>=len){
    length(len+1);
    str[len]=c;
    }
  else{
    str[pos]=c;
    }
  return *this;
  }


// Replace the m characters at pos with n characters c
FXString& FXString::replace(FXint pos,FXint m,FXchar c,FXint n){
  register FXint len=length();
  if(pos<0){
    m+=pos;
    if(m<0) m=0;
    pos=0;
    }
  if(pos+m>len){
    if(pos>len) pos=len;
    m=len-pos;
    }
  if(m<n){
    length(len+n-m);
    memmove(str+pos+n,str+pos+m,len-pos-m);
    }
  else if(m>n){
    memmove(str+pos+n,str+pos+m,len-pos-m);
    length(len+n-m);
    }
  memset(str+pos,c,n);
  return *this;
  }


// Replace part of string
FXString& FXString::replace(FXint pos,FXint m,const FXchar* s,FXint n){
  register FXint len=length();
  if(pos<0){
    m+=pos;
    if(m<0) m=0;
    pos=0;
    }
  if(pos+m>len){
    if(pos>len) pos=len;
    m=len-pos;
    }
  if(m<n){
    length(len+n-m);
    memmove(str+pos+n,str+pos+m,len-pos-m);
    }
  else if(m>n){
    memmove(str+pos+n,str+pos+m,len-pos-m);
    length(len+n-m);
    }
  memcpy(str+pos,s,n);
  return *this;
  }


// Replace part of wide character string
FXString& FXString::replace(FXint pos,FXint m,const FXwchar* s,FXint n){
  register FXint w=utfslen(s,n);
  register FXint len=length();
  if(pos<0){
    m+=pos;
    if(m<0) m=0;
    pos=0;
    }
  if(pos+m>len){
    if(pos>len) pos=len;
    m=len-pos;
    }
  if(m<w){
    length(len+w-m);
    memmove(str+pos+w,str+pos+m,len-pos-m);
    }
  else if(m>w){
    memmove(str+pos+w,str+pos+m,len-pos-m);
    length(len+w-m);
    }
  wc2utfs(str+pos,s,n);
  return *this;
  }


// Replace part of narrow character string
FXString& FXString::replace(FXint pos,FXint m,const FXnchar* s,FXint n){
  register FXint w=utfslen(s,n);
  register FXint len=length();
  if(pos<0){
    m+=pos;
    if(m<0) m=0;
    pos=0;
    }
  if(pos+m>len){
    if(pos>len) pos=len;
    m=len-pos;
    }
  if(m<w){
    length(len+w-m);
    memmove(str+pos+w,str+pos+m,len-pos-m);
    }
  else if(m>w){
    memmove(str+pos+w,str+pos+m,len-pos-m);
    length(len+w-m);
    }
  nc2utfs(str+pos,s,n);
  return *this;
  }


// Replace part of string
FXString& FXString::replace(FXint pos,FXint m,const FXchar* s){
  return replace(pos,m,s,strlen(s));
  }


// Replace part of string
FXString& FXString::replace(FXint pos,FXint m,const FXwchar* s){
  return replace(pos,m,s,strlen(s));
  }


// Replace part of string
FXString& FXString::replace(FXint pos,FXint m,const FXnchar* s){
  return replace(pos,m,s,strlen(s));
  }


// Replace part of string
FXString& FXString::replace(FXint pos,FXint m,const FXString& s){
  return replace(pos,m,s.str,s.length());
  }


// Move range of m characters from src position to dst position
FXString& FXString::move(FXint dst,FXint src,FXint n){
  register FXint len=length();
  if(0<n && 0<=src && src+n<=len){
    if(dst<0){                                  // Move below begin
      if(dst<-n) dst=-n;
      length(len-dst);
      memmove(str-dst,str,len);
      memmove(str,str-dst+src,n);
      }
    else if(dst+n>len){                         // Move beyond end
      if(dst>len) dst=len;
      length(dst+n);
      memmove(str+dst,str+src,n);
      }
    else{
      memmove(str+dst,str+src,n);               // Move inside
      }
    }
  return *this;
  }


// Remove one character
FXString& FXString::erase(FXint pos){
  register FXint len=length();
  if(0<=pos && pos<len){
    memmove(str+pos,str+pos+1,len-pos-1);
    length(len-1);
    }
  return *this;
  }


// Remove section from buffer
FXString& FXString::erase(FXint pos,FXint n){
  if(0<n){
    register FXint len=length();
    if(pos<len && pos+n>0){
      if(pos<0){n+=pos;pos=0;}
      if(pos+n>len){n=len-pos;}
      memmove(str+pos,str+pos+n,len-pos-n);
      length(len-n);
      }
    }
  return *this;
  }


// Return number of occurrences of ch in string
FXint FXString::contains(FXchar ch) const {
  register FXint len=length();
  register FXint c=ch;
  register FXint m=0;
  register FXint i=0;
  while(i<len){
    if(str[i]==c){
      m++;
      }
    i++;
    }
  return m;
  }


// Return number of occurrences of string sub in string
FXint FXString::contains(const FXchar* sub,FXint n) const {
  register FXint len=length()-n;
  register FXint m=0;
  register FXint i=0;
  while(i<=len){
    if(compare(str+i,sub,n)==0){
      m++;
      }
    i++;
    }
  return m;
  }


// Return number of occurrences of string sub in string
FXint FXString::contains(const FXchar* sub) const {
  return contains(sub,strlen(sub));
  }


// Return number of occurrences of string sub in string
FXint FXString::contains(const FXString& sub) const {
  return contains(sub.text(),sub.length());
  }


// Concatenate two FXStrings
FXString operator+(const FXString& s1,const FXString& s2){
  FXString result(s1);
  return result.append(s2);
  }


// Concatenate FXString and string
FXString operator+(const FXString& s1,const FXchar* s2){
  FXString result(s1);
  return result.append(s2);
  }


// Concatenate FXString and wide character string
FXString operator+(const FXString& s1,const FXwchar* s2){
  FXString result(s1);
  return result.append(s2);
  }


// Concatenate FXString and narrow character string
FXString operator+(const FXString& s1,const FXnchar* s2){
  FXString result(s1);
  return result.append(s2);
  }


// Concatenate string and FXString
FXString operator+(const FXchar* s1,const FXString& s2){
  FXString result(s1);
  return result.append(s2);
  }


// Concatenate wide character string and FXString
FXString operator+(const FXwchar* s1,const FXString& s2){
  FXString result(s1);
  return result.append(s2);
  }


// Concatenate narrow character string and FXString
FXString operator+(const FXnchar* s1,const FXString& s2){
  FXString result(s1);
  return result.append(s2);
  }


// Concatenate FXString and character
FXString operator+(const FXString& s,FXchar c){
  FXString result(s);
  return result.append(c);
  }


// Concatenate character and FXString
FXString operator+(FXchar c,const FXString& s){
  FXString result(&c,1);
  return result.append(s);
  }


// Substitute one character by another
FXString& FXString::substitute(FXchar org,FXchar sub,bool all){
  register FXint len=length();
  register FXint c=org;
  register FXint s=sub;
  register FXint i=0;
  while(i<len){
    if(str[i]==c){
      str[i]=s;
      if(!all) break;
      }
    i++;
    }
  return *this;
  }


// Substitute one string by another
FXString& FXString::substitute(const FXchar* org,FXint olen,const FXchar* rep,FXint rlen,bool all){
  if(0<olen){
    register FXint pos=0;
    while(pos<=length()-olen){
      if(compare(str+pos,org,olen)==0){
        replace(pos,olen,rep,rlen);
        if(!all) break;
        pos+=rlen;
        continue;
        }
      pos++;
      }
    }
  return *this;
  }


// Substitute one string by another
FXString& FXString::substitute(const FXchar* org,const FXchar* rep,bool all){
  return substitute(org,strlen(org),rep,strlen(rep),all);
  }


// Substitute one string by another
FXString& FXString::substitute(const FXString& org,const FXString& rep,bool all){
  return substitute(org.text(),org.length(),rep.text(),rep.length(),all);
  }


// Simplify whitespace in string
FXString& FXString::simplify(){
  if(str!=EMPTY){
    register FXint s=0;
    register FXint d=0;
    register FXint e=length();
    while(s<e && Ascii::isSpace(str[s])) s++;
    while(1){
      while(s<e && !Ascii::isSpace(str[s])) str[d++]=str[s++];
      while(s<e && Ascii::isSpace(str[s])) s++;
      if(s>=e) break;
      str[d++]=' ';
      }
    length(d);
    }
  return *this;
  }


// Remove leading and trailing whitespace
FXString& FXString::trim(){
  if(str!=EMPTY){
    register FXint s=0;
    register FXint e=length();
    while(0<e && Ascii::isSpace(str[e-1])) e--;
    while(s<e && Ascii::isSpace(str[s])) s++;
    memmove(str,&str[s],e-s);
    length(e-s);
    }
  return *this;
  }


// Remove leading whitespace
FXString& FXString::trimBegin(){
  if(str!=EMPTY){
    register FXint s=0;
    register FXint e=length();
    while(s<e && Ascii::isSpace(str[s])) s++;
    memmove(str,str+s,e-s);
    length(e-s);
    }
  return *this;
  }


// Remove trailing whitespace
FXString& FXString::trimEnd(){
  if(str!=EMPTY){
    register FXint e=length();
    while(0<e && Ascii::isSpace(str[e-1])) e--;
    length(e);
    }
  return *this;
  }


// Truncate string
FXString& FXString::trunc(FXint pos){
  register FXint len=length();
  if(pos>len) pos=len;
  length(pos);
  return *this;
  }


// Clean string
FXString& FXString::clear(){
  length(0);
  return *this;
  }


// Get leftmost part
FXString FXString::left(FXint n) const {
  if(0<n){
    register FXint len=length();
    if(n>len) n=len;
    return FXString(str,n);
    }
  return FXString::null;
  }


// Get rightmost part
FXString FXString::right(FXint n) const {
  if(0<n){
    register FXint len=length();
    if(n>len) n=len;
    return FXString(str+len-n,n);
    }
  return FXString::null;
  }


// Get some part in the middle
FXString FXString::mid(FXint pos,FXint n) const {
  if(0<n){
    register FXint len=length();
    if(pos<len && pos+n>0){
      if(pos<0){n+=pos;pos=0;}
      if(pos+n>len){n=len-pos;}
      return FXString(str+pos,n);
      }
    }
  return FXString::null;
  }


// Return all characters before the nth occurrence of ch, searching forward
FXString FXString::before(FXchar c,FXint n) const {
  register FXint len=length();
  register FXint p=0;
  if(0<n){
    while(p<len){
      if(str[p]==c && --n==0) break;
      p++;
      }
    }
  return FXString(str,p);
  }


// Return all characters before the nth occurrence of ch, searching backward
FXString FXString::rbefore(FXchar c,FXint n) const {
  register FXint p=length();
  if(0<n){
    while(0<p){
      p--;
      if(str[p]==c && --n==0) break;
      }
    }
  return FXString(str,p);
  }


// Return all characters after the nth occurrence of ch, searching forward
FXString FXString::after(FXchar c,FXint n) const {
  register FXint len=length();
  register FXint p=0;
  if(0<n){
    while(p<len){
      p++;
      if(str[p-1]==c && --n==0) break;
      }
    }
  return FXString(str+p,len-p);
  }


// Return all characters after the nth occurrence of ch, searching backward
FXString FXString::rafter(FXchar c,FXint n) const {
  register FXint len=length();
  register FXint p=len;
  if(0<n){
    while(0<p){
      if(str[p-1]==c && --n==0) break;
      p--;
      }
    }
  return FXString(str+p,len-p);
  }


// Convert to lower case
FXString& FXString::lower(){
  FXString string;
  for(FXint p=0; p<length(); p=inc(p)){
    FXwchar w=Unicode::toLower(wc(p));
    string.append(&w,1);
    }
  adopt(string);
  return *this;
  }


// Convert to upper case
FXString& FXString::upper(){
  FXString string;
  for(FXint p=0; p<length(); p=inc(p)){
    FXwchar w=Unicode::toUpper(wc(p));
    string.append(&w,1);
    }
  adopt(string);
  return *this;
  }


// Compare strings
FXint compare(const FXchar* s1,const FXchar* s2){
  register const FXuchar *p1=(const FXuchar *)s1;
  register const FXuchar *p2=(const FXuchar *)s2;
  register FXint c1,c2;
  do{
    c1=*p1++;
    c2=*p2++;
    }
  while(c1 && (c1==c2));
  return c1-c2;
  }


FXint compare(const FXchar* s1,const FXString& s2){
  return compare(s1,s2.str);
  }


FXint compare(const FXString& s1,const FXchar* s2){
  return compare(s1.str,s2);
  }


FXint compare(const FXString& s1,const FXString& s2){
  return compare(s1.str,s2.str);
  }


// Compare strings up to n
FXint compare(const FXchar* s1,const FXchar* s2,FXint n){
  register const FXuchar *p1=(const FXuchar *)s1;
  register const FXuchar *p2=(const FXuchar *)s2;
  register FXint c1,c2;
  if(0<n){
    do{
      c1=*p1++;
      c2=*p2++;
      }
    while(--n && c1 && (c1==c2));
    return c1-c2;
    }
  return 0;
  }


FXint compare(const FXchar* s1,const FXString& s2,FXint n){
  return compare(s1,s2.str,n);
  }


FXint compare(const FXString& s1,const FXchar* s2,FXint n){
  return compare(s1.str,s2,n);
  }


FXint compare(const FXString& s1,const FXString& s2,FXint n){
  return compare(s1.str,s2.str,n);
  }


// Compare utf-8 strings case insensitive
// At each iteration through the loop, the following is true:
//   o Both characters are ascii.  In this case, fold using Ascii
//     and compare.
//   o One of the two is ascii.  This means the other is > 0x80
//     so fold using Ascii [leaving upper 128 codes invariant], and
//     compare; they will be unequal so we'll fall out of the loop.
//   o Both characters are wide.  This is the complex case, and
//     here we have to obtain the wide character, convert to lower
//     case using the Unicode, and compare.  Skip to the start of
//     the next character by consulting character-width table.
FXint comparecase(const FXchar* s1,const FXchar* s2){
  register FXint c1,c2;
  do{
    if((*s1 & 0x80) && (*s2 & 0x80)){
      c1=Unicode::toLower(wc(s1)); s1+=wclen(s1);
      c2=Unicode::toLower(wc(s2)); s2+=wclen(s2);
      }
    else{
      c1=Ascii::toLower(*s1); s1+=1;
      c2=Ascii::toLower(*s2); s2+=1;
      }
    }
  while(c1 && (c1==c2));
  return c1-c2;
  }


// Compare strings case insensitive up to n
FXint comparecase(const FXchar* s1,const FXchar* s2,FXint n){
  register FXint c1,c2;
  if(0<n){
    do{
      if((*s1 & 0x80) && (*s2 & 0x80)){
        c1=Unicode::toLower(wc(s1)); s1+=wclen(s1);
        c2=Unicode::toLower(wc(s2)); s2+=wclen(s2);
        }
      else{
        c1=Ascii::toLower(*s1); s1+=1;
        c2=Ascii::toLower(*s2); s2+=1;
        }
      }
    while(--n && c1 && (c1==c2));
    return c1-c2;
    }
  return 0;
  }


#if 0
// Compare strings case insensitive
FXint comparecase(const FXchar* s1,const FXchar* s2){
  register const FXuchar *p1=(const FXuchar *)s1;
  register const FXuchar *p2=(const FXuchar *)s2;
  register FXint c1,c2;
  do{
    c1=Ascii::toLower(*p1++);
    c2=Ascii::toLower(*p2++);
    }
  while(c1 && (c1==c2));
  return c1-c2;
  }
#endif

FXint comparecase(const FXchar* s1,const FXString& s2){
  return comparecase(s1,s2.str);
  }


FXint comparecase(const FXString& s1,const FXchar* s2){
  return comparecase(s1.str,s2);
  }


FXint comparecase(const FXString& s1,const FXString& s2){
  return comparecase(s1.str,s2.str);
  }


#if 0
// Compare strings case insensitive up to n
FXint comparecase(const FXchar* s1,const FXchar* s2,FXint n){
  register const FXuchar *p1=(const FXuchar *)s1;
  register const FXuchar *p2=(const FXuchar *)s2;
  register FXint c1,c2;
  if(0<n){
    do{
      c1=Ascii::toLower(*p1++);
      c2=Ascii::toLower(*p2++);
      }
    while(--n && c1 && (c1==c2));
    return c1-c2;
    }
  return 0;
  }
#endif

FXint comparecase(const FXchar* s1,const FXString& s2,FXint n){
  return comparecase(s1,s2.str,n);
  }

FXint comparecase(const FXString& s1,const FXchar* s2,FXint n){
  return comparecase(s1.str,s2,n);
  }

FXint comparecase(const FXString& s1,const FXString& s2,FXint n){
  return comparecase(s1.str,s2.str,n);
  }

// Comparison operators
bool operator==(const FXString& s1,const FXString& s2){
  return compare(s1.str,s2.str)==0;
  }

bool operator==(const FXString& s1,const FXchar* s2){
  return compare(s1.str,s2)==0;
  }

bool operator==(const FXchar* s1,const FXString& s2){
  return compare(s1,s2.str)==0;
  }

bool operator!=(const FXString& s1,const FXString& s2){
  return compare(s1.str,s2.str)!=0;
  }

bool operator!=(const FXString& s1,const FXchar* s2){
  return compare(s1.str,s2)!=0;
  }

bool operator!=(const FXchar* s1,const FXString& s2){
  return compare(s1,s2.str)!=0;
  }

bool operator<(const FXString& s1,const FXString& s2){
  return compare(s1.str,s2.str)<0;
  }

bool operator<(const FXString& s1,const FXchar* s2){
  return compare(s1.str,s2)<0;
  }

bool operator<(const FXchar* s1,const FXString& s2){
  return compare(s1,s2.str)<0;
  }

bool operator<=(const FXString& s1,const FXString& s2){
  return compare(s1.str,s2.str)<=0;
  }

bool operator<=(const FXString& s1,const FXchar* s2){
  return compare(s1.str,s2)<=0;
  }

bool operator<=(const FXchar* s1,const FXString& s2){
  return compare(s1,s2.str)<=0;
  }

bool operator>(const FXString& s1,const FXString& s2){
  return compare(s1.str,s2.str)>0;
  }

bool operator>(const FXString& s1,const FXchar* s2){
  return compare(s1.str,s2)>0;
  }

bool operator>(const FXchar* s1,const FXString& s2){
  return compare(s1,s2.str)>0;
  }

bool operator>=(const FXString& s1,const FXString& s2){
  return compare(s1.str,s2.str)>=0;
  }

bool operator>=(const FXString& s1,const FXchar* s2){
  return compare(s1.str,s2)>=0;
  }

bool operator>=(const FXchar* s1,const FXString& s2){
  return compare(s1,s2.str)>=0;
  }


// Find n-th occurrence of character, searching forward; return position or -1
FXint FXString::find(FXchar c,FXint pos,FXint n) const {
  register FXint len=length();
  register FXint p=pos;
  register FXint cc=c;
  if(p<0) p=0;
  if(n<=0) return p;
  while(p<len){
    if(str[p]==cc){ if(--n==0) return p; }
    ++p;
    }
  return -1;
  }


// Find n-th occurrence of character, searching backward; return position or -1
FXint FXString::rfind(FXchar c,FXint pos,FXint n) const {
  register FXint len=length();
  register FXint p=pos;
  register FXint cc=c;
  if(p>=len) p=len-1;
  if(n<=0) return p;
  while(0<=p){
    if(str[p]==cc){ if(--n==0) return p; }
    --p;
    }
  return -1;
  }


// Find a character, searching forward; return position or -1
FXint FXString::find(FXchar c,FXint pos) const {
  register FXint len=length();
  register FXint p=pos;
  register FXint cc=c;
  if(p<0) p=0;
  while(p<len){ if(str[p]==cc){ return p; } ++p; }
  return -1;
  }


// Find a character, searching backward; return position or -1
FXint FXString::rfind(FXchar c,FXint pos) const {
  register FXint len=length();
  register FXint p=pos;
  register FXint cc=c;
  if(p>=len) p=len-1;
  while(0<=p){ if(str[p]==cc){ return p; } --p; }
  return -1;
  }


// Find a substring of length n, searching forward; return position or -1
FXint FXString::find(const FXchar* substr,FXint n,FXint pos) const {
  register FXint len=length();
  if(0<=pos && 0<n && n<=len){
    register FXint c=substr[0];
    len=len-n+1;
    while(pos<len){
      if(str[pos]==c){
        if(!compare(str+pos,substr,n)){
          return pos;
          }
        }
      pos++;
      }
    }
  return -1;
  }


// Find a substring, searching forward; return position or -1
FXint FXString::find(const FXchar* substr,FXint pos) const {
  return find(substr,strlen(substr),pos);
  }


// Find a substring, searching forward; return position or -1
FXint FXString::find(const FXString& substr,FXint pos) const {
  return find(substr.text(),substr.length(),pos);
  }


// Find a substring of length n, searching backward; return position or -1
FXint FXString::rfind(const FXchar* substr,FXint n,FXint pos) const {
  register FXint len=length();
  if(0<=pos && 0<n && n<=len){
    register FXint c=substr[0];
    len-=n;
    if(pos>len) pos=len;
    while(0<=pos){
      if(str[pos]==c){
        if(!compare(str+pos,substr,n)){
          return pos;
          }
        }
      pos--;
      }
    }
  return -1;
  }


// Find a substring, searching backward; return position or -1
FXint FXString::rfind(const FXchar* substr,FXint pos) const {
  return rfind(substr,strlen(substr),pos);
  }


// Find a substring, searching backward; return position or -1
FXint FXString::rfind(const FXString& substr,FXint pos) const {
  return rfind(substr.text(),substr.length(),pos);
  }


// Find first character in the set of size n, starting from pos; return position or -1
FXint FXString::find_first_of(const FXchar* set,FXint n,FXint pos) const {
  register FXint len=length();
  register FXint p=pos;
  if(p<0) p=0;
  while(p<len){
    register FXint c=str[p];
    register FXint i=n;
    while(--i>=0){ if(set[i]==c) return p; }
    p++;
    }
  return -1;
  }


// Find first character in the set, starting from pos; return position or -1
FXint FXString::find_first_of(const FXchar* set,FXint pos) const {
  return find_first_of(set,strlen(set),pos);
  }


// Find first character in the set, starting from pos; return position or -1
FXint FXString::find_first_of(const FXString& set,FXint pos) const {
  return find_first_of(set.text(),set.length(),pos);
  }


// Find first character, starting from pos; return position or -1
FXint FXString::find_first_of(FXchar c,FXint pos) const {
  register FXint len=length();
  register FXint p=pos;
  register FXint cc=c;
  if(p<0) p=0;
  while(p<len){ if(str[p]==cc){ return p; } p++; }
  return -1;
  }


// Find last character in the set of size n, starting from pos; return position or -1
FXint FXString::find_last_of(const FXchar* set,FXint n,FXint pos) const {
  register FXint len=length();
  register FXint p=pos;
  if(p>=len) p=len-1;
  while(0<=p){
    register FXint c=str[p];
    register FXint i=n;
    while(--i>=0){ if(set[i]==c) return p; }
    p--;
    }
  return -1;
  }


// Find last character in the set, starting from pos; return position or -1
FXint FXString::find_last_of(const FXchar* set,FXint pos) const {
  return find_last_of(set,strlen(set),pos);
  }


// Find last character in the set, starting from pos; return position or -1
FXint FXString::find_last_of(const FXString& set,FXint pos) const {
  return find_last_of(set.text(),set.length(),pos);
  }


// Find last character, starting from pos; return position or -1
FXint FXString::find_last_of(FXchar c,FXint pos) const {
  register FXint len=length();
  register FXint p=pos;
  register FXint cc=c;
  if(p>=len) p=len-1;
  while(0<=p){ if(str[p]==cc){ return p; } p--; }
  return -1;
  }



// Find first character NOT in the set of size n, starting from pos; return position or -1
FXint FXString::find_first_not_of(const FXchar* set,FXint n,FXint pos) const {
  register FXint len=length();
  register FXint p=pos;
  if(p<0) p=0;
  while(p<len){
    register FXint c=str[p];
    register FXint i=n;
    while(--i>=0){ if(set[i]==c) goto x; }
    return p;
x:  p++;
    }
  return -1;
  }


// Find first character NOT in the set, starting from pos; return position or -1
FXint FXString::find_first_not_of(const FXchar* set,FXint pos) const {
  return find_first_not_of(set,strlen(set),pos);
  }


// Find first character NOT in the set, starting from pos; return position or -1
FXint FXString::find_first_not_of(const FXString& set,FXint pos) const {
  return find_first_not_of(set.text(),set.length(),pos);
  }


// Find first character NOT equal to c, starting from pos; return position or -1
FXint FXString::find_first_not_of(FXchar c,FXint pos) const {
  register FXint len=length();
  register FXint p=pos;
  register FXint cc=c;
  if(p<0) p=0;
  while(p<len){ if(str[p]!=cc){ return p; } p++; }
  return -1;
  }



// Find last character NOT in the set of size n, starting from pos; return position or -1
FXint FXString::find_last_not_of(const FXchar* set,FXint n,FXint pos) const {
  register FXint len=length();
  register FXint p=pos;
  if(p>=len) p=len-1;
  while(0<=p){
    register FXint c=str[p];
    register FXint i=n;
    while(--i>=0){ if(set[i]==c) goto x; }
    return p;
x:  p--;
    }
  return -1;
  }

// Find last character NOT in the set, starting from pos; return position or -1
FXint FXString::find_last_not_of(const FXchar* set,FXint pos) const {
  return find_last_not_of(set,strlen(set),pos);
  }

// Find last character NOT in the set, starting from pos; return position or -1
FXint FXString::find_last_not_of(const FXString& set,FXint pos) const {
  return find_last_not_of(set.text(),set.length(),pos);
  }


// Find last character NOT equal to c, starting from pos; return position or -1
FXint FXString::find_last_not_of(FXchar c,FXint pos) const {
  register FXint len=length();
  register FXint p=pos;
  register FXint cc=c;
  if(p>=len) p=len-1;
  while(0<=p){ if(str[p]!=cc){ return p; } p--; }
  return -1;
  }


// Get hash value
FXuint FXString::hash() const {
  register FXint len=length();
  register FXuint h=0;
  for(register FXint i=0; i<len; i++){  // This should be a very good hash function:- just 4 collisions
    h = ((h << 5) + h) ^ str[i];        // on the webster web2 dictionary of 234936 words, and no
    }                                   // collisions at all on the standard dict!
  return h;
  }


// Save
FXStream& operator<<(FXStream& store,const FXString& s){        // Note stream format incompatible with FOX 1.0
  FXint len=s.length();
  store << len;
  store.save(s.str,len);
  return store;
  }


// Load
FXStream& operator>>(FXStream& store,FXString& s){              // Note stream format incompatible with FOX 1.0
  FXint len;
  store >> len;
  s.length(len);
  store.load(s.str,len);
  return store;
  }


// Print formatted string a-la vprintf
FXString& FXString::vformat(const FXchar* fmt,va_list args){
  register FXint len=0;
  if(fmt && *fmt){
    register FXint n=strlen(fmt);       // Result is longer than format string
#if defined(WIN32) || defined(HAVE_VSNPRINTF)
//    va_list a;
//    n+=128;                             // Add a bit of slop
//x:  length(n);
//    va_copy(a,args);
//    len=vsnprintf(str,n+1,fmt,a);
//    if(len<0){ n<<=1; goto x; }         // Some implementations return -1 if not enough room
//    if(n<len){ n=len; goto x; }         // Others return how much space would be needed
    n+=1024;                            // Add a lot of slop
    length(n);                          // Some implementations return -1 if not enough room
    len=vsnprintf(str,n+1,fmt,args);    // Others return how much space would be needed
#else
    n+=1024;                            // Add a lot of slop
    length(n);
    len=vsprintf(str,fmt,args);
#endif
    FXASSERT(0<=len && len<=n);
    }
  length(len);
  return *this;
  }


// Print formatted string a-la printf
FXString& FXString::format(const FXchar* fmt,...){
  va_list args;
  va_start(args,fmt);
  vformat(fmt,args);
  va_end(args);
  return *this;
  }


// Furnish our own version if we have to
#ifndef HAVE_VSSCANF
extern "C" int vsscanf(const char* str, const char* format, va_list arg_ptr);
#endif


// Scan
FXint FXString::vscan(const FXchar* fmt,va_list args) const {
  return vsscanf((char*)str,fmt,args);          // Cast needed for HP-UX 11, which has wrong prototype for vsscanf
  }


FXint FXString::scan(const FXchar* fmt,...) const {
  FXint result;
  va_list args;
  va_start(args,fmt);
  result=vscan(fmt,args);
  va_end(args);
  return result;
  }


// Format a string a-la vprintf
FXString FXStringVFormat(const FXchar* fmt,va_list args){
  FXString result;
  result.vformat(fmt,args);
  return result;
  }


// Format a string a-la printf
FXString FXStringFormat(const FXchar* fmt,...){
  FXString result;
  va_list args;
  va_start(args,fmt);
  result.vformat(fmt,args);
  va_end(args);
  return result;
  }


// Conversion of integer to string
FXString FXStringVal(FXlong num,FXint base){
  FXchar buf[66];
  register FXchar *p=buf+66;
  register FXulong nn=(FXulong)num;
  if(base<2 || base>16){ fxerror("FXStringVal: base out of range.\n"); }
  if(num<0){nn=(FXulong)(~num)+1;}
  do{
    *--p=FXString::HEX[nn%base];
    nn/=base;
    }
  while(nn);
  if(num<0) *--p='-';
  FXASSERT(buf<=p);
  return FXString(p,buf+66-p);
  }


// Conversion of unsigned long to string
FXString FXStringVal(FXulong num,FXint base){
  FXchar buf[66];
  register FXchar *p=buf+66;
  register FXulong nn=num;
  if(base<2 || base>16){ fxerror("FXStringVal: base out of range.\n"); }
  do{
    *--p=FXString::HEX[nn%base];
    nn/=base;
    }
  while(nn);
  FXASSERT(buf<=p);
  return FXString(p,buf+66-p);
  }


// Conversion of integer to string
FXString FXStringVal(FXint num,FXint base){
  FXchar buf[34];
  register FXchar *p=buf+34;
  register FXuint nn=(FXuint)num;
  if(base<2 || base>16){ fxerror("FXStringVal: base out of range.\n"); }
  if(num<0){nn=(FXuint)(~num)+1;}
  do{
    *--p=FXString::HEX[nn%base];
    nn/=base;
    }
  while(nn);
  if(num<0) *--p='-';
  FXASSERT(buf<=p);
  return FXString(p,buf+34-p);
  }


// Conversion of unsigned integer to string
FXString FXStringVal(FXuint num,FXint base){
  FXchar buf[34];
  register FXchar *p=buf+34;
  register FXuint nn=num;
  if(base<2 || base>16){ fxerror("FXStringVal: base out of range.\n"); }
  do{
    *--p=FXString::HEX[nn%base];
    nn/=base;
    }
  while(nn);
  FXASSERT(buf<=p);
  return FXString(p,buf+34-p);
  }


// Formatting for reals
static const char *const expo[]={"%.*f","%.*E","%.*G"};


// Conversion of float to string
FXString FXStringVal(FXfloat num,FXint prec,FXint exp){
  return FXStringFormat(expo[exp],prec,num);
  }


// Conversion of double to string
FXString FXStringVal(FXdouble num,FXint prec,FXint exp){
  return FXStringFormat(expo[exp],prec,num);
  }


#ifndef HAVE_STRTOLL
extern "C" FXlong strtoll(const char *nptr, char **endptr, int base);
#endif


#ifndef HAVE_STRTOULL
extern "C" FXulong strtoull(const char *nptr, char **endptr, int base);
#endif


// Conversion of string to integer
FXlong FXLongVal(const FXString& s,FXint base){
  return (FXlong)strtoll(s.str,NULL,base);
  }

// Conversion of string to unsigned integer
FXulong FXULongVal(const FXString& s,FXint base){
  return (FXulong)strtoull(s.str,NULL,base);
  }

// Conversion of string to integer
FXint FXIntVal(const FXString& s,FXint base){
  return (FXint)strtol(s.str,NULL,base);
  }


// Conversion of string to unsigned integer
FXuint FXUIntVal(const FXString& s,FXint base){
  return (FXuint)strtoul(s.str,NULL,base);
  }


// Conversion of string to float
FXfloat FXFloatVal(const FXString& s){
  return (FXfloat)strtod(s.str,NULL);
  }


// Conversion of string to double
FXdouble FXDoubleVal(const FXString& s){
  return strtod(s.str,NULL);
  }


// Return utf8 from ascii containing unicode escapes
FXString fromAscii(const FXString& s){
  register FXint p=0;
  FXString result;
  FXwchar c;
  while(p<s.length()){
    c=s[p++];
    if(c=='\\' && p<s.length()){
      c=s[p++];
      if(c=='u'){
        if(Ascii::isHexDigit(s[p])){
          c=Ascii::digitValue(s[p++]);
          if(Ascii::isHexDigit(s[p])){
            c=(c<<4)+Ascii::digitValue(s[p++]);
            if(Ascii::isHexDigit(s[p])){
              c=(c<<4)+Ascii::digitValue(s[p++]);
              if(Ascii::isHexDigit(s[p])){
                c=(c<<4)+Ascii::digitValue(s[p++]);
                }
              }
            }
          }
        result.append(&c,1);
        continue;
        }
      }
    result.append(c);
    }
  return result;
  }


// Return ascii containing unicode escapes from utf8
FXString toAscii(const FXString& s){
  register FXint p=0;
  FXString result;
  FXwchar c;
  while(p<s.length()){
    c=s.wc(p);
    if(0x80<=c){
      result.append("\\u");
      result.append(FXString::HEX[(c>>12)&15]);
      result.append(FXString::HEX[(c>>8)&15]);
      result.append(FXString::HEX[(c>>4)&15]);
      c=FXString::HEX[c&15];
      }
    result.append(c);
    p+=s.extent(p);
    }
  return result;
  }


// Escape special characters in a string
FXString escape(const FXString& s){
  register FXint p=0;
  register FXint c;
  FXString result;
  while(p<s.length()){
    c=s[p++];
    switch(c){
      case '\n':
        result.append("\\n");
        break;
      case '\r':
        result.append("\\r");
        break;
      case '\b':
        result.append("\\b");
        break;
      case '\v':
        result.append("\\v");
        break;
      case '\a':
        result.append("\\a");
        break;
      case '\f':
        result.append("\\f");
        break;
      case '\t':
        result.append("\\t");
        break;
      case '\\':
        result.append("\\\\");
        break;
      case '"':
        result.append("\\\"");
        break;
      case '\'':
        result.append("\\\'");
        break;
      default:
        if(0x20<=c && c<=0x7F){
          result.append(c);
          }
        else{
          result.append("\\x");
          result.append(FXString::HEX[(c>>4)&15]);
          result.append(FXString::HEX[c&15]);
          }
        break;
      }
    }
  return result;
  }


// Unescape special characters in a string
FXString unescape(const FXString& s){
  register FXint p=0;
  register FXint c;
  FXString result;
  while(p<s.length()){
    c=s[p++];
    if(c=='\\' && p<s.length()){
      c=s[p++];
      switch(c){
        case 'n':
          result.append('\n');
          break;
        case 'r':
          result.append('\r');
          break;
        case 'b':
          result.append('\b');
          break;
        case 'v':
          result.append('\v');
          break;
        case 'a':
          result.append('\a');
          break;
        case 'f':
          result.append('\f');
          break;
        case 't':
          result.append('\t');
          break;
        case '\\':
          result.append('\\');
          break;
        case '"':
          result.append('\"');
          break;
        case '\'':
          result.append('\'');
          break;
        case 'x':               // Hex escape
          if(Ascii::isHexDigit(s[p])){
            c=Ascii::digitValue(s[p++]);
            if(Ascii::isHexDigit(s[p])){
              c=(c<<4)+Ascii::digitValue(s[p++]);
              }
            }
          result.append(c);
          break;
        case '0':               // Octal escape
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
          c=c-'0';
          if('0'<=s[p] && s[p]<='7'){
            c=(c<<3)+s[p++]-'0';
            if('0'<=s[p] && s[p]<='7'){
              c=(c<<3)+s[p++]-'0';
              }
            }
          result.append(c);
          break;
        default:
          result.append(c);
          break;
        }
      continue;
      }
    result.append(c);
    }
  return result;
  }


// Hangul decomposition
enum {
  SBase  = 0xAC00,
  LBase  = 0x1100,
  VBase  = 0x1161,
  TBase  = 0x11A7,
  LCount = 19,
  VCount = 21,
  TCount = 28,
  NCount = VCount*TCount,
  SCount = LCount*NCount
  };


// Decompose hangul method, if it is hangul (from TR# 15)
static FXint decomposehangul(FXwchar *result,FXwchar w){
  register FXwchar SIndex=w-SBase;
  register FXwchar L,V,T;
  if(0<=SIndex && SIndex<SCount){
    L=LBase+SIndex/NCount;
    V=VBase+(SIndex%NCount)/TCount;
    T=TBase+SIndex%TCount;
    result[0]=L;
    result[1]=V;
    if(T!=TBase){
      result[2]=T;
      return 3;
      }
    return 2;
    }
  result[0]=w;
  return 1;
  }


// Compose hangul in situ; return new length (from TR# 15)
static FXint composehangul(FXwchar *result,FXint len){
  register FXwchar w,last,LIndex,VIndex,SIndex,TIndex;
  register FXint p,q;
  if(0<len){
    last=result[0];
    for(p=q=1; q<len; q++){
      w=result[q];

      // Check to see if two current characters are L and V
      LIndex=last-LBase;
      if(0<=LIndex && LIndex<LCount){

        // Make syllable of form LV
        VIndex=w-VBase;
        if(0<=VIndex && VIndex<VCount){
          last=SBase+(LIndex*VCount+VIndex)*TCount;
          result[p-1]=last;
          continue;
          }
        }

      // Check to see if two current characters are LV and T
      SIndex=last-SBase;
      if(0<=SIndex && SIndex<SCount && (SIndex%TCount)==0){

        // Make syllable of form LVT
        TIndex=w-TBase;
        if(0<TIndex && TIndex<TCount){
          last+=TIndex;
          result[p-1]=last;
          continue;
          }
        }

      // Otherwise just add the character
      last=w;
      result[p++]=w;
      }
    return p;
    }
  return 0;
  }


// Recursive decomposition of type kind
static FXint decomposerecursive(FXwchar *result,FXwchar w,FXuint kind){
  register const FXwchar* decomposition=Unicode::charDecompose(w);
  if((FXuint)decomposition[-2]>=kind){
    register FXint p=0;
    register FXint n=0;
    while(p<decomposition[-1]){
      n+=decomposerecursive(result+n,decomposition[p++],kind);
      }
    return n;
    }
  return decomposehangul(result,w);
  }


// Canonicalize wide character string s, by rearranging combining marks
static FXwchar *normalize(FXwchar* result,FXint len){
  register FXwchar uf,us,cf,cs;
  register FXint p=0;
  while(p+1<len){

    // Second character is a starter; advance by 2
    us=result[p+1];
    FXASSERT(us<0x110000);
    cs=Unicode::charCombining(us);
    if(cs==0){
      p+=2;
      continue;
      }

    // First character class greater; swap and back off by 1
    uf=result[p];
    FXASSERT(uf<0x110000);
    cf=Unicode::charCombining(uf);
    if(cf>cs){
      result[p]=us;
      result[p+1]=uf;
      if(p>0) p--;
      continue;
      }

    // Already in right order; advance by one
    p++;
    }
  return result;
  }


// Compose characters from canonical/compatible decomposition
static FXint compose(FXwchar* result,FXint len){
  register FXint p,q,cc,starterpos,startercc;
  register FXwchar w;
  if(0<len){
    starterpos=0;
    startercc=0;
    for(q=0; q<len; q++){
      cc=Unicode::charCombining(result[q]);
      if(0<q && (startercc==0 || startercc<cc) && (w=Unicode::charCompose(result[starterpos],result[q]))!=0){
        result[starterpos]=w;
        for(p=q+1; p<len; p++) result[p-1]=result[p];
        len--;
        q--;
        if(q==starterpos)
          startercc=0;
        else
          startercc=Unicode::charCombining(result[q-1]);

        continue;
        }
      if(cc==0) starterpos=q;
      startercc=cc;
      }
    }
  return len;
  }



// Return normalized string
FXString normalize(const FXString& s){
  FXwchar* wcs=(FXwchar*)malloc(s.length()*sizeof(FXwchar));
  FXString result;
  if(wcs){
    FXint n=utf2wcs(wcs,s.text(),s.length());
    normalize(wcs,n);
    result.assign(wcs,n);
    free(wcs);
    }
  return result;
  }


// Return decomposition of string, as utf8; this depends on knowing
// the length of the worst recursive decomposition (18).  If unicode
// tables change, make sure this code is updated.   We have an assert
// just in case.
FXString decompose(const FXString& s,FXuint kind){
  FXwchar* wcs=(FXwchar*)malloc(s.length()*sizeof(FXwchar)*18);
  FXString result;
  if(wcs){
    FXwchar* ptr=wcs+s.length()*17;
    FXint m=utf2wcs(ptr,s.text(),s.length());
    FXint p=0;
    FXint n=0;
    while(p<m){
      n+=decomposerecursive(&wcs[n],ptr[p++],kind);
      }
    FXASSERT(n<=s.length()*18);
    normalize(wcs,n);
    result.assign(wcs,n);
    free(wcs);
    }
  return result;
  }


// Return normalized composition of string, as utf8
FXString compose(const FXString& s,FXuint kind){
  FXwchar* wcs=(FXwchar*)malloc(s.length()*sizeof(FXwchar)*18);
  FXString result;
  if(wcs){
    FXwchar* ptr=wcs+s.length()*17;
    FXint m=utf2wcs(ptr,s.text(),s.length());
    FXint p=0;
    FXint n=0;
    while(p<m){
      n+=decomposerecursive(&wcs[n],ptr[p++],kind);
      }
    FXASSERT(n<=s.length()*18);
    normalize(wcs,n);
    n=compose(wcs,n);
    result.assign(wcs,n);
    free(wcs);
    }
  return result;
  }


enum {
  S_N = 0x0,    // Normal
  S_I = 0x4,    // comparing integral part
  S_F = 0x8,    // comparing fractional parts
  S_Z = 0xC     // idem but with leading Zeroes only
  };

enum {
  CMP = 2,      // return diff
  LEN = 3       // compare using len_diff/diff
  };


// Compare S1 and S2 as strings holding indices/version numbers,
// returning less than, equal to or greater than zero if S1 is less than,
// equal to or greater than S2 (for more info, see the texinfo doc).
FXint compareversion(const FXchar *s1,const FXchar *s2){
  register const FXuchar *p1=(const FXuchar*)s1;
  register const FXuchar *p2=(const FXuchar*)s2;
  register FXuchar c1,c2;
  register FXint state;
  register FXint diff;

  /* Symbol(s)    0       [1-9]   others  (padding)
     Transition   (10) 0  (01) d  (00) x  (11) -   */
  static const unsigned int next_state[]={
    /* state    x    d    0    - */
    /* S_N */  S_N, S_I, S_Z, S_N,
    /* S_I */  S_N, S_I, S_I, S_I,
    /* S_F */  S_N, S_F, S_F, S_F,
    /* S_Z */  S_N, S_F, S_Z, S_Z
    };

  static const int result_type[]={
    /* state   x/x  x/d  x/0  x/-  d/x  d/d  d/0  d/-  0/x  0/d  0/0  0/-  -/x  -/d  -/0  -/- */
    /* S_N */  CMP, CMP, CMP, CMP, CMP, LEN, CMP, CMP, CMP, CMP, CMP, CMP, CMP, CMP, CMP, CMP,
    /* S_I */  CMP,  -1,  -1, CMP,  +1, LEN, LEN, CMP,  +1, LEN, LEN, CMP, CMP, CMP, CMP, CMP,
    /* S_F */  CMP, CMP, CMP, CMP, CMP, LEN, CMP, CMP, CMP, CMP, CMP, CMP, CMP, CMP, CMP, CMP,
    /* S_Z */  CMP,  +1,  +1, CMP,  -1, CMP, CMP, CMP,  -1, CMP, CMP, CMP
    };

  if(p1==p2) return 0;

  c1 = *p1++;
  c2 = *p2++;

  // Hint: '0' is a digit too.
  state=S_N | ((c1=='0')+(Ascii::isDigit(c1)!=0));
  while((diff=c1-c2)==0 && c1!='\0'){
    state=next_state[state];
    c1=*p1++;
    c2=*p2++;
    state|=(c1=='0')+(Ascii::isDigit(c1)!=0);
    }
  state=result_type[state<<2 | (((c2=='0')+(Ascii::isDigit(c2)!=0)))];
  switch(state){
    case LEN:
      while(Ascii::isDigit(*p1++)){
	if(!Ascii::isDigit(*p2++)) return 1;
        }
      if(Ascii::isDigit(*p2)) return -1;
    case CMP:
      return diff;
    }
  return state;
  }


FXint compareversion(const FXchar* s1,const FXString& s2){
  return compareversion(s1,s2.str);
  }


FXint compareversion(const FXString& s1,const FXchar* s2){
  return compareversion(s1.str,s2);
  }


FXint compareversion(const FXString& s1,const FXString& s2){
  return compareversion(s1.str,s2.str);
  }


// Convert to dos
FXString& unixToDos(FXString& str){
  register FXint f=0;
  register FXint t=0;
  while(f<str.length()){
    if(str[f++]=='\n') t++; t++;
    }
  str.length(t);
  while(0<f){
    if((str[--t]=str[--f])=='\n') str[--t]='\r';
    }
  return str;
  }


// Convert from dos
FXString& dosToUnix(FXString& str){
  register FXint f=0,t=0,c;
  while(f<str.length()){
    if((c=str[f++])!='\r') str[t++]=c;
    }
  str.length(t);
  return str;
  }


// Delete
FXString::~FXString(){
  if(str!=EMPTY){free(str-sizeof(FXint));}
  }

}


