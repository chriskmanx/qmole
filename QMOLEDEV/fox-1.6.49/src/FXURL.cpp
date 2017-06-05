/********************************************************************************
*                                                                               *
*                       U R L   M a n i p u l a t i o n                         *
*                                                                               *
*********************************************************************************
* Copyright (C) 2000,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXURL.cpp,v 1.32.2.1 2008/02/19 22:13:21 fox Exp $                           *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxpriv.h"
#include "fxascii.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXPath.h"
#include "FXURL.h"



/*
  Notes:

  - Functions contributed by Sean Hubbell and Sander Jansen.

  - The result of gethostname should be fed into gethostbyname() to obtain
    the official host name.

  - About drive letters in URL's, Daniel Gehriger has some some
    empirical tests, and determined the following:

     NS = works on Netscape
     IE = works on IE
     O  = works on Opera

     - file:///C|/TEMP/                    NS, IE, O
     - file:///C:/TEMP/                    NS, IE, O

     - file://localhost/C:/TEMP/           NS, IE, O
     - file://localhost/C|/TEMP/           NS, IE, O

     - file://C:/TEMP/                     NS, IE, --
     - file:///C/TEMP/                     --, --, --

    The conclusion seems to be we should probably try to handle all
    of these possibilities, although keeping the `:' seems favorable.

  - For now we don't encode any reserved characters. They need to be encoded
    if they're not part of the scheme. I don't have a way of figuring
    out if they're part of a scheme or not.

*/

using namespace FX;

/*******************************************************************************/

namespace FX {


// Return host name
FXString FXURL::hostname(){
  FXchar name[512];
  if(gethostname(name,sizeof(name))==0){
    return name;
    }
  return "localhost";
  }


// Return URL of filename
FXString FXURL::fileToURL(const FXString& file){
#ifndef WIN32
  return "file:"+file;        // UNIX is easy
#else
  FXString absfile=FXPath::absolute(file).substitute(PATHSEP,'/');
  if(Ascii::isLetter(absfile[0]) && absfile[1]==':') return "file://"+FXURL::hostname()+"/"+absfile;     // Drive letter
  return "file://"+FXURL::hostname()+absfile;
  //if(isalpha(absfile[0]) && absfile[1]==':') return "file:///"+absfile;     // Drive letter
  //return "file://"+absfile;
#endif
  }


// Return filename from URL, empty if url is not a local file
FXString FXURL::fileFromURL(const FXString& url){
#ifndef WIN32
  FXint t;
  if(comparecase("file:",url,5)==0){
    if(url[5]==PATHSEP && url[6]==PATHSEP){
      t=url.find(PATHSEP,7);
      if(7<t) return url.mid(t,2000);       // We ignore host designation
      return url.mid(7,2000);               // Empty hostname part
      }
    return url.mid(5,2000);                 // No hostname
    }
  return FXString::null;
#else
  FXString localurl=url;
  localurl.substitute('/',PATHSEP);
  if(comparecase("file:" PATHSEPSTRING PATHSEPSTRING,localurl,7)==0){
    FXString result;
    FXint path=localurl.find(PATHSEP,7);
    if(path<0 || path==7){
      result=localurl.mid(7,2000);
      }
    else{
      FXString host=localurl.mid(7,path-7);
      if(host=="localhost" || host==FXURL::hostname()){
        result=localurl.mid(path,2000);
        }
      }
    if(result[0]==PATHSEP && Ascii::isLetter(result[1]) && (result[2]==':' || result[2]=='|')){
      result.erase(0,1);
      if(result[1]=='|') result[1]=':';
      }
    return result;
    }
  return FXString::null;
#endif
  }


// Decode url string
FXString FXURL::decode(const FXString& url){
  register FXint p=0;
  register FXint c;
  FXString result;
  while(p<url.length()){
    c=url[p++];
    if(c=='%'){
      if(Ascii::isHexDigit(url[p])){
        c=Ascii::digitValue(url[p++]);
        if(Ascii::isHexDigit(url[p])){
          c=(c<<4)+Ascii::digitValue(url[p++]);
          }
        }
      }
    result.append(c);
    }
  return result;
  }


#define URL_UNSAFE   "$-_.+!*'(),"          // Always Encode
#define URL_RESERVED ";/?:@=&"              // Only encode if not used as reserved by scheme


// Encode url string
FXString FXURL::encode(const FXString& url){
  register FXint p=0;
  register FXint c;
  FXString result;
  while(p<url.length()){
    c=url[p++];
    if(!Ascii::isAlphaNumeric(c) && (c<=' ' || c>='{') && strchr(URL_UNSAFE URL_RESERVED,c)){
      result.append('%');
      result.append(FXString::HEX[(c>>4)&15]);
      result.append(FXString::HEX[c&15]);
      continue;
      }
    result.append(c);
    }
  return result;
  }

}
