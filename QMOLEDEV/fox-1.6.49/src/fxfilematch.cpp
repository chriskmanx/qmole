/********************************************************************************
*                                                                               *
*                   W i l d c a r d   M a t c h   F u n c t i o n               *
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
* $Id: fxfilematch.cpp,v 1.16.2.1 2007/09/16 04:31:13 fox Exp $                     *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxascii.h"
#include "fxkeys.h"


/*
  Notes:
  - This is "upward compatible" from the standard fnmatch function in glibc,
    in addition to the basic matching, fxfilematch can also handle alternatives.

  - Match patterns are as follows:

    ?           Matches single character.
    *           Matches zero or more characters.
    [abc]       Matches a single character, which must be a, b, or c.
    [^abc]      Matches a single character, which must be anything other than a, b, or c.
    [!abc]      Ditto.
    [a-zA-Z]    Matches single character, which must be one of a-z or A-Z.
    [^a-zA-Z]   Matches single character, which must be anything other than a-z or A-Z.
    [!a-zA-Z]   Ditto.
    pat1|pat2   Matches either pat1 or pat2.
    pat1,pat2   Ditto.
    (pat1|pat2) Matches either pat1 or pat2; patterns may be nested.
    (pat1,pat2) Ditto.

  - Examples:

    *.cpp|*.cc|*.cxx|*.C  Matches some common extensions for C++ source files.

    image.(bmp,gif,jpg)   Matches a file called image given as either bmp, gif, or jpg.

    *.[^o]                Matches any file except object files.

  - You can escape meta characters like '?', '*', '(', ')', '|', '^', '!', and ','
    with the backslash '\'.

  - Match modes:

    FILEMATCH_FILE_NAME     No wildcard can ever match "/" (or "\","/" under Windows).
    FILEMATCH_NOESCAPE      Backslashes don't quote special chars ("\" is treated as "\").
    FILEMATCH_PERIOD        Leading "." is matched only explicitly (Useful to match hidden files on Unix).
    FILEMATCH_LEADING_DIR   Ignore "/..." after a match.
    FILEMATCH_CASEFOLD      Compare without regard to case.

  - Note that under Windows, FILEMATCH_NOESCAPE must be passed
*/

// If folding case, make lower case
#define FOLD(c)          ((flags&FILEMATCH_CASEFOLD)?Ascii::toLower(c):(c))

using namespace FX;

/*******************************************************************************/

namespace FX {


// Perform match
static bool domatch(const char *pattern,const char *string,FXuint flags){
  register const char *p=pattern;
  register const char *q=string;
  register const char *s;
  register char c,cs,ce,cc,neg;
  register int level;
  while((c=*p++)!='\0'){
    switch(c){
      case '?':
        if(*q=='\0') return false;
        if((flags&FILEMATCH_FILE_NAME) && ISPATHSEP(*q)) return false;
        if((flags&FILEMATCH_PERIOD) && (*q=='.') && ((q==string) || ((flags&FILEMATCH_FILE_NAME) && ISPATHSEP(*(q-1))))) return false;
        q++;
        break;
      case '*':
        c=*p;
        while(c=='*') c=*++p;
        if((flags&FILEMATCH_PERIOD) && (*q=='.') && ((q==string) || ((flags&FILEMATCH_FILE_NAME) && ISPATHSEP(*(q-1))))) return false;
        if(c=='\0'){    // Optimize for case of trailing '*'
          if(flags&FILEMATCH_FILE_NAME){ for(s=q; *s; s++){ if(ISPATHSEP(*s)) return false; } }
          return 1;
          }
        while(!domatch(p,q,flags&~FILEMATCH_PERIOD)){
          if((flags&FILEMATCH_FILE_NAME) && ISPATHSEP(*q)) return false;
          if(*q++=='\0') return false;
          }
        return 1;
      case '[':
        if(*q=='\0') return false;
        if((flags&FILEMATCH_PERIOD) && (*q=='.') && ((q==string) || ((flags&FILEMATCH_FILE_NAME) && ISPATHSEP(*(q-1))))) return false;
        cc=FOLD(*q);
        neg=((*p=='!') || (*p=='^'));
        if(neg) p++;
        c=*p++;
        do{
          if(c=='\\' && !(flags&FILEMATCH_NOESCAPE)) c=*p++;
          cs=ce=FOLD(c);
          if(c=='\0') return false;
          c=*p++;
          c=FOLD(c);
          if((flags&FILEMATCH_FILE_NAME) && ISPATHSEP(c)) return false;
          if(c=='-' && *p!=']'){
            c = *p++;
            if(c=='\\' && !(flags&FILEMATCH_NOESCAPE)) c=*p++;
            if(c=='\0') return false;
            ce=FOLD(c);
            c=*p++;
            }
          if(((FXuchar)cs)<=((FXuchar)cc) && ((FXuchar)cc)<=((FXuchar)ce)) goto match;
          }
        while(c!=']');
        if(!neg) return false;
        q++;
        break;
match:  while(c!=']'){
          if(c=='\0') return false;
          c=*p++;
          if(c=='\\' && !(flags&FILEMATCH_NOESCAPE)) p++;
          }
        if(neg) return false;
        q++;
        break;
      case '(':
nxt:    if(domatch(p,q,flags)) return true;
        for(level=0; *p && 0<=level; ){
          switch(*p++){
            case '\\': if(*p) p++; break;
            case '(': level++; break;
            case ')': level--; break;
            case '|':
            case ',': if (level==0) goto nxt;
            }
          }
        return false;
      case ')':
        break;
      case '|':
      case ',':
        for(level=0; *p && 0<=level; ){
          switch(*p++){
            case '\\': if(*p) p++; break;
            case '(': level++; break;
            case ')': level--; break;
            }
          }
        break;
      case '\\':
        if(*p && !(flags&FILEMATCH_NOESCAPE)) c=*p++;   // Trailing escape represents itself
      default:
        if(FOLD(c)!=FOLD(*q)) return false;
        q++;
        break;
      }
    }
  return (*q=='\0') || (ISPATHSEP(*q) && (flags&FILEMATCH_LEADING_DIR));
  }


// Public API to matcher
bool fxfilematch(const char *pattern,const char *string,FXuint flags){
  register const char *p=pattern;
  register const char *q=string;
  register int level;
  if(p && q){
nxt:if(domatch(p,q,flags)) return true;
    for(level=0; *p && 0<=level; ){
      switch(*p++){
        case '\\': if(*p) p++; break;
        case '(': level++; break;
        case ')': level--; break;
        case '|':
        case ',': if (level==0) goto nxt;
        }
      }
    }
  return false;
  }

}
