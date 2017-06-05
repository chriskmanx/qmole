/********************************************************************************
*                                                                               *
*                  P a t h   N a m e   M a n i p u l a t i o n                  *
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
* $Id: FXPath.cpp,v 1.20.2.5 2008/03/10 22:06:33 fox Exp $                          *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxascii.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXPath.h"
#include "FXSystem.h"
#include "FXStat.h"
#include "FXFile.h"
#include "FXDir.h"
#ifdef WIN32
#include <shellapi.h>
#endif



/*
  Notes:

  - These functions manipulate pathnames.
  - Windows 95 and NT:
      -  1 to 255 character name.
      -  Complete path for a file or project name cannot exceed 259
         characters, including the separators.
      -  May not begin or end with a space.
      -  May not begin with a $
      -  May contain 1 or more file extensions (eg. MyFile.Ext1.Ext2.Ext3.Txt).
      -  Legal characters in the range of 32 - 255 but not ?"/\<>*|:
      -  Filenames may be mixed case.
      -  Filename comparisons are case insensitive (eg. ThIs.TXT = this.txt).
  - MS-DOS and Windows 3.1:
      -  1 to 11 characters in the 8.3 naming convention.
      -  Legal characters are A-Z, 0-9, Double Byte Character Set (DBCS)
         characters (128 - 255), and _^$~!#%&-{}@'()
      -  May not contain spaces, 0 - 31, and "/\[]:;|=,
      -  Must not begin with $
      -  Uppercase only filename.
  - Perhaps use GetEnvironmentVariable instead of getenv?
  - Perhaps also taking into account certain environment variables in the
    contraction function?
  - Deal with Windows paths "\\?\" long pathname convention.
*/


using namespace FX;

/*******************************************************************************/

namespace FX {



// Return root of given path; this is just "/" or "" if not absolute
FXString FXPath::root(const FXString& file){
#ifndef WIN32
  if(ISPATHSEP(file[0])){
    return PATHSEPSTRING;
    }
#else
  if(ISPATHSEP(file[0])){
    if(ISPATHSEP(file[1])) return PATHSEPSTRING PATHSEPSTRING;   // UNC
    return FXSystem::getCurrentDrive()+PATHSEPSTRING;
    }
  if(Ascii::isLetter(file[0]) && file[1]==':'){
    if(ISPATHSEP(file[2])) return file.left(3);
    return file.left(2)+PATHSEPSTRING;
    }
#endif
  return FXString::null;
  }



// Return directory part of pathname, assuming full pathname.
// Note that directory("/bla/bla/") is "/bla/bla" and NOT "/bla".
// However, directory("/bla/bla") is "/bla" as we expect!
FXString FXPath::directory(const FXString& file){
  if(!file.empty()){
    FXString result=file;
    FXint p=0,q=0,s;
#ifdef WIN32
    if(ISPATHSEP(result[q])){         // UNC
      result[p++]=PATHSEP; q++;
      if(ISPATHSEP(result[q])){
        result[p++]=PATHSEP;
        while(ISPATHSEP(result[q])) q++;
        }
      }
    else if(Ascii::isLetter(result[q]) && result[q+1]==':'){
      result[p++]=result[q++]; result[p++]=':'; q++;
      if(ISPATHSEP(result[q])){
        result[p++]=PATHSEP;
        while(ISPATHSEP(result[q])) q++;
        }
      }
#else
    if(ISPATHSEP(result[q])){
      result[p++]=PATHSEP;
      while(ISPATHSEP(result[q])) q++;
      }
#endif
    s=p;
    while(result[q]){
      if(ISPATHSEP(result[q])){
        result[s=p++]=PATHSEP;
        while(ISPATHSEP(result[q])) q++;
        continue;
        }
      result[p++]=result[q++];
      }
    return result.trunc(s);
    }
  return FXString::null;
  }


// Return name and extension part of pathname.
// Note that name("/bla/bla/") is "" and NOT "bla".
// However, name("/bla/bla") is "bla" as we expect!
FXString FXPath::name(const FXString& file){
  register FXint f,n;
  if(!file.empty()){
    n=0;
#ifdef WIN32
    if(Ascii::isLetter(file[0]) && file[1]==':') n=2;
#endif
    f=n;
    while(file[n]){
      if(ISPATHSEP(file[n])) f=n+1;
      n++;
      }
    return FXString(file.text()+f,n-f);
    }
  return FXString::null;
  }


// Return file title, i.e. document name only:
//
//  /path/aa        -> aa
//  /path/aa.bb     -> aa
//  /path/aa.bb.cc  -> aa.bb
//  /path/.aa       -> .aa
FXString FXPath::title(const FXString& file){
  register FXint f,e,b,i;
  if(!file.empty()){
    i=0;
#ifdef WIN32
    if(Ascii::isLetter(file[0]) && file[1]==':') i=2;
#endif
    f=i;
    while(file[i]){
      if(ISPATHSEP(file[i])) f=i+1;
      i++;
      }
    b=f;
    if(file[b]=='.') b++;     // Leading '.'
    e=i;
    while(b<i){
      if(file[--i]=='.'){ e=i; break; }
      }
    return FXString(file.text()+f,e-f);
    }
  return FXString::null;
  }


// Return extension, if there is one:
//
//  /path/aa        -> ""
//  /path/aa.bb     -> bb
//  /path/aa.bb.cc  -> cc
//  /path/.aa       -> ""
FXString FXPath::extension(const FXString& file){
  register FXint f,e,i,n;
  if(!file.empty()){
    n=0;
#ifdef WIN32
    if(Ascii::isLetter(file[0]) && file[1]==':') n=2;
#endif
    f=n;
    while(file[n]){
      if(ISPATHSEP(file[n])) f=n+1;
      n++;
      }
    if(file[f]=='.') f++;     // Leading '.'
    e=i=n;
    while(f<i){
      if(file[--i]=='.'){ e=i+1; break; }
      }
    return FXString(file.text()+e,n-e);
    }
  return FXString::null;
  }


// Return file name less the extension
//
//  /path/aa        -> /path/aa
//  /path/aa.bb     -> /path/aa
//  /path/aa.bb.cc  -> /path/aa.bb
//  /path/.aa       -> /path/.aa
FXString FXPath::stripExtension(const FXString& file){
  if(!file.empty()){
    FXString result=file;
    FXint p=0,q=0,s,e;
#ifdef WIN32
    if(ISPATHSEP(result[q])){         // UNC
      result[p++]=PATHSEP; q++;
      if(ISPATHSEP(result[q])){
        result[p++]=PATHSEP;
        while(ISPATHSEP(result[q])) q++;
        }
      }
    else if(Ascii::isLetter(result[q]) && result[q+1]==':'){
      result[p++]=result[q++]; result[p++]=':'; q++;
      if(ISPATHSEP(result[q])){
        result[p++]=PATHSEP;
        while(ISPATHSEP(result[q])) q++;
        }
      }
#else
    if(ISPATHSEP(result[q])){
      result[p++]=PATHSEP;
      while(ISPATHSEP(result[q])) q++;
      }
#endif
    s=p;
    while(result[q]){
      if(ISPATHSEP(result[q])){
        result[p++]=PATHSEP; s=p;
        while(ISPATHSEP(result[q])) q++;
        continue;
        }
      result[p++]=result[q++];
      }
    if(result[s]=='.') s++;     // Leading '.'
    e=p;
    while(s<p){
      if(result[--p]=='.'){ e=p; break; }
      }
    return result.trunc(e);
    }
  return FXString::null;
  }


#ifdef WIN32

// Return drive letter prefix "c:"
FXString FXPath::drive(const FXString& file){
  FXchar buffer[3];
  if(Ascii::isLetter(file[0]) && file[1]==':'){
    buffer[0]=Ascii::toLower(file[0]);
    buffer[1]=':';
    buffer[2]='\0';
    return FXString(buffer,2);
    }
  return FXString::null;
  }

#else

// Return drive letter prefix "c:"
FXString FXPath::drive(const FXString&){
  return FXString::null;
  }

#endif

// Perform tilde or environment variable expansion
FXString FXPath::expand(const FXString& file){
#ifndef WIN32
  if(!file.empty()){
    register FXint b,e,n;
    FXString result;

    // Expand leading tilde of the form ~/filename or ~user/filename
    n=0;
    if(file[n]=='~'){
      n++;
      b=n;
      while(file[n] && !ISPATHSEP(file[n])) n++;
      e=n;
      result.append(FXSystem::getUserDirectory(file.mid(b,e-b)));
      }

    // Expand environment variables of the form $HOME, ${HOME}, or $(HOME)
    while(file[n]){
      if(file[n]=='$'){
        n++;
        if(file[n]=='{' || file[n]=='(') n++;
        b=n;
        while(Ascii::isAlphaNumeric(file[n]) || file[n]=='_') n++;
        e=n;
        if(file[n]=='}' || file[n]==')') n++;
        result.append(FXSystem::getEnvironment(file.mid(b,e-b)));
        continue;
        }
      result.append(file[n]);
      n++;
      }
    return result;
    }
  return FXString::null;
#else
  if(!file.empty()){
    FXchar buffer[2048];

    // Expand environment variables of the form %HOMEPATH%
    if(ExpandEnvironmentStringsA(file.text(),buffer,sizeof(buffer))){
      return buffer;
      }
    return file;
    }
  return FXString::null;
#endif
  }


// Contract path based on environment variables
//
//      /home/jeroen/junk
//      /home/someoneelse/junk
//      /usr/local/ACE_wrappers/TAO
//
//    to:
//
//      ~/junk
//      ~someoneelse/junk
//      $ACE_ROOT/TAO
//
FXString FXPath::contract(const FXString& file,const FXString& user,const FXString& var){
  FXString result=file;
  if(!result.empty()){
    FXString dir=FXSystem::getUserDirectory(user);
    if(compare(result,dir,dir.length())==0){
      result.replace(0,dir.length(),"~"+user);
      }
    dir=FXSystem::getEnvironment(var);
    result.substitute(dir,"$"+var);
    }
  return result;
  }


// Simplify a file path; the path will remain relative if it was relative,
// or absolute if it was absolute.  Also, a trailing "/" will be preserved
// as this is important in other functions.
//
// Examples:
//
//  /aa/bb/../cc    -> /aa/cc
//  /aa/bb/../cc/   -> /aa/cc/
//  /aa/bb/../..    -> /
//  ../../bb        -> ../../bb
//  ../../bb/       -> ../../bb/
//  /../            -> /
//  ./aa/bb/../../  -> ./
//  a/..            -> .
//  a/../           -> ./
//  ./a             -> ./a
//  /////./././     -> /
//  c:/../          -> c:/
//  c:a/..          -> c:
//  /.              -> /
FXString FXPath::simplify(const FXString& file){
  if(!file.empty()){
    FXString result=file;
    register FXint p=0;
    register FXint q=0;
    register FXint s;
#ifdef WIN32
    if(ISPATHSEP(result[q])){         // UNC
      result[p++]=PATHSEP; q++;
      if(ISPATHSEP(result[q])){
        result[p++]=PATHSEP;
        while(ISPATHSEP(result[q])) q++;
        }
      }
    else if(Ascii::isLetter(result[q]) && result[q+1]==':'){
      result[p++]=result[q++]; result[p++]=':'; q++;
      if(ISPATHSEP(result[q])){
        result[p++]=PATHSEP;
        while(ISPATHSEP(result[q])) q++;
        }
      }
#else
    if(ISPATHSEP(result[q])){
      result[p++]=PATHSEP;
      while(ISPATHSEP(result[q])) q++;
      }
#endif
    s=p;
    while(result[q]){
      while(result[q] && !ISPATHSEP(result[q])){
        result[p++]=result[q++];
        }
      if(ISPATHSEP(result[q])){
        result[p++]=PATHSEP;
        while(ISPATHSEP(result[q])) q++;
        }
      if(2<=p && ISPATHSEP(result[p-2]) && result[p-1]=='.'){   // Case "xxx/."
        p--;
        if(s<p) p--;
        }
      else if(3<=p && ISPATHSEP(result[p-3]) && result[p-2]=='.' && ISPATHSEP(result[p-1])){    // Case "xxx/./"
        p-=2;
        }
      else if(3<=p && ISPATHSEP(result[p-3]) && result[p-2]=='.' && result[p-1]=='.' && !(((6<=p && ISPATHSEP(result[p-6])) || 5==p) && result[p-5]=='.' && result[p-4]=='.')){ // Case "xxx/.."
        p-=2;
        if(s<p){                // Pathological case "/.." will become "/"
          p--;
          while(s<p && !ISPATHSEP(result[p-1])) p--;
          if(s<p && ISPATHSEP(result[p-1])) p--;
          if(p==0){                             // Don't allow empty path
            result[p++]='.';
            }
          }
        }
      else if(4<=p && ISPATHSEP(result[p-4]) && result[p-3]=='.' && result[p-2]=='.' && ISPATHSEP(result[p-1]) && !(((7<=p && ISPATHSEP(result[p-7])) || 6==p) && result[p-6]=='.' && result[p-5]=='.')){       // Case "xxx/../"
        p-=3;
        if(s<p){                // Pathological case "/../" will become "/"
          p--;
          while(s<p && !ISPATHSEP(result[p-1])) p--;
          if(p==0){                             // Don't allow empty path
            result[p++]='.';
            result[p++]=PATHSEP;                // Keep trailing "/" around
            }
          }
        }
      }
    return result.trunc(p);
    }
  return FXString::null;
  }


// Build absolute pathname
FXString FXPath::absolute(const FXString& file){
  if(file.empty()) return FXSystem::getCurrentDirectory();
#ifndef WIN32
  if(ISPATHSEP(file[0])) return FXPath::simplify(file);
#else
  if(ISPATHSEP(file[0])){
    if(ISPATHSEP(file[1])) return FXPath::simplify(file);   // UNC
    return FXPath::simplify(FXSystem::getCurrentDrive()+file);
    }
  if(Ascii::isLetter(file[0]) && file[1]==':'){
    if(ISPATHSEP(file[2])) return FXPath::simplify(file);
    return FXPath::simplify(file.mid(0,2)+PATHSEPSTRING+file.mid(2,2147483647));
    }
#endif
  return FXPath::simplify(FXSystem::getCurrentDirectory()+PATHSEPSTRING+file);
  }


// Build absolute pathname from parts
FXString FXPath::absolute(const FXString& base,const FXString& file){
  if(file.empty()) return FXPath::absolute(base);
#ifndef WIN32
  if(ISPATHSEP(file[0])) return FXPath::simplify(file);
#else
  if(ISPATHSEP(file[0])){
    if(ISPATHSEP(file[1])) return FXPath::simplify(file);   // UNC
    return FXPath::simplify(FXSystem::getCurrentDrive()+file);
    }
  if(Ascii::isLetter(file[0]) && file[1]==':'){
    if(ISPATHSEP(file[2])) return FXPath::simplify(file);
    return FXPath::simplify(file.mid(0,2)+PATHSEPSTRING+file.mid(2,2147483647));
    }
#endif
  return FXPath::simplify(FXPath::absolute(base)+PATHSEPSTRING+file);
  }


#define ISSEP(c) (((c)=='\0') || ISPATHSEP(c))

// Return relative path of file to given base directory
//
// Examples:
//
//  Base       File         Result      Comment
//  /          /a/b         /a/b        Branch point is /
//  /p/q/r     /a/b/c       /a/b/c      Branch point is /
//  /a/b/c     /a/b/c/d     d           Branch point is /a/b/c
//  /a/b/c/    /a/b/c/d     d           Branch point is /a/b/c
//  /a/b/c/d   /a/b/c       ../         Branch point is /a/b/c
//  /a/b/c/d   /a/b/q       ../../q     Branch point is /a/b
//  /a/b/c     /a/b/c       .           Equal
//  /a/b/c/    /a/b/c/      .           Equal
//  ../a/b/c   ../a/b/c/d   d           Branch point is ../a/b/c
//  ./a        ./b          ../b        Branch point assumed to be ..
//  a          b            ../b        Branch point assumed to be ..
FXString FXPath::relative(const FXString& base,const FXString& file){
  if(!base.empty() && !FXPath::isTopDirectory(base)){
    register FXint p=0,q=0,bp=0,bq=0;

    // Find branch point
#ifdef WIN32
    while(base[p] && ((Ascii::toLower(base[p])==Ascii::toLower(file[q])) || (ISPATHSEP(base[p]) && ISPATHSEP(file[q])))){
      if(ISPATHSEP(base[p])){
        bp=p;
        bq=q;
        while(0<p && ISPATHSEP(base[p+1])) p++;           // Eat multiple slashes, but not the UNC "\\" at the start
        while(0<q && ISPATHSEP(file[q+1])) q++;
        }
      p++;
      q++;
      }
#else
    while(base[p] && (base[p]==file[q])){
      if(ISPATHSEP(base[p])){
        bp=p;
        bq=q;
        while(ISPATHSEP(base[p+1])) p++;                  // Eat multiple slashes
        while(ISPATHSEP(file[q+1])) q++;
        }
      p++;
      q++;
      }
#endif

    // Common prefix except for trailing path separator
    if((base[p]=='\0' || ISPATHSEP(base[p])) && (file[q]=='\0' || ISPATHSEP(file[q]))){
      bp=p;
      bq=q;
      }

    // If branch point is not root
#ifdef WIN32
    if(!((ISPATHSEP(base[0]) && (bp==0 || (ISPATHSEP(base[1]) && bp==1))) || (Ascii::isLetter(base[0]) && base[1]==':' && (bp==1 || (ISPATHSEP(base[2]) && bp==2))))){
#else
    if(!(ISPATHSEP(base[0]) && bp==0)){
#endif

      // Strip leading path character off, if any
      while(ISPATHSEP(file[bq])) bq++;

      // Non trivial
      if(file[bq]){
        FXString result;

        // Up to branch point
        while(base[bp]){
          while(ISPATHSEP(base[bp])) bp++;
          if(base[bp]){
            result.append(".." PATHSEPSTRING);
            while(base[bp] && !ISPATHSEP(base[bp])) bp++;
            }
          }

        // Append tail end
        result.append(&file[bq]);
        return result;
        }
      return ".";
      }
    }
  return file;
  }


// Return relative path of file to the current directory
FXString FXPath::relative(const FXString& file){
  return FXPath::relative(FXSystem::getCurrentDirectory(),file);
  }


// Return path following local path separator conventions
FXString FXPath::convert(const FXString& path){
  FXString result(path);
  for(FXint i=0; i<result.length(); i++){
    if(ISPATHSEP(result[i])) result[i]=PATHSEP;
    }
  return result;
  }


// Up one level, given absolute path
FXString FXPath::upLevel(const FXString& file){
  if(!file.empty()){
    FXString result=file;
    FXint p=0,q=0,s;
#ifdef WIN32
    if(ISPATHSEP(result[q])){         // UNC
      result[p++]=PATHSEP;
      if(ISPATHSEP(result[++q])){
        result[p++]=PATHSEP;
        while(ISPATHSEP(result[q])) q++;
        }
      }
    else if(Ascii::isLetter(result[q]) && result[q+1]==':'){
      result[p++]=result[q++];
      result[p++]=result[q++];
      if(ISPATHSEP(result[q])){
        result[p++]=PATHSEP;
        while(ISPATHSEP(result[q])) q++;
        }
      }
#else
    if(ISPATHSEP(result[q])){
      result[p++]=PATHSEP;
      while(ISPATHSEP(result[q])) q++;
      }
#endif
    s=p;
    while(result[q]){
      result[p++]=result[q++];
      if(ISPATHSEP(result[q-1])){
        while(ISPATHSEP(result[q])) q++;
        result[p-1]=PATHSEP;
        if(result[q]) s=p-1;
        }
      }
    return result.trunc(s);
    }
  return PATHSEPSTRING;
  }


// Check if file represents absolute pathname
bool FXPath::isAbsolute(const FXString& file){
#ifndef WIN32
  return ISPATHSEP(file[0]);
#else
  return ISPATHSEP(file[0]) || (Ascii::isLetter(file[0]) && file[1]==':');
#endif
  }


// Does file represent topmost directory
bool FXPath::isTopDirectory(const FXString& file){
#ifndef WIN32
  return ISPATHSEP(file[0]) && file[1]=='\0';
#else
  return (ISPATHSEP(file[0]) && (file[1]=='\0' || (ISPATHSEP(file[1]) && file[2]=='\0'))) || (Ascii::isLetter(file[0]) && file[1]==':' && (file[2]=='\0' || (ISPATHSEP(file[2]) && file[3]=='\0')));
#endif
  }



#ifndef WIN32

// Check if file represents a file share
bool FXPath::isShare(const FXString&){
  return false;
  }

#else

// Check if file represents a file share
bool FXPath::isShare(const FXString& file){
  return ISPATHSEP(file[0]) && ISPATHSEP(file[1]) && file.find(PATHSEP,2)<0;
  }

#endif


#ifndef WIN32                 // UNIX

// Enquote filename to make safe for shell
FXString FXPath::enquote(const FXString& file,bool force){
  FXString result(file);
  if(0<file.length()){
    register FXint p,q,e,c;
    p=q=e=0;
    while(p<file.length()){
      switch(file[p++]){
        case '\'':              // Quote needs to be escaped to ...'\''....
          q+=2;                 // Two if quote is not inside quotation
          e+=2;                 // Two more if it is
          continue;
        case '\\':              // Back slash
        case '!':               // Special in csh
        case '"':
        case '$':               // Variable substitution
        case '&':
        case '(':
        case ')':
        case ';':
        case '<':               // Redirections, pipe
        case '>':
        case '|':
        case '`':               // Command substitution
        case '^':               // Special in sh
        case '*':               // Wildcard characters
        case '+':
        case '?':
        case '[':
        case ']':
        case '\t':              // White space
        case '\n':
        case '\v':
        case ' ':
          force=true;           // Force quotes
          q++;
          continue;
        case '#':               // Comments
        case '~':               // Username substitution
          if(p==1) force=true;  // Force quotes if at beginning
        default:                // Normal character
          q++;
          continue;
        }
      }
    if(force) q+=e+2;           // Each escape adds two, quoting adds two more
    if(result.length()<q){
      result.length(q);         // Make longer if quoted
      p=q=0;
      if(force) result[q++]='\'';
      while(p<file.length()){
        if((c=file[p++])=='\''){        // Quote needs to be escaped
          if(force) result[q++]='\'';   // End quotation run first
          result[q++]='\\';
          result[q++]=c;
          if(force) result[q++]='\'';   // Start next quotation run
          continue;
          }
        result[q++]=c;
        }
      if(force) result[q++]='\'';
      }
    FXASSERT(result.length()==q);
    }
  return result;
  }


// Decode filename to get original again
FXString FXPath::dequote(const FXString& file){
  FXString result(file);
  if(0<result.length()){
    register FXint e=result.length();
    register FXint b=0;
    register FXint r=0;
    register FXint q=0;

    // Trim tail
    while(0<e && Ascii::isSpace(result[e-1])) --e;

    // Trim head
    while(b<e && Ascii::isSpace(result[b])) ++b;

    // Dequote the rest
    while(b<e){
      if(result[b]=='\''){ q=!q; b++; continue; }
      if(result[b]=='\\' && result[b+1]=='\'' && !q){ b++; }
      result[r++]=result[b++];
      }

    // Trunc to size
    result.trunc(r);
    }
  return result;
  }



#else                         // WINDOWS

// Enquote filename to make safe for shell
FXString FXPath::enquote(const FXString& file,bool force){
  FXString result(file);
  if(0<file.length()){
    register FXint p,q,c;
    p=q=0;
    while(p<file.length()){
      switch(file[p++]){
        case '^':               // Escape character
        case '"':               // Quote characters
        case '<':               // Redirection
        case '>':               // Redirection
        case '|':               // Command separators
        case '&':               // Command separators
        case ':':               // Drive letter separator
        case '*':               // Wildcard
        case '?':               // Wildcard
        case '\\':              // Path separator
        case '/':               // Alternate path separator
          q+=2;                 // Room for escape code
          continue;
        case ' ':               // White space
        case '\t':
        case '\v':
          force=true;
        default:                // Normal characters
          q++;
          continue;
        }
      }
    if(force) q+=2;             // Surround by quotes as well
    if(result.length()<q){
      result.length(q);         // Make longer if quoted
      p=q=0;
      if(force) result[q++]='\"';
      while(p<file.length()){
        switch(c=file[p++]){
          case '^':             // Escape character
          case '"':             // Quote characters
          case '<':             // Redirection
          case '>':             // Redirection
          case '|':             // Command separators
          case '&':             // Command separators
          case ':':             // Drive letter separator
          case '*':             // Wildcard
          case '?':             // Wildcard
          case '\\':            // Path separator
          case '/':             // Alternate path separator
            result[q++]='^';
            result[q++]=c;
            continue;
          case ' ':             // White space
          case '\t':
          case '\v':
          default:              // Normal characters
            result[q++]=c;
            continue;
          }
        }
      if(force) result[q++]='\"';
      }
    FXASSERT(result.length()==q);
    }
  return result;
  }


// Decode filename to get original again
FXString FXPath::dequote(const FXString& file){
  FXString result(file);
  if(0<result.length()){
    register FXint e=result.length();
    register FXint b=0;
    register FXint r=0;
    register FXint q=0;

    // Trim tail
    while(0<e && Ascii::isSpace(result[e-1])) --e;

    // Trim head
    while(b<e && Ascii::isSpace(result[b])) ++b;

    // Dequote the rest
    while(b<e){
      if(result[b]=='\"'){ q=!q; b++; continue; }
      if(result[b]=='^' && b+1<e){ b++; }
      result[r++]=result[b++];
      }

    // Trunc to size
    result.trunc(r);
    }
  return result;
  }

#endif


// Match filenames using *, ?, [^a-z], and so on
bool FXPath::match(const FXString& pattern,const FXString& file,FXuint flags){
  return fxfilematch(pattern.text(),file.text(),flags);
  }


// Generate unique filename of the form pathnameXXX.ext, where
// pathname.ext is the original input file, and XXX is a number,
// possibly empty, that makes the file unique.
// (From: Mathew Robertson <mathew.robertson@mi-services.com>)
FXString FXPath::unique(const FXString& file){
  if(!FXStat::exists(file)) return file;
  FXString ext=FXPath::extension(file);
  FXString path=FXPath::stripExtension(file);           // Use the new API (Jeroen)
  FXString filename;
  register FXint count=0;
  if(!ext.empty()) ext.prepend('.');            // Only add period when non-empty extension
  while(count<1000){
    filename.format("%s%i%s",path.text(),count,ext.text());
    if(!FXStat::exists(filename)) return filename;      // Return result here (Jeroen)
    count++;
    }
  return FXString::null;
  }


// Search pathlist for file
FXString FXPath::search(const FXString& pathlist,const FXString& file){
  if(!file.empty()){
    FXString path;
    FXint beg,end;
#ifndef WIN32
    if(ISPATHSEP(file[0])){
      if(FXStat::exists(file)) return file;
      return FXString::null;
      }
#else
    if(ISPATHSEP(file[0])){
      if(ISPATHSEP(file[1])){
        if(FXStat::exists(file)) return file;   // UNC
        return FXString::null;
        }
      path=FXSystem::getCurrentDrive()+file;
      if(FXStat::exists(path)) return path;
      return FXString::null;
      }
    if(Ascii::isLetter(file[0]) && file[1]==':'){
      if(FXStat::exists(file)) return file;
      return FXString::null;
      }
#endif
    for(beg=0; pathlist[beg]; beg=end){
      while(pathlist[beg]==PATHLISTSEP) beg++;
      for(end=beg; pathlist[end] && pathlist[end]!=PATHLISTSEP; end++);
      if(beg==end) break;
      path=FXPath::absolute(FXPath::expand(pathlist.mid(beg,end-beg)),file);
      if(FXStat::exists(path)) return path;
      }
    }
  return FXString::null;
  }


}

