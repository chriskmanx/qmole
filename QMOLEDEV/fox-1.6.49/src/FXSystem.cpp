/********************************************************************************
*                                                                               *
*         M i s c e l l a n e o u s   S y s t e m   F u n c t i o n s           *
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
* $Id: FXSystem.cpp,v 1.18.2.1 2006/08/09 01:55:08 fox Exp $                        *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxascii.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXIO.h"
#include "FXSystem.h"
#include "FXStat.h"



/*
  Notes:
  - A bric-a-brack of various functions we could not place anywhere else.
*/


#ifndef TIMEFORMAT
#define TIMEFORMAT "%m/%d/%Y %H:%M:%S"
#endif


using namespace FX;

/*******************************************************************************/

namespace FX {


// Return current time
FXTime FXSystem::now(){
  return (FXTime)::time(NULL);
  }


// Convert file time to string
FXString FXSystem::time(FXTime value){
  return FXSystem::time(TIMEFORMAT,value);
  }


// Convert file time to string as per strftime format
FXString FXSystem::time(const FXchar *format,FXTime filetime){
#ifndef WIN32
#if defined(FOX_THREAD_SAFE) && !defined(__FreeBSD__) && !defined(__OpenBSD__)
  time_t tmp=(time_t)FXMAX(filetime,0);
  struct tm tmresult;
  FXchar buffer[512];
  FXint len=strftime(buffer,sizeof(buffer),format,localtime_r(&tmp,&tmresult));
  return FXString(buffer,len);
#else
  time_t tmp=(time_t)FXMAX(filetime,0);
  FXchar buffer[512];
  FXint len=strftime(buffer,sizeof(buffer),format,localtime(&tmp));
  return FXString(buffer,len);
#endif
#else
  time_t tmp=(time_t)FXMAX(filetime,0);
  FXchar buffer[512];
  FXint len=strftime(buffer,sizeof(buffer),format,localtime(&tmp));
  return FXString(buffer,len);
#endif
  }


// Get effective user id
FXuint FXSystem::user(){
#ifndef WIN32
  return geteuid();
#else
  return 0;
#endif
  }


// Get effective group id
FXuint FXSystem::group(){
#ifndef WIN32
  return getegid();
#else
  return 0;
#endif
  }


// Return owner name from uid
FXString FXSystem::userName(FXuint uid){
  FXchar result[64];
#ifndef WIN32
#if defined(FOX_THREAD_SAFE) && !defined(__FreeBSD__) && !defined(__OpenBSD__)
  struct passwd pwdresult,*pwd;
  char buffer[1024];
  if(getpwuid_r(uid,&pwdresult,buffer,sizeof(buffer),&pwd)==0 && pwd) return pwd->pw_name;
#else
  struct passwd *pwd=getpwuid(uid);
  if(pwd) return pwd->pw_name;
#endif
#endif
  sprintf(result,"%u",uid);
  return result;
  }


// Return group name from gid
FXString FXSystem::groupName(FXuint gid){
  FXchar result[64];
#ifndef WIN32
#if defined(FOX_THREAD_SAFE) && !defined(__FreeBSD__) && !defined(__OpenBSD__)
  ::group grpresult;
  ::group *grp;
  char buffer[1024];
  if(getgrgid_r(gid,&grpresult,buffer,sizeof(buffer),&grp)==0 && grp) return grp->gr_name;
#else
  ::group *grp=getgrgid(gid);
  if(grp) return grp->gr_name;
#endif
#endif
  sprintf(result,"%u",gid);
  return result;
  }


// Get current user name
FXString FXSystem::currentUserName(){
#ifndef WIN32
#if defined(FOX_THREAD_SAFE) && !defined(__FreeBSD__) && !defined(__OpenBSD__)
  struct passwd pwdresult,*pwd;
  char buffer[1024];
  if(getpwuid_r(geteuid(),&pwdresult,buffer,sizeof(buffer),&pwd)==0 && pwd) return pwd->pw_name;
#else
  struct passwd *pwd=getpwuid(geteuid());
  if(pwd) return pwd->pw_name;
#endif
#else
  TCHAR buffer[MAXPATHLEN];
  DWORD size=MAXPATHLEN;
  if(GetUserName(buffer,&size)) return buffer;
#endif
  return FXString::null;
  }


// Get current effective group name
FXString FXSystem::currentGroupName(){
#ifndef WIN32
#if defined(FOX_THREAD_SAFE) && !defined(__FreeBSD__) && !defined(__OpenBSD__)
  ::group grpresult;
  ::group *grp;
  char buffer[1024];
  if(getgrgid_r(getegid(),&grpresult,buffer,sizeof(buffer),&grp)==0 && grp) return grp->gr_name;
#else
  ::group *grp=getgrgid(getegid());
  if(grp) return grp->gr_name;
#endif
#endif
  return FXString::null;
  }


// Return permissions string
FXString FXSystem::modeString(FXuint mode){
  FXchar result[11];
  result[0]=(mode&FXIO::SymLink) ? 'l' : (mode&FXIO::File) ? '-' : (mode&FXIO::Directory) ? 'd' : (mode&FXIO::Character) ? 'c' : (mode&FXIO::Block) ? 'b' : (mode&FXIO::Fifo) ? 'p' : (mode&FXIO::Socket) ? 's' : '?';
  result[1]=(mode&FXIO::OwnerRead) ? 'r' : '-';
  result[2]=(mode&FXIO::OwnerWrite) ? 'w' : '-';
  result[3]=(mode&FXIO::SetUser) ? 's' : (mode&FXIO::OwnerExec) ? 'x' : '-';
  result[4]=(mode&FXIO::GroupRead) ? 'r' : '-';
  result[5]=(mode&FXIO::GroupWrite) ? 'w' : '-';
  result[6]=(mode&FXIO::SetGroup) ? 's' : (mode&FXIO::GroupExec) ? 'x' : '-';
  result[7]=(mode&FXIO::OtherRead) ? 'r' : '-';
  result[8]=(mode&FXIO::OtherWrite) ? 'w' : '-';
  result[9]=(mode&FXIO::Sticky) ? 't' : (mode&FXIO::OtherExec) ? 'x' : '-';
  result[10]=0;
  return result;
  }


// Return value of environment variable name
FXString FXSystem::getEnvironment(const FXString& name){
  if(!name.empty()){
#ifndef WIN32
    return FXString(getenv(name.text()));
#else
    FXchar value[1024];
    DWORD len=GetEnvironmentVariableA(name.text(),value,1024);
    return FXString(value,len);
#endif
    }
  return FXString::null;
  }


// Change value of environment variable name
bool FXSystem::setEnvironment(const FXString& name,const FXString& value){
  if(!name.empty()){
#ifndef WIN32
#ifdef __GNU_LIBRARY__
    if(!value.empty()){
      return setenv(name.text(),value.text(),TRUE)==0;
      }
    unsetenv(name.text());
    return true;
#endif
#else
    if(!value.empty()){
      return SetEnvironmentVariableA(name.text(),value.text())!=0;
      }
    return SetEnvironmentVariableA(name.text(),NULL)!=0;
#endif
    }
  return false;
  }


// Get current working directory
FXString FXSystem::getCurrentDirectory(){
#ifndef WIN32
  FXchar buffer[MAXPATHLEN];
  if(getcwd(buffer,MAXPATHLEN)) return buffer;
#else
  TCHAR buffer[MAXPATHLEN];
  if(GetCurrentDirectory(MAXPATHLEN,buffer)) return buffer;
#endif
  return FXString::null;
  }


// Change current directory
FXbool FXSystem::setCurrentDirectory(const FXString& path){
  if(!path.empty()){
#ifdef WIN32
#ifdef UNICODE
    TCHAR buffer[MAXPATHLEN];
    utf2ncs(buffer,path.text(),path.length()+1);
    return SetCurrentDirectory(buffer);
#else
    return SetCurrentDirectory(path.text());
#endif
#else
    return chdir(path.text())==0;
#endif
    }
  return FALSE;
  }


// Get current drive prefix "a:", if any
// This is the same method as used in VC++ CRT.
FXString FXSystem::getCurrentDrive(){
#ifdef WIN32
  FXchar buffer[MAXPATHLEN];
  if(GetCurrentDirectoryA(MAXPATHLEN,buffer) && Ascii::isLetter((FXuchar)buffer[0]) && buffer[1]==':') return FXString(buffer,2);
#endif
  return FXString::null;
  }


#ifdef WIN32

// Change current drive prefix "a:"
// This is the same method as used in VC++ CRT.
FXbool FXSystem::setCurrentDrive(const FXString& prefix){
  FXchar buffer[3];
  if(!prefix.empty() && Ascii::isLetter(prefix[0]) && prefix[1]==':'){
    buffer[0]=prefix[0];
    buffer[1]=':';
    buffer[2]='\0';
    return SetCurrentDirectoryA(buffer);
    }
  return FALSE;
  }

#else

// Change current drive prefix "a:"
FXbool FXSystem::setCurrentDrive(const FXString&){
  return TRUE;
  }

#endif


// Get executable path
FXString FXSystem::getExecPath(){
  return FXString(getenv("PATH"));
  }


// Return the home directory for the current user.
FXString FXSystem::getHomeDirectory(){
  return FXSystem::getUserDirectory(FXString::null);
  }


// Get home directory for a given user
FXString FXSystem::getUserDirectory(const FXString& user){
#ifndef WIN32
#if defined(FOX_THREAD_SAFE) && !defined(__FreeBSD__) && !defined(__OpenBSD__)
  struct passwd pwdresult,*pwd;
  char buffer[1024];
  if(user.empty()){
    register const FXchar* str;
    if((str=getenv("HOME"))!=NULL) return str;
    if((str=getenv("USER"))!=NULL || (str=getenv("LOGNAME"))!=NULL){
      if(getpwnam_r(str,&pwdresult,buffer,sizeof(buffer),&pwd)==0 && pwd) return pwd->pw_dir;
      }
    if(getpwuid_r(getuid(),&pwdresult,buffer,sizeof(buffer),&pwd)==0 && pwd) return pwd->pw_dir;
    return PATHSEPSTRING;
    }
  if(getpwnam_r(user.text(),&pwdresult,buffer,sizeof(buffer),&pwd)==0 && pwd) return pwd->pw_dir;
  return PATHSEPSTRING;
#else
  register struct passwd *pwd;
  if(user.empty()){
    register const FXchar* str;
    if((str=getenv("HOME"))!=NULL) return str;
    if((str=getenv("USER"))!=NULL || (str=getenv("LOGNAME"))!=NULL){
      if((pwd=getpwnam(str))!=NULL) return pwd->pw_dir;
      }
    if((pwd=getpwuid(getuid()))!=NULL) return pwd->pw_dir;
    return PATHSEPSTRING;
    }
  if((pwd=getpwnam(user.text()))!=NULL) return pwd->pw_dir;
  return PATHSEPSTRING;
#endif
#else
  if(user.empty()){
    register const FXchar *str1,*str2;
    FXchar home[MAXPATHLEN];
    DWORD size=MAXPATHLEN;
    HKEY hKey;
    LONG result;
    if((str1=getenv("USERPROFILE"))!=NULL) return str1; // Daniël Hörchner <dbjh@gmx.net>
    if((str1=getenv("HOME"))!=NULL) return str1;
    if((str2=getenv("HOMEPATH"))!=NULL){      // This should be good for WinNT, Win2K according to MSDN
      if((str1=getenv("HOMEDRIVE"))==NULL) str1="c:";
      strncpy(home,str1,MAXPATHLEN);
      strncat(home,str2,MAXPATHLEN);
      return home;
      }
//  FXchar buffer[MAX_PATH]
//  if(SHGetFolderPath(NULL,CSIDL_PERSONAL|CSIDL_FLAG_CREATE,NULL,O,buffer)==S_OK){
//    return buffer;
//    }
    if(RegOpenKeyExA(HKEY_CURRENT_USER,"Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders",0,KEY_READ,&hKey)==ERROR_SUCCESS){
      result=RegQueryValueExA(hKey,"Personal",NULL,NULL,(LPBYTE)home,&size);  // Change "Personal" to "Desktop" if you want...
      RegCloseKey(hKey);
      if(result==ERROR_SUCCESS) return home;
      }
    return "c:" PATHSEPSTRING;
    }
  return "c:" PATHSEPSTRING;
#endif
  }


// Return temporary directory.
FXString FXSystem::getTempDirectory(){
#ifndef WIN32
  const FXchar* dir;
  if((dir=getenv("TMPDIR"))!=NULL){
    return FXString(dir);
    }
  return FXString("/tmp");
#else
  TCHAR buffer[MAXPATHLEN];
  DWORD len=GetTempPath(MAXPATHLEN,buffer);
  if(1<len && ISPATHSEP(buffer[len-1]) && !ISPATHSEP(buffer[len-2])) len--;
  return FXString(buffer,len);
#endif
  }


// Get DLL name for given base name
FXString FXSystem::dllName(const FXString& name){
#if defined(WIN32)
  return name+".dll";
#elif defined(_HPUX_) || defined(_HPUX_SOURCE)
  return "lib"+name+".sl";
#elif  defined(_APPLE_)
  return "lib"+name+".dylib";
#else
  return "lib"+name+".so";
#endif
  }


}

