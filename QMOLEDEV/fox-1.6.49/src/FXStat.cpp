/********************************************************************************
*                                                                               *
*                        F i l e   S t a t i s t i c s                          *
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
* $Id: FXStat.cpp,v 1.28 2006/01/22 17:58:42 fox Exp $                          *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxascii.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXStat.h"
#include "FXFile.h"



/*
  Notes:

*/


using namespace FX;

/*******************************************************************************/

namespace FX {


// Return time anything was changed
FXTime FXStat::touched() const {
  return FXMAX(modifyTime,createTime);
  }

// Return true if it is a hidden file (note: Windows-only attribute)
bool FXStat::isHidden() const {
  return (modeFlags&FXIO::Hidden)!=0;
  }

// Return true if it is a regular file
bool FXStat::isFile() const {
  return (modeFlags&FXIO::File)!=0;
  }

// Return true if it is a link
bool FXStat::isLink() const {
  return (modeFlags&FXIO::SymLink)!=0;
  }

// Return true if character device
bool FXStat::isCharacter() const {
  return (modeFlags&FXIO::Character)!=0;
  }

// Return true if block device
bool FXStat::isBlock() const {
  return (modeFlags&FXIO::Block)!=0;
  }

// Return true if socket device
bool FXStat::isSocket() const {
  return (modeFlags&FXIO::Socket)!=0;
  }

// Return true if fifo device
bool FXStat::isFifo() const {
  return (modeFlags&FXIO::Fifo)!=0;
  }

// Return true if input path is a directory
bool FXStat::isDirectory() const {
  return (modeFlags&FXIO::Directory)!=0;
  }

// Return true if file is readable
bool FXStat::isReadable() const {
  return (modeFlags&(FXIO::OtherRead|FXIO::GroupRead|FXIO::OwnerRead))!=0;
  }

// Return true if file is writable
bool FXStat::isWritable() const {
  return (modeFlags&(FXIO::OtherWrite|FXIO::GroupWrite|FXIO::OwnerWrite))!=0;
  }

// Return true if file is executable
bool FXStat::isExecutable() const {
  return (modeFlags&(FXIO::OtherExec|FXIO::GroupExec|FXIO::OwnerExec))!=0;
  }

// Return true if owner has read-write-execute permissions
bool FXStat::isOwnerReadWriteExecute() const {
  return (modeFlags&FXIO::OwnerExec) && (modeFlags&FXIO::OwnerWrite) && (modeFlags&FXIO::OwnerRead);
  }

// Return true if owner has read permissions
bool FXStat::isOwnerReadable() const {
  return (modeFlags&FXIO::OwnerRead)!=0;
  }

// Return true if owner has write permissions
bool FXStat::isOwnerWritable() const {
  return (modeFlags&FXIO::OwnerWrite)!=0;
  }

// Return true if owner has execute permissions
bool FXStat::isOwnerExecutable() const {
  return (modeFlags&FXIO::OwnerExec)!=0;
  }

// Return true if group has read-write-execute permissions
bool FXStat::isGroupReadWriteExecute() const {
  return (modeFlags&FXIO::GroupExec) && (modeFlags&FXIO::GroupWrite) && (modeFlags&FXIO::GroupRead);
  }

// Return true if group has read permissions
bool FXStat::isGroupReadable() const {
  return (modeFlags&FXIO::GroupRead)!=0;
  }

// Return true if group has write permissions
bool FXStat::isGroupWritable() const {
  return (modeFlags&FXIO::GroupWrite)!=0;
  }

// Return true if group has execute permissions
bool FXStat::isGroupExecutable() const {
  return (modeFlags&FXIO::GroupExec)!=0;
  }

// Return true if others have read-write-execute permissions
bool FXStat::isOtherReadWriteExecute() const {
  return (modeFlags&FXIO::OtherExec) && (modeFlags&FXIO::OtherWrite) && (modeFlags&FXIO::OtherRead);
  }

// Return true if others have read permissions
bool FXStat::isOtherReadable() const {
  return (modeFlags&FXIO::OtherRead)!=0;
  }

// Return true if others have write permissions
bool FXStat::isOtherWritable() const {
  return (modeFlags&FXIO::OtherWrite)!=0;
  }

// Return true if others have execute permissions
bool FXStat::isOtherExecutable() const {
  return (modeFlags&FXIO::OtherExec)!=0;
  }

// Return true if the file sets the user id on execution
bool FXStat::isSetUid() const {
  return (modeFlags&FXIO::SetUser)!=0;
  }

// Return true if the file sets the group id on execution
bool FXStat::isSetGid() const {
  return (modeFlags&FXIO::SetGroup)!=0;
  }

// Return true if the file has the sticky bit set
bool FXStat::isSetSticky() const {
  return (modeFlags&FXIO::Sticky)!=0;
  }


// Convert FILETIME (# 100ns since 01/01/1601) to time_t (# s since 01/01/1970)
#ifdef WIN32
static inline time_t fxfiletime(const FILETIME& ft){
  FXulong ll=(((FXulong)ft.dwHighDateTime)<<32)|((FXulong)ft.dwLowDateTime);
#if defined(__CYGWIN__) || defined(__MINGW32__) || defined(__SC__)
  ll=ll-116444736000000000ULL;
#else
  ll=ll-116444736000000000UL;
#endif
  ll=ll/10000000;
  return (time_t)ll;
  }
#endif


// Get statistics of given file
bool FXStat::statFile(const FXString& file,FXStat& info){
  info.modeFlags=0;
  info.userNumber=0;
  info.groupNumber=0;
  info.createTime=0;
  info.accessTime=0;
  info.modifyTime=0;
  info.fileSize=0;
  if(!file.empty()){
#ifdef WIN32
#ifdef UNICODE
    TCHAR buffer[MAXPATHLEN];
    WIN32_FILE_ATTRIBUTE_DATA data;
    SHFILEINFO sfi;
    utf2ncs(buffer,file.text(),file.length()+1);
    if(::GetFileAttributesExW(buffer,GetFileExInfoStandard,&data)){
      info.modeFlags=FXIO::OwnerFull|FXIO::GroupFull|FXIO::OtherFull;
      if(data.dwFileAttributes&FILE_ATTRIBUTE_HIDDEN) info.modeFlags|=FXIO::Hidden;
      if(data.dwFileAttributes&FILE_ATTRIBUTE_READONLY) info.modeFlags&=~(FXIO::OwnerWrite|FXIO::GroupWrite|FXIO::OtherWrite);
      if(data.dwFileAttributes&FILE_ATTRIBUTE_DIRECTORY) info.modeFlags|=FXIO::Directory|FXIO::OwnerWrite|FXIO::GroupWrite|FXIO::OtherWrite; else info.modeFlags|=FXIO::File;
      if(::SHGetFileInfoW(buffer,0,&sfi,sizeof(SHFILEINFO),SHGFI_EXETYPE)==0) info.modeFlags&=~(FXIO::OwnerExec|FXIO::GroupExec|FXIO::OtherExec);
      info.userNumber=0;
      info.groupNumber=0;
      info.accessTime=fxfiletime(data.ftLastAccessTime);
      info.modifyTime=fxfiletime(data.ftLastWriteTime);
      info.createTime=fxfiletime(data.ftCreationTime);
      info.fileSize=(((FXlong)data.nFileSizeHigh)<<32)|((FXlong)data.nFileSizeLow);
      return true;
      }
#else
    WIN32_FILE_ATTRIBUTE_DATA data;
    SHFILEINFO sfi;
    if(::GetFileAttributesExA(file.text(),GetFileExInfoStandard,&data)){
      info.modeFlags=FXIO::OwnerFull|FXIO::GroupFull|FXIO::OtherFull;
      if(data.dwFileAttributes&FILE_ATTRIBUTE_HIDDEN) info.modeFlags|=FXIO::Hidden;
      if(data.dwFileAttributes&FILE_ATTRIBUTE_READONLY) info.modeFlags&=~(FXIO::OwnerWrite|FXIO::GroupWrite|FXIO::OtherWrite);
      if(data.dwFileAttributes&FILE_ATTRIBUTE_DIRECTORY) info.modeFlags|=FXIO::Directory|FXIO::OwnerWrite|FXIO::GroupWrite|FXIO::OtherWrite; else info.modeFlags|=FXIO::File;
      if(::SHGetFileInfoA(file.text(),0,&sfi,sizeof(SHFILEINFO),SHGFI_EXETYPE)==0) info.modeFlags&=~(FXIO::OwnerExec|FXIO::GroupExec|FXIO::OtherExec);
      info.userNumber=0;
      info.groupNumber=0;
      info.accessTime=fxfiletime(data.ftLastAccessTime);
      info.modifyTime=fxfiletime(data.ftLastWriteTime);
      info.createTime=fxfiletime(data.ftCreationTime);
      info.fileSize=(((FXlong)data.nFileSizeHigh)<<32)|((FXlong)data.nFileSizeLow);
      return true;
      }
#endif
#else
    struct stat data;
    if(::stat(file.text(),&data)==0){
      info.modeFlags=(data.st_mode&0777);
      if(S_ISDIR(data.st_mode)) info.modeFlags|=FXIO::Directory;
      if(S_ISREG(data.st_mode)) info.modeFlags|=FXIO::File;
      if(S_ISLNK(data.st_mode)) info.modeFlags|=FXIO::SymLink;
      if(S_ISCHR(data.st_mode)) info.modeFlags|=FXIO::Character;
      if(S_ISBLK(data.st_mode)) info.modeFlags|=FXIO::Block;
      if(S_ISFIFO(data.st_mode)) info.modeFlags|=FXIO::Fifo;
      if(S_ISSOCK(data.st_mode)) info.modeFlags|=FXIO::Socket;
      if(data.st_mode&S_ISUID) info.modeFlags|=FXIO::SetUser;
      if(data.st_mode&S_ISGID) info.modeFlags|=FXIO::SetGroup;
      if(data.st_mode&S_ISVTX) info.modeFlags|=FXIO::Sticky;
      info.userNumber=data.st_uid;
      info.groupNumber=data.st_gid;
      info.accessTime=data.st_atime;
      info.modifyTime=data.st_mtime;
      info.createTime=data.st_ctime;
      info.fileSize=data.st_size;
      return true;
      }
#endif
    }
  return false;
  }


// Get statistice of the linked file
bool FXStat::statLink(const FXString& file,FXStat& info){
  info.modeFlags=0;
  info.userNumber=0;
  info.groupNumber=0;
  info.createTime=0;
  info.accessTime=0;
  info.modifyTime=0;
  info.fileSize=0;
  if(!file.empty()){
#ifdef WIN32
#ifdef UNICODE
    TCHAR buffer[MAXPATHLEN];
    WIN32_FILE_ATTRIBUTE_DATA data;
    SHFILEINFO sfi;
    utf2ncs(buffer,file.text(),file.length()+1);
    if(::GetFileAttributesExW(buffer,GetFileExInfoStandard,&data)){
      info.modeFlags=FXIO::OwnerFull|FXIO::GroupFull|FXIO::OtherFull;
      if(data.dwFileAttributes&FILE_ATTRIBUTE_HIDDEN) info.modeFlags|=FXIO::Hidden;
      if(data.dwFileAttributes&FILE_ATTRIBUTE_READONLY) info.modeFlags&=~(FXIO::OwnerWrite|FXIO::GroupWrite|FXIO::OtherWrite);
      if(data.dwFileAttributes&FILE_ATTRIBUTE_DIRECTORY) info.modeFlags|=FXIO::Directory|FXIO::OwnerWrite|FXIO::GroupWrite|FXIO::OtherWrite; else info.modeFlags|=FXIO::File;
      if(::SHGetFileInfoW(buffer,0,&sfi,sizeof(SHFILEINFO),SHGFI_EXETYPE)==0) info.modeFlags&=~(FXIO::OwnerExec|FXIO::GroupExec|FXIO::OtherExec);
      info.userNumber=0;
      info.groupNumber=0;
      info.accessTime=fxfiletime(data.ftLastAccessTime);
      info.modifyTime=fxfiletime(data.ftLastWriteTime);
      info.createTime=fxfiletime(data.ftCreationTime);
      info.fileSize=(((FXlong)data.nFileSizeHigh)<<32)|((FXlong)data.nFileSizeLow);
      return true;
      }
#else
    WIN32_FILE_ATTRIBUTE_DATA data;
    SHFILEINFO sfi;
    if(::GetFileAttributesExA(file.text(),GetFileExInfoStandard,&data)){
      info.modeFlags=FXIO::OwnerFull|FXIO::GroupFull|FXIO::OtherFull;
      if(data.dwFileAttributes&FILE_ATTRIBUTE_HIDDEN) info.modeFlags|=FXIO::Hidden;
      if(data.dwFileAttributes&FILE_ATTRIBUTE_READONLY) info.modeFlags&=~(FXIO::OwnerWrite|FXIO::GroupWrite|FXIO::OtherWrite);
      if(data.dwFileAttributes&FILE_ATTRIBUTE_DIRECTORY) info.modeFlags|=FXIO::Directory|FXIO::OwnerWrite|FXIO::GroupWrite|FXIO::OtherWrite; else info.modeFlags|=FXIO::File;
      if(::SHGetFileInfoA(file.text(),0,&sfi,sizeof(SHFILEINFO),SHGFI_EXETYPE)==0) info.modeFlags&=~(FXIO::OwnerExec|FXIO::GroupExec|FXIO::OtherExec);
      info.userNumber=0;
      info.groupNumber=0;
      info.accessTime=fxfiletime(data.ftLastAccessTime);
      info.modifyTime=fxfiletime(data.ftLastWriteTime);
      info.createTime=fxfiletime(data.ftCreationTime);
      info.fileSize=(((FXlong)data.nFileSizeHigh)<<32)|((FXlong)data.nFileSizeLow);
      return true;
      }
#endif
#else
    struct stat data;
    if(::lstat(file.text(),&data)==0){
      info.modeFlags=(data.st_mode&0777);
      if(S_ISDIR(data.st_mode)) info.modeFlags|=FXIO::Directory;
      if(S_ISREG(data.st_mode)) info.modeFlags|=FXIO::File;
      if(S_ISLNK(data.st_mode)) info.modeFlags|=FXIO::SymLink;
      if(S_ISCHR(data.st_mode)) info.modeFlags|=FXIO::Character;
      if(S_ISBLK(data.st_mode)) info.modeFlags|=FXIO::Block;
      if(S_ISFIFO(data.st_mode)) info.modeFlags|=FXIO::Fifo;
      if(S_ISSOCK(data.st_mode)) info.modeFlags|=FXIO::Socket;
      if(data.st_mode&S_ISUID) info.modeFlags|=FXIO::SetUser;
      if(data.st_mode&S_ISGID) info.modeFlags|=FXIO::SetGroup;
      if(data.st_mode&S_ISVTX) info.modeFlags|=FXIO::Sticky;
      info.userNumber=data.st_uid;
      info.groupNumber=data.st_gid;
      info.accessTime=data.st_atime;
      info.modifyTime=data.st_mtime;
      info.createTime=data.st_ctime;
      info.fileSize=data.st_size;
      return true;
      }
#endif
    }
  return false;
  }


// Get statistice of the linked file
bool FXStat::stat(const FXFile& file,FXStat& info){
  info.modeFlags=0;
  info.userNumber=0;
  info.groupNumber=0;
  info.createTime=0;
  info.accessTime=0;
  info.modifyTime=0;
  info.fileSize=0;
#ifdef WIN32
  BY_HANDLE_FILE_INFORMATION data;
  if(::GetFileInformationByHandle(file.handle(),&data)){
    info.modeFlags=FXIO::OwnerFull|FXIO::GroupFull|FXIO::OtherFull;
    if(data.dwFileAttributes&FILE_ATTRIBUTE_HIDDEN) info.modeFlags|=FXIO::Hidden;
    if(data.dwFileAttributes&FILE_ATTRIBUTE_READONLY) info.modeFlags&=~(FXIO::OwnerWrite|FXIO::GroupWrite|FXIO::OtherWrite);
    if(data.dwFileAttributes&FILE_ATTRIBUTE_DIRECTORY) info.modeFlags|=FXIO::Directory|FXIO::OwnerWrite|FXIO::GroupWrite|FXIO::OtherWrite; else info.modeFlags|=FXIO::File;
    info.userNumber=0;
    info.groupNumber=0;
    info.accessTime=fxfiletime(data.ftLastAccessTime);
    info.modifyTime=fxfiletime(data.ftLastWriteTime);
    info.createTime=fxfiletime(data.ftCreationTime);
    info.fileSize=(((FXulong)data.nFileSizeHigh)<<32)|((FXulong)data.nFileSizeLow);
    return true;
    }
#else
  struct stat data;
  if(::fstat(file.handle(),&data)==0){
    info.modeFlags=(data.st_mode&0777);
    if(S_ISDIR(data.st_mode)) info.modeFlags|=FXIO::Directory;
    if(S_ISREG(data.st_mode)) info.modeFlags|=FXIO::File;
    if(S_ISLNK(data.st_mode)) info.modeFlags|=FXIO::SymLink;
    if(S_ISCHR(data.st_mode)) info.modeFlags|=FXIO::Character;
    if(S_ISBLK(data.st_mode)) info.modeFlags|=FXIO::Block;
    if(S_ISFIFO(data.st_mode)) info.modeFlags|=FXIO::Fifo;
    if(S_ISSOCK(data.st_mode)) info.modeFlags|=FXIO::Socket;
    if(data.st_mode&S_ISUID) info.modeFlags|=FXIO::SetUser;
    if(data.st_mode&S_ISGID) info.modeFlags|=FXIO::SetGroup;
    if(data.st_mode&S_ISVTX) info.modeFlags|=FXIO::Sticky;
    info.userNumber=data.st_uid;
    info.groupNumber=data.st_gid;
    info.accessTime=data.st_atime;
    info.modifyTime=data.st_mtime;
    info.createTime=data.st_ctime;
    info.fileSize=data.st_size;
    return true;
    }
#endif
  return false;
  }


// Return file mode flags
FXuint FXStat::mode(const FXString& file){
  FXStat data;
  statFile(file,data);
  return data.mode();
  }


// Change the mode flags for this file
bool FXStat::mode(const FXString& file,FXuint perm){
  if(!file.empty()){
#ifndef WIN32
    FXuint bits=perm&0777;
    if(perm&FXIO::SetUser) bits|=S_ISUID;
    if(perm&FXIO::SetGroup) bits|=S_ISGID;
    if(perm&FXIO::Sticky) bits|=S_ISVTX;
    return ::chmod(file.text(),bits)==0;
#else
    return false; // FIXME Unimplemented yet
#endif
    }
  return false;
  }


// Return true if file exists
bool FXStat::exists(const FXString& file){
  if(!file.empty()){
#ifdef WIN32
#ifdef UNICODE
    FXnchar name[1024];
    utf2ncs(name,file.text(),file.length()+1);
    return ::GetFileAttributesW(name)!=0xffffffff;
#else
    return ::GetFileAttributesA(file.text())!=0xffffffff;
#endif
#else
    struct stat status;
    return ::stat(file.text(),&status)==0;
#endif
    }
  return false;
  }


// Get file size
FXlong FXStat::size(const FXString& file){
  FXStat data;
  statFile(file,data);
  return data.size();
  }


// Return time file was last modified
FXTime FXStat::modified(const FXString& file){
  FXStat data;
  statFile(file,data);
  return data.modified();
  }


// Return time file was last accessed
FXTime FXStat::accessed(const FXString& file){
  FXStat data;
  statFile(file,data);
  return data.accessed();
  }


// Return time when created
FXTime FXStat::created(const FXString& file){
  FXStat data;
  statFile(file,data);
  return data.created();
  }


// Return time anything was changed
FXTime FXStat::touched(const FXString& file){
  FXStat data;
  statFile(file,data);
  return data.touched();
  }


// Return true if file is hidden
bool FXStat::isHidden(const FXString& file){
  FXStat data;
  return statFile(file,data) && data.isHidden();
  }


// Check if file represents a file
bool FXStat::isFile(const FXString& file){
  FXStat data;
  return statFile(file,data) && data.isFile();
  }


// Check if file represents a link
bool FXStat::isLink(const FXString& file){
  FXStat data;
  return statLink(file,data) && data.isLink();
  }


// Check if file represents a directory
bool FXStat::isDirectory(const FXString& file){
  FXStat data;
  return statFile(file,data) && data.isDirectory();
  }


// Return true if file is readable
bool FXStat::isReadable(const FXString& file){
  FXStat data;
  return statFile(file,data) && data.isReadable();
  }


// Return true if file is writable
bool FXStat::isWritable(const FXString& file){
  FXStat data;
  return statFile(file,data) && data.isWritable();
  }


// Return true if file is executable
bool FXStat::isExecutable(const FXString& file){
  FXStat data;
  return statFile(file,data) && data.isExecutable();
  }


// Check if owner has full permissions
bool FXStat::isOwnerReadWriteExecute(const FXString& file){
  FXStat data;
  return statFile(file,data) && data.isOwnerReadWriteExecute();
  }


// Check if owner can read
bool FXStat::isOwnerReadable(const FXString& file){
  FXStat data;
  return statFile(file,data) && data.isOwnerReadable();
  }


// Check if owner can write
bool FXStat::isOwnerWritable(const FXString& file){
  FXStat data;
  return statFile(file,data) && data.isOwnerWritable();
  }


// Check if owner can execute
bool FXStat::isOwnerExecutable(const FXString& file){
  FXStat data;
  return statFile(file,data) && data.isOwnerExecutable();
  }


// Check if group has full permissions
bool FXStat::isGroupReadWriteExecute(const FXString& file){
  FXStat data;
  return statFile(file,data) && data.isGroupReadWriteExecute();
  }


// Check if group can read
bool FXStat::isGroupReadable(const FXString& file){
  FXStat data;
  return statFile(file,data) && data.isGroupReadable();
  }


// Check if group can write
bool FXStat::isGroupWritable(const FXString& file){
  FXStat data;
  return statFile(file,data) && data.isGroupWritable();
  }


// Check if group can execute
bool FXStat::isGroupExecutable(const FXString& file){
  FXStat data;
  return statFile(file,data) && data.isGroupExecutable();
  }


// Check if everybody has full permissions
bool FXStat::isOtherReadWriteExecute(const FXString& file){
  FXStat data;
  return statFile(file,data) && data.isOtherReadWriteExecute();
  }


// Check if everybody can read
bool FXStat::isOtherReadable(const FXString& file){
  FXStat data;
  return statFile(file,data) && data.isOtherReadable();
  }


// Check if everybody can write
bool FXStat::isOtherWritable(const FXString& file){
  FXStat data;
  return statFile(file,data) && data.isOtherWritable();
  }


// Check if everybody can execute
bool FXStat::isOtherExecutable(const FXString& file){
  FXStat data;
  return statFile(file,data) && data.isOtherExecutable();
  }


// Test if suid bit set
bool FXStat::isSetUid(const FXString& file){
  FXStat data;
  return statFile(file,data) && data.isSetUid();
  }


// Test if sgid bit set
bool FXStat::isSetGid(const FXString& file){
  FXStat data;
  return statFile(file,data) && data.isSetGid();
  }


// Test if sticky bit set
bool FXStat::isSetSticky(const FXString& file){
  FXStat data;
  return statFile(file,data) && data.isSetSticky();
  }


}

