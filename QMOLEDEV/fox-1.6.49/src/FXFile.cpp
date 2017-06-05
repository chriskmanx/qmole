/********************************************************************************
*                                                                               *
*                             F i l e   C l a s s                               *
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
* $Id: FXFile.cpp,v 1.249.2.2 2006/11/07 15:58:52 fox Exp $                         *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxascii.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXPath.h"
#include "FXIO.h"
#include "FXStat.h"
#include "FXFile.h"
#include "FXPipe.h"
#include "FXDir.h"



/*
  Notes:

  - Implemented many functions in terms of FXFile and FXDir
    so we won't have to worry about unicode stuff.
  - Perhaps we should assume FXString contains string encoded in the locale
    of the system [which in case of Windows would mean it contains UTF-16]?
    Because it isn't between 8-bit or 16-bit, but also about utf-8 v.s. other
    encodings..
  - This should be in FXSystem; FXSystem needs to determine the locale, then
    determine the codec needed for that locale, and then use this codec for
    encoding our strings to that locale.
*/

#ifdef WIN32
#define BadHandle INVALID_HANDLE_VALUE
#else
#define BadHandle -1
#endif

#ifdef WIN32
#ifndef INVALID_SET_FILE_POINTER
#define INVALID_SET_FILE_POINTER ((DWORD)-1)
#endif
#endif

using namespace FX;

/*******************************************************************************/

namespace FX {



// Construct file and attach existing handle h
FXFile::FXFile(FXInputHandle handle,FXuint mode){
  FXIO::open(handle,mode);
  }


// Construct and open a file
FXFile::FXFile(const FXString& file,FXuint mode,FXuint perm){
  open(file,mode,perm);
  }


// Open file
bool FXFile::open(const FXString& file,FXuint mode,FXuint perm){
  if(!file.empty() && !isOpen()){
#ifdef WIN32
    DWORD flags=GENERIC_READ;
    DWORD creation=OPEN_EXISTING;

    // Basic access mode
    switch(mode&(ReadOnly|WriteOnly)){
      case ReadOnly: flags=GENERIC_READ; break;
      case WriteOnly: flags=GENERIC_WRITE; break;
      case ReadWrite: flags=GENERIC_READ|GENERIC_WRITE; break;
      }

    // Creation and truncation mode
    switch(mode&(Create|Truncate|Exclusive)){
      case Create: creation=OPEN_ALWAYS; break;
      case Truncate: creation=TRUNCATE_EXISTING; break;
      case Create|Truncate: creation=CREATE_ALWAYS; break;
      case Create|Truncate|Exclusive: creation=CREATE_NEW; break;
      }

    // Non-blocking mode
    if(mode&NonBlocking){
      // FIXME
      }

#ifdef UNICODE
    FXnchar unifile[1024];
    utf2ncs(unifile,file.text(),file.length()+1);
    device=::CreateFileW(unifile,flags,FILE_SHARE_READ|FILE_SHARE_WRITE,NULL,creation,FILE_ATTRIBUTE_NORMAL,NULL);
#else
    device=::CreateFileA(file.text(),flags,FILE_SHARE_READ|FILE_SHARE_WRITE,NULL,creation,FILE_ATTRIBUTE_NORMAL,NULL);
#endif
    access=mode;

    // Appending
    if(mode&Append) ::SetFilePointer(device,0,NULL,FILE_END);
    return (device!=BadHandle);
#else
    FXuint bits=perm&0777;
    FXuint flags=0;

    // Basic access mode
    switch(mode&(ReadOnly|WriteOnly)){
      case ReadOnly: flags=O_RDONLY; break;
      case WriteOnly: flags=O_WRONLY; break;
      case ReadWrite: flags=O_RDWR; break;
      }
// O_LARGEFILE

    // Appending and truncation
    if(mode&Append) flags|=O_APPEND;
    if(mode&Truncate) flags|=O_TRUNC;

    // Non-blocking mode
    if(mode&NonBlocking) flags|=O_NONBLOCK;

    // Creation mode
    if(mode&Create){
      flags|=O_CREAT;
      if(mode&Exclusive) flags|=O_EXCL;
      }

    // Permission bits
    if(perm&FXIO::SetUser) bits|=S_ISUID;
    if(perm&FXIO::SetGroup) bits|=S_ISGID;
    if(perm&FXIO::Sticky) bits|=S_ISVTX;

    // Do it
    device=::open(file.text(),flags,bits);
    access=mode;
    return (device!=BadHandle);
#endif
    }
  return false;
  }


// Open device with access mode and handle
bool FXFile::open(FXInputHandle handle,FXuint mode){
  return FXIO::open(handle,mode);
  }


// Get position
FXlong FXFile::position() const {
  if(isOpen()){
#ifdef WIN32
    LARGE_INTEGER pos;
    pos.QuadPart=0;
    pos.LowPart=::SetFilePointer(device,0,&pos.HighPart,FILE_CURRENT);
    if(pos.LowPart==INVALID_SET_FILE_POINTER && GetLastError()!=NO_ERROR) pos.QuadPart=-1;
    return pos.QuadPart;
#else
    return ::lseek(device,0,SEEK_CUR);
#endif
    }
  return -1;
  }


// Move to position
FXlong FXFile::position(FXlong offset,FXuint from){
  if(isOpen()){
#ifdef WIN32
    LARGE_INTEGER pos;
    pos.QuadPart=offset;
    pos.LowPart=::SetFilePointer(device,pos.LowPart,&pos.HighPart,from);
    if(pos.LowPart==INVALID_SET_FILE_POINTER && GetLastError()!=NO_ERROR) pos.QuadPart=-1;
    return pos.QuadPart;
#else
    return ::lseek(device,offset,from);
#endif
    }
  return -1;
  }


// Read block
FXival FXFile::readBlock(void* data,FXival count){
  FXival nread=-1;
  if(isOpen()){
#ifdef WIN32
    DWORD nr;
    if(::ReadFile(device,data,(DWORD)count,&nr,NULL)!=0){
      nread=(FXival)nr;
      }
#else
    do{
      nread=::read(device,data,count);
      }
    while(nread<0 && errno==EINTR);
#endif
    }
  return nread;
  }


// Write block
FXival FXFile::writeBlock(const void* data,FXival count){
  FXival nwritten=-1;
  if(isOpen()){
#ifdef WIN32
    DWORD nw;
    if(::WriteFile(device,data,(DWORD)count,&nw,NULL)!=0){
      nwritten=(FXival)nw;
      }
#else
    do{
      nwritten=::write(device,data,count);
      }
    while(nwritten<0 && errno==EINTR);
#endif
    }
  return nwritten;
  }


// Truncate file
FXlong FXFile::truncate(FXlong size){
  if(isOpen()){
#ifdef WIN32
    LARGE_INTEGER oldpos,newpos;
    oldpos.QuadPart=0;
    newpos.QuadPart=size;
    oldpos.LowPart=::SetFilePointer(device,0,&oldpos.HighPart,FILE_CURRENT);
    newpos.LowPart=::SetFilePointer(device,newpos.LowPart,&newpos.HighPart,FILE_BEGIN);
    if((newpos.LowPart==INVALID_SET_FILE_POINTER && GetLastError()!=NO_ERROR) || ::SetEndOfFile(device)==0) newpos.QuadPart=-1;
    ::SetFilePointer(device,oldpos.LowPart,&oldpos.HighPart,FILE_BEGIN);
    return newpos.QuadPart;
#else
    if(::ftruncate(device,size)==0) return size;
#endif
    }
  return -1;
  }


// Flush to disk
bool FXFile::flush(){
  if(isOpen()){
#ifdef WIN32
    return ::FlushFileBuffers(device)!=0;
#else
    return ::fsync(device)==0;
#endif
    }
  return false;
  }


// Test if we're at the end
bool FXFile::eof(){
  if(isOpen()){
    register FXlong pos=position();
    return 0<=pos && size()<=pos;
    }
  return true;
  }


// Return file size
FXlong FXFile::size(){
  if(isOpen()){
#ifdef WIN32
    ULARGE_INTEGER result;
    result.LowPart=::GetFileSize(device,&result.HighPart);
    return result.QuadPart;
#else
    struct stat data;
    if(::fstat(device,&data)==0) return data.st_size;
#endif
    }
  return -1;
  }


// Close file
bool FXFile::close(){
  if(isOpen()){
    FXInputHandle dev=device;
    device=BadHandle;
#ifdef WIN32
    return ::CloseHandle(dev)!=0;
#else
    return ::close(dev)==0;
#endif
    }
  return false;
  }


// Create new (empty) file
bool FXFile::create(const FXString& file,FXuint perm){
  if(!file.empty()){
#ifdef WIN32
#ifdef UNICODE
    FXnchar buffer[1024];
    utf2ncs(buffer,file.text(),file.length()+1);
    FXInputHandle h=::CreateFileW(buffer,GENERIC_WRITE,FILE_SHARE_READ,NULL,CREATE_NEW,FILE_ATTRIBUTE_NORMAL,NULL);
#else
    FXInputHandle h=::CreateFileA(file.text(),GENERIC_WRITE,FILE_SHARE_READ,NULL,CREATE_NEW,FILE_ATTRIBUTE_NORMAL,NULL);
#endif
    if(h!=BadHandle){ ::CloseHandle(h); return true; }
#else
    FXInputHandle h=::open(file.text(),O_CREAT|O_WRONLY|O_TRUNC|O_EXCL,perm);
    if(h!=BadHandle){ ::close(h); return true; }
#endif
    }
  return false;
  }


// Remove a file
bool FXFile::remove(const FXString& file){
  if(!file.empty()){
#ifdef WIN32
#ifdef UNICODE
    FXnchar buffer[1024];
    utf2ncs(buffer,file.text(),file.length()+1);
    return ::DeleteFileW(buffer)!=0;
#else
    return ::DeleteFileA(file.text())!=0;
#endif
#else
    return ::unlink(file.text())==0;
#endif
    }
  return false;
  }


// Rename file
bool FXFile::rename(const FXString& srcfile,const FXString& dstfile){
  if(srcfile!=dstfile){
#ifdef WIN32
#ifdef UNICODE
    FXnchar oldname[1024],newname[1024];
    utf2ncs(oldname,srcfile.text(),srcfile.length()+1);
    utf2ncs(newname,dstfile.text(),dstfile.length()+1);
    return ::MoveFileExW(oldname,newname,MOVEFILE_REPLACE_EXISTING)!=0;
#else
    return ::MoveFileExA(srcfile.text(),dstfile.text(),MOVEFILE_REPLACE_EXISTING)!=0;
#endif
#else
    return ::rename(srcfile.text(),dstfile.text())==0;
#endif
    }
  return false;
  }


#ifdef WIN32

typedef BOOL (WINAPI *FunctionCreateHardLink)(const TCHAR*,const TCHAR*,LPSECURITY_ATTRIBUTES);

static BOOL WINAPI HelpCreateHardLink(const TCHAR*,const TCHAR*,LPSECURITY_ATTRIBUTES);

static FunctionCreateHardLink MyCreateHardLink=HelpCreateHardLink;


// The first time its called, we're setting the function pointer, so
// subsequent calls will experience no additional overhead whatsoever!
static BOOL WINAPI HelpCreateHardLink(const TCHAR* newname,const TCHAR* oldname,LPSECURITY_ATTRIBUTES sa){
#ifdef UNICODE
  HMODULE hkernel=LoadLibraryW(L"Kernel32");
  if(hkernel){
    MyCreateHardLink=(FunctionCreateHardLink)::GetProcAddress(hkernel,"CreateHardLinkW");
    ::FreeLibrary(hkernel);
    return MyCreateHardLink(newname,oldname,sa);
    }
#else
  HMODULE hkernel=LoadLibraryA("Kernel32");
  if(hkernel){
    MyCreateHardLink=(FunctionCreateHardLink)::GetProcAddress(hkernel,"CreateHardLinkA");
    ::FreeLibrary(hkernel);
    return MyCreateHardLink(newname,oldname,sa);
    }
#endif
  return 0;
  }

#endif


// Link file
bool FXFile::link(const FXString& oldfile,const FXString& newfile){
  if(newfile!=oldfile){
#ifdef WIN32
#ifdef UNICODE
    FXnchar oldname[1024],newname[1024];
    utf2ncs(oldname,oldfile.text(),oldfile.length()+1);
    utf2ncs(newname,newfile.text(),newfile.length()+1);
    return MyCreateHardLink(newname,oldname,NULL)!=0;
#else
    return MyCreateHardLink(newfile.text(),oldfile.text(),NULL)!=0;
#endif
#else
    return ::link(oldfile.text(),newfile.text())==0;
#endif
    }
  return false;
  }


// Read symbolic link
FXString FXFile::symlink(const FXString& file){
  if(!file.empty()){
#ifndef WIN32
    FXchar lnk[MAXPATHLEN+1];
    FXint len=::readlink(file.text(),lnk,MAXPATHLEN);
    if(0<=len){
      return FXString(lnk,len);
      }
#endif
    }
  return FXString::null;
  }


// Symbolic Link file
bool FXFile::symlink(const FXString& oldfile,const FXString& newfile){
  if(newfile!=oldfile){
#ifndef WIN32
    return ::symlink(oldfile.text(),newfile.text())==0;
#endif
    }
  return false;
  }


// Return true if files are identical
bool FXFile::identical(const FXString& file1,const FXString& file2){
  if(file1!=file2){
#ifdef WIN32
    BY_HANDLE_FILE_INFORMATION info1,info2;
    HANDLE hFile1,hFile2;
    bool same=false;
#ifdef UNICODE
    FXnchar name1[1024],name2[1024];
    utf2ncs(name1,file1.text(),file1.length()+1);
    utf2ncs(name2,file2.text(),file2.length()+1);
    hFile1=::CreateFile(name1,GENERIC_READ,FILE_SHARE_READ,NULL,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,NULL);
    if(hFile1!=INVALID_HANDLE_VALUE){
      hFile2=::CreateFile(name2,GENERIC_READ,FILE_SHARE_READ,NULL,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,NULL);
      if(hFile2!=INVALID_HANDLE_VALUE){
        if(::GetFileInformationByHandle(hFile1,&info1) && ::GetFileInformationByHandle(hFile2,&info2)){
          same=(info1.nFileIndexLow==info2.nFileIndexLow && info1.nFileIndexHigh==info2.nFileIndexHigh && info1.dwVolumeSerialNumber==info2.dwVolumeSerialNumber);
          }
        ::CloseHandle(hFile2);
        }
      ::CloseHandle(hFile1);
      }
    return same;
#else
    hFile1=::CreateFile(file1.text(),GENERIC_READ,FILE_SHARE_READ,NULL,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,NULL);
    if(hFile1!=INVALID_HANDLE_VALUE){
      hFile2=::CreateFile(file2.text(),GENERIC_READ,FILE_SHARE_READ,NULL,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,NULL);
      if(hFile2!=INVALID_HANDLE_VALUE){
        if(::GetFileInformationByHandle(hFile1,&info1) && ::GetFileInformationByHandle(hFile2,&info2)){
          same=(info1.nFileIndexLow==info2.nFileIndexLow && info1.nFileIndexHigh==info2.nFileIndexHigh && info1.dwVolumeSerialNumber==info2.dwVolumeSerialNumber);
          }
        ::CloseHandle(hFile2);
        }
      ::CloseHandle(hFile1);
      }
    return same;
#endif
#else
    struct stat stat1,stat2;
    return !::lstat(file1.text(),&stat1) && !::lstat(file2.text(),&stat2) && stat1.st_ino==stat2.st_ino && stat1.st_dev==stat2.st_dev;
#endif
    }
  return true;
  }


// Copy srcfile to dstfile, overwriting dstfile if allowed
bool FXFile::copy(const FXString& srcfile,const FXString& dstfile,bool overwrite){
  if(srcfile!=dstfile){
    FXuchar buffer[4096]; FXival nwritten,nread; FXStat stat;
    FXFile src(srcfile,FXIO::Reading);
    if(src.isOpen()){
      if(FXStat::stat(src,stat)){
        FXFile dst(dstfile,overwrite?FXIO::Writing:FXIO::Writing|FXIO::Exclusive,stat.mode());
        if(dst.isOpen()){
          while(1){
            nread=src.readBlock(buffer,sizeof(buffer));
            if(nread<0) return false;
            if(nread==0) break;
            nwritten=dst.writeBlock(buffer,nread);
            if(nwritten<0) return false;
            }
          return true;
          }
        }
      }
    }
  return false;
  }


// Concatenate srcfile1 and srcfile2 to dstfile, overwriting dstfile if allowed
bool FXFile::concat(const FXString& srcfile1,const FXString& srcfile2,const FXString& dstfile,bool overwrite){
  FXuchar buffer[4096]; FXival nwritten,nread;
  if(srcfile1!=dstfile && srcfile2!=dstfile){
    FXFile src1(srcfile1,FXIO::Reading);
    if(src1.isOpen()){
      FXFile src2(srcfile2,FXIO::Reading);
      if(src2.isOpen()){
        FXFile dst(dstfile,overwrite?FXIO::Writing:FXIO::Writing|FXIO::Exclusive);
        if(dst.isOpen()){
          while(1){
            nread=src1.readBlock(buffer,sizeof(buffer));
            if(nread<0) return false;
            if(nread==0) break;
            nwritten=dst.writeBlock(buffer,nread);
            if(nwritten<0) return false;
            }
          while(1){
            nread=src2.readBlock(buffer,sizeof(buffer));
            if(nread<0) return false;
            if(nread==0) break;
            nwritten=dst.writeBlock(buffer,nread);
            if(nwritten<0) return false;
            }
          return true;
          }
        }
      }
    }
  return false;
  }


// Recursively copy files or directories from srcfile to dstfile, overwriting dstfile if allowed
bool FXFile::copyFiles(const FXString& srcfile,const FXString& dstfile,bool overwrite){
  if(srcfile!=dstfile){
    FXString name,linkname;
    FXStat srcstat;
    FXStat dststat;
    FXTRACE((100,"FXFile::copyFiles(%s,%s)\n",srcfile.text(),dstfile.text()));
    if(FXStat::statLink(srcfile,srcstat)){

      // Destination is a directory?
      if(FXStat::statLink(dstfile,dststat)){
        if(!dststat.isDirectory()){
          if(!overwrite) return false;
          //FXTRACE((100,"FXFile::remove(%s)\n",dstfile.text()));
          if(!FXFile::remove(dstfile)) return false;
          }
        }

      // Source is a directory
      if(srcstat.isDirectory()){

        // Make destination directory if needed
        if(!dststat.isDirectory()){
          //FXTRACE((100,"FXDir::create(%s)\n",dstfile.text()));

          // Make directory
          if(!FXDir::create(dstfile,srcstat.mode()|FXIO::OwnerWrite)) return false;
          }

        // Open source directory
        FXDir dir(srcfile);

        // Copy source directory
        while(dir.next()){

          // Next name
          name=dir.name();

          // Skip '.' and '..'
          if(name[0]=='.' && (name[1]=='\0' || (name[1]=='.' && name[2]=='\0'))) continue;

          // Recurse
          if(!FXFile::copyFiles(srcfile+PATHSEP+name,dstfile+PATHSEP+name,overwrite)) return false;
          }

        // OK
        return true;
        }

      // Source is a file
      if(srcstat.isFile()){
        //FXTRACE((100,"FXFile::copyFile(%s,%s)\n",srcfile.text(),dstfile.text()));

        // Simply copy
        if(!FXFile::copy(srcfile,dstfile,overwrite)) return false;

        // OK
        return true;
        }

      // Source is symbolic link: make a new one
      if(srcstat.isLink()){
        linkname=FXFile::symlink(srcfile);
        //FXTRACE((100,"symlink(%s,%s)\n",srcfile.text(),dstfile.text()));

        // New symlink to whatever old one referred to
        if(!FXFile::symlink(srcfile,dstfile)) return false;

        // OK
        return true;
        }

      // Source is fifo: make a new one
      if(srcstat.isFifo()){
        //FXTRACE((100,"FXPipe::create(%s)\n",dstfile.text()));

        // Make named pipe
        if(!FXPipe::create(dstfile,srcstat.mode())) return false;

        // OK
        return true;
        }

/*
  // Source is device: make a new one
  if(S_ISBLK(status1.st_mode) || S_ISCHR(status1.st_mode) || S_ISSOCK(status1.st_mode)){
    FXTRACE((100,"mknod(%s)\n",newfile.text()));
    return ::mknod(newfile.text(),status1.st_mode,status1.st_rdev)==0;
    }
*/

      }
    }
  return false;
  }



// Recursively copy or move files or directories from srcfile to dstfile, overwriting dstfile if allowed
bool FXFile::moveFiles(const FXString& srcfile,const FXString& dstfile,bool overwrite){
  if(srcfile!=dstfile){
    if(FXStat::exists(srcfile)){
      if(FXStat::exists(dstfile)){
        if(!overwrite) return false;
        if(!FXFile::removeFiles(dstfile,true)) return false;
        }
      if(FXDir::rename(srcfile,dstfile)) return true;
      if(FXFile::copyFiles(srcfile,dstfile,overwrite)){
        return FXFile::removeFiles(srcfile,true);
        }
      }
    }
  return false;
  }


// Remove file or directory, recursively if allowed
bool FXFile::removeFiles(const FXString& path,bool recursive){
  FXStat stat;
  FXTRACE((100,"removeFiles(%s)\n",path.text()));
  if(FXStat::statLink(path,stat)){
    if(stat.isDirectory()){
      if(recursive){
        FXDir dir(path);
        FXString name;
        while(dir.next()){
          name=dir.name();
          if(name[0]=='.' && (name[1]=='\0' || (name[1]=='.' && name[2]=='\0'))) continue;
          if(!FXFile::removeFiles(path+PATHSEP+name,true)) return false;
          }
        }
      return FXDir::remove(path);
      }
    return FXFile::remove(path);
    }
  return false;
  }


// Destroy
FXFile::~FXFile(){
  close();
  }


}

