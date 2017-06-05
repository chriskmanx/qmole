/********************************************************************************
*                                                                               *
*                    D i r e c t o r y   E n u m e r a t o r                    *
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
* $Id: FXDir.cpp,v 1.39 2006/01/22 17:58:22 fox Exp $                           *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXStat.h"
#include "FXFile.h"
#include "FXPath.h"
#include "FXDir.h"

/*
  Notes:
  - This class implements a way to list the files in a directory.
  - We just want to wrap directory iteration, nothing fancy.
  - Maybe add positioning for seek and tell type functions.
*/


using namespace FX;

/*******************************************************************************/

namespace FX {


#ifdef WIN32
struct SPACE {
  HANDLE          handle;
  FXuint          first;
  WIN32_FIND_DATA result;
  };
#else
struct SPACE {
  DIR*            handle;
  struct dirent*  dp;
  struct fxdirent result;
  };
#endif


// Construct directory enumerator
FXDir::FXDir(){
  // If this fails on your machine, determine what sizeof(SPACE) is
  // on your machine and mail it to: jeroen@fox-toolkit.org!
  //FXTRACE((150,"sizeof(SPACE)=%ld\n",sizeof(SPACE)));
  FXASSERT(sizeof(SPACE)<=sizeof(space));
#ifdef WIN32
  ((SPACE*)space)->handle=INVALID_HANDLE_VALUE;
#else
  ((SPACE*)space)->handle=NULL;
#endif
  }


// Construct directory enumerator
FXDir::FXDir(const FXString& path){
  // If this fails on your machine, determine what sizeof(SPACE) is
  // on your machine and mail it to: jeroen@fox-toolkit.org!
  //FXTRACE((150,"sizeof(SPACE)=%ld\n",sizeof(SPACE)));
  FXASSERT(sizeof(SPACE)<=sizeof(space));
#ifdef WIN32
  ((SPACE*)space)->handle=INVALID_HANDLE_VALUE;
#else
  ((SPACE*)space)->handle=NULL;
#endif
  open(path);
  }


// Open directory to path, return true if ok.
bool FXDir::open(const FXString& path){
  if(!path.empty()){
#ifdef WIN32
#ifdef UNICODE
    FXnchar buffer[1024];
    utf2ncs(buffer,path.text(),path.length()+1);
    wcsncat(buffer,TEXT("\\*"),sizeof(buffer));
#else
    FXchar buffer[1024];
    strncpy(buffer,path.text(),sizeof(buffer));
    strncat(buffer,"\\*",sizeof(buffer));
#endif
    ((SPACE*)space)->handle=FindFirstFile(buffer,&((SPACE*)space)->result);
    if(((SPACE*)space)->handle!=INVALID_HANDLE_VALUE){
      ((SPACE*)space)->first=TRUE;
      return true;
      }
#else
    ((SPACE*)space)->handle=opendir(path.text());
    if(((SPACE*)space)->handle!=NULL){
      return true;
      }
#endif
    }
  return false;
  }


// Returns true if the directory is open
bool FXDir::isOpen() const {
#ifdef WIN32
  return (((SPACE*)space)->handle!=INVALID_HANDLE_VALUE);
#else
  return (((SPACE*)space)->handle!=NULL);
#endif
  }


// Get next file name
bool FXDir::next(){
  if(isOpen()){
#ifdef WIN32
    if(((SPACE*)space)->first || FindNextFile(((SPACE*)space)->handle,&((SPACE*)space)->result)){
      ((SPACE*)space)->first=false;
      return true;
      }
#else
#if defined(FOX_THREAD_SAFE) && !defined(__FreeBSD__) && !defined(__OpenBSD__)
    if(!readdir_r(((SPACE*)space)->handle,&((SPACE*)space)->result,&((SPACE*)space)->dp) && ((SPACE*)space)->dp){
      return true;
      }
#else
    if((((SPACE*)space)->dp=readdir(((SPACE*)space)->handle))!=NULL){
      return true;
      }
#endif
#endif
    }
  return false;
  }


// Return current file name
FXString FXDir::name() const {
  if(isOpen()){
#ifdef WIN32
    return ((SPACE*)space)->result.cFileName;
#else
    return ((SPACE*)space)->dp->d_name;
#endif
    }
  return FXString::null;
  }


// Close directory
void FXDir::close(){
  if(isOpen()){
#ifdef WIN32
    FindClose(((SPACE*)space)->handle);
    ((SPACE*)space)->handle=INVALID_HANDLE_VALUE;
#else
    closedir(((SPACE*)space)->handle);
    ((SPACE*)space)->handle=NULL;
#endif
    }
  }


// Create new directory
bool FXDir::create(const FXString& path,FXuint mode){
  if(!path.empty()){
#ifdef WIN32
#ifdef UNICODE
    FXnchar buffer[1024];
    utf2ncs(buffer,path.text(),path.length()+1);
    return CreateDirectoryW(buffer,NULL)!=0;
#else
    return CreateDirectoryA(path.text(),NULL)!=0;
#endif
#else
    return ::mkdir(path.text(),mode)==0;
#endif
    }
  return false;
  }


// Remove directory
bool FXDir::remove(const FXString& path){
  if(!path.empty()){
#ifdef WIN32
#ifdef UNICODE
    FXnchar buffer[1024];
    utf2ncs(buffer,path.text(),path.length()+1);
    return RemoveDirectoryW(buffer)!=0;
#else
    return RemoveDirectoryA(path.text())!=0;
#endif
#else
    return ::rmdir(path.text())==0;
#endif
    }
  return false;
  }


// Rename directory
bool FXDir::rename(const FXString& srcpath,const FXString& dstpath){
  if(srcpath!=dstpath){
#ifdef WIN32
#ifdef UNICODE
    FXnchar oldname[1024],newname[1024];
    utf2ncs(oldname,srcpath.text(),srcpath.length()+1);
    utf2ncs(newname,dstpath.text(),dstpath.length()+1);
    return ::MoveFileW(oldname,newname)!=0;
#else
    return ::MoveFileA(srcpath.text(),dstpath.text())!=0;
#endif
#else
    return ::rename(srcpath.text(),dstpath.text())==0;
#endif
    }
  return false;
  }


// List all the files in directory
FXint FXDir::listFiles(FXString*& filelist,const FXString& path,const FXString& pattern,FXuint flags){
  FXDir dir(path);

  // Initialize to empty
  filelist=NULL;

  // Get directory stream pointer
  if(dir.isOpen()){
    FXuint    mode=FILEMATCH_FILE_NAME|FILEMATCH_NOESCAPE;
    FXString *newlist;
    FXint     size=0;
    FXint     count=0;
    FXString  pathname;
    FXString  name;
    FXStat    data;

    // Folding case
    if(flags&CaseFold) mode|=FILEMATCH_CASEFOLD;

    // Loop over directory entries
    while(dir.next()){

      // Get name
      name=dir.name();

      // Build full pathname
      pathname=path;
      if(!ISPATHSEP(pathname[pathname.length()-1])) pathname+=PATHSEPSTRING;
      pathname+=name;

      // Get info on file
      if(!FXStat::statFile(pathname,data)) continue;

#ifdef WIN32

      // Filter out files; a bit tricky...
      if(!data.isDirectory() && ((flags&NoFiles) || (data.isHidden() && !(flags&HiddenFiles)) || (!(flags&AllFiles) && !FXPath::match(pattern,name,mode)))) continue;

      // Filter out directories; even more tricky!
      if(data.isDirectory() && ((flags&NoDirs) || (data.isHidden() && !(flags&HiddenDirs)) || (name[0]=='.' && (name[1]==0 || (name[1]=='.' && name[2]==0 && (flags&NoParent)))) || (!(flags&AllDirs) && !FXPath::match(pattern,name,mode)))) continue;

#else

      // Filter out files; a bit tricky...
      if(!data.isDirectory() && ((flags&NoFiles) || (name[0]=='.' && !(flags&HiddenFiles)) || (!(flags&AllFiles) && !FXPath::match(pattern,name,mode)))) continue;

      // Filter out directories; even more tricky!
      if(data.isDirectory() && ((flags&NoDirs) || (name[0]=='.' && (name[1]==0 || (name[1]=='.' && name[2]==0 && (flags&NoParent)) || (name[1]!='.' && !(flags&HiddenDirs)))) || (!(flags&AllDirs) && !FXPath::match(pattern,name,mode)))) continue;

#endif

      // Grow list
      if(count+1>=size){
        size=size?(size<<1):256;
        newlist=new FXString [size];
        for(int i=0; i<count; i++){
          newlist[i].adopt(filelist[i]);
          }
        delete [] filelist;
        filelist=newlist;
        }

      // Add to list
      filelist[count++].adopt(name);
      }
    return count;
    }
  return 0;
  }


// FIXME maybe an FXDrive class for iteration over drives is better?

// List drives, i.e. roots of directory trees.
FXint FXDir::listDrives(FXString*& drivelist){
  register FXint count=0;
#ifdef WIN32
  TCHAR drives[256],*drive;
  GetLogicalDriveStrings(256,drives);
  drivelist=new FXString [33];
  for(drive=drives; *drive && count<32; drive++){
    drivelist[count++].assign(drive);
    while(*drive) drive++;
    }
#else
  drivelist=new FXString [2];
  drivelist[count++].assign(PATHSEP);
#endif
  return count;
  }


// Cleanup
FXDir::~FXDir(){
  close();
  }

}








#if 0

// List all the files in directory
FXint FXDir::listFiles(FXString*& filelist,const FXString& path,const FXString& pattern,FXuint flags){
  FXuint matchmode=FILEMATCH_FILE_NAME|FILEMATCH_NOESCAPE;
  FXString pathname;
  FXString name;
  FXString *newlist;
  FXint count=0;
  FXint size=0;
  WIN32_FIND_DATA ffData;
  DWORD nCount,nSize,i,j;
  HANDLE hFindFile,hEnum;
  FXchar server[200];

  // Initialize to empty
  filelist=NULL;

/*
  // Each drive is a root on windows
  if(path.empty()){
    FXchar letter[4];
    letter[0]='a';
    letter[1]=':';
    letter[2]=PATHSEP;
    letter[3]='\0';
    filelist=new FXString[28];
    for(DWORD mask=GetLogicalDrives(); mask; mask>>=1,letter[0]++){
      if(mask&1) list[count++]=letter;
      }
    filelist[count++]=PATHSEPSTRING PATHSEPSTRING;    // UNC for file shares
    return count;
    }
*/
/*
  // A UNC name was given of the form "\\" or "\\server"
  if(ISPATHSEP(path[0]) && ISPATHSEP(path[1]) && path.find(PATHSEP,2)<0){
    NETRESOURCE host;

    // Fill in
    host.dwScope=RESOURCE_GLOBALNET;
    host.dwType=RESOURCETYPE_DISK;
    host.dwDisplayType=RESOURCEDISPLAYTYPE_GENERIC;
    host.dwUsage=RESOURCEUSAGE_CONTAINER;
    host.lpLocalName=NULL;
    host.lpRemoteName=(char*)path.text();
    host.lpComment=NULL;
    host.lpProvider=NULL;

    // Open network enumeration
    if(WNetOpenEnum((path[2]?RESOURCE_GLOBALNET:RESOURCE_CONTEXT),RESOURCETYPE_DISK,0,(path[2]?&host:NULL),&hEnum)==NO_ERROR){
      NETRESOURCE resource[16384/sizeof(NETRESOURCE)];
      FXTRACE((1,"Enumerating=%s\n",path.text()));
      while(1){
        nCount=-1;    // Read as many as will fit
        nSize=sizeof(resource);
        if(WNetEnumResource(hEnum,&nCount,resource,&nSize)!=NO_ERROR) break;
        for(i=0; i<nCount; i++){

          // Dump what we found
          FXTRACE((1,"dwScope=%s\n",resource[i].dwScope==RESOURCE_CONNECTED?"RESOURCE_CONNECTED":resource[i].dwScope==RESOURCE_GLOBALNET?"RESOURCE_GLOBALNET":resource[i].dwScope==RESOURCE_REMEMBERED?"RESOURCE_REMEMBERED":"?"));
          FXTRACE((1,"dwType=%s\n",resource[i].dwType==RESOURCETYPE_ANY?"RESOURCETYPE_ANY":resource[i].dwType==RESOURCETYPE_DISK?"RESOURCETYPE_DISK":resource[i].dwType==RESOURCETYPE_PRINT?"RESOURCETYPE_PRINT":"?"));
          FXTRACE((1,"dwDisplayType=%s\n",resource[i].dwDisplayType==RESOURCEDISPLAYTYPE_DOMAIN?"RESOURCEDISPLAYTYPE_DOMAIN":resource[i].dwDisplayType==RESOURCEDISPLAYTYPE_SERVER?"RESOURCEDISPLAYTYPE_SERVER":resource[i].dwDisplayType==RESOURCEDISPLAYTYPE_SHARE?"RESOURCEDISPLAYTYPE_SHARE":resource[i].dwDisplayType==RESOURCEDISPLAYTYPE_GENERIC?"RESOURCEDISPLAYTYPE_GENERIC":resource[i].dwDisplayType==6?"RESOURCEDISPLAYTYPE_NETWORK":resource[i].dwDisplayType==7?"RESOURCEDISPLAYTYPE_ROOT":resource[i].dwDisplayType==8?"RESOURCEDISPLAYTYPE_SHAREADMIN":resource[i].dwDisplayType==9?"RESOURCEDISPLAYTYPE_DIRECTORY":resource[i].dwDisplayType==10?"RESOURCEDISPLAYTYPE_TREE":resource[i].dwDisplayType==11?"RESOURCEDISPLAYTYPE_NDSCONTAINER":"?"));
          FXTRACE((1,"dwUsage=%s\n",resource[i].dwUsage==RESOURCEUSAGE_CONNECTABLE?"RESOURCEUSAGE_CONNECTABLE":resource[i].dwUsage==RESOURCEUSAGE_CONTAINER?"RESOURCEUSAGE_CONTAINER":"?"));
          FXTRACE((1,"lpLocalName=%s\n",resource[i].lpLocalName));
          FXTRACE((1,"lpRemoteName=%s\n",resource[i].lpRemoteName));
          FXTRACE((1,"lpComment=%s\n",resource[i].lpComment));
          FXTRACE((1,"lpProvider=%s\n\n",resource[i].lpProvider));

          // Grow list
          if(count+1>=size){
            size=size?(size<<1):256;
            newlist=new FXString[size];
            for(j=0; j<count; j++) newlist[j]=list[j];
            delete [] filelist;
            filelist=newlist;
            }

          // Add remote name to list
          filelist[count]=resource[i].lpRemoteName;
          count++;
          }
        }
      WNetCloseEnum(hEnum);
      }
    return count;
    }
*/
  // Folding case
  if(flags&LIST_CASEFOLD) matchmode|=FILEMATCH_CASEFOLD;

  // Copy directory name
  pathname=path;
  if(!ISPATHSEP(pathname[pathname.length()-1])) pathname+=PATHSEPSTRING;
  pathname+="*";

  // Open directory
  hFindFile=FindFirstFile(pathname.text(),&ffData);
  if(hFindFile!=INVALID_HANDLE_VALUE){

    // Loop over directory entries
    do{

      // Get name
      name=ffData.cFileName;

      // Filter out files; a bit tricky...
      if(!(ffData.dwFileAttributes&FILE_ATTRIBUTE_DIRECTORY) && ((flags&LIST_NO_FILES) || ((ffData.dwFileAttributes&FILE_ATTRIBUTE_HIDDEN) && !(flags&LIST_HIDDEN_FILES)) || (!(flags&LIST_ALL_FILES) && !match(pattern,name,matchmode)))) continue;

      // Filter out directories; even more tricky!
      if((ffData.dwFileAttributes&FILE_ATTRIBUTE_DIRECTORY) && ((flags&LIST_NO_DIRS) || ((ffData.dwFileAttributes&FILE_ATTRIBUTE_HIDDEN) && !(flags&LIST_HIDDEN_DIRS)) || (name[0]=='.' && (name[1]==0 || (name[1]=='.' && name[2]==0 && (flags&LIST_NO_PARENT)))) || (!(flags&LIST_ALL_DIRS) && !match(pattern,name,matchmode)))) continue;

      // Grow list
      if(count+1>=size){
        size=size?(size<<1):256;
        newlist=new FXString[size];
        for(int f=0; f<count; f++){
          newlist[f].adopt(filelist[f]);
          }
        delete [] filelist;
        filelist=newlist;
        }

      // Add to list
      filelist[count++]=name;
      }
    while(FindNextFile(hFindFile,&ffData));
    FindClose(hFindFile);
    }
  return count;
  }












// List root directories
void FXDirList::listRootItems(){
  FXDirItem      *oldlist,*newlist,**po,**pp,**pn,*item,*link;
  FXIcon         *openicon;
  FXIcon         *closedicon;
  FXFileAssoc    *fileassoc;
  FXString        name;
  DWORD           mask;
  UINT            drivetype;

  // Build new insert-order list
  oldlist=list;
  newlist=NULL;

  // Assemble lists
  po=&oldlist;
  pn=&newlist;

  // Loop over drive letters
  for(mask=GetLogicalDrives(),name="A:\\"; mask; mask>>=1,name[0]++){

    // Skip unavailable drives
    if(!(mask&1)) continue;

    // Find it, and take it out from the old list if found
    for(pp=po; (item=*pp)!=NULL; pp=&item->link){
      if(comparecase(item->label,name)==0){
        *pp=item->link;
        item->link=NULL;
        po=pp;
        goto fnd;
        }
      }

    // Not found; prepend before list
    item=(FXDirItem*)appendItem(NULL,name,open_folder,closed_folder,NULL,TRUE);

    // Next gets hung after this one
fnd:*pn=item;
    pn=&item->link;

    // Its a folder
    item->state=FXDirItem::FOLDER|FXDirItem::HASITEMS;

    // Assume no associations
    fileassoc=NULL;
    drivetype=GetDriveType(name.text());
    switch(drivetype){
      case DRIVE_REMOVABLE:
        if(name[0]=='A' || name[0]=='B'){
          openicon=floppyicon;
          closedicon=floppyicon;
          }
        else{
          openicon=zipdiskicon;
          closedicon=zipdiskicon;
          }
        break;
      case DRIVE_REMOTE:
        openicon=networkicon;
        closedicon=networkicon;
        break;
      case DRIVE_CDROM:
        openicon=cdromicon;
        closedicon=cdromicon;
        break;
      case DRIVE_RAMDISK:
        openicon=open_folder;
        closedicon=closed_folder;
        break;
      case DRIVE_FIXED:
        openicon=harddiskicon;
        closedicon=harddiskicon;
        break;
      case DRIVE_UNKNOWN:
      case DRIVE_NO_ROOT_DIR:
      default:
        openicon=open_folder;
        closedicon=closed_folder;
        break;
      }
    if(associations) fileassoc=associations->findDirBinding(name.text());

    // If association is found, use it
    if(fileassoc){
      if(fileassoc->miniicon) closedicon=fileassoc->miniicon;
      if(fileassoc->miniiconopen) openicon=fileassoc->miniiconopen;
      }

    // Update item information
    item->openIcon=openicon;
    item->closedIcon=closedicon;
    item->size=0L;
    item->assoc=fileassoc;
    item->date=0;

    // Create item
    if(id()) item->create();
    }

  // FIXME what about network neighborhood?

  // Wipe items remaining in list:- they have disappeared!!
  for(item=oldlist; item; item=link){
    link=item->link;
    removeItem(item,TRUE);
    }

  // Remember new list
  list=newlist;
  }






void fxenumWNetContainerResource(NETRESOURCE* netResource,FXObjectListOf<FXStringObject>& netResourceList,DWORD openEnumScope){
//  Comments are mine, unless indicated otherwise. - Daniël Hörchner <dbjh@gmx.net>
//
//  Passing the value RESOURCE_GLOBALNET for openEnumScope will make this
//  function search recursively through the network shares for disk resources.
//  Passing the value RESOURCE_CONTEXT will make this function list the servers
//  in the network neighbourhood.
  DWORD retVal;
  HANDLE handle;

  //  WNetEnumResource() reports containers as being disk resources if
  //  WNetOpenEnum() is called with RESOURCETYPE_DISK. This does not happen if
  //  it's called with RESOURCETYPE_ANY.
  //  BTW RESOURCETYPE_DISK does not guarantee that only disk resources are
  //  reported (I also get a printer container in the list).
  if((retVal=WNetOpenEnum(openEnumScope,RESOURCETYPE_DISK,0,netResource,&handle))!=NO_ERROR){
    // we get here also if access was denied to enumerate the container
    FXTRACE((1,"ERROR: WNetOpenEnum() (%d)\n", retVal));
    return;
    }

  NETRESOURCE *netResources;
  DWORD netResourcesSize=16*1024;               // 16 kB is a good size, according to MSDN
  if ((netResources=(NETRESOURCE *)malloc(netResourcesSize))==NULL){
    FXTRACE((1,"ERROR: Not enough memory for NETRESOURCE structures\n"));
    WNetCloseEnum(handle);
    return;
    }

  do{

    DWORD nEntries=(DWORD)-1;
    retVal=WNetEnumResource(handle,&nEntries,netResources,&netResourcesSize);
    // netResourcesSize is not modified if the buffer is large enough

    if(retVal==ERROR_MORE_DATA){
      // MSDN info is not correct; ERROR_MORE_DATA means the buffer was too
      //  small for a _single_ entry
      // netResourcesSize (now) contains required size
      if((netResources=(NETRESOURCE *)realloc(netResources,netResourcesSize))==NULL){
        FXTRACE((1,"ERROR: Reallocation for NETRESOURCE structures failed\n"));
        WNetCloseEnum(handle);
        return;
        }
      nEntries=(DWORD)-1;
      retVal=WNetEnumResource(handle,&nEntries,netResources,&netResourcesSize);
      }

    if(retVal!=NO_ERROR && retVal!=ERROR_NO_MORE_ITEMS){
      char *str;
      switch (retVal){
        case ERROR_MORE_DATA: str="more data"; break; // shouldn't happen
        case ERROR_INVALID_HANDLE: str="invalid handle"; break;
        case ERROR_NO_NETWORK: str="no network"; break;
        case ERROR_EXTENDED_ERROR: str="extended error"; break;
        default: str="unknown";
        }
      FXTRACE((1,"ERROR: Network enum error: %s (%d)\n",str,retVal));
      free(netResources);
      WNetCloseEnum(handle);
      return;
      }

    for(DWORD n=0; n < nEntries; n++){
      bool isContainer=FALSE;

      // if RESOURCE_CONTEXT was passed to WNetOpenEnum(), dwScope will be
      //  RESOURCE_GLOBALNET
      if(netResources[n].dwScope==RESOURCE_GLOBALNET && (netResources[n].dwUsage&RESOURCEUSAGE_CONTAINER)){
        isContainer=TRUE;

        //  If RESOURCE_CONTEXT was passed to WNetOpenEnum(), the first entry is
        //  a "self reference". For example, starting from the network root one
        //  can find a container entry with lpComment "Entire Network"
        //  (lpRemoteName and lpLocalName are NULL for me). This container
        //  contains an entry with the same properties. In order to avoid getting
        //  into an infinite loop, we must handle this case.
        //  However, trying to enumerate normal containers while RESOURCE_CONTEXT
        //  was used causes WNetOpenEnum() to return ERROR_INVALID_PARAMETER.
        if(netResources[n].lpRemoteName){
          netResourceList.append(new FXStringObject(((FXString)netResources[n].lpRemoteName)+PATHSEP));
          if(openEnumScope!=RESOURCE_CONTEXT){
            fxenumWNetContainerResource(&netResources[n],netResourceList,openEnumScope);
            }
          }
        }

      // Using the variable isContainer is necessary if WNetOpenEnum() is
      //  called with RESOURCETYPE_DISK. See above.
      if(netResources[n].dwType==RESOURCETYPE_DISK && !isContainer){
        netResourceList.append(new FXStringObject(((FXString)netResources[n].lpRemoteName)+PATHSEP));
        }
      }
    }
  while(retVal!=ERROR_NO_MORE_ITEMS);

  free(netResources);
  WNetCloseEnum(handle);                        // it makes no sense to check for NO_ERROR
  }






  // Folding case
  if(flags&LIST_CASEFOLD) matchmode|=FILEMATCH_CASEFOLD;

  if(FXFile::isShareServer(path)){
    pathname=path;
    // pathname must not have an ending back slash or else
    //  fxenumWNetContainerResource() (WNetOpenEnum()) will fail
    if(ISPATHSEP(pathname[pathname.length()-1])) pathname.trunc(pathname.length()-1);
    NETRESOURCE netResource;
    memset(&netResource,0,sizeof(NETRESOURCE));
    netResource.lpRemoteName=(char *)pathname.text();

    FXObjectListOf<FXStringObject> netResourceList;
    // a share server can only provide shares, which are similar to directories
    if(!(flags&LIST_NO_DIRS))
      fxenumWNetContainerResource(&netResource,netResourceList,RESOURCE_GLOBALNET);

    for(int n=0; n < netResourceList.no(); n++){
      // Get name
      name=*netResourceList[n];
      if(ISPATHSEP(name[name.length()-1])) name.trunc(name.length()-1);
      name=FXFile::name(name);

      // Filter out directories
      if(!(flags&LIST_ALL_DIRS) && !match(pattern,name,matchmode)) continue;

      // Grow list
      if(count+1>=size){
        size=size?(size<<1):256;
        newlist=new FXString[size];
        for(int f=0; f<count; f++) newlist[f]=filelist[f];
        delete [] filelist;
        filelist=newlist;
        }

      // Add to list
      filelist[count++]=name;
      }
    for(int n=0; n < netResourceList.no(); n++) delete netResourceList[n];
    netResourceList.clear();
    }
  else{
    // Copy directory name
    pathname=path;
    if(!ISPATHSEP(pathname[pathname.length()-1])) pathname+=PATHSEPSTRING;
    pathname+="*";

    // Open directory
    hFindFile=FindFirstFile(pathname.text(),&ffData);
    if(hFindFile!=INVALID_HANDLE_VALUE){

      // Loop over directory entries
      do{

        // Get name
        name=ffData.cFileName;

        // Filter out files; a bit tricky...
        if(!(ffData.dwFileAttributes&FILE_ATTRIBUTE_DIRECTORY) && ((flags&LIST_NO_FILES) || ((ffData.dwFileAttributes&FILE_ATTRIBUTE_HIDDEN) && !(flags&LIST_HIDDEN_FILES)) || (!(flags&LIST_ALL_FILES) && !match(pattern,name,matchmode)))) continue;

        // Filter out directories; even more tricky!
        if((ffData.dwFileAttributes&FILE_ATTRIBUTE_DIRECTORY) && ((flags&LIST_NO_DIRS) || ((ffData.dwFileAttributes&FILE_ATTRIBUTE_HIDDEN) && !(flags&LIST_HIDDEN_DIRS)) || (name[0]=='.' && (name[1]==0 || (name[1]=='.' && name[2]==0 && (flags&LIST_NO_PARENT)))) || (!(flags&LIST_ALL_DIRS) && !match(pattern,name,matchmode)))) continue;

        // Grow list
        if(count+1>=size){
          size=size?(size<<1):256;
          newlist=new FXString[size];
          for(int f=0; f<count; f++) newlist[f]=filelist[f];
          delete [] filelist;
          filelist=newlist;
          }

        // Add to list
        filelist[count++]=name;
        }
      while(FindNextFile(hFindFile,&ffData));
      FindClose(hFindFile);
      }
    }
  return count;
  }


// Hack code below for testing if volume is mounted

// #if defined (HKS_NT)
//
// static int check_nfs (const char* name)
// {
// char drive[8];
//
// char* cp = strchr (name, ':');
// if (cp)
// {
// strncpy (drive, name, cp - name);
// drive[cp - name] = '\0';
// }
// else
// {
// drive[0] = 'A' + _getdrive() - 1;
// drive[1] = '\0';
// }
//
// strcat (drive, ":\\");
//
// return GetDriveType(drive) == DRIVE_REMOTE;
// }
//
// #elif defined(LINUX)
//
// static int check_nfs (int fd)
// {
// struct statfs statbuf;
// if (fstatfs(fd,&statbuf) < 0)
// {
// RFM_RAISE_SYSTEM_ERROR("statfs");
// return 0;
// }
// if (statbuf.f_type == NFS_SUPER_MAGIC)
// return 1;
// else
// return 0;
// }
//
// #else
//
// static int check_nfs (int fd)
// {
//
// struct statvfs statbuf;
//
// if (fstatvfs (fd, &statbuf) < 0)
// {
// RFM_RAISE_SYSTEM_ERROR ("fstatvfs");
// }
// return strncmp (statbuf.f_basetype, "nfs", 3) == 0 || strncmp
// (statbuf.f_basetype, "NFS", 3) == 0;
// }
// #endif



#endif
