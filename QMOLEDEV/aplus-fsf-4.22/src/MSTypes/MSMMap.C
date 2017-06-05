///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#if HAVE_FSTREAM
#include <fstream>
#else
#include <fstream.h>
#endif
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <errno.h>
#include <MSTypes/MSMMap.H>
#include <MSTypes/MSMessageLog.H>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif


#if defined(MS_NEED_FSYNC_DECLARATION)
extern "C" int fsync(int);
#endif

MSMMap::MSMMap(void) :
_aplusData(0),_mappedData(0),_fileSize(0)
{
}

MSMMap::MSMMap(const char *fileName_) :
_aplusData(0),_mappedData(0),_fileSize(0)
{
  beamIn(fileName_);
}

MSMMap::~MSMMap(void)
{ 
  if (_mappedData!=0) 
    {
      munmap(_mappedData,_fileSize);
      _mappedData=0;
      _aplusData=0;
    }
  else if(_aplusData!=0)
    {
      MSA::dc(_aplusData);
      _aplusData=0;
    }
}

MSBoolean MSMMap::beamIn(const char *fileName_) {
  int fd;
  if (fileName_!=0)
   {
     fstream mappedFile;
     fd = open(fileName_,0);
     if (fd != -1)
      {
        lseek(fd,0,SEEK_SET);
	_fileSize=(int)lseek(fd,0,SEEK_END);
        char *mfp=(char *)mmap(0,_fileSize,PROT_READ,MAP_SHARED,fd,0);
	if (mfp!=((void *)-1)) //64 bit clean check
	 {
           _mappedData=mfp;
	   _aplusData=(MSAStruct *)mfp;
	   close(fd);
	   return checkEndiness();
	 }
        else MSMessageLog::errorMessage("MSMMap: Unable to map data: %s - errno: %d\n",fileName_,errno);
        close(fd);
      }
     else MSMessageLog::errorMessage("MSMMap: Unable to map data: %s - cannot open file\n",fileName_);
   }
  return MSFalse;
}

int MSMMap::isWrongEndian(MSAStruct *aobj)
{
  return (aobj->r)?(!(1<=aobj->r&&MSA::MAXRANK>=aobj->r)):1!=aobj->n;
}

MSBoolean MSMMap::checkEndiness(void)
{
  static MSAStruct tempAobj;
  if(isWrongEndian(_aplusData))
    {
      MSMessageLog::warningMessage("MSMMap warning: Mapped file is wrong endian. Making local copy\n");
      MSAStruct *z=0;
      long n,t;
      MSA::ndnicopy((long *)_aplusData,(long *)&tempAobj,MSA::AHEADER/sizeof(long));
      z=MSA::gd(tempAobj.t,&tempAobj);
      n=z->n; t=z->t;
      switch(t)
	{
	case MSA::INTEGERTYPE:
	  MSA::ndnicopy((long *)_aplusData->p,(long *)z->p,n);
	  break;
	case MSA::FLOATTYPE:
	  MSA::ndnfcopy((double *)_aplusData->p,(double *)z->p,n);
	  break;
	case MSA::CHARACTERTYPE:
	  memcpy(z->p,_aplusData->p,n+1);
	  break;
	default:
	  MSMessageLog::errorMessage("MSMMap error: Endian conversion encountered bad type:%d\n",t);
	  z=0;
	  break;
	}
      munmap(_mappedData,_fileSize);
     _mappedData=0;
     _aplusData=z;
   }
  return (_aplusData==0)?MSFalse:MSTrue;
}

