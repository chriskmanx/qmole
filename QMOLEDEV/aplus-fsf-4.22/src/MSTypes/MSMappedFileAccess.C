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
#include <errno.h>
#include <fcntl.h>
#include <MSTypes/MSMappedFileAccess.H>
#include <MSTypes/MSTypeData.H>
#include <MSTypes/MSMMap.H>
#include <MSTypes/MSMessageLog.H>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#if defined(MS_NEED_FSYNC_DECLARATION)
extern "C" int fsync(int);
#endif

#if defined(MS_NEED_MUNMAP_DECLARATION) 
extern "C" int munmap(caddr_t,int);
#endif

template <class Type>
inline void getArray(MSMMap& aMMap, const Type*& t_)
{ t_=0; }

inline void getArray(MSMMap& aMMap, const int*& t_)
{ t_=aMMap.getIntArray(); }
inline void getArray(MSMMap& aMMap, const char*& t_)
{ t_=aMMap.getCharArray(); }
inline void getArray(MSMMap& aMMap, const double*& t_)
{ t_=aMMap.getDoubleArray(); }
inline void getArray(MSMMap& aMMap, const long*& t_)
{ t_=aMMap.getLongArray(); }

template <class Type>
static MSBoolean beamInVector(MSTypeVector<Type>& vector_, const char *fileName_, MSA::AType type_)
{
  int count=0;
  MSTypeData<Type,MSAllocator<Type> > *d=0;
  MSMMap aMMap;

  if (aMMap.beamIn(fileName_)==MSTrue)
   {
    if (aMMap.type()==type_)
     {
       if (aMMap.rank()==1)
        {
          count=(unsigned)aMMap.count();
          d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(count);
          Type *dp=d->elements();
	  const Type *sp;
	  getArray(aMMap,sp);
	  memcpy(dp,sp,count*sizeof(Type));
        }
       else MSMessageLog::errorMessage("Unable to map data: %s - rank != 1.\n",fileName_);
     }
    else MSMessageLog::errorMessage("Unable to map data: %s - incorrect type.\n",fileName_);
   }
  if(d!=0&&count>0)
   {
     vector_=MSTypeVector<Type>(d,count);
     return MSTrue;
   }
  else
   {
     vector_=MSTypeVector<Type>();
     return MSFalse;
   }
}

template <class Type>
static MSBoolean beamInMatrix(MSTypeMatrix<Type>& matrix_, const char *fileName_, MSA::AType type_)
{
  int count=0, rows=0, columns=0;
  MSTypeData<Type,MSAllocator<Type> > *d=0;
  MSMMap aMMap;

  if (aMMap.beamIn(fileName_)==MSTrue)
   {
    if (aMMap.type()==type_)
     {
       if (aMMap.rank()==2)
        {
          count=aMMap.count();
	  rows=aMMap.aplusData()->d[0];
	  columns=aMMap.aplusData()->d[1];
          d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(count);
          Type *dp=d->elements();
	  const Type *sp;
	  getArray(aMMap,sp);
 	  memcpy(dp,sp,count*sizeof(Type));
        }
       else MSMessageLog::errorMessage("Unable to map data: %s - rank != 2.\n",fileName_);
     }
    else MSMessageLog::errorMessage("Unable to map data: %s - incorrect type.\n",fileName_);
   }
  if(d!=0&&count>0)
   {
     matrix_=MSTypeMatrix<Type>(d,rows,columns);
     return MSTrue;
   }
  else
   {
     matrix_=MSTypeMatrix<Type>();
     return MSFalse;
   }
}

template <class Type>
static void writeOut(const char *fileName_,const Type *data_, MSA::AType type_,
	      unsigned int rows_,unsigned int columns_,int rank_) {
  int fd;

  if (fileName_!=0)
   {
     fd = open(fileName_, 1);
     if (fd != -1)
      {
        (void)lseek(fd,0,SEEK_SET);
         MSAStruct a;

	a.i=(long)rows_*columns_;
	a.c=0;
	a.t=(long)type_;
	a.r=rank_;
	a.n=a.i;
	a.d[0]=rows_;
	a.d[1]=columns_;

        for (int i=2;i<MSA::MAXRANK;i++) a.d[i]=0;

        (void)write(fd,(char *)&a,MSA::AHEADER);

        unsigned n=(sizeof(Type)*(long)a.n);
        char *d=(char *)data_;

        int wc=write(fd,d,n);

	d+=wc,n-=wc;

        for (;wc!=-1&&n>0;d+=wc,n-=wc) wc=write(fd,d,n);

        fsync(fd);
        close(fd);
      }
     else MSMessageLog::errorMessage("Unable to map data: %s - cannot open file\n",fileName_);
   }
}


MSBoolean MSMappedFileAccess::beamIn(MSCharVector &vector_,const char *fileName_)
{
  return beamInVector(vector_,fileName_,MSA::CHARACTERTYPE);
}

MSBoolean MSMappedFileAccess::beamIn(MSIntVector &vector_,const char *fileName_)
{
  return beamInVector(vector_,fileName_,MSA::INTEGERTYPE);
}

MSBoolean MSMappedFileAccess::beamIn(MSLongVector &vector_,const char *fileName_)
{
  return beamInVector(vector_,fileName_,MSA::INTEGERTYPE);
}

MSBoolean MSMappedFileAccess::beamIn(MSFloatVector &vector_,const char *fileName_)
{
  return beamInVector(vector_,fileName_,MSA::FLOATTYPE);
}

MSBoolean MSMappedFileAccess::beamIn(MSCharMatrix &matrix_,const char *fileName_)
{
  return beamInMatrix(matrix_,fileName_,MSA::CHARACTERTYPE);
}

MSBoolean MSMappedFileAccess::beamIn(MSIntMatrix &matrix_,const char *fileName_)
{
  return beamInMatrix(matrix_,fileName_,MSA::INTEGERTYPE);
}

MSBoolean MSMappedFileAccess::beamIn(MSFloatMatrix &matrix_,const char *fileName_)
{ 
  return beamInMatrix(matrix_,fileName_,MSA::FLOATTYPE);
}

MSBoolean MSMappedFileAccess::beamIn(MSLongMatrix &matrix_,const char *fileName_)
{
  return beamInMatrix(matrix_,fileName_,MSA::INTEGERTYPE);
}

void MSMappedFileAccess::beamOut(const char *fileName_,const MSCharVector &vector_)
{
  writeOut(fileName_,vector_.data(),MSA::CHARACTERTYPE,vector_.length(),1U,1); 
}

void MSMappedFileAccess::beamOut(const char *fileName_,const MSIntVector &vector_)
{
  writeOut(fileName_,vector_.data(),MSA::INTEGERTYPE,vector_.length(),1U,1);
}

void MSMappedFileAccess::beamOut(const char *fileName_,const MSLongVector &vector_)
{
  writeOut(fileName_,vector_.data(),MSA::INTEGERTYPE,vector_.length(),1U,1); 
}

void MSMappedFileAccess::beamOut(const char *fileName_,const MSFloatVector &vector_)
{
  writeOut(fileName_,vector_.data(),MSA::FLOATTYPE,vector_.length(),1U,1); 
}

void MSMappedFileAccess::beamOut(const char *fileName_,const MSCharMatrix &matrix_)
{
  writeOut(fileName_,matrix_.data(),MSA::CHARACTERTYPE,matrix_.rows(),matrix_.columns(),2); 
}

void MSMappedFileAccess::beamOut(const char *fileName_,const MSIntMatrix &matrix_)
{
  writeOut(fileName_,matrix_.data(),MSA::INTEGERTYPE,matrix_.rows(),matrix_.columns(),2); 
}

void MSMappedFileAccess::beamOut(const char *fileName_,const MSLongMatrix &matrix_)
{
  writeOut(fileName_,matrix_.data(),MSA::INTEGERTYPE,matrix_.rows(),matrix_.columns(),2); 
}

void MSMappedFileAccess::beamOut(const char *fileName_,const MSFloatMatrix &matrix_)
{
  writeOut(fileName_,matrix_.data(),MSA::FLOATTYPE,matrix_.rows(),matrix_.columns(),2);
}
