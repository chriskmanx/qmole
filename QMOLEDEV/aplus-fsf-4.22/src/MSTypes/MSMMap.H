#ifndef MSMMapHEADER
#define MSMMapHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSEnum.H>
#include <MSTypes/MSA.H>

class MSTypesExport MSMMap
{
public:
  MSMMap(void);
  MSMMap(const char *fileName_);
  ~MSMMap(void);

  MSBoolean beamIn(const char *fileName_);
  MSAStruct *aplusData(void) const {return _aplusData;}
  
  MSBoolean isValid(void) const {return (_aplusData!=0)?MSTrue:MSFalse;}
  long type(void)         const {return (_aplusData!=0)?_aplusData->t:MSA::UTYPE;}
  long rank(void)         const {return (_aplusData!=0)?_aplusData->r:0;}
  long size(void)         const {return (_aplusData!=0)?(MSA::dataSize(_aplusData->t,_aplusData->n)):0;}
  long count(void)        const {return (_aplusData!=0)?_aplusData->n:0;}

  inline int getIntAt(unsigned)       const;
  inline long getLongAt(unsigned)       const;
  inline double getDoubleAt(unsigned) const;
  inline char getCharAt(unsigned) const;

  inline const int    *getIntArray(void) const;
  inline const long   *getLongArray(void) const;
  inline const double *getDoubleArray(void) const;
  inline const char   *getCharArray(void) const;

protected:
  MSAStruct *_aplusData;
  char      *_mappedData;
  long       _fileSize;
  

  MSBoolean checkEndiness(void);
  int isWrongEndian(MSAStruct*);
  
};
inline const int* MSMMap::getIntArray(void) const
{
  if (_aplusData!=0) return (int *)(_aplusData->p);
  else return 0;
}

inline const long* MSMMap::getLongArray(void) const
{
  if (_aplusData!=0) return (long *)(_aplusData->p);
  else return 0;
}
inline const double* MSMMap::getDoubleArray(void) const
{
  if (_aplusData!=0) return (double *)(_aplusData->p);
  else return 0;
}  
inline const char* MSMMap::getCharArray(void) const
{
  if (_aplusData!=0) return (char *)(_aplusData->p);
  else return 0;
} 

inline int MSMMap::getIntAt(unsigned index_) const
{
  if (_aplusData!=0&&index_<_aplusData->n)
   {
     int *pData=(int *)(_aplusData->p);
     return pData[index_];
   }
  return 0;
}

inline long MSMMap::getLongAt(unsigned index_) const
{
  if (_aplusData!=0&&index_<_aplusData->n)
   {
     long *pData=(long *)(_aplusData->p);
     return pData[index_];
   }
  return 0;
}

inline double MSMMap::getDoubleAt(unsigned index_) const
{
  if (_aplusData!=0&&index_<_aplusData->n)
   {
     double *pData=(double *)(_aplusData->p);
     return pData[index_];
   }
  return 0;
}

inline char MSMMap::getCharAt(unsigned index_) const
{
 if (_aplusData!=0&&index_<_aplusData->n)
  {
     char *pData=(char *)(_aplusData->p);
     return pData[index_];
   }
  return 0;
}
  
#endif
