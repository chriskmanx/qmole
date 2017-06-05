#ifndef MSNameSpaceHEADER
#define MSNameSpaceHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSEnum.H>

#ifdef MS_MULTI_THREAD
#include <MSTypes/MSMutex.H>
#endif

class MSHashTable;
class MSNameSpaceHashTable;
class MSHashEntry;
#if HAVE_IOSTREAM
#include <iostream>
// #include <fstream>
using namespace std;
#else
class ostream;
#endif


class MSTypesExport MSNameSpace
{
public:
  MSNameSpace(unsigned size_=128);
  ~MSNameSpace(void);

  static MSAtom nullAtom(void);
  MSAtom intern(const char *symbol_);
  MSAtom lookup(const char *symbol_) const;
  const char *atomName(MSAtom atom_) const;
  unsigned averageChainLengthThreshold(void) const;

  void averageChainLengthThreshold(unsigned);

  friend MSTypesExport ostream& operator<<(ostream& aStream_,const MSNameSpace& aNameSpace_);

private:
  MSNameSpaceHashTable  *_symbolHashTable;
  char                 **_stringTable;
  unsigned               _count;
  unsigned               _size;
  unsigned               _averageChainLengthThreshold;
  MSAtom                 _nextAtomValue;

#ifdef MS_MULTI_THREAD    
  MSMutex                _mutex;      // namespace lock
#endif

  MSNameSpaceHashTable *symbolHashTable(void) const;
  MSAtom nextAtomValue(void) const;
  unsigned size(void) const;
  unsigned count(void) const;

  void reserve(unsigned size_);
  MSHashEntry *addSymbol(const char *,MSAtom);
};

inline MSAtom MSNameSpace::nullAtom(void)
{ return MSNullAtom; }

inline MSNameSpaceHashTable *MSNameSpace::symbolHashTable(void) const 
{ return _symbolHashTable; }
inline MSAtom MSNameSpace::nextAtomValue(void) const 
{ return _nextAtomValue; }
inline unsigned MSNameSpace::size(void) const 
{ return _size; }
inline unsigned MSNameSpace::count(void) const 
{ return _count; }

inline unsigned MSNameSpace::averageChainLengthThreshold(void) const
{ return _averageChainLengthThreshold; }
inline void MSNameSpace::averageChainLengthThreshold(unsigned avg_)
{ if (avg_>0) _averageChainLengthThreshold=avg_; }

inline const char *MSNameSpace::atomName(MSAtom atom_) const
{ return (const char *)_stringTable[atom_]; }

#endif





