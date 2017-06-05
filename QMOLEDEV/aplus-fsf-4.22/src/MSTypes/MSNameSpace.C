///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSHashTable.H>
#include <MSTypes/MSNameSpace.H>
#include <MSTypes/MSMessageLog.H>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif

static const unsigned MSSymbolHashTableChainLengthThreshold=4;
static char *nullString="";

class MSNameSpaceHashTable : public MSHashTable
{
public:
  MSNameSpaceHashTable(unsigned size_);
  MSHashEntry *addSymbol(const char *,MSAtom);
};

MSNameSpaceHashTable::MSNameSpaceHashTable(unsigned size_) :
MSHashTable(size_)
{
  _notFound=MSNullAtom;
}

MSHashEntry *MSNameSpaceHashTable::addSymbol(const char *pSymbol_,MSAtom atomValue_)
{
  if (size()==0)
   {
   MSMessageLog::errorMessage("MSHashTable: add failed - hash table size==0\n");
   return 0;
   }
  unsigned whichBucket=hash(pSymbol_);  
  MSHashEntry *entry=searchBucketFor(bucket(whichBucket),pSymbol_);
  if (entry!=0)
   {
     MSMessageLog::errorMessage("MSHashTable: add failed - key already in table: %s\n",pSymbol_); 
     return 0;
   }
  entry=addElement(pSymbol_,whichBucket);
  entry->value((void *)atomValue_);
  return entry;
}

MSNameSpace::MSNameSpace(unsigned size_) 
{
  _size=size_;
  _symbolHashTable=new MSNameSpaceHashTable(size());
  _stringTable=new char*[size()];
  _averageChainLengthThreshold=MSSymbolHashTableChainLengthThreshold;
  for (unsigned i=0;i<size();i++) _stringTable[i]=0;
  _stringTable[MSNullAtom]=nullString;
  _count=1;
  _nextAtomValue=1;
}

MSNameSpace::~MSNameSpace(void) 
{
  for (unsigned i=0;i<size();i++) _stringTable[i]=0;
  delete [] _stringTable;
  delete _symbolHashTable;
}

MSAtom MSNameSpace::lookup(const char *pSymbol_) const
{ return (MSAtom)_symbolHashTable->lookup(pSymbol_); } 

MSHashEntry *MSNameSpace::addSymbol(const char *pSymbol_,MSAtom atomValue_)
{ return _symbolHashTable->addSymbol(pSymbol_,atomValue_); }

// make sure there are at least size_+1 elements in the array
void MSNameSpace::reserve(unsigned size_)
{
  unsigned n=size_+1;
  if (size()<n)
   {
     unsigned newSize=(size()==0)?n<<1:size()<<1;
     unsigned i;
     char **array=new char*[newSize];
     for (i=0;i<size();i++) 
      {
        array[i]=_stringTable[i];
        _stringTable[i]=0;
      }
     for (i=size();i<newSize;i++) array[i]=0;
     delete [] _stringTable;
     _stringTable=array;
     _size=newSize;
     if (_symbolHashTable->averageChainLength()>averageChainLengthThreshold()) 
      { 
	_symbolHashTable->resize(_symbolHashTable->size()<<1);
      }
   }
}

MSAtom MSNameSpace::intern(const char *symbol_)
{
  MSAtom r=MSNullAtom;
  if (symbol_!=0)
   {
     if ((r=(MSAtom)_symbolHashTable->lookup(symbol_))==MSNullAtom)
      { 
#ifdef MS_MULTI_THREAD    
        MSGuard guard(_mutex);
        //double check again under guard to prevent possible double addition
        //if 2 threads trying to intern the same thing at the same time.
        if ((r=(MSAtom)_symbolHashTable->lookup(symbol_))==MSNullAtom)
         {
#endif
           r=nextAtomValue();
           MSHashEntry *entry=addSymbol(symbol_,_nextAtomValue);
           if (entry!=0)
            {
              _nextAtomValue++;
              reserve(count());
              _stringTable[count()]=entry->stringKey();
              _count++;
            }
         }
#ifdef MS_MULTI_THREAD
      }
#endif
   }
  return r;
}

ostream& operator<<(ostream& aStream_,const MSNameSpace& aNameSpace_)
{
  aStream_<<*(aNameSpace_._symbolHashTable)<<endl;
  unsigned n=aNameSpace_.count();
  for (unsigned i=0;i<n;i++)
   {
     aStream_<<"Atom: "<<i<<"\t\tSymbol: "<<aNameSpace_.atomName((MSAtom)i)<<endl;
   }
  return aStream_;
}
