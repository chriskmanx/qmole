#ifndef MSHashTableHEADER
#define MSHashTableHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSEnum.H>

#if HAVE_IOSTREAM
#include <iostream>
// #include <fstream>
using namespace std;
#else
class ostream;
#endif


class MSTypesExport MSHashEntry
{
friend class MSHashTable;
private:
  unsigned long  _key;
  char          *_stringKey;
  void          *_value;
  MSHashEntry   *_next;
  MSHashEntry   *_prev;

public:
  MSHashEntry(void);
  MSHashEntry(unsigned long key_); 
  MSHashEntry(const char *key_);
  ~MSHashEntry(void);

  unsigned long key(void) const { return _key; }
  char *stringKey(void)   const { return _stringKey; }
  void *value(void)       const { return _value; }
  MSHashEntry *next(void) const { return _next; }
  MSHashEntry *prev(void) const { return _prev; }

  void value(void *value_)      { _value=value_; }
  void next(MSHashEntry *next_) { _next=next_; }
  void prev(MSHashEntry *prev_) { _prev=prev_; }
};

class MSTypesExport MSHashTable
{
public:
  MSHashTable(unsigned size_=0);
  virtual ~MSHashTable(void);

  unsigned size(void) const;
  unsigned long notFound(void) const;

  void init(unsigned size_);
  void resize(unsigned size_);
  void notFound(unsigned long value_);

  MSBoolean add(unsigned long key_,void *value_);
  MSBoolean add(const char *key_,void *value_);
  MSBoolean remove(unsigned long key_);
  MSBoolean remove(const char *key_);
  void *lookup(unsigned long key_) const;  // return _notFound when it fails
  void *lookup(const char *key_) const;    // return _notFound when it fails

  virtual void removeAll(void);  

  unsigned averageChainLength(void) const;
  unsigned maximumChainLength(void) const;
  unsigned chainLength(unsigned index_) const;
  ostream& printStringKeys(ostream& aStream) const;
  ostream& printChainLengths(ostream& aStream) const;
  ostream& printHashStatistics(ostream& aStream) const;

  friend MSTypesExport ostream& operator<<(ostream& aStream,const MSHashTable& aHashTable);

protected:
  unsigned        _size;  
  MSHashEntry   **_bucket;
  unsigned long   _notFound;
   
  MSHashEntry **bucket(void)           const { return _bucket; }
  MSHashEntry *bucket(unsigned index_) const { return _bucket[index_]; }  

  unsigned hash(unsigned long key_) const;
  unsigned hash(const char *key_) const;

  void bucket(unsigned index_,MSHashEntry *entry_);
  void addEntry(MSHashEntry *);

  MSHashEntry *addElement(unsigned long key_,unsigned whichBucket_);
  MSHashEntry *addElement(const char *key_,unsigned whichBucket_);
  MSHashEntry *searchBucketFor(MSHashEntry *entry_,unsigned long key_) const;
  MSHashEntry *searchBucketFor(MSHashEntry *entry_,const char *key_) const;
};

inline unsigned MSHashTable::size(void) const 
{ return _size; }
inline unsigned long MSHashTable::notFound(void) const 
{ return _notFound; }

inline void MSHashTable::notFound(unsigned long value_) 
{ _notFound=value_; }
inline void MSHashTable::bucket(unsigned index_,MSHashEntry *entry_) 
{ _bucket[index_]=entry_; }

//  this represents a special case of a MSHashTable 
//  the keyed data is a char string that needs to be freed when the entry
//  is deleted.
//  it will be used for things like binding pixel values to strings or
//  font ids to strings.
class MSTypesExport MSStringHashTable : public MSHashTable
{
public:
  MSStringHashTable(unsigned size_=0);
  ~MSStringHashTable(void);
};

#endif
