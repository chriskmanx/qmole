///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <string.h>
#include <stdlib.h>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <MSTypes/MSHashTable.H>
#include <MSTypes/MSMessageLog.H>

static const unsigned long RESOURCE_ID_MASK=0xfffff;  // used in unsigned long key hashing
static const unsigned long DefaultNotFound=0x0;       // default NotFound value

static char *stringDuplicate(const char *src_)
{
  char *dest=0;
  if (src_!=0)
   {
     unsigned len=strlen(src_);
     dest=new char[len+1];
     for (unsigned i=0;i<len;i++) dest[i]=src_[i];
     dest[len]='\0';
   }
  return dest;
}

// helps us guarantee that the hash table size is a power of 2
// thus, we can value&(size-1) for the hash value, which is
// faster than value%size
static unsigned computeSize(unsigned requestedSize_)
{
  unsigned newSize=1;
  while (newSize<requestedSize_) newSize<<=1; 
  return newSize;
}

MSHashEntry::MSHashEntry(void)
{ _stringKey=0,_key=0,_value=0,_next=0,_prev=0; }
MSHashEntry::MSHashEntry(const char *key_)
{ _stringKey=stringDuplicate(key_),_key=0,_value=0,_next=0,_prev=0; }
MSHashEntry::MSHashEntry(unsigned long key_) 
{ _key=key_,_stringKey=0,_value=0,_next=0,_prev=0; }

MSHashEntry::~MSHashEntry(void)
{
  if (next()!=0) next()->_prev=prev();
  if (prev()!=0) prev()->_next=next();
  if (stringKey()!=0) delete [] _stringKey; 
  _stringKey=0,_value=0,_key=0,_next=0,_prev=0;
}

MSHashTable::MSHashTable(unsigned size_) 
{ 
  _size=size_,_bucket=0,_notFound=DefaultNotFound;
  if (size()>0) init(size_); 
}

MSHashTable::~MSHashTable(void)
{ removeAll(); }

void MSHashTable::removeAll(void)
{
  for (unsigned i=0;i<size();i++)
   {
     MSHashEntry *entry=bucket(i);
     while (entry!=0)
      {
	_bucket[i]=entry->next();
	delete entry;
	entry=bucket(i);
      }
     _bucket[i]=0;
   }
  if (_bucket!=0) delete [] _bucket;
  _bucket=0;
  _size=0;
}

void MSHashTable::init(unsigned size_)
{
  if (bucket()==0)
   {
     _size=computeSize(size_);
     _bucket=new MSHashEntry*[size()];
     unsigned i=size();
     MSHashEntry **p=bucket();
     while(i--) *p++=0;
   }
  else resize(size_);
}

void MSHashTable::resize(unsigned size_)
{
  MSHashEntry **oldBuckets=bucket();
  unsigned oldSize=size();
  
  _size=computeSize(size_);
  _bucket=new MSHashEntry*[size()];
  unsigned i=size();
  MSHashEntry **p=bucket();
  while(i--) *p++=0;

  if (oldBuckets!=0)
   {
     for (unsigned j=0;j<oldSize;j++)
      {
        // we want to add them in reverse order of the chains, i.e. add them in the order that they
        // were originally added - latest additions are at the top of the bucket chain
	MSHashEntry *entry=oldBuckets[j];
	MSHashEntry *prev;
	while (entry!=0&&entry->next()!=0) entry=entry->next(); 
	while (entry!=0)
	 {
	   prev=entry->prev();
           entry->prev(0),entry->next(0);
	   addEntry(entry); // rehash and add to the new table
	   entry=prev;
	 }
	oldBuckets[j]=0;
      }
     delete [] oldBuckets;  
   }
}

// this hash routine comes from the mit sample X server
// and is particularly well suited to hashing on memory
// addresses
unsigned MSHashTable::hash(unsigned long key_) const
{
  key_&=RESOURCE_ID_MASK;
  return unsigned((key_^(key_>>11))&(size()-1));
}

void *MSHashTable::lookup(unsigned long key_) const
{
  MSHashEntry *entry=searchBucketFor(bucket(hash(key_)),key_);
  if (entry==0) return (void *)notFound();
  else return entry->value();
}

MSHashEntry *MSHashTable::searchBucketFor(MSHashEntry *entry_,unsigned long key_) const
{
  while (entry_!=0)
   {
     if (entry_->key()==key_) return entry_;
     entry_=entry_->next();
   }
  return entry_;
}

MSHashEntry *MSHashTable::addElement(unsigned long key_,unsigned whichBucket_)
{
  MSHashEntry *entry=new MSHashEntry(key_);
  entry->next(bucket(whichBucket_));
  if (bucket(whichBucket_)!=0) bucket(whichBucket_)->prev(entry);
  bucket(whichBucket_,entry);
  return entry;
}

MSBoolean MSHashTable::add(unsigned long key_,void *value_)
{
  if (size()==0)
   {
     MSMessageLog::errorMessage("MSHashTable: add failed - hash table size==0\n");
     return MSFalse;
   }
  unsigned whichBucket=hash(key_);  
  MSHashEntry *entry=searchBucketFor(bucket(whichBucket),key_);
  if (entry!=0) 
   {
     MSMessageLog::errorMessage("MSHashTable: add failed - key already in table: %d\n",key_); 
     return MSFalse;
   }
  entry=addElement(key_,whichBucket);
  entry->value(value_);
  return MSTrue;
}

MSBoolean MSHashTable::remove(unsigned long key_)
{
  unsigned whichBucket=hash(key_);  
  MSHashEntry *entry=searchBucketFor(bucket(whichBucket),key_);
  if (entry!=0)
   {
     if (bucket(whichBucket)==entry) bucket(whichBucket,entry->next());
     delete entry;
     return MSTrue;
   }
  return MSFalse;
}

// this works because the MSHashTable forces size to be a power
// of two, otherwise we would have to return value%size(), which
// is less efficient than value&(size()-1)
unsigned MSHashTable::hash(const char *key_) const
{
  unsigned long h=0;
  unsigned long c;
  while ((c=(unsigned long)*key_++)!=0) h+=(h<<5)+c;
  return unsigned(h&(size()-1));
}

void *MSHashTable::lookup(const char *key_) const
{
  MSHashEntry *entry=(key_!=0)?searchBucketFor(bucket(hash(key_)),key_):0;
  if (entry==0) return (void *)notFound();
  else return entry->value();
}

MSHashEntry *MSHashTable::searchBucketFor(MSHashEntry *entry_,const char *key_) const
{
  while (entry_!=0)
   {
     if (strcmp(entry_->stringKey(),key_)==0) return entry_;
     entry_=entry_->next();
   }
  return entry_;
}

MSHashEntry *MSHashTable::addElement(const char *key_,unsigned whichBucket_)
{
  MSHashEntry *entry=new MSHashEntry(key_);
  entry->next(bucket(whichBucket_));
  if (bucket(whichBucket_)) bucket(whichBucket_)->prev(entry);
  bucket(whichBucket_,entry);
  return entry;
}

MSBoolean MSHashTable::add(const char *key_, void *value_)
{
  if (size()==0)
   {
     MSMessageLog::errorMessage("MSHashTable: add failed - hash table size==0\n");
     return MSFalse;
   }
  unsigned whichBucket=hash(key_);  
  MSHashEntry *entry=searchBucketFor(bucket(whichBucket),key_);
  if (entry!=0)
   {
     MSMessageLog::errorMessage("MSHashTable: add failed - key already in table: %d\n",key_);
     return MSFalse;
   }
  entry=addElement(key_,whichBucket);
  entry->value(value_);
  return MSTrue;
}

MSBoolean MSHashTable::remove(const char *key_)
{
  unsigned whichBucket=hash(key_);  
  MSHashEntry *entry=searchBucketFor(bucket(whichBucket),key_);
  if (entry!=0)
   {
     if (bucket(whichBucket)==entry) bucket(whichBucket,entry->next());
     delete entry;
     return MSTrue;
   }  
  return MSFalse;
}

// special add method for adding an entry back into the table i.e. rehashing it is
// useful for a hash table resize
void MSHashTable::addEntry(MSHashEntry *entry_)
{
  unsigned whichBucket=(entry_->stringKey()==0)?hash(entry_->key()):hash(entry_->stringKey());
  entry_->next(bucket(whichBucket));
  if (bucket(whichBucket)!=0) bucket(whichBucket)->prev(entry_);
  bucket(whichBucket,entry_);
}

//#######################################################################
// hash statistic  methods

unsigned MSHashTable::averageChainLength(void) const
{
  MSHashEntry *entry;
  unsigned n=size();
  unsigned r=0;
  for (unsigned i=0;i<n;i++)
   {
     entry=bucket(i);
     while (entry!=0)
      {
        entry=entry->next();
        r++;
      }
   }
  return (n==0)?0:r/n;
}

unsigned MSHashTable::maximumChainLength(void) const
{
  MSHashEntry *entry;
  unsigned n=size();
  unsigned r=0;
  unsigned max=0;
  for (unsigned i=0;i<n;i++)
   {
     r=0;
     entry=bucket(i);
     while (entry!=0)
      {
        entry=entry->next();
        r++;
      }
     if (r>max) max=r;
   }
  return max;
}

unsigned MSHashTable::chainLength(unsigned index_) const
{
  unsigned r=0;
  if (index_<size())
   {
     MSHashEntry *entry=bucket(index_);
     while (entry!=0)
      {
        entry=entry->next();
        r++;
      }
   }
  return r;
}

ostream& MSHashTable::printChainLengths(ostream& aStream_) const
{
  for (unsigned i=0;i<size();i++) aStream_<<chainLength(i)<<" ";
  return aStream_<<endl;
}

ostream& MSHashTable::printHashStatistics(ostream& aStream_) const
{
  unsigned zeroLengthCount=0;
  for (unsigned i=0;i<size();i++) if (bucket(i)==0) zeroLengthCount++;
  
  aStream_<<"Hash Table Size:                "<<size()<<endl;
  aStream_<<"Average Chain Length:           "<<averageChainLength()<<endl;
  aStream_<<"Maximum Chain Length:           "<<maximumChainLength()<<endl;
  aStream_<<"Number of Zero Length Chains:   "<<zeroLengthCount<<endl;
  aStream_<<"Number of Active Chains:        "<<size()-zeroLengthCount<<endl;
  return printChainLengths(aStream_);
}

ostream& MSHashTable::printStringKeys(ostream& aStream_) const
{
  for (unsigned i=0;i<size();i++) 
   {
     MSHashEntry *entry=bucket(i);
     while (entry!=0)
      {
        aStream_<<entry->stringKey()<<" ";
        entry=entry->next();
      }
     aStream_<<endl;
   }
  return aStream_;
}

ostream &operator<<(ostream &aStream_,const MSHashTable &aHashTable_)
{ return aHashTable_.printHashStatistics(aStream_); }

//#######################################################################
// MSStringHashTable
MSStringHashTable::MSStringHashTable(unsigned size_) : MSHashTable(size_) {}

// need to delete strings that are duped and stored 
// as data in the hash table
MSStringHashTable::~MSStringHashTable(void)
{
  for (unsigned i=0;i<size();i++)
   {
     MSHashEntry *entry=bucket(i);
     char *data;
     while (entry!=0)
      {
	_bucket[i]=entry->next();
        data=(char *)entry->value();
        if (data!=0) delete [] data;   
	delete entry;
	entry=bucket(i);
      }
     _bucket[i]=0;
   }
  if (_bucket!=0) delete [] _bucket;
  _size=0;
  _bucket=0;
}

