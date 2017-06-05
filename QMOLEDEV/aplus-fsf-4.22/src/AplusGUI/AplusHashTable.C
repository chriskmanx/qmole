///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <MSTypes/MSTime.H>
#include <a/fncdcls.h>
#include <AplusGUI/AplusHashTable.H>

AplusHashTable::AplusHashTable(unsigned int size_) : MSHashTable(size_)  {}
AplusHashTable::~AplusHashTable(void) {}

A AplusHashTable::listAllEntries(void) const
{
  unsigned int sz=size(), count=0;
  unsigned int i ;
  for (i=0; i<sz; i++)
    {
      count += chainLength(i);
    }

  if (count==0)
    {
      return aplus_nl;
    }

  // count>0
  A entries=gv(Et,count);

  for (i=0, count=0; i<sz; i++)
   {
     MSHashEntry *entry=bucket(i);
     char *data;
     while (entry!=0)
      {
	entries->p[count++]=MS(si((char *)entry->value()));
	entry=entry->next();
      }
   }

  return entries;
}

  
