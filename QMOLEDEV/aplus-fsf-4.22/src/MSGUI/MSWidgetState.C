///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <MSGUI/MSWidgetState.H>
#include <MSGUI/MSAttrValue.H>
#include <MSTypes/MSString.H>

MSWidgetState::MSWidgetState (istream &is_)
: MSHashTable(1024)
{
  if (!is_) return;
  while (!is_.eof())
   {
     MSString line=MSString::lineFrom(is_);
     if (line.length()==0) continue;
     if (line(0)=='#') continue;
     unsigned index;
     MSString attributeToken(".has.");
     if ((index=line.indexOf(attributeToken))!=line.length()&&index!=0)
      {
	MSString pathname=line.subString(0,index);
	line=line.subString(index+attributeToken.length());
	if ((index=line.indexOf('('))==line.length()||index==0) continue;
	MSString attributeName=line.subString(0,index);
	line=line.subString(index+1);

	if ((index=line.indexOf(')'))==line.length()) continue;
	MSString attributeValue=line.subString(0,index);
	MSAttrValueList *list;
	if ((list=(MSAttrValueList *)lookup(pathname))==0)
	 {
	   list=new MSAttrValueList;
	   *list<<MSAttrValue(attributeName,attributeValue);
	   add(pathname.string(),list);
	 }
	else *list<<MSAttrValue(attributeName,attributeValue);
      }
   }
}

MSWidgetState::~MSWidgetState(void)
{
  for (unsigned i=0;i<size();i++)
   {
     MSHashEntry *entry=bucket(i);
     while (entry!=0)
      {
	_bucket[i]=entry->next();
	MSAttrValueList *list=(MSAttrValueList *)entry->value();
	if (list!=0) delete list;
	delete entry;
	entry=bucket(i);
      }
     _bucket[i]=0;
   }
  if (_bucket!=0) delete [] _bucket;
  _bucket=0;
  _size=0;

}
