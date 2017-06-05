///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSResourceCodeSet.H>
#include <MSTypes/MSStringVector.H>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
  
MSResourceCodeSet::MSResourceCodeSet()
{}

MSResourceCodeSet::MSResourceCodeSet(const MSResourceCodeSet & rCodeSet_)
    : _stringVector(rCodeSet_._stringVector)
{}

MSResourceCodeSet::MSResourceCodeSet(const MSResourceCode & rCode_)
    : _stringVector(rCode_)
{}

MSBoolean MSResourceCodeSet::add(const MSResourceCode & rCode_)
{
  if (_stringVector.indexOf(rCode_)==_stringVector.length())
   {
     _stringVector.append(rCode_);
     return MSTrue;
   }
  return MSFalse;
}

void MSResourceCodeSet::removeAll(void)
{ 
  _stringVector.removeAll(); 
}

MSResourceCodeSet & MSResourceCodeSet::operator=(const MSResourceCodeSet & rCodeSet_) 
{
  if (this!=&rCodeSet_) 
   {
     _stringVector=rCodeSet_._stringVector;
   } 
  return *this;
}

MSResourceCodeSet::~MSResourceCodeSet()
{}

unsigned int MSResourceCodeSet::numberOfElements() const
{
  return _stringVector.length() ;
}

MSBoolean MSResourceCodeSet::isEmpty() const
{ return (numberOfElements()<=0 ? MSTrue : MSFalse); }

const MSResourceCode& MSResourceCodeSet::elementAt(unsigned int i) const
{
  return _stringVector.elementAt(i);
}

ostream& operator<<(ostream& os,const MSResourceCodeSet& rCodeSet_)
{
  os <<"MSResourceCodeSet: " ;
  int num=rCodeSet_.numberOfElements();
  os << num << " elements:<" ;
  for (int i=0; i<num; ++i)
   {
     os << rCodeSet_._stringVector.elementAt(i);
     if (i<num-1) os <<',';
   }
  os << '>' << endl;
  return os;
}

MSString MSResourceCodeSet::asDebugInfo(void) const
{
  MSString result("MSResourceCodeSet(@");
  result+=MSString((unsigned long) this).d2x().lowerCase();
  result+=",<";
  int num=numberOfElements();
  for (int i=0; i<num; ++i)
   {
     result +=_stringVector.elementAt(i);
     if (i<num-1) result+=',';
   }
  result += ">)";
  return result;
}
