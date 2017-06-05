#ifndef MSUnsignedINLINES
#define MSUnsignedINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif


INLINELINKAGE MSUnsigned::MSUnsigned(void) :
_isSet(MSFalse),_unsigned(0)
{}
INLINELINKAGE MSUnsigned::MSUnsigned(unsigned i_) :
_isSet(MSTrue),_unsigned(i_)
{}
INLINELINKAGE MSUnsigned::MSUnsigned(const MSUnsigned& i_) :
_isSet(i_._isSet),_unsigned(i_._unsigned)
{}

INLINELINKAGE MSUnsigned::operator unsigned() const
{ return _unsigned; }
INLINELINKAGE MSUnsigned::operator double() const
{ return double(_unsigned); }
INLINELINKAGE unsigned MSUnsigned::operator()() const
{ return _unsigned; } // function call operator

INLINELINKAGE MSUnsigned& MSUnsigned::operator=(const MSUnsigned& aUnsigned_)
{
  if (this!=&aUnsigned_)
   {
     _unsigned=aUnsigned_._unsigned;
     _isSet=aUnsigned_._isSet;
     changed();
   }
  return *this;
}
INLINELINKAGE MSUnsigned& MSUnsigned::operator=(unsigned aUnsigned_)
{
  _unsigned=aUnsigned_;
  _isSet=MSTrue;
  return changed(),*this;
}

INLINELINKAGE MSUnsigned& MSUnsigned::operator=(const char *value_)
{ set(value_); return *this; }

INLINELINKAGE MSBoolean MSUnsigned::operator==(const MSUnsigned& i_) const
{ return MSBoolean(_unsigned==i_._unsigned); }
INLINELINKAGE MSBoolean MSUnsigned::operator!=(const MSUnsigned& i_) const
{ return MSBoolean(_unsigned!=i_._unsigned); }
INLINELINKAGE MSBoolean MSUnsigned::operator< (const MSUnsigned& i_) const
{ return MSBoolean(_unsigned< i_._unsigned); }
INLINELINKAGE MSBoolean MSUnsigned::operator<=(const MSUnsigned& i_) const
{ return MSBoolean(_unsigned<=i_._unsigned); }
INLINELINKAGE MSBoolean MSUnsigned::operator> (const MSUnsigned& i_) const
{ return MSBoolean(_unsigned> i_._unsigned); }
INLINELINKAGE MSBoolean MSUnsigned::operator>=(const MSUnsigned& i_) const
{ return MSBoolean(_unsigned>=i_._unsigned); }

INLINELINKAGE MSBoolean MSUnsigned::operator==(unsigned i_) const
{ return MSBoolean(_unsigned==i_); } 
INLINELINKAGE MSBoolean MSUnsigned::operator!=(unsigned i_) const
{ return MSBoolean(_unsigned!=i_); } 
INLINELINKAGE MSBoolean MSUnsigned::operator< (unsigned i_) const
{ return MSBoolean(_unsigned< i_); } 
INLINELINKAGE MSBoolean MSUnsigned::operator<=(unsigned i_) const
{ return MSBoolean(_unsigned<=i_); } 
INLINELINKAGE MSBoolean MSUnsigned::operator> (unsigned i_) const
{ return MSBoolean(_unsigned> i_); } 
INLINELINKAGE MSBoolean MSUnsigned::operator>=(unsigned i_) const
{ return MSBoolean(_unsigned>=i_); } 

// Prefix - add/subtract one, then return result
INLINELINKAGE MSUnsigned& MSUnsigned::operator++()
{ return ++_unsigned,changed(),*this; } 
INLINELINKAGE MSUnsigned& MSUnsigned::operator--()
{ return --_unsigned,changed(),*this; } 

// Postfix - add/subtract one, then return the initial value
INLINELINKAGE MSUnsigned MSUnsigned::operator++(int)
{ unsigned i=_unsigned++; return changed(),MSUnsigned(i); } 
INLINELINKAGE MSUnsigned MSUnsigned::operator--(int)
{ unsigned i=_unsigned--; return changed(),MSUnsigned(i); } 

INLINELINKAGE MSUnsigned& MSUnsigned::operator+=(unsigned i_)
{ _unsigned+=i_; return changed(),*this; }
INLINELINKAGE MSUnsigned& MSUnsigned::operator-=(unsigned i_)
{ _unsigned-=i_; return changed(),*this; }
INLINELINKAGE MSUnsigned& MSUnsigned::operator*=(unsigned i_)
{ _unsigned*=i_; return changed(),*this; }
INLINELINKAGE MSUnsigned& MSUnsigned::operator/=(unsigned i_)
{ _unsigned/=i_; return changed(),*this; }

INLINELINKAGE MSUnsigned MSUnsigned::operator-()
{ return -_unsigned; }

INLINELINKAGE MSBoolean MSUnsigned::isSet(void) const
{ return _isSet; }

#endif
