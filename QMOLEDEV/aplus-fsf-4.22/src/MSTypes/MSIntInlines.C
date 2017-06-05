#ifndef MSIntINLINES
#define MSIntINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////



INLINELINKAGE MSInt::MSInt(void) :
_isSet(MSFalse),_int(0)
{}
INLINELINKAGE MSInt::MSInt(int i_) :
_isSet(MSTrue),_int(i_)
{}
INLINELINKAGE MSInt::MSInt(const MSInt& i_) :
_isSet(i_._isSet),_int(i_._int)
{}

INLINELINKAGE MSInt::operator int() const
{ return _int; }
INLINELINKAGE MSInt::operator double() const
{ return double(_int); }
INLINELINKAGE int MSInt::operator()() const
{ return _int; } // function call operator

INLINELINKAGE MSInt& MSInt::operator=(const MSInt& aInt_)
{
  if (this!=&aInt_)
   {
     _int=aInt_._int;
     _isSet=aInt_._isSet;
     changed();
   }
  return *this;
}
INLINELINKAGE MSInt& MSInt::operator=(int aInt_)
{
  _int=aInt_;
  _isSet=MSTrue;
  return changed(),*this;
}

INLINELINKAGE MSInt& MSInt::operator=(const char *value_)
{ set(value_); return *this; }

INLINELINKAGE MSBoolean MSInt::operator==(const MSInt& i_) const
{ return MSBoolean(_int==i_._int); }
INLINELINKAGE MSBoolean MSInt::operator!=(const MSInt& i_) const
{ return MSBoolean(_int!=i_._int); }
INLINELINKAGE MSBoolean MSInt::operator< (const MSInt& i_) const
{ return MSBoolean(_int< i_._int); }
INLINELINKAGE MSBoolean MSInt::operator<=(const MSInt& i_) const
{ return MSBoolean(_int<=i_._int); }
INLINELINKAGE MSBoolean MSInt::operator> (const MSInt& i_) const
{ return MSBoolean(_int> i_._int); }
INLINELINKAGE MSBoolean MSInt::operator>=(const MSInt& i_) const
{ return MSBoolean(_int>=i_._int); }

INLINELINKAGE MSBoolean MSInt::operator==(int i_) const
{ return MSBoolean(_int==i_); } 
INLINELINKAGE MSBoolean MSInt::operator!=(int i_) const
{ return MSBoolean(_int!=i_); } 
INLINELINKAGE MSBoolean MSInt::operator< (int i_) const
{ return MSBoolean(_int< i_); } 
INLINELINKAGE MSBoolean MSInt::operator<=(int i_) const
{ return MSBoolean(_int<=i_); } 
INLINELINKAGE MSBoolean MSInt::operator> (int i_) const
{ return MSBoolean(_int> i_); } 
INLINELINKAGE MSBoolean MSInt::operator>=(int i_) const
{ return MSBoolean(_int>=i_); } 

// Prefix - add/subtract one, then return result
INLINELINKAGE MSInt& MSInt::operator++()
{ return ++_int,changed(),*this; } 
INLINELINKAGE MSInt& MSInt::operator--()
{ return --_int,changed(),*this; } 

// Postfix - add/subtract one, then return the initial value
INLINELINKAGE MSInt MSInt::operator++(int)
{ int i=_int++; return changed(),MSInt(i); } 
INLINELINKAGE MSInt MSInt::operator--(int)
{ int i=_int--; return changed(),MSInt(i); } 

INLINELINKAGE MSInt& MSInt::operator+=(int i_)
{ _int+=i_; return changed(),*this; }
INLINELINKAGE MSInt& MSInt::operator-=(int i_)
{ _int-=i_; return changed(),*this; }
INLINELINKAGE MSInt& MSInt::operator*=(int i_)
{ _int*=i_; return changed(),*this; }
INLINELINKAGE MSInt& MSInt::operator/=(int i_)
{ _int/=i_; return changed(),*this; }

INLINELINKAGE MSInt MSInt::operator-()
{ return -_int; }

INLINELINKAGE MSBoolean MSInt::isSet(void) const
{ return _isSet; }

#endif
