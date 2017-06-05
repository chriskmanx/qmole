///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDefines.H>

#if defined(MS_FORCE_INTERNAL_TEMPLATE_INSTANTIATION)
#define MS_MSCharVector_INSTANTIATE
#define MSTK_MANUAL_INSTANTIATION
#endif

#include <MSTypes/MSCharVector.H>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#include <strstream.h>
#endif

#ifdef MS_NO_INLINES
#include <MSTypes/MSCharVectorInlines.C>
#endif // MS_NO_INLINES

MSTypeVector<char>::MSTypeVector() : BuiltinVectorChar()
{
}


MSTypeVector<char>::MSTypeVector (unsigned int length_) : BuiltinVectorChar (length_)
{
}


MSTypeVector<char>::MSTypeVector (unsigned int length_, const char filler_) : BuiltinVectorChar (length_, filler_)
{
}


MSTypeVector<char>::MSTypeVector (const MSTypeVector<char> & vect_) : BuiltinVectorChar (vect_)
{
}


MSTypeVector<char>::MSTypeVector (const BuiltinVectorChar & vect_) : BuiltinVectorChar (vect_)
{
}


MSTypeVector<char>::MSTypeVector (const BaseVectorChar & vect_) : BuiltinVectorChar ((BuiltinVectorChar &)vect_)
{
}


MSTypeVector<char>::MSTypeVector (const char *pString_) : BuiltinVectorChar (pString_)
{
}

  
MSTypeVector<char>::MSTypeVector (MSTypeData<char,MSAllocator<char> > *pData_, unsigned int len_)
  : BuiltinVectorChar (pData_, len_)
{
}


MSTypeVector<char>::MSTypeVector (const char *pElements_, unsigned int len_)
  : BuiltinVectorChar (pElements_, len_)
{
}


MSTypeVector<char>::~MSTypeVector()
{
}


MSTypeVector<char> & MSTypeVector<char>::operator= (const MSTypeVector<char> & vect_)
{
  return (MSTypeVector<char> &) BuiltinVectorChar::operator= (vect_);
}


MSTypeVector<char> & MSTypeVector<char>::operator= (const BuiltinVectorChar & vect_)
{
  return (*this = (MSTypeVector<char> &)vect_);
}


MSTypeVector<char> & MSTypeVector<char>::operator= (const BaseVectorChar & vect_)
{
  return (*this = (MSTypeVector<char> &)vect_);
}


MSTypeVector<char> & MSTypeVector<char>::operator= (const char value_)
{
  return (MSTypeVector<char> &) BuiltinVectorChar::operator= (value_);
}


MSTypeVector<char> & MSTypeVector<char>::operator= (const char *pString_)
{
  return (*this = MSTypeVector<char>(pString_));
//  return (MSTypeVector<char> &) BuiltinVectorChar::operator= (pString_);
}


MSString MSTypeVector<char>::className() const
{
  return MSString ("MSTypeVector<char>");
}


const MSSymbol & MSTypeVector<char>::type() const
{
  return symbol();
}


MSModel * MSTypeVector<char>::clone() const
{
  return new MSTypeVector<char> (*this);
}


MSModel * MSTypeVector<char>::create() const
{
  return new MSTypeVector<char>;
}


const MSSymbol & MSTypeVector<char>::symbol()
{
  static MSSymbol sym ("MSTypeVector<char>");
  return sym;
}


#if HAVE_SSTREAM
void whitespace (const char &, istringstream & ist)
#else
void whitespace (const char &, istrstream & ist)
#endif
{
  ist.unsetf (ios::skipws);
}



#if defined(MSTK_MANUAL_INSTANTIATION)
#include <MSTypes/MSBuiltinTypeVector.C>
#include <MSTypes/MSBuiltinSPick.C>

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeVector<char>)
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)

#pragma instantiate MSBaseVector<char,MSAllocator<char> >
#pragma instantiate MSBuiltinVector<char>
#pragma instantiate MSBuiltinSPick<char>
#if !defined(MS_TEMPLATE_MANUAL_FRIEND_BUG)
// The version of EDG used by SGI's dcc compiler has a bug related to access from friends
#pragma instantiate ostream & operator<<(ostream&, const MSBaseVector<char,MSAllocator<char> >&)
#endif  //__DELTA
#endif  //__sgi || __edgfe

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSBaseVector<char,MSAllocator<char> >;
template MSBaseVectorOps<char,MSAllocator<char> >;
template MSBuiltinVector<char>;
template MSBuiltinVectorOps<char>;
template MSBuiltinSPick<char>;
static int __instantiateFriends__()     // instantiate non-inline friend template functions
{
  if (0)
    {
      MSBuiltinVector<char> dummy;
      operator<<(cout, dummy);
    }
      
  return 0;
}
static int __dummyInt__=__instantiateFriends__();
#endif  // MS_VC_TEMPLATE_INSTANTIATION

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSBaseVector<char,MSAllocator<char> >;
template class MSBaseVectorOps<char,MSAllocator<char> >;
template class MSBuiltinVector<char>;
template class MSBuiltinVectorOps<char>;
template class MSBuiltinSPick<char>;
template ostream& operator<<(ostream&,const MSBaseVector<char,MSAllocator<char> >&);
#endif

#endif // MSTK_MANUAL_INSTANTIATION
