///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDefines.H>

#if defined(MS_FORCE_INTERNAL_TEMPLATE_INSTANTIATION)
#define MS_MSSymbolVector_INSTANTIATE
#define MSTK_MANUAL_INSTANTIATION
#endif

#include <MSTypes/MSSymbolVector.H>

#if defined(MSTK_MANUAL_INSTANTIATION)

#include <MSTypes/MSTypeData.C>
#include <MSTypes/MSBaseTypeVector.C>

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeVector<MSSymbol>)
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSBaseVector<MSSymbol,MSAllocator<MSSymbol> >

#if !defined (MS_TEMPLATE_MANUAL_FRIEND_BUG)
// The version of EDG used by SGI's dcc compiler has a bug related to access from friends
#pragma instantiate ostream & operator<< (ostream &, const MSBaseVector<MSSymbol,MSAllocator<MSSymbol> > &)
#endif  // MS_TEMPLATE_MANUAL_FRIEND_BUG
#endif  // MS_EDG_TEMPLATE_INSTANTIATION

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSBaseVector<MSSymbol,MSAllocator<MSSymbol> >;
template MSBaseVectorOps<MSSymbol,MSAllocator<MSSymbol> >;
// instantiate non-inline friend template functions
static int __dummy01__()  {  if (0) operator<<(cout, MSBaseVector<MSSymbol,MSAllocator<MSSymbol> >()); return 0;  }
static int __dummyInt01__=__dummy01__();
#endif  //MS_VC_TEMPLATE_INSTANTIATION

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSBaseVector<MSSymbol,MSAllocator<MSSymbol> >;
template class MSBaseVectorOps<MSSymbol,MSAllocator<MSSymbol> >;
template ostream& operator<<(ostream&,const MSBaseVector<MSSymbol,MSAllocator<MSSymbol> >&);
#endif

#endif // MSTK_MANUAL_INSTANTIATION

MSTypeVector<MSSymbol>::MSTypeVector() : MSBaseVector<MSSymbol,MSAllocator<MSSymbol> >()
{
}


MSTypeVector<MSSymbol>::MSTypeVector (unsigned int length_)
#if !defined MS_TEMPLATE_CONSTRUCTOR_OVERLOAD_BUG
 : MSBaseVector<MSSymbol,MSAllocator<MSSymbol> > (length_)
#else  // Visual C++ bug in overloading resolution
 : MSBaseVector<MSSymbol,MSAllocator<MSSymbol> > (length_, 0, 0)
#endif  //MS_TEMPLATE_CONSTRUCTOR_OVERLOAD_BUG
{
}


MSTypeVector<MSSymbol>::MSTypeVector (unsigned int length_, const MSSymbol & filler_)
  : MSBaseVector<MSSymbol,MSAllocator<MSSymbol> > (length_, filler_)
{
}


MSTypeVector<MSSymbol>::MSTypeVector (const MSTypeVector<MSSymbol> & vect_) : MSBaseVector<MSSymbol,MSAllocator<MSSymbol> > (vect_)
{
}


MSTypeVector<MSSymbol>::MSTypeVector (const MSBaseVector<MSSymbol,MSAllocator<MSSymbol> > & vect_) : MSBaseVector<MSSymbol,MSAllocator<MSSymbol> > (vect_)
{
}


MSTypeVector<MSSymbol>::MSTypeVector (const char *pString_) : MSBaseVector<MSSymbol,MSAllocator<MSSymbol> > (pString_)
{
}

  
MSTypeVector<MSSymbol>::MSTypeVector (MSTypeData<MSSymbol,MSAllocator<MSSymbol> > *pData_, unsigned int len_)
  : MSBaseVector<MSSymbol,MSAllocator<MSSymbol> > (pData_, len_)
{
}


MSTypeVector<MSSymbol>::MSTypeVector (const MSSymbol *pElements_, unsigned int len_)
  : MSBaseVector<MSSymbol,MSAllocator<MSSymbol> > (pElements_, len_)
{
}


MSTypeVector<MSSymbol>::~MSTypeVector()
{
}


MSTypeVector<MSSymbol> & MSTypeVector<MSSymbol>::operator= (const MSTypeVector<MSSymbol> & vect_)
{
  return (MSTypeVector<MSSymbol> &) MSBaseVector<MSSymbol,MSAllocator<MSSymbol> >::operator= (vect_);
}


MSTypeVector<MSSymbol> & MSTypeVector<MSSymbol>::operator= (const MSBaseVector<MSSymbol,MSAllocator<MSSymbol> > & vect_)
{
  return (*this = (MSTypeVector<MSSymbol> &)vect_);
}


MSTypeVector<MSSymbol> & MSTypeVector<MSSymbol>::operator= (const MSSymbol & value_)
{
  return (MSTypeVector<MSSymbol> &) MSBaseVector<MSSymbol,MSAllocator<MSSymbol> >::operator= (value_);
}


MSTypeVector<MSSymbol> & MSTypeVector<MSSymbol>::operator= (const char *pString_)
{
  return (*this = MSTypeVector<MSSymbol>(pString_));
}


MSString MSTypeVector<MSSymbol>::className() const
{
  return MSString ("MSTypeVector<MSSymbol>");
}


const MSSymbol & MSTypeVector<MSSymbol>::type() const
{
  return symbol();
}


MSModel * MSTypeVector<MSSymbol>::clone() const
{
  return new MSTypeVector<MSSymbol> (*this);
}


MSModel * MSTypeVector<MSSymbol>::create() const
{
  return new MSTypeVector<MSSymbol>;
}


const MSSymbol & MSTypeVector<MSSymbol>::symbol()
{
  static MSSymbol sym ("MSTypeVector<MSSymbol>");
  return sym;
}


MSTypeVector<MSSymbol>::SPick & MSTypeVector<MSSymbol>::SPick::operator= (const MSTypeVector<MSSymbol>::SPick & sPick_)
{
  _pVector->set (_index, (*sPick_._pVector)(sPick_._index));
  return *this;
}


MSTypeVector<MSSymbol>::SPick & MSTypeVector<MSSymbol>::SPick::operator= (const MSSymbol & value_)
{
  _pVector->set (_index, value_);
  return *this;
}


istream & operator>> (istream & stream_, MSTypeVector<MSSymbol>::SPick & symPick_)
{
  MSSymbol sym;
  stream_ >> sym;
  symPick_._pVector->set (symPick_._index, sym);
  return stream_;
}


