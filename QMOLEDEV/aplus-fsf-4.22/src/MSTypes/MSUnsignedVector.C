///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDefines.H>

#if defined(MS_FORCE_INTERNAL_TEMPLATE_INSTANTIATION)
#define MS_MSUnsignedVector_INSTANTIATE
#define MSTK_MANUAL_INSTANTIATION
#endif

#include <MSTypes/MSUnsignedVector.H>

#ifdef MS_NO_INLINES
#include <MSTypes/MSUnsignedVectorInlines.C>
#endif //MS_NO_INLINES

#if defined(MSTK_MANUAL_INSTANTIATION)

#include <MSTypes/MSBuiltinTypeVector.C>
#include <MSTypes/MSBuiltinSPick.C>

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeVector<unsigned int>)
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSBaseVector<unsigned int,MSAllocator<unsigned int> >
#pragma instantiate MSBuiltinVector<unsigned int>
#pragma instantiate MSBuiltinSPick<unsigned int>
#if !defined (MS_TEMPLATE_MANUAL_FRIEND_BUG)
// The version of EDG used by SGI's dcc compiler has a bug related to access from friends
#pragma instantiate ostream & operator<<(ostream&, const MSBaseVector<unsigned int,MSAllocator<unsigned int> >&)
#endif  // MS_TEMPLATE_MANUAL_FRIEND_BUG

#endif  // MS_EDG_TEMPLATE_INSTANTIATION

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSBaseVector<unsigned int,MSAllocator<unsigned int> >;
template MSBaseVectorOps<unsigned int,MSAllocator<unsigned int> >;
template MSBuiltinVector<unsigned int>;
template MSBuiltinVectorOps<unsigned int>;
template MSBuiltinSPick<unsigned int>;
static int __instantiateFriends__()     // instantiate non-inline friend template functions
{
  if (0)
    {
      MSBuiltinVector<unsigned int> dummy;
      operator<<(cout, dummy);
    }
  return 0;
}
static int __dummyInt__=__instantiateFriends__();
#endif  // MS_VC_TEMPLATE_INSTANTIATION

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSBaseVector<unsigned int, MSAllocator<unsigned int> >;
template class MSBaseVectorOps<unsigned int, MSAllocator<unsigned int> >;
template class MSBuiltinVector<unsigned>;
template class MSBuiltinVectorOps<unsigned>;
template class MSBuiltinSPick<unsigned>;
template ostream & operator<<(ostream&, const MSBaseVector<unsigned int, MSAllocator<unsigned int> >&);
#endif

#endif // MSTK_MANUAL_INSTANTIATION

MSTypeVector<unsigned int>::MSTypeVector() : BuiltinVectorUnsigned()
{
}


MSTypeVector<unsigned int>::MSTypeVector (unsigned int length_) : BuiltinVectorUnsigned (length_)
{
}


MSTypeVector<unsigned int>::MSTypeVector (unsigned int length_, const unsigned int filler_)
  : BuiltinVectorUnsigned (length_, filler_)
{
}


MSTypeVector<unsigned int>::MSTypeVector (const MSTypeVector<unsigned int> & vect_)
  : BuiltinVectorUnsigned (vect_)
{
}


MSTypeVector<unsigned int>::MSTypeVector (const BuiltinVectorUnsigned & vect_)
  : BuiltinVectorUnsigned (vect_)
{
}


MSTypeVector<unsigned int>::MSTypeVector (const BaseVectorUnsigned & vect_)
  : BuiltinVectorUnsigned ((BuiltinVectorUnsigned &)vect_)
{
}


MSTypeVector<unsigned int>::MSTypeVector (const char *pString_) : BuiltinVectorUnsigned (pString_)
{
}

  
MSTypeVector<unsigned int>::MSTypeVector (MSTypeData<unsigned int,MSAllocator<unsigned int> > *pData_, unsigned int len_)
  : BuiltinVectorUnsigned (pData_, len_)
{
}


MSTypeVector<unsigned int>::MSTypeVector (const unsigned int *pElements_, unsigned int len_)
  : BuiltinVectorUnsigned (pElements_, len_)
{
}


MSTypeVector<unsigned int>::~MSTypeVector()
{
}


MSTypeVector<unsigned int> & MSTypeVector<unsigned int>::operator= (const MSTypeVector<unsigned int> & vect_)
{
  return (MSTypeVector<unsigned int> &) BuiltinVectorUnsigned::operator= (vect_);
}


MSTypeVector<unsigned int> & MSTypeVector<unsigned int>::operator= (const BuiltinVectorUnsigned & vect_)
{
  return (*this = (MSTypeVector<unsigned int> &)vect_);
}


MSTypeVector<unsigned int> & MSTypeVector<unsigned int>::operator= (const BaseVectorUnsigned & vect_)
{
  return (*this = (MSTypeVector<unsigned int> &)vect_);
}


MSTypeVector<unsigned int> & MSTypeVector<unsigned int>::operator= (const unsigned int & value_)
{
  return (MSTypeVector<unsigned int> &) BuiltinVectorUnsigned::operator= (value_);
}


MSTypeVector<unsigned int> & MSTypeVector<unsigned int>::operator= (const char *pString_)
{
  return (*this = MSTypeVector<unsigned int>(pString_));
//  return (MSTypeVector<unsigned int> &) BuiltinVectorUnsigned::operator= (pString_);
}


MSString MSTypeVector<unsigned int>::className() const
{
  return MSString ("MSTypeVector<unsigned int>");
}


const MSSymbol & MSTypeVector<unsigned int>::type() const
{
  return symbol();
}


MSModel * MSTypeVector<unsigned int>::clone() const
{
  return new MSTypeVector<unsigned int> (*this);
}


MSModel * MSTypeVector<unsigned int>::create() const
{
  return new MSTypeVector<unsigned int>;
}


const MSSymbol & MSTypeVector<unsigned int>::symbol()
{
  static MSSymbol sym ("MSTypeVector<unsigned int>");
  return sym;
}




