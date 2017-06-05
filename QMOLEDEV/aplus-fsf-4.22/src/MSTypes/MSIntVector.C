///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDefines.H>

#if defined(MS_FORCE_INTERNAL_TEMPLATE_INSTANTIATION)
#define MS_MSIntVector_INSTANTIATE
#define MSTK_MANUAL_INSTANTIATION
#endif

#include <MSTypes/MSIntVector.H>

#ifdef MS_NO_INLINES
#include <MSTypes/MSIntVectorInlines.C>
#endif // MS_NO_INLINES

#ifdef MSTK_MANUAL_INSTANTIATION
#include <MSTypes/MSBuiltinTypeVector.C>
#include <MSTypes/MSBuiltinSPick.C>

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeVector<int>)
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSBaseVector<int, MSAllocator<int> >
#pragma instantiate MSBuiltinVector<int>
#pragma instantiate MSBuiltinSPick<int>
#if !defined(MS_TEMPLATE_MANUAL_FRIEND_BUG)
// The version of EDG used by SGI's dcc compiler has a bug related to access from friends
#pragma instantiate ostream & operator<<(ostream&, const MSBaseVector<int, MSAllocator<int> >&)
#endif  //MS_TEMPLATE_MANUAL_FRIEND_BUG

#endif  // MS_EDG_TEMPLATE_INSTANTIATION

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSBaseVector<int, MSAllocator<int> >;
template MSBaseVectorOps<int, MSAllocator<int> >;
template MSBuiltinVector<int>;
template MSBuiltinVectorOps<int>;
template MSBuiltinSPick<int>;
static int __instantiateFriends__()     // instantiate non-inline friend template functions
{
  if (0)
    {
      MSBuiltinVector<int> dummy;
      operator<<(cout, dummy);
    }
      
  return 0;
}
static int __dummyInt__=__instantiateFriends__();
#endif  //MS_VC_TEMPLATE_INSTANTIATION

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSBaseVector<int, MSAllocator<int> >;
template class MSBaseVectorOps<int, MSAllocator<int> >;
template class MSBuiltinVector<int>;
template class MSBuiltinVectorOps<int>;
template class MSBuiltinSPick<int>;
template ostream & operator<<(ostream&, const MSBaseVector<int, MSAllocator<int> >&);
#endif

#endif // MSTK_MANUAL_INSTANTIATION

MSTypeVector<int>::MSTypeVector() : BuiltinVectorInt()
{
}


MSTypeVector<int>::MSTypeVector (unsigned int length_) : BuiltinVectorInt (length_)
{
}


MSTypeVector<int>::MSTypeVector (unsigned int length_, const int filler_)
  : BuiltinVectorInt (length_, filler_)
{
}


MSTypeVector<int>::MSTypeVector (const MSTypeVector<int> & vect_) : BuiltinVectorInt (vect_)
{
}


MSTypeVector<int>::MSTypeVector (const BuiltinVectorInt & vect_) : BuiltinVectorInt (vect_)
{
}


MSTypeVector<int>::MSTypeVector (const BaseVectorInt & vect_)
  : BuiltinVectorInt ((BuiltinVectorInt &)vect_)
{
}


MSTypeVector<int>::MSTypeVector (const char *pString_) : BuiltinVectorInt (pString_)
{
}

  
MSTypeVector<int>::MSTypeVector (MSTypeData<int, MSAllocator<int> > *pData_, unsigned int len_)
  : BuiltinVectorInt (pData_, len_)
{
}


MSTypeVector<int>::MSTypeVector (const int *pElements_, unsigned int len_)
  : BuiltinVectorInt (pElements_, len_)
{
}


MSTypeVector<int>::~MSTypeVector()
{
}


MSTypeVector<int> & MSTypeVector<int>::operator= (const MSTypeVector<int> & vect_)
{
  return (MSTypeVector<int> &) BuiltinVectorInt::operator= (vect_);
}


MSTypeVector<int> & MSTypeVector<int>::operator= (const BuiltinVectorInt & vect_)
{
  return (*this = (MSTypeVector<int> &)vect_);
}


MSTypeVector<int> & MSTypeVector<int>::operator= (const BaseVectorInt & vect_)
{
  return (*this = (MSTypeVector<int> &)vect_);
}


MSTypeVector<int> & MSTypeVector<int>::operator= (const int & value_)
{
  return (MSTypeVector<int> &) BuiltinVectorInt::operator= (value_);
}


MSTypeVector<int> & MSTypeVector<int>::operator= (const char *pString_)
{
  return (*this = MSTypeVector<int>(pString_));
}


MSString MSTypeVector<int>::className() const
{
  return MSString ("MSTypeVector<int>");
}


const MSSymbol & MSTypeVector<int>::type() const
{
  return symbol();
}


MSModel * MSTypeVector<int>::clone() const
{
  return new MSTypeVector<int> (*this);
}


MSModel * MSTypeVector<int>::create() const
{
  return new MSTypeVector<int>;
}


const MSSymbol & MSTypeVector<int>::symbol()
{
  static MSSymbol sym ("MSTypeVector<int>");
  return sym;
}




