///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDefines.H>

#if defined(MS_FORCE_INTERNAL_TEMPLATE_INSTANTIATION)
#define MS_MSLongVector_INSTANTIATE
#define MSTK_MANUAL_INSTANTIATION
#endif


#include <MSTypes/MSLongVector.H>

#ifdef MS_NO_INLINES
#include <MSTypes/MSLongVectorInlines.C>
#endif // MS_NO_INLINES

#ifdef MSTK_MANUAL_INSTANTIATION

#include <MSTypes/MSBuiltinTypeVector.C>
#include <MSTypes/MSBuiltinSPick.C>


#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeVector<long>)
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)

#pragma instantiate MSBaseVector<long,MSAllocator<long> >
#pragma instantiate MSBuiltinVector<long>
#pragma instantiate MSBuiltinSPick<long>
#if !defined(MS_TEMPLATE_MANUAL_FRIEND_BUG)
// The version of EDG used by SGI's dcc compiler has a bug related to access from friends
#pragma instantiate ostream & operator<<(ostream&, const MSBaseVector<long,MSAllocator<long> >&)
#endif  //MS_TEMPLATE_MANUAL_FRIEND_BUG

#endif  //MS_EDG_TEMPLATE_INSTANTIATION

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSBaseVector<long,MSAllocator<long> >;
template MSBaseVectorOps<long,MSAllocator<long> >;
template MSBuiltinVector<long>;
template MSBuiltinVectorOps<long>;
template MSBuiltinSPick<long>;
static int __instantiateFriends__()     // instantiate non-inline friend template functions
{
  if (0)
    {
      MSBuiltinVector<long> dummy;
      operator<<(cout, dummy);
    }
  return 0;
}
static int __dummyInt__=__instantiateFriends__();
#endif  //MS_VC_TEMPLATE_INSTANTIATION

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSBaseVector<long,MSAllocator<long> >;
template class MSBaseVectorOps<long,MSAllocator<long> >;
template class MSBuiltinVector<long>;
template class MSBuiltinVectorOps<long>;
template class MSBuiltinSPick<long>;
template ostream & operator<<(ostream&, const MSBaseVector<long,MSAllocator<long> >&);
#endif

#endif // MSTK_MANUAL_INSTANTIATION

MSTypeVector<long>::MSTypeVector() : BuiltinVectorLong()
{
}


MSTypeVector<long>::MSTypeVector (unsigned int length_) : BuiltinVectorLong (length_)
{
}


MSTypeVector<long>::MSTypeVector (unsigned int length_, const long & filler_)
  : BuiltinVectorLong (length_, filler_)
{
}


MSTypeVector<long>::MSTypeVector (const MSTypeVector<long> & vect_) : BuiltinVectorLong (vect_)
{
}


MSTypeVector<long>::MSTypeVector (const BuiltinVectorLong & vect_) : BuiltinVectorLong (vect_)
{
}


MSTypeVector<long>::MSTypeVector (const BaseVectorLong & vect_)
  : BuiltinVectorLong ((BuiltinVectorLong &)vect_)
{
}


MSTypeVector<long>::MSTypeVector (const char *pString_) : BuiltinVectorLong (pString_)
{
}

  
MSTypeVector<long>::MSTypeVector (MSTypeData<long,MSAllocator<long> > *pData_, unsigned int len_)
  : BuiltinVectorLong (pData_, len_)
{
}


MSTypeVector<long>::MSTypeVector (const long *pElements_, unsigned int len_)
  : BuiltinVectorLong (pElements_, len_)
{
}


MSTypeVector<long>::~MSTypeVector()
{
}


MSTypeVector<long> & MSTypeVector<long>::operator= (const MSTypeVector<long> & vect_)
{
  return (MSTypeVector<long> &) BuiltinVectorLong::operator= (vect_);
}


MSTypeVector<long> & MSTypeVector<long>::operator= (const BuiltinVectorLong & vect_)
{
  return (*this = (MSTypeVector<long> &)vect_);
}


MSTypeVector<long> & MSTypeVector<long>::operator= (const BaseVectorLong & vect_)
{
  return (*this = (MSTypeVector<long> &)vect_);
}


MSTypeVector<long> & MSTypeVector<long>::operator= (const long & value_)
{
  return (MSTypeVector<long> &) BuiltinVectorLong::operator= (value_);
}


MSTypeVector<long> & MSTypeVector<long>::operator= (const char *pString_)
{
  return (*this = MSTypeVector<long>(pString_));
//  return (MSTypeVector<long> &) BuiltinVectorLong::operator= (pString_);
}


MSString MSTypeVector<long>::className() const
{
  return MSString ("MSTypeVector<long>");
}


const MSSymbol & MSTypeVector<long>::type() const
{
  return symbol();
}


MSModel * MSTypeVector<long>::clone() const
{
  return new MSTypeVector<long> (*this);
}


MSModel * MSTypeVector<long>::create() const
{
  return new MSTypeVector<long>;
}


const MSSymbol & MSTypeVector<long>::symbol()
{
  static MSSymbol sym ("MSTypeVector<long>");
  return sym;
}


     

