///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDefines.H>

#if defined(MS_FORCE_INTERNAL_TEMPLATE_INSTANTIATION)
#define MS_MSUnsignedLongVector_INSTANTIATE
#define MSTK_MANUAL_INSTANTIATION
#endif


#include <MSTypes/MSUnsignedLongVector.H>

#ifdef MS_NO_INLINES
#include <MSTypes/MSUnsignedLongVectorInlines.C>
#endif // MS_NO_INLINES

#if defined(MSTK_MANUAL_INSTANTIATION)

#include <MSTypes/MSBuiltinTypeVector.C>
#include <MSTypes/MSBuiltinSPick.C>


#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeVector<unsigned long>)
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSBaseVector<unsigned long, MSAllocator<unsigned long> >
#pragma instantiate MSBuiltinVector<unsigned long>
#pragma instantiate MSBuiltinSPick<unsigned long>
#if !defined (MS_TEMPLATE_MANUAL_FRIEND_BUG)
// The version of EDG used by SGI's dcc compiler has a bug related to access from friends
#pragma instantiate ostream & operator<<(ostream&, const MSBaseVector<unsigned long, MSAllocator<unsigned long> >&)
#endif  // MS_TEMPLATE_MANUAL_FRIEND_BUG

#endif  // MS_EDG_TEMPLATE_INSTANTIATION

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSBaseVector<unsigned long, MSAllocator<unsigned long> >;
template MSBaseVectorOps<unsigned long, MSAllocator<unsigned long> >;
template MSBuiltinVector<unsigned long>;
template MSBuiltinVectorOps<unsigned long>;
template MSBuiltinSPick<unsigned long>;
static int __instantiateFriends__()     // instantiate non-inline friend template functions
{
  if (0)
    {
      MSBuiltinVector<unsigned long> dummy;
      operator<<(cout, dummy);
    }
  return 0;
}
static int __dummyInt__=__instantiateFriends__();
#endif  // MS_VC_TEMPLATE_INSTANTIATION

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSBaseVector<unsigned long, MSAllocator<unsigned long> >;
template class MSBaseVectorOps<unsigned long, MSAllocator<unsigned long> >;
template class MSBuiltinVector<unsigned long>;
template class MSBuiltinVectorOps<unsigned long>;
template class MSBuiltinSPick<unsigned long>;
template ostream & operator<<(ostream&, const MSBaseVector<unsigned long, MSAllocator<unsigned long> >&);
#endif 

#endif // MSTK_MANUAL_INSTANTIATION

MSTypeVector<unsigned long>::MSTypeVector() : BuiltinVectorUnsignedLong()
{
}


MSTypeVector<unsigned long>::MSTypeVector (unsigned int length_) : BuiltinVectorUnsignedLong (length_)
{
}


MSTypeVector<unsigned long>::MSTypeVector (unsigned int length_, const unsigned long & filler_)
  : BuiltinVectorUnsignedLong (length_, filler_)
{
}


MSTypeVector<unsigned long>::MSTypeVector (const MSTypeVector<unsigned long> & vect_)
  : BuiltinVectorUnsignedLong (vect_)
{
}


MSTypeVector<unsigned long>::MSTypeVector (const BuiltinVectorUnsignedLong & vect_)
  : BuiltinVectorUnsignedLong (vect_)
{
}


MSTypeVector<unsigned long>::MSTypeVector (const BaseVectorUnsignedLong & vect_)
  : BuiltinVectorUnsignedLong ((BuiltinVectorUnsignedLong &)vect_)
{
}


MSTypeVector<unsigned long>::MSTypeVector (const char *pString_) : BuiltinVectorUnsignedLong (pString_)
{
}

  
MSTypeVector<unsigned long>::MSTypeVector (MSTypeData<unsigned long, MSAllocator<unsigned long> > *pData_,
					   unsigned int len_)
  : BuiltinVectorUnsignedLong (pData_, len_)
{
}


MSTypeVector<unsigned long>::MSTypeVector (const unsigned long *pElements_, unsigned int len_)
  : BuiltinVectorUnsignedLong (pElements_, len_)
{
}


MSTypeVector<unsigned long>::~MSTypeVector()
{
}


MSTypeVector<unsigned long> & MSTypeVector<unsigned long>::operator= (const MSTypeVector<unsigned long> & vect_)
{
  return (MSTypeVector<unsigned long> &) BuiltinVectorUnsignedLong::operator= (vect_);
}


MSTypeVector<unsigned long> & MSTypeVector<unsigned long>::operator= (const BuiltinVectorUnsignedLong & vect_)
{
  return (*this = (MSTypeVector<unsigned long> &)vect_);
}


MSTypeVector<unsigned long> & MSTypeVector<unsigned long>::operator= (const BaseVectorUnsignedLong & vect_)
{
  return (*this = (MSTypeVector<unsigned long> &)vect_);
}


MSTypeVector<unsigned long> & MSTypeVector<unsigned long>::operator= (const unsigned long & value_)
{
  return (MSTypeVector<unsigned long> &) BuiltinVectorUnsignedLong::operator= (value_);
}


MSTypeVector<unsigned long> & MSTypeVector<unsigned long>::operator= (const char *pString_)
{
  return (*this = MSTypeVector<unsigned long>(pString_));
//  return (MSTypeVector<unsigned long> &) BuiltinVectorUnsignedLong::operator= (pString_);
}


MSString MSTypeVector<unsigned long>::className() const
{
  return MSString ("MSTypeVector<unsigned long>");
}


const MSSymbol & MSTypeVector<unsigned long>::type() const
{
  return symbol();
}


MSModel * MSTypeVector<unsigned long>::clone() const
{
  return new MSTypeVector<unsigned long> (*this);
}


MSModel * MSTypeVector<unsigned long>::create() const
{
  return new MSTypeVector<unsigned long>;
}


const MSSymbol & MSTypeVector<unsigned long>::symbol()
{
  static MSSymbol sym ("MSTypeVector<unsigned long>");
  return sym;
}




