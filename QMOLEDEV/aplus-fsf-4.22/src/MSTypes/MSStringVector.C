///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDefines.H>

#if defined(MS_FORCE_INTERNAL_TEMPLATE_INSTANTIATION)
#define MS_MSStringVector_INSTANTIATE
#define MSTK_MANUAL_INSTANTIATION
#endif


#include <MSTypes/MSStringVector.H>
#include <MSTypes/MSGlobalInlines.H>

#ifdef MS_NO_INLINES
#include <MSTypes/MSIntVectorInlines.C>
#endif // MS_NO_INLINES

#if defined(MSTK_MANUAL_INSTANTIATION)

#include <MSTypes/MSObjectTypeVector.C>
#include <MSTypes/MSTypeData.C>

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeVector<MSString>)
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)

#pragma instantiate MSBaseVector<MSString,MSVectorModelAllocator<MSString> >
#pragma instantiate MSObjectVector<MSString>
#if !defined (MS_TEMPLATE_MANUAL_FRIEND_BUG)
// The version of EDG used by SGI's dcc compiler has a bug related to access from friends
#pragma instantiate ostream & operator<< (ostream &, const MSBaseVector<MSString,MSVectorModelAllocator<MSString> > &)
#endif  // MS_TEMPLATE_MANUAL_FRIEND_BUG
#endif  // MS_EDG_TEMPLATE_INSTANTIATION

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSBaseVector<MSString,MSVectorModelAllocator<MSString> >;
template MSBaseVectorOps<MSString,MSVectorModelAllocator<MSString> >;
template MSObjectVector<MSString>;
template MSVectorElement<MSString>;
// instantiate non-inline friend template functions
static int __dummy01__()  {  if (0) operator<<(cout, MSBaseVector<MSString,MSVectorModelAllocator<MSString> >()); return 0;  }
static int __dummyInt01__=__dummy01__();
#endif  // MS_VC_TEMPLATE_INSTANTIATION



#if defined(MSTK_MANUAL_INSTANTIATION)

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSBaseVector<MSString,MSVectorModelAllocator<MSString> >;
template class MSBaseVectorOps<MSString,MSVectorModelAllocator<MSString> >;
template class MSObjectVector<MSString>;
template class MSVectorElement<MSString>;
template ostream& operator<<(ostream&,const MSBaseVector<MSString,MSVectorModelAllocator<MSString> >&);
#endif

#endif

#endif // MSTK_MANUAL_INSTANTIATION

MSTypeVector<MSString>::MSTypeVector() : ObjectVectorString()
{
}


MSTypeVector<MSString>::MSTypeVector (unsigned int length_) : ObjectVectorString (length_)
{
}


MSTypeVector<MSString>::MSTypeVector (unsigned int length_, const MSString & filler_)
  : ObjectVectorString (length_, filler_)
{
}


MSTypeVector<MSString>::MSTypeVector (const MSTypeVector<MSString> & vect_) : ObjectVectorString (vect_)
{
}


MSTypeVector<MSString>::MSTypeVector (const BaseVectorString & vect_) : ObjectVectorString (vect_)
{
}


MSTypeVector<MSString>::MSTypeVector (const char *pString_, const char delimiter_) : ObjectVectorString (pString_, delimiter_)
{
}


MSTypeVector<MSString>::MSTypeVector (MSTypeData<MSString,MSVectorModelAllocator<MSString> > *pData_, unsigned int len_)
  : ObjectVectorString (pData_, len_)
{
}


MSTypeVector<MSString>::MSTypeVector (const MSString *pElements_, unsigned int len_)
  : ObjectVectorString (pElements_, len_)
{
}


MSTypeVector<MSString>::~MSTypeVector()
{
}


MSTypeVector<MSString> & MSTypeVector<MSString>::operator= (const MSTypeVector<MSString> & vect_)
{
  return (MSTypeVector<MSString> &) ObjectVectorString::operator= (vect_);
}


MSTypeVector<MSString> & MSTypeVector<MSString>::operator= (const BaseVectorString & vect_)
{
  return (*this = (MSTypeVector<MSString> &)vect_);
}


MSTypeVector<MSString> & MSTypeVector<MSString>::operator= (const MSString & value_)
{
  return (MSTypeVector<MSString> &) ObjectVectorString::operator= ((const char *)value_);
}


MSTypeVector<MSString> & MSTypeVector<MSString>::operator= (const char *pString_)
{
  return (MSTypeVector<MSString> &) ObjectVectorString::operator= (pString_);
}


MSString MSTypeVector<MSString>::asString() const
{
  return MSBaseVector<MSString,MSVectorModelAllocator<MSString> >::asString();
}


MSString MSTypeVector<MSString>::className() const
{
  return MSString ("MSTypeVector<MSString>");
}


const MSSymbol & MSTypeVector<MSString>::type() const
{
  return symbol();
}


MSModel * MSTypeVector<MSString>::clone() const
{
  return new MSTypeVector<MSString> (*this);
}


MSModel * MSTypeVector<MSString>::create() const
{
  return new MSTypeVector<MSString>;
}


MSError::ErrorStatus MSTypeVector<MSString>::set (const char *pString_)
{
  return set (pString_, '\n');
}


MSError::ErrorStatus MSTypeVector<MSString>::set (const char *pString_, const char delimiter_)
{
  MSError::ErrorStatus retval = _pImpl->setFromString (pString_, delimiter_);
  changed();
  return retval;
}


const MSSymbol & MSTypeVector<MSString>::symbol()
{
  static MSSymbol sym ("MSTypeVector<MSString>");
  return sym;
}


MSTypeVector<MSString> & MSTypeVector<MSString>::append (const char *pString_, const char delimiter_)
{
  unsigned int numEls = _pImpl->append (pString_, delimiter_);
  if (numEls)
    appendUpdate (_pImpl->length(), numEls);

  return *this;
}

