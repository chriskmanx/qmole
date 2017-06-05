#ifndef MSBaseTypeVectorINLINES
#define MSBaseTypeVectorINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution
//
///////////////////////////////////////////////////////////////////////////////



// The version of EDG used by SGI's dcc compiler has a bug related to access from friends during manual instantiation;
// therefore, for SGI, make operator<<() inline so that it does not need to be manually instantiated in the library.
#if defined(MS_TEMPLATE_MANUAL_FRIEND_BUG)
#include <iostream.h>
#endif  //MS_MANUAL_FRIEND_ACCESS_BUG

#include <MSTypes/MSIndexVector.H>
#include <MSTypes/MSBinaryVector.H>
#include <MSTypes/MSVectorImpl.H>


#ifndef MS_NO_INLINES
 #include <MSTypes/MSVectorImplInlines.C>
#endif // MS_NO_INLINES


template <class Type, class Allocator>
INLINELINKAGE unsigned int MSBaseVector<Type,Allocator>::length() const
{
//   // length() method might potentially get called AFTER the destructor of MSBaseVector<Type,Allocator>;
//   // therefore, safeguard yourself here by checking if _pImpl has already been deleted...
   return _pImpl ? _pImpl->length() : 0;
}

template <class Type, class Allocator>
INLINELINKAGE MSBaseVector<Type,Allocator>::MSBaseVector (MSVectorImpl *pImpl_) : MSVector(), _pImpl(pImpl_), _blocked(MSFalse)
{
}


template <class Type, class Allocator>
INLINELINKAGE unsigned int MSBaseVector<Type,Allocator>::indexOf (const Type & value_, unsigned int startPos_) const
{
  return _pImpl->indexOf ((void *)&value_, startPos_);
}


template <class Type, class Allocator>
INLINELINKAGE unsigned int MSBaseVector<Type,Allocator>::lastIndexOf (const Type & value_, unsigned int startPos_) const
{
  return _pImpl->lastIndexOf ((void *)&value_, startPos_);
}


template <class Type, class Allocator>
INLINELINKAGE unsigned int MSBaseVector<Type,Allocator>::occurrencesOf (const Type & value_, unsigned int startPos_) const
{
  return _pImpl->occurrencesOf ((void *)&value_, startPos_);
}


template <class Type, class Allocator>
INLINELINKAGE MSIndexVector MSBaseVector<Type,Allocator>::indicesOf (const MSBaseVector<Type,Allocator> &v_) const
{
  return _pImpl->indicesOf (*v_._pImpl);
}


template <class Type, class Allocator>
INLINELINKAGE MSBinaryVector MSBaseVector<Type,Allocator>::memberOf (const MSBaseVector<Type,Allocator> &v_) const
{
  return _pImpl->memberOf (*v_._pImpl);
}


template <class Type, class Allocator>
INLINELINKAGE MSBinaryVector MSBaseVector<Type,Allocator>::uniqueElements() const
{
  return _pImpl->unique();
}


template <class Type, class Allocator>
INLINELINKAGE MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::replaceAt (unsigned int index_, const Type & aType_)
{
  set (index_, aType_);
  return *this;
}       


template <class Type, class Allocator>
INLINELINKAGE MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::selectUnique()
{
  return compress (_pImpl->unique());
}
  

template <class Type, class Allocator>
INLINELINKAGE MSBaseVector<Type,Allocator> MSBaseVector<Type,Allocator>::selectUnique (const MSBaseVector<Type,Allocator> & vect_)
{
  return compress (vect_, vect_._pImpl->unique());
}


template <class Type, class Allocator>
INLINELINKAGE MSTypeData<Type,Allocator> * MSBaseVector<Type,Allocator>::vectorData() const
{
  return (MSTypeData<Type,Allocator>*)_pImpl->data();
}


template <class Type, class Allocator>
INLINELINKAGE unsigned int MSBaseVector<Type,Allocator>::maxLength() const
{
  return _pImpl->maxLength();
}


template <class Type, class Allocator>
INLINELINKAGE unsigned int MSBaseVector<Type,Allocator>::size() const
{
  return vectorData()->size();
}


template <class Type, class Allocator>
INLINELINKAGE MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::sortUp()
{
  permute (gradeUp());
  return *this;
}


template <class Type, class Allocator>
INLINELINKAGE MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::sortDown()
{
  permute (gradeDown());
  return *this;
}


#if defined(MS_TEMPLATE_MANUAL_FRIEND_BUG)
// The version of EDG used by SGI's dcc compiler has a bug related to access from friends during manual instantiation;
// therefore, for SGI, make operator<<() inline so that it does not need to be manually instantiated in the library.
template <class Type, class Allocator>
INLINELINKAGE ostream & operator<< (ostream & stream_, const MSBaseVector<Type,Allocator> & vect_)
{
  vect_._pImpl->print (stream_);
  return stream_;
}
#endif  //MS_TEMPLATE_INLINE_FRIEND_BUG

template <class Type, class Allocator>
INLINELINKAGE long MSBaseVector<Type,Allocator>::compare (const MSBaseVector<Type,Allocator> & vect_) const
{
  return _pImpl->compare (*vect_._pImpl);
}


template <class Type, class Allocator>
INLINELINKAGE MSBinaryVector MSBaseVector<Type,Allocator>::binaryCompare (const MSBaseVector<Type,Allocator> & vect_,
								MSComparison comp_) const
{
  return _pImpl->binaryCompare (*vect_._pImpl, comp_);
}


template <class Type, class Allocator>
INLINELINKAGE MSBinaryVector MSBaseVector<Type,Allocator>::binaryCompare (const Type & value_,
								MSComparison comp_) const
{
  return _pImpl->binaryCompare ((void *)&value_, comp_);
}


template <class Type, class Allocator>
INLINELINKAGE MSBoolean operator< (const MSBaseVector<Type,Allocator> & vect1_, const MSBaseVector<Type,Allocator> & vect2_)
{
  return MSBoolean (vect1_.compare(vect2_) < 0);
}


template <class Type, class Allocator>
INLINELINKAGE MSBoolean operator> (const MSBaseVector<Type,Allocator> & vect1_, const MSBaseVector<Type,Allocator> & vect2_)
{
  return MSBoolean (vect1_.compare(vect2_) > 0);
}


template <class Type, class Allocator>
INLINELINKAGE MSBoolean operator<= (const MSBaseVector<Type,Allocator> & vect1_, const MSBaseVector<Type,Allocator> & vect2_)
{
  return MSBoolean (vect1_.compare(vect2_) <= 0);
}


template <class Type, class Allocator>
INLINELINKAGE MSBoolean operator>= (const MSBaseVector<Type,Allocator> & vect1_, const MSBaseVector<Type,Allocator> & vect2_)
{
  return MSBoolean (vect1_.compare(vect2_) >= 0);
}


template <class Type, class Allocator>
INLINELINKAGE MSBoolean operator< (const MSBaseVector<Type,Allocator> & vect_, const Type & value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSLessThan);
}


template <class Type, class Allocator>
INLINELINKAGE MSBoolean operator< (const Type & value_, const MSBaseVector<Type,Allocator> & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSGreaterThan);
}


template <class Type, class Allocator>
INLINELINKAGE MSBoolean operator> (const MSBaseVector<Type,Allocator> & vect_, const Type & value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSGreaterThan);
}


template <class Type, class Allocator>
INLINELINKAGE MSBoolean operator> (const Type & value_, const MSBaseVector<Type,Allocator> & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSLessThan);
}


template <class Type, class Allocator>
INLINELINKAGE MSBoolean operator<= (const MSBaseVector<Type,Allocator> & vect_, const Type & value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSLessThanOrEqualTo);
}


template <class Type, class Allocator>
INLINELINKAGE MSBoolean operator<= (const Type & value_, const MSBaseVector<Type,Allocator> & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSGreaterThanOrEqualTo);
}


template <class Type, class Allocator>
INLINELINKAGE MSBoolean operator>= (const MSBaseVector<Type,Allocator> & vect_, const Type & value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSGreaterThanOrEqualTo);
}


template <class Type, class Allocator>
INLINELINKAGE MSBoolean operator>= (const Type & value_, const MSBaseVector<Type,Allocator> & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSLessThanOrEqualTo);
}


template <class Type, class Allocator>
INLINELINKAGE MSBoolean operator== (const MSBaseVector<Type,Allocator> & vect_, const Type & value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSEqualTo);
}


template <class Type, class Allocator>
INLINELINKAGE MSBoolean operator== (const Type & value_, const MSBaseVector<Type,Allocator> & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSEqualTo);
}


template <class Type, class Allocator>
INLINELINKAGE MSBoolean operator!= (const MSBaseVector<Type,Allocator> & vect_, const Type & value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSNotEqualTo);
}


template <class Type, class Allocator>
INLINELINKAGE MSBoolean operator!= (const Type & value_, const MSBaseVector<Type,Allocator> & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSNotEqualTo);
}


template <class Type, class Allocator>
INLINELINKAGE MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::operator<<= (const MSBaseVector<Type,Allocator> & vect_)
{
  return append (vect_);
}


template <class Type, class Allocator>
INLINELINKAGE MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::operator<<= (const Type & value_)
{
  return append (value_);
}


template <class Type, class Allocator>
INLINELINKAGE MSBoolean operator==(const MSBaseVector<Type,Allocator> &a_, const MSBaseVector<Type,Allocator> & b_)
{
  return MSBoolean (a_.compare(b_) == 0);
}

template <class Type, class Allocator>
INLINELINKAGE MSBoolean operator!=(const MSBaseVector<Type,Allocator> &a_, const MSBaseVector<Type,Allocator> & b_)
{
  return MSBoolean (a_.compare(b_) != 0);
}


template <class Type, class Allocator>
INLINELINKAGE MSBaseVector<Type,Allocator>& operator<<( MSBaseVector<Type,Allocator> & a_, const MSBaseVector<Type,Allocator> & b_)
{
  return a_.append(b_);
}

template <class Type, class Allocator>
INLINELINKAGE MSBaseVector<Type,Allocator>& operator<<( MSBaseVector<Type,Allocator> & vect_, const Type & value_)
{
  return vect_.append(value_);
}

template <class Type>
INLINELINKAGE unsigned int msVectorElementLength (const Type &)
{
  return 0;
}

inline unsigned int msVectorElementLength (const MSString & str_)
{
  return str_.length();
}


inline unsigned int msVectorElementLength (const MSSymbol & sym_)
{
  const char *pString = sym_.symbolName();
  return (pString) ? strlen (pString) : 0;
}


#endif  // MSBaseTypeVectorINLINES
