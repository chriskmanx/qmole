#ifndef MSBinaryVectorINLINES
#define MSBinaryVectorINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSIndexVector.H>
#include <MSTypes/MSVectorImpl.H>

#ifndef MS_NO_INLINES
#include <MSTypes/MSVectorImplInlines.C>
#endif  //MS_NO_INLINES

INLINELINKAGE unsigned int MSBinaryVector::size() const
{
  return ((Data *)_pImpl->data())->size();
}


INLINELINKAGE unsigned int MSBinaryVector::indexOf (const unsigned char value_, unsigned int startPos_) const
{
  unsigned char c = value_ ? 1 : 0;
  return _pImpl->indexOf ((void *)&c, startPos_);
}


INLINELINKAGE unsigned int MSBinaryVector::lastIndexOf (const unsigned char value_, unsigned int startPos_) const
{
  unsigned char c = value_ ? 1 : 0;
  return _pImpl->lastIndexOf ((void *)&c, startPos_);
}
  

INLINELINKAGE unsigned int MSBinaryVector::occurrencesOf (const unsigned char value_, unsigned int startPos_) const
{
  unsigned char c = value_ ? 1 : 0;
  return _pImpl->occurrencesOf ((const void *)&c, startPos_);
}


INLINELINKAGE MSIndexVector MSBinaryVector::indicesOf (const MSBinaryVector &v_) const
{
  return _pImpl->indicesOf (*v_._pImpl);
}


INLINELINKAGE MSBinaryVector MSBinaryVector::memberOf (const MSBinaryVector &v_) const
{
  return _pImpl->memberOf (*v_._pImpl);
}


INLINELINKAGE MSBinaryVector & MSBinaryVector::replaceAt (unsigned int index_, const unsigned char value_)
{
  set (index_, value_);
  return *this;
}


INLINELINKAGE void MSBinaryVector::permute (const MSIndexVector & iVect_)
{
  _pImpl->permute (iVect_);
  changed();
}


INLINELINKAGE unsigned char MSBinaryVector::firstElement() const
{
  return elementAt (0);
}


INLINELINKAGE unsigned char MSBinaryVector::lastElement() const
{
  return elementAt (_pImpl->length() -1);
}


INLINELINKAGE unsigned char MSBinaryVector::operator[] (unsigned int index_) const
{
  return elementAt (index_);
}


INLINELINKAGE unsigned char MSBinaryVector::operator() (unsigned int index_) const
{
  return elementAt (index_);
}


INLINELINKAGE const unsigned char & MSBinaryVector::elementAt (unsigned int index_) const
{
#if !defined(MS_NO_INDEX_ERROR)
  if (index_ >= _pImpl->length())
    {
      _pImpl->vectorIndexError (index_);
      return *(const unsigned char *)ops().badData();
    }
#endif  //!MS_NO_INDEX_ERROR
  return data()[index_];
}


INLINELINKAGE MSBinaryVector::SPick MSBinaryVector::operator[] (unsigned int index_)
{
  return SPick (*this, index_);
}


INLINELINKAGE MSBinaryVector MSBinaryVector::operator[] (const MSIndexVector & iVect_) const
{
  return select (*this, iVect_);
}


INLINELINKAGE MSBinaryVector MSBinaryVector::operator[] (const MSBinaryVector & bVect_) const
{
  return compress (*this, bVect_);
}


INLINELINKAGE long MSBinaryVector::compare (const MSBinaryVector & vect_) const
{
  return _pImpl->compare (*vect_._pImpl);
}


INLINELINKAGE MSBinaryVector MSBinaryVector::binaryCompare (const MSBinaryVector & vect_, MSComparison comp_) const
{
  return _pImpl->binaryCompare (*vect_._pImpl, comp_);
}


INLINELINKAGE MSBinaryVector MSBinaryVector::binaryCompare (const unsigned char value_, MSComparison comp_) const
{
  return _pImpl->binaryCompare ((void *)&value_, comp_);
}


INLINELINKAGE MSBoolean MSBinaryVector::operator== (const MSBinaryVector & vect_) const
{
  return MSBoolean (compare(vect_) == 0);
}


INLINELINKAGE MSBoolean MSBinaryVector::operator!= (const MSBinaryVector & vect_) const
{
  return MSBoolean (compare(vect_) != 0);
}


INLINELINKAGE MSBoolean operator< (const MSBinaryVector & vect1_, const MSBinaryVector & vect2_)
{
  return MSBoolean (vect1_.compare(vect2_) < 0);
}


INLINELINKAGE MSBoolean operator> (const MSBinaryVector & vect1_, const MSBinaryVector & vect2_)
{
  return MSBoolean (vect1_.compare(vect2_) > 0);
}


INLINELINKAGE MSBoolean operator<= (const MSBinaryVector & vect1_, const MSBinaryVector & vect2_)
{
  return MSBoolean (vect1_.compare(vect2_) <= 0);
}


INLINELINKAGE MSBoolean operator>= (const MSBinaryVector & vect1_, const MSBinaryVector & vect2_)
{
  return MSBoolean (vect1_.compare(vect2_) >= 0);
}


INLINELINKAGE MSBoolean operator< (const MSBinaryVector & vect_, const unsigned char value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSLessThan);
}


INLINELINKAGE MSBoolean operator< (const unsigned char value_, const MSBinaryVector & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSGreaterThan);
}


INLINELINKAGE MSBoolean operator> (const MSBinaryVector & vect_, const unsigned char value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSGreaterThan);
}


INLINELINKAGE MSBoolean operator> (const unsigned char value_, const MSBinaryVector & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSLessThan);
}


INLINELINKAGE MSBoolean operator<= (const MSBinaryVector & vect_, const unsigned char value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSLessThanOrEqualTo);
}


INLINELINKAGE MSBoolean operator<= (const unsigned char value_, const MSBinaryVector & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSGreaterThanOrEqualTo);
}


INLINELINKAGE MSBoolean operator>= (const MSBinaryVector & vect_, const unsigned char value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSGreaterThanOrEqualTo);
}


INLINELINKAGE MSBoolean operator>= (const unsigned char value_, const MSBinaryVector & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSLessThanOrEqualTo);
}


INLINELINKAGE MSBoolean operator== (const MSBinaryVector & vect_, const unsigned char value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSEqualTo);
}


INLINELINKAGE MSBoolean operator== (const unsigned char value_, const MSBinaryVector & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSEqualTo);
}


INLINELINKAGE MSBoolean operator!= (const MSBinaryVector & vect_, const unsigned char value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSNotEqualTo);
}


INLINELINKAGE MSBoolean operator!= (const unsigned char value_, const MSBinaryVector & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSNotEqualTo);
}


INLINELINKAGE MSBinaryVector & MSBinaryVector::operator<<= (const unsigned char value_)
{
  return append (value_);
}


INLINELINKAGE MSBinaryVector & MSBinaryVector::operator<<= (const MSBinaryVector & vect_)
{
  return append (vect_);
}


INLINELINKAGE MSBinaryVector & MSBinaryVector::operator<< (const unsigned char value_)
{
  return append (value_);
}


INLINELINKAGE MSBinaryVector & MSBinaryVector::operator<< (const MSBinaryVector & vect_)
{
  return append (vect_);
}

/***
INLINELINKAGE MSBinaryVector & operator<< (MSBinaryVector & vect1_, const MSBinaryVector & vect2_)
{
  return vect1_.append (vect2_);
}


INLINELINKAGE MSBinaryVector & operator<< (MSBinaryVector & vect_, const unsigned char value_)
{
  return vect_.append (value_);
}
***/  

INLINELINKAGE MSBinaryVector operator& (const MSBinaryVector & vect1_, const MSBinaryVector & vect2_)
{
  return MSBinaryVector::doBitwiseOp (vect1_, vect2_, ::bitwiseAND);
}


INLINELINKAGE MSBinaryVector operator& (const unsigned char value_, const MSBinaryVector & vect_)
{
  return MSBinaryVector::doBitwiseOp (vect_, value_, ::bitwiseAND);
}


INLINELINKAGE MSBinaryVector operator& (const MSBinaryVector & vect_, const unsigned char value_)
{
  return MSBinaryVector::doBitwiseOp (vect_, value_, ::bitwiseAND);
}


INLINELINKAGE MSBinaryVector & MSBinaryVector::operator&= (const MSBinaryVector & vect_)
{
  return doBitwiseOp (vect_, ::bitwiseAND, ::bitwiseAND);
}


INLINELINKAGE MSBinaryVector & MSBinaryVector::operator&= (const unsigned char value_)
{
  return doBitwiseOp (value_, ::bitwiseAND, ::bitwiseAND);
}


INLINELINKAGE MSBinaryVector operator| (const MSBinaryVector & vect1_, const MSBinaryVector & vect2_)
{
  return MSBinaryVector::doBitwiseOp (vect1_, vect2_, ::bitwiseOR);
}


INLINELINKAGE MSBinaryVector operator| (const unsigned char value_, const MSBinaryVector & vect_)
{
  return MSBinaryVector::doBitwiseOp (vect_, value_, ::bitwiseOR);
}


INLINELINKAGE MSBinaryVector operator| (const MSBinaryVector & vect_, const unsigned char value_)
{
  return MSBinaryVector::doBitwiseOp (vect_, value_, ::bitwiseOR);
}


INLINELINKAGE MSBinaryVector & MSBinaryVector::operator|= (const MSBinaryVector & vect_)
{
  return doBitwiseOp (vect_, ::bitwiseOR, ::bitwiseOR);
}


INLINELINKAGE MSBinaryVector & MSBinaryVector::operator|= (const unsigned char value_)
{
  return doBitwiseOp (value_, ::bitwiseOR, ::bitwiseOR);
}


INLINELINKAGE MSBinaryVector operator^ (const MSBinaryVector & vect1_, const MSBinaryVector & vect2_)
{
  return MSBinaryVector::doBitwiseOp (vect1_, vect2_, ::bitwiseXOR);
}


INLINELINKAGE MSBinaryVector operator^ (const unsigned char value_, const MSBinaryVector & vect_)
{
  return MSBinaryVector::doBitwiseOp (vect_, value_, ::bitwiseXOR);
}


INLINELINKAGE MSBinaryVector operator^ (const MSBinaryVector & vect_, const unsigned char value_)
{
  return MSBinaryVector::doBitwiseOp (vect_, value_, ::bitwiseXOR);
}


INLINELINKAGE MSBinaryVector & MSBinaryVector::operator^= (const MSBinaryVector & vect_)
{
  return doBitwiseOp (vect_, ::bitwiseXOR, ::bitwiseXOR);
}


INLINELINKAGE MSBinaryVector & MSBinaryVector::operator^= (const unsigned char value_)
{
  return doBitwiseOp (value_, ::bitwiseXOR, ::bitwiseXOR);
}


INLINELINKAGE MSBinaryVector operator~ (const MSBinaryVector & vect_)
{
  return ! vect_;
}


INLINELINKAGE MSBinaryVector::Data * MSBinaryVector::Data::incrementCount() 
{
  _refCount++;
  return this;
}


INLINELINKAGE void MSBinaryVector::Data::decrementCount() 
{
  if (--_refCount == 0)
    delete this;
}


INLINELINKAGE const unsigned char * MSBinaryVector::Data::elements() const
{
  return &_pElements[0];
}


// same as elements(); used for backward compatibility only
INLINELINKAGE const unsigned char * MSBinaryVector::Data::data() const
{
  return &_pElements[0];
}


INLINELINKAGE unsigned char * MSBinaryVector::Data::elements()
{
  return &_pElements[0];
}
  

// same as elements(); used for backward compatibility only
INLINELINKAGE unsigned char * MSBinaryVector::Data::data()
{
  return &_pElements[0];
}
  

INLINELINKAGE const unsigned char & MSBinaryVector::Data::elementAt (unsigned index_) const
{
  return elements()[index_];
}


INLINELINKAGE unsigned char & MSBinaryVector::Data::elementAt (unsigned index_)
{
  return elements()[index_];
}


INLINELINKAGE MSBinaryVector::SPick::SPick (MSBinaryVector & aVector_, unsigned int index_)
  : _index(index_), _pVector(&aVector_)
{ 
}


INLINELINKAGE MSBinaryVector::SPick::SPick (const SPick & aPick_)
  :  _index(aPick_._index), _pVector(aPick_._pVector)
{
}


INLINELINKAGE MSBinaryVector::SPick & MSBinaryVector::SPick::operator&=  (const unsigned char value_)
{
  _pVector->set (_index, (*_pVector)(_index) & value_);
  return *this;
}


INLINELINKAGE MSBinaryVector::SPick & MSBinaryVector::SPick::operator|=  (const unsigned char value_)
{
  _pVector->set (_index, (*_pVector)(_index) | value_);
  return *this;
}


INLINELINKAGE MSBinaryVector::SPick & MSBinaryVector::SPick::operator^=  (const unsigned char value_)
{
  _pVector->set (_index, (*_pVector)(_index) ^ value_);
  return *this;
}


INLINELINKAGE MSBinaryVector::SPick::operator unsigned char() const
{
  return (*_pVector)(_index);
}

#endif  // MSBinaryVectorINLINES
