#ifndef MSIndexVectorINLINES
#define MSIndexVectorINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSMathDeclare.H>

#include <MSTypes/MSBinaryVector.H>
#include <MSTypes/MSBuiltinVectorImpl.H>

#ifndef MS_NO_INLINES
#include <MSTypes/MSBuiltinVectorImplInlines.C>
#endif  //MS_NO_INLINES

INLINELINKAGE unsigned int MSIndexVector::size() const
{
  return ((Data *)_pImpl->data())->size();  
}


INLINELINKAGE unsigned int MSIndexVector::indexOf (const unsigned int value_, unsigned int startPos_) const
{
  return _pImpl->indexOf ((void *)&value_, startPos_);
}


INLINELINKAGE unsigned int MSIndexVector::lastIndexOf (const unsigned int value_, unsigned int startPos_) const
{
  return _pImpl->lastIndexOf ((void *)&value_, startPos_);
}
  

INLINELINKAGE unsigned int MSIndexVector::occurrencesOf (const unsigned int value_, unsigned int startPos_) const
{
  return _pImpl->occurrencesOf ((const void *)&value_, startPos_);
}


INLINELINKAGE MSIndexVector MSIndexVector::indicesOf (const MSIndexVector &v_) const
{
  return _pImpl->indicesOf (*v_._pImpl);
}


INLINELINKAGE MSBinaryVector MSIndexVector::memberOf (const MSIndexVector &v_) const
{
  return _pImpl->memberOf (*v_._pImpl);
}


INLINELINKAGE MSBinaryVector MSIndexVector::uniqueElements() const
{
  return _pImpl->unique();
}


INLINELINKAGE MSIndexVector & MSIndexVector::replaceAt (unsigned int index_, const unsigned int value_)
{
  set (index_, value_);
  return *this;
}


INLINELINKAGE MSIndexVector & MSIndexVector::selectUnique()
{
  return compress (_pImpl->unique());
}
  

INLINELINKAGE MSIndexVector MSIndexVector::selectUnique (const MSIndexVector & vect_)
{
  return compress (vect_, vect_._pImpl->unique());
}


INLINELINKAGE MSIndexVector::Data *MSIndexVector::vectorData() const
{
  return (Data *)_pImpl->data();
}


INLINELINKAGE MSIndexVector & MSIndexVector::sortUp()
{
  permute (gradeUp());
  return *this;
}


INLINELINKAGE MSIndexVector & MSIndexVector::sortDown()
{
  permute (gradeDown());
  return *this;
}


INLINELINKAGE unsigned int MSIndexVector::firstElement() const
{
  return elementAt (0);
}


INLINELINKAGE unsigned int MSIndexVector::lastElement() const
{
  return elementAt (_pImpl->length() -1);
}


INLINELINKAGE unsigned int MSIndexVector::operator[] (unsigned int index_) const
{
  return elementAt (index_);
}


INLINELINKAGE unsigned int MSIndexVector::operator() (unsigned int index_) const
{
  return elementAt (index_);
}


INLINELINKAGE const unsigned int & MSIndexVector::elementAt (unsigned int index_) const
{
#ifndef MSPRODUCTION_BUILD
  if (index_ >= _pImpl->length())
    {
      _pImpl->vectorIndexError (index_);
      return *(const unsigned int *)ops().badData();
    }
#endif  //MSPRODUCTION_BUILD
  return data()[index_];
}


INLINELINKAGE MSIndexVector::SPick MSIndexVector::operator[] (unsigned int index_)
{
  return SPick (*this, index_);
}


INLINELINKAGE MSIndexVector MSIndexVector::operator[] (const MSIndexVector & iVect_) const
{
  return select (*this, iVect_);
}


INLINELINKAGE MSIndexVector MSIndexVector::operator[] (const MSBinaryVector & bVect_) const
{
  return compress (*this, bVect_);
}


INLINELINKAGE long MSIndexVector::compare (const MSIndexVector & vect_) const
{
  return _pImpl->compare (*vect_._pImpl);
}


INLINELINKAGE MSBinaryVector MSIndexVector::binaryCompare (const MSIndexVector & vect_, MSComparison comp_) const
{
  return _pImpl->binaryCompare (*vect_._pImpl, comp_);
}


INLINELINKAGE MSBinaryVector MSIndexVector::binaryCompare (const unsigned int value_, MSComparison comp_) const
{
  return _pImpl->binaryCompare ((void *)&value_, comp_);
}


INLINELINKAGE MSBoolean MSIndexVector::operator== (const MSIndexVector & vect_) const
{
  return MSBoolean (compare(vect_) == 0);
}


INLINELINKAGE MSBoolean MSIndexVector::operator!= (const MSIndexVector & vect_) const
{
  return MSBoolean (compare(vect_) != 0);
}


INLINELINKAGE MSBoolean operator< (const MSIndexVector & vect1_, const MSIndexVector & vect2_)
{
  return MSBoolean (vect1_.compare(vect2_) < 0);
}


INLINELINKAGE MSBoolean operator> (const MSIndexVector & vect1_, const MSIndexVector & vect2_)
{
  return MSBoolean (vect1_.compare(vect2_) > 0);
}


INLINELINKAGE MSBoolean operator<= (const MSIndexVector & vect1_, const MSIndexVector & vect2_)
{
  return MSBoolean (vect1_.compare(vect2_) <= 0);
}


INLINELINKAGE MSBoolean operator>= (const MSIndexVector & vect1_, const MSIndexVector & vect2_)
{
  return MSBoolean (vect1_.compare(vect2_) >= 0);
}


INLINELINKAGE MSBoolean operator< (const MSIndexVector & vect_, const unsigned int value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSLessThan);
}


INLINELINKAGE MSBoolean operator< (const unsigned int value_, const MSIndexVector & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSGreaterThan);
}


INLINELINKAGE MSBoolean operator> (const MSIndexVector & vect_, const unsigned int value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSGreaterThan);
}


INLINELINKAGE MSBoolean operator> (const unsigned int value_, const MSIndexVector & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSLessThan);
}


INLINELINKAGE MSBoolean operator<= (const MSIndexVector & vect_, const unsigned int value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSLessThanOrEqualTo);
}


INLINELINKAGE MSBoolean operator<= (const unsigned int value_, const MSIndexVector & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSGreaterThanOrEqualTo);
}


INLINELINKAGE MSBoolean operator>= (const MSIndexVector & vect_, const unsigned int value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSGreaterThanOrEqualTo);
}


INLINELINKAGE MSBoolean operator>= (const unsigned int value_, const MSIndexVector & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSLessThanOrEqualTo);
}


INLINELINKAGE MSBoolean operator== (const MSIndexVector & vect_, const unsigned int value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSEqualTo);
}


INLINELINKAGE MSBoolean operator== (const unsigned int value_, const MSIndexVector & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSEqualTo);
}


INLINELINKAGE MSBoolean operator!= (const MSIndexVector & vect_, const unsigned int value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSNotEqualTo);
}


INLINELINKAGE MSBoolean operator!= (const unsigned int value_, const MSIndexVector & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSNotEqualTo);
}


INLINELINKAGE MSIndexVector & MSIndexVector::operator<<= (const unsigned int value_)
{
  return append (value_);
}


INLINELINKAGE MSIndexVector & MSIndexVector::operator<<= (const MSIndexVector & vect_)
{
  return append (vect_);
}


INLINELINKAGE MSIndexVector & MSIndexVector::operator<< (const unsigned int value_)
{
  return append (value_);
}


INLINELINKAGE MSIndexVector & MSIndexVector::operator<< (const MSIndexVector & vect_)
{
  return append (vect_);
}


INLINELINKAGE MSIndexVector& MSIndexVector::series(void)
{
  return series(_pImpl->length());
}


// Postfix increment operator
INLINELINKAGE MSIndexVector MSIndexVector::operator++ (int)
{
  MSBuiltinVectorImpl *pResImpl = (MSBuiltinVectorImpl *)_pImpl->clone();
  return operator++(), MSIndexVector(pResImpl);	// call the prefix operator and return the old data
}


// Postfix decrement operator
INLINELINKAGE MSIndexVector MSIndexVector::operator-- (int)
{
  MSBuiltinVectorImpl *pResImpl = (MSBuiltinVectorImpl *)_pImpl->clone();
  return operator--(), MSIndexVector(pResImpl);	// call the prefix operator and return the old data
}


INLINELINKAGE double MSIndexVector::avg() const
{
  return sum()/_pImpl->length();
}


INLINELINKAGE double MSIndexVector::mean() const
{
  return sum()/_pImpl->length();
}


INLINELINKAGE double MSIndexVector::median() const
{
  return _pImpl->median();
}


INLINELINKAGE double MSIndexVector::variance() const
{
  return _pImpl->variance(mean());
}


INLINELINKAGE double MSIndexVector::stdDeviation() const
{
  return ::sqrt (variance());
}


INLINELINKAGE MSIndexVector movingAverage (const MSIndexVector & vect_, unsigned int width_)
{
  return MSIndexVector (vect_._pImpl->movingAverage (width_));
}


INLINELINKAGE MSIndexVector operator+ (const unsigned int value_, const MSIndexVector & vect_)
{
  return vect_+value_;
}


INLINELINKAGE MSIndexVector operator* (const unsigned int value_, const MSIndexVector & vect_)
{
  return vect_*value_;
}


INLINELINKAGE MSIndexVector::Data * MSIndexVector::Data::incrementCount() 
{
  _refCount++;
  return this;
}


INLINELINKAGE void MSIndexVector::Data::decrementCount() 
{
  if (--_refCount == 0)
    deallocate();
}


INLINELINKAGE const unsigned int * MSIndexVector::Data::elements() const
{
  return &_pElements[0];
}


// same as elements(); used for backward compatibility only
INLINELINKAGE const unsigned int * MSIndexVector::Data::data() const
{
  return &_pElements[0];
}


INLINELINKAGE unsigned int * MSIndexVector::Data::elements()
{
  return &_pElements[0];
}
  

// same as elements(); used for backward compatibility only
INLINELINKAGE unsigned int * MSIndexVector::Data::data()
{
  return &_pElements[0];
}
  

INLINELINKAGE const unsigned int & MSIndexVector::Data::elementAt (unsigned index_) const
{
  return elements()[index_];
}


INLINELINKAGE unsigned int & MSIndexVector::Data::elementAt (unsigned index_)
{
  return elements()[index_];  
}


INLINELINKAGE MSIndexVector::SPick::SPick (MSIndexVector & aVector_, unsigned int index_)
  : _pVector(&aVector_), _index(index_)
{ 
}


INLINELINKAGE MSIndexVector::SPick::SPick (const SPick & aPick_)
  : _pVector(aPick_._pVector), _index(aPick_._index)
{
}


// prefix increment
INLINELINKAGE MSIndexVector::SPick & MSIndexVector::SPick::operator++()
{
  _pVector->set (_index, (*_pVector)(_index) +1);
  return *this;
}


// prefix decrement
INLINELINKAGE MSIndexVector::SPick & MSIndexVector::SPick::operator--()
{
  _pVector->set (_index, (*_pVector)(_index) -1);
  return *this;
}


INLINELINKAGE MSIndexVector::SPick & MSIndexVector::SPick::operator+=  (const unsigned int value_)
{
  _pVector->set (_index, (*_pVector)(_index) + value_);
  return *this;
}


INLINELINKAGE MSIndexVector::SPick & MSIndexVector::SPick::operator-=  (const unsigned int value_)
{
  _pVector->set (_index, (*_pVector)(_index) - value_);
  return *this;
}


INLINELINKAGE MSIndexVector::SPick & MSIndexVector::SPick::operator*=  (const unsigned int value_)
{
  _pVector->set (_index, (*_pVector)(_index) * value_);
  return *this;
}


INLINELINKAGE MSIndexVector::SPick & MSIndexVector::SPick::operator/=  (const unsigned int value_)
{
  _pVector->set (_index, (*_pVector)(_index) / value_);
  return *this;
}


INLINELINKAGE MSIndexVector::SPick & MSIndexVector::SPick::operator%=  (const unsigned int value_)
{
  _pVector->set (_index, (*_pVector)(_index) % value_);
  return *this;
}


INLINELINKAGE MSIndexVector::SPick & MSIndexVector::SPick::operator<<= (const unsigned int value_)
{
  _pVector->set (_index, (*_pVector)(_index) << value_);
  return *this;
}


INLINELINKAGE MSIndexVector::SPick & MSIndexVector::SPick::operator>>= (const unsigned int value_)
{
  _pVector->set (_index, (*_pVector)(_index) >> value_);
  return *this;
}


INLINELINKAGE MSIndexVector::SPick & MSIndexVector::SPick::operator&=  (const unsigned int value_)
{
  _pVector->set (_index, (*_pVector)(_index) & value_);
  return *this;
}


INLINELINKAGE MSIndexVector::SPick & MSIndexVector::SPick::operator|=  (const unsigned int value_)
{
  _pVector->set (_index, (*_pVector)(_index) | value_);
  return *this;
}


INLINELINKAGE MSIndexVector::SPick & MSIndexVector::SPick::operator^=  (const unsigned int value_)
{
  _pVector->set (_index, (*_pVector)(_index) ^ value_);
  return *this;
}


INLINELINKAGE MSIndexVector::SPick::operator unsigned int() const
{
  return (*_pVector)(_index);
}

#endif  // MSIndexVectorINLINES
