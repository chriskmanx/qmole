#ifndef MSBuiltinTypeVectorInlinesHEADER
#define MSBuiltinTypeVectorInlinesHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSMathDeclare.H>
#include <MSTypes/MSString.H>
#include <MSTypes/MSBuiltinSPick.H>

#ifndef MS_NO_INLINES
#include <MSTypes/MSBuiltinVectorImplInlines.C>
#endif  // MS_NO_INLINES

template <class Type>
INLINELINKAGE Type MSBuiltinVector<Type>::firstElement() const
{
  return elementAt (0);
}


template <class Type>
INLINELINKAGE Type MSBuiltinVector<Type>::lastElement() const
{
  return elementAt (this->_pImpl->length() -1);
}


template <class Type>
INLINELINKAGE Type MSBuiltinVector<Type>::operator() (unsigned int index_) const
{
  return elementAt (index_);
}


template <class Type>
INLINELINKAGE Type MSBuiltinVector<Type>::operator[] (unsigned int index_) const
{
  return elementAt (index_);
}


template <class Type>
INLINELINKAGE const Type & MSBuiltinVector<Type>::elementAt (unsigned int index_) const
{
#if !defined(MS_NO_INDEX_ERROR)
  if (index_ >= this->_pImpl->length())
    {
      this->_pImpl->vectorIndexError (index_);
      return *(const Type *)this->ops().badData();
    }
#endif  //MS_NO_INDEX_ERROR
  return this->data()[index_];
}


template <class Type>
INLINELINKAGE MSBuiltinSPick<Type> MSBuiltinVector<Type>::operator[] (unsigned int index_)
{
  return MSBuiltinSPick<Type> (*this, index_);
}


template <class Type>
INLINELINKAGE MSBuiltinVector<Type> MSBuiltinVector<Type>::operator[] (const MSIndexVector & iVect_) const
{
  return this->select (*this, iVect_);
}


template <class Type>
INLINELINKAGE MSBuiltinVector<Type> MSBuiltinVector<Type>::operator[] (const MSBinaryVector & bVect_) const
{
  return compress (*this, bVect_);
}


template <class Type>
INLINELINKAGE MSBuiltinVector<Type> & MSBuiltinVector<Type>::series()
{
  return series(this->_pImpl->length());
}


template <class Type>
INLINELINKAGE double MSBuiltinVector<Type>::avg() const
{
  return sum()/this->_pImpl->length();
}


template <class Type>
INLINELINKAGE double MSBuiltinVector<Type>::mean() const
{
  return sum()/this->_pImpl->length();
}


template <class Type>
INLINELINKAGE double MSBuiltinVector<Type>::median() const
{
  return ((MSBuiltinVectorImpl *)this->_pImpl)->median();
}


template <class Type>
INLINELINKAGE double MSBuiltinVector<Type>::variance(MSEstimateType estType_) const
{
  return ((MSBuiltinVectorImpl *)this->_pImpl)->variance(mean(), estType_);
}


template <class Type>
INLINELINKAGE double MSBuiltinVector<Type>::stdDeviation(MSEstimateType estType_) const
{
  return ::sqrt(variance(estType_));
}


template <class Type>
INLINELINKAGE MSBuiltinVector<Type> movingAverage (const MSBuiltinVector<Type> & vect_, unsigned int width_)
{
  return MSBuiltinVector<Type> (((MSBuiltinVectorImpl *)vect_._pImpl)->movingAverage (width_));
}

// Unary minus operator
template <class Type>
INLINELINKAGE MSBuiltinVector<Type> operator- (const MSBuiltinVector<Type> & vect_)
{
  return MSBuiltinVector<Type>(MSBuiltinVector<Type>::doMath(vect_,1,MSBuiltinVector<Type>::Unary));
}

// Prefix increment operator
template <class Type>
INLINELINKAGE MSBuiltinVector<Type>& MSBuiltinVector<Type>::operator++()
{
  doMath(1,Incr); return *this;
}


// Prefix decrement operator
template <class Type>
INLINELINKAGE MSBuiltinVector<Type>& MSBuiltinVector<Type>::operator--()
{
  doMath(1,Decr); return *this;
}

// Postfix increment operator
template <class Type>
INLINELINKAGE MSBuiltinVector<Type> MSBuiltinVector<Type>::operator++(int)
{
  MSBuiltinVectorImpl *pResImpl = (MSBuiltinVectorImpl *)this->_pImpl->clone();
  return operator++(), MSBuiltinVector<Type>(pResImpl);	// call the prefix operator & return the old data
}

// Postfix decrement operator
template <class Type>
INLINELINKAGE MSBuiltinVector<Type> MSBuiltinVector<Type>::operator--(int)
{
  MSBuiltinVectorImpl *pResImpl = (MSBuiltinVectorImpl *)this->_pImpl->clone();
  return operator--(), MSBuiltinVector<Type>(pResImpl);	// call the prefix operator & return the old data
}

// operator +
template <class Type>
INLINELINKAGE MSBuiltinVector<Type> operator+ (const MSBuiltinVector<Type> & vect1_, const MSBuiltinVector<Type> & vect2_)
{
  return MSBuiltinVector<Type>(MSBuiltinVector<Type>::doMath(vect1_,vect2_,MSBuiltinVector<Type>::Plus));
}

template <class Type>
INLINELINKAGE MSBuiltinVector<Type> operator+ (const MSBuiltinVector<Type>& vect_, const Type& value_)
{
  return MSBuiltinVector<Type>(MSBuiltinVector<Type>::doMath(vect_,value_,MSBuiltinVector<Type>::Plus));
}

template <class Type>
INLINELINKAGE MSBuiltinVector<Type> operator+ (const Type & value_, const MSBuiltinVector<Type> & vect_)
{
  return MSBuiltinVector<Type>(MSBuiltinVector<Type>::doMath(value_,vect_,MSBuiltinVector<Type>::Plus));
}

// operator -
template <class Type>
INLINELINKAGE MSBuiltinVector<Type> operator- (const MSBuiltinVector<Type> & vect1_, const MSBuiltinVector<Type> & vect2_)
{
  return MSBuiltinVector<Type>(MSBuiltinVector<Type>::doMath(vect1_,vect2_,MSBuiltinVector<Type>::Minus));
}
template <class Type>
INLINELINKAGE MSBuiltinVector<Type> operator- (const MSBuiltinVector<Type> & vect_, const Type & value_)
{
  return MSBuiltinVector<Type>(MSBuiltinVector<Type>::doMath(vect_,value_,MSBuiltinVector<Type>::Minus));
}
template <class Type>
INLINELINKAGE MSBuiltinVector<Type> operator- (const Type & value_, const MSBuiltinVector<Type> & vect_)
{
  return MSBuiltinVector<Type>(MSBuiltinVector<Type>::doMath(value_,vect_,MSBuiltinVector<Type>::Minus));
}
// operator *
template <class Type>
INLINELINKAGE MSBuiltinVector<Type> operator* (const MSBuiltinVector<Type> & vect1_, const MSBuiltinVector<Type> & vect2_)
{
  return MSBuiltinVector<Type>(MSBuiltinVector<Type>::doMath(vect1_,vect2_,MSBuiltinVector<Type>::Times));
}
template <class Type>
INLINELINKAGE MSBuiltinVector<Type> operator* (const MSBuiltinVector<Type> & vect_, const Type & value_)
{
  return MSBuiltinVector<Type>(MSBuiltinVector<Type>::doMath(vect_,value_,MSBuiltinVector<Type>::Times));
}
template <class Type>
INLINELINKAGE MSBuiltinVector<Type> operator* (const Type & value_, const MSBuiltinVector<Type> & vect_)
{
  return MSBuiltinVector<Type>(MSBuiltinVector<Type>::doMath(value_,vect_,MSBuiltinVector<Type>::Times));
}
// operator -
template <class Type>
INLINELINKAGE MSBuiltinVector<Type> operator/(const MSBuiltinVector<Type>& vect1_, const MSBuiltinVector<Type>& vect2_)
{
  return MSBuiltinVector<Type>(MSBuiltinVector<Type>::doMath(vect1_,vect2_,MSBuiltinVector<Type>::Divide));
}
template <class Type>
INLINELINKAGE MSBuiltinVector<Type> operator/(const MSBuiltinVector<Type>& vect_, const Type& value_)
{
  return MSBuiltinVector<Type>(MSBuiltinVector<Type>::doMath(vect_,value_,MSBuiltinVector<Type>::Divide));
}
template <class Type>
INLINELINKAGE MSBuiltinVector<Type> operator/(const Type& value_, const MSBuiltinVector<Type>& vect_)
{
  return MSBuiltinVector<Type>(MSBuiltinVector<Type>::doMath(value_,vect_,MSBuiltinVector<Type>::Divide));
}

// operator +=
template <class Type>
INLINELINKAGE MSBuiltinVector<Type>& MSBuiltinVector<Type>::operator+=(const MSBuiltinVector<Type>& vect_)
{
  doMath(vect_,Plus); return *this;
}
template <class Type>
INLINELINKAGE MSBuiltinVector<Type>& MSBuiltinVector<Type>::operator+=(const Type& value_)
{
  doMath(value_,Plus); return *this;
}

// operator -=
template <class Type>
INLINELINKAGE MSBuiltinVector<Type>& MSBuiltinVector<Type>::operator-=(const MSBuiltinVector<Type>& vect_)
{
  doMath(vect_,Minus); return *this;
}
template <class Type>
INLINELINKAGE MSBuiltinVector<Type>& MSBuiltinVector<Type>::operator-=(const Type& value_)
{
  doMath(value_,Minus);return *this;
}

// operator *=
template <class Type>
INLINELINKAGE MSBuiltinVector<Type>& MSBuiltinVector<Type>::operator*=(const MSBuiltinVector<Type>& vect_)
{
  doMath(vect_,Times); return *this;
}
template <class Type>
INLINELINKAGE MSBuiltinVector<Type>& MSBuiltinVector<Type>::operator*=(const Type& value_)
{
  doMath(value_,Times);return *this;
}

// operator /=
template <class Type>
INLINELINKAGE MSBuiltinVector<Type>& MSBuiltinVector<Type>::operator/=(const MSBuiltinVector<Type>& vect_)
{
  doMath(vect_,Divide); return *this;
}
template <class Type>
INLINELINKAGE MSBuiltinVector<Type>& MSBuiltinVector<Type>::operator/=(const Type& value_)
{
  doMath(value_,Divide);return *this;
}

template <class Type>
INLINELINKAGE unsigned int stringLen (const Type &, const char *pString_)
{
  MSString str (pString_);
  return str.numWords();
}


INLINELINKAGE unsigned int stringLen (const char &, const char *pString_)
{
  return strlen (pString_);
}


INLINELINKAGE unsigned int stringLen (const unsigned char &, const char *pString_)
{
  return strlen (pString_);
}

#endif // MSBuiltinTypeVectorInlinesHEADER
