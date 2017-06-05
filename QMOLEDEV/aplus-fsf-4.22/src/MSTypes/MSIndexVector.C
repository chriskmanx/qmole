///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

//#include <MSTypes/MSMergeSort.H>
#include <MSTypes/MSIndexVector.H>

#include <stddef.h>	// defines offsetof macro used in MSIndexVector::Data::operator new()
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif

#include <MSTypes/MSRandom.H>

#if HAVE_SSTREAM
#include <sstream>
#include <string>
#else
#include <strstream.h>
#endif

#ifdef MS_NO_INLINES
#include <MSTypes/MSIndexVectorInlines.C>
#endif // MS_NO_INLINES

#include <MSTypes/MSMergeSort.C>

#ifdef MS_MULTI_THREAD
MSMutex MSIndexVector::_operationsMutex;
MSMutex MSIndexVector::Operations::_nullDataMutex;
#endif

MSIndexVector::MSIndexVector() : MSVector()
{
  _pImpl = new MSBuiltinVectorImpl(&ops(),&ops());
}


MSIndexVector::MSIndexVector (const MSIndexVector & vect_)
  : MSVector(), _pImpl (new MSBuiltinVectorImpl (*vect_._pImpl))
{
// ASSERTION:  pOps has been initialized already at latest when vect_ was created
}


MSIndexVector::MSIndexVector (unsigned int length_) : MSVector()
{
  _pImpl = new MSBuiltinVectorImpl(&ops(),&ops(),length_);
}


MSIndexVector::MSIndexVector (unsigned int length_, const unsigned int filler_) : MSVector()
{
  _pImpl = new MSBuiltinVectorImpl(&ops(), &ops(), length_, (void *)&filler_);
}


MSIndexVector::MSIndexVector (const char *pString_) : MSVector()
{
  _pImpl = new MSBuiltinVectorImpl(&ops(),&ops());
  _pImpl->setFromString (pString_);
}

  
MSIndexVector::MSIndexVector (MSIndexVector::Data *pData_, unsigned int len_) : MSVector()
{
  _pImpl = new MSBuiltinVectorImpl(&ops(), &ops(), (void *)pData_, len_);
}


MSIndexVector::MSIndexVector (const unsigned int *pElements_, unsigned int len_) : MSVector()
{
  Data *pData = Data::allocateWithLength (len_);
  Data::copy (pElements_, pData->elements(), len_);

  _pImpl = new MSBuiltinVectorImpl(&ops(), &ops(), (void *)pData, len_);
}


MSIndexVector::MSIndexVector (MSBuiltinVectorImpl *pImpl_) : MSVector(), _pImpl (pImpl_)
{
}


MSIndexVector::~MSIndexVector()
{
  delete _pImpl;
}


MSIndexVector & MSIndexVector::operator= (const MSIndexVector & vect_)
{
  if (this != &vect_)
    {
      *_pImpl = *vect_._pImpl;
      changed();
    }

  return *this;
}


MSIndexVector & MSIndexVector::operator= (const unsigned int value_)
{
  _pImpl->setAll ((void *)&value_);
  changed();
  return *this;
}


const MSIndexVector & MSIndexVector::nullVector()
{
  static MSIndexVector nullVect;
  return nullVect;
}


MSString MSIndexVector::asString() const
{
  return _pImpl->asString();
}


MSString MSIndexVector::asMSF() const
{
  return _pImpl->asMSF();
}


MSString MSIndexVector::asDebugInfo() const
{
  MSString result (className());
  
  return result << "(@" << MSString((unsigned long)this).d2x().lowerCase()
		<< ",_elements=" << ((Data *)_pImpl->data())->asDebugInfo()
		<< ",_size=" << MSString(size()) << ",_length=" << MSString(length())
		<< ",_type=" << type().symbolName() << ")";
}


MSString MSIndexVector::className() const
{
  return MSString ("MSIndexVector");
}


const MSSymbol & MSIndexVector::type() const
{
  return symbol();
}


MSModel * MSIndexVector::clone() const
{
  return new MSIndexVector (*this);
}


MSModel * MSIndexVector::create() const
{
  return new MSIndexVector;
}


void MSIndexVector::assign (const MSModel & aModel_)
{
  *this = (MSIndexVector &)aModel_;
}


long MSIndexVector::compare (const MSModel & aModel_) const
{
  return compare ((MSIndexVector &)aModel_);
}


MSError::ErrorStatus MSIndexVector::set (const char *pString_)
{
  MSError::ErrorStatus retval = _pImpl->setFromString (pString_);
  changed();
  return retval;
}


MSError::ErrorStatus MSIndexVector::setFromMSF (const char *pString_)
{
  MSError::ErrorStatus retval = _pImpl->setFromMSF (pString_);
  changed();
  return retval;
}


const MSSymbol & MSIndexVector::symbol()
{
  static MSSymbol sym ("MSIndexVector");
  return sym;
}


MSError::ErrorStatus MSIndexVector::set (unsigned int index_, const unsigned int value_)
{
  if (index_ < _pImpl->length())
    {
      _pImpl->set (index_, (void *)&value_);
      if (doChanged() == MSTrue)
	changed (index_);

      return MSError::MSSuccess;
    }
  else
    {
      _pImpl->vectorIndexError (index_);
      return MSError::MSFailure;
    }
}

  
unsigned int * MSIndexVector::data() const
{
  return ((Data *)_pImpl->data())->elements();
}


unsigned int MSIndexVector::length() const
{
  // length() method might potentially be called after the destructor of MSIndexVector,
  // so safeguard yourself here by checking if _pImpl has already been deleted...
  return _pImpl ? _pImpl->length() : 0;
}


// virtual version of length()
unsigned int MSIndexVector::getLength() const
{
  return _pImpl->length();
}


MSIndexVector & MSIndexVector::append (const unsigned int value_)
{
  _pImpl->append ((void *) &value_);
  appendUpdate (_pImpl->length(), 1);

  return *this;
}


MSIndexVector & MSIndexVector::append (const MSIndexVector & vect_)
{
  if (_pImpl->append (*vect_._pImpl) == MSError::MSSuccess)
    appendUpdate (_pImpl->length(), vect_._pImpl->length());

  return *this;
}


MSIndexVector & MSIndexVector::insertAt (unsigned int index_, const unsigned int value_)
{
  if (index_ == _pImpl->length())   // NEW FEATURE:  insert right after the last element
    return append (value_);

  // if inserting inside the vector
  if (_pImpl->insertAt (index_, (void *) &value_) == MSError::MSSuccess)
    changed();

  return *this;
}


MSIndexVector & MSIndexVector::insertAt (unsigned int index_, const MSIndexVector & vect_)
{
  if (index_ == _pImpl->length())  // NEW FEATURE:  insert right after the last element
    return append (vect_);
  
  // if inserting inside the vector
  if (_pImpl->insertAt (index_, *vect_._pImpl) == MSError::MSSuccess)
    changed();

  return *this;
}


MSIndexVector & MSIndexVector::removeAt (unsigned int index_)
{
  if (_pImpl->removeAt (index_) == MSError::MSSuccess)
    changed();

  return *this;
}


MSIndexVector & MSIndexVector::removeAt (unsigned int startPos_, unsigned int numEls_)
{
  if (numEls_ > 0)
    if (_pImpl->removeAt (startPos_, numEls_) == MSError::MSSuccess)
      changed();

  return *this;
}


MSIndexVector & MSIndexVector::remove (const MSIndexVector & iVect_)
{
  if (_pImpl->remove (iVect_) == MSError::MSSuccess)
    changed();
  
  return *this;
}


MSIndexVector & MSIndexVector::remove (const MSBinaryVector & bVect_)
{
  if (_pImpl->remove (bVect_) == MSError::MSSuccess)
    changed();
  
  return *this;
}


MSIndexVector & MSIndexVector::removeAll()
{
  if (_pImpl->removeAll() == MSError::MSSuccess)
    changed();

  return *this;
}


MSIndexVector & MSIndexVector::select (const MSIndexVector & iVect_)
{
  if (_pImpl->select (iVect_) == MSError::MSSuccess)
    changed();

  return *this;
}

MSIndexVector & MSIndexVector::compress  (const MSBinaryVector & bVect_)
{
  if (_pImpl->compress (bVect_) == MSError::MSSuccess)
    changed();
  
  return *this;
}


MSIndexVector & MSIndexVector::reshape (unsigned int newLen_)
{
  if (_pImpl->reshape (newLen_) == MSError::MSSuccess)
    changed();
  
  return *this;
}


MSIndexVector & MSIndexVector::exchange (unsigned int index1_, unsigned int index2_)
{
  if (_pImpl->exchange (index1_, index2_) == MSError::MSSuccess && doChanged() == MSTrue)
    {
      MSIndexVector iVect (2);
      iVect.data()[0] = index1_;
      iVect.data()[1] = index2_;
      changed (iVect);
    }
  
  return *this;
}


MSIndexVector & MSIndexVector::reverse()
{
  if (_pImpl->reverse() == MSError::MSSuccess)
    changed();
  
  return *this;
}


MSIndexVector & MSIndexVector::rotate (int amount_)
{
  if (_pImpl->rotate (amount_) == MSError::MSSuccess)
    changed();
  
  return *this;
}


MSIndexVector & MSIndexVector::take (int numEls_, const unsigned int filler_)
{
  if (_pImpl->take (numEls_, (void *)&filler_) == MSError::MSSuccess)
    changed();
  
  return *this;
}


MSIndexVector & MSIndexVector::drop (int numEls_)
{
  if (_pImpl->drop (numEls_) == MSError::MSSuccess)
    changed();
  
  return *this;
}


MSIndexVector MSIndexVector::reverse (const MSIndexVector & vect_)
{
  MSBuiltinVectorImpl *impl = (MSBuiltinVectorImpl *)vect_._pImpl->create();
  impl->reverse (*vect_._pImpl);
  return MSIndexVector (impl);
}


MSIndexVector MSIndexVector::rotate (const MSIndexVector & vect_, int amount_)
{
  MSBuiltinVectorImpl *impl = (MSBuiltinVectorImpl *)vect_._pImpl->create();
  impl->rotate (*vect_._pImpl, amount_);
  return MSIndexVector (impl);
}


MSIndexVector MSIndexVector::take (const MSIndexVector & vect_, int numEls_, const unsigned int filler_)
{
  MSBuiltinVectorImpl *impl = (MSBuiltinVectorImpl *)vect_._pImpl->create();
  impl->take (*vect_._pImpl, numEls_, (void *)&filler_);
  return MSIndexVector (impl);
}


MSIndexVector MSIndexVector::drop (const MSIndexVector & vect_, int numEls_)
{
  MSBuiltinVectorImpl *impl = (MSBuiltinVectorImpl *)vect_._pImpl->create();
  impl->drop (*vect_._pImpl, numEls_);
  return MSIndexVector (impl);
}


MSIndexVector MSIndexVector::select (const MSIndexVector & vect_, const MSIndexVector & iVect_)
{
  MSBuiltinVectorImpl *impl = (MSBuiltinVectorImpl *)vect_._pImpl->create();
  impl->select (*vect_._pImpl, iVect_);
  return MSIndexVector (impl);
}


MSIndexVector MSIndexVector::compress (const MSIndexVector & vect_, const MSBinaryVector & bVect_)
{
  MSBuiltinVectorImpl *impl = (MSBuiltinVectorImpl *)vect_._pImpl->create();
  impl->compress (*vect_._pImpl, bVect_);
  return MSIndexVector (impl);
}


MSIndexVector & MSIndexVector::selectiveAssign (const MSIndexVector & iVect_, const unsigned int value_)
{
  _pImpl->setSelected (iVect_, (void *)&value_);
  changed (iVect_);
  return *this;
}


MSIndexVector & MSIndexVector::selectiveAssign (const MSIndexVector & iVect_, const MSIndexVector & vect_)
{
  _pImpl->setSelected (iVect_, *vect_._pImpl);
  changed (iVect_);
  return *this;
}


MSIndexVector & MSIndexVector::selectiveAssign (const MSBinaryVector & bVect_, const unsigned int value_)
{
  if (doChanged() == MSTrue)
    changed (_pImpl->setIndexSelected (bVect_, (void *)&value_));
  else
    _pImpl->setSelected (bVect_, (void *)&value_);
    
  return *this;
}


MSIndexVector & MSIndexVector::selectiveAssign (const MSBinaryVector & bVect_, const MSIndexVector & vect_)
{
  if (doChanged() == MSTrue)
    changed (_pImpl->setIndexSelected (bVect_, *vect_._pImpl));
  else
    _pImpl->setSelected (bVect_, *vect_._pImpl);
    
  return *this;
}


MSIndexVector MSIndexVector::gradeUp() const
{
  return _pImpl->gradeUp();
}


MSIndexVector MSIndexVector::gradeDown() const
{
  return _pImpl->gradeDown();
}


void MSIndexVector::permute (const MSIndexVector & iVect_)
{
  _pImpl->permute (iVect_);
  changed();
}


ostream & operator<< (ostream & stream_, const MSIndexVector & vect_)
{
  vect_._pImpl->print (stream_);
  return stream_;
}


MSIndexVector & MSIndexVector::random (unsigned int limit_)
{
  unsigned int len=_pImpl->length();
  if (len>0)
    {
      _pImpl->prepareToChangeWithoutCopy();
      
      if (limit_==0)  limit_=len;

      MSRandom rand;
      unsigned int *pData=data();

      while (len--)
	{
	  *pData++ = (unsigned int)rand(limit_);
	}
  
      changed();
    }

  return *this;
}


MSIndexVector & MSIndexVector::series (unsigned int length_, unsigned int offset_)
{
  _pImpl->reallocateInPlace(length_);

  unsigned int *pData=data();

  while (length_--)
    {
      *pData++ = offset_++;
    }
  
  changed();
  return *this;
}


unsigned int MSIndexVector::min() const
{
  unsigned int len=_pImpl->length();

  if (len==0)  return 0;

  unsigned int *pData=data();
  unsigned int min=pData[0];

  for (unsigned int i=1; i<len; i++)
    {
      if (pData[i]<min)
	{
	  min = pData[i];
	}
    }

  return min;
}


unsigned int MSIndexVector::max() const
{
  unsigned int len=_pImpl->length();

  if (len==0)  return 0;

  unsigned int *pData=data();
  unsigned int max=pData[0];

  for (unsigned int i=1; i<len; i++)
    {
      if (pData[i]>max)
	{
	  max = pData[i];
	}
    }

  return max;
}


double MSIndexVector::sum() const
{
  double sum=0.0;
  unsigned int len=_pImpl->length();
  unsigned int *pData=data();

  while (len--)
    {
      sum += (double)*pData++;
    }

  return sum;
}


// Unary minus operator
MSIndexVector operator- (const MSIndexVector & vect_)
{
  unsigned int len=vect_._pImpl->length();
  MSBuiltinVectorImpl *pNewImpl = (MSBuiltinVectorImpl *)vect_._pImpl->create(len, vect_.vectorData()->size());
  
  unsigned int *pSrc=vect_.data(), *pDest=((MSIndexVector::Data *)pNewImpl->data())->elements();

  while (len--)
    {
      *pDest++ = -*pSrc++;
    }

  return MSIndexVector (pNewImpl);
}

  
// Prefix increment operator
MSIndexVector & MSIndexVector::operator++()
{
  unsigned int len=_pImpl->length();

  if (len>0)
    {
      unsigned int *pData=data();
      _pImpl->prepareToChangeWithoutCopy();

      if (pData==data())	// if no reallocation was necessary
	{
	  for (unsigned int i=0; i<len; i++)
	    {
	      ++pData[i];
	    }
	}
      else	// if we had to reallocate
	{
	  // ASSERTION:  pData is still valid; otherwise, we wouldn't have reallocated
	  unsigned int *pNewData=data();

	  while (len--)
	    {
	      *pNewData++ = *pData++ +1;   // increment on-copy
	    }
	}

      changed();
    }

  return *this;
}


// Prefix decrement operator
MSIndexVector & MSIndexVector::operator--()
{
  unsigned int len=_pImpl->length();

  if (len>0)
    {
      unsigned int *pData=data();
      _pImpl->prepareToChangeWithoutCopy();

      if (pData==data())	// if no reallocation was necessary
	{
	  for (unsigned int i=0; i<len; i++)
	    {
	      --pData[i];
	    }
	}
      else	// if we had to reallocate
	{
	  // ASSERTION:  pData is still valid; otherwise, we wouldn't have reallocated
	  unsigned int *pNewData=data();

	  while (len--)
	    {
	      *pNewData++ = *pData++ -1;
	    }
	}

      changed();
    }

  return *this;
}


MSIndexVector operator+ (const MSIndexVector & vect1_, const MSIndexVector & vect2_)
{
  unsigned int len=vect1_._pImpl->length();
  assert (len==vect2_._pImpl->length());

  MSBuiltinVectorImpl *pResImpl = (MSBuiltinVectorImpl *)vect1_._pImpl->create(len, vect1_.vectorData()->size());

  unsigned int *pData1=vect1_.data(), *pData2=vect2_.data(), *pResData=((MSIndexVector::Data *)pResImpl->data())->elements();

  while (len--)
    {
      *pResData++ = *pData1++ + *pData2++;
    }

  return MSIndexVector(pResImpl);
}


MSIndexVector operator+ (const MSIndexVector & vect_, const unsigned int value_)
{
  unsigned int len=vect_._pImpl->length();
  MSBuiltinVectorImpl *pResImpl = (MSBuiltinVectorImpl *)vect_._pImpl->create(len,vect_.vectorData()->size());
  unsigned int *pData=vect_.data(), *pResData=((MSIndexVector::Data *)pResImpl->data())->elements();

  while (len--)
    {
      *pResData++ = *pData++ + value_;
    }

  return MSIndexVector(pResImpl);
}


MSIndexVector operator- (const MSIndexVector & vect1_, const MSIndexVector & vect2_)
{
  unsigned int len=vect1_._pImpl->length();
  assert (len==vect2_._pImpl->length());

  MSBuiltinVectorImpl *pResImpl = (MSBuiltinVectorImpl *)vect1_._pImpl->create(len, vect1_.vectorData()->size());

  unsigned int *pData1=vect1_.data(), *pData2=vect2_.data(), *pResData=((MSIndexVector::Data *)pResImpl->data())->elements();

  while (len--)
    {
      *pResData++ = *pData1++ - *pData2++;
    }

  return MSIndexVector(pResImpl);
}


MSIndexVector operator- (const MSIndexVector & vect_, const unsigned int value_)
{
  unsigned int len=vect_._pImpl->length();
  MSBuiltinVectorImpl *pResImpl = (MSBuiltinVectorImpl *)vect_._pImpl->create(len,vect_.vectorData()->size());
  unsigned int *pData=vect_.data(), *pResData=((MSIndexVector::Data *)pResImpl->data())->elements();

  while (len--)
    {
      *pResData++ = *pData++ - value_;
    }

  return MSIndexVector(pResImpl);
}


MSIndexVector operator- (const unsigned int value_, const MSIndexVector & vect_)
{
  unsigned int len=vect_._pImpl->length();
  MSBuiltinVectorImpl *pResImpl = (MSBuiltinVectorImpl *)vect_._pImpl->create(len,vect_.vectorData()->size());
  unsigned int *pData=vect_.data(), *pResData=((MSIndexVector::Data *)pResImpl->data())->elements();

  while (len--)
    {
      *pResData++ = value_ - *pData++;
    }

  return MSIndexVector(pResImpl);
}


MSIndexVector operator* (const MSIndexVector & vect1_, const MSIndexVector & vect2_)
{
  unsigned int len=vect1_._pImpl->length();
  assert (len==vect2_._pImpl->length());

  MSBuiltinVectorImpl *pResImpl = (MSBuiltinVectorImpl *)vect1_._pImpl->create(len, vect1_.vectorData()->size());

  unsigned int *pData1=vect1_.data(), *pData2=vect2_.data(), *pResData=((MSIndexVector::Data *)pResImpl->data())->elements();

  while (len--)
    {
      *pResData++ = *pData1++ * *pData2++;
    }

  return MSIndexVector(pResImpl);
}


MSIndexVector operator* (const MSIndexVector & vect_, const unsigned int value_)
{
  unsigned int len=vect_._pImpl->length();
  MSBuiltinVectorImpl *pResImpl = (MSBuiltinVectorImpl *)vect_._pImpl->create(len,vect_.vectorData()->size());
  unsigned int *pData=vect_.data(), *pResData=((MSIndexVector::Data *)pResImpl->data())->elements();

  while (len--)
    {
      *pResData++ = *pData++ * value_;
    }

  return MSIndexVector(pResImpl);
}


MSIndexVector operator/ (const MSIndexVector & vect1_, const MSIndexVector & vect2_)
{
  unsigned int len=vect1_._pImpl->length();
  assert (len==vect2_._pImpl->length());

  MSBuiltinVectorImpl *pResImpl = (MSBuiltinVectorImpl *)vect1_._pImpl->create(len, vect1_.vectorData()->size());

  unsigned int *pData1=vect1_.data(), *pData2=vect2_.data(), *pResData=((MSIndexVector::Data *)pResImpl->data())->elements();

  while (len--)
    {
      *pResData++ = *pData1++ / *pData2++;
    }

  return MSIndexVector(pResImpl);
}


MSIndexVector operator/ (const MSIndexVector & vect_, const unsigned int value_)
{
  unsigned int len=vect_._pImpl->length();
  MSBuiltinVectorImpl *pResImpl = (MSBuiltinVectorImpl *)vect_._pImpl->create(len,vect_.vectorData()->size());
  unsigned int *pData=vect_.data(), *pResData=((MSIndexVector::Data *)pResImpl->data())->elements();

  while (len--)
    {
      *pResData++ = *pData++ / value_;
    }

  return MSIndexVector(pResImpl);
}


MSIndexVector operator/ (const unsigned int value_, const MSIndexVector & vect_)
{
  unsigned int len=vect_._pImpl->length();
  MSBuiltinVectorImpl *pResImpl = (MSBuiltinVectorImpl *)vect_._pImpl->create(len,vect_.vectorData()->size());
  unsigned int *pData=vect_.data(), *pResData=((MSIndexVector::Data *)pResImpl->data())->elements();

  while (len--)
    {
      *pResData++ = value_ / *pData++;
    }

  return MSIndexVector(pResImpl);
}


MSIndexVector & MSIndexVector::operator+= (const MSIndexVector & vect_)
{
  unsigned int len=_pImpl->length();

  assert(len=vect_._pImpl->length());

  if (len>0)
    {
      unsigned int *pData=data(), *pVectData=vect_.data();
      _pImpl->prepareToChangeWithoutCopy();

      if (pData==data())	// no reallocation was necessary
	{
	  while (len--)
	    {
	      *pData++ += *pVectData++;
	    }
	}
      else	// if we had to reallocate
	{
	  // ASSERTION:  pData is still valid
	  unsigned int *pNewData=data();

	  while (len--)
	    {
	      *pNewData++ = *pData++ + *pVectData++;   // increment on-copy
	    }
	}
	  
      changed();
    }

  return *this;
}


MSIndexVector & MSIndexVector::operator+= (const unsigned int value_)
{
  unsigned int len=_pImpl->length();

  if (len>0)
    {
      unsigned int *pData=data();
      _pImpl->prepareToChangeWithoutCopy();

      if (pData==data())	// no reallocation was necessary
	{
	  while (len--)
	    {
	      *pData++ += value_;
	    }
	}
      else	// if we had to reallocate
	{
	  // ASSERTION:  pData is still valid
	  unsigned int *pNewData=data();

	  while (len--)
	    {
	      *pNewData++ = *pData++ + value_;   // increment on-copy
	    }
	}
	  
      changed();
    }

  return *this;
}


MSIndexVector & MSIndexVector::operator-= (const MSIndexVector & vect_)
{
  unsigned int len=_pImpl->length();

  assert(len=vect_._pImpl->length());

  if (len>0)
    {
      unsigned int *pData=data(), *pVectData=vect_.data();
      _pImpl->prepareToChangeWithoutCopy();

      if (pData==data())	// no reallocation was necessary
	{
	  while (len--)
	    {
	      *pData++ -= *pVectData++;
	    }
	}
      else	// if we had to reallocate
	{
	  // ASSERTION:  pData is still valid
	  unsigned int *pNewData=data();

	  while (len--)
	    {
	      *pNewData++ = *pData++ - *pVectData++;   // increment on-copy
	    }
	}
	  
      changed();
    }

  return *this;
}


MSIndexVector & MSIndexVector::operator-= (const unsigned int value_)
{
  unsigned int len=_pImpl->length();

  if (len>0)
    {
      unsigned int *pData=data();
      _pImpl->prepareToChangeWithoutCopy();

      if (pData==data())	// no reallocation was necessary
	{
	  while (len--)
	    {
	      *pData++ -= value_;
	    }
	}
      else	// if we had to reallocate
	{
	  // ASSERTION:  pData is still valid
	  unsigned int *pNewData=data();

	  while (len--)
	    {
	      *pNewData++ = *pData++ - value_;   // increment on-copy
	    }
	}
	  
      changed();
    }

  return *this;
}


MSIndexVector & MSIndexVector::operator*= (const MSIndexVector & vect_)
{
  unsigned int len=_pImpl->length();

  assert(len=vect_._pImpl->length());

  if (len>0)
    {
      unsigned int *pData=data(), *pVectData=vect_.data();
      _pImpl->prepareToChangeWithoutCopy();

      if (pData==data())	// no reallocation was necessary
	{
	  while (len--)
	    {
	      *pData++ *= *pVectData++;
	    }
	}
      else	// if we had to reallocate
	{
	  // ASSERTION:  pData is still valid
	  unsigned int *pNewData=data();

	  while (len--)
	    {
	      *pNewData++ = *pData++ * *pVectData++;   // increment on-copy
	    }
	}
	  
      changed();
    }

  return *this;
}


MSIndexVector & MSIndexVector::operator*= (const unsigned int value_)
{
  unsigned int len=_pImpl->length();

  if (len>0)
    {
      unsigned int *pData=data();
      _pImpl->prepareToChangeWithoutCopy();

      if (pData==data())	// no reallocation was necessary
	{
	  while (len--)
	    {
	      *pData++ *= value_;
	    }
	}
      else	// if we had to reallocate
	{
	  // ASSERTION:  pData is still valid
	  unsigned int *pNewData=data();

	  while (len--)
	    {
	      *pNewData++ = *pData++ * value_;   // increment on-copy
	    }
	}
	  
      changed();
    }

  return *this;
}


MSIndexVector & MSIndexVector::operator/= (const MSIndexVector & vect_)
{
  unsigned int len=_pImpl->length();

  assert(len=vect_._pImpl->length());

  if (len>0)
    {
      unsigned int *pData=data(), *pVectData=vect_.data();
      _pImpl->prepareToChangeWithoutCopy();

      if (pData==data())	// no reallocation was necessary
	{
	  while (len--)
	    {
	      *pData++ /= *pVectData++;
	    }
	}
      else	// if we had to reallocate
	{
	  // ASSERTION:  pData is still valid
	  unsigned int *pNewData=data();

	  while (len--)
	    {
	      *pNewData++ = *pData++ / *pVectData++;   // increment on-copy
	    }
	}
	  
      changed();
    }

  return *this;
}


MSIndexVector & MSIndexVector::operator/= (const unsigned int value_)
{
  unsigned int len=_pImpl->length();

  if (len>0)
    {
      unsigned int *pData=data();
      _pImpl->prepareToChangeWithoutCopy();

      if (pData==data())	// no reallocation was necessary
	{
	  while (len--)
	    {
	      *pData++ /= value_;
	    }
	}
      else	// if we had to reallocate
	{
	  // ASSERTION:  pData is still valid
	  unsigned int *pNewData=data();

	  while (len--)
	    {
	      *pNewData++ = *pData++ / value_;   // increment on-copy
	    }
	}
	  
      changed();
    }

  return *this;
}


MSIndexVector::Operations& MSIndexVector::ops(void)
{
  MS_SAFE_STATIC_INIT(Operations,_operationsMutex);
}


MSIndexVector::Operations::Operations()
{
}


MSIndexVector::Operations::~Operations()
{
}


void * MSIndexVector::Operations::allocate (unsigned int length_, unsigned int, MSAllocationFlag) const
{
  if (length_)
    return Data::allocateWithLength (length_);
  else  // length_ == 0
    return nullData().incrementCount();
}


void * MSIndexVector::Operations::allocateWithSize (unsigned int size_, unsigned int, MSAllocationFlag) const
{
  if (size_)
    return Data::allocateWithSize (size_);
  else  // size_ == 0
    return nullData().incrementCount();
}  


void MSIndexVector::Operations::deallocate (void *pData_, unsigned int, MSAllocationFlag) const
{
  ((Data *)pData_)->decrementCount();
}


void MSIndexVector::Operations::incrementCount (void *data_) const
{
  ((Data *)data_)->incrementCount();
}


unsigned int MSIndexVector::Operations::refCount (const void *data_) const
{
  return ((Data *)data_)->refCount();
}


void MSIndexVector::Operations::set (void *pData_, unsigned int index_, const void *pValue_, MSAllocationFlag) const
{
  ((Data *)pData_)->elements()[index_] = *(unsigned int *)pValue_;
}


void MSIndexVector::Operations::set (void *pDest_, unsigned int destInd_, const void *pSrc_, unsigned int srcInd_,
				     MSAllocationFlag) const
{
  ((Data *)pDest_)->elements()[destInd_] = ((Data *)pSrc_)->elements()[srcInd_];
}


void MSIndexVector::Operations::fill (void *pElements_, unsigned int start_, unsigned int numToFill_,
				      const void *pValue_, MSAllocationFlag) const
{
  unsigned int *pStart = ((Data *)pElements_)->elements() + start_;

  if (pValue_)
    Data::fill (pStart, numToFill_, *(unsigned int *)pValue_);
  else
    Data::fill (pStart, numToFill_, *(unsigned int *)defaultFiller());
}


void MSIndexVector::Operations::copy (const void *src_, void *dest_, unsigned int length_,
				      unsigned int srcInd_, unsigned int destInd_, MSAllocationFlag) const
{
  Data::copy (((Data *)src_)->elements() + srcInd_, ((Data *)dest_)->elements() + destInd_, length_);
}


void MSIndexVector::Operations::copyBackward (void *pElements_, unsigned int src_, unsigned int dest_,
					      unsigned int numToCopy_) const
{
  unsigned int *pElements = ((Data *)pElements_)->elements();

  Data::copyBackward (pElements+src_, pElements+dest_, numToCopy_);
}


void MSIndexVector::Operations::destroy (void *, unsigned int, unsigned int) const
{
}


int MSIndexVector::Operations::isElementEqual (const void *pElements_, unsigned int index_, const void *pValue_) const
{
  return ((Data *)pElements_)->elements()[index_] == *(unsigned int *)pValue_;
}


int MSIndexVector::Operations::isElementLess (const void *pElements_, unsigned int index_, const void *pValue_) const
{
  return ((Data *)pElements_)->elements()[index_] < *(unsigned int *)pValue_;
}


int MSIndexVector::Operations::isElementLessEqual (const void *pElements_, unsigned int index_, const void *pValue_) const
{
  return ((Data *)pElements_)->elements()[index_] <= *(unsigned int *)pValue_;
}


long MSIndexVector::Operations::compareElement (const void *data_, unsigned int index_, const void *value_) const
{
  const unsigned int el1 = ((Data *)data_)->elements()[index_], el2 = *(unsigned int *)value_;
  return (el1 == el2) ? 0 : ((el1 < el2) ? -1 : 1);
}


void *MSIndexVector::Operations::elementAt (const void *pElements_, unsigned int index_) const
{
  return & ((Data *)pElements_)->elementAt (index_);
}


unsigned int MSIndexVector::Operations::size (const void *pElements_) const
{
  return ((Data *)pElements_)->size();
}


void MSIndexVector::Operations::swapElements (void *data_, unsigned int ind1_, unsigned int ind2_) const
{
  unsigned int *pElements = ((Data *)data_)->elements();
  unsigned int temp = pElements[ind1_];
  pElements[ind1_] = pElements[ind2_], pElements[ind2_] = temp;
}
  

unsigned int MSIndexVector::Operations::gradeUp (const void *pData_, unsigned int len_, unsigned int *pResult_) const
{
  return msMergeSortUp (len_, ((Data *)pData_)->elements(), pResult_, (unsigned int)0, len_);
}


unsigned int MSIndexVector::Operations::gradeDown (const void *pData_, unsigned int len_, unsigned int *pResult_) const
{
  return msMergeSortDown (len_, ((Data *)pData_)->elements(), pResult_, (unsigned int)0, len_);
}


void *MSIndexVector::Operations::badData() const
{
  static unsigned int badValue =0;
  return (void *)&badValue;
}


void *MSIndexVector::Operations::defaultFiller() const
{
  static unsigned int filler =0;
  return (void *)&filler;
}


MSString MSIndexVector::Operations::asString (const void *pData_, unsigned int index_) const
{
  return MSString (((Data *)pData_)->elements()[index_]);
}


MSString MSIndexVector::Operations::asMSF (const void *pData_, unsigned int index_) const
{
  return MSString (((Data *)pData_)->elements()[index_]);
}


unsigned int MSIndexVector::Operations::elementLen (const void *, unsigned int) const
{
  return 0;
}


MSError::ErrorStatus MSIndexVector::Operations::setFromString (void *pData_, unsigned int index_, const char *pString_) const
{
  char *cp = 0;
  ((Data *)pData_)->elements()[index_] = (unsigned int) strtoul (pString_, &cp, 10);  // Base 10
  return (cp == pString_) ? MSError::MSFailure : MSError::MSSuccess;
}


MSError::ErrorStatus MSIndexVector::Operations::setFromMSF (void *pData_, unsigned int index_, const char *pString_) const
{
  return setFromString (pData_, index_, pString_);
}


void MSIndexVector::Operations::setFromMSString (void *pData_, unsigned int vectIndex_,
						 const MSString & str_, unsigned int& startPos_, const char) const
{
  if (startPos_<str_.length())
    {
      if (isspace(str_(startPos_)))
	{
	  startPos_ = str_.indexOfAnyBut(MSStringTest(APLUS_ISPACE),startPos_+1);
	  if (startPos_>=str_.length())
	    {
	      return;
	    }
	}

      unsigned int endPos = str_.indexOfAnyOf(MSStringTest(APLUS_ISPACE),startPos_);
      ((Data *)pData_)->elements()[vectIndex_] = str_.subString(startPos_,endPos-startPos_).asUnsigned();
      startPos_ = str_.indexOfAnyBut(MSStringTest(APLUS_ISPACE),endPos);
    }
}


unsigned int MSIndexVector::Operations::numElements (const MSString & str_, const char) const
{
  return str_.numWords();
}


void MSIndexVector::Operations::print (const void *pData_, unsigned int index_, ostream & stream_) const
{
  stream_ << ((Data *)pData_)->elements()[index_] << " ";
}


void MSIndexVector::Operations::setToNumber (void *pElements_, unsigned int index_, double number_) const
{
  ((Data *)pElements_)->elements()[index_] = (unsigned int)number_;  
}


double MSIndexVector::Operations::getAsNumber (const void *pElements_, unsigned int index_) const
{
  return (double)((Data *)pElements_)->elements()[index_];
}


#if HAVE_SSTREAM
void MSIndexVector::Operations::readFromStream (void *pData_, unsigned int index_, istringstream & ist) const
#else
void MSIndexVector::Operations::readFromStream (void *pData_, unsigned int index_, istrstream & ist) const
#endif
{
  ist >> ((Data *)pData_)->elements()[index_];
}


#if HAVE_SSTREAM
void MSIndexVector::Operations::writeToStream (const void *pData_, unsigned int index_, ostringstream & ost) const
#else
void MSIndexVector::Operations::writeToStream (const void *pData_, unsigned int index_, ostrstream & ost) const
#endif
{
  ost << ((Data *)pData_)->elements()[index_];
}


#if HAVE_SSTREAM
void MSIndexVector::Operations::whitespace (istringstream &) const
#else
void MSIndexVector::Operations::whitespace (istrstream &) const
#endif
{
}


unsigned int MSIndexVector::Operations::stringLen (const char *pString_) const
{
  MSString str (pString_);
  return str.numWords();
}


MSIndexVector::Data& MSIndexVector::Operations::nullData(void)
{
  MS_SAFE_STATIC_INIT(Data,_nullDataMutex);
}


MSIndexVector::Data::Data (unsigned int size_) : MSData (size_)
{
}


MSIndexVector::Data::~Data()
{
}


MSString MSIndexVector::Data::asDebugInfo() const
{
  MSString result ("MSIndexVector::Data(@");
  result += MSString((unsigned long)this).d2x().lowerCase();
  result += ",_data=";
  result += MSString((unsigned long)elements()).d2x().lowerCase();
  result += ",_refCount=";
  result += MSString(refCount());
  result += ")";
  return result;
}


void * MSIndexVector::Data::operator new (size_t, unsigned int numEls_)
{
  // We need to allocate memory for sizeof(Data) plus the size of the data elements array
  // except for the first element which is included in the size of Data
  //
  unsigned int realSize;
  if (numEls_>1)
    {
      realSize = sizeof(Data) + (numEls_-1)*sizeof(unsigned int);
    }
  else	// 0 or 1 elements to be allocated
    {
      realSize = sizeof(Data);	// Data already includes 1 data element
    }

  return (void *) ::new char[realSize];
}
  

MSIndexVector::Data *MSIndexVector::Data::allocateWithLength (unsigned length_) 
{ 
  unsigned int newLength = MSData::computeSize(length_);  
  return new (newLength) Data (newLength); 
}


MSIndexVector::Data *MSIndexVector::Data::allocateWithSize (unsigned size_) 
{
  return new (size_)Data(size_);
}


void MSIndexVector::Data::deallocate()
{
  delete this;
}


void MSIndexVector::Data::fill (unsigned int *pElements_, unsigned int length_, const unsigned int value_)
{
  while (length_--)
    *pElements_++ = value_;
}


void MSIndexVector::Data::copy(const unsigned int *src_, unsigned int *dst_, unsigned int length_)
{
  while (length_--)
    *dst_++ = *src_++;
}


void MSIndexVector::Data::copyBackward (const unsigned int *pSrc_, unsigned int *pDest_, unsigned int length_)
{
  while (length_--)
    *pDest_-- = *pSrc_--;
}


MSIndexVector::SPick & MSIndexVector::SPick::operator= (const MSIndexVector::SPick & sPick_)
{
  _pVector->set (_index, (*sPick_._pVector)(sPick_._index));
  return *this;
}


MSIndexVector::SPick & MSIndexVector::SPick::operator= (const unsigned int aScalar_)
{
  _pVector->set (_index, aScalar_);
  return *this;
}


// postfix increment
unsigned int MSIndexVector::SPick::operator++ (int)
{
  unsigned int temp = (*_pVector)(_index);
  _pVector->set (_index, temp+1);
  return temp;
}


// postfix decrement
unsigned int MSIndexVector::SPick::operator-- (int)
{
  unsigned int temp = (*_pVector)(_index);
  _pVector->set (_index, temp-1);
  return temp;
}
