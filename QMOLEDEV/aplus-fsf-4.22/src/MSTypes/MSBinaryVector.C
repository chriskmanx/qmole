///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stddef.h>	// defines offsetof macro used in MSTypeData<Type>::operator new
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif

#include <MSTypes/MSString.H>
#include <MSTypes/MSMergeSort.H>
#include <MSTypes/MSRandom.H>

#include <MSTypes/MSIndexVector.H>
#include <MSTypes/MSBinaryVector.H>

#if defined(MSTK_MANUAL_INSTANTIATION)
#include <MSTypes/MSMergeSort.C>
#endif // MSTK_MANUAL_INSTANTIATION

#ifdef MS_NO_INLINES
#include <MSTypes/MSBinaryVectorInlines.C>
#endif //MS_NO_INLINES

#ifdef MS_MULTI_THREAD
MSMutex MSBinaryVector::_operationsMutex;
MSMutex MSBinaryVector::Operations::_nullDataMutex;
#endif

MSBinaryVector::MSBinaryVector() : MSVector()
{
  _pImpl = new MSVectorImpl(&ops());
}


MSBinaryVector::MSBinaryVector(unsigned int length_, unsigned char filler_) : MSVector()
{
  unsigned char c = (filler_==0) ? 0 : 1;
  _pImpl = new MSVectorImpl(&ops(), length_, (void *)&c);
}


MSBinaryVector::MSBinaryVector (const MSBinaryVector & vect_)
  : MSVector(), _pImpl (new MSVectorImpl (*vect_._pImpl))
{
}


MSBinaryVector::MSBinaryVector (const char *pString_) : MSVector()
{
  _pImpl = new MSVectorImpl(&ops());
  _pImpl->setFromString (pString_);
}

  
MSBinaryVector::MSBinaryVector (MSVectorImpl *pImpl_) : MSVector(), _pImpl (pImpl_)
{
}


MSBinaryVector::MSBinaryVector (MSBinaryVector::Data *pData_, unsigned int len_) : MSVector()
{
  _pImpl = new MSVectorImpl(&ops(), (void *)pData_, len_);
}


MSBinaryVector::MSBinaryVector (const unsigned char *pElements_, unsigned int len_) : MSVector()
{
  Data *pData = Data::allocateWithLength (len_);
  Data::copy (pElements_, pData->elements(), len_);

  _pImpl = new MSVectorImpl(&ops(), (void *)pData, len_);
}


MSBinaryVector::~MSBinaryVector()
{
  delete _pImpl;
}


MSBinaryVector & MSBinaryVector::operator= (const MSBinaryVector & vect_)
{
  if (this != &vect_)
    {
      *_pImpl = *vect_._pImpl;
      changed();
    }

  return *this;
}


MSBinaryVector & MSBinaryVector::operator= (const unsigned char value_)
{
  unsigned char c = value_ ? 1 : 0;
  _pImpl->setAll ((void *)&c);
  changed();

  return *this;
}


MSString MSBinaryVector::asString() const
{
  return _pImpl->asString();
}


MSString MSBinaryVector::asMSF() const
{
  return _pImpl->asMSF();
}


MSString MSBinaryVector::asDebugInfo() const
{
  MSString result (className());
  
  return result << "(@" << MSString((unsigned long)this).d2x().lowerCase()
		<< ",_elements=" << ((Data *)_pImpl->data())->asDebugInfo()
		<< ",_size=" << MSString(size()) << ",_length=" << MSString(length())
		<< ",_type=" << type().symbolName() << ")";
}


MSString MSBinaryVector::className() const
{
  return MSString ("MSBinaryVector");
}


const MSSymbol & MSBinaryVector::type() const
{
  return symbol();
}


MSModel * MSBinaryVector::clone() const
{
  return new MSBinaryVector (*this);
}


MSModel * MSBinaryVector::create() const
{
  return new MSBinaryVector;
}


void MSBinaryVector::assign (const MSModel & aModel_)
{
  *this = (MSBinaryVector &)aModel_;
}


long MSBinaryVector::compare (const MSModel & aModel_) const
{
  return compare ((MSBinaryVector &)aModel_);
}


MSError::ErrorStatus MSBinaryVector::set (const char *pString_)
{
  MSError::ErrorStatus retval = _pImpl->setFromString (pString_);
  changed();
  return retval;
}


MSError::ErrorStatus MSBinaryVector::setFromMSF (const char *pString_)
{
  MSError::ErrorStatus retval = _pImpl->setFromMSF (pString_);
  changed();
  return retval;
}


const MSSymbol & MSBinaryVector::symbol()
{
  static MSSymbol sym ("MSBinaryVector");
  return sym;
}


unsigned char * MSBinaryVector::data() const
{
  return ((Data *)_pImpl->data())->elements();
}


unsigned int MSBinaryVector::length() const
{
  // length() method might potentially be called after the destructor of MSBinaryVector,
  // so safeguard yourself here by checking if _pImpl has already been deleted...
  return _pImpl ? _pImpl->length() : 0;
}


// virtual version of length()
unsigned int MSBinaryVector::getLength() const
{
  return _pImpl->length();
}


MSError::ErrorStatus MSBinaryVector::set (unsigned int index_, const unsigned char value_)
{
  unsigned char c = value_ ? 1 : 0;
  if (index_ < _pImpl->length())
    {
      _pImpl->set (index_, (void *)&c);
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

  
MSBinaryVector & MSBinaryVector::append (const unsigned char value_)
{
  unsigned char c = value_ ? 1 : 0;
  _pImpl->append ((void *) &c);
  appendUpdate (_pImpl->length(), 1);

  return *this;
}


MSBinaryVector & MSBinaryVector::append (const MSBinaryVector & vect_)
{
  if (_pImpl->append (*vect_._pImpl) == MSError::MSSuccess)
    appendUpdate (_pImpl->length(), vect_._pImpl->length());

  return *this;
}


MSBinaryVector & MSBinaryVector::insertAt (unsigned int index_, const unsigned char value_)
{
  if (index_ == _pImpl->length())   // NEW FEATURE:  insert right after the last element
    return append (value_);

  // if inserting inside the vector
  unsigned char c = value_ ? 1 : 0;
  if (_pImpl->insertAt (index_, (void *) &c) == MSError::MSSuccess)
    changed();

  return *this;
}


MSBinaryVector & MSBinaryVector::insertAt (unsigned int index_, const MSBinaryVector & vect_)
{
  if (index_ == _pImpl->length())  // NEW FEATURE:  insert right after the last element
    return append (vect_);
  
  // if inserting inside the vector
  if (_pImpl->insertAt (index_, *vect_._pImpl) == MSError::MSSuccess)
    changed();

  return *this;
}


MSBinaryVector & MSBinaryVector::removeAt (unsigned int index_)
{
  if (_pImpl->removeAt (index_) == MSError::MSSuccess)
    changed();

  return *this;
}


MSBinaryVector & MSBinaryVector::removeAt (unsigned int startPos_, unsigned int numEls_)
{
  if (numEls_ > 0)
    if (_pImpl->removeAt (startPos_, numEls_) == MSError::MSSuccess)
      changed();

  return *this;
}


MSBinaryVector & MSBinaryVector::remove (const MSIndexVector & iVect_)
{
  if (_pImpl->remove (iVect_) == MSError::MSSuccess)
    changed();
  
  return *this;
}


MSBinaryVector & MSBinaryVector::remove (const MSBinaryVector & bVect_)
{
  if (_pImpl->remove (bVect_) == MSError::MSSuccess)
    changed();
  
  return *this;
}


MSBinaryVector & MSBinaryVector::removeAll()
{
  if (_pImpl->removeAll() == MSError::MSSuccess)
    changed();

  return *this;
}


MSBinaryVector & MSBinaryVector::select (const MSIndexVector & iVect_)
{
  if (_pImpl->select (iVect_) == MSError::MSSuccess)
    changed();

  return *this;
}


MSBinaryVector & MSBinaryVector::compress  (const MSBinaryVector & bVect_)
{
  if (_pImpl->compress (bVect_) == MSError::MSSuccess)
    changed();
  
  return *this;
}


MSBinaryVector & MSBinaryVector::reshape (unsigned int newLen_)
{
  if (_pImpl->reshape (newLen_) == MSError::MSSuccess)
    changed();
  
  return *this;
}


MSBinaryVector & MSBinaryVector::exchange (unsigned int index1_, unsigned int index2_)
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


MSBinaryVector & MSBinaryVector::reverse()
{
  if (_pImpl->reverse() == MSError::MSSuccess)
    changed();
  
  return *this;
}


MSBinaryVector & MSBinaryVector::rotate (int amount_)
{
  if (_pImpl->rotate (amount_) == MSError::MSSuccess)
    changed();
  
  return *this;
}


MSBinaryVector & MSBinaryVector::take (int numEls_, const unsigned char filler_)
{
  const unsigned char c = filler_ ? 1 : 0;

  if (_pImpl->take (numEls_, (void *)&c) == MSError::MSSuccess)
    changed();
  
  return *this;
}


MSBinaryVector & MSBinaryVector::drop (int numEls_)
{
  if (_pImpl->drop (numEls_) == MSError::MSSuccess)
    changed();
  
  return *this;
}


MSBinaryVector MSBinaryVector::reverse (const MSBinaryVector & vect_)
{
  MSVectorImpl *impl = vect_._pImpl->create();
  impl->reverse (*vect_._pImpl);
  return MSBinaryVector (impl);
}


MSBinaryVector MSBinaryVector::rotate (const MSBinaryVector & vect_, int amount_)
{
  MSVectorImpl *impl = vect_._pImpl->create();
  impl->rotate (*vect_._pImpl, amount_);
  return MSBinaryVector (impl);
}


MSBinaryVector MSBinaryVector::take (const MSBinaryVector & vect_, int numEls_, const unsigned char filler_)
{
  const unsigned char c = filler_ ? 1 : 0;
  MSVectorImpl *impl = vect_._pImpl->create();
  impl->take (*vect_._pImpl, numEls_, (void *)&c);
  return MSBinaryVector (impl);
}


MSBinaryVector MSBinaryVector::drop (const MSBinaryVector & vect_, int numEls_)
{
  MSVectorImpl *impl = vect_._pImpl->create();
  impl->drop (*vect_._pImpl, numEls_);
  return MSBinaryVector (impl);
}


MSBinaryVector MSBinaryVector::select (const MSBinaryVector & vect_, const MSIndexVector & iVect_)
{
  MSVectorImpl *impl = vect_._pImpl->create();
  impl->select (*vect_._pImpl, iVect_);
  return MSBinaryVector (impl);
}


MSBinaryVector MSBinaryVector::compress (const MSBinaryVector & vect_, const MSBinaryVector & bVect_)
{
  MSVectorImpl *impl = vect_._pImpl->create();
  impl->compress (*vect_._pImpl, bVect_);
  return MSBinaryVector (impl);
}


MSBinaryVector & MSBinaryVector::selectiveAssign (const MSIndexVector & iVect_, const unsigned char value_)
{
  unsigned char c = value_ ? 1 : 0;
  _pImpl->setSelected (iVect_, (void *)&c);
  changed (iVect_);
  return *this;
}


MSBinaryVector & MSBinaryVector::selectiveAssign (const MSIndexVector & iVect_, const MSBinaryVector & vect_)
{
  _pImpl->setSelected (iVect_, *vect_._pImpl);
  changed (iVect_);
  return *this;
}


MSBinaryVector & MSBinaryVector::selectiveAssign (const MSBinaryVector & bVect_, const unsigned char value_)
{
  unsigned char c = value_ ? 1 : 0;
  if (doChanged() == MSTrue)
    changed (_pImpl->setIndexSelected (bVect_, (void *)&c));
  else
    _pImpl->setSelected (bVect_, (void *)&c);
    
  return *this;
}


MSBinaryVector & MSBinaryVector::selectiveAssign (const MSBinaryVector & bVect_, const MSBinaryVector & vect_)
{
  if (doChanged() == MSTrue)
    changed (_pImpl->setIndexSelected (bVect_, *vect_._pImpl));
  else
    _pImpl->setSelected (bVect_, *vect_._pImpl);
    
  return *this;
}


MSBinaryVector& MSBinaryVector::random(void)
{
  unsigned int len=_pImpl->length();
  if (len>0)
    {
      _pImpl->prepareToChangeWithoutCopy();

      MSRandom rand;
      unsigned char *pData=data();

      while (len--)
	{
	  *pData++ = (unsigned char)rand(2);  // limit is always 2; nothing else makes sense
	}
  
      changed();
    }

  return *this;
}


ostream & operator<< (ostream & stream_, const MSBinaryVector & vect_)
{
  vect_._pImpl->print (stream_);
  return stream_;
}


MSBinaryVector MSBinaryVector::doBitwiseOp (const MSBinaryVector & vect1_, const MSBinaryVector & vect2_,
					    MSBinaryVector::BitwiseOp op_)
{
  unsigned int len = vect1_.length();

  assert (len == vect2_.length());
  
  MSVectorImpl *pImpl = vect1_._pImpl->create (len, vect1_.size());

  const unsigned char *pData1 = vect1_.data(), *pData2 = vect2_.data();
  unsigned char *pData = ((Data *)pImpl->data())->elements();

  while (len--)
    (*op_)(*pData++, *pData1++, *pData2++);    
  
  return MSBinaryVector (pImpl);
}


MSBinaryVector MSBinaryVector::doBitwiseOp (const MSBinaryVector & vect_, const unsigned char value_,
					    MSBinaryVector::BitwiseOp op_)
{
  unsigned int len = vect_.length();
  const unsigned char c = (value_ > 0) ? 1 : 0;

  MSVectorImpl *pImpl = vect_._pImpl->create (len, vect_.size());
  
  const unsigned char *pData1 = vect_.data();
  unsigned char *pData = ((Data *)pImpl->data())->elements();
  
  while (len--)
    (*op_)(*pData++, c, *pData1++);
  
  return MSBinaryVector (pImpl);
}


MSBinaryVector & MSBinaryVector::doBitwiseOp (const MSBinaryVector & vect_,
					      MSBinaryVector::BitwiseAssignOp assignOp_, MSBinaryVector::BitwiseOp op_)
{
  unsigned len = length();

  assert (len == vect_.length());

  unsigned char *pData = data(), *pDataSrc = vect_.data();


  // What we do below is a little bit of a hack; since we have a reference counted vector data, we have to check
  // the reference count to see if we need to allocate our own copy of the data.  However, the data itself is stored
  // in MSVectorImpl, and we don't have direct access to it.  Therefore, we here reallocate the whole implementation
  // class, which shouldn't be much slower since the bulk of it is the vector data anyway.
  //
  if (ops().refCount(_pImpl->data())>1)
    {
      MSVectorImpl *pNewImpl = _pImpl->create (len, size());
      unsigned char *pNewData = ((Data *)pNewImpl->data())->elements();

      while (len--)
	(*op_)(*pNewData++, *pData++, *pDataSrc++);

      delete _pImpl;
      _pImpl = pNewImpl;
    }
  else 	// refCount == 1
    while (len--)
      (*assignOp_)(*pData++, *pDataSrc++);

  changed();
  return *this;
}


MSBinaryVector & MSBinaryVector::doBitwiseOp (const unsigned char value_,
					      MSBinaryVector::BitwiseAssignOp assignOp_, MSBinaryVector::BitwiseOp op_)
{
  unsigned len = length();
  const unsigned char c = (value_ > 0) ? 1 : 0;
  unsigned char *pData = data();

  if (ops().refCount(_pImpl->data())>1)
    {
      MSVectorImpl *pNewImpl = _pImpl->create (len, size());
      unsigned char *pNewData = ((Data *)pNewImpl->data())->elements();

      while (len--)
	(*op_)(*pNewData++, *pData++, c);

      delete _pImpl;
      _pImpl = pNewImpl;
    }
  else 	// refCount == 1
    while (len--)
      (*assignOp_)(*pData++, c);

  changed();
  return *this;
}


MSBinaryVector operator! (const MSBinaryVector & vect_)
{
  unsigned int len = vect_.length();
  
  MSVectorImpl *pImpl = vect_._pImpl->create (len, vect_.size());
  
  const unsigned char *pDataSrc = vect_.data();
  unsigned char *pDataDest = ((MSBinaryVector::Data *)pImpl->data())->elements();
  
  while (len--)
    *pDataDest++ = !(*pDataSrc++);
  
  return MSBinaryVector (pImpl);
}


MSBinaryVector & MSBinaryVector::complement()
{
  unsigned int len = length();
  unsigned char *pData = data();

  if (ops().refCount(_pImpl->data())>1)
    {
      MSVectorImpl *pNewImpl = _pImpl->create (len, size());
      unsigned char *pNewData = ((Data *)pNewImpl->data())->elements();

      while (len--)
	*pNewData++ = !(*pData++);

      delete _pImpl;
      _pImpl = pNewImpl;
    }
  else 	// refCount == 1
    for (unsigned int i=0; i<len; i++)
      pData[i] = !pData[i];
  
  changed();
  return *this;
}


double MSBinaryVector::sum() const
{
  double sum = 0.0;
  unsigned char *dp = data();
  unsigned n = length();
  
  while (n--)
    sum += *dp++;
  
  return sum;
}


MSBinaryVector::Operations& MSBinaryVector::ops(void)
{
  MS_SAFE_STATIC_INIT(Operations,_operationsMutex);
}


MSBinaryVector::Operations::Operations()
{
}


MSBinaryVector::Operations::~Operations()
{
}


void * MSBinaryVector::Operations::allocate (unsigned int length_, unsigned int, MSAllocationFlag) const
{
  if (length_)
    return Data::allocateWithLength (length_);
  else  // length_ == 0
    return nullData().incrementCount();
}


void * MSBinaryVector::Operations::allocateWithSize (unsigned int size_, unsigned int, MSAllocationFlag) const
{
  if (size_)
    return Data::allocateWithSize (size_);
  else  // size_ == 0
    return nullData().incrementCount();
}  


void MSBinaryVector::Operations::deallocate (void *data_, unsigned int, MSAllocationFlag) const
{
  ((Data *)data_)->decrementCount();
}


void MSBinaryVector::Operations::incrementCount (void *data_) const
{
  ((Data *)data_)->incrementCount();
}


unsigned int MSBinaryVector::Operations::refCount (const void *data_) const
{
  return ((Data *)data_)->refCount();
}


void MSBinaryVector::Operations::set (void *data_, unsigned int index_, const void *value_, MSAllocationFlag) const
{
  ((Data *)data_)->elements()[index_] = *(unsigned char *)value_;
}


void MSBinaryVector::Operations::set (void *pDest_, unsigned int destInd_, const void *pSrc_, unsigned int srcInd_,
				      MSAllocationFlag) const
{
  ((Data *)pDest_)->elements()[destInd_] = ((Data *)pSrc_)->elements()[srcInd_];
}


void MSBinaryVector::Operations::fill (void *pElements_, unsigned int start_, unsigned int numToFill_,
				       const void *pValue_, MSAllocationFlag) const
{
  unsigned char *pStart = ((Data *)pElements_)->elements() + start_;

  if (pValue_)
    Data::fill (pStart, numToFill_, *(unsigned char *)pValue_);
  else
    Data::fill (pStart, numToFill_, *(unsigned char *)defaultFiller());
}


void MSBinaryVector::Operations::copy (const void *src_, void *dest_, unsigned int length_,
				       unsigned int srcInd_, unsigned int destInd_, MSAllocationFlag) const
{
  Data::copy (((Data *)src_)->elements() + srcInd_, ((Data *)dest_)->elements() + destInd_, length_);
}


void MSBinaryVector::Operations::copyBackward (void *pElements_, unsigned int src_, unsigned int dest_,
					       unsigned int numToCopy_) const
{
  unsigned char *pElements = ((Data *)pElements_)->elements();

  Data::copyBackward (pElements+src_, pElements+dest_, numToCopy_);
}


void MSBinaryVector::Operations::destroy (void *, unsigned int, unsigned int) const
{
}


int MSBinaryVector::Operations::isElementEqual (const void *pElements_, unsigned int index_, const void *pValue_) const
{
  return ((Data *)pElements_)->elements()[index_] == *(unsigned char *)pValue_;
}


int MSBinaryVector::Operations::isElementLess (const void *pElements_, unsigned int index_, const void *pValue_) const
{
  return ((Data *)pElements_)->elements()[index_] < *(unsigned char *)pValue_;
}


int MSBinaryVector::Operations::isElementLessEqual (const void *pElements_, unsigned int index_, const void *pValue_) const
{
  return ((Data *)pElements_)->elements()[index_] <= *(unsigned char *)pValue_;
}


long MSBinaryVector::Operations::compareElement (const void *data_, unsigned int index_, const void *value_) const
{
  const unsigned char el1 = ((Data *)data_)->elements()[index_], el2 = *(unsigned char *)value_;
  return (el1 == el2) ? 0 : ((el1 < el2) ? -1 : 1);
}


void *MSBinaryVector::Operations::elementAt (const void *pElements_, unsigned int index_) const
{
  return & ((Data *)pElements_)->elementAt (index_);
}


unsigned int MSBinaryVector::Operations::size (const void *pElements_) const
{
  return ((Data *)pElements_)->size();
}


void MSBinaryVector::Operations::swapElements (void *data_, unsigned int ind1_, unsigned int ind2_) const
{
  unsigned char *pElements = ((Data *)data_)->elements();
  unsigned char temp = pElements[ind1_];
  pElements[ind1_] = pElements[ind2_], pElements[ind2_] = temp;
}
  

unsigned int MSBinaryVector::Operations::gradeUp (const void *pData_, unsigned int len_, unsigned int *pResult_) const
{
  return msMergeSortUp (len_, ((Data *)pData_)->elements(), pResult_, (unsigned int)0, len_);
}


unsigned int MSBinaryVector::Operations::gradeDown (const void *pData_, unsigned int len_, unsigned int *pResult_) const
{
  return msMergeSortDown (len_, ((Data *)pData_)->elements(), pResult_, (unsigned int)0, len_);
}


void *MSBinaryVector::Operations::badData() const
{
  static unsigned char badValue =0;
  return (void *)&badValue;
}


void *MSBinaryVector::Operations::defaultFiller() const
{
  static unsigned char filler =0;
  return (void *)&filler;
}


MSString MSBinaryVector::Operations::asString (const void *pData_, unsigned int index_) const
{
  return MSString ((unsigned int)((Data *)pData_)->elements()[index_]);
}


MSString MSBinaryVector::Operations::asMSF (const void *pData_, unsigned int index_) const
{
  return MSString ((unsigned int)((Data *)pData_)->elements()[index_]);
}


unsigned int MSBinaryVector::Operations::elementLen (const void *, unsigned int) const
{
  return 0;
}


MSError::ErrorStatus MSBinaryVector::Operations::setFromString (void *pData_, unsigned int index_, const char *pString_) const
{
  ((Data *)pData_)->elements()[index_] = (*pString_=='0') ? 0 : 1;
  return MSError::MSSuccess;
}


MSError::ErrorStatus MSBinaryVector::Operations::setFromMSF (void *pData_, unsigned int index_, const char *pString_) const
{
  return setFromString (pData_, index_, pString_);
}


void MSBinaryVector::Operations::setFromMSString (void *pData_, unsigned int vectIndex_,
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
      ((Data *)pData_)->elements()[vectIndex_] = (str_(startPos_)=='0') ? 0 : 1;
      startPos_ = str_.indexOfAnyBut(MSStringTest(APLUS_ISPACE),endPos);
    }
}


unsigned int MSBinaryVector::Operations::numElements (const MSString & str_, const char) const
{
  return str_.numWords();
}


void MSBinaryVector::Operations::print (const void *pData_, unsigned int index_, ostream & stream_) const
{
  stream_ << (unsigned int)((Data *)pData_)->elements()[index_] << " ";
}


MSBinaryVector::Data& MSBinaryVector::Operations::nullData(void)
{
  MS_SAFE_STATIC_INIT(Data,_nullDataMutex);
}


MSBinaryVector::Data::Data (unsigned int size_) : MSData (size_)
{
}


MSBinaryVector::Data::~Data()
{
}


MSString MSBinaryVector::Data::asDebugInfo() const
{
  MSString result ("MSBinaryVector::Data(@");
  result += MSString((unsigned long)this).d2x().lowerCase();
  result += ",_data=";
  result += MSString((unsigned long)elements()).d2x().lowerCase();
  result += ",_refCount=";
  result += MSString(refCount());
  result += ")";
  return result;
}


void * MSBinaryVector::Data::operator new(size_t, unsigned int numEls_)
{
  // We need to allocate memory for sizeof(Data) plus the size of the data elements array
  // except for the first element which is included in the size of Data
  //
  unsigned int realSize;
  if (numEls_>1)
    {
      realSize = sizeof(Data) + (numEls_-1)*sizeof(unsigned char);
    }
  else	// 0 or 1 elements to be allocated
    {
      realSize = sizeof(Data);	// Data already includes 1 data element
    }

  return (void *) ::new char[realSize];
}
  

MSBinaryVector::Data *MSBinaryVector::Data::allocateWithLength (unsigned length_) 
{ 
  unsigned int newLength = MSData::computeSize(length_);  
  return new (newLength)Data(newLength); 
}


MSBinaryVector::Data *MSBinaryVector::Data::allocateWithSize (unsigned size_) 
{
  return new (size_)Data(size_);
}


void MSBinaryVector::Data::deallocate()
{
  delete this;
}


void MSBinaryVector::Data::fill (unsigned char *pElements_, unsigned int length_, const unsigned char value_)
{
  while (length_--)
    *pElements_++ = value_;
}


void MSBinaryVector::Data::copy(const unsigned char *src_, unsigned char *dst_, unsigned int length_)
{
  while (length_--)
    *dst_++ = *src_++;
}


void MSBinaryVector::Data::copyBackward (const unsigned char *pSrc_, unsigned char *pDest_, unsigned int length_)
{
  while (length_--)
    *pDest_-- = *pSrc_--;
}


MSBinaryVector::SPick & MSBinaryVector::SPick::operator= (const MSBinaryVector::SPick & sPick_)
{
  _pVector->set (_index, (*sPick_._pVector)(sPick_._index));
  return *this;
}


MSBinaryVector::SPick & MSBinaryVector::SPick::operator= (const unsigned char aScalar_)
{
  _pVector->set (_index, aScalar_);
  return *this;
}


void bitwiseAND (unsigned char & res, const unsigned char a, const unsigned char b)
{
  res = a & b;
}


unsigned char & bitwiseAND (unsigned char & res, const unsigned char a)
{
  return (res &= a);
}


void bitwiseOR (unsigned char & res, const unsigned char a, const unsigned char b)
{
  res = a | b;
}


unsigned char & bitwiseOR (unsigned char & res, const unsigned char a)
{
  return (res |= a);
}


void bitwiseXOR (unsigned char & res, const unsigned char a, const unsigned char b)
{
  res = a ^ b;
}


unsigned char & bitwiseXOR (unsigned char & res, const unsigned char a)
{
  return (res ^= a);
}
