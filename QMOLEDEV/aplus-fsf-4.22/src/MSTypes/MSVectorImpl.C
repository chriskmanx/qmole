///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <MSTypes/MSUtil.H>
#include <MSTypes/MSTypes_MSF.H>

#include <MSTypes/MSVectorImpl.H>
#include <MSTypes/MSVectorImplInlines.C>
#include <MSTypes/MSMessageLog.H>

MSVectorImpl::MSVectorImpl (MSVectorImplOps *pOperations_, unsigned int length_)
  : _pOperations(pOperations_), _len(length_), _pElements(pOperations_->allocate(length_, length_))
{
}
  

MSVectorImpl::MSVectorImpl (MSVectorImplOps *pOps_, unsigned int length_, void *pFiller_)
  : _pOperations(pOps_), _len(length_), _pElements(pOps_->allocate(length_))
{
  _pOperations->fill (_pElements, 0, _len, pFiller_, MSRaw);
}
  

MSVectorImpl::MSVectorImpl (const MSVectorImpl & vImpl_)
  : _pOperations(vImpl_._pOperations), _len(vImpl_._len), _pElements(vImpl_._pElements)
{
  _pOperations->incrementCount (_pElements);
}


MSVectorImpl::MSVectorImpl (MSVectorImplOps *pOps_, void *pData_, unsigned int len_)
  : _pOperations(pOps_), _len(len_), _pElements(pData_)
{
}


MSVectorImpl::~MSVectorImpl()
{
  _pOperations->deallocate (_pElements, _len);
}


MSVectorImpl *MSVectorImpl::create (unsigned int len_, unsigned int size_) const
{
  if (size_)
    return new MSVectorImpl (_pOperations, _pOperations->allocateWithSize (size_, len_), len_);
  else
    return new MSVectorImpl (_pOperations, len_);
}


MSVectorImpl *MSVectorImpl::clone() const
{
  return new MSVectorImpl (*this);
}


void MSVectorImpl::indexError (unsigned int index_) const
{
  MSMessageLog::errorMessage("MSVectorImpl Index Error:  index %d\tlength %d\n",index_, _len );
}


unsigned int MSVectorImpl::minSize()
{
  static unsigned int size = 8;
  return size;
}


void *MSVectorImpl::reallocate (unsigned int newLen_)
{
  unsigned int size = _pOperations->size (_pElements);
  unsigned int min_size = minSize();
  
  if (newLen_ > size || _pOperations->refCount (_pElements) > 1 || (newLen_<size/2 && size>min_size))
    return (newLen_ <= min_size) ? _pOperations->allocateWithSize (min_size) : _pOperations->allocate (newLen_);
  else
    return _pElements;
}


void MSVectorImpl::reallocateAndCopy(unsigned int newLen_)
{
  void *pNewElements=reallocate(newLen_);
  if (pNewElements!=_pElements)	 // if reallocation took place
    {
      _pOperations->copy(_pElements,pNewElements,_len,0,0,MSRaw);
      _pOperations->deallocate(_pElements,_len);
      _pElements=pNewElements;
    }
}


void MSVectorImpl::blockLeft (unsigned int start_, unsigned int width_, unsigned int moveCount_)
{
// ASSERTION:  the move is within bounds

  unsigned int target = start_ - moveCount_;
  
  _pOperations->copy (_pElements, _pElements, width_, start_, target);
}

  
void MSVectorImpl::blockRight (unsigned int start_, unsigned int width_, unsigned int moveCount_)
{
  // *** ASSERTION ***:  the move is within bounds
  //
  if (width_==0 || moveCount_==0)
    {
      return;
    }

  unsigned int end = start_ + width_;
  unsigned int targetEnd = end + moveCount_;
  
  if (targetEnd>_len)	// if we have to construct some elements on the end
    {
      // find how many elements need to be constructed
      unsigned int numToConstruct = (start_+moveCount_<_len) ? targetEnd-_len : width_;
      // construct those elements with the copy constructor
      _pOperations->copy (_pElements, _pElements, numToConstruct, end-numToConstruct, targetEnd-numToConstruct, MSRaw);
      // copy the rest of the elements
      _pOperations->copyBackward (_pElements, end-numToConstruct-1, targetEnd-numToConstruct-1, width_-numToConstruct);
    }
  else	// if no element construction is necessary
    {
      _pOperations->copyBackward (_pElements, end-1, targetEnd-1, width_);
    }
}


void MSVectorImpl::prepareToChangeWithoutCopy()
{
  if (_pOperations->refCount (_pElements) > 1)
    {
      _pOperations->deallocate (_pElements);
      _pElements = _pOperations->allocateWithSize (_pOperations->size (_pElements));
    }
}


void MSVectorImpl::reallocateInPlace(unsigned int length_)
{
  void *pNewData = reallocate (length_);
  if (pNewData != _pElements)
    {
      _pOperations->deallocate (_pElements);
      _pElements = pNewData;
    }

  _len = length_;
}


void MSVectorImpl::makeUniqueCopy()
{
  void *newData = _pOperations->allocate (_len);

  _pOperations->copy (_pElements, newData, _len, 0, 0, MSRaw);
  _pOperations->deallocate (_pElements, _len);
  _pElements = newData;
}


MSVectorImpl & MSVectorImpl::operator= (const MSVectorImpl & vectImpl_)
{
  if (this != &vectImpl_)
    {
      _pOperations->deallocate (_pElements, _len);   // decrement reference count on the current data      
      _pElements = vectImpl_._pElements;
      _pOperations->incrementCount (_pElements);   // increment reference count on the new data
      _len = vectImpl_._len;
    }
  
  return *this;
}


MSString MSVectorImpl::asString (const char separator_)
{
  MSString result;

  if (_len)
    {
      if (separator_)
	{
	  for (unsigned int i=0;;) 
	    {
	      // _len is at least 1
	      result << _pOperations->asString (_pElements, i++);
	      if (i < _len)
		result << separator_;
	      else
		break;
	    }
	}
      else  // we don't wont any separators between the elements
	{
	  for (unsigned int i=0; i<_len; ++i)
	    result << _pOperations->asString (_pElements, i);
	}
    }

  return result;
}


MSString MSVectorImpl::asMSF()
{
  MSString result;

  result << MSMSF_US << MSString (_len);

  for (unsigned int i=0; i<_len; i++)
    result << MSMSF_US << _pOperations->asMSF (_pElements, i);

  return result;
}


MSError::ErrorStatus MSVectorImpl::setFromString (const char *pString_, const char delimiter_)
{
  MSError::ErrorStatus rc = MSError::MSSuccess;

  _pOperations->deallocate (_pElements, _len);
  _len = 0;

  if (pString_)
    {
      _pElements = _pOperations->allocate (0);  // here it's almost a no-op simply returning the null VectorData
      if (! append (pString_, delimiter_))
	rc = MSError::MSFailure;
    }
  else  // if pString is NULL
    _pElements = _pOperations->allocate (0);
  
  return rc;
}


MSError::ErrorStatus MSVectorImpl::setFromMSF (const char *pString_)
{
  MSError::ErrorStatus rc;
  unsigned int i;
  unsigned int startpos;
  unsigned int value;
  char *pstring;
  const char *pcurrent;
  
  if (pString_ && (*pString_ == MSMSF_US) && (strlen (pString_) > sizeof(MSMSF_US)))
    {
      rc = MSError::MSSuccess;
      _pOperations->deallocate (_pElements, _len);
      _len = 0;
     
      MSString decode (pString_);
      unsigned int slen = decode.length();
      startpos = sizeof(MSMSF_US);
      pcurrent = (const char *)decode.string()+startpos;

      // first get the size of the array....it the first element in the string then point to the beginning of the data
      //
      value = 0;
      if (isdigit (*pcurrent))
	{
	  value = strtoul (pcurrent, &pstring, 10);
	  if (*pstring != '\0')
	    startpos = decode.indexOf (MSMSF_US, startpos);
	  else
	    {
	      value = 0;
	      rc = MSError::BadMSFString;
	    }
	}

      // needs to handle two cases as single MSMSF_US between vector elements and two
      // MSMSF_US between vector elements.  There will be only one delimiter in 
      // between nonvector types and there will be two delimiters separating vector
      // types

      _len = value;
      _pElements = _pOperations->allocate (_len, _len);

      if (_len > 0)
	{
	  unsigned int nextpos, objectsize, nextvector, startvector;

	  for (i=0; i<_len; i++)
	    {
	      if (startpos < slen)
		{ 
		  startpos = startpos + sizeof(MSMSF_US);
		  nextpos = decode.indexOf (MSMSF_US, startpos);
		  objectsize = 0;

                  // if its a vector in a vector need to parse past the size and all its elements for
                  // each element in the contained vector, be sure to exclude end of string

		  if (nextpos == startpos) 
		    {
		      startvector = nextpos; 
		      nextvector = startvector + sizeof(MSMSF_US);
		      value = 0;
		      pcurrent = (const char *)decode.string() + nextvector;
		      if (isdigit (*pcurrent))
			{
			  value = strtoul (pcurrent, &pstring, 10);
			  if (*pstring != '\0')
			    nextvector = decode.indexOf (MSMSF_US, nextvector);
			  else
			    value=0;
			}

		      if ((value == 0) || (nextvector >= slen))
			{
			  rc = MSError::BadMSFString;
			  break;
			}

                      // now skip past all the elements in this subvector

		      for (int j=0; j<value; j++)
			{
			  if (nextvector < slen)
			    {
			      nextvector = nextvector + sizeof(MSMSF_US);
			      nextvector = decode.indexOf (MSMSF_US, nextvector);
			    }
			  else
			    {
			      rc = MSError::BadMSFString;
			      break;
			    }
			}
		      
		      objectsize = nextvector-startvector;
		      startpos = startvector;
		      nextpos = nextvector;
		    }
                 // this is all you need to do for the nonvector case
		  else
		    objectsize = nextpos - startpos;

                 // now copy the element and put it into the object

		  char *buf = new char[objectsize+1];
		  strncpy (buf, (const char *)decode.string()+startpos, objectsize);
		  buf[objectsize] = '\0';
		  rc = _pOperations->setFromMSF (_pElements, i, buf);
		  delete [] buf;
		  
		  if (rc == MSError::MSSuccess)
		    startpos = nextpos;
		  else
		    break;
		}
	      else
		{
		  rc = MSError::BadMSFString;
		  break;
		}
	    }
	}
      else
	rc = MSError::BadMSFString;
    }
  else
    rc = MSError::BadMSFString;

  if (rc != MSError::MSSuccess)
    removeAll();
  
  return rc;
}


void MSVectorImpl::set (unsigned index_, void *value_)
{ 
  // we avoid calling reallocate() here to reduce the number of comparisons that it makes to determine
  // if reallocation is needed; we need only to check the reference count since the length remains the
  // same
  if (_pOperations->refCount (_pElements) > 1)
    {
      void *pNewData = _pOperations->allocateWithSize (_pOperations->size (_pElements));

      _pOperations->copy (_pElements, pNewData, index_, 0, 0, MSRaw);
      _pOperations->copy (_pElements, pNewData, _len-index_-1, index_+1, index_+1, MSRaw);
      _pOperations->set  (pNewData, index_, value_, MSRaw);

      _pOperations->deallocate (_pElements, _len);
      _pElements = pNewData;
    }
  else  // if (refCount == 1) --> no reallocation
    _pOperations->set (_pElements, index_, value_);
}


void MSVectorImpl::setAll (const void *pValue_)
{
  if (_pOperations->refCount (_pElements) > 1)
    {
      _pOperations->deallocate (_pElements, _len);
      _pElements = _pOperations->allocateWithSize (_pOperations->size (_pElements));
      _pOperations->fill (_pElements, 0, _len, pValue_, MSRaw);
    }
  else	// if (refCount == 1) --> no reallocation
    _pOperations->fill (_pElements, 0, _len, pValue_);
}


void MSVectorImpl::setSelected (const MSIndexVector & iVect_, const MSVectorImpl & vImpl_)
{
  assert (vImpl_._len == iVect_.length());
  
  // The test below means that if &vImpl_ == this, we are, in effect, permuting the vector (since the
  // index vector is not necessarily sorted up).  This situation may arise, for example, if the user is doing
  // something like this:
  //
  //                   vect [vect.gradeUp()] = vect;
  //
  // This will effectively sort the vector up.  However, it will be slower than using sortUp() directly.
  //
  if (this == &vImpl_)
    {
      permute (iVect_);
      return;
    }

// we don't optimize reallocation here since there is no easy way to copy the elements NOT selected by an index vector
  
  prepareToChange();

  const unsigned int *pv = iVect_.data();
 
  for (unsigned int i=0; i<vImpl_._len; i++)
    internalSet (pv[i], vImpl_, i);
}


void MSVectorImpl::setSelected (const MSIndexVector & iVect_, const void *value_)
{
// we don't optimize reallocation here since there is no easy way to copy the elements NOT selected by an index vector
  
  prepareToChange();

  unsigned int n = iVect_.length();
  const unsigned *pIndices = iVect_.data();

  for (unsigned int i=0; i<n; i++)
    internalSet (pIndices[i], value_);
}


void MSVectorImpl::setSelected (const MSBinaryVector & bVect_, const MSVectorImpl & vImpl_)
{
  assert (vImpl_._len == bVect_.sum());
  
  if (this == &vImpl_)  // since we've already checked that lengths are equal, this, in effect, does nothing
    return;

  const unsigned char *pBinaries = bVect_.data();
  unsigned int i=0, j=0;

  if (_pOperations->refCount (_pElements) == 1)
    {
      for (; j<vImpl_._len; i++)
	if (pBinaries[i])
	  internalSet (i, vImpl_, j++);
    }
  else	// if we have reallocated
    {
      void *pNewData = _pOperations->allocateWithSize (_pOperations->size (_pElements));
      unsigned int end = MSUtil::min (_len, bVect_.length());
      
      for (; i<end; i++)
	{
	  if (pBinaries[i])
	    _pOperations->set (pNewData, i, vImpl_._pElements, j++, MSRaw);
	  else
	    _pOperations->set (pNewData, i, _pElements, i, MSRaw);
	}

      if (i < _len)	// if bVect_.length() < _len
	_pOperations->copy (_pElements, pNewData, _len-i, i, i, MSRaw);   // copy the rest of the elements
      
      _pOperations->deallocate (_pElements, _len);
      _pElements = pNewData;
    }
}


void MSVectorImpl::setSelected (const MSBinaryVector & bVect_, const void *pValue_)
{
  unsigned int n=bVect_.length(), i=0;
  const unsigned char *pBinaries = bVect_.data();

  if (_pOperations->refCount (_pElements) == 1)
    {
      for (; i<n; i++)
	if (pBinaries[i])
	  internalSet (i, pValue_);
    }
  else   // we have reallocated
    {
      void *pNewData = _pOperations->allocateWithSize (_pOperations->size (_pElements));
      unsigned int end = MSUtil::min (_len, bVect_.length());

      for (; i<end; i++)
	{
	  if (pBinaries[i])
	    _pOperations->set (pNewData, i, pValue_, MSRaw);
	  else
	    _pOperations->set (pNewData, i, _pElements, i, MSRaw);
	}

      if (i < _len)
	_pOperations->copy (_pElements, pNewData, _len-i, i, i, MSRaw);  // copy the rest of the elements

      _pOperations->deallocate (_pElements, _len);
      _pElements = pNewData;
    }
}


MSIndexVector MSVectorImpl::setIndexSelected (const MSBinaryVector & bVect_, const MSVectorImpl & vImpl_)
{
  assert (vImpl_._len == bVect_.sum());
  
  if (this == &vImpl_)
    return MSIndexVector::nullVector();

  const unsigned char *pBinaries = bVect_.data();

  MSIndexVector::Data *pIndexData = MSIndexVector::Data::allocateWithSize (_pOperations->size (vImpl_._pElements));
  unsigned int *pIndices = pIndexData->elements();
  unsigned int i=0, j=0;

  if (_pOperations->refCount (_pElements) == 1)
    {
      for (; j<vImpl_._len; i++)
	{
	  if (pBinaries[i])
	    {
	      internalSet (i, vImpl_, j++);
	      *pIndices++ = i;
	    }
	}
    }
  else   // if we have reallocated
    {
      void *pNewData = _pOperations->allocateWithSize (_pOperations->size (_pElements));
      unsigned int end = MSUtil::min (_len, bVect_.length());
      
      for (; i<end; i++)
	{
	  if (pBinaries[i])
	    {
	      _pOperations->set (pNewData, i, vImpl_._pElements, j++, MSRaw);
	      *pIndices++ = i;
	    }
	  else
	    _pOperations->set (pNewData, i, _pElements, i, MSRaw);
	}

      if (i < _len)
	_pOperations->copy (_pElements, pNewData, _len-i, i, i, MSRaw);   // copy the rest of the elements

      _pOperations->deallocate (_pElements, _len);
      _pElements = pNewData;
    }

  return MSIndexVector (pIndexData, vImpl_._len);
}


MSIndexVector MSVectorImpl::setIndexSelected (const MSBinaryVector & bVect_, const void *pValue_)
{

  unsigned int n = bVect_.length(), numChanged = (unsigned int)bVect_.sum(), i=0;
  const unsigned char *pBinaries = bVect_.data();
  
  MSIndexVector::Data *pIndexData = MSIndexVector::Data::allocateWithLength (numChanged);
  unsigned int *pIndices = pIndexData->elements();

  if (_pOperations->refCount (_pElements) == 1)
    {
      for (; i<n; i++)
	{
	  if (pBinaries[i])
	    {
	      internalSet (i, pValue_);
	      *pIndices++ = i;
	    }
	}
    }
  else  // if we have reallocated
    {
      void *pNewData = _pOperations->allocateWithSize (_pOperations->size (_pElements));
      unsigned int end = MSUtil::min (_len, bVect_.length());
      
      for (; i<end; i++)
	{
	  if (pBinaries[i])
	    {
	      _pOperations->set (pNewData, i, pValue_, MSRaw);
	      *pIndices++ = i;
	    }
	  else
	    _pOperations->set (pNewData, i, _pElements, i, MSRaw);
	}

      if (i < _len)
	_pOperations->copy (_pElements, pNewData, _len-i, i, i, MSRaw);   // copy the rest of the elements

      _pOperations->deallocate (_pElements, _len);
      _pElements = pNewData;
    }

  return MSIndexVector (pIndexData, numChanged);
}


void MSVectorImpl::print (ostream & stream_) const
{
  for (unsigned int i=0; i<_len; i++)
    _pOperations->print (_pElements, i, stream_);
  
  stream_ << flush;
}  


unsigned int MSVectorImpl::indexOf (void *value_, unsigned int startPos_) const
{
  for (unsigned int i=startPos_; i<_len; i++)
    if (_pOperations->isElementEqual (_pElements, i, value_))
      return i;

  return _len;
}


unsigned int MSVectorImpl::lastIndexOf (void *value_, unsigned int startPos_) const
{
  if (_len > 0)
    {
      unsigned int i;
      
      i = (startPos_ < _len) ? startPos_ : _len -1;

      for (; i; --i)
	if (_pOperations->isElementEqual (_pElements, i, value_))
	  return i;
     
      if (_pOperations->isElementEqual (_pElements, i, value_))
	return i;
    }

  return _len;
}
  

unsigned int MSVectorImpl::occurrencesOf (const void *pValue_, unsigned int startPos_) const
{
  unsigned int i,count;
  for (i=startPos_, count=0; i<_len; ++i)
    if (_pOperations->isElementEqual (_pElements, i, pValue_))
      ++count;

  return count;
}


MSIndexVector MSVectorImpl::indicesOf (const MSVectorImpl & vImpl_) const
{
  if (_len > 0 && vImpl_._len > 0)
   {
     MSIndexVector::Data *d = MSIndexVector::Data::allocateWithSize (_pOperations->size (vImpl_._pElements));
     unsigned int *pIndices = d->elements();
     unsigned int i, j;
     
     for (i=0; i<vImpl_._len; i++)
      {
	void *element = vImpl_._pOperations->elementAt (vImpl_._pElements, i);
	
	for (j=0; j<_len; j++)
	 {
	   if (_pOperations->isElementEqual (_pElements, j, element))
	    {
              pIndices[i] = j;
	      break;
	    }
	 }
	
	if (j == _len)
	  pIndices[i] = _len;
      }

     return MSIndexVector (d, vImpl_._len);
   }

  return MSIndexVector();
}
  

MSBinaryVector MSVectorImpl::memberOf (const MSVectorImpl & vImpl_) const
{
  if (_len > 0 && vImpl_._len > 0)
   {
     MSBinaryVector::Data *d = MSBinaryVector::Data::allocateWithSize (_pOperations->size (_pElements));
     unsigned char *pIndices = d->elements();
     unsigned int i, j;

     for (i=0; i<_len; i++)
      {
	void *element = _pOperations->elementAt (_pElements, i);

	for (j=0; j<vImpl_._len; j++)
	 {
	   if (vImpl_._pOperations->isElementEqual (vImpl_._pElements, j, element))
	    {
              pIndices[i] = 1;
	      break;
	    }
	 }

	if (j == vImpl_._len)
	  pIndices[i] = 0;
      }
     
     return MSBinaryVector (d, _len);
   }
  
  return MSBinaryVector();
}


MSError::ErrorStatus MSVectorImpl::append (void *newElement)
{
  reallocateAndCopy(_len+1);
  _pOperations->set (_pElements, _len, newElement, MSRaw);
  ++_len;

  return MSError::MSSuccess;
}


unsigned int MSVectorImpl::append (const char *pString_, const char delimiter_)
{
  MSString aString (pString_);
  unsigned int numNew = _pOperations->numElements (aString, delimiter_);
  if (!numNew)
    return 0;

  reallocateAndCopy(_len + numNew);

  // a little hack:  default-construct the new elements so that setFromMSString doesn't have
  // to worry about it
  _pOperations->fill (_pElements, _len, numNew, 0, MSRaw);

  unsigned int startPos=0;

  for (unsigned int i=0; i<numNew; ++i)
    {
      //This call will modify startPos to point to the next element
      //for subsequent calls.
      _pOperations->setFromMSString(_pElements, _len++, aString, startPos, delimiter_);
    }

  return numNew;
}


MSError::ErrorStatus MSVectorImpl::append (const MSVectorImpl & vImpl_)
{
  if (vImpl_._len > 0)
    {
      reallocateAndCopy(_len + vImpl_._len);
      _pOperations->copy (vImpl_._pElements, _pElements, vImpl_._len, 0, _len, MSRaw);
      _len += vImpl_._len;

      return MSError::MSSuccess;
    }

  return MSError::MSFailure;
}  


MSError::ErrorStatus MSVectorImpl::insertAt (unsigned int index_, void *value_)
{
  // ASSERTION:  index_ != _len

  if (index_ < _len)
    {
      // checkpoint:  _len > 0 since index_<_len

      void *pNewData = reallocate (_len +1);
      
      if (pNewData != _pElements)   // if we had to do reallocation
	{
	  _pOperations->copy (_pElements, pNewData, index_, 0, 0, MSRaw);
	  _pOperations->copy (_pElements, pNewData, _len-index_, index_, index_+1, MSRaw);
	  _pOperations->set  (pNewData, index_, value_, MSRaw);

	  // we have copied all of existing data; now we can get switch to the new copy
	  _pOperations->deallocate (_pElements, _len);
	  _pElements = pNewData;
	}
      else  // if we are doing insertion in-place
	{
	  blockRight (index_, _len-index_, 1);
	  _pOperations->set (_pElements, index_, value_);
	}
  
      _len++;

      return MSError::MSSuccess;
    }

  return MSError::MSFailure;
}


MSError::ErrorStatus MSVectorImpl::insertAt (unsigned int index_, const MSVectorImpl & vImpl_)
{
  // ASSERTION:  index_ != _len
  
  if (index_ < _len)
    {
      unsigned int newLen = _len + vImpl_._len;

      // checkpoint:  newLen > 0 since index_<_len

      void *pNewData = reallocate (newLen);
      
      if (pNewData != _pElements)   // if we had to do reallocation
	{
	  // copy elements before index_
	  _pOperations->copy (_pElements, pNewData, index_, 0, 0, MSRaw);
	  // copy elements after index_
	  _pOperations->copy (_pElements, pNewData, _len-index_, index_, index_ + vImpl_._len, MSRaw);
	  // copy the elements from vImpl_ starting at index_
	  _pOperations->copy (vImpl_._pElements, pNewData, vImpl_._len, 0, index_, MSRaw);

	  // we can get switch to the new copy of the data
	  _pOperations->deallocate (_pElements, _len);
	  _pElements = pNewData;
	}
      else  // if we are doing insertion in-place
	{
	  blockRight (index_, _len-index_, vImpl_._len);
	  if (index_+vImpl_._len > _len)   // if some elements will need to be constructed
	    {
	      unsigned int n = _len-index_;
	      // copy the already-constructed elements
	      _pOperations->copy (vImpl_._pElements, _pElements, n, 0, index_);
	      // copy the uninitialized (raw) elements
	      _pOperations->copy (vImpl_._pElements, _pElements, vImpl_._len-n, n, _len, MSRaw);
	    }
	  else	// if no element construction is necessary
	    {
	      _pOperations->copy (vImpl_._pElements, _pElements, vImpl_._len, 0, index_);
	    }
	}

      _len = newLen;

      return MSError::MSSuccess;
    }

  return MSError::MSFailure;
}


MSError::ErrorStatus MSVectorImpl::removeAt (unsigned int startPos_, unsigned int numEls_)
{
  if (startPos_ < _len)
    {
      unsigned int numDoomed = (startPos_+numEls_ <= _len) ? numEls_ : _len - startPos_;
      unsigned int newLen = _len - numDoomed, endPos = startPos_ + numDoomed;
  
      void *pNewData = reallocate (newLen);

      if (pNewData != _pElements)    // if we had to do reallocation
	{
	  _pOperations->copy (_pElements, pNewData, startPos_, 0, 0, MSRaw);
	  _pOperations->copy (_pElements, pNewData, _len-endPos, endPos, startPos_, MSRaw);

	  // we have copied all of existing data; now we can switch to the new copy
	  _pOperations->deallocate (_pElements, _len);
	  _pElements = pNewData;
	}
      else	// if removing in-place
	{
	  if (startPos_ < newLen)  // if we are not removing the last elements, move them to the left
	    blockLeft (endPos, _len-endPos, numDoomed);

	  _pOperations->destroy (_pElements, newLen, numDoomed);
	}

      _len = newLen;

      return MSError::MSSuccess;
    }

  return MSError::MSFailure;
}  


MSError::ErrorStatus MSVectorImpl::remove (const MSIndexVector & iVect_)
{
  const unsigned int iVectLen = iVect_.length();
  
  if (iVectLen > 0) 
    {
      const MSIndexVector sortVect = iVect_.gradeUp();

      unsigned int numRemoved = 0;
      unsigned int dataIndex = iVect_(sortVect(0));   // start at the first element to be removed
      unsigned int newDataIndex = dataIndex;

      unsigned int *pIndices = iVect_.data();
      unsigned int *pSortedIndices = sortVect.data();
     
      // since we don't know for sure the resulting length, we will make reallocate() do reallocation only if the
      // reference count is >1
      void *pNewData = reallocate (_len);
      MSAllocationFlag flag = (pNewData==_pElements) ? MSConstructed : MSRaw;

      for (unsigned int iVectIndex=0; dataIndex < _len && iVectIndex < iVectLen; dataIndex++) 
	{
	  if (dataIndex == pIndices [pSortedIndices[iVectIndex]]) // if this element is to be removed
	    {
	      numRemoved++;
	      do		// skip through all indices that are same as current iVectIndex
		iVectIndex++;
	      while (iVectIndex < iVectLen && dataIndex==pIndices[pSortedIndices[iVectIndex]]);
	    }
	  else    // if this element will remain in the vector, move it to new location
	    _pOperations->set (pNewData, newDataIndex++, _pElements, dataIndex, flag);
	}

      unsigned int newLen = _len - numRemoved;

      if (pNewData == _pElements)
	{
	  if (dataIndex < _len)  // if all corresponding elements have been removed, move the rest to the left
	    blockLeft (dataIndex, _len - dataIndex, dataIndex - newDataIndex);

	  _pOperations->destroy (_pElements, newLen, numRemoved);
	}
      else	// we had to reallocate
	{
	  // copy the beginning and trailing elements (before and after the boundaries of iVect_)
	  _pOperations->copy (_pElements, pNewData, pIndices[pSortedIndices[0]], 0, 0, MSRaw);
	  _pOperations->copy (_pElements, pNewData, _len-dataIndex, dataIndex, newDataIndex, MSRaw);

	  _pOperations->deallocate (_pElements, _len);
	  _pElements = pNewData;
	}
     
      _len = newLen;

      if (numRemoved > 0)
	return MSError::MSSuccess;
    }

  return MSError::MSFailure;
}


MSError::ErrorStatus MSVectorImpl::remove (const MSBinaryVector & bVect_)
{
  unsigned int numDoomed = (unsigned int)bVect_.sum();
  unsigned int numLeft = _len - numDoomed;

  if (numLeft != _len && _len==bVect_.length()) 
    {
      unsigned char *pBinaries = bVect_.data();
      unsigned int i=0, j=0;
      
      void *pNewData = reallocate (numLeft);

      if (pNewData != _pElements)    // if had to reallocate data
	{
	  for (; j<numLeft; i++)
	    if (! pBinaries[i])	  // if the element remains in the vector
	      _pOperations->set (pNewData, j++, internalGet (i), MSRaw);

	  _pOperations->deallocate (_pElements, _len);
	  _pElements = pNewData;
	}
      else	// if removing in-place
	{
	  for (; j<numLeft; i++)
	    {
	      if (! pBinaries[i])
		{
		  if (i != j)
		    _pOperations->set (_pElements, j, internalGet (i));

		  j++;
		}
	    }

	  _pOperations->destroy (_pElements, numLeft, numDoomed);
	}

      _len = numLeft;
      
      return MSError::MSSuccess;
    }
  
  return MSError::MSFailure;
}  


MSError::ErrorStatus MSVectorImpl::removeAll()
{
  if (_len > 0)
    {
      _pOperations->deallocate (_pElements, _len);
      _pElements = _pOperations->allocate (0);
      _len = 0;

      return MSError::MSSuccess;
    }

  return MSError::MSFailure;
}


MSError::ErrorStatus MSVectorImpl::select (const MSIndexVector & iVect_)
{
  unsigned int newLen = iVect_.length();
  
  if (newLen == 0)	// selecting no elements
    removeAll();
  else
    {
      // we CANNOT do select() in-place:  we always need additional storage so that we don't overwrite any
      // elements before we actually move them to the proper location
      void *newData = _pOperations->allocate (newLen);
      unsigned int *pIndices = iVect_.data();

      for (unsigned int i=0; i<newLen; i++)
	_pOperations->set (newData, i, internalGet (pIndices[i]), MSRaw);

      _pOperations->deallocate (_pElements, _len);
      _pElements = newData;
      _len = newLen;
    }

  return MSError::MSSuccess;
}


MSError::ErrorStatus MSVectorImpl::compress (const MSBinaryVector & bVect_)
{
  unsigned int newLen = (unsigned int)bVect_.sum();
  unsigned int bVectLen = bVect_.length();

  if (_len != bVectLen || _len == newLen)  // if bVect_ is invalid or ineffective
    return MSError::MSFailure;
  
  if (newLen == 0)
    removeAll();
  else
    {
      // unlike with select(), we CAN compress() in-place since the binary vector is ordered; here, there is no issue
      // of overwriting elements which we have not yet moved
      //
      unsigned char *pBinaries = bVect_.data();
      unsigned int i=0, j=0;
      
      void *pNewData = reallocate (newLen);

      if (pNewData == _pElements)   // we are doing compression in-place
	{
	  for (; j<newLen; i++)
	    if (pBinaries[i])
	      {
		if (i != j)
		  _pOperations->set (_pElements, j, internalGet (i));

		j++;
	      }

	  _pOperations->destroy (_pElements, newLen, _len-newLen);
	}
      else
	{
	  for (; j<newLen; i++)
	    if (pBinaries[i])
	      _pOperations->set (pNewData, j++, internalGet (i), MSRaw);

	  _pOperations->deallocate (_pElements, _len);
	  _pElements = pNewData;
	}
     
      _len = newLen;
    }
  
  return MSError::MSSuccess;
}


MSBinaryVector MSVectorImpl::unique()
{
  MSBinaryVector::Data *d = MSBinaryVector::Data::allocateWithSize (_pOperations->size (_pElements));
  unsigned char *pBinaries = d->elements();
  
  switch (_len)	  // for efficiency, we'll deal with a couple of the simplest cases individually
    {
      case 0:
        break;

      case 1:
        pBinaries[0] = 1;
	break;

      case 2:
	pBinaries[0] = 1;

	if (_pOperations->isElementEqual (_pElements, 0, _pOperations->elementAt (_pElements, 1)))
	  pBinaries[1] = 0;
	else	// if the two elements are different
	  pBinaries[1] = 1;

	break;

      default:	// _len > 2
	MSIndexVector iVect = gradeUp();
	unsigned int *pIndices = iVect.data();

	for (unsigned int i=0, j=1; i<_len; i=j++)
	  {
	    pBinaries[pIndices[i]] = 1;

	    void *pUnique = _pOperations->elementAt (_pElements, pIndices[i]);
		
	    while (j<_len && _pOperations->isElementEqual (_pElements, pIndices[j], pUnique))
	      pBinaries[pIndices[j++]] = 0;
	  }

	break;
    }

  return MSBinaryVector (d, _len);
}


MSError::ErrorStatus MSVectorImpl::reshape (unsigned int newLen_)
{
  if (newLen_ == _len)
    return MSError::MSFailure;   // do nothing

  if (newLen_ == 0)
    removeAll();
  else
    {
      void *pNewData = reallocate (newLen_);

      if (pNewData == _pElements)  // if reshaping in-place
	{
	  if (newLen_ < _len)
	    _pOperations->destroy (_pElements, newLen_, _len-newLen_);
	  else
	    {
	      if (_len)
		{
		  unsigned int numCycles=newLen_/_len, lastCycleLen=newLen_%_len;
		  
		  for (unsigned int i=1; i<numCycles; i++)
		    _pOperations->copy (_pElements, _pElements, _len, 0, i*_len, MSRaw);

		  _pOperations->copy (_pElements, _pElements, lastCycleLen, 0, numCycles*_len, MSRaw);
		}
	      else	// _len == 0
		_pOperations->fill (_pElements, 0, newLen_, 0, MSRaw);
	    }
	}
      else	 // if reallocation was done
	{
	  if (newLen_ < _len)
	    _pOperations->copy (_pElements, pNewData, newLen_, 0, 0, MSRaw);
	  else  // if newLen_>_len (we've tested for equality in the beginning)
	    {
	      if (_len)	  // if copying is needed
		{
		  unsigned int numCycles = newLen_ / _len;
		  unsigned int lastCycleLen = newLen_ % _len;
		      
		  for (unsigned int i=0; i<numCycles; i++)
		    _pOperations->copy (_pElements, pNewData, _len, 0, i*_len, MSRaw);
		  
		  _pOperations->copy (_pElements, pNewData, lastCycleLen, 0, numCycles*_len, MSRaw);
		}
	      else  // _len == 0
		_pOperations->fill (pNewData, 0, newLen_, 0, MSRaw);
	    }

	  _pOperations->deallocate (_pElements, _len);
	  _pElements = pNewData;
	}
	  
      _len = newLen_;
    }

  return MSError::MSSuccess;
}  


MSError::ErrorStatus MSVectorImpl::exchange (unsigned int index1_, unsigned int index2_)
{
  if (index1_ < _len && index2_ < _len && index1_ != index2_)
    {
      if (_pOperations->refCount (_pElements) > 1)
	{
	  void *pNewData = _pOperations->allocateWithSize (_pOperations->size (_pElements));
	  
	  if (index1_ < index2_)
	    {
	      _pOperations->copy (_pElements, pNewData, index1_, 0, 0, MSRaw);
	      _pOperations->copy (_pElements, pNewData, index2_-index1_-1, index1_+1, index1_+1, MSRaw);
	      _pOperations->copy (_pElements, pNewData, _len-index2_-1, index2_+1, index2_+1, MSRaw);
	    }
	  else  // index1_ > index2_
	    {
	      _pOperations->copy (_pElements, pNewData, index2_, 0, 0, MSRaw);
	      _pOperations->copy (_pElements, pNewData, index1_-index2_-1, index2_+1, index2_+1, MSRaw);
	      _pOperations->copy (_pElements, pNewData, _len-index1_-1, index1_+1, index1_+1, MSRaw);
	    }

	  _pOperations->set (pNewData, index1_, _pElements, index2_, MSRaw);
	  _pOperations->set (pNewData, index2_, _pElements, index1_, MSRaw);

	  _pOperations->deallocate (_pElements, _len);
	  _pElements = pNewData;
	}
      else	// if refCount == 1
	_pOperations->swapElements (_pElements, index1_, index2_);
	
      return MSError::MSSuccess;
    }

  return MSError::MSFailure;
}


MSError::ErrorStatus MSVectorImpl::reverse()
{
  if (_len > 1)
   {
     unsigned int i=0, j=_len-1;

     if (_pOperations->refCount (_pElements) > 1)
       {
	 void *pNewData = _pOperations->allocateWithSize (_pOperations->size (_pElements));
	 
	 for (; i<_len; i++, j--)
	   _pOperations->set (pNewData, i, _pElements, j, MSRaw);

	 _pOperations->deallocate (_pElements, _len);
	 _pElements = pNewData;
       }
     else  // if refCount == 1
       for (; i < j; i++, j--)
	 _pOperations->swapElements (_pElements, i, j);

     return MSError::MSSuccess;
   }

  return MSError::MSFailure;
}


MSError::ErrorStatus MSVectorImpl::rotate (int amount_)
{
  unsigned int absAmount = MSUtil::abs (amount_) % _len;
      
  if (absAmount)	// if amount_ is neither 0 nor a multiple of _len
    {
      if (_pOperations->refCount (_pElements) > 1)
	{
	  void *pNewData = _pOperations->allocateWithSize (_pOperations->size (_pElements));
	  unsigned int numRest = _len - absAmount;
	  
	  if (amount_ > 0)
	    {
	      _pOperations->copy (_pElements, pNewData, numRest, absAmount, 0, MSRaw);
	      _pOperations->copy (_pElements, pNewData, absAmount, 0, numRest, MSRaw);
	    }
	  else  // if amount_ < 0
	    {
	      _pOperations->copy (_pElements, pNewData, numRest, 0, absAmount, MSRaw);
	      _pOperations->copy (_pElements, pNewData, absAmount, numRest, 0, MSRaw);
	    }

	  _pOperations->deallocate (_pElements, _len);
	  _pElements = pNewData;
	}
      else
	{
	  // rotating n elements in one direction is equivalent to rotating _len-n elements in the opposite
	  // direction; therefore, we can optimize for additional temporary memory allocated below
	  // (savedElements) by doing a check here and flipping sign if necessary...
	  if (absAmount > _len / 2)
	    {
	      amount_ = - amount_; // we will need only the sign of the amount_ below, the value is in absAmount
	      absAmount = _len - absAmount;
	    }
	  
	  unsigned int blockWidth = _len - absAmount;
	  
	  void *savedElmts = _pOperations->allocateWithSize (absAmount);
	  
	  if (amount_ > 0)	// rotating counter-clockwise
	    {
	      _pOperations->copy (_pElements, savedElmts, absAmount, 0, 0, MSRaw);
	      blockLeft (absAmount, blockWidth, absAmount);
	      _pOperations->copy (savedElmts, _pElements, absAmount, 0, blockWidth);
	    }
	  else	// rotating clockwise
	    {
	      _pOperations->copy (_pElements, savedElmts, absAmount, blockWidth, 0, MSRaw);
	      blockRight (0, blockWidth, absAmount);
	      _pOperations->copy (savedElmts, _pElements, absAmount);
	    }
	  
	  _pOperations->deallocate (savedElmts, absAmount);
	}
	  
      return MSError::MSSuccess;
    }

  return MSError::MSFailure;
}


MSError::ErrorStatus MSVectorImpl::take (int numEls_, const void *pFiller_)
{
  unsigned int newLen = MSUtil::abs (numEls_);
  
  if (newLen == _len)
    return MSError::MSFailure;
  
  if (newLen)
    {
      unsigned int lenDiff;

      if (newLen > _len)
	{
	  lenDiff = newLen-_len;
	  
	  void *pNewData = reallocate (newLen);

	  if (pNewData == _pElements)   // if we are doing take() in-place
	    {
	      if (numEls_ < 0)	// if elements have to be at the end of the array
		{
		  blockRight (0, _len, lenDiff);
		  if (_len < lenDiff) // i.e., if newLen>2*_len, which means some elements are unconstructed
		    {
		      // simply fill in the constructed elements
		      _pOperations->fill (_pElements, 0, _len, pFiller_);
		      // initialize the unconstructed elements with the copy constructor
		      _pOperations->fill (_pElements, _len, lenDiff-_len, pFiller_, MSRaw);
		    }
		  else	// newLen<=2*_len, which means that all the elements are constructed by now
		    {
		      _pOperations->fill (_pElements, 0, lenDiff, pFiller_);
		    }
		}
	      else
		_pOperations->fill (_pElements, _len, lenDiff, pFiller_, MSRaw);
	    }
	  else	  // if reallocation has been done
	    {
	      void *pOldData = _pElements;
	      _pElements = pNewData;
	      
	      if (numEls_ < 0)   // copy the elements to the end of the new array
		{
		  _pOperations->copy (pOldData, _pElements, _len, 0, lenDiff, MSRaw);
		  _pOperations->fill (_pElements, 0, lenDiff, pFiller_, MSRaw);
		}
	      else    // if numEls_>0  (we've already tested for the equality)
		{
		  _pOperations->copy (pOldData, _pElements, _len, 0, 0, MSRaw);
		  _pOperations->fill (_pElements, _len, lenDiff, pFiller_, MSRaw);
		}
	  
	      _pOperations->deallocate (pOldData, _len);
	    }

	  _len = newLen;
	}
      else   // if (newLen < _len)
	{
	  lenDiff = _len - newLen;
	  
	  if (numEls_ < 0)
	    removeAt (0, lenDiff);   // leave only the last newLen elements
	  else
	    removeAt (newLen, lenDiff);   // leave only the first newLen elements
	}
    }
  else	// if (newLen == 0)
    removeAll();

  return MSError::MSSuccess;
}


MSError::ErrorStatus MSVectorImpl::drop (int numEls_)
{
  if (numEls_ == 0)
    return MSError::MSFailure;

  unsigned int numDoomed = MSUtil::abs (numEls_);

  if (numDoomed >= _len)
    removeAll();
  else	// if numDoomed < _len
    {
      if (numEls_ > 0)   
	removeAt (0, numDoomed);   // remove the first numDoomed elements
      else  // if numEls_ < 0
	removeAt (_len-numDoomed, numDoomed);   // remove the last numDoomed elements
    }

  return MSError::MSSuccess;
}


void MSVectorImpl::reverse (const MSVectorImpl & vImpl_)
{
  if (this == &vImpl_)
    {
      reverse();
      return;
    }
  
  _pOperations->deallocate (_pElements, _len);
  _pElements = _pOperations->allocateWithSize (vImpl_._pOperations->size (vImpl_._pElements));
  _len = vImpl_._len;
  unsigned int fwdInd,bkwdInd;
  for (fwdInd=0, bkwdInd=_len-1; fwdInd < bkwdInd; fwdInd++, bkwdInd--)
    {
      _pOperations->copy (vImpl_._pElements, _pElements, 1, bkwdInd, fwdInd, MSRaw);
      _pOperations->copy (vImpl_._pElements, _pElements, 1, fwdInd, bkwdInd, MSRaw);
    }

  if (fwdInd == bkwdInd)  // if _len is odd, the middle element doesn't get set in the loop
    _pOperations->set (_pElements, fwdInd, vImpl_._pElements, fwdInd, MSRaw);
}


void MSVectorImpl::rotate (const MSVectorImpl & vImpl_, int amount_)
{
  if (this == &vImpl_)
    {
      rotate (amount_);
      return;
    }
  
  _pOperations->deallocate (_pElements, _len);
  _pElements = _pOperations->allocateWithSize (vImpl_._pOperations->size (vImpl_._pElements));
  _len = vImpl_._len;

  unsigned int absAmount = MSUtil::abs (amount_) % _len;
  unsigned int blockWidth = _len - absAmount;
  
  if (amount_ > 0)	// rotating counter-clockwise
    {
      _pOperations->copy (vImpl_._pElements, _pElements, blockWidth, absAmount, 0, MSRaw);
      _pOperations->copy (vImpl_._pElements, _pElements, absAmount, 0, blockWidth, MSRaw);
    }
  else	// rotating clockwise
    {
      _pOperations->copy (vImpl_._pElements, _pElements, absAmount, blockWidth, 0, MSRaw);
      _pOperations->copy (vImpl_._pElements, _pElements, blockWidth, 0, absAmount, MSRaw);
    }
}


void MSVectorImpl::take (const MSVectorImpl & vImpl_, int numEls_, const void *pFiller_)
{
  if (this == &vImpl_)
    {
      take (numEls_);
      return;
    }

  _pOperations->deallocate (_pElements, _len);
  _len = MSUtil::abs (numEls_);
  _pElements = _pOperations->allocate (_len);

  if (_len)
    {
      if (_len > vImpl_._len)
	{
	  unsigned int lenDiff = _len - vImpl_._len;
	  
	  if (numEls_ > 0)  // copy the elements to the beginning of the vector
	    {
	      _pOperations->copy (vImpl_._pElements, _pElements, vImpl_._len, 0, 0, MSRaw);
	      _pOperations->fill (_pElements, vImpl_._len, lenDiff, pFiller_, MSRaw);
	    }
	  else  // copy the elements to the end of the vector
	    {
	      _pOperations->copy (vImpl_._pElements, _pElements, vImpl_._len, 0, _len - vImpl_._len, MSRaw);
	      _pOperations->fill (_pElements, 0, lenDiff, pFiller_, MSRaw);
	    }
	}
      else   // if (_len <= vImpl_._len)
	{
	  if (numEls_ > 0)   // copy only the first _len elements
	    _pOperations->copy (vImpl_._pElements, _pElements, _len, 0, 0, MSRaw);
	  else  // if numEls_ < 0, copy only the last _len elements
	    _pOperations->copy (vImpl_._pElements, _pElements, _len, vImpl_._len - _len, 0, MSRaw);
	}
    }
}


void MSVectorImpl::drop (const MSVectorImpl & vImpl_, int numEls_)
{
  if (this == &vImpl_)
    {
      drop (numEls_);
      return;
    }
  
  unsigned int numDoomed = MSUtil::abs (numEls_);

  _pOperations->deallocate (_pElements, _len);

  if (numDoomed < vImpl_._len)	 // if there will be any elements left in the vector
    {
      _len = vImpl_._len - numDoomed;
      _pElements = _pOperations->allocate (_len);
      
      if (numEls_ > 0)  // copy everything except the first numDoomed elements
	_pOperations->copy (vImpl_._pElements, _pElements, _len, numDoomed, 0, MSRaw);  
      else  // if numEls_ <= 0, copy everything except the last numDoomed elements
	_pOperations->copy (vImpl_._pElements, _pElements, _len, 0, 0, MSRaw);
    }
  else
    {
      _len = 0;
      _pElements = _pOperations->allocate (0);
    }
}


void MSVectorImpl::select (const MSVectorImpl & vImpl_, const MSIndexVector & iVect_)
{
  if (this == &vImpl_)
    {
      select (iVect_);
      return;
    }

  _pOperations->deallocate (_pElements, _len);
  _len = iVect_.length();
  _pElements = _pOperations->allocate (_len);

  if (_len)   // if selecting at least one element
    {
      unsigned int *pIndices = iVect_.data();

      for (unsigned int i=0; i<_len; i++)
	_pOperations->set (_pElements, i, vImpl_.internalGet (pIndices[i]), MSRaw);
    }
}


void MSVectorImpl::compress (const MSVectorImpl & vImpl_, const MSBinaryVector & bVect_)
{
  if (this == &vImpl_)
    {
      compress (bVect_);
      return;
    }

  _pOperations->deallocate (_pElements, _len);
  _len = (unsigned int)bVect_.sum();
  _pElements = _pOperations->allocate (_len);

  if (_len)	// if there will be any elements left in the vector
    {
      unsigned char *pBinaries = bVect_.data();

      for (unsigned int i=0, newDataInd=0; newDataInd<_len; i++)
	if (pBinaries[i])
	  _pOperations->set (_pElements, newDataInd++, vImpl_.internalGet (i), MSRaw);
    }
}


MSIndexVector MSVectorImpl::gradeUp() const
{
  unsigned int size = _pOperations->size (_pElements);
  
  MSIndexVector::Data *d = MSIndexVector::Data::allocateWithSize (size);

  if (_len > 0)
    {
      unsigned int *pGradient = new unsigned int [_len];
      mergeSortUp(pGradient,d->elements());
      delete [] pGradient; 
    }

  return MSIndexVector (d, _len); 
}


MSIndexVector MSVectorImpl::gradeDown() const
{
  unsigned int size = _pOperations->size (_pElements);
  
  MSIndexVector::Data *d = MSIndexVector::Data::allocateWithSize (size);

  if (_len > 0)
    {
      unsigned int *pGradient = new unsigned int [_len];
      mergeSortDown(pGradient,d->elements());
      delete [] pGradient; 
    }

  return MSIndexVector(d, _len); 
}


unsigned int MSVectorImpl::maxLength() const
{
  unsigned int max = 0;
  
  for (unsigned int i=0; i<_len; i++)
    max = MSUtil::max (max, _pOperations->elementLen (_pElements, i));

  return max;
}


long MSVectorImpl::compare (const MSVectorImpl & vImpl_) const
{
  long result;
  
  unsigned n = MSUtil::min (_len, vImpl_._len);
  
  for (unsigned int i=0; i<n; i++)
   {
     result = _pOperations->compareElement (_pElements, i, _pOperations->elementAt (vImpl_._pElements, i));
     if (result != 0)
       return result;
   }
  
  return (_len == vImpl_._len) ? 0 : ((_len < vImpl_._len) ? -1 : 1);
}


MSBinaryVector MSVectorImpl::binaryCompare (const MSVectorImpl & vImpl_, MSComparison cmp_) const
{
  assert (_len == vImpl_._len);
  
  unsigned int size = _pOperations->size (_pElements);
  MSBinaryVector::Data *d = MSBinaryVector::Data::allocateWithSize (_pOperations->size (_pElements));
  unsigned char *pCmpData = d->elements();
  unsigned int i;
  
  switch (cmp_)
    {
      case MSLessThan:
        for (i=0; i<_len; i++)
	  pCmpData[i] = _pOperations->isElementLess (_pElements, i, _pOperations->elementAt (vImpl_._pElements, i));
	break;

      case MSLessThanOrEqualTo:
	for (i=0; i<_len; i++)
	  pCmpData[i] = _pOperations->isElementLessEqual (_pElements, i, _pOperations->elementAt (vImpl_._pElements, i));
	break;

      case MSEqualTo:
	for (i=0; i<_len; i++)
	  pCmpData[i] = _pOperations->isElementEqual (_pElements, i, _pOperations->elementAt (vImpl_._pElements, i));
	break;
      
      case MSNotEqualTo:
	for (i=0; i<_len; i++)
	  pCmpData[i] = ! _pOperations->isElementEqual (_pElements, i, _pOperations->elementAt (vImpl_._pElements, i));
	break;
      
      case MSGreaterThan:
	for (i=0; i<_len; i++)
	  pCmpData[i] = ! _pOperations->isElementLessEqual (_pElements, i, _pOperations->elementAt (vImpl_._pElements, i));
	break;

      case MSGreaterThanOrEqualTo:
	for (i=0; i<_len; i++)
	  pCmpData[i] = ! _pOperations->isElementLess (_pElements, i, _pOperations->elementAt (vImpl_._pElements, i));
	break;
    }

  return MSBinaryVector (d, _len);
}


MSBinaryVector MSVectorImpl::binaryCompare (const void *value_, MSComparison cmp_) const
{
  unsigned int size = _pOperations->size (_pElements);
  MSBinaryVector::Data *d = MSBinaryVector::Data::allocateWithSize (_pOperations->size (_pElements));
  unsigned char *pCmpData = d->elements();
  unsigned int i;
  
  switch (cmp_)
    {
      case MSLessThan:
        for (i=0; i<_len; i++)
	  pCmpData[i] = _pOperations->isElementLess (_pElements, i, value_);
	break;

      case MSLessThanOrEqualTo:
	for (i=0; i<_len; i++)
	  pCmpData[i] = _pOperations->isElementLessEqual (_pElements, i, value_);
	break;

      case MSEqualTo:
	for (i=0; i<_len; i++)
	  pCmpData[i] = _pOperations->isElementEqual (_pElements, i, value_);
	break;
      
      case MSNotEqualTo:
	for (i=0; i<_len; i++)
	  pCmpData[i] = ! _pOperations->isElementEqual (_pElements, i, value_);
	break;
      
      case MSGreaterThan:
	for (i=0; i<_len; i++)
	  pCmpData[i] = ! _pOperations->isElementLessEqual (_pElements, i, value_);
	break;

      case MSGreaterThanOrEqualTo:
	for (i=0; i<_len; i++)
	  pCmpData[i] = ! _pOperations->isElementLess (_pElements, i, value_);
	break;
    }

  return MSBinaryVector (d, _len);
}


MSBoolean MSVectorImpl::scalarCompare (const void *pValue_, MSComparison cmp_) const
{
  if (_len > 0)
    {
      unsigned int i;
      
      switch (cmp_)
	{
	  case MSLessThan:
	    for (i=0; i<_len; i++)
	      if (! _pOperations->isElementLess (_pElements, i, pValue_))
		return MSFalse;
	    break;

	  case MSLessThanOrEqualTo:
	    for (i=0; i<_len; i++)
	      if (! _pOperations->isElementLessEqual (_pElements, i, pValue_))
		return MSFalse;
	    break;

	  case MSEqualTo:
	    for (i=0; i<_len; i++)
	      if (! _pOperations->isElementEqual (_pElements, i, pValue_))
		return MSFalse;
	    break;
	  
	  case MSNotEqualTo:
	    for (i=0; i<_len; i++)
	      if (_pOperations->isElementEqual (_pElements, i, pValue_))
		return MSFalse;
	    break;
	  
	  case MSGreaterThan:
	    for (i=0; i<_len; i++)
	      if (_pOperations->isElementLessEqual (_pElements, i, pValue_))
		return MSFalse;
	    break;
	  
	  case MSGreaterThanOrEqualTo:
	    for (i=0; i<_len; i++)
	      if (_pOperations->isElementLess (_pElements, i, pValue_))
		return MSFalse;
	    break;
	  
	  default:
	    return MSFalse;
	}
      
      return MSTrue;
    }
  
  return (cmp_ == MSNotEqualTo) ? MSTrue : MSFalse;
}


void MSVectorImpl::mergeSortUp(unsigned int *pGradient_, unsigned int *pIndices_) const
{
  // This is a purely internal function.  It assumes that _len>0 and that pGradient_
  // and pIndices_ point to an allocated piece of memory of _len*sizeof(unsigned int) bytes.
  //
  pIndices_[0]=_pOperations->gradeUp(_pElements,_len,pGradient_);
  for (unsigned int i=0; i<_len-1; ++i)
    {
      pIndices_[i+1] = pGradient_[pIndices_[i]];
    }
}


void MSVectorImpl::mergeSortDown(unsigned int *pGradient_, unsigned int *pIndices_) const
{
  // This is a purely internal function.  It assumes that _len>0 and that pGradient_
  // and pIndices_ point to an allocated piece of memory of _len*sizeof(unsigned int) bytes.
  //
  pIndices_[0]=_pOperations->gradeDown(_pElements,_len,pGradient_);
  for (unsigned int i=0; i<_len-1; ++i)
    {
      pIndices_[i+1] = pGradient_[pIndices_[i]];
    }
}


MSVectorImplOps::MSVectorImplOps()
{
}


MSVectorImplOps::~MSVectorImplOps()
{
}
