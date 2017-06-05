#ifndef MSBaseTypeVectorIMPLEMENTATION
#define MSBaseTypeVectorIMPLEMENTATION

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
#include <MSTypes/MSString.H>
#include <MSTypes/MSMergeSort.H>
#include <MSTypes/MSStandardOps.H>
#include <MSTypes/MSGlobalInlines.H>

#include <MSTypes/MSMergeSort.C>

#include <MSTypes/MSBaseTypeVector.H>

#ifdef MS_NO_INLINES
#include <MSTypes/MSBaseTypeVectorInlines.C>
#endif // MS_NO_INLINES

#ifdef MS_MULTI_THREAD
template <class Type, class Allocator> MSMutex MSBaseVector<Type,Allocator>::_operationsMutex;
template <class Type, class Allocator> MSMutex MSBaseVectorOps<Type,Allocator>::_nullDataMutex;
#endif

template <class Type, class Allocator>
MSBaseVector<Type,Allocator>::MSBaseVector() : MSVector(), _blocked(MSFalse)
{
  _pImpl = new MSVectorImpl(&ops());
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator>::MSBaseVector (const MSBaseVector<Type,Allocator> & vect_)
  : MSVector(), _pImpl (new MSVectorImpl (*vect_._pImpl)), _blocked(MSFalse)
{
}
 

#if defined(MS_TEMPLATE_CONSTRUCTOR_OVERLOAD_BUG)
template <class Type, class Allocator>
MSBaseVector<Type,Allocator>::MSBaseVector (unsigned int length_, int, int) : MSVector(), _blocked(MSFalse)
{
  _pImpl = new MSVectorImpl(&ops(),length_);
}
#else
template <class Type, class Allocator>
MSBaseVector<Type,Allocator>::MSBaseVector (unsigned int length_) : MSVector(), _blocked(MSFalse)
{
  _pImpl = new MSVectorImpl(&ops(),length_);
}
#endif  //MS_TEMPLATE_CONSTRUCTOR_OVERLOAD_BUG


template <class Type, class Allocator>
MSBaseVector<Type,Allocator>::MSBaseVector (unsigned int length_, const Type & filler_) : MSVector(), _blocked(MSFalse)
{
  _pImpl = new MSVectorImpl(&ops(),length_,(void *)&filler_);
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator>::MSBaseVector (const char *pString_, const char delimiter_) : MSVector(), _blocked(MSFalse)
{
  _pImpl = new MSVectorImpl(&ops());
  _pImpl->setFromString(pString_,delimiter_);
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator>::MSBaseVector (MSTypeData<Type,Allocator> *pData_, unsigned int len_) : MSVector(), _blocked(MSFalse)
{
  _pImpl = new MSVectorImpl(&ops(),pData_,len_);
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator>::~MSBaseVector()
{
  delete _pImpl;
  _pImpl = 0;
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator>::MSBaseVector (const Type *pElements_, unsigned int len_) : MSVector(), _blocked(MSFalse)
{
  MSTypeData<Type,Allocator> *pData = MSTypeData<Type,Allocator>::allocateWithLength (len_, MSRaw);
  MSTypeData<Type,Allocator>::copy (pElements_, pData->elements(), len_, MSRaw);

  _pImpl = new MSVectorImpl(&ops(),pData,len_);
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::operator= (const MSBaseVector<Type,Allocator> & aVector_)
{
  if (this != &aVector_)
    {
      _blocked = MSTrue;
      *_pImpl = *(aVector_._pImpl);
      _blocked = MSFalse;
      changed();
    }

  return *this;
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::operator= (const Type & value_)
{
  _blocked = MSTrue;
  _pImpl->setAll ((void *)&value_);
  _blocked = MSFalse;
  changed();
  return *this;
}


template <class Type, class Allocator>
MSString MSBaseVector<Type,Allocator>::asString() const
{
  return ::msVectorAsString((Type*)0,_pImpl);
}


template <class Type, class Allocator>
MSString MSBaseVector<Type,Allocator>::asMSF() const
{
  return _pImpl->asMSF();
}


template <class Type, class Allocator>
MSString MSBaseVector<Type,Allocator>::asDebugInfo() const
{
  MSString result (className());
  
  return result << "(@" << MSString((unsigned long)this).d2x().lowerCase()
		<< ",_elements=" << vectorData()->asDebugInfo()
		<< ",_size=" << MSString(size()) << ",_length=" << MSString(_pImpl->length())
		<< ",_type=" << type().symbolName() << ")";
}


template <class Type, class Allocator>
MSString MSBaseVector<Type,Allocator>::className() const
{
  return MSString();
}


template <class Type, class Allocator>
const MSSymbol & MSBaseVector<Type,Allocator>::type() const
{
  static MSSymbol sym;
  return sym;
}


template <class Type, class Allocator>
MSModel * MSBaseVector<Type,Allocator>::clone() const
{
  return new MSBaseVector<Type,Allocator> (*this);
}


template <class Type, class Allocator>
MSModel * MSBaseVector<Type,Allocator>::create() const
{
  return new MSBaseVector<Type,Allocator>;
}


template <class Type, class Allocator>
void MSBaseVector<Type,Allocator>::assign (const MSModel & aModel_)
{
  *this = (MSBaseVector<Type,Allocator> &)aModel_;
}


template <class Type, class Allocator>
long MSBaseVector<Type,Allocator>::compare (const MSModel & aModel_) const
{
  return compare ((MSBaseVector<Type,Allocator> &)aModel_);
}


template <class Type, class Allocator>
MSError::ErrorStatus MSBaseVector<Type,Allocator>::set (const char *pString_)
{
  _blocked = MSTrue;
  MSError::ErrorStatus retval = _pImpl->setFromString (pString_);
  _blocked = MSFalse;
  changed();
  return retval;
}


template <class Type, class Allocator>
MSError::ErrorStatus MSBaseVector<Type,Allocator>::setFromMSF (const char *pString_)
{
  _blocked = MSTrue;
  MSError::ErrorStatus retval = _pImpl->setFromMSF (pString_);
  _blocked = MSFalse;
  changed();
  return retval;
}


template <class Type, class Allocator>
MSError::ErrorStatus MSBaseVector<Type,Allocator>::set (unsigned int index_, const Type & value_)
{
  if (index_ < _pImpl->length())
    {
      _blocked = MSTrue;
      _pImpl->set (index_, (void *)&value_);
      _blocked = MSFalse;
      changed (index_);

      return MSError::MSSuccess;
    }
  else
    {
      _pImpl->vectorIndexError (index_);
      return MSError::MSFailure;
    }
}


template <class Type, class Allocator>
MSError::ErrorStatus MSBaseVector<Type,Allocator>::set (unsigned int index_, const char *pString_)
{
  Type value;
  return (::msSetFromString (value, pString_) == MSError::MSSuccess) ? set (index_, value) : MSError::MSFailure;
}


template <class Type, class Allocator>
Type * MSBaseVector<Type,Allocator>::data() const
{
  return vectorData()->elements();
}


template <class Type, class Allocator>
Type * MSBaseVector<Type,Allocator>::elements() const
{
  return vectorData()->elements();
}


// the virtual version of length()
template <class Type, class Allocator>
unsigned int MSBaseVector<Type,Allocator>::getLength() const
{
  return _pImpl->length();
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::removeAll()
{
  _blocked = MSTrue;
  if (_pImpl->removeAll() == MSError::MSSuccess)
    changed();
  _blocked = MSFalse;

  return *this;
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::append (const Type & newElement)
{
  _blocked = MSTrue;
  _pImpl->append ((void *)& newElement);
  _blocked = MSFalse;
  appendUpdate (_pImpl->length(), 1);

  return *this;
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::append (const MSBaseVector<Type,Allocator> & vect_)
{
  if (_pImpl->append (*vect_._pImpl) == MSError::MSSuccess)
    appendUpdate (_pImpl->length(), vect_._pImpl->length());

  return *this;
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::insertAt  (unsigned int index_, const Type & value_)
{
  _blocked = MSTrue;

  if (index_ == _pImpl->length())   // NEW FEATURE:  insert right after the last element
    return append (value_);

  // if inserting inside the vector
  if (_pImpl->insertAt (index_, (void *)& value_) == MSError::MSSuccess)
    changed();

  _blocked = MSFalse;
  return *this;
}



template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::insertAt (unsigned int index_, const MSBaseVector<Type,Allocator> & vect_)
{
  _blocked = MSTrue;

  if (index_ == _pImpl->length())  // NEW FEATURE:  insert right after the last element
    return append (vect_);
  
  // if inserting inside the vector
  if (_pImpl->insertAt (index_, *vect_._pImpl) == MSError::MSSuccess)
    changed();

  _blocked = MSFalse;
  return *this;
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::removeAt (unsigned int index_)
{
  _blocked = MSTrue;
  if (_pImpl->removeAt (index_) == MSError::MSSuccess)
    changed();
  _blocked = MSFalse;

  return *this;
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::removeAt (unsigned int startPos_, unsigned int numEls_)
{
  _blocked = MSTrue;
  if (numEls_ > 0)
    if (_pImpl->removeAt (startPos_, numEls_) == MSError::MSSuccess)
      changed();
  _blocked = MSFalse;

  return *this;
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::remove (const MSIndexVector & iVect_)
{
  _blocked = MSTrue;
  if (_pImpl->remove (iVect_) == MSError::MSSuccess)
    changed();
  _blocked = MSFalse;
  
  return *this;
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::remove (const MSBinaryVector & bVect_)
{
  _blocked = MSTrue;
  if (_pImpl->remove (bVect_) == MSError::MSSuccess)
    changed();
  _blocked = MSFalse;
  
  return *this;
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::select (const MSIndexVector & iVect_)
{
  _blocked = MSTrue;
  if (_pImpl->select (iVect_) == MSError::MSSuccess)
    changed();
  _blocked = MSFalse;

  return *this;
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::compress  (const MSBinaryVector & bVect_)
{
  _blocked = MSTrue;
  if (_pImpl->compress (bVect_) == MSError::MSSuccess)
    changed();
  _blocked = MSFalse;
  
  return *this;
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::reshape (unsigned int newLen_)
{
  _blocked = MSTrue;
  if (_pImpl->reshape (newLen_) == MSError::MSSuccess)
    changed();
  _blocked = MSFalse;
  
  return *this;
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::exchange (unsigned int index1_, unsigned int index2_)
{
  _blocked = MSTrue;
  if (_pImpl->exchange (index1_, index2_) == MSError::MSSuccess && doChanged() == MSTrue)
    {
      MSIndexVector iVect (2);
      iVect.data()[0] = index1_;
      iVect.data()[1] = index2_;
      changed (iVect);
    }
  _blocked = MSFalse;
  
  return *this;
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::reverse()
{
  _blocked = MSTrue;
  if (_pImpl->reverse() == MSError::MSSuccess)
    changed();
  _blocked = MSFalse;
  
  return *this;
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::rotate (int amount_)
{
  _blocked = MSTrue;
  if (_pImpl->rotate (amount_) == MSError::MSSuccess)
    changed();
  _blocked = MSFalse;
  
  return *this;
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::take (int numEls_)
{
  _blocked = MSTrue;
  if (_pImpl->take (numEls_) == MSError::MSSuccess)
    changed();
  _blocked = MSFalse;
  
  return *this;
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::take (int numEls_, const Type & filler_)
{
  _blocked = MSTrue;
  if (_pImpl->take (numEls_, (const void *)&filler_) == MSError::MSSuccess)
    changed();
  _blocked = MSFalse;
  
  return *this;
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::drop (int numEls_)
{
  _blocked = MSTrue;
  if (_pImpl->drop (numEls_) == MSError::MSSuccess)
    changed();
  _blocked = MSFalse;
  
  return *this;
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> MSBaseVector<Type,Allocator>::reverse (const MSBaseVector<Type,Allocator> & vect_)
{
  MSVectorImpl *impl = vect_._pImpl->create();
  impl->reverse (*vect_._pImpl);
  return MSBaseVector<Type,Allocator> (impl);
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> MSBaseVector<Type,Allocator>::rotate (const MSBaseVector<Type,Allocator> & vect_, int amount_)
{
  MSVectorImpl *impl = vect_._pImpl->create();
  impl->rotate (*vect_._pImpl, amount_);
  return MSBaseVector<Type,Allocator> (impl);
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> MSBaseVector<Type,Allocator>::take (const MSBaseVector<Type,Allocator> & vect_, int numEls_)
{
  MSVectorImpl *impl = vect_._pImpl->create();
  impl->take (*vect_._pImpl, numEls_);
  return MSBaseVector<Type,Allocator> (impl);
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> MSBaseVector<Type,Allocator>::take (const MSBaseVector<Type,Allocator> & vect_, int numEls_, const Type & filler_)
{
  MSVectorImpl *impl = vect_._pImpl->create();
  impl->take (*vect_._pImpl, numEls_, (void *)&filler_);
  return MSBaseVector<Type,Allocator> (impl);
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> MSBaseVector<Type,Allocator>::drop (const MSBaseVector<Type,Allocator> & vect_, int numEls_)
{
  MSVectorImpl *impl = vect_._pImpl->create();
  impl->drop (*vect_._pImpl, numEls_);
  return MSBaseVector<Type,Allocator> (impl);
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> MSBaseVector<Type,Allocator>::select (const MSBaseVector<Type,Allocator> & vect_, const MSIndexVector & iVect_)
{
  MSVectorImpl *impl = vect_._pImpl->create();
  impl->select (*vect_._pImpl, iVect_);
  return MSBaseVector<Type,Allocator> (impl);
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> MSBaseVector<Type,Allocator>::compress (const MSBaseVector<Type,Allocator> & vect_, const MSBinaryVector & bVect_)
{
  MSVectorImpl *impl = vect_._pImpl->create();
  impl->compress (*vect_._pImpl, bVect_);
  return MSBaseVector<Type,Allocator> (impl);
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::selectiveAssign (const MSIndexVector & iVect_, const Type & value_)
{
  _blocked = MSTrue;
  _pImpl->setSelected (iVect_, (void *)&value_);
  _blocked = MSFalse;
  changed (iVect_);
  return *this;
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::selectiveAssign (const MSIndexVector & iVect_, const MSBaseVector<Type,Allocator> & vect_)
{
  _blocked = MSTrue;
  _pImpl->setSelected (iVect_, *vect_._pImpl);
  _blocked = MSFalse;
  changed (iVect_);
  return *this;
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::selectiveAssign (const MSBinaryVector & bVect_, const Type & value_)
{
  _blocked = MSTrue;
  if (doChanged() == MSTrue)
    changed (_pImpl->setIndexSelected (bVect_, (void *)&value_));
  else
    _pImpl->setSelected (bVect_, (void *)&value_);
  _blocked = MSFalse;
    
  return *this;
}


template <class Type, class Allocator>
MSBaseVector<Type,Allocator> & MSBaseVector<Type,Allocator>::selectiveAssign (const MSBinaryVector & bVect_, const MSBaseVector<Type,Allocator> & vect_)
{
  _blocked = MSTrue;
  if (doChanged() == MSTrue)
    changed (_pImpl->setIndexSelected (bVect_, *vect_._pImpl));
  else
    _pImpl->setSelected (bVect_, *vect_._pImpl);
  _blocked = MSFalse;
    
  return *this;
}


template <class Type, class Allocator>
MSIndexVector MSBaseVector<Type,Allocator>::gradeUp() const
{
  return _pImpl->gradeUp();
}


template <class Type, class Allocator>
MSIndexVector MSBaseVector<Type,Allocator>::gradeDown() const
{
  return _pImpl->gradeDown();
}


template <class Type, class Allocator>
void MSBaseVector<Type,Allocator>::permute (const MSIndexVector & iVect_)
{
  _blocked = MSTrue;
  _pImpl->permute (iVect_);
  _blocked = MSFalse;
  changed();
}


template <class Type, class Allocator>
typename MSBaseVector<Type,Allocator>::Operations& MSBaseVector<Type,Allocator>::ops(void)
{
  MS_SAFE_STATIC_INIT(Operations,_operationsMutex);
}


#if !defined(MS_TEMPLATE_MANUAL_FRIEND_BUG)
// The version of EDG used by SGI's dcc compiler has a bug related to access from friends during manual instantiation;
// therefore, for SGI, make operator<<() inline so that it does not need to be manually instantiated in the library.
template <class Type, class Allocator>
ostream & operator<< (ostream & stream_, const MSBaseVector<Type,Allocator> & vect_)
{
  vect_._pImpl->print (stream_);
  return stream_;
}
#endif  //MS_TEMPLATE_MANUAL_FRIEND_BUG


template <class Type, class Allocator>
MSBaseVectorOps<Type,Allocator>::MSBaseVectorOps()
{
}


template <class Type, class Allocator>
MSBaseVectorOps<Type,Allocator>::~MSBaseVectorOps()
{
}


template <class Type, class Allocator>
void * MSBaseVectorOps<Type,Allocator>::allocate (unsigned int length_, unsigned int numToConstruct_,
						 MSAllocationFlag flag_) const
{
  if (length_ > 0)
    return MSTypeData<Type,Allocator>::allocateWithLength (length_, flag_, numToConstruct_);
  else  // length_ == 0
    return nullData().incrementCount();
}


template <class Type, class Allocator>
void * MSBaseVectorOps<Type,Allocator>::allocateWithSize (unsigned int size_, unsigned int numToConstruct_,
							 MSAllocationFlag flag_) const
{
  if (size_ > 0)
    return MSTypeData<Type,Allocator>::allocateWithSize (size_, flag_, numToConstruct_);
  else  // size_ == 0
    return nullData().incrementCount();
}  


template <class Type, class Allocator>
void MSBaseVectorOps<Type,Allocator>::deallocate (void *data_, unsigned int numToDestroy_, MSAllocationFlag flag_) const
{
  ((MSTypeData<Type,Allocator> *)data_)->decrementCount (flag_, numToDestroy_);
}


template <class Type, class Allocator>
void MSBaseVectorOps<Type,Allocator>::incrementCount (void *data_) const
{
  ((MSTypeData<Type,Allocator> *)data_)->incrementCount();
}


template <class Type, class Allocator>
unsigned int MSBaseVectorOps<Type,Allocator>::refCount (const void *data_) const
{
  return ((MSTypeData<Type,Allocator> *)data_)->refCount();
}


template <class Type, class Allocator>
void MSBaseVectorOps<Type,Allocator>::set (void *data_, unsigned int index_, const void *pValue_,
					  MSAllocationFlag flag_) const
{
  ((MSTypeData<Type,Allocator> *)data_)->set (index_, *(Type *)pValue_, flag_);
}


template <class Type, class Allocator>
void MSBaseVectorOps<Type,Allocator>::set (void *pDest_, unsigned int destInd_,
					  const void *pSrc_, unsigned int srcInd_, MSAllocationFlag flag_) const
{
  ((MSTypeData<Type,Allocator> *)pDest_)->set (destInd_, ((MSTypeData<Type,Allocator> *)pSrc_)->elements()[srcInd_], flag_);
}


template <class Type, class Allocator>
void MSBaseVectorOps<Type,Allocator>::fill (void *pElements_, unsigned int start_, unsigned int numToFill_,
					   const void *pValue_, MSAllocationFlag flag_) const
{
  Type *pStart = ((MSTypeData<Type,Allocator> *)pElements_)->elements() + start_;

  if (pValue_)
    MSTypeData<Type,Allocator>::fill (pStart, numToFill_, *(Type *)pValue_, flag_);
  else
    MSTypeData<Type,Allocator>::fill (pStart, numToFill_, *(Type *)defaultFiller(), flag_);
}


template <class Type, class Allocator>
void MSBaseVectorOps<Type,Allocator>::copy (const void *src_, void *dest_, unsigned int length_,
					   unsigned int srcInd_, unsigned int destInd_, MSAllocationFlag flag_) const
{
  MSTypeData<Type,Allocator>::copy (((MSTypeData<Type,Allocator> *)src_)->elements() + srcInd_,
			  ((MSTypeData<Type,Allocator> *)dest_)->elements() + destInd_, length_, flag_);
}


template <class Type, class Allocator>
void MSBaseVectorOps<Type,Allocator>::copyBackward (void *pElements_, unsigned int src_, unsigned int dest_,
						   unsigned int numToCopy_) const
{
  Type *pElements = ((MSTypeData<Type,Allocator> *)pElements_)->elements();

  MSTypeData<Type,Allocator>::copyBackward (pElements+src_, pElements+dest_, numToCopy_);
}


template <class Type, class Allocator>
void MSBaseVectorOps<Type,Allocator>::destroy (void *pElements_, unsigned int start_, unsigned int numEls_) const
{
// This is an obscure problem occurring only with DCC compiler on SGI.  Calling MSTypeData::destroyElements() to destroy
// numEls_ elements of the vector apparently corrupts the stack somehow.  The simplest example is creating
// a string vector of 10 elements and doing removeAt(0).  The program segv's soon after the removeAt() call.
// However, if I do not call MSTypeData::destroyElements(), but put the same code HERE (destroying each element individually),
// everything works fine.  Everything also works fine with all the other compilers (lcc, lexa, xlC), and
// Purify displays no run-time errors (which would usually account for the stack corruption -- such as
// out-of-bounds array read/write).
//
#if !defined(MS_TEMPLATE_TYPE_DESTROY_BUG)
  MSTypeData<Type,Allocator>::destroyElements(((MSTypeData<Type,Allocator> *)pElements_)->elements()+start_, numEls_);
#else
  Type *pData = ((MSTypeData<Type,Allocator> *)pElements_)->elements() + start_;
  Allocator alloc;

  while (numEls_--)
    {
      alloc.destroy(pData++);
    }
#endif	//__sgi
}


template <class Type, class Allocator>
MSError::ErrorStatus MSBaseVectorOps<Type,Allocator>::setFromString (void *pData_, unsigned int index_,
									     const char *pString_) const
{
  return ::msSetFromString (((MSTypeData<Type,Allocator> *)pData_)->elements()[index_], pString_);
}


template <class Type, class Allocator>
int MSBaseVectorOps<Type,Allocator>::isElementEqual (const void *pElements_, unsigned int index_,
							     const void *pValue_) const
{
  return ((MSTypeData<Type,Allocator> *)pElements_)->elements()[index_] == *(Type *)pValue_;
}


template <class Type, class Allocator>
int MSBaseVectorOps<Type,Allocator>::isElementLess (const void *pElements_, unsigned int index_,
							    const void *pValue_) const
{
  return ((MSTypeData<Type,Allocator> *)pElements_)->elements()[index_] < *(Type *)pValue_;
}


template <class Type, class Allocator>
int MSBaseVectorOps<Type,Allocator>::isElementLessEqual (const void *pElements_, unsigned int index_,
								 const void *pValue_) const
{
  return ((MSTypeData<Type,Allocator> *)pElements_)->elements()[index_] <= *(Type *)pValue_;
}


template <class Type, class Allocator>
long MSBaseVectorOps<Type,Allocator>::compareElement (const void *data_, unsigned int index_,
							      const void *value_) const
{
  const Type & value1 = ((MSTypeData<Type,Allocator> *)data_)->elements()[index_], & value2 = *(Type *)value_;
  return ::compare(value1,value2);
}


template <class Type, class Allocator>
void *MSBaseVectorOps<Type,Allocator>::elementAt (const void *pElements_, unsigned int index_) const
{
  return & ((MSTypeData<Type,Allocator> *)pElements_)->elementAt (index_);
}


template <class Type, class Allocator>
unsigned int MSBaseVectorOps<Type,Allocator>::size (const void *pElements_) const
{
  return ((MSTypeData<Type,Allocator> *)pElements_)->size();
}


template <class Type, class Allocator>
void MSBaseVectorOps<Type,Allocator>::swapElements (void *data_, unsigned int ind1_, unsigned int ind2_) const
{
  Type *pElements = ((MSTypeData<Type,Allocator> *)data_)->elements();
  
  Type temp (pElements[ind1_]);
  pElements[ind1_] = pElements[ind2_], pElements[ind2_] = temp;
}
  

template <class Type, class Allocator>
unsigned int MSBaseVectorOps<Type,Allocator>::gradeUp (const void *pData_, unsigned int len_,
                                             unsigned int *pResult_) const
{
  return msMergeSortUp (len_,((MSTypeData<Type,Allocator> *)pData_)->elements(),pResult_,(unsigned int)0,len_);
}


template <class Type, class Allocator>
unsigned int MSBaseVectorOps<Type,Allocator>::gradeDown (const void *pData_, unsigned int len_,
                                               unsigned int *pResult_) const
{
  return msMergeSortDown (len_,((MSTypeData<Type,Allocator> *)pData_)->elements(),pResult_,(unsigned int)0,len_);
}


template <class Type, class Allocator>
void * MSBaseVectorOps<Type,Allocator>::badData() const
{
  static Type badValue = ::msBadData(Type());
  return (void *) &badValue;
}


template <class Type, class Allocator>
void * MSBaseVectorOps<Type,Allocator>::defaultFiller() const
{
  static Type filler;
  return (void *) &filler;
}


template <class Type, class Allocator>
MSString MSBaseVectorOps<Type,Allocator>::asString (const void *pData_, unsigned int index_) const
{
  return ::msAsString (((MSTypeData<Type,Allocator> *)pData_)->elements()[index_]);
}


template <class Type, class Allocator>
MSString MSBaseVectorOps<Type,Allocator>::asMSF (const void *pData_, unsigned int index_) const
{
  return ::msAsMSF (((MSTypeData<Type,Allocator> *)pData_)->elements()[index_]);
}


template <class Type, class Allocator>
unsigned int MSBaseVectorOps<Type,Allocator>::elementLen (const void *pData_, unsigned int index_) const
{
  return ::msVectorElementLength (((MSTypeData<Type,Allocator> *)pData_)->elements()[index_]);
}


template <class Type, class Allocator>
MSError::ErrorStatus MSBaseVectorOps<Type,Allocator>::setFromMSF (void *pData_, unsigned int index_,
									  const char *pBuf_) const
{
  return ::msSetFromMSF (((MSTypeData<Type,Allocator> *)pData_)->elements()[index_], pBuf_);
}


template <class Type, class Allocator>
void MSBaseVectorOps<Type,Allocator>::print (const void *pData_, unsigned int index_, ostream & ost_) const
{
  ost_ << ((MSTypeData<Type,Allocator> *)pData_)->elements()[index_] << endl;
}


template <class Type, class Allocator>
unsigned int MSBaseVectorOps<Type,Allocator>::numElements (const MSString & str_, const char delimiter_) const
{
  return ::msNumElements(((Type*)0),str_,delimiter_);
}


template <class Type, class Allocator>
void MSBaseVectorOps<Type,Allocator>::setFromMSString(void *pData_, unsigned int vectIndex_,
						      const MSString& str_, unsigned int& startPos_, const char delimiter_) const
{
   ::msVectorSetFromString (((MSTypeData<Type,Allocator> *)pData_)->elements()[vectIndex_],
                           str_,startPos_,delimiter_);
}


template <class Type, class Allocator>
typename MSBaseVectorOps<Type,Allocator>::Data& MSBaseVectorOps<Type,Allocator>::nullData(void)
{
  MS_SAFE_STATIC_INIT(Data,_nullDataMutex);
}

#endif  // MSBaseTypeVectorIMPLEMENTATION
