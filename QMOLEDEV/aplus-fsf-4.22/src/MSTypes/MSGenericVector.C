#ifndef MSGenericVectorIMPLEMENTATION
#define MSGenericVectorIMPLEMENTATION

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <limits.h>	  // define UINT_MAX for the merge sort routines
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <MSTypes/MSStandardOps.H>    // define compareElements() method

#include <MSTypes/MSGenericVector.H>
#include <MSTypes/MSAllocator.H>

#ifdef MS_NO_INLINES
#include <MSTypes/MSGenericVectorInlines.C>
#endif // MS_NO_INLINES

#ifdef MS_MULTI_THREAD
template <class Type> MSMutex MSGenericVector<Type>::_operationsMutex;
template <class Type> MSMutex MSGenericVectorOps<Type>::_nullDataMutex;
#endif

template <class Type>
MSGenericVector<Type>::MSGenericVector()
{
  _pImpl = new MSVectorImpl(&ops());
}


template <class Type>
MSGenericVector<Type>::MSGenericVector (const MSGenericVector<Type> & vect_)
  : _pImpl (new MSVectorImpl (*vect_._pImpl))
{
}


template <class Type>
MSGenericVector<Type>::MSGenericVector (unsigned int length_)
{
  _pImpl = new MSVectorImpl(&ops(), length_);
}


template <class Type>
MSGenericVector<Type>::MSGenericVector (unsigned int length_, const Type & filler_)
{
  _pImpl = new MSVectorImpl(&ops(), length_, (void *)&filler_);
}


template <class Type>
MSGenericVector<Type>::MSGenericVector (MSGenericData<Type> *pData_, unsigned int len_)
{
  _pImpl = new MSVectorImpl(&ops(), (void *)pData_, len_);
}


template <class Type>
MSGenericVector<Type>::MSGenericVector (const Type *pElements_, unsigned int len_)
{
  MSGenericData<Type> *pData = MSGenericData<Type>::allocateWithLength (len_, MSRaw);
  MSGenericData<Type>::copy (pElements_, pData->elements(), len_, MSRaw);

  _pImpl = new MSVectorImpl(&ops(), (void *)pData, len_);
}


template <class Type>
MSGenericVector<Type>::MSGenericVector (MSVectorImpl *pImpl_) : _pImpl (pImpl_)
{
}


template <class Type>
MSGenericVector<Type>::~MSGenericVector()
{
  delete _pImpl;
}


template <class Type>
MSGenericVector<Type> & MSGenericVector<Type>::operator= (const MSGenericVector<Type> & vect_)
{
  if (this != &vect_)
    *_pImpl = *vect_._pImpl;

  return *this;
}


template <class Type>
MSGenericVector<Type> & MSGenericVector<Type>::operator= (const Type & value_)
{
  _pImpl->setAll ((void *)&value_);
  return *this;
}


template <class Type>
MSString MSGenericVector<Type>::asDebugInfo() const
{
  MSString result ("MSGenericVector<Type>");
  
  return result << "(@" << MSString((unsigned long)this).d2x().lowerCase()
		<< ",_elements=" << ((MSGenericData<Type> *)_pImpl->data())->asDebugInfo()
		<< ",_size=" << MSString(size()) << ",_length=" << MSString(length());
}


template <class Type>
MSError::ErrorStatus MSGenericVector<Type>::set (unsigned int index_, const Type & value_)
{
  if (index_ < _pImpl->length())
    {
      _pImpl->set (index_, (void *)&value_);
      return MSError::MSSuccess;
    }
  else
    {
      _pImpl->vectorIndexError (index_);
      return MSError::MSFailure;
    }
}


template <class Type>
MSGenericVector<Type> & MSGenericVector<Type>::insertAt (unsigned int index_, const Type & value_)
{
  if (index_ == _pImpl->length())   // NEW FEATURE:  insert right after the last element
    return append (value_);

  // if inserting inside the vector
  _pImpl->insertAt (index_, (void *) &value_);
  return *this;
}


template <class Type>
MSGenericVector<Type> & MSGenericVector<Type>::insertAt (unsigned int index_, const MSGenericVector<Type> & vect_)
{
  if (index_ == _pImpl->length())  // NEW FEATURE:  insert right after the last element
    return append (vect_);
  
  // if inserting inside the vector
  _pImpl->insertAt (index_, *vect_._pImpl);
  return *this;
}


template <class Type>
MSGenericVector<Type> MSGenericVector<Type>::reverse (const MSGenericVector<Type> & vect_)
{
  MSVectorImpl *pImpl = (MSVectorImpl *)vect_._pImpl->create();
  pImpl->reverse (*vect_._pImpl);
  return MSGenericVector<Type> (pImpl);
}


template <class Type>
MSGenericVector<Type> MSGenericVector<Type>::rotate (const MSGenericVector<Type> & vect_, int amount_)
{
  MSVectorImpl *pImpl = (MSVectorImpl *)vect_._pImpl->create();
  pImpl->rotate (*vect_._pImpl, amount_);
  return MSGenericVector<Type> (pImpl);
}


template <class Type>
MSGenericVector<Type> MSGenericVector<Type>::take (const MSGenericVector<Type> & vect_, int numEls_)
{
  MSVectorImpl *pImpl = (MSVectorImpl *)vect_._pImpl->create();
  pImpl->take (*vect_._pImpl, numEls_);
  return MSGenericVector<Type> (pImpl);
}


template <class Type>
MSGenericVector<Type> MSGenericVector<Type>::take (const MSGenericVector<Type> & vect_, int numEls_, const Type & filler_)
{
  MSVectorImpl *pImpl = (MSVectorImpl *)vect_._pImpl->create();
  pImpl->take (*vect_._pImpl, numEls_, (void *)&filler_);
  return MSGenericVector<Type> (pImpl);
}


template <class Type>
MSGenericVector<Type> MSGenericVector<Type>::drop (const MSGenericVector<Type> & vect_, int numEls_)
{
  MSVectorImpl *pImpl = (MSVectorImpl *)vect_._pImpl->create();
  pImpl->drop (*vect_._pImpl, numEls_);
  return MSGenericVector<Type> (pImpl);
}


template <class Type>
MSGenericVector<Type> MSGenericVector<Type>::select (const MSGenericVector<Type> & vect_, const MSIndexVector & iVect_)
{
  MSVectorImpl *pImpl = (MSVectorImpl *)vect_._pImpl->create();
  pImpl->select (*vect_._pImpl, iVect_);
  return MSGenericVector<Type> (pImpl);
}


template <class Type>
MSGenericVector<Type> MSGenericVector<Type>::compress (const MSGenericVector<Type> & vect_, const MSBinaryVector & bVect_)
{
  MSVectorImpl *pImpl = (MSVectorImpl *)vect_._pImpl->create();
  pImpl->compress (*vect_._pImpl, bVect_);
  return MSGenericVector<Type> (pImpl);
}


template <class Type>
Type & MSGenericVector<Type>::elementAt  (unsigned int index_)
{
  // We call prepareToChange() here because we are, in part, releasing control of the vector's element.
  // By making sure that this vector contains a unique copy of the data, we avoid some unpleasant situations.
  // For example, consider the case when we don't provide any safeguards, but simply return the reference to the
  // element here:
  // 
  //   MSGenericVector<int> vect1(5,0), vect2(10,1);
  //   vect1 = vect2;
  //   vect1[0] = 2;
  //
  // After this piece of code is executed, both vect1[0] and vect2[0] have the value of 2, since they are actually
  // referring to the same location in memory.  The expected result would be that only vect1[0]==2, but vect2[0]==1.
  //
#if !defined(MS_NO_INDEX_ERROR)
  if (index_ >= _pImpl->length())
    {
      _pImpl->vectorIndexError (index_);
      return *(Type *)ops().badData();
    }
  else
#endif  //MS_NO_INDEX_ERROR
  if (((MSGenericData<Type> *)_pImpl->data())->refCount() > 1)
    _pImpl->makeUniqueCopy();

  return ((MSGenericData<Type> *)_pImpl->data())->elements()[index_];
}


template <class Type>
typename MSGenericVector<Type>::Operations& MSGenericVector<Type>::ops(void)
{
  MS_SAFE_STATIC_INIT(Operations,_operationsMutex);
}


template <class Type>
ostream& operator<<(ostream& stream_, const MSGenericVector<Type>& vect_)
{
  // We implement this friend function here rather than go through MSVectorImpl to avoid
  // imposing a restriction on the element class that it must implement stream operator<<().
  // See the comment in MSGenericVectorOps::print() method.
  //
  unsigned int len = vect_.length();
  Type *pData = vect_.elements();

  for (unsigned int i=0; i<len; ++i)
    {
      stream_ << pData[i] << endl;
    }

  stream_ << flush;
  return stream_;
}


template <class Type>
MSGenericVectorOps<Type>::MSGenericVectorOps()
{
}


template <class Type>
MSGenericVectorOps<Type>::~MSGenericVectorOps()
{
}


template <class Type>
void * MSGenericVectorOps<Type>::allocate (unsigned int length_, unsigned int numToConstruct_,
						    MSAllocationFlag flag_) const
{
  if (length_ > 0)
    {
      return MSGenericData<Type>::allocateWithLength (length_, flag_, numToConstruct_);
    }
  else  // length_ == 0
    {
      return nullData().incrementCount();
    }
}


template <class Type>
void * MSGenericVectorOps<Type>::allocateWithSize (unsigned int size_, unsigned int numToConstruct_,
							    MSAllocationFlag flag_) const
{
  if (size_ > 0)
    {
      return MSGenericData<Type>::allocateWithSize (size_, flag_, numToConstruct_);
    }
  else  // size_ == 0
    {
      return nullData().incrementCount();
    }
}  


template <class Type>
void MSGenericVectorOps<Type>::deallocate (void *data_, unsigned int numToDestroy_, MSAllocationFlag flag_) const
{
  ((MSGenericData<Type> *)data_)->decrementCount (flag_, numToDestroy_);
}


template <class Type>
void MSGenericVectorOps<Type>::incrementCount (void *data_) const
{
  ((MSGenericData<Type> *)data_)->incrementCount();
}


template <class Type>
unsigned int MSGenericVectorOps<Type>::refCount (const void *data_) const
{
  return ((MSGenericData<Type> *)data_)->refCount();
}


template <class Type>
void MSGenericVectorOps<Type>::set (void *data_, unsigned int index_, const void *pValue_,
					     MSAllocationFlag flag_) const
{
  ((MSGenericData<Type> *)data_)->set (index_, *(Type *)pValue_, flag_);
}


template <class Type>
void MSGenericVectorOps<Type>::set (void *pDest_, unsigned int destInd_,
					     const void *pSrc_, unsigned int srcInd_, MSAllocationFlag flag_) const
{
  ((MSGenericData<Type> *)pDest_)->set (destInd_, ((MSGenericData<Type> *)pSrc_)->elements()[srcInd_], flag_);
}


template <class Type>
void MSGenericVectorOps<Type>::fill (void *pElements_, unsigned int start_, unsigned int numToFill_,
					      const void *pValue_, MSAllocationFlag flag_) const
{
  Type *pStart = ((MSGenericData<Type> *)pElements_)->elements() + start_;

  if (pValue_)
    MSGenericData<Type>::fill (pStart, numToFill_, *(Type *)pValue_, flag_);
  else
    MSGenericData<Type>::fill (pStart, numToFill_, *(Type *)defaultFiller(), flag_);
}


template <class Type>
void MSGenericVectorOps<Type>::copy (const void *src_, void *dest_, unsigned int length_,
					      unsigned int srcInd_, unsigned int destInd_, MSAllocationFlag flag_) const
{
  MSGenericData<Type>::copy (((MSGenericData<Type> *)src_)->elements() + srcInd_, ((MSGenericData<Type> *)dest_)->elements() + destInd_, length_, flag_);
}


template <class Type>
void MSGenericVectorOps<Type>::copyBackward (void *pElements_, unsigned int src_, unsigned int dest_,
						      unsigned int numToCopy_) const
{
  Type *pElements = ((MSGenericData<Type> *)pElements_)->elements();
  MSGenericData<Type>::copyBackward (pElements+src_, pElements+dest_, numToCopy_);
}


template <class Type>
void MSGenericVectorOps<Type>::destroy (void *pElements_, unsigned int start_, unsigned int numEls_) const
{
// This is an obscure problem occurring only with DCC compiler on SGI.  Calling MSGenericData::destroyElements() to destroy
// numEls_ elements of the vector apparently corrupts the stack somehow.  The simplest example is creating
// a string vector of 10 elements and doing removeAt(0).  The program segv's soon after the removeAt() call.
// However, if I do not call destroyElements(), but put the same code HERE (destroying each element individually),
// everything works fine.  Everything also works fine with all the other compilers (lcc, lexa, xlC), and
// Purify displays no run-time errors (which would usually account for the stack corruption -- such as
// out-of-bounds array read/write).
//
#if !defined(MS_TEMPLATE_TYPE_DESTROY_BUG)
  MSGenericData<Type>::destroyElements(((MSGenericData<Type> *)pElements_)->elements()+start_, numEls_);
#else
  Type *pData = ((MSGenericData<Type> *)pElements_)->elements() + start_;
  MSAllocator<Type> alloc;

  while (numEls_--)
    {
      alloc.destroy(pData++);
    }
#endif	//__sgi
}


template <class Type>
MSError::ErrorStatus MSGenericVectorOps<Type>::setFromString (void *, unsigned int, const char *) const
{
  return MSError::MSFailure;
}


template <class Type>
int MSGenericVectorOps<Type>::isElementEqual (const void *pElements_, unsigned int index_,
						       const void *pValue_) const
{
  return (::compareElements(((MSGenericData<Type> *)pElements_)->elements()[index_],*(Type *)pValue_)==0);
}


template <class Type>
int MSGenericVectorOps<Type>::isElementLess (const void *pElements_, unsigned int index_,
						      const void *pValue_) const
{
  return (::compareElements(((MSGenericData<Type> *)pElements_)->elements()[index_],*(Type *)pValue_) < 0);
}


template <class Type>
int MSGenericVectorOps<Type>::isElementLessEqual (const void *pElements_, unsigned int index_,
							   const void *pValue_) const
{
  return (::compareElements(((MSGenericData<Type> *)pElements_)->elements()[index_],*(Type *)pValue_) <= 0);
}


template <class Type>
long MSGenericVectorOps<Type>::compareElement (const void *data_, unsigned int index_, const void *value_) const
{
  return ::compareElements (((MSGenericData<Type> *)data_)->elements()[index_], *(Type *)value_);
}


template <class Type>
void * MSGenericVectorOps<Type>::elementAt (const void *pElements_, unsigned int index_) const
{
  return & ((MSGenericData<Type> *)pElements_)->elementAt (index_);
}


template <class Type>
unsigned int MSGenericVectorOps<Type>::size (const void *pElements_) const
{
  return ((MSGenericData<Type> *)pElements_)->size();
}


template <class Type>
void MSGenericVectorOps<Type>::swapElements (void *data_, unsigned int ind1_, unsigned int ind2_) const
{
  Type *pElements = ((MSGenericData<Type> *)data_)->elements();
  
  Type temp (pElements[ind1_]);
  pElements[ind1_] = pElements[ind2_], pElements[ind2_] = temp;
}
  

template <class Type>
unsigned int MSGenericVectorOps<Type>::gradeUp (const void *pData_, unsigned int len_,
							 unsigned int *pResult_) const
{
  return mergeSortUp (len_, ((MSGenericData<Type> *)pData_)->elements(), pResult_, 0, len_);  
}


template <class Type>
unsigned int MSGenericVectorOps<Type>::gradeDown (const void *pData_, unsigned int len_,
							   unsigned int *pResult_) const
{
  return mergeSortDown (len_, ((MSGenericData<Type> *)pData_)->elements(), pResult_, 0, len_);  
}


template <class Type>
void *MSGenericVectorOps<Type>::badData() const
{
  static Type badData;
  return (void *)&badData;
}


template <class Type>
void *MSGenericVectorOps<Type>::defaultFiller() const
{
  static Type filler;
  return (void *)&filler;
}


template <class Type>
MSString MSGenericVectorOps<Type>::asString (const void *, unsigned int) const
{
  return MSString();
}


template <class Type>
MSString MSGenericVectorOps<Type>::asMSF (const void *, unsigned int) const
{
  return MSString();
}


template <class Type>
unsigned int MSGenericVectorOps<Type>::elementLen (const void *, unsigned int) const
{
  return 0;
}


template <class Type>
MSError::ErrorStatus MSGenericVectorOps<Type>::setFromMSF (void *, unsigned int, const char *) const
{
  return MSError::MSFailure;
}


template <class Type>
void MSGenericVectorOps<Type>::print(const void *, unsigned int, ostream&) const
{
  // This function is empty here because we don't go through MSVectorImpl to print MSGenericVector to
  // a stream.  We implement printing the vector (looping over the elements) in MSGenericVector itself.
  // This was done in order to avoid calling the friend ostream operator<<() with the element Type:
  // we don't want to impose a restriction that the element Type class of MSGenericVector must implement
  // ostream operator<<() friend function.  This way, if the application never uses the vector's stream
  // operator<<(), then it is not instantiated and the application is still be able use MSGenericVector
  // even though the element class may not support ostream operator<<().
}


template <class Type>
unsigned int MSGenericVectorOps<Type>::numElements (const MSString &, const char) const
{
  return 0;
}


template <class Type>
void MSGenericVectorOps<Type>::setFromMSString (void *, unsigned int, const MSString &, unsigned int&, const char) const
{
}


template<class Type>
unsigned MSGenericVectorOps<Type>::mergeSortUp (unsigned n_,Type *sp_,unsigned *p_,unsigned low_,unsigned high_)
{
  unsigned t,m=(low_+high_+1)>>1;
  if (high_==m) {p_[low_]=UINT_MAX;return low_;}
  high_=mergeSortUp(n_,sp_,p_,m,high_);
  low_=mergeSortUp(n_,sp_,p_,low_,m);
  if (indexCompareUp(sp_,high_,low_)) {m=low_;low_=high_;high_=m;}
  for (t=low_;;low_=p_[low_]=high_,high_=m)
   {
L:m=p_[low_];
    if (UINT_MAX==m) {p_[low_]=high_;return t;}
    if (indexCompareUp(sp_,m,high_)) {low_=m;goto L;}
   }
}


template<class Type>
unsigned MSGenericVectorOps<Type>::mergeSortDown (unsigned n_,Type *sp_,unsigned *p_,unsigned low_,unsigned high_)
{
  unsigned t,m=(low_+high_+1)>>1;
  if (high_==m) {p_[low_]=UINT_MAX;return low_;}
  high_=mergeSortDown(n_,sp_,p_,m,high_);
  low_=mergeSortDown(n_,sp_,p_,low_,m);
  if (indexCompareDown(sp_,high_,low_)) {m=low_;low_=high_;high_=m;}
  for (t=low_;;low_=p_[low_]=high_,high_=m)
   {
L:m=p_[low_];
    if (UINT_MAX==m) {p_[low_]=high_;return t;}
    if (indexCompareDown(sp_,m,high_)) {low_=m;goto L;}
   }
}


template <class Type>
typename MSGenericVectorOps<Type>::Data& MSGenericVectorOps<Type>::nullData(void)
{
  MS_SAFE_STATIC_INIT(Data,_nullDataMutex);
}


template <class Type>
MSGenericData<Type>::MSGenericData (unsigned int size_) : MSData (size_)
{
}


template <class Type>
MSGenericData<Type>::~MSGenericData()
{
}


template <class Type>
MSString MSGenericData<Type>::asDebugInfo() const
{
  MSString result ("MSGenericData<Type>(@");
  result += MSString((unsigned long)this).d2x().lowerCase();
  result += ",_data=";
  result += MSString((unsigned long)elements()).d2x().lowerCase();
  result += ",_refCount=";
  result += MSString(refCount());
  result += ")";
  return result;
}


template <class Type>
void MSGenericData<Type>::constructElements(Type *pElements_, unsigned int numToConstruct_, const Type & value_)
{
  MSAllocator<Type> alloc;

  while (numToConstruct_--)
    {
      alloc.construct(pElements_++, value_);
    }
}


template <class Type>
void MSGenericData<Type>::destroyElements(Type *pElements_, unsigned int numToDestroy_)
{
  MSAllocator<Type> alloc;

  while (numToDestroy_--)
    {
      alloc.destroy(pElements_++);
    }
}


template <class Type>
void * MSGenericData<Type>::operator new(size_t, unsigned int numEls_)
{
  // The data elements are going to be allocated right after the end of MSData structure (padded for
  // possible alignment).  We need to allocate memory for everything in MSData (including possible
  // padding) plus everything for the data elements (number of elements times size of each one).
  //
  // We cannot just use sizeof(MSData) in the computation of the total size because we need to know
  // how it would be aligned given that it's followed by an array of elements of type Type.  Instead,
  // we call the function dataOffset(), which will return where the data elements part starts, including
  // the alignment padding, which is equivalent to the real size of MSData.
  // 
  MSAllocator<Type> alloc;
  return alloc.allocate(dataOffset() + numEls_*sizeof(Type));
}
  

template <class Type>
void MSGenericData<Type>::operator delete(void *p_)
{
  MSAllocator<Type> alloc;
  alloc.deallocate((Type *)p_);
}


template <class Type>
void MSGenericData<Type>::set (unsigned int index_, const Type & value_, MSAllocationFlag flag_)
{
  if (flag_ == MSConstructed)
    {
      elements()[index_] = value_;
    }
  else  // if (flag_==MSRaw)
    {
      MSAllocator<Type> alloc;
      alloc.construct(elements()+index_, value_);
    }
}


template <class Type>
void MSGenericData<Type>::fill (Type *pElements_, unsigned int length_, const Type & value_, MSAllocationFlag flag_)
{
  if (flag_ == MSConstructed)
    {
      while (length_--)
	{
	  *pElements_++ = value_;
	}
    }
  else	 // if (flag_==MSRaw) --> elements haven't been constructed yet
    {
      constructElements(pElements_, length_, value_);
    }
}


template <class Type>
void MSGenericData<Type>::copy (const Type *pSrc_, Type *pDest_, unsigned int length_, MSAllocationFlag flag_)
{
  if (flag_ == MSConstructed)
    {
      while (length_--)
	{
	  *pDest_++ = *pSrc_++;
	}
    }
  else	// if (flag_==MSRaw) --> elements haven't been constructed yet
    {
      MSAllocator<Type> alloc;

      while (length_--)
	{
	  alloc.construct(pDest_++, *pSrc_++);
	}
    }
}


template <class Type>
void MSGenericData<Type>::copyBackward (const Type *pSrc_, Type *pDest_, unsigned int length_)
{
// ASSERTION:  elements in pSrc_ have been constructed

  while (length_--)
    *pDest_-- = *pSrc_--;
}


template <class Type> 
MSGenericData<Type> *MSGenericData<Type>::allocateWithLength (unsigned int length_, MSAllocationFlag flag_,
									      unsigned int numToConstruct_) 
{
  return MSGenericData<Type>::allocateWithSize (MSData::computeSize(length_), flag_, numToConstruct_);
}


template <class Type> 
MSGenericData<Type> *MSGenericData<Type>::allocateWithSize (unsigned int size_, MSAllocationFlag flag_,
									    unsigned int numToConstruct_)
{
  MSGenericData<Type> *pData = new (size_) MSGenericData<Type> (size_);

  if (isGenericConstructionNeeded((Type *)0)==MSTrue) // default construction is *not* needed for built-in types
    {
      if (flag_ == MSConstructed)
	{
	  constructElements(pData->elements(), size_, Type()); // default-construct all elements
	}
      else  // if (flag_==MSRaw)
	{
	  // default-construct first numToConstruct_ elements
	  //
	  constructElements(pData->elements(), numToConstruct_, Type()); 
	}
    }

  return pData;
}


template <class Type>
void MSGenericData<Type>::deallocate (MSAllocationFlag flag_, unsigned int numToDestroy_)
{
  if (isGenericDestructionNeeded((Type *)0)==MSTrue) // destruction is *not* needed for built-in types
    {
      if (flag_ == MSConstructed)
	{
	  destroyElements(elements(), size());
	}
      else	 // if (flag_==MSRaw)
	{
	  destroyElements(elements(), numToDestroy_);
	}
    }

  delete this;
}

#endif  // MSGenericVectorIMPLEMENTATION
