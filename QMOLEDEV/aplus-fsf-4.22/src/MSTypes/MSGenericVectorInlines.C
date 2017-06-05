#ifndef MSGenericVectorINLINES
#define MSGenericVectorINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.

// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSStandardOps.H>    // define compareElements() method

#ifndef MS_NO_INLINES
#include <MSTypes/MSVectorImplInlines.C>
#endif  // MS_NO_INLINES

template <class Type>
INLINELINKAGE unsigned int MSGenericVector<Type>::length() const
{
  return _pImpl->length();
}


template <class Type>
INLINELINKAGE unsigned int MSGenericVector<Type>::size() const
{
  return ((MSGenericData<Type> *)_pImpl->data())->size();
}


template <class Type>
INLINELINKAGE Type * MSGenericVector<Type>::elements() const
{
  return ((MSGenericData<Type> *)_pImpl->data())->elements();
}


template <class Type>
INLINELINKAGE unsigned int MSGenericVector<Type>::indexOf (const Type & value_, unsigned int startPos_) const
{
  return _pImpl->indexOf ((void *)&value_, startPos_);
}


template <class Type>
INLINELINKAGE unsigned int MSGenericVector<Type>::lastIndexOf (const Type & value_, unsigned int startPos_) const
{
  return _pImpl->lastIndexOf ((void *)&value_, startPos_);
}
  

template <class Type>
INLINELINKAGE unsigned int MSGenericVector<Type>::occurrencesOf (const Type & value_, unsigned int startPos_) const
{
  return _pImpl->occurrencesOf ((const void *)&value_, startPos_);
}


template <class Type>
INLINELINKAGE MSIndexVector MSGenericVector<Type>::indicesOf (const MSGenericVector<Type> & v_) const
{
  return _pImpl->indicesOf (*v_._pImpl);
}


template <class Type>
INLINELINKAGE MSBinaryVector MSGenericVector<Type>::memberOf (const MSGenericVector<Type> &v_) const
{
  return _pImpl->memberOf (*v_._pImpl);
}


template <class Type>
INLINELINKAGE MSBinaryVector MSGenericVector<Type>::uniqueElements() const
{
  return _pImpl->unique();
}


template <class Type>
INLINELINKAGE MSGenericVector<Type> & MSGenericVector<Type>::append (const Type & value_)
{
  _pImpl->append ((void *) &value_);
  return *this;
}


template <class Type>
INLINELINKAGE MSGenericVector<Type> & MSGenericVector<Type>::append (const MSGenericVector<Type> & vect_)
{
  _pImpl->append (*vect_._pImpl);
  return *this;
}


template <class Type>
INLINELINKAGE MSGenericVector<Type> & MSGenericVector<Type>::removeAt (unsigned int index_)
{
  _pImpl->removeAt (index_);
  return *this;
}


template <class Type>
INLINELINKAGE MSGenericVector<Type> & MSGenericVector<Type>::removeAt (unsigned int startPos_, unsigned int numEls_)
{
  if (numEls_ > 0)
    _pImpl->removeAt (startPos_, numEls_);

  return *this;
}


template <class Type>
INLINELINKAGE MSGenericVector<Type> & MSGenericVector<Type>::remove (const MSIndexVector & iVect_)
{
  _pImpl->remove (iVect_);
  return *this;
}


template <class Type>
INLINELINKAGE MSGenericVector<Type> & MSGenericVector<Type>::remove (const MSBinaryVector & bVect_)
{
  _pImpl->remove (bVect_);
  return *this;
}


template <class Type>
INLINELINKAGE MSGenericVector<Type> & MSGenericVector<Type>::removeAll()
{
  _pImpl->removeAll();
  return *this;
}


template <class Type>
INLINELINKAGE MSGenericVector<Type> & MSGenericVector<Type>::select (const MSIndexVector & iVect_)
{
  _pImpl->select (iVect_);
  return *this;
}


template <class Type>
INLINELINKAGE MSGenericVector<Type> & MSGenericVector<Type>::compress  (const MSBinaryVector & bVect_)
{
  _pImpl->compress (bVect_);
  return *this;
}


template <class Type>
INLINELINKAGE MSGenericVector<Type> & MSGenericVector<Type>::reshape (unsigned int newLen_)
{
  _pImpl->reshape (newLen_);
  return *this;
}


template <class Type>
INLINELINKAGE MSGenericVector<Type> & MSGenericVector<Type>::exchange (unsigned int index1_, unsigned int index2_)
{
  _pImpl->exchange (index1_, index2_);
  return *this;
}


template <class Type>
INLINELINKAGE MSGenericVector<Type> & MSGenericVector<Type>::reverse()
{
  _pImpl->reverse();
  return *this;
}


template <class Type>
INLINELINKAGE MSGenericVector<Type> & MSGenericVector<Type>::rotate (int amount_)
{
  _pImpl->rotate (amount_);
  return *this;
}


template <class Type>
INLINELINKAGE MSGenericVector<Type> & MSGenericVector<Type>::take (int numEls_)
{
  _pImpl->take (numEls_);
  return *this;
}


template <class Type>
INLINELINKAGE MSGenericVector<Type> & MSGenericVector<Type>::take (int numEls_, const Type & filler_)
{
  _pImpl->take (numEls_, (void *)&filler_);
  return *this;
}


template <class Type>
INLINELINKAGE MSGenericVector<Type> & MSGenericVector<Type>::drop (int numEls_)
{
  _pImpl->drop (numEls_);
  return *this;
}


template <class Type>
INLINELINKAGE MSGenericVector<Type> & MSGenericVector<Type>::replaceAt (unsigned int index_, const Type & value_)
{
  set (index_, value_);
  return *this;
}


template <class Type>
INLINELINKAGE MSGenericVector<Type> & MSGenericVector<Type>::selectUnique()
{
  return compress (_pImpl->unique());
}
  

template <class Type>
INLINELINKAGE MSGenericVector<Type> MSGenericVector<Type>::selectUnique (const MSGenericVector<Type> & vect_)
{
  return compress (vect_, vect_._pImpl->unique());
}


template <class Type>
INLINELINKAGE MSGenericVector<Type>& MSGenericVector<Type>::selectiveAssign(const MSIndexVector& iVect_,
									    const Type& value_)
{
  _pImpl->setSelected(iVect_,(void *)&value_);
  return *this;
}


template <class Type>
INLINELINKAGE MSGenericVector<Type>& MSGenericVector<Type>::selectiveAssign(const MSIndexVector& iVect_,
									    const MSGenericVector<Type>& vect_)
{
  _pImpl->setSelected(iVect_,*vect_._pImpl);
  return *this;
}


template <class Type>
INLINELINKAGE MSGenericVector<Type>& MSGenericVector<Type>::selectiveAssign(const MSBinaryVector& bVect_,
									    const Type& value_)
{
  _pImpl->setSelected(bVect_,(void *)&value_);
  return *this;
}


template <class Type>
INLINELINKAGE MSGenericVector<Type>& MSGenericVector<Type>::selectiveAssign(const MSBinaryVector& bVect_,
									    const MSGenericVector<Type>& vect_)
{
  _pImpl->setSelected(bVect_,*vect_._pImpl);
  return *this;
}


template <class Type>
INLINELINKAGE MSIndexVector MSGenericVector<Type>::gradeUp() const
{
  return _pImpl->gradeUp();
}


template <class Type>
INLINELINKAGE MSIndexVector MSGenericVector<Type>::gradeDown() const
{
  return _pImpl->gradeDown();
}


template <class Type>
INLINELINKAGE void MSGenericVector<Type>::permute (const MSIndexVector & iVect_)
{
  _pImpl->permute (iVect_);
}


template <class Type>
INLINELINKAGE MSGenericVector<Type> & MSGenericVector<Type>::sortUp()
{
  permute (gradeUp());
  return *this;
}


template <class Type>
INLINELINKAGE MSGenericVector<Type> & MSGenericVector<Type>::sortDown()
{
  permute (gradeDown());
  return *this;
}


template <class Type>
INLINELINKAGE Type & MSGenericVector<Type>::firstElement()
{
  return elementAt (0);
}


template <class Type>
INLINELINKAGE Type & MSGenericVector<Type>::lastElement()
{
  return elementAt (_pImpl->length() -1);
}


template <class Type>
INLINELINKAGE const Type & MSGenericVector<Type>::firstElement() const
{
  return elementAt (0);
}


template <class Type>
INLINELINKAGE const Type & MSGenericVector<Type>::lastElement() const
{
  return elementAt (_pImpl->length() -1);
}


template <class Type>
INLINELINKAGE const Type & MSGenericVector<Type>::operator[] (unsigned int index_) const
{
  return elementAt (index_);
}


template <class Type>
INLINELINKAGE const Type & MSGenericVector<Type>::operator() (unsigned int index_) const
{
  return elementAt (index_);
}


template <class Type>
INLINELINKAGE const Type & MSGenericVector<Type>::elementAt (unsigned int index_) const
{
#if !defined(MS_NO_INDEX_ERROR)
  if (index_ >= _pImpl->length())
    {
      _pImpl->vectorIndexError (index_);
      return *(const Type *)ops().badData();
    }
  else
#endif  //MSPRODUCTION_BUILD
    return ((MSGenericData<Type> *)_pImpl->data())->elements()[index_];
}


template <class Type>
INLINELINKAGE Type & MSGenericVector<Type>::operator[] (unsigned int index_)
{
  return elementAt (index_);
}


template <class Type>
INLINELINKAGE MSGenericVector<Type> MSGenericVector<Type>::operator[](const MSIndexVector& iVect_) const
{
  return select(*this, iVect_);
}


template <class Type>
INLINELINKAGE MSGenericVector<Type> MSGenericVector<Type>::operator[](const MSBinaryVector& bVect_) const
{
  return compress(*this, bVect_);
}


template <class Type>
INLINELINKAGE long MSGenericVector<Type>::compare (const MSGenericVector<Type> & vect_) const
{
  return _pImpl->compare (*vect_._pImpl);
}


template <class Type>
INLINELINKAGE MSBinaryVector MSGenericVector<Type>::binaryCompare (const MSGenericVector<Type> & vect_, MSComparison comp_) const
{
  return _pImpl->binaryCompare (*vect_._pImpl, comp_);
}


template <class Type>
INLINELINKAGE MSBinaryVector MSGenericVector<Type>::binaryCompare (const Type & value_, MSComparison comp_) const
{
  return _pImpl->binaryCompare ((void *)&value_, comp_);
}


template <class Type>
INLINELINKAGE MSBoolean operator< (const MSGenericVector<Type> & vect1_, const MSGenericVector<Type> & vect2_)
{
  return MSBoolean (vect1_.compare(vect2_) < 0);
}


template <class Type>
INLINELINKAGE MSBoolean operator> (const MSGenericVector<Type> & vect1_, const MSGenericVector<Type> & vect2_)
{
  return MSBoolean (vect1_.compare(vect2_) > 0);
}


template <class Type>
INLINELINKAGE MSBoolean operator<= (const MSGenericVector<Type> & vect1_, const MSGenericVector<Type> & vect2_)
{
  return MSBoolean (vect1_.compare(vect2_) <= 0);
}


template <class Type>
INLINELINKAGE MSBoolean operator>= (const MSGenericVector<Type> & vect1_, const MSGenericVector<Type> & vect2_)
{
  return MSBoolean (vect1_.compare(vect2_) >= 0);
}


template <class Type>
INLINELINKAGE MSBoolean operator< (const MSGenericVector<Type> & vect_, const Type & value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSLessThan);
}


template <class Type>
INLINELINKAGE MSBoolean operator< (const Type & value_, const MSGenericVector<Type> & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSGreaterThan);
}


template <class Type>
INLINELINKAGE MSBoolean operator> (const MSGenericVector<Type> & vect_, const Type & value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSGreaterThan);
}


template <class Type>
INLINELINKAGE MSBoolean operator> (const Type & value_, const MSGenericVector<Type> & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSLessThan);
}


template <class Type>
INLINELINKAGE MSBoolean operator<= (const MSGenericVector<Type> & vect_, const Type & value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSLessThanOrEqualTo);
}


template <class Type>
INLINELINKAGE MSBoolean operator<= (const Type & value_, const MSGenericVector<Type> & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSGreaterThanOrEqualTo);
}


template <class Type>
INLINELINKAGE MSBoolean operator>= (const MSGenericVector<Type> & vect_, const Type & value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSGreaterThanOrEqualTo);
}


template <class Type>
INLINELINKAGE MSBoolean operator>= (const Type & value_, const MSGenericVector<Type> & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSLessThanOrEqualTo);
}


template <class Type>
INLINELINKAGE MSBoolean operator== (const MSGenericVector<Type> & vect_, const Type & value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSEqualTo);
}


template <class Type>
INLINELINKAGE MSBoolean operator== (const Type & value_, const MSGenericVector<Type> & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSEqualTo);
}


template <class Type>
INLINELINKAGE MSBoolean operator!= (const MSGenericVector<Type> & vect_, const Type & value_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSNotEqualTo);
}


template <class Type>
INLINELINKAGE MSBoolean operator!= (const Type & value_, const MSGenericVector<Type> & vect_)
{
  return vect_._pImpl->scalarCompare ((void *)&value_, MSNotEqualTo);
}


template <class Type>
INLINELINKAGE MSBoolean operator== (const MSGenericVector<Type> & vect1_, const MSGenericVector<Type> & vect2_)
{
  return MSBoolean (vect1_.compare(vect2_) == 0);
}

template <class Type>
INLINELINKAGE MSBoolean operator!= (const MSGenericVector<Type> & vect1_, const MSGenericVector<Type> & vect2_)
{
  return MSBoolean (vect1_.compare(vect2_) != 0);
}


template <class Type>
INLINELINKAGE MSGenericVector<Type> & operator<< (MSGenericVector<Type> & vect1_, const MSGenericVector<Type> & vect2_)
{
  return vect1_.append(vect2_);
}

template <class Type>
INLINELINKAGE MSGenericVector<Type> & operator<< (MSGenericVector<Type> & vect_, const Type& value_)
{
  return vect_.append(value_);
}

template<class Type>
INLINELINKAGE unsigned MSGenericVectorOps<Type>::indexCompareUp (Type *p_, unsigned i_, unsigned j_)
{
  long val = ::compareElements (p_[i_], p_[j_]);
  return (val != 0) ? val<0 : i_<j_;
}


template<class Type>
INLINELINKAGE unsigned MSGenericVectorOps<Type>::indexCompareDown (Type *p_, unsigned i_, unsigned j_)
{
  long val = ::compareElements (p_[i_], p_[j_]);
  return (val != 0) ? val>0 : i_<j_;
}


template <class Type>
INLINELINKAGE MSGenericData<Type> * MSGenericData<Type>::incrementCount() 
{
#ifdef MS_MULTI_THREAD
  _mutex.acquire();
#endif  
  _refCount++;
#ifdef MS_MULTI_THREAD
  _mutex.release();
#endif  
  return this;
}


template <class Type>
INLINELINKAGE void MSGenericData<Type>::decrementCount (MSAllocationFlag flag_, unsigned int numToDestroy_) 
{
#ifdef MS_MULTI_THREAD
  _mutex.acquire();
  if(--_refCount==0)
   {
     _mutex.release();
     deallocate(flag_, numToDestroy_);
   }
  else
   {
     _mutex.release();
   }
#else
  if (--_refCount == 0)
    {
      deallocate (flag_, numToDestroy_);
    }
#endif  
}


template <class Type>
INLINELINKAGE const Type *MSGenericData<Type>::elements() const
{
  // use dataOffset() method to find the proper offset of the data elements
  //
  return (const Type *)(((char *)this) + dataOffset());
}


// same as elements(); used for backward compatibility only
template <class Type>
INLINELINKAGE const Type *MSGenericData<Type>::data() const
{
  return elements();
}


template <class Type>
INLINELINKAGE Type * MSGenericData<Type>::elements()
{
  // use dataOffset() method to find the proper offset of the data elements
  // 
  return (Type *)(((char *)this) + dataOffset());
}
  

// same as elements(); used for backward compatibility only
template <class Type>
INLINELINKAGE Type * MSGenericData<Type>::data()
{
  return elements();
}
  

template <class Type>
INLINELINKAGE const Type & MSGenericData<Type>::elementAt (unsigned index_) const
{
  return elements()[index_];
}


template <class Type>
INLINELINKAGE Type & MSGenericData<Type>::elementAt (unsigned index_)
{
  return elements()[index_];
}


template <class Type>
INLINELINKAGE unsigned int MSGenericData<Type>::dataOffset(void)
{
  // we have to use MSDataAlignment<Type> structure to find out the proper offset
  // of the data elements, taking possible alignment into consideration
  //
  return offsetof(MSDataAlignment<Type>,_pElements);
}


template <class Type>
INLINELINKAGE MSBoolean isGenericConstructionNeeded(Type *) { return MSTrue; }

INLINELINKAGE MSBoolean isGenericConstructionNeeded(void *)             { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(char *)             { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(unsigned char *)    { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(short *)            { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(unsigned short *)   { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(int *)              { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(unsigned int *)     { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(long *)             { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(unsigned long *)    { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(float *)            { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(double *)           { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(void **)            { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(char **)            { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(unsigned char **)   { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(short **)           { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(unsigned short **)  { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(int **)             { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(unsigned int **)    { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(long **)            { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(unsigned long **)   { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(float **)           { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(double **)          { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(void ***)           { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(char ***)           { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(unsigned char ***)  { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(short ***)          { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(unsigned short ***) { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(int ***)            { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(unsigned int ***)   { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(long ***)           { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(unsigned long ***)  { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(float ***)          { return MSFalse; }
INLINELINKAGE MSBoolean isGenericConstructionNeeded(double ***)         { return MSFalse; }

template <class Type>
INLINELINKAGE MSBoolean isGenericDestructionNeeded(Type *) { return MSTrue; }

INLINELINKAGE MSBoolean isGenericDestructionNeeded(void *)             { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(char *)             { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(unsigned char *)    { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(short *)            { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(unsigned short *)   { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(int *)              { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(unsigned int *)     { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(long *)             { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(unsigned long *)    { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(float *)            { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(double *)           { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(void **)            { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(char **)            { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(unsigned char **)   { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(short **)           { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(unsigned short **)  { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(int **)             { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(unsigned int **)    { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(long **)            { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(unsigned long **)   { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(float **)           { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(double **)          { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(void ***)           { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(char ***)           { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(unsigned char ***)  { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(short ***)          { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(unsigned short ***) { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(int ***)            { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(unsigned int ***)   { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(long ***)           { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(unsigned long ***)  { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(float ***)          { return MSFalse; }
INLINELINKAGE MSBoolean isGenericDestructionNeeded(double ***)         { return MSFalse; }

#endif  //MSGenericVectorINLINES
