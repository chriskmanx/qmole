#ifndef MSVectorImplINLINES
#define MSVectorImplINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSIndexVector.H>
#include <MSTypes/MSBinaryVector.H>

INLINELINKAGE void MSVectorImpl::prepareToChange()
{
  if (_pOperations->refCount (_pElements) > 1)
    makeUniqueCopy();
}


INLINELINKAGE void MSVectorImpl::internalSet (unsigned int index_, const void *pValue_)
{
#if !defined (MS_NO_INDEX_ERROR)
  if (index_ >= _len)
    vectorIndexError (index_);
  else
#endif 
    _pOperations->set (_pElements, index_, pValue_);
}


INLINELINKAGE void MSVectorImpl::internalSet (unsigned int index_, const MSVectorImpl & srcImpl_, unsigned int srcIndex_)
{
#if !defined (MS_NO_INDEX_ERROR)
  if (index_ >= _len)
    vectorIndexError (index_);
  else
#endif  
    _pOperations->set (_pElements, index_, srcImpl_._pElements, srcIndex_);
}


INLINELINKAGE void *MSVectorImpl::internalGet (unsigned int index_) const
{
#if !defined (MS_NO_INDEX_ERROR)
  if (index_ >= _len)
    {
      vectorIndexError (index_);
      return _pOperations->badData();
    }
  else
#endif  
    return _pOperations->elementAt (_pElements, index_);
}


#if defined (MS_NO_INDEX_ERROR)
INLINELINKAGE void MSVectorImpl::vectorIndexError(unsigned int) const
{
}
#else
INLINELINKAGE void MSVectorImpl::vectorIndexError(unsigned int index_) const
{
  indexError(index_);
}
#endif //MS_NO_INDEX_ERROR


INLINELINKAGE unsigned int MSVectorImpl::length() const
{
  return _len;
}


INLINELINKAGE void * MSVectorImpl::data() const
{
  return _pElements;
}


INLINELINKAGE void MSVectorImpl::permute (const MSIndexVector & iVect_)
{
  select (iVect_);
}

#endif  // MSVectorImplINLINES
