#ifndef MSTypeDataIMPLEMENTATION
#define MSTypeDataIMPLEMENTATION

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <stddef.h>		// defines offsetof macro used in MSTypeData<Type,Allocator>::operator new
#include <MSTypes/MSString.H>
#include <MSTypes/MSTypeData.H>

#ifdef MS_NO_INLINES
#include <MSTypes/MSTypeDataInlines.C>
#endif // MS_NO_INLINES

template <class Type, class Allocator>
MSTypeData<Type,Allocator>::MSTypeData() : MSData()
{
}


template <class Type, class Allocator>
MSTypeData<Type,Allocator>::MSTypeData (unsigned int size_) : MSData (size_)
{
}


template <class Type, class Allocator>
MSTypeData<Type,Allocator>::~MSTypeData()
{
}


template <class Type, class Allocator>
MSString MSTypeData<Type,Allocator>::asDebugInfo() const
{
  MSString result ("MSTypeData<Type,Allocator>(@");
  result += MSString((unsigned long)this).d2x().lowerCase();
  result += ",_data=";
  result += MSString((unsigned long)elements()).d2x().lowerCase();
  result += ",_refCount=";
  result += MSString(refCount());
  result += ")";
  return result;
}


template <class Type, class Allocator>
void MSTypeData<Type,Allocator>::constructElements(Type *pElements_, unsigned int numToConstruct_, const Type & value_)
{
  Allocator alloc;

  while (numToConstruct_--)
    {
      alloc.construct(pElements_++, value_);
    }
}


template <class Type, class Allocator>
void MSTypeData<Type,Allocator>::destroyElements(Type *pElements_, unsigned int numToDestroy_)
{
  Allocator alloc;

  while (numToDestroy_--)
    {
      alloc.destroy(pElements_++);
    }
}


template <class Type, class Allocator>
void *MSTypeData<Type,Allocator>::operator new(size_t, unsigned int numEls_)
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
  Allocator alloc;
  return alloc.allocate(dataOffset() + numEls_*sizeof(Type));
}
  

template <class Type, class Allocator>
void MSTypeData<Type,Allocator>::operator delete(void *p_)
{
  Allocator alloc;
  alloc.deallocate((Type *)p_);
}


template <class Type, class Allocator>
void MSTypeData<Type,Allocator>::set (unsigned int index_, const Type & value_, MSAllocationFlag flag_)
{
  if (flag_ == MSConstructed)
    {
      elements()[index_] = value_;
    }
  else  // if (flag_==MSRaw)
    {
      Allocator alloc;
      alloc.construct(elements()+index_, value_);
    }
}


template <class Type, class Allocator>
void MSTypeData<Type,Allocator>::fill (Type *pElements_, unsigned int length_, const Type & value_, MSAllocationFlag flag_)
{
  if (flag_==MSConstructed)
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


template <class Type, class Allocator>
void MSTypeData<Type,Allocator>::copy (const Type *pSrc_, Type *pDest_, unsigned int length_, MSAllocationFlag flag_)
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
      Allocator alloc;

      while (length_--)
	{
	  alloc.construct(pDest_++, *pSrc_++);
	}
    }
}


template <class Type, class Allocator>
void MSTypeData<Type,Allocator>::copyBackward (const Type *pSrc_, Type *pDest_, unsigned int length_)
{
// ASSERTION:  elements in pSrc_ have been constructed

  while (length_--)
    *pDest_-- = *pSrc_--;
}


template <class Type, class Allocator> 
MSTypeData<Type,Allocator> *MSTypeData<Type,Allocator>::allocateWithLength (unsigned int length_, MSAllocationFlag flag_,
									    unsigned int numToConstruct_) 
{
  return MSTypeData<Type,Allocator>::allocateWithSize (MSData::computeSize(length_), flag_, numToConstruct_);
}


template <class Type, class Allocator> 
MSTypeData<Type,Allocator> *MSTypeData<Type,Allocator>::allocateWithSize (unsigned int size_, MSAllocationFlag flag_,
									  unsigned int numToConstruct_)
{
  MSTypeData<Type,Allocator> *pData = new (size_) MSTypeData<Type,Allocator> (size_);

  if (isConstructionNeeded((Type *)0)==MSTrue) // default construction is *not* needed for built-in types
    {
      if (flag_==MSConstructed)
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


template <class Type, class Allocator>
void MSTypeData<Type,Allocator>::deallocate (MSAllocationFlag flag_, unsigned int numToDestroy_)
{
  if (isDestructionNeeded((Type *)0)==MSTrue) // destruction is *not* needed for built-in types
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

#endif  // MSTypeDataIMPLEMENTATION
