#ifndef MSTypeDataINLINES
#define MSTypeDataINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


template <class Type, class Allocator>
INLINELINKAGE MSTypeData<Type,Allocator> * MSTypeData<Type,Allocator>::incrementCount() 
{
#ifdef MS_MULTI_THREAD
  _mutex.acquire();
#endif  
  ++_refCount;
#ifdef MS_MULTI_THREAD
  _mutex.release();
#endif  
  return this;
}


template <class Type, class Allocator>
INLINELINKAGE void MSTypeData<Type,Allocator>::decrementCount (MSAllocationFlag flag_, unsigned int numToDestroy_) 
{
#ifdef MS_MULTI_THREAD
  _mutex.acquire();
  if (--_refCount==0)
   { 
     _mutex.release();
     deallocate(flag_, numToDestroy_);
   }
  else
   {
     _mutex.release();
   }
#else
  if (--_refCount==0) deallocate(flag_, numToDestroy_);
#endif
}


template <class Type, class Allocator>
INLINELINKAGE const Type *MSTypeData<Type,Allocator>::elements() const
{
  // use dataOffset() method to find the proper offset of the data elements
  //
  return (const Type *)(((char *)this) + dataOffset());
}


// same as elements(); used for backward compatibility only
template <class Type, class Allocator>
INLINELINKAGE const Type *MSTypeData<Type,Allocator>::data() const
{
  return elements();
}


template <class Type, class Allocator>
INLINELINKAGE Type *MSTypeData<Type,Allocator>::elements()
{
  // use dataOffset() method to find the proper offset of the data elements
  // 
  return (Type *)(((char *)this) + dataOffset());
}
  

// same as elements(); used for backward compatibility only
template <class Type, class Allocator>
INLINELINKAGE Type *MSTypeData<Type,Allocator>::data()
{
  return elements();
}
  

template <class Type, class Allocator>
INLINELINKAGE const Type & MSTypeData<Type,Allocator>::elementAt (unsigned index_) const
{
  return elements()[index_];
}


template <class Type, class Allocator>
INLINELINKAGE Type & MSTypeData<Type,Allocator>::elementAt (unsigned index_)
{
  return elements()[index_];
}


template <class Type, class Allocator>
INLINELINKAGE unsigned int MSTypeData<Type,Allocator>::dataOffset(void)
{
  // we have to use MSDataAlignment<Type> structure to find out the proper offset
  // of the data elements, taking possible alignment into consideration
  //
  return offsetof(MSDataAlignment<Type>,_pElements);
}


template <class Type>
INLINELINKAGE MSBoolean isConstructionNeeded(Type *) { return MSTrue; }

INLINELINKAGE MSBoolean isConstructionNeeded(void *)             { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(char *)             { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(unsigned char *)    { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(short *)            { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(unsigned short *)   { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(int *)              { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(unsigned int *)     { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(long *)             { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(unsigned long *)    { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(float *)            { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(double *)           { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(void **)            { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(char **)            { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(unsigned char **)   { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(short **)           { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(unsigned short **)  { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(int **)             { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(unsigned int **)    { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(long **)            { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(unsigned long **)   { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(float **)           { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(double **)          { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(void ***)           { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(char ***)           { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(unsigned char ***)  { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(short ***)          { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(unsigned short ***) { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(int ***)            { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(unsigned int ***)   { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(long ***)           { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(unsigned long ***)  { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(float ***)          { return MSFalse; }
INLINELINKAGE MSBoolean isConstructionNeeded(double ***)         { return MSFalse; }

template <class Type>
INLINELINKAGE MSBoolean isDestructionNeeded(Type *) { return MSTrue; }

INLINELINKAGE MSBoolean isDestructionNeeded(void *)             { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(char *)             { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(unsigned char *)    { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(short *)            { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(unsigned short *)   { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(int *)              { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(unsigned int *)     { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(long *)             { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(unsigned long *)    { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(float *)            { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(double *)           { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(void **)            { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(char **)            { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(unsigned char **)   { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(short **)           { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(unsigned short **)  { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(int **)             { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(unsigned int **)    { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(long **)            { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(unsigned long **)   { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(float **)           { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(double **)          { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(void ***)           { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(char ***)           { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(unsigned char ***)  { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(short ***)          { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(unsigned short ***) { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(int ***)            { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(unsigned int ***)   { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(long ***)           { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(unsigned long ***)  { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(float ***)          { return MSFalse; }
INLINELINKAGE MSBoolean isDestructionNeeded(double ***)         { return MSFalse; }

#endif  //MSTypeDataINLINES
