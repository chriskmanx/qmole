#ifndef MSAllocatorINLINES
#define MSAllocatorINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

template <class Type> INLINELINKAGE MSAllocator<Type>::MSAllocator(void) {}
template <class Type> INLINELINKAGE MSAllocator<Type>::~MSAllocator(void) {}

template <class Type>
INLINELINKAGE Type *MSAllocator<Type>::allocate(size_t n_, Type *)
{
  return (Type *)::operator new(n_*sizeof(Type));
}


template <class Type>
INLINELINKAGE Type *MSAllocator<Type>::allocate(size_t size_)
{
  return (Type *)::operator new(size_);
}


template <class Type>
INLINELINKAGE void MSAllocator<Type>::deallocate(Type *p_)
{
  ::operator delete(p_);
}


template <class Type>
INLINELINKAGE void MSAllocator<Type>::construct(Type *p_, const Type& value_)
{
  msConstruct(p_,value_);
}


template <class Type>
INLINELINKAGE void MSAllocator<Type>::destroy(Type *p_)
{
  msDestroy(p_);
}


template <class Type>
INLINELINKAGE void msConstruct(Type *p_, const Type& value_)
{
  new (p_) Type(value_);
}  


template <class Type>
INLINELINKAGE void msDestroy(Type *p_)
{
  p_->~Type();
}


INLINELINKAGE void msDestroy(void *)             {}
INLINELINKAGE void msDestroy(char *)             {}
INLINELINKAGE void msDestroy(unsigned char *)    {}
INLINELINKAGE void msDestroy(short *)            {}
INLINELINKAGE void msDestroy(unsigned short *)   {}
INLINELINKAGE void msDestroy(int *)              {}
INLINELINKAGE void msDestroy(unsigned int *)     {}
INLINELINKAGE void msDestroy(long *)             {}
INLINELINKAGE void msDestroy(unsigned long *)    {}
INLINELINKAGE void msDestroy(float *)            {}
INLINELINKAGE void msDestroy(double *)           {}
INLINELINKAGE void msDestroy(void **)            {}
INLINELINKAGE void msDestroy(char **)            {}
INLINELINKAGE void msDestroy(unsigned char **)   {}
INLINELINKAGE void msDestroy(short **)           {}
INLINELINKAGE void msDestroy(unsigned short **)  {}
INLINELINKAGE void msDestroy(int **)             {}
INLINELINKAGE void msDestroy(unsigned int **)    {}
INLINELINKAGE void msDestroy(long **)            {}
INLINELINKAGE void msDestroy(unsigned long **)   {}
INLINELINKAGE void msDestroy(float **)           {}
INLINELINKAGE void msDestroy(double **)          {}
INLINELINKAGE void msDestroy(void ***)           {}
INLINELINKAGE void msDestroy(char ***)           {}
INLINELINKAGE void msDestroy(unsigned char ***)  {}
INLINELINKAGE void msDestroy(short ***)          {}
INLINELINKAGE void msDestroy(unsigned short ***) {}
INLINELINKAGE void msDestroy(int ***)            {}
INLINELINKAGE void msDestroy(unsigned int ***)   {}
INLINELINKAGE void msDestroy(long ***)           {}
INLINELINKAGE void msDestroy(unsigned long ***)  {}
INLINELINKAGE void msDestroy(float ***)          {}
INLINELINKAGE void msDestroy(double ***)         {}


#endif //MSAllocatorINLINES
