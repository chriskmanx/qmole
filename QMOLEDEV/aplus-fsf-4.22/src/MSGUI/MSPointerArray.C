#ifndef MSPointerArrayIMPLEMENTATION
#define MSPointerArrayIMPLEMENTATION

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


template<class Type>
MSPointerArray<Type>::MSPointerArray(void) 
{_size=0,_count=0,_array=0,_frozen=MSFalse;}
template<class Type>
MSPointerArray<Type>::~MSPointerArray(void) 
{
  if (_array!=0)
   {
     for (unsigned i=0;i<count();i++) _array[i]=0;
     delete [] _array;
   }
}

template<class Type>
void MSPointerArray<Type>::reserve(unsigned size_)
{
  unsigned n=size_+1;
  if (size()<n)
   {
     unsigned newSize=(size()==0)?n<<1:size()<<1;
     unsigned i;
     Type **array=new Type*[newSize];
     for (i=0;i<size();i++) 
      {
        array[i]=_array[i];
        _array[i]=0;
      }
     for (i=size();i<newSize;i++) array[i]=0;
     if (_array!=0) delete [] _array;
     _array=array;
     _size=newSize;
   }
}

template<class Type>
MSBoolean MSPointerArray<Type>::assign(Type* object_,unsigned location_)
{
  if (location_<count())
   {
     _array[location_]=object_;
     return MSTrue;
   }
  else return add(object_);
}

template<class Type>
MSBoolean MSPointerArray<Type>::find(Type *object_)
{
  unsigned n=count();
  for (unsigned i=0;i<n;i++) { if (_array[i]==object_) return MSTrue; }
  return MSFalse;
}

template<class Type>
MSBoolean MSPointerArray<Type>::add(Type *object_)
{
  if (find(object_)==MSFalse)
   {
     reserve(count());
     _array[_count++]=object_;
     return MSTrue;
   }
  return MSFalse;
}

template<class Type>
MSBoolean MSPointerArray<Type>::remove(Type *object_)
{
  if (frozen()==MSFalse)
   {
     unsigned n=count();
     for (unsigned i=0;i<n;i++)
      {
	if (_array[i]==object_) 
	 {
	   for (unsigned j=i;j<n-1;j++) _array[j]=_array[j+1];
	   _array[--_count]=0;
	   return MSTrue;
	 }
      }
   }
  return MSFalse;
}

template<class Type>
MSBoolean MSPointerArray<Type>::removeAll(void)
{
  if (frozen()==MSFalse)
   {
     while (count()>0) _array[--_count]=0;
     return MSTrue;
   }
  return MSFalse;
}

template<class Type>
MSBoolean MSPointerArray<Type>::insert(Type *object_,unsigned location_)
{
  if (location_<count())
   {
     remove(object_);
     reserve(count());
     for (unsigned i=count();i>location_;i--) _array[i]=_array[i-1];
     _array[location_]=object_;
     _count++;
     return MSTrue;
   }
  else if (location_==count())
   {
     return add(object_);
   }
  return MSFalse;
}

template<class Type>
MSBoolean MSPointerArray<Type>::exchange(unsigned x_,unsigned y_)
{
  if (x_<count()&&y_<count())
   {
     Type *t=_array[x_];
     _array[x_]=_array[y_];
     _array[y_]=t;
     return MSTrue;
   }
  return MSFalse;
}

#endif
