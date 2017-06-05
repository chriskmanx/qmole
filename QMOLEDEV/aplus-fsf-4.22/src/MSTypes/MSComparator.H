#ifndef MSOVComparatorHEADER
#define MSOVComparatorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSEnum.H>
#include <MSTypes/MSStandardOps.H> 
#include <MSTypes/MSManagedPointer.H>
#include <itbseq.h>

//
// MSComparator, encapsulates a pointer to a class member function.  
//

//
// MSComparatorList, a sequence of pointers to MSComparator objects
//

template <class ArrayElementType>
class MSBaseComparator
{
public:
  MSBaseComparator(MSBoolean ascendingOrder_=MSTrue) :_ascendingOrder(ascendingOrder_)  {}
  virtual ~MSBaseComparator(void) {}
  MSBaseComparator(const MSBaseComparator<ArrayElementType> &comp_)
  {
    _ascendingOrder=comp_._ascendingOrder;
  }
  MSBaseComparator<ArrayElementType> &operator=(const MSBaseComparator<ArrayElementType> &comp_)
  {
    if (this!=&comp_) _ascendingOrder=comp_._ascendingOrder;
    return *this;
  }

  virtual long compare(const ArrayElementType&, const ArrayElementType&) { return 0;}
  void ascendingOrder(MSBoolean b_){_ascendingOrder=b_;}
  MSBoolean ascendingOrder(void) const { return _ascendingOrder; } 

  virtual MSBaseComparator<ArrayElementType>* clone(void) const { return new MSBaseComparator<ArrayElementType>(*this);}
  
protected:
  MSBoolean _ascendingOrder;
};

template <class ElementType, class ArrayElementType, class Type>
class MSComparator : public MSBaseComparator<ArrayElementType>
{
public:
  typedef const Type& (ElementType::*GetMethod)(void) const;

  MSComparator(GetMethod getMethod_, MSBoolean order_ = MSTrue)
      :MSBaseComparator<ArrayElementType>(order_),_getMethod(getMethod_)
  {}

  virtual long compare(const ArrayElementType& r1_, const ArrayElementType& r2_) 
  {
    const Type& aType = (elementForOps(r1_).*_getMethod)();
    const Type& bType = (elementForOps(r2_).*_getMethod)();

    if ( &aType != &bType) // check if it's not a reference to the same static variable
     {
       return _ascendingOrder ?  ::compare(aType,bType) : ::compare(bType,aType); 
     }
    else //if it is make local copies
     {
       const Type bValue =  bType;
       const Type aValue =  (elementForOps(r1_).*_getMethod)();
       return _ascendingOrder ?  ::compare(aValue,bValue) : ::compare(bValue,aValue);
     }
  }
  
  virtual MSBaseComparator<ArrayElementType>* clone(void) const
  {
    return new MSComparator<ElementType,ArrayElementType,Type>(*this);
  }

protected:
  GetMethod _getMethod;
};

template <class ElementType, class ArrayElementType, class Type>
class MSAscComparator : public MSComparator<ElementType,ArrayElementType,Type>
{
public:
  typedef MSComparator<ElementType,ArrayElementType,Type>::GetMethod GetMethod;

  MSAscComparator(GetMethod getMethod_) :
      MSComparator<ElementType,ArrayElementType,Type>(getMethod_) {}
  ~MSAscComparator() {}
};

template <class ElementType, class ArrayElementType, class Type>
class MSDescComparator : public MSComparator<ElementType,ArrayElementType,Type>
{
public:
  typedef MSComparator<ElementType,ArrayElementType,Type>::GetMethod GetMethod;

  MSDescComparator(GetMethod getMethod_) :
      MSComparator<ElementType,ArrayElementType,Type>(getMethod_,MSFalse) {}
  ~MSDescComparator() {}
};


//
// Array of MSComparators. Keeps a MSManagedPointer to a comparator.
// Use comparator clone function to make sure comparator created on the heap.
// Provides simple IBM Sequence interface
//

template < class ArrayElementType >
class MSComparatorList : private
  ITabularSequence<MSManagedPointer<MSBaseComparator<ArrayElementType> > >
{

  typedef MSBaseComparator<ArrayElementType>     MSBaseComparatorType;
  typedef MSManagedPointer<MSBaseComparatorType> MSComparatorListElem;
  typedef ITabularSequence<MSComparatorListElem> MSComparatorListBaseType;

public:

  MSComparatorList() {}
  ~MSComparatorList() {}
  
  // Add functions
  void add(const MSBaseComparatorType& comp_)
  { MSComparatorListBaseType::add(MSComparatorListElem(comp_.clone(), MSInit)); }

  void addAsLast(const MSBaseComparatorType& comp_)
  { MSComparatorListBaseType::addAsLast(MSComparatorListElem(comp_.clone(), MSInit)); }
  
  void addAtPosition(unsigned pos_,const MSBaseComparatorType& comp_)
  { MSComparatorListBaseType::addAtPosition(pos_,MSComparatorListElem(comp_.clone(), MSInit)); }

  // Access functions
  const MSBaseComparatorType& elementAtPosition(unsigned pos_) const
  { return *MSComparatorListBaseType::elementAtPosition(pos_); }

  MSBaseComparatorType& elementAtPosition(unsigned pos_) 
  { return *MSComparatorListBaseType::elementAtPosition(pos_); }

  const MSBaseComparatorType& firstElement() const
  { return *MSComparatorListBaseType::firstElement(); }

  const MSBaseComparatorType& lastElement() const
  { return *MSComparatorListBaseType::lastElement(); }
  
  unsigned long numberOfElements() const
  { return MSComparatorListBaseType::numberOfElements(); }
 
  // Remove functions
  void removeAtPosition(unsigned pos_)
  { MSComparatorListBaseType::removeAtPosition(pos_); }

  void removeAll()
  { MSComparatorListBaseType::removeAll(); }
  
};


#endif
