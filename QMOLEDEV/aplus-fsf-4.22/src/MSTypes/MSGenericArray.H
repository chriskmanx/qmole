#ifndef MSGenericArrayHEADER
#define MSGenericArrayHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#ifndef MSEnumHEADER
#include <MSTypes/MSEnum.H>
#endif

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#include <MSTypes/MSIndexedModel.H>
#include <MSTypes/MSEventReceiver.H>
#include <MSTypes/MSSymbol.H>
#include <MSTypes/MSIndexedAspectEvent.H>
#include <itbseq.h>

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSGenericArray.C")
#endif

/*************************************************************************************/
//  MSGenericArray is a zero based array of event senders, currently implemented on
//  ITabularSequence. Elements are coupled with array on addition and decoupled on remove or
//  replace. MSGenericArray receives an event from it's elements and sends an indexed event
//  to the array event receiver. Both objects derived from MSEventSender and it's pointers
//  can be stored in the array.
/*************************************************************************************/


class MSIndexVector;
template<class Record> class MSBaseComparator;
template<class Record> class MSComparatorList;


template<class Record>
class MSGenericArray : public MSIndexedModel, public MSEventReceiver
{
public:

  typedef ITabularSequence<Record>            ImplClass;
  typedef ITabularSequence<Record>::Cursor    CursorClass;
  typedef long (*CompareFunction)(const Record&, const Record&);

  class Cursor : public ITabularSequence<Record>::Cursor
  {
  public:
    Cursor (const MSGenericArray<Record>& array_) : CursorClass(array_.sequence())
    {}
  };
  
  MSGenericArray(unsigned size_=100);
  MSGenericArray(const MSGenericArray<Record>& );
  MSGenericArray<Record> & operator=(MSGenericArray<Record> const &) ;
  virtual ~MSGenericArray(void) ;

  // MSModel support methods
  virtual void receiveEvent(MSEvent & aEvent_);
  virtual MSString asString(void) const;
  virtual MSString asDebugInfo(void) const;
  virtual MSString asMSF(void) const;
  virtual MSString className(void) const;
  virtual const MSSymbol& type(void) const;
  virtual MSError::ErrorStatus set(const char*);
  virtual MSError::ErrorStatus setFromMSF(const char*);
  virtual MSModel *clone(void) const;
  virtual MSBoolean addSender(MSEventSender*);
  virtual MSBoolean removeSender(MSEventSender*);
  virtual void addReceiverNotify(MSEventReceiver*);
  virtual void removeReceiverNotify(MSEventReceiver*);
  static const MSSymbol& symbol(void);

  // access functions
  inline const Record& elementAt(unsigned) const; 
  inline const Record& elementAt(const CursorClass&) const;
  inline const Record& elementAtPosition(unsigned) const; 
  inline const Record& firstElement() const;
  inline const Record& lastElement() const;
  inline Record& elementAt(unsigned); 
  inline Record& elementAt(const CursorClass&);

  inline unsigned numberOfElements(void) const;
  inline unsigned maxNumberOfElements(void) const;
  inline MSBoolean isEmpty(void) const;
  inline MSBoolean isLast(CursorClass const & cursor_) const;
  inline MSBoolean isFirst(CursorClass const & cursor_) const;

  // set functions
  inline MSBoolean setToFirst(CursorClass & cursor_) const;
  inline MSBoolean setToLast(CursorClass & cursor_) const;
  inline MSBoolean setToNext(CursorClass & cursor_) const;
  inline MSBoolean setToPrevious(CursorClass & cursor_) const;
  inline void setToPosition(unsigned int,CursorClass & cursor_) const;
  
  inline MSBoolean blocked(void) const;
  inline void blocked(MSBoolean);

  // add functions
  virtual MSBoolean add(Record const&);
  virtual MSBoolean add(Record const&,CursorClass&);
  virtual void addAsFirst(Record const&);
  virtual void addAsFirst(Record const&,CursorClass&);
  virtual void addAsLast(Record const&);
  virtual void addAsLast(Record const&,CursorClass&);
  virtual void addAsNext(Record const&,CursorClass &);
  virtual void addAsPrevious(Record const&,CursorClass &);
  virtual void addAllFrom(MSGenericArray<Record> const & array_);
  virtual void addAtPosition(unsigned position_,Record const& record_);  
  virtual void addAtPosition(unsigned position_,Record const& record_,CursorClass & cursor_);

  // remove functions
  unsigned removeAll(MSBoolean (*property_)(Record const &,void*),void *additionalArgument_=0);
  void removeAll(void);
  void removeAt(CursorClass & cursor_);
  void removeAtPosition(unsigned position_);
  void removeFirst(void);
  void removeLast(void);
  void replaceAt(const CursorClass & cursor_,Record const & record_);

  // grade & sort functions
  void sort(CompareFunction);
  void sort(const MSBaseComparator<Record>&);
  void sort(const MSComparatorList<Record>&);
  void rangeSort(const MSBaseComparator<Record>&,unsigned start_,unsigned end_);
  void rangeSort(const MSComparatorList<Record>&,unsigned start_,unsigned end_);
  MSIndexVector grade(const MSBaseComparator<Record>&) const;
  MSIndexVector grade(const MSComparatorList<Record>&) const;
  MSIndexVector rangeGrade(const MSBaseComparator<Record>&, unsigned,unsigned) const;
  MSIndexVector rangeGrade(const MSComparatorList<Record>&, unsigned,unsigned) const;
  void permute(const MSIndexVector&);

  const ImplClass&  sequence() const { return _seq; }

protected:

  virtual MSBoolean deferredDeleteNeeded(void) const; 
  void dereference(const CursorClass &); 
  inline void changed(const MSIndexVector& indices_=MSIndexedModel::nullIndexVector());
  inline unsigned int adjustPosition(unsigned int) const;
  // coupling and decoupling functions
  virtual void handleNotificationsFor(Record & aRecord_);
  virtual void stopHandlingNotificationsFor(Record & aRecord_);
  void handleNotificationsForAllElements(void);
  void stopHandlingNotificationsForAllElements(void);
  void handleNotificationsForBothEnds(void);
  void handleNotificationsForInsertion(Record&);
  void handleNotificationsForAddition(Record&);
  void stopHandlingNotificationsForBothEnds(void);

  void internalCheck(void);
  MSBoolean hasImplSizeChanged(void);
  void notifyReceivers(const MSIndexedAspectEvent & ev_);

  // members
  ImplClass     _seq;
  unsigned int  _addr;
  unsigned int  _allocSize;
  MSBoolean     _isElementPointer;
  MSBoolean     _blocked;

  
  static long     _pseudoCompare(const MSComparatorList<Record>&,const Record&,const Record&);
  static unsigned mergeSort(const MSComparatorList<Record>&,const MSGenericArray<Record>& a_,unsigned *p_,unsigned low_,unsigned high_);

};


// inline functions definition

template <class Record>
inline const Record & MSGenericArray<Record>::elementAt(unsigned index_) const
{
  return _seq.elementAtPosition(adjustPosition(index_));
}
template <class Record>
inline const Record & MSGenericArray<Record>::elementAtPosition(unsigned index_) const
{
  return _seq.elementAtPosition(adjustPosition(index_));
}
template <class Record>
const Record & MSGenericArray<Record>::elementAt(const CursorClass & cursor_) const
{
  return _seq.elementAt(cursor_);
}
template <class Record>
inline Record & MSGenericArray<Record>::elementAt(const CursorClass & cursor_)
{
  return _seq.elementAt(cursor_);
}
template <class Record>
inline Record & MSGenericArray<Record>::elementAt(unsigned index_) 
{
  CursorClass cursor(_seq);
  _seq.setToPosition(adjustPosition(index_),cursor);
  return _seq.elementAt(cursor);
}
template <class Record>
inline const Record & MSGenericArray<Record>::firstElement() const
{
  return _seq.firstElement();
}
template <class Record>
inline const Record & MSGenericArray<Record>::lastElement() const
{
  return _seq.lastElement();
}
template <class Record>
inline unsigned  MSGenericArray<Record>::numberOfElements(void) const
{
  return _seq.numberOfElements();
}
template <class Record>
inline unsigned  MSGenericArray<Record>::maxNumberOfElements(void) const
{
  return _seq.maxNumberOfElements();
}
template <class Record>
inline MSBoolean  MSGenericArray<Record>::isEmpty(void) const
{
  return (_seq.isEmpty()==True ? MSTrue : MSFalse);
}
template <class Record>
inline MSBoolean  MSGenericArray<Record>::isLast(CursorClass const & cursor_) const
{
  return (_seq.isLast(cursor_)==True ? MSTrue : MSFalse); 
}
template <class Record>
inline MSBoolean  MSGenericArray<Record>::isFirst(CursorClass const & cursor_) const
{
  return (_seq.isFirst(cursor_)==True ? MSTrue : MSFalse );
}
template <class Record>
inline MSBoolean MSGenericArray<Record>::setToFirst(CursorClass & cursor_) const
{
  return (_seq.setToFirst(cursor_)==True ? MSTrue : MSFalse );
} 
template <class Record>
inline MSBoolean MSGenericArray<Record>::setToNext(CursorClass & cursor_) const
{
  return (_seq.setToNext(cursor_)==True ? MSTrue : MSFalse );
}
template <class Record>
inline MSBoolean MSGenericArray<Record>::setToLast(CursorClass & cursor_) const
{
  return (_seq.setToLast(cursor_)==True ? MSTrue : MSFalse );
}
template <class Record>
inline MSBoolean MSGenericArray<Record>::setToPrevious(CursorClass & cursor_) const
{
  return (_seq.setToPrevious(cursor_)==True ? MSTrue : MSFalse );
}
template <class Record>
inline void MSGenericArray<Record>::setToPosition(unsigned int position_,CursorClass & cursor_) const
{
  _seq.setToPosition(adjustPosition(position_),cursor_);
}
template <class Record>
inline MSBoolean MSGenericArray<Record>::blocked(void) const
{
  return _blocked;
}
template <class Record>
inline void MSGenericArray<Record>::blocked(MSBoolean aBoolean_)
{
  _blocked=aBoolean_;
}
template <class Record>
inline void MSGenericArray<Record>::changed(const MSIndexVector& indices_)
{
  if (receiverList()!=0&&blocked()==MSFalse) sendIndexedEvent(indices_);
}
template <class Record>
inline unsigned int MSGenericArray<Record>::adjustPosition(unsigned int position_) const
{
  return position_+1;
}



#endif


