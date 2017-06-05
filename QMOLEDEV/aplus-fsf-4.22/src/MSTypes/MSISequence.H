#ifndef MSISequenceHEADER
#define MSISequenceHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997 -2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDefines.H>
#include <MSTypes/MSEnum.H>
#include <MSTypes/MSStandardOps.H>
#include <MSTypes/MSException.H>

template<class Element> class MSISequence;

template<class Element>
class MSISequenceNode 
{
public:
  MSISequenceNode (Element const& element) : ivElement (element) {};
  MSISequenceNode<Element>* ivNext;
  MSISequenceNode<Element>* ivPrevious;
  Element ivElement;
};

template<class Element>
class MSISequenceCursor 
{
public:
friend class MSISequence <Element>;
  
  MSISequenceCursor (MSISequence < Element> const& c) : ivCollection (&c), ivNode (0) {};
  ~MSISequenceCursor(void) {}

  MSBoolean setToFirst ()       { return ivCollection->setToFirst (*this);}
  MSBoolean setToNext  ()       { return ivCollection->setToNext (*this); }
  MSBoolean isValid    () const { return MSBoolean(ivNode!=0);}
  void    invalidate ()         { ivNode = 0; }

  Element const& element      () const
  { return ivCollection->elementAt (*this); }
  MSBoolean setToLast     ()
  { return ivCollection->setToLast (*this); }
  MSBoolean setToPrevious ()
  { return ivCollection->setToPrevious (*this); }
  MSBoolean operator==(MSISequenceCursor<Element> const& cursor) const
  { return ivCollection==cursor.ivCollection&&ivNode==cursor.ivNode?MSTrue:MSFalse; }
  MSBoolean operator!=(MSISequenceCursor<Element> const& cursor) const
  { return operator==(cursor)==MSTrue?MSFalse:MSTrue; }

protected:
  MSISequence<Element> const *ivCollection;
  MSISequenceNode<Element>   *ivNode;

  MSBoolean isFor (MSISequence < Element> const& c) const
  { return MSBoolean(ivCollection==&c); }
};

template < class Element >
class MSISequence  {
public:

  typedef MSISequenceCursor<Element> Cursor;
  typedef MSISequenceNode<Element> Node;

  MSISequence             (unsigned long numberOfElements = 100);
  MSISequence             (MSISequence < Element> const&);
  MSISequence < Element>& operator=(MSISequence < Element> const&);
  ~MSISequence             ();

  MSBoolean      add(Element const&);
  MSBoolean      add(Element const&,Cursor&);
  void           addAllFrom(MSISequence < Element> const&);
  Element const& elementAt(Cursor const&) const;
  Element&       elementAt(Cursor const&);
  Element const& anyElement() const;
  void           removeAt(Cursor const&);
  unsigned long  removeAll(MSBoolean(*property)(Element const&,void*),void* additionalArgument=0);
  void           replaceAt(Cursor const&,Element const&);
  void           removeAll();
  MSBoolean      isBounded() const;
  unsigned long  maxNumberOfElements() const;
  unsigned long  numberOfElements() const;
  MSBoolean      isEmpty() const;
  MSBoolean      isFull() const;
  Cursor*        newCursor() const;
  MSBoolean      setToFirst(Cursor&) const;
  MSBoolean      setToNext(Cursor&) const;
  MSBoolean      allElementsDo(MSBoolean (*function)(Element&, void*),void*additionalArgument=0);
  MSBoolean      allElementsDo(MSBoolean (*function)(Element const&, void*),
                               void* additionalArgument=0) const;
  MSBoolean      isConsistent() const;
  void           removeFirst();
  void           removeLast();
  void           removeAtPosition(unsigned long);
  Element const& firstElement() const;
  Element const& lastElement                 () const;
  Element const& elementAtPosition(unsigned long) const;
  MSBoolean      setToLast(Cursor&) const;
  MSBoolean      setToPrevious(Cursor&) const;
  void           setToPosition(unsigned long,Cursor&) const;
  MSBoolean      isFirst(Cursor const&) const;
  MSBoolean      isLast(Cursor const&) const;
  void           addAsFirst (Element const&);
  void           addAsFirst (Element const&,Cursor&);
  void           addAsLast  (Element const&);
  void           addAsLast  (Element const&,Cursor&);
  void           addAsNext  (Element const&,Cursor&);
  void           addAsPrevious(Element const&,Cursor&);
  void           addAtPosition(unsigned long,Element const&);
  void           addAtPosition(unsigned long,Element const&,Cursor&);
  void           sort(long (*comparisonFunction)(Element const&,Element const&));

private:

#if defined(MS_NESTED_CLASSES_ACCESS_BUG)
public:
#endif  
  friend class Operations;
  class  Operations 
  {
  public:
    friend class MSISequence<Element>;
    virtual ~Operations(void) {}
  private:
    Node* newNode (Node const* node) const
    {
      Node *result = new Node (node->ivElement);
      return result;
    }
    void deleteNode (Node* node) const
    { delete node; }
    
    MSBoolean constantFunctionIteration (void *iterationFunction,void* env,Node const* node) const
    {
      return (*(MSBoolean (*) (Element const&, void*)) iterationFunction)
        (node->ivElement, env);
    }
    MSBoolean functionIteration (void *iterationFunction, void* env, Node* node)
    {
      return (*(MSBoolean (*) (Element&, void*)) iterationFunction)
        (node->ivElement, env);
    }
    
    long functionComparison (void *comparisonFunction,Node const* node1,Node const* node2)
    {
      return (*(long (*) (Element const&, Element const&)) comparisonFunction)
        ( node1->ivElement,node2->ivElement );
    }
  };

#if defined(MS_NESTED_CLASSES_ACCESS_BUG)
private:
#endif
  Node *ivFirst;
  Node *ivLast;
  unsigned long ivNumberOfElements;
  Operations ivOps;

private:
  void      checkNotEmpty() const;
  void      checkCursorIsForThis (Cursor const&) const;
  void      checkCursor (Cursor const&) const;
  void      checkPositionExists (unsigned long) const;
  void      checkPositionForAdd (unsigned long) const;
  MSBoolean setToFirst (Node*& node) const;
  MSBoolean setToNext (Node*& node) const;
  MSBoolean setToLast (Node*& node) const;
  MSBoolean setToPrevious (Node*& node) const;
  void      setToPosition (unsigned long position, Node*& node) const;
  void      removeAt (Node* node);
  Node*     addAsFirst (Node* newNode);
  Node*     addAsLast  (Node* newNode);
  Node*     addAsNext  (Node *node, Node* newNode);
  Node*     addAsPrevious (Node *node, Node* newNode);
  Node*     addAtPosition (unsigned long position, Node* newNode);

};

#ifndef MSICollectionCheck
#if defined (MS_NO_EXCEPTIONS)
#define MSICollectionCheck(test,msgID) 
#else
#define MSICollectionCheck(test,msgID) if (!test) throw(MSCollectionError(msgID));
#endif
#endif

// inline definitions

template < class Element >
inline void MSISequence < Element>::checkNotEmpty () const
{
  MSICollectionCheck (!isEmpty(),"collection is empty")
}

template < class Element >
inline void MSISequence <Element>::checkCursorIsForThis(MSISequence<Element>::Cursor
                                                        const& cursor) const
{
  MSICollectionCheck (cursor.isFor(*this),"cursor not for this collection")
}


template < class Element >
inline void MSISequence < Element>::checkCursor (MSISequence<Element>::Cursor const& cursor) const
{
  MSICollectionCheck (cursor.isFor(*this),"cursor not for this collection")
  MSICollectionCheck (cursor.isValid(),"invalid cursor")
}

template < class Element >
inline void MSISequence <Element>::checkPositionExists (unsigned long position) const
{
  MSICollectionCheck ((position<=numberOfElements()+1),"invalid position")
}

template < class Element >
inline void MSISequence <Element>::checkPositionForAdd (unsigned long position) const
{
  MSICollectionCheck ((position<=numberOfElements()+1),"invalid position")
}

template < class Element >
inline MSBoolean MSISequence<Element>::add (Element const& element, MSISequence<Element>::Cursor& cursor)
{
  addAsLast (element, cursor);
  return MSTrue;
}

template < class Element >
inline MSBoolean MSISequence <Element>::add (Element const& element)
{
  Cursor cursor (*this);
  return add (element, cursor);
}


template <class Element>
inline Element const& MSISequence <Element>::elementAt (MSISequence<Element>::Cursor const& cursor) const
{
  checkCursor (cursor);
  return cursor.ivNode->ivElement;
}

template <class Element>
inline Element& MSISequence <Element>::elementAt (MSISequence<Element>::Cursor const& cursor)
{
  checkCursor (cursor);
  return cursor.ivNode->ivElement;
}

template <class Element>
inline Element const& MSISequence <Element>::anyElement () const

{
  return firstElement ();
}

template < class Element > 
inline void MSISequence <Element>::removeAt (MSISequence<Element>::Cursor const& cursor)
{
  checkCursor (cursor);
  removeAt(cursor.ivNode);
}

template < class Element > 
inline void MSISequence <Element>::replaceAt (MSISequence<Element>::Cursor const& cursor, Element const& element)
{
  checkCursor (cursor);
  cursor.ivNode->ivElement=element;
}

template <class Element>
inline MSBoolean MSISequence <Element>::isBounded () const
{
  return MSFalse;
}

template <class Element>
inline unsigned long MSISequence <Element>::maxNumberOfElements () const
{
  return 0;
}

template <class Element>
inline unsigned long MSISequence <Element>::numberOfElements () const
{
  return ivNumberOfElements;
}

template <class Element>
inline MSBoolean MSISequence <Element>::isEmpty () const
{
  return MSBoolean(ivNumberOfElements==0);
}


template <class Element>
inline MSBoolean MSISequence <Element>::isFull () const
{
  return MSFalse;
}

template <class Element>
inline MSISequence<Element>::Cursor* MSISequence <Element>::newCursor () const
{
  Cursor* result = new Cursor (*this);
  MSICollectionCheck (result!=0,"out of memory")
  return result;
}

template <class Element>
inline MSBoolean MSISequence <Element>::setToFirst (MSISequence<Element>::Cursor& cursor) const
{
  checkCursorIsForThis (cursor);
  return setToFirst(cursor.ivNode);
}

template <class Element>
inline MSBoolean MSISequence <Element>::setToNext (MSISequence<Element>::Cursor& cursor) const
{
  checkCursor (cursor);
  return setToNext(cursor.ivNode);
}

template <class Element>
inline Element const& MSISequence <Element>::firstElement () const
{
  checkNotEmpty ();
  Node *node;
  setToFirst (node);
  return node->ivElement;
}

template <class Element>
inline Element const& MSISequence <Element>::lastElement () const
{
  checkNotEmpty ();
  Node *node;
  setToLast (node);
  return node->ivElement;
}

template <class Element>
inline Element const& MSISequence <Element>::elementAtPosition (unsigned long position) const
{
  checkPositionExists (position);
  Node *node;
  setToPosition (position, node);
  return node->ivElement;
}

template <class Element>
inline MSBoolean MSISequence <Element>::setToLast (MSISequence<Element>::Cursor& cursor) const
{
  checkCursorIsForThis (cursor);
  return setToLast(cursor.ivNode);
}

template <class Element>
inline MSBoolean MSISequence <Element>::setToPrevious (MSISequence<Element>::Cursor& cursor) const
{
  checkCursor (cursor);
  return setToPrevious(cursor.ivNode);
}

template <class Element>
inline void MSISequence <Element>::setToPosition (unsigned long position, MSISequence<Element>::Cursor& cursor) const
{
  checkPositionExists (position);
  checkCursorIsForThis (cursor);
  setToPosition (position, cursor.ivNode);
}

template <class Element>
inline MSBoolean MSISequence <Element>::isFirst (MSISequence<Element>::Cursor const& cursor) const
{
  checkCursor (cursor);
  return cursor.ivNode!=0&&cursor.ivNode==ivFirst?MSTrue:MSFalse;
}

template <class Element>
inline MSBoolean MSISequence <Element>::isLast (MSISequence<Element>::Cursor const& cursor) const
{
  checkCursor (cursor);
  return cursor.ivNode!=0&&cursor.ivNode==ivLast?MSTrue:MSFalse;
}

template <class Element>
inline void MSISequence <Element>::addAsFirst (Element const& element)
{
  Cursor cursor (*this);
  addAsFirst (element, cursor);
}

template <class Element>
inline void MSISequence <Element>::addAsFirst (Element const& element, MSISequence<Element>::Cursor& cursor)
{
  checkCursorIsForThis (cursor);
  Node* newNode = new Node (element);
  MSICollectionCheck (newNode!=0,"out of memory")
  cursor.ivNode = addAsFirst (newNode);
}

template <class Element>
inline void MSISequence <Element>::addAsLast (Element const& element)
{
  Cursor cursor (*this);
  addAsLast (element, cursor);
}

template <class Element>
inline void MSISequence <Element>::addAsLast (Element const& element,
                                              MSISequence<Element>::Cursor& cursor)
{
  checkCursorIsForThis (cursor);
  Node* newNode = new Node (element);
  MSICollectionCheck (newNode!=0,"out of memory")
  cursor.ivNode = addAsLast (newNode);
}

template <class Element>
inline void MSISequence <Element>::addAsNext (Element const& element,
                                              MSISequence<Element>::Cursor& cursor)
{
  checkCursor (cursor);
  Node* newNode = new Node (element);
  MSICollectionCheck (newNode!=0,"out of memory")
  cursor.ivNode=addAsNext (cursor.ivNode, newNode);
}

template <class Element>
inline void MSISequence <Element>::addAsPrevious (Element const& element,
                                                  MSISequence<Element>::Cursor& cursor)
{
  checkCursor (cursor);
  Node* newNode = new Node (element);
  MSICollectionCheck (newNode!=0,"out of memory")
  cursor.ivNode=addAsPrevious (cursor.ivNode, newNode);
}

template <class Element>
inline void MSISequence <Element>::addAtPosition (unsigned long position, Element const& element)
{
  Cursor cursor (*this);
  addAtPosition (position, element, cursor);
}

template <class Element>
inline void MSISequence <Element>::addAtPosition (unsigned long position,
                                                  Element const& element,
                                                  MSISequence<Element>::Cursor& cursor)
{
  checkPositionForAdd (position);
  checkCursorIsForThis (cursor);
  Node* newNode = new Node (element);
  MSICollectionCheck (newNode!=0,"out of memory")
  cursor.ivNode=addAtPosition (position,newNode);
}

#if defined(MS_COMPILE_INSTANTIATE)
#include <MSTypes/MSISequence.C>
#endif

#endif
