#ifndef MSIHashKeySetHEADER
#define MSIHashKeySetHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997 -2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDefines.H>
#include <MSTypes/MSEnum.H>
#include <MSTypes/MSException.H>
#include <MSTypes/MSStandardOps.H>  

template<class Element,class Key> class MSIHashKeySet;

template<class Element>
class MSIHashKeySetNode
{
public:
  MSIHashKeySetNode<Element>    *ivNext;        
  Element  ivElement;

  MSIHashKeySetNode(Element const& element) : ivElement(element),ivNext(0)
  {};
};

template<class Element,class Key>
class MSIHashKeySetCursor
{
public:
  ~MSIHashKeySetCursor(void) {}
protected:
  MSIHashKeySet<Element,Key> const *ivCollection;
  unsigned long                  ivEntryNumber;
  MSIHashKeySetNode<Element>  *ivNode;

  friend class MSIHashKeySet<Element,Key>;

  MSBoolean isFor(MSIHashKeySet<Element,Key> const& c) const
  { return ivCollection==&c?MSTrue:MSFalse; }
public:
  MSIHashKeySetCursor()
  : ivCollection(0),ivNode(0) {};

  MSIHashKeySetCursor(MSIHashKeySet<Element,Key> const& c)
  : ivCollection(&c),ivNode(0) {};

  MSBoolean setToFirst()    {return ivCollection->setToFirst(*this);}
  MSBoolean setToNext()     {return ivCollection->setToNext(*this);}
  MSBoolean isValid() const {return ivNode!=0?MSTrue:MSFalse;}
  void invalidate()    {ivNode=0;}

  Element const& element() const
  { return ivCollection->elementAt(*this); }
  MSBoolean operator==(MSIHashKeySetCursor<Element,Key> const& cursor) const
  { return ivCollection==cursor.ivCollection&&ivNode==cursor.ivNode&&ivEntryNumber==cursor.ivEntryNumber?MSTrue:MSFalse; }
  MSBoolean operator!=(MSIHashKeySetCursor<Element,Key> const& cursor) const
  { return operator==(cursor)==MSTrue?MSFalse:MSTrue; }
};

template<class Element,class Key>
class MSIHashKeySet
{
public:
  typedef MSIHashKeySetCursor<Element,Key> Cursor;
  typedef MSIHashKeySetNode<Element> Node;

  MSIHashKeySet(unsigned long numberOfElements=16);
  MSIHashKeySet(MSIHashKeySet<Element,Key> const&);
  ~MSIHashKeySet();

  MSIHashKeySet<Element,Key>& operator=(MSIHashKeySet<Element,Key> const&);

  MSBoolean      add(Element const&);
  MSBoolean      add(Element const&,Cursor&);
  void           addAllFrom(MSIHashKeySet<Element,Key> const&);
  Element const& elementAt(Cursor const&) const;
  Element&       elementAt(Cursor const&);
  Element const& anyElement() const;
  void           removeAt(Cursor const&);
  unsigned long  removeAll(MSBoolean(*property)(Element const&,void*),void* additionalArgument=0);
  void           replaceAt(Cursor const&,Element const&);
  void           removeAll();
  unsigned long  maxNumberOfElements() const;
  unsigned long  numberOfElements() const { return ivNoElements; }
  MSBoolean      isEmpty() const { return ivNoElements==0?MSTrue:MSFalse; }
  MSBoolean      setToFirst(Cursor&) const;
  MSBoolean      setToNext(Cursor&) const;
  Key const&     key(Element const&) const;
  MSBoolean      containsElementWithKey(Key const&) const;
  MSBoolean      containsAllKeysFrom(MSIHashKeySet<Element,Key> const&) const;
  MSBoolean      locateElementWithKey(Key const&,Cursor&)const;
  MSBoolean      replaceElementWithKey(Element const&);
  MSBoolean      replaceElementWithKey(Element const&,Cursor&);
  MSBoolean      locateOrAddElementWithKey(Element const&);
  MSBoolean      locateOrAddElementWithKey(Element const&,Cursor&);
  MSBoolean      addOrReplaceElementWithKey(Element const&);
  MSBoolean      addOrReplaceElementWithKey(Element const&,Cursor&);
  MSBoolean      removeElementWithKey(Key const&);
  Element const& elementWithKey(Key const&) const;
  Element&       elementWithKey(Key const&);

private:

#if defined(MS_NESTED_CLASSES_ACCESS_BUG)
public:
#endif  
  friend class Operations;
  class Operations
  {
    friend class MSIHashKeySet<Element,Key>;
  public:
    ~Operations(void) {}
  private:
    Node* newNode(Element const& element) const { return new Node(element); }
    Node* copyNode(Node const* node) const      { return new Node(node->ivElement); }

    void deleteNode(Node * node) const {delete node;}
    void* newBlock(size_t size) const
    { return ::operator new (size); }
    void removeBlock(void* p,size_t) const
    { ::operator delete (p);; }

    unsigned long getHashvalue(Key const& key,unsigned long noEntries) const
    { return ::hash(key,noEntries); }

    Element& elementAt(Node* node)
    { return node->ivElement; }
    const Element& elementAt(Node* node) const
    { return node->ivElement; }

    Key const& key(Element const& element) const
    { return key(elementForOps(element));}
#if !defined(MS_CONST_ELEMENT_FOR_OPS_BUG)
    Key const& key(Element& element)
    { return key(elementForOps(element));}
#endif
    Key const& keyAt(Node const* node) const
    { return key(node->ivElement);}

    MSBoolean isKeyEqualToElement(Node const* node,Element const& element) const
    { return isKeysEqual(keyAt(node),key(element)); }

    MSBoolean isKeyEqualToKey(const Node *node,Key const& key) const
    { return isKeysEqual(keyAt(node),key); }

    MSBoolean isKeysEqual(const Key& key1, const Key& key2) const
    { return key1==key2?MSTrue:MSFalse; }
  
    void copyFrom(Node *node,Element const& element) const
    { node->ivElement=element; }

    MSBoolean constantFunctionIteration(void *iterationFunction,void* env,Node const* node)
    { return(*(MSBoolean(*)(Element const&,void*)) iterationFunction)(node->ivElement,env); }

#if !defined(MS_GCC_FUNCTION_CAST_BUG)
    MSBoolean functionIteration(void *iterationFunction,void* env,Node* node)
    { return(*(MSBoolean(*)(Element&,void*)) iterationFunction)(node->ivElement,env); }
#endif

  };
#if defined(MS_NESTED_CLASSES_ACCESS_BUG)
private:
#endif
  Operations         ivOps;
  unsigned long      ivNoEntries;
  unsigned long      ivNoElements;
  Node             **ivTable;
  unsigned long     *ivCollList;

private:
  void checkNotEmpty() const;
  void checkCursorIsForThis(Cursor const&) const;
  void checkCursor(Cursor const&) const;
  void createHashtable(unsigned long);
  void copyHashtable(MSIHashKeySet<Element,Key> const&);
  void resize(Node*,unsigned long&);

  MSBoolean      add(Element const& element,unsigned long,Cursor&);
  MSBoolean      add(Element const& element,unsigned long);
  unsigned long  removeAll(void* predicate,void* environment);
  MSBoolean      containsElementWithKey(Key const& key,unsigned long) const;
  unsigned long  numberOfElementsWithKey(Key const& key,unsigned long) const;
  MSBoolean      locateElementWithKey(Key const& key,unsigned long,Cursor&) const;
  MSBoolean      locateElementWithKey(Key const& key,unsigned long,Node *&) const;
  MSBoolean      locateElementWithKeyOfElement(Element const& element,unsigned long,Cursor&) const;
  MSBoolean      replaceElementWithKey(Element const& element,unsigned long,Cursor&);
  MSBoolean      replaceElementWithKey(Element const& element,unsigned long);
  MSBoolean      locateOrAddElementWithKey(Element const& element,unsigned long,Cursor&);
  MSBoolean      locateOrAddElementWithKey(Element const& element,unsigned long);
  MSBoolean      addOrReplaceElementWithKey(Element const& element,unsigned long,Cursor&);
  MSBoolean      addOrReplaceElementWithKey(Element const& element,unsigned long);
  MSBoolean      removeElementWithKey(Key const& key,unsigned long);
  const Element& elementWithKey(Key const& key,unsigned long) const;
  Element&       elementWithKey(Key const& key,unsigned long);
  MSBoolean      locateNextElementWithKey(Key const& key,unsigned long,Cursor&) const;
  unsigned long  removeAllElementsWithKey(Key const& key,unsigned long);
  unsigned long  numberOfDifferentKeys() const;
  MSBoolean      setToNextWithDifferentKey(Cursor&) const;
  unsigned long  noEntries() const { return ivNoEntries; }

  // this private constructor was created to avoid memory leak or extra deallocation
  // in resize function
  inline MSIHashKeySet(unsigned,unsigned) {}

};

#ifndef MSICollectionCheck
#if defined (MS_NO_EXCEPTIONS)
#define MSICollectionCheck(test,msgID) 
#else
#define MSICollectionCheck(test,msgID) if (!test) throw(MSCollectionError(msgID));
#endif
#endif


// inline definitions

template <class Element,class Key>
inline void MSIHashKeySet<Element,Key>::
checkNotEmpty() const
{
  MSICollectionCheck(!isEmpty(),"collection is empty")
}

template <class Element,class Key>
inline void MSIHashKeySet<Element,Key>::
checkCursorIsForThis(MSIHashKeySet<Element,Key>::Cursor const& c) const
{
  MSICollectionCheck(((Cursor const&)c).isFor(*this),"cursor not for this collection")
}

template <class Element,class Key>
inline void MSIHashKeySet<Element,Key>::
checkCursor(MSIHashKeySet<Element,Key>::Cursor const& cursor) const
{
  MSICollectionCheck(cursor.isFor(*this),"cursor not for this collection")
  MSICollectionCheck(cursor.isValid(),"invalid cursor")
}

template <class Element,class Key>
inline MSBoolean MSIHashKeySet<Element,Key>::
add(Element const& e,MSIHashKeySet<Element,Key>::Cursor& c)
{
  return locateOrAddElementWithKey(e,c)==MSTrue?MSFalse:MSTrue;
}

template <class Element,class Key>
inline MSBoolean MSIHashKeySet<Element,Key>::
add(Element const& e)
{
  return locateOrAddElementWithKey(e)==MSTrue?MSFalse:MSTrue;
}

template <class Element,class Key>
inline Element const& MSIHashKeySet<Element,Key>::
elementAt(MSIHashKeySet<Element,Key>::Cursor const& c) const
{
  checkCursor(c);
  return ivOps.elementAt(c.ivNode);
}

template <class Element,class Key>
inline Element& MSIHashKeySet<Element,Key>::
elementAt(MSIHashKeySet<Element,Key>::Cursor const& c)
{
  checkCursor(c);

  return ivOps.elementAt(c.ivNode);
}

template <class Element,class Key>
inline unsigned long MSIHashKeySet<Element,Key>::
removeAll(MSBoolean(*predicate)(Element const&,void*),void* env)
{
  return removeAll(predicate,env);
}

template <class Element,class Key>
inline void MSIHashKeySet<Element,Key>::
replaceAt(MSIHashKeySet<Element,Key>::Cursor const& c,Element const& e)
{
  checkCursor(c);
  MSICollectionCheck(ivOps.isKeysEqual(ivOps.key(((Cursor const&)c).ivNode->ivElement),ivOps.key(e)),
                      "invalid replacement")
  ivOps.copyFrom(c.ivNode,e);
}

template <class Element,class Key>
inline unsigned long MSIHashKeySet<Element,Key>::
maxNumberOfElements() const
{
  return 0;
}

template <class Element,class Key>
inline Key const& MSIHashKeySet<Element,Key>::
key(Element const& element) const
{
  return ivOps.key(element);
}

template <class Element,class Key>
inline MSBoolean MSIHashKeySet<Element,Key>::
containsElementWithKey(Key const& k) const
{
  return containsElementWithKey(k,::hash(k,noEntries()));
}

template <class Element,class Key>
inline MSBoolean MSIHashKeySet<Element,Key>::
locateElementWithKey(Key const& k,MSIHashKeySet<Element,Key>::Cursor& c) const
{
  checkCursorIsForThis(c);
  return locateElementWithKey(k,::hash(k,noEntries()),((Cursor&)c));
}

template <class Element,class Key>
inline MSBoolean MSIHashKeySet<Element,Key>::
replaceElementWithKey(Element const& e,MSIHashKeySet<Element,Key>::Cursor& c)
{
  checkCursorIsForThis(c);
  return replaceElementWithKey(e,::hash((ivOps.key(e)),noEntries()),((Cursor&)c));
}

template <class Element,class Key>
inline MSBoolean MSIHashKeySet<Element,Key>::
replaceElementWithKey(Element const& e)
{
  return replaceElementWithKey(e,::hash((ivOps.key(e)),noEntries()));
}

template <class Element,class Key>
inline MSBoolean MSIHashKeySet<Element,Key>::
locateOrAddElementWithKey(Element const& e,MSIHashKeySet<Element,Key>::Cursor& c)
{
  checkCursorIsForThis(c);
  return locateOrAddElementWithKey(e,::hash((ivOps.key(e)),noEntries()),((Cursor&)c));
}

template <class Element,class Key>
inline MSBoolean MSIHashKeySet<Element,Key>::
locateOrAddElementWithKey(Element const& e)
{
  return locateOrAddElementWithKey(e,::hash((ivOps.key(e)),noEntries()));
}

template <class Element,class Key>
inline MSBoolean MSIHashKeySet<Element,Key>::
addOrReplaceElementWithKey(Element const& e,MSIHashKeySet<Element,Key>::Cursor& c)
{
  checkCursorIsForThis(c);
  return addOrReplaceElementWithKey(e,::hash((ivOps.key(e)),noEntries()),((Cursor&)c));
}

template <class Element,class Key>
inline MSBoolean MSIHashKeySet<Element,Key>::
addOrReplaceElementWithKey(Element const& e)
{
  return addOrReplaceElementWithKey(e,::hash((ivOps.key(e)),noEntries()));
}

template <class Element,class Key>
inline MSBoolean MSIHashKeySet<Element,Key>::
removeElementWithKey(Key const& k)
{
  return removeElementWithKey(k,::hash(k,noEntries()));
}

// the following lookup functions are hand coded for speed, i.e
// they eliminate all generalized levels of indirection that would
// cause function call overhead
template <class Element,class Key>
inline Element const& MSIHashKeySet<Element,Key>::
elementWithKey(Key const& k) const
{
  Node *pNode=ivTable[::hash(k,ivNoEntries)];
  while (!(pNode==0||ivOps.isKeysEqual(ivOps.key(pNode->ivElement),k))) pNode=pNode->ivNext;
  MSICollectionCheck(pNode,"key not contained");
  return pNode->ivElement;
}

template <class Element,class Key>
inline Element& MSIHashKeySet<Element,Key>::
elementWithKey(Key const& k)
{
  Node *pNode=ivTable[::hash(k,ivNoEntries)];
  while (!(pNode==0||ivOps.isKeysEqual(ivOps.key(pNode->ivElement),k))) pNode=pNode->ivNext;  
  MSICollectionCheck(pNode,"key not contained");
  return pNode->ivElement;
}

template <class Element,class Key>
inline MSBoolean MSIHashKeySet<Element,Key>::
locateElementWithKey(Key const& k,unsigned long hashvalue,MSIHashKeySet<Element,Key>::Cursor& cursor) const
{
  cursor.ivEntryNumber=hashvalue;
  cursor.ivNode=ivTable[cursor.ivEntryNumber];
  while (!(cursor.ivNode==0||ivOps.isKeysEqual(ivOps.key(cursor.ivNode->ivElement),k)))
   { cursor.ivNode=cursor.ivNode->ivNext; }
  return cursor.ivNode!=0?MSTrue:MSFalse;
}

template <class Element,class Key>
inline MSBoolean MSIHashKeySet<Element,Key>::
locateElementWithKeyOfElement(Element const& element,unsigned long hashvalue,MSIHashKeySet<Element,Key>::Cursor& cursor) const
{
  cursor.ivEntryNumber=hashvalue;
  cursor.ivNode=ivTable[cursor.ivEntryNumber];
  while (!(cursor.ivNode==0 || ivOps.isKeysEqual(ivOps.key(cursor.ivNode->ivElement),
                                                 ivOps.key(element)) ))
   { cursor.ivNode=cursor.ivNode->ivNext; }
  return cursor.ivNode!=0?MSTrue:MSFalse;
}

#endif
