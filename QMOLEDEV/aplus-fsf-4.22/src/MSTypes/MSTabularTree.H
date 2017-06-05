#ifndef MSTabularTreeHEADER
#define MSTabularTreeHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSTabularTreeDefs.H>
#include <MSTypes/MSIndexVector.H>
#include <MSTypes/MSEnum.H>

template <class Element>
class MSTabularTreeIterator
{
public:
  virtual MSBoolean applyTo(Element&)=0;
};

template <class Element>
class MSTabularTreeConstantIterator
{
public:
  virtual MSBoolean applyTo(Element const&)=0;
};

#include <MSTypes/MSTabularTreeCursor.H>

template <class Element>
class MSTabularTreeNode
{
//friend class MSTabularTree<Element>;
//friend class MSTabularTreeCursor<Element>;
public:
  Element                       _element;  
  unsigned long                 _numberOfChildren;
  MSTabularTreeNode<Element>   *_pParentNode;
  MSTabularTreeNode<Element>  **_pChildren;
  
  MSTabularTreeNode(Element const& element) :
      _numberOfChildren(0),_pChildren(0),_pParentNode(0),_element(element)
  {}
  ~MSTabularTreeNode(void)
  { if (_numberOfChildren>0) delete [] _pChildren; }
};

template <class Element>
class MSTabularTree
{
private:
  MSTabularTreeNode<Element> *_pRootNode;

public:
  typedef MSTabularTreeCursor<Element> Cursor;
  
  MSTabularTree(void);
  MSTabularTree(MSTabularTree<Element> const&);
  ~MSTabularTree(void);
  
  MSTabularTree<Element>& operator=(MSTabularTree<Element> const&);
  
  void copy(MSTabularTree<Element> const&);
  void copySubtree(MSTabularTree<Element> const&,MSTabularTreeCursor<Element> const&);
                            
  void addAsRoot(Element const&);
  void addAsChild(MSTabularTreeCursor<Element> const&,
                  unsigned long,
                  Element const&);
                            
  void attachAsRoot(MSTabularTree<Element>&);
  void attachAsChild(MSTabularTreeCursor<Element> const&,
                     unsigned long,
                     MSTabularTree<Element>&);
  void attachSubtreeAsRoot(MSTabularTree<Element>&,
                           MSTabularTreeCursor<Element> const&);
  void attachSubtreeAsChild(MSTabularTreeCursor<Element> const&,
                            unsigned long,
                            MSTabularTree<Element>&,
                            MSTabularTreeCursor<Element> const&);

  void permuteChildren(MSTabularTreeCursor<Element> const&, const MSIndexVector&);
  
  unsigned long removeAll(void);
  unsigned long removeSubtree(MSTabularTreeCursor<Element> const&);

  Element const& elementAt(MSTabularTreeCursor<Element> const&) const;
  Element& elementAt(MSTabularTreeCursor<Element> const&);

  void replaceAt(MSTabularTreeCursor<Element> const&,Element const&);

  unsigned long numberOfElements(void) const;
  unsigned long numberOfSubtreeElements(MSTabularTreeCursor<Element> const&) const;
  unsigned long numberOfLeaves(void) const;
  unsigned long numberOfSubtreeLeaves(MSTabularTreeCursor<Element> const&) const;
  unsigned long position(MSTabularTreeCursor<Element> const&) const;
  
  MSTabularTreeCursor<Element>* newCursor(void) const;

  MSBoolean isEmpty(void) const;
  MSBoolean isRoot(MSTabularTreeCursor<Element> const&) const;
  MSBoolean isLeaf(MSTabularTreeCursor<Element> const&) const;
       
  MSBoolean hasChild(unsigned long,MSTabularTreeCursor<Element> const&) const;
       
  MSBoolean setToRoot(MSTabularTreeCursor<Element>&) const;
  MSBoolean setToChild(unsigned long,MSTabularTreeCursor<Element>&) const;
  MSBoolean setToParent(MSTabularTreeCursor<Element>&) const;
  MSBoolean setToFirstExistingChild(MSTabularTreeCursor<Element>&) const;
  MSBoolean setToNextExistingChild(MSTabularTreeCursor<Element>&) const;
  MSBoolean setToLastExistingChild(MSTabularTreeCursor<Element>&) const;
  MSBoolean setToPreviousExistingChild(MSTabularTreeCursor<Element>&) const;
       
  MSBoolean setToFirst(MSTabularTreeCursor<Element>&,MSTabularTreeIterationOrder) const;
  MSBoolean setToNext(MSTabularTreeCursor<Element>&,MSTabularTreeIterationOrder) const;
  MSBoolean setToLast(MSTabularTreeCursor<Element>&,MSTabularTreeIterationOrder) const;
  MSBoolean setToPrevious(MSTabularTreeCursor<Element>&,MSTabularTreeIterationOrder) const;
       
  MSBoolean allElementsDo(MSBoolean (*function)(Element&,void*),
                     MSTabularTreeIterationOrder,
                     void* additionalArgument=0);
       
  MSBoolean allElementsDo(MSTabularTreeIterator<Element>&,
                     MSTabularTreeIterationOrder);
       
  MSBoolean allElementsDo(MSBoolean (*function)(Element const&,void*),
                     MSTabularTreeIterationOrder,
                     void* additionalArgument=0) const;
       
  MSBoolean allElementsDo(MSTabularTreeConstantIterator<Element>&,
                     MSTabularTreeIterationOrder) const;
       
  MSBoolean allSubtreeElementsDo(MSTabularTreeCursor<Element> const&,
                            MSBoolean (*function)(Element&,void*),
                            MSTabularTreeIterationOrder,
                            void* additionalArgument=0);
       
  MSBoolean allSubtreeElementsDo(MSTabularTreeCursor<Element> const&,
                            MSTabularTreeIterator<Element>&,MSTabularTreeIterationOrder);
       
  MSBoolean allSubtreeElementsDo(MSTabularTreeCursor<Element> const&,
                            MSBoolean (*function)(Element const&,void*),
                            MSTabularTreeIterationOrder,void* additionalArgument=0) const;
       
  MSBoolean allSubtreeElementsDo(MSTabularTreeCursor<Element> const&,
                            MSTabularTreeConstantIterator<Element>&,
                            MSTabularTreeIterationOrder) const;
       
  MSBoolean isConsistent(void) const;

private:

  void copySubtree(MSTabularTreeNode<Element>* from);
  void copySubtree(MSTabularTreeNode<Element>*& to,MSTabularTreeNode<Element>* from);
  unsigned long removeSubtree(MSTabularTreeNode<Element>*);

  MSTabularTreeNode<Element> *newNode(Element const&);

  MSTabularTreeNode<Element> *nodeAt(MSTabularTreeCursor<Element> const&) const;
  MSTabularTreeNode<Element> *nodeAt(MSTabularTreeCursor<Element>&);
  
  void checkCursorIsForThis(MSTabularTreeCursor<Element> const&) const;
  void checkCursor(MSTabularTreeCursor<Element> const&) const;
  void checkCursorOfTree(MSTabularTree<Element> const&,MSTabularTreeCursor<Element> const&) const;
  void reservePosition(unsigned long position,MSTabularTreeNode<Element>*);
  void checkPosition(unsigned long position,MSTabularTreeNode<Element>*) const;
  void checkRootNotExists(void) const;
  void checkChildNotExists(MSTabularTreeNode<Element>*) const;
  
  static MSBoolean constantFunctionIteration(void *function,void* env,void const* node);
  static MSBoolean functionIteration(void *function,void* env,void* node);
  static MSBoolean constantIteratorIteration(void* iterator,void const* node);
  static MSBoolean iteratorIteration(void* iterator,void* node);

  MSBoolean setToParent(MSTabularTreeNode<Element>*& node) const;
  MSBoolean setToFirstExistingChild(MSTabularTreeNode<Element>*& node) const;
  MSBoolean setToNextExistingChild(MSTabularTreeNode<Element>*& node) const;
  MSBoolean setToLastExistingChild(MSTabularTreeNode<Element>*& node) const;
  MSBoolean setToPreviousExistingChild(MSTabularTreeNode<Element>*& node) const;

  MSBoolean setToFirst(MSTabularTreeNode<Element>*& node,MSTabularTreeIterationOrder order) const;
  MSBoolean setToNext(MSTabularTreeNode<Element>*& node,MSTabularTreeIterationOrder order) const;
  MSBoolean setToLast(MSTabularTreeNode<Element>*& node,MSTabularTreeIterationOrder order) const;
  MSBoolean setToPrevious(MSTabularTreeNode<Element>*& node,MSTabularTreeIterationOrder order) const;

  void initNode(MSTabularTreeNode<Element>* node,MSTabularTreeNode<Element>* parent) const;
  unsigned long numberOfSubtreeElements(MSTabularTreeNode<Element> const*) const;
  unsigned long numberOfSubtreeLeaves(MSTabularTreeNode<Element> const*) const;
  MSBoolean isLeaf(MSTabularTreeNode<Element> const*) const;
  unsigned long position(MSTabularTreeNode<Element> const*) const;

  void attachSubtreeAsRoot(MSTabularTree<Element>&,MSTabularTreeNode<Element>*);
  void attachSubtreeAsChild(MSTabularTreeNode<Element>* to,
                            unsigned long pos,
                            MSTabularTree<Element>&,
                            MSTabularTreeNode<Element>* from);

  MSBoolean allElementsDo(void* function,
                     MSTabularTreeIterationOrder order,
                     void* env,
                     MSBoolean (*apply) (void *function,void* env,void* node),
                     MSTabularTreeNode<Element>* subtreeRoot);
                    
  MSBoolean allElementsDo(void* function,
                     MSTabularTreeIterationOrder order,
                     void* env,
                     MSBoolean (*apply) (void *function,void* env,void const* node),
                     MSTabularTreeNode<Element>* subtreeRoot) const;
                    
  MSBoolean allElementsDo(void* iterator,
                     MSTabularTreeIterationOrder order,
                     MSBoolean (*apply) (void* iterator,void* node),
                     MSTabularTreeNode<Element>* subtreeRoot);
                    
  MSBoolean allElementsDo(void* iterator,
                     MSTabularTreeIterationOrder order,
                     MSBoolean (*apply) (void* iterator,void const* node),
                     MSTabularTreeNode<Element>* subtreeRoot) const;
                    
  MSBoolean checkNode(MSTabularTreeNode<Element> const*,MSTabularTreeNode<Element> const*) const;
  MSBoolean checkNode(MSTabularTreeNode<Element> const*) const;

protected:
  MSBoolean isConsistent(MSTabularTreeNode<Element> const*) const;
};

#endif
