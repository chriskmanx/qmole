#ifndef MSTabularTreeIMPLEMENTATION
#define MSTabularTreeIMPLEMENTATION

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSTabularTree.H>

// *****************************************************************************
// MSTabularTree<Element> implementation
// *****************************************************************************
template <class Element>
void MSTabularTree<Element>::reservePosition(unsigned long position,
                                             MSTabularTreeNode<Element>* node)
{
  if (position==0) MSTKTHROWEXCEPTION(MSTabularTreePositionInvalid(MSTabularTreeInvalidPositionText));
  unsigned long numChildren=node->_numberOfChildren;
  if (position>numChildren)
   {
     unsigned long newNumChildren;
     if (numChildren==0) newNumChildren=position;
     else
      {
        newNumChildren=2*numChildren;
        newNumChildren=(position>newNumChildren)?position:newNumChildren;
      }
     
     MSTabularTreeNode<Element> **pNodes=new MSTabularTreeNode<Element>*[newNumChildren];
     if (pNodes==0) MSTKTHROWEXCEPTION(MSTabularTreeOutOfMemory(MSTabularTreeOutOfCollectionMemoryText));
     unsigned long i;
     for (i=0;i<numChildren;i++) pNodes[i]=node->_pChildren[i];
     for (i=numChildren;i<newNumChildren;i++) pNodes[i]=0;
     if (node->_pChildren!=0) delete[] node->_pChildren;
     node->_pChildren=pNodes;
     node->_numberOfChildren=newNumChildren;
   }
}

// work needed
template <class Element>
void MSTabularTree<Element>::copySubtree(MSTabularTreeNode<Element>*& to,
                                         MSTabularTreeNode<Element>* from)
{
//  IASSERT(from!=0);
  to=newNode(from->_element);
  if (from->_numberOfChildren>0)
   {
     reservePosition(from->_numberOfChildren,to);
     for (unsigned long n=0;n<from->_numberOfChildren;n++)
      {
        if (from->_pChildren[n]==0) to->_pChildren[n]=0;
        else
         {
           copySubtree(to->_pChildren[n],from->_pChildren[n]);
           to->_pChildren[n]->_pParentNode=to;
         }
      }
   }
}

template <class Element>
unsigned long MSTabularTree<Element>::removeSubtree(MSTabularTreeNode<Element>* node)
{
//  IASSERT(node!=0);
  unsigned long result=0;
  for (unsigned long n=0;n<node->_numberOfChildren;n++)
   {
     if (node->_pChildren[n]!=0) result += removeSubtree(node->_pChildren[n]);
   }
  delete node;
  return result + 1;
}

template <class Element>
MSBoolean MSTabularTree<Element>::constantFunctionIteration(void *iterationFunction,
                                                       void* env,
                                                       void const* node)
{
  return MSFalse;
  //((MSBoolean(*)(Element const&,void*)) iterationFunction)
  //          (((MSTabularTreeNode<Element> const*)node)->_element,env);
}

template <class Element>
MSBoolean MSTabularTree<Element>::functionIteration(void *iterationFunction,void* env,void* node)
{
  return MSFalse;
  //((MSBoolean(*)(Element&,void*)) iterationFunction)
  //          (((MSTabularTreeNode<Element>*)node)->_element,env);
}

template <class Element>
MSBoolean MSTabularTree<Element>::constantIteratorIteration(void* iterator,void const* node)
{
  return((MSTabularTreeConstantIterator<Element>*)iterator)->
    applyTo(((MSTabularTreeNode<Element> const*)node)->_element);
}

template <class Element>
MSBoolean MSTabularTree<Element>::iteratorIteration(void* iterator,void* node)
{
  return((MSTabularTreeIterator<Element>*)iterator)->
    applyTo(((MSTabularTreeNode<Element>*)node)->_element);
}

// *****************************************************************************
// cursor manipulation
// *****************************************************************************
template <class Element>
MSBoolean MSTabularTree<Element>::setToParent(MSTabularTreeCursor<Element>& cursor) const
{
  checkCursor(cursor);
//  MSTabularTreeNode<Element>* node=cursor._pNode;
  return setToParent(cursor._pNode);
}

template <class Element>
MSBoolean MSTabularTree<Element>::setToParent(MSTabularTreeNode<Element>*& node) const
{
  node=node->_pParentNode;
  return MSBoolean(node!=0);
}

template <class Element>
MSBoolean MSTabularTree<Element>::setToFirstExistingChild(MSTabularTreeCursor<Element>& cursor) const
{
  checkCursor(cursor);
//  MSTabularTreeNode<Element>* node=cursor._pNode;
  return setToFirstExistingChild(cursor._pNode);
}

template <class Element>
MSBoolean MSTabularTree<Element>::setToFirstExistingChild(MSTabularTreeNode<Element>*& node) const
{
  for (unsigned long n=0;n<node->_numberOfChildren;n++)
   {
     if (node->_pChildren[n]!=0)
      {
        node=node->_pChildren[n];
        return MSTrue;
      }
   }
  node=0;
  return MSFalse;
}

template <class Element>
MSBoolean MSTabularTree<Element>::setToNextExistingChild(MSTabularTreeCursor<Element>& cursor) const
{
  checkCursor(cursor);
//  MSTabularTreeNode<Element>* node=cursor._pNode;
  return setToNextExistingChild(cursor._pNode);
}

template <class Element>
MSBoolean MSTabularTree<Element>::setToNextExistingChild(MSTabularTreeNode<Element>*& node) const
{
  MSTabularTreeNode<Element>* parent=node->_pParentNode;
  if (parent!=0)
   {
     MSBoolean skippedNode=MSFalse;
     for (unsigned long n=0;n<parent->_numberOfChildren;n++)
      {
        if (skippedNode)
         {
           if (parent->_pChildren[n]!=0)
            {
              node=parent->_pChildren[n];
              return MSTrue;
            }
         }
        else
         {
           if (parent->_pChildren[n]==node)
            {
              skippedNode=MSTrue;
            }
         }
      }
   }
  node=0;
  return MSFalse;
}

template <class Element>
MSBoolean MSTabularTree<Element>::setToLastExistingChild(MSTabularTreeCursor<Element>& cursor) const
{
  checkCursor(cursor);
  return setToLastExistingChild(cursor._pNode);
}

template <class Element>
MSBoolean MSTabularTree<Element>::setToLastExistingChild(MSTabularTreeNode<Element>*& node) const
{
  for (unsigned long n=node->_numberOfChildren;n>0;)
   {
     n--;
     if (node->_pChildren[n]!=0)
      {
        node=node->_pChildren[n];
        return MSTrue;
      }
   }
  node=0;
  return MSFalse;
}

template <class Element>
MSBoolean MSTabularTree<Element>::setToPreviousExistingChild(MSTabularTreeCursor<Element>& cursor) const
{
  checkCursor(cursor);
  return setToPreviousExistingChild(cursor._pNode);
}

template <class Element>
MSBoolean MSTabularTree<Element>::setToPreviousExistingChild(MSTabularTreeNode<Element>*& node) const
{
  MSTabularTreeNode<Element>* parent=node->_pParentNode;
  if (parent!=0)
   {
     MSBoolean skippedNode=MSFalse;
     for (unsigned long n=parent->_numberOfChildren;n>0;)
      {
        n--;
        if (skippedNode)
         {
           if (parent->_pChildren[n]!=0)
            {
              node=parent->_pChildren[n];
              return MSTrue;
            }
         }
        else
         {
           if (parent->_pChildren[n]==node)
            {
              skippedNode=MSTrue;
            }
         }
      }
   }
  node=0;
  return MSFalse;
}

template <class Element>
MSBoolean MSTabularTree<Element>::setToFirst(MSTabularTreeCursor<Element>& cursor,
                                        MSTabularTreeIterationOrder order) const
{
  checkCursorIsForThis(cursor);
  return setToFirst(cursor._pNode,order);
}

template <class Element>
MSBoolean MSTabularTree<Element>::setToFirst(MSTabularTreeNode<Element>*& node,
                                        MSTabularTreeIterationOrder order) const
{
  node=_pRootNode;
  if (_pRootNode==0) return MSFalse;
  if (order==MSPostorder)
   {
     MSTabularTreeNode<Element>* firstChild=node;
     while (setToFirstExistingChild(firstChild)==MSTrue) node=firstChild;
   }
  return MSTrue;
}

template <class Element>
MSBoolean MSTabularTree<Element>::setToNext(MSTabularTreeCursor<Element>& cursor,
                                       MSTabularTreeIterationOrder order) const
{
  checkCursorIsForThis(cursor);
//  MSTabularTreeNode<Element>* node=cursor._pNode;
  return setToNext(cursor._pNode,order);
}

template <class Element>
MSBoolean MSTabularTree<Element>::setToNext(MSTabularTreeNode<Element>*& node,
                                       MSTabularTreeIterationOrder order) const
{
  if (order==MSPreorder)
   {
     MSTabularTreeNode<Element>* firstChild=node;
     if (setToFirstExistingChild(firstChild)==MSTrue)
      {
        node=firstChild;
        return MSTrue;
      }
     else
      {
        MSTabularTreeNode<Element>* nextChild=node;
        while (setToNextExistingChild(nextChild)==MSFalse)
         {
           if (!setToParent(node)) return MSFalse;
           nextChild=node;
         }
        node=nextChild;
        return MSTrue;
      }
   }
  else
   {
     MSTabularTreeNode<Element>* nextChild=node;
     if (setToNextExistingChild(nextChild)==MSTrue)
      {
        node=nextChild;
        while (setToFirstExistingChild(nextChild)==MSTrue) node=nextChild;
        return MSTrue;
      }
     else return setToParent(node);
   }
}

template <class Element>
MSBoolean MSTabularTree<Element>::setToLast(MSTabularTreeCursor<Element>& cursor,
                                       MSTabularTreeIterationOrder order) const
{
  checkCursorIsForThis(cursor);
//  MSTabularTreeNode<Element>* node=cursor._pNode;
  return setToLast(cursor._pNode,order);
}

template <class Element>
MSBoolean MSTabularTree<Element>::setToLast(MSTabularTreeNode<Element>*& node,
                                       MSTabularTreeIterationOrder order) const
{
  node=_pRootNode;
  if (_pRootNode==0) return MSFalse;
  if (order==MSPreorder)
   {
     MSTabularTreeNode<Element>* lastChild=node;
     while (setToLastExistingChild(lastChild)==MSTrue) node=lastChild;
   }
  return MSTrue;
}

template <class Element>
MSBoolean MSTabularTree<Element>::setToPrevious(MSTabularTreeCursor<Element>& cursor,
                                           MSTabularTreeIterationOrder order) const
{
  checkCursorIsForThis(cursor);
//  MSTabularTreeNode<Element>* node=cursor._pNode;
  return setToPrevious(cursor._pNode,order);
}

template <class Element>
MSBoolean MSTabularTree<Element>::setToPrevious(MSTabularTreeNode<Element>*& node,
                                           MSTabularTreeIterationOrder order) const
{
  if (order==MSPostorder)
   {
     MSTabularTreeNode<Element>* lastChild=node;
     if (setToLastExistingChild(lastChild)==MSTrue)
      {
        node=lastChild;
        return MSTrue;
      }
     else
      {
        MSTabularTreeNode<Element>* previousChild=node;
        while (setToPreviousExistingChild(previousChild)==MSFalse)
         {
           if (setToParent(node)==MSFalse) return MSFalse;
           previousChild=node;
         }
        node=previousChild;
        return MSTrue;
      }
   }
  else
   {
     MSTabularTreeNode<Element>* previousChild=node;
     if (setToPreviousExistingChild(previousChild)==MSTrue)
      {
        node=previousChild;
        while (setToLastExistingChild(previousChild)==MSTrue) node=previousChild;
        return MSTrue;
      }
     else return setToParent(node);
   }
}


// *****************************************************************************
// implementation functions
// *****************************************************************************

template <class Element>
void MSTabularTree<Element>::initNode(MSTabularTreeNode<Element>* node,
                                      MSTabularTreeNode<Element>* parent) const
{
  node->_pParentNode=parent;
  for (unsigned long n=0;n<node->_numberOfChildren;n++) node->_pChildren[n]=0;
}

template <class Element>
unsigned long MSTabularTree<Element>::numberOfSubtreeElements(MSTabularTreeNode<Element> const* node) const
{
//  IASSERT(node!=0);
  unsigned long result=0;
  for (unsigned long n=0;n<node->_numberOfChildren;n++)
   {
     if (node->_pChildren[n]!=0) result+=numberOfSubtreeElements(node->_pChildren[n]);
   }
  return result + 1;
}

template <class Element>
unsigned long MSTabularTree<Element>::numberOfSubtreeLeaves(MSTabularTreeNode<Element> const* node) const
{
//  IASSERT(node!=0);
  unsigned long result=0;
  for (unsigned long n=0;n<node->_numberOfChildren;n++)
   {
     if (node->_pChildren[n]!=0) result+=numberOfSubtreeLeaves(node->_pChildren[n]);
   }
  // node is a leaf
  if (result==0) result=1;
  return result;
}

template <class Element>
MSBoolean MSTabularTree<Element>::isLeaf(MSTabularTreeNode<Element> const* node) const
{
  for (unsigned long n=0;n<node->_numberOfChildren;n++)
   {
     if (node->_pChildren[n]!=0) return MSFalse;
   }
  return MSTrue;
}

template <class Element>
unsigned long MSTabularTree<Element>::position(MSTabularTreeNode<Element> const* node) const
{
  MSTabularTreeNode<Element>* parent=node->_pParentNode;
  if (parent==0) return 1;
  for (unsigned long n=0;n<parent->_numberOfChildren;n++)
   {
     if (parent->_pChildren[n]==node) return n+1;
   }
//  IASSERT(MSFalse);
  return 1;
}

template <class Element>
void MSTabularTree<Element>::attachSubtreeAsRoot(MSTabularTree<Element>& tree,
                                                 MSTabularTreeNode<Element>* node)
{
  if (node->_pParentNode==0) tree._pRootNode=0;
  else node->_pParentNode->_pChildren[tree.position(node)-1]=0;
  _pRootNode=node;
  if (_pRootNode!=0) _pRootNode->_pParentNode=0;
}

template <class Element>
void MSTabularTree<Element>::attachSubtreeAsChild(MSTabularTreeNode<Element>* parent,
                                                  unsigned long pos,
                                                  MSTabularTree<Element>& tree,
                                                  MSTabularTreeNode<Element>* from)
{
  if (from->_pParentNode==0) tree._pRootNode=0;
  else from->_pParentNode->_pChildren[tree.position(from)-1]=0;
  parent->_pChildren[pos-1]=from;
  if (from!=0) from->_pParentNode=parent;
}


// *****************************************************************************
// allElementsDo
// *****************************************************************************

template <class Element>
MSBoolean MSTabularTree<Element>::allElementsDo(void* function,
                                           MSTabularTreeIterationOrder order,
                                           void* env,
                                           MSBoolean(*apply)(void *function,void* env,void* node),
                                           MSTabularTreeNode<Element>* subtreeRoot)
{
  if (order==MSPreorder)
   {
     if (!(*apply)(function,env,subtreeRoot)) return MSFalse;
     for (int i=0;i<subtreeRoot->_numberOfChildren;i++)
      {
        MSTabularTreeNode<Element>* child=subtreeRoot->_pChildren[i];
        if (child!=0)
         {
           if (!allElementsDo(function,order,env,apply,child)) return MSFalse;
         }
      }
   }
  else
   {
     for (int i=0;i<subtreeRoot->_numberOfChildren;i++)
      {
        MSTabularTreeNode<Element>* child=subtreeRoot->_pChildren[i];
        if (child!=0)
         {
           if (!allElementsDo(function,order,env,apply,child)) return MSFalse;
         }
      }
     if (!(*apply)(function,env,subtreeRoot)) return MSFalse;
   }
  return MSTrue;
}

template <class Element>
MSBoolean MSTabularTree<Element>::allElementsDo(void* function,
                                           MSTabularTreeIterationOrder order,
                                           void* env,
                                           MSBoolean(*apply)(void *function,void* env,void const* node),
                                           MSTabularTreeNode<Element>* subtreeRoot) const
{
  if (order==MSPreorder)
   {
     if (!(*apply)(function,env,subtreeRoot)) return MSFalse;
     for (int i=0;i<subtreeRoot->_numberOfChildren;i++)
      {
        MSTabularTreeNode<Element>* child=subtreeRoot->_pChildren[i];
        if (child!=0)
         {
           if (!allElementsDo(function,order,env,apply,child)) return MSFalse;
         }
      }
   }
  else
   {
     for (int i=0;i<subtreeRoot->_numberOfChildren;i++)
      {
        MSTabularTreeNode<Element>* child=subtreeRoot->_pChildren[i];
        if (child!=0)
         {
           if (!allElementsDo(function,order,env,apply,child)) return MSFalse;
         }
      }
     if (!(*apply)(function,env,subtreeRoot)) return MSFalse;
   }
  return MSTrue;
}

template <class Element>
MSBoolean MSTabularTree<Element>::allElementsDo(void* iterator,
                                           MSTabularTreeIterationOrder order,
                                           MSBoolean(*apply)(void* iterator,void* node),
                                           MSTabularTreeNode<Element>* subtreeRoot)
{
  if (order==MSPreorder)
   {
     if (!(*apply)(iterator,subtreeRoot)) return MSFalse;
     for (int i=0;i<subtreeRoot->_numberOfChildren;i++)
      {
        MSTabularTreeNode<Element>* child=subtreeRoot->_pChildren[i];
        if (child!=0)
         {
           if (!allElementsDo(iterator,order,apply,child)) return MSFalse;
         }
      }
   }
  else
   {
     for (int i=0;i<subtreeRoot->_numberOfChildren;i++)
      {
        MSTabularTreeNode<Element>* child=subtreeRoot->_pChildren[i];
        if (child!=0)
         {
           if (!allElementsDo(iterator,order,apply,child)) return MSFalse;
         }
      }
     if (!(*apply)(iterator,subtreeRoot)) return MSFalse;
   }
  return MSTrue;
}

template <class Element>
MSBoolean MSTabularTree<Element>::allElementsDo(void* iterator,
                                           MSTabularTreeIterationOrder order,
                                           MSBoolean(*apply)(void* iterator,void const* node),
                                           MSTabularTreeNode<Element>* subtreeRoot) const
{
  if (order==MSPreorder)
   {
     if (!(*apply)(iterator,subtreeRoot)) return MSFalse;
     for (int i=0;i<subtreeRoot->_numberOfChildren;i++)
      {
        MSTabularTreeNode<Element>* child=subtreeRoot->_pChildren[i];
        if (child!=0)
         {
           if (!allElementsDo(iterator,order,apply,child)) return MSFalse;
         }
      }
   }
  else
   {
     for (int i=0;i<subtreeRoot->_numberOfChildren;i++)
      {
        MSTabularTreeNode<Element>* child=subtreeRoot->_pChildren[i];
        if (child!=0)
         {
           if (!allElementsDo(iterator,order,apply,child)) return MSFalse;
         }
      }
     if (!(*apply)(iterator,subtreeRoot)) return MSFalse;
   }
  return MSTrue;
}

template <class Element>
MSBoolean MSTabularTree<Element>::checkNode(MSTabularTreeNode<Element> const* node) const
{
  if (_pRootNode!=0) return checkNode(node,_pRootNode);
  return MSFalse;
}

template <class Element>
MSBoolean MSTabularTree<Element>::checkNode(MSTabularTreeNode<Element> const* node,
                                       MSTabularTreeNode<Element> const* current) const
{
  if (node==current) return MSTrue;
  for (int i=0;i<current->_numberOfChildren;i++)
   {
     if (current->_pChildren[i]!=0&&
         checkNode(node,current->_pChildren[i])) return MSTrue;
   }
  return MSFalse;
}

template <class Element>
MSBoolean MSTabularTree<Element>::isConsistent(MSTabularTreeNode<Element> const* node) const
{
  for (int i=0;i<node->_numberOfChildren;i++)
   {
     if (node->_pChildren[i]!=0&&
         (node->_pChildren[i]->_pParentNode!=node||
          isConsistent(node->_pChildren[i])==MSFalse)) return MSFalse;
   }
  return MSTrue;
}

// *****************************************************************************
// MSTabularTree<Element> inlines
// *****************************************************************************

template <class Element>
MSTabularTreeNode<Element> *MSTabularTree<Element>::nodeAt(MSTabularTreeCursor<Element> const& cursor) const
{ return(MSTabularTreeNode<Element> *)cursor._pNode; }

template <class Element>
MSTabularTreeNode<Element> *MSTabularTree<Element>::nodeAt(MSTabularTreeCursor<Element>& cursor)
{ return(MSTabularTreeNode<Element> *)cursor._pNode; }

template <class Element>
void MSTabularTree<Element>::
checkCursorIsForThis(MSTabularTreeCursor<Element> const& cursor) const
{
  if (cursor.isFor(*this)==MSFalse)
   {
     MSTKTHROWEXCEPTION(MSTabularTreeCursorInvalid(MSTabularTreeCursorNotForThisText));
   }
}

template <class Element>
void MSTabularTree<Element>::checkCursor(MSTabularTreeCursor<Element> const& cursor) const
{
  if (cursor.isFor(*this)==MSFalse)
   {
     MSTKTHROWEXCEPTION(MSTabularTreeCursorInvalid(MSTabularTreeCursorNotForThisText));
   }
  if (cursor.isValid()==MSFalse) MSTKTHROWEXCEPTION(MSTabularTreeCursorInvalid(MSTabularTreeInvalidCursorText));
  if (checkNode(cursor._pNode)==MSFalse)
   {
     MSTKTHROWEXCEPTION(MSTabularTreeCursorInvalid(MSTabularTreeCursorNotContainedText));
   }
}

template <class Element>
void MSTabularTree<Element>::checkCursorOfTree(MSTabularTree<Element> const& tree,
                                               MSTabularTreeCursor<Element> const& cursor) const
{
  if (cursor._pTabularTree!=&tree)
   {
     MSTKTHROWEXCEPTION(MSTabularTreeCursorInvalid(MSTabularTreeCursorNotForGivenText));
   }
  if (cursor.isValid()==MSFalse) MSTKTHROWEXCEPTION(MSTabularTreeCursorInvalid(MSTabularTreeInvalidCursorText));
  if (tree.checkNode(cursor._pNode)==MSFalse)
   {
     MSTKTHROWEXCEPTION(MSTabularTreeCursorInvalid(MSTabularTreeCursorNotContainedText));
   }
}

template <class Element>
void MSTabularTree<Element>::checkPosition(unsigned long position,
                                           MSTabularTreeNode<Element>* node) const
{
  if (position<1||position>node->_numberOfChildren)
   {
     MSTKTHROWEXCEPTION(MSTabularTreePositionInvalid(MSTabularTreeInvalidPositionText));
   }
}

template <class Element>
void MSTabularTree<Element>::checkRootNotExists() const
{
  if (_pRootNode!=0) MSTKTHROWEXCEPTION(MSTabularTreeRootAlreadyExists(MSTabularTreeRootAlreadyExistsText));
}

template <class Element>
void MSTabularTree<Element>::checkChildNotExists(MSTabularTreeNode<Element>* n) const
{
  if (n!=0) MSTKTHROWEXCEPTION(MSTabularTreeChildAlreadyExists(MSTabularTreeChildAlreadyExistsText));
}

template <class Element>
void MSTabularTree<Element>::copySubtree(MSTabularTreeNode<Element>* from)
{
  if (from==0) _pRootNode=0;
  else
   {
     copySubtree((MSTabularTreeNode<Element>*&)_pRootNode,from);
     _pRootNode->_pParentNode=0;
   }
}

template <class Element>
MSTabularTree<Element>::MSTabularTree() 
    :_pRootNode(0)
{
}

template <class Element>
MSTabularTree<Element>::MSTabularTree(MSTabularTree<Element> const& tabularTree)
{
  copySubtree((MSTabularTreeNode<Element>*)tabularTree._pRootNode);
}

template <class Element>
MSTabularTree<Element>::~MSTabularTree()
{
  removeAll();
}

template <class Element>
MSTabularTree<Element>&
MSTabularTree<Element>::operator=(MSTabularTree<Element> const& tree)
{
  copy(tree);
  return *this;
}

template <class Element>
void MSTabularTree<Element>::copy(MSTabularTree<Element> const& tree)
{
  if (&tree!=this)
   {
     removeAll();
     copySubtree((MSTabularTreeNode<Element>*)tree._pRootNode);
   }
}

template <class Element>
void MSTabularTree<Element>::copySubtree(MSTabularTree<Element> const& tree,
                                         MSTabularTreeCursor<Element> const& cursor)
{
  checkCursorOfTree(tree,cursor);
  if (&tree!=this)
   {
     removeAll();
     copySubtree((MSTabularTreeNode<Element>*)nodeAt(cursor));
   }
  else
   {
     MSTabularTree<Element> t;
     t.attachSubtreeAsRoot(*this,cursor);
     removeAll();
     attachAsRoot(t);
   }
}

template <class Element>
void MSTabularTree<Element>::addAsRoot(Element const& element)
{
  checkRootNotExists();
  _pRootNode=(MSTabularTreeNode<Element>*) newNode(element);
  initNode(_pRootNode,0);
}

template <class Element>
void MSTabularTree<Element>::addAsChild(MSTabularTreeCursor<Element> const& cursor,
                                        unsigned long position,
                                        Element const& element)
{
  checkCursor(cursor);
  MSTabularTreeNode<Element>* parent=cursor._pNode;
  reservePosition(position,parent);
  MSTabularTreeNode<Element>*& child=parent->_pChildren[position-1];
  checkChildNotExists(child);
  child=(MSTabularTreeNode<Element>*) newNode(element);
  initNode(child,parent);
}

template <class Element>
void MSTabularTree<Element>::attachAsRoot(MSTabularTree<Element>& tree)
{
  checkRootNotExists();
  _pRootNode=tree._pRootNode;
  tree._pRootNode=0;
}

template <class Element>
void MSTabularTree<Element>::attachAsChild(MSTabularTreeCursor<Element> const& cursor,
                                           unsigned long position,
                                           MSTabularTree<Element>& tree)
{
  checkCursor(cursor);
  MSTabularTreeNode<Element>* parent=cursor._pNode;
  reservePosition(position,parent);
  MSTabularTreeNode<Element>*& child=parent->_pChildren[position-1];
  checkChildNotExists(child);
  child=tree._pRootNode;
  if (child!=0) child->_pParentNode=parent;
  tree._pRootNode=0;
}

template <class Element>
void MSTabularTree<Element>::attachSubtreeAsRoot(MSTabularTree<Element>& tree,
                                                 MSTabularTreeCursor<Element> const& cursor)
{
  checkCursorOfTree(tree,cursor);
  checkRootNotExists();
  MSTabularTreeNode<Element>* node=cursor._pNode;
  attachSubtreeAsRoot(tree,node);
}

template <class Element>
void MSTabularTree<Element>::attachSubtreeAsChild(MSTabularTreeCursor<Element> const& cursorTo,
                                                  unsigned long position,
                                                  MSTabularTree<Element>& tree,
                                                  MSTabularTreeCursor<Element> const& cursorFrom)
{
  checkCursorOfTree(tree,cursorFrom);
  checkCursor(cursorTo);
  MSTabularTreeNode<Element>* parent=nodeAt(cursorTo);
  reservePosition(position,parent);
  MSTabularTreeNode<Element>*& child=parent->_pChildren[position-1];
  checkChildNotExists(child);
  MSTabularTreeNode<Element>* nodeFrom=nodeAt(cursorFrom);
  attachSubtreeAsChild(parent,position,tree,nodeFrom);
}

template <class Element>
unsigned long MSTabularTree<Element>::removeAll()
{
  unsigned long result;
  if (_pRootNode!=0)
   {
     result=removeSubtree((MSTabularTreeNode<Element>*)_pRootNode);
     _pRootNode=0;
   }
  else result=0;
  return result;
}

template <class Element>
unsigned long MSTabularTree<Element>::removeSubtree(MSTabularTreeCursor<Element> const& cursor)
{
  checkCursor(cursor);
  MSTabularTreeNode<Element>* node=nodeAt(cursor);
  if (node->_pParentNode==0) _pRootNode=0;
  else node->_pParentNode->_pChildren[position(cursor)-1]=0;
  return removeSubtree((MSTabularTreeNode<Element>*)node);
}

template <class Element>
Element const& MSTabularTree<Element>::elementAt(MSTabularTreeCursor<Element> const& cursor) const
{
  checkCursor(cursor);
  return nodeAt(cursor)->_element;
}

template <class Element>
Element& MSTabularTree<Element>::elementAt(MSTabularTreeCursor<Element> const& cursor)
{
  checkCursor(cursor);
  return nodeAt(cursor)->_element;
}

template <class Element>
void MSTabularTree<Element>::replaceAt(MSTabularTreeCursor<Element> const& cursor,
                                       Element const& element_)
{
  checkCursor(cursor);
//  assign(((MSTabularTreeNode<Element>*)nodeAt(cursor)->_element,element);
  nodeAt(cursor)->_element=element_;
}

template <class Element>
unsigned long MSTabularTree<Element>::numberOfElements() const
{
  if (_pRootNode==0) return 0;
  return numberOfSubtreeElements(_pRootNode);
}

template <class Element>
unsigned long MSTabularTree<Element>::numberOfSubtreeElements(MSTabularTreeCursor<Element> const& cursor) const
{
  checkCursor(cursor);
  return numberOfSubtreeElements(nodeAt(cursor));
}

template <class Element>
unsigned long MSTabularTree<Element>::numberOfLeaves() const
{
  if (_pRootNode==0) return 0;
  return numberOfSubtreeLeaves(_pRootNode);
}

template <class Element>
unsigned long MSTabularTree<Element>::numberOfSubtreeLeaves(MSTabularTreeCursor<Element> const& cursor) const
{
  checkCursor(cursor);
  return numberOfSubtreeLeaves(nodeAt(cursor));
}

template <class Element>
MSBoolean MSTabularTree<Element>::isEmpty() const
{ return MSBoolean(_pRootNode==0); }

template <class Element>
MSTabularTreeCursor<Element>* MSTabularTree<Element>::newCursor() const
{
  MSTabularTreeCursor<Element>* result=new MSTabularTreeCursor<Element>(*this);
  if (result==0) MSTKTHROWEXCEPTION(MSTabularTreeOutOfMemory(MSTabularTreeOutOfCollectionMemoryText));
  return result;
}

template <class Element>
MSBoolean MSTabularTree<Element>::isRoot(MSTabularTreeCursor<Element> const& cursor) const
{
  checkCursor(cursor);
  return MSBoolean(_pRootNode==nodeAt(cursor));
}

template <class Element>
MSBoolean MSTabularTree<Element>::isLeaf(MSTabularTreeCursor<Element> const& cursor) const
{
  checkCursor(cursor);
  return isLeaf(nodeAt(cursor));
}

template <class Element>
unsigned long MSTabularTree<Element>::position(MSTabularTreeCursor<Element> const& cursor) const
{
  checkCursor(cursor);
  return position(nodeAt(cursor));
}

template <class Element>
MSBoolean MSTabularTree<Element>::hasChild(unsigned long position,
                                      MSTabularTreeCursor<Element> const& cursor) const
{
  checkCursor(cursor);
  MSTabularTreeNode<Element>* node=nodeAt(cursor);
  checkPosition(position,node);
  return MSBoolean(node->_pChildren[position-1]!=0);
}

template <class Element>
MSBoolean MSTabularTree<Element>::setToRoot(MSTabularTreeCursor<Element>& cursor) const
{
  checkCursorIsForThis(cursor);
  cursor._pNode=(MSTabularTreeNode<Element>*)_pRootNode;
  return MSBoolean(_pRootNode!=0);
}

template <class Element>
MSBoolean MSTabularTree<Element>::setToChild(unsigned long position,
                                        MSTabularTreeCursor<Element>& cursor) const
{
  checkCursor(cursor);
  MSTabularTreeNode<Element>* node=cursor._pNode;
  checkPosition(position,node);
  cursor._pNode=node->_pChildren[position-1];
  return MSBoolean(cursor._pNode!=0);
}

// *****************************************************************************
// allElementsDo
// *****************************************************************************

template <class Element>
MSBoolean MSTabularTree<Element>::allElementsDo(MSBoolean(*function)(Element&,void*),
                                           MSTabularTreeIterationOrder order,
                                           void* additionalArgument)
{
  if (_pRootNode==0) return MSTrue;
  return allElementsDo((void *)function,order,additionalArgument,
                       functionIteration,_pRootNode);
}

template <class Element>
MSBoolean MSTabularTree<Element>::allElementsDo(MSTabularTreeIterator<Element>& iterator,
                                           MSTabularTreeIterationOrder order)
{
  if (_pRootNode==0) return MSTrue;
  return allElementsDo(&iterator,order,
                       iteratorIteration,_pRootNode);
}

template <class Element>
MSBoolean MSTabularTree<Element>::allElementsDo(MSBoolean(*function)(Element const&,void*),
                                           MSTabularTreeIterationOrder order,
                                           void* additionalArgument) const
{
  if (_pRootNode==0) return MSTrue;
  return allElementsDo((void *)function,order,additionalArgument,
                       constantFunctionIteration,_pRootNode);
}

template <class Element>
MSBoolean MSTabularTree<Element>::allElementsDo(MSTabularTreeConstantIterator<Element>& iterator,
                                           MSTabularTreeIterationOrder order) const
{
  if (_pRootNode==0) return MSTrue;
  return allElementsDo(&iterator,order,
                       constantIteratorIteration,_pRootNode);
}

template <class Element>
MSBoolean MSTabularTree<Element>::isConsistent() const
{ return MSBoolean(_pRootNode==0||(_pRootNode->_pParentNode==0&&isConsistent(_pRootNode))); }

template <class Element>
MSBoolean MSTabularTree<Element>::allSubtreeElementsDo(MSTabularTreeCursor<Element> const& cursor,
                                                  MSBoolean(*function)(Element&,void*),
                                                  MSTabularTreeIterationOrder order,
                                                  void* additionalArgument)
{
  checkCursor(cursor);
  return allElementsDo((void*)function,order,additionalArgument,
                       functionIteration,cursor._pNode);
}

template <class Element>
MSBoolean MSTabularTree<Element>::allSubtreeElementsDo(MSTabularTreeCursor<Element> const& cursor,
                                                  MSTabularTreeIterator<Element>& iterator,
                                                  MSTabularTreeIterationOrder order)
{
  checkCursor(cursor);
  return allElementsDo(&iterator,order,iteratorIteration,
                       nodeAt(cursor));
}

template <class Element>
MSBoolean MSTabularTree<Element>::allSubtreeElementsDo(MSTabularTreeCursor<Element> const& cursor,
                                                  MSBoolean(*function)(Element const&,void*),
                                                  MSTabularTreeIterationOrder order,
                                                  void* additionalArgument) const
{
  checkCursor(cursor);
  return allElementsDo((void *)function,order,additionalArgument,
                       constantFunctionIteration,nodeAt(cursor));
}

template <class Element>
MSBoolean MSTabularTree<Element>::allSubtreeElementsDo(MSTabularTreeCursor<Element> const& cursor,
                                                  MSTabularTreeConstantIterator<Element>& iterator,
                                                  MSTabularTreeIterationOrder order) const
{
  checkCursor(cursor);
  return allElementsDo(&iterator,order,constantIteratorIteration,
                       nodeAt(cursor));
}

template <class Element>
MSTabularTreeNode<Element>* MSTabularTree<Element>::newNode(Element const& element)
{
  MSTabularTreeNode<Element> *result=new MSTabularTreeNode<Element>(element);
  if (result==0) MSTKTHROWEXCEPTION(MSTabularTreeOutOfMemory(MSTabularTreeOutOfCollectionMemoryText));
  return result;
}

template <class Element>
void MSTabularTree<Element>::permuteChildren(MSTabularTreeCursor<Element> const& cursor_,
                                             const MSIndexVector& index_)
{
  checkCursor(cursor_);
  MSTabularTreeNode<Element>* parent=nodeAt(cursor_);
  MSTabularTreeNode<Element> **newChildren=new MSTabularTreeNode<Element>*[parent->_numberOfChildren];
  unsigned long i,n=index_.length(),pos;
  for(i=0;i<n;i++)
   {
     pos=index_(i);
     checkPosition(pos,parent);
     pos--;
     newChildren[i]=parent->_pChildren[pos];
     parent->_pChildren[pos] = 0;
   }
  for(;i<parent->_numberOfChildren;i++)
   {
     newChildren[i]=0;
   }
  for(i=0;i<n;i++)
   {
     if(parent->_pChildren[i] !=0) removeSubtree(parent->_pChildren[i]);
   }
  delete parent->_pChildren;
  parent->_pChildren=newChildren;
}


#endif
