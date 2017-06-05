#ifndef MSObservableTreeIMPLEMENTATION
#define MSObservableTreeIMPLEMENTATION

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSObservableTree.H>
#include <MSTypes/MSString.H>

template <class Element>
MSObservableTree<Element>::MSObservableTree(void)
{}

template <class Element>
MSObservableTree<Element>::MSObservableTree(const MSObservableTree<Element>& aTree_)
{
  MSTabularTree<Element>::copy(aTree_);
}

template <class Element>
MSObservableTree<Element>& MSObservableTree<Element>::operator=(const MSObservableTree<Element>& aTree_)
{
  if (this!=&aTree_) copy(aTree_);
  return *this;
}

template <class Element>
void MSObservableTree<Element>::changed(const MSTabularTreeCursor<Element>& cursor_,unsigned long position_,
                                        MSObservableTreeEventType type_)
{
  if (receiverList()!=0)
   {
     MSObservableTreeEvent<Element> e(cursor_,position_,type_);
     sendEvent(e);
   }
}

template <class Element>
void MSObservableTree<Element>::changed(void)
{
  if (receiverList()!=0)
   {
     MSObservableTreeEvent<Element> e(MSTabularTreeCursor<Element>(*this),
				      0,
				      MSObservableTreeCopy);
     sendEvent(e);
   }
}

template <class Element>
MSObservableTree<Element>::~MSObservableTree(void)
{}

template <class Element>
void MSObservableTree<Element>::copy(MSTabularTree<Element> const& tree_)
{
  if (&tree_!=this)
  {
    MSTabularTree<Element>::copy(tree_);
    changed();
  }
}

template <class Element>
void MSObservableTree<Element>::copySubtree(MSTabularTree<Element>const& tree_,
                                            MSTabularTreeCursor<Element> const& cursor_)
{
  MSTabularTree<Element>::copySubtree(tree_,cursor_);
  changed();
}

template <class Element>
void MSObservableTree<Element>::addAsChild(MSTabularTreeCursor<Element> const& cursor_,
                                           unsigned long position_,
                                           Element const& element_)
{
  MSTabularTree<Element>::addAsChild(cursor_,position_,element_);
  changed(cursor_,position_,MSObservableTreeInsert);
}

template <class Element>
void MSObservableTree<Element>::removeAll(void)
{
  MSTabularTree<Element>::removeAll();
  changed();
}

template <class Element>
void MSObservableTree<Element>::removeSubtree(const MSTabularTreeCursor<Element>& cursor_)
{
  MSTabularTreeCursor<Element> cursor2(cursor_);
  unsigned long pos=position(cursor2);
  cursor2.setToParent();
  MSTabularTree<Element>::removeSubtree(cursor_);
  if (cursor2.isValid()) changed(cursor2,pos,MSObservableTreeDelete);
  else changed();
}


template <class Element>
void MSObservableTree<Element>::addAsRoot(Element const& element_)
{
  MSTabularTree<Element>::addAsRoot(element_);
  changed();
}

template <class Element>
void MSObservableTree<Element>::attachAsRoot(MSObservableTree<Element> &tree_)
{
  MSTabularTree<Element>::attachAsRoot(tree_);
  changed();
  tree_.changed();
}

template <class Element>
void MSObservableTree<Element>::attachAsChild(MSTabularTreeCursor<Element> const& cursor_,
                                              unsigned long position_,
                                              MSObservableTree<Element>& tree_)
{
  MSTabularTree<Element>::attachAsChild(cursor_,position_,tree_);
  changed(cursor_,position_,MSObservableTreeInsert);
  tree_.changed();
}

template <class Element>
void MSObservableTree<Element>::attachSubtreeAsRoot(MSObservableTree<Element>& tree_,
                                                    const MSTabularTreeCursor<Element>& cursor_)
{
  MSTabularTreeCursor<Element> cursor2(cursor_);
  unsigned long p=tree_.position(cursor2);
  cursor2.setToParent();
  
  MSTabularTree<Element>::attachSubtreeAsRoot(tree_,cursor_);
  changed();
  
  if (cursor2.isValid()) tree_.changed(cursor2,p,MSObservableTreeDelete);
  else tree_.changed(); // whole tree is deleted
}

template <class Element>
void MSObservableTree<Element>::attachSubtreeAsChild(const MSTabularTreeCursor<Element> &cursorTo_,
                                                     unsigned long pos_,
                                                     MSObservableTree<Element>& tree_,
                                                     MSTabularTreeCursor<Element> const& cursorFrom_)
{
  MSTabularTreeCursor<Element> cursor2(cursorFrom_);
  unsigned long pos2=tree_.position(cursor2);
  cursor2.setToParent();
  
  MSTabularTree<Element>::attachSubtreeAsChild(cursorTo_,pos_,tree_,cursorFrom_);
  changed(cursorTo_,pos_,MSObservableTreeInsert);
  if (cursor2.isValid()) tree_.changed(cursor2,pos2,MSObservableTreeDelete);
  else tree_.changed();  // whole tree is deleted
}

template <class Element>
void MSObservableTree<Element>::replaceAt(MSTabularTreeCursor<Element> const& cursor_,Element const& element_)
{
  MSTabularTree<Element>::replaceAt(cursor_,element_);
  changed(cursor_,position(cursor_),MSObservableTreeAssign);
}

template <class Element>
void MSObservableTree<Element>::replaceAt(MSTabularTreeCursor<Element> const& cursor_,MSObservableTree<Element>& tree_)
{
  if (&tree_!=this)
   {
     if (isRoot(cursor_)) copy(tree_);
     else
      {
	unsigned long pos=position(cursor_);
	MSTabularTreeCursor<Element> cursor=cursor_;
	cursor.setToParent();
	MSTabularTree<Element>::removeSubtree(cursor_);
	MSTabularTree<Element>::attachAsChild(cursor,pos,tree_);
	changed(cursor,pos,MSObservableTreeReplace);
      }
   }
}

template <class Element>
void MSObservableTree<Element>::permuteChildren(MSTabularTreeCursor<Element> const& cursor_,
                                                const MSIndexVector& index_)
{
  MSTabularTree<Element>::permuteChildren(cursor_,index_);
  if (receiverList()!=0)
   { 
     MSObservableTreeEvent<Element> e(cursor_,index_,MSObservableTreePermute);
     sendEvent(e);
   }
}


template <class Element>
void MSObservableTree<Element>::elementChanged(MSTabularTreeCursor<Element> const& cursor_)
{
  changed(cursor_,position(cursor_),MSObservableTreeAssign);
}

template <class Element>
const MSSymbol& MSObservableTree<Element>::symbol(void)
{
  static MSSymbol aSymbol("MSObservableTree<Element>");
  return aSymbol;
}

template <class Element>
MSString MSObservableTree<Element>::asString(void) const
{ return MSString();}

template <class Element>
MSString MSObservableTree<Element>::asDebugInfo(void) const
{ return MSString();}

template <class Element>
MSString MSObservableTree<Element>::asMSF(void) const
{ return MSString();}

template <class Element>
MSString MSObservableTree<Element>::className(void) const
{ return MSString("MSObservableTree"); }

template <class Element>
const MSSymbol& MSObservableTree<Element>::type(void) const
{ return symbol(); }

template <class Element>
MSError::ErrorStatus MSObservableTree<Element>::set(const char *)
{ return MSError::MSSuccess; }

template <class Element>
MSError::ErrorStatus MSObservableTree<Element>::setFromMSF(const char *)
{ return MSError::MSSuccess; }

template <class Element>
MSModel *MSObservableTree<Element>::clone(void) const
{ return new MSObservableTree<Element>(*this); }

#endif
