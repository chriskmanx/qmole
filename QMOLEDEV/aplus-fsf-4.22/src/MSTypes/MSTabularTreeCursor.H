#ifndef MSTabularTreeCursorHEADER
#define MSTabularTreeCursorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSTabularTreeDefs.H>

template <class Element>
class MSTabularTree;

template <class Element>
class MSTabularTreeNode;

template <class Element>
class MSTabularTreeCursor
{
protected:
  MSTabularTree<Element> const *_pTabularTree;
  MSTabularTreeNode<Element>   *_pNode;

friend class MSTabularTree<Element>;

  MSBoolean isFor(MSTabularTree<Element> const& c) const
  { return MSBoolean(_pTabularTree==&c); }
public:
  MSTabularTreeCursor(MSTabularTree<Element> const& collection) :
      _pTabularTree(&collection),_pNode(0)
  {};

  MSTabularTreeCursor() :
      _pTabularTree(0),_pNode(0)
  {};

  MSBoolean setToRoot();
  MSBoolean setToChild(unsigned long);
  MSBoolean setToParent();
  MSBoolean setToFirstExistingChild();
  MSBoolean setToNextExistingChild();
  MSBoolean setToLastExistingChild();
  MSBoolean setToPreviousExistingChild();
  MSBoolean isValid() const;
  void invalidate();

  Element const& element() const
  { return _pTabularTree->elementAt(*this); }

  MSBoolean operator==(MSTabularTreeCursor<Element> const& cursor) const
  { return (_pTabularTree==cursor._pTabularTree&&_pNode==cursor._pNode)?MSTrue:MSFalse; }
  MSBoolean operator!=(MSTabularTreeCursor<Element> const& cursor) const
  { return (_pTabularTree!=cursor._pTabularTree||_pNode!=cursor._pNode)?MSTrue:MSFalse; }
};

#endif
