#ifndef MSObservableTreeHEADER
#define MSObservableTreeHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSModel.H>
#include <MSTypes/MSString.H>
#include <MSTypes/MSTabularTree.H>
#include <MSTypes/MSObservableTreeEvent.H>

template <class Element>
class MSObservableTree : public MSModel, public MSTabularTree<Element>
{
public:
  typedef MSTabularTreeCursor<Element> Cursor;
  typedef MSObservableTreeEvent<Element> TreeEvent;
  
  MSObservableTree(void);
  MSObservableTree(const MSObservableTree<Element>&);
  ~MSObservableTree(void);

  MSObservableTree<Element>& operator=(const MSObservableTree<Element>&);
  
  static const MSSymbol& symbol(void);

  void elementChanged(const MSTabularTreeCursor<Element>&);

  //The following are the same API's as MSTabularTree 
  void addAsChild(const MSTabularTreeCursor<Element>&,unsigned long,const Element&);
  void addAsRoot(const Element&);
  void attachAsChild(const MSTabularTreeCursor<Element>&,unsigned long,MSObservableTree<Element>&);
  void attachSubtreeAsChild(const MSTabularTreeCursor<Element>&,unsigned long,
                            MSObservableTree<Element>&,const MSTabularTreeCursor<Element>&);
  void attachAsRoot(MSObservableTree<Element>&);
  void attachSubtreeAsRoot(MSObservableTree<Element>&,const MSTabularTreeCursor<Element>&);
  void copy(const MSTabularTree<Element>&);
  void copySubtree(const MSTabularTree<Element>&,const MSTabularTreeCursor<Element>&);
  void removeAll(void);
  void removeSubtree(const MSTabularTreeCursor<Element>&);
  void replaceAt(const MSTabularTreeCursor<Element>&,const Element&);
  void replaceAt(const MSTabularTreeCursor<Element>&,MSObservableTree<Element>&);

  void permuteChildren(MSTabularTreeCursor<Element> const&, const MSIndexVector&);
  
  //Fill in the pure virtuals from MSModel
  virtual MSString asString(void) const;
  virtual MSString asDebugInfo(void) const;
  virtual MSString asMSF(void) const;
  virtual MSString className(void) const;
  virtual const MSSymbol& type(void) const;
  virtual MSError::ErrorStatus set(const char *);
  virtual MSError::ErrorStatus setFromMSF(const char *);
  virtual MSModel *clone(void) const;
  
protected:
  void changed(void);
  void changed(const MSTabularTreeCursor<Element>&,unsigned long,MSObservableTreeEventType);
};

#endif
