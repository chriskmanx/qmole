#ifndef MSObservableTreeEventHEADER
#define MSObservableTreeEventHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSSymbol.H>
#include <MSTypes/MSEvent.H>
#include <MSTypes/MSTabularTree.H>

enum MSObservableTreeEventType {MSObservableTreeInsert,MSObservableTreeDelete,
                                MSObservableTreeCopy,MSObservableTreeAssign,
                                MSObservableTreeReplace,MSObservableTreePermute };

template <class Element>
class MSObservableTreeEvent : public MSEvent
{
public:
  MSObservableTreeEvent(const MSObservableTreeEvent<Element> &aEvent_)
      :MSEvent(aEvent_),
       _position(aEvent_._position),
       _treeEventType(aEvent_._treeEventType),
       _cursor(aEvent_._cursor)
  {}

  MSObservableTreeEvent(const MSTabularTreeCursor<Element> &cursor_,
                        unsigned long position_,MSObservableTreeEventType treeEventType_)
      :MSEvent(symbol()),
       _position(position_),
       _cursor(cursor_),
       _treeEventType(treeEventType_)
  {}

  MSObservableTreeEvent(const MSTabularTreeCursor<Element> &cursor_,
                        const MSIndexVector& index_,MSObservableTreeEventType treeEventType_)
      :MSEvent(symbol()),
       _position(0),
       _index(index_),
       _cursor(cursor_),
       _treeEventType(treeEventType_)
  {}

  ~MSObservableTreeEvent(void);

  MSObservableTreeEvent<Element>& operator=(const MSObservableTreeEvent<Element>&);

  const MSTabularTreeCursor<Element>& cursor(void) const {return _cursor;}
  unsigned long position(void)                     const {return _position;}
  MSObservableTreeEventType treeEventType(void)    const {return _treeEventType;}
  const MSIndexVector& index(void)                 const {return _index; }
    
  static const MSSymbol& symbol(void);

protected:
  unsigned long                _position;
  MSTabularTreeCursor<Element> _cursor;
  MSObservableTreeEventType    _treeEventType;
  MSIndexVector                _index;
};

#endif
