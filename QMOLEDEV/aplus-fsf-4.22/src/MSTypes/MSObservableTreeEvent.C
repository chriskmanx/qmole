#ifndef MSObservableTreeEventIMPLEMENTATION
#define MSObservableTreeEventIMPLEMENTATION

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSObservableTreeEvent.H>

template <class Element>
MSObservableTreeEvent<Element>::~MSObservableTreeEvent(void)
{}

template <class Element>
const MSSymbol& MSObservableTreeEvent<Element>::symbol(void)
{
  static MSSymbol aSymbol("MSObservableTreeEvent");
  return aSymbol;
}

template <class Element>
MSObservableTreeEvent<Element>& MSObservableTreeEvent<Element>::operator=(const MSObservableTreeEvent<Element>& aEvent_)
{
  if (this!=&aEvent_)
   {
     _cursor=aEvent_._cursor;
     _position=aEvent_._position;
     _index=aEvent_._index;
     sender(aEvent_.sender());
   }
  return *this;
}

#endif
