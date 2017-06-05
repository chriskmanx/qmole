#ifndef MSTabularTreeCursorIMPLEMENTATION
#define MSTabularTreeCursorIMPLEMENTATION

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSTabularTreeCursor.H>

// *****************************************************************************
// MSTabularTreeCursor<Element implementation
// *****************************************************************************
template <class Element>
MSBoolean MSTabularTreeCursor<Element>::setToRoot()
{
  return _pTabularTree->setToRoot(*this);
}

template <class Element>
MSBoolean MSTabularTreeCursor<Element>::setToChild(unsigned long position)
{
  return _pTabularTree->setToChild(position,*this);
}

template <class Element>
MSBoolean MSTabularTreeCursor<Element>::setToParent()
{
  return _pTabularTree->setToParent(*this);
}

template <class Element>
MSBoolean MSTabularTreeCursor<Element>::setToFirstExistingChild()
{
  return _pTabularTree->setToFirstExistingChild(*this);
}

template <class Element>
MSBoolean MSTabularTreeCursor<Element>::setToNextExistingChild()
{
  return _pTabularTree->setToNextExistingChild(*this);
}

template <class Element>
MSBoolean MSTabularTreeCursor<Element>::setToLastExistingChild()
{
  return _pTabularTree->setToLastExistingChild(*this);
}

template <class Element>
MSBoolean MSTabularTreeCursor<Element>::setToPreviousExistingChild()
{
  return _pTabularTree->setToPreviousExistingChild(*this);
}

template <class Element>
MSBoolean MSTabularTreeCursor<Element>::isValid() const
{
  return (_pNode!=0)?MSTrue:MSFalse;
}

template <class Element>
void MSTabularTreeCursor<Element>::invalidate()
{
  _pNode=0;
}

#endif
