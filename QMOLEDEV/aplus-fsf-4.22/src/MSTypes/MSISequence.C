#ifndef MSISequenceIMPLEMENTATION
#define MSISequenceIMPLEMENTATION

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997 -2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSISequence.H>

template < class Element >
MSISequence <Element>::MSISequence (unsigned long)
{
  ivFirst = 0;
  ivLast = 0;
  ivNumberOfElements = 0;
}

template < class Element >
MSISequence <Element>::MSISequence (const MSISequence <Element>& sequence)
{
  ivFirst = 0;
  ivLast = 0;
  ivNumberOfElements = 0;
  addAllFrom (sequence);
}

template < class Element >
MSISequence <Element>::~MSISequence ()
{
  removeAll ();
}

template <class Element>
MSISequence <Element>& MSISequence <Element>::operator=(MSISequence < Element> const& sequence)
{
  if (&sequence != this)
   {
    removeAll ();
    addAllFrom (sequence);
  }
  return *this;
}

template < class Element >
void MSISequence <Element>::addAllFrom (MSISequence < Element> const& sequence)
{
  MSICollectionCheck( (this!=&sequence),"identical collection")
    for (Node *node = sequence.ivFirst; node != 0; node = node->ivNext)
   {
     addAsLast (ivOps.newNode (node));
   }
}

template < class Element > 
MSBoolean MSISequence <Element>::setToFirst (MSISequence <Element>::Node*& node) const
{
  node = ivFirst;
  return MSBoolean(node!=0);
}

template < class Element > 
MSBoolean MSISequence <Element>::setToNext (MSISequence <Element>::Node*& node) const
{
  node = node->ivNext;
  return MSBoolean(node != 0);
}

template < class Element > 
MSBoolean MSISequence <Element>::setToLast (MSISequence <Element>::Node*& node) const
{
  node = ivLast;
  return MSBoolean(node != 0);
}

template < class Element > 
MSBoolean MSISequence <Element>::setToPrevious (MSISequence <Element>::Node*& node) const
{
  node = node->ivPrevious;
  return MSBoolean(node != 0);
}

template < class Element > 
unsigned long MSISequence < Element>::
removeAll (MSBoolean (*predicate) (Element const&, void*), void* env)
{
  Node *current, *next;
  unsigned long removed = 0;
  for (setToFirst (current), setToFirst (next); current != 0; current = next)
   {
    setToNext (next);
    if (ivOps.constantFunctionIteration((void*)predicate, env, current))
     {
      removeAt (current);
      removed++;
     }
  }
  return removed;
}


template <class Element>
void MSISequence <Element>::removeAll ()
{
  Node *node2;
  for (Node *node1 = ivFirst; node1 != 0; node1 = node2)
   {
     node2 = node1->ivNext;
     ivOps.deleteNode (node1);
   }
  
  ivFirst = 0;
  ivLast = 0;
  ivNumberOfElements = 0;
}

template <class Element>
MSBoolean MSISequence <Element>::
allElementsDo (MSBoolean (*function) (Element&, void*), void* env)
{
  for (Node *node = ivFirst; node != 0; node = node->ivNext)
   {
     if (! ivOps.functionIteration ((void*)function, env, node)) return MSFalse;
  }
  return MSTrue;
}

template <class Element>
inline MSBoolean MSISequence <Element>::
allElementsDo (MSBoolean (*function) (Element const&, void*),void* env) const
{
  for (Node *node = ivFirst; node != 0; node = node->ivNext)
   {
     if ( !ivOps.constantFunctionIteration ((void *)function, env, node)) return MSFalse;
   }
  return MSTrue;
}

template <class Element>
inline MSBoolean MSISequence <Element>::isConsistent () const
{
  unsigned long nrOfElm = 0;
  if (ivFirst != 0 && ivFirst->ivPrevious != 0) return MSFalse;
  for (Node *n = ivFirst; n != 0; n = n->ivNext)
   {
     if (n->ivNext != 0 && n->ivNext->ivPrevious != n)
      return MSFalse;
     nrOfElm++;
   }
  return MSBoolean(nrOfElm==ivNumberOfElements);
}

template <class Element>
void MSISequence <Element>::removeFirst ()
{
  checkNotEmpty ();
  Node* node;
  setToFirst (node);
  removeAt (node);
}

template <class Element>
void MSISequence <Element>::removeLast ()
{
  checkNotEmpty ();
  Node* node;
  setToLast (node);
  removeAt (node);
}

template <class Element>
void MSISequence<Element>::removeAt (MSISequence<Element>::Node* node)
{
  if (node == ivFirst) ivFirst = node->ivNext;
  if (node == ivLast)  ivLast = node->ivPrevious;

  if (node->ivNext != 0) node->ivNext->ivPrevious = node->ivPrevious;
  if (node->ivPrevious != 0) node->ivPrevious->ivNext = node->ivNext;

  ivOps.deleteNode (node);
  ivNumberOfElements--;
}

template <class Element>
void MSISequence <Element>::removeAtPosition (unsigned long position)
{
  checkPositionExists (position);
  Node* node;
  setToPosition (position, node);
  removeAt (node);
}

template <class Element>
void MSISequence<Element>::setToPosition (unsigned long position, MSISequence<Element>::Node*& node) const
{
  setToFirst (node);
  for (int i = 1; i < position; i++)
   {
    node = node->ivNext;
   }
}

template <class Element>
MSISequence<Element>::Node* MSISequence<Element>::addAsFirst (MSISequence<Element>::Node* newNode)
{
  return addAsNext (0, newNode);
}

template <class Element>
MSISequence<Element>::Node* MSISequence<Element>::addAsLast (MSISequence<Element>::Node* newNode)
{
  return addAsNext (ivLast, newNode);
}

template <class Element>
MSISequence<Element>::Node* MSISequence<Element>::addAsNext (MSISequence<Element>::Node* node,
                                                             MSISequence<Element>::Node* newNode)
{
  if (node == 0) newNode->ivNext = ivFirst;
  else newNode->ivNext = node->ivNext;

  newNode->ivPrevious = node;

  if (newNode->ivNext != 0) newNode->ivNext->ivPrevious = newNode;

  if (newNode->ivPrevious != 0) newNode->ivPrevious->ivNext = newNode;

  if (node == 0) ivFirst = newNode;

  if (node == ivLast) ivLast = newNode;

  ivNumberOfElements++;

  return newNode;
}

template <class Element>
MSISequence<Element>::Node* MSISequence <Element>::addAsPrevious (MSISequence<Element>::Node* node,
                                                                  MSISequence<Element>::Node* newNode)
{
  return addAsNext (node->ivPrevious, newNode);
}

template <class Element>
MSISequence<Element>::Node* MSISequence <Element>::addAtPosition (unsigned long position,
                                                                  MSISequence<Element>::Node* newNode)
{
  Node *node;
  if (position == 1) node = 0;
  else
   {
     node = ivFirst;
     for (int i = 2; i < position; i++)
      {
        node = node->ivNext;
      }
   }
  return addAsNext (node, newNode);
}

template <class Element>
void MSISequence <Element>::sort (long (*comparisonFunction) (Element const&, Element const&))
{
  Node *current, *next, *last;

  for (last = ivLast; last != ivFirst; last = last->ivPrevious)
   {
    for (current = ivFirst; current != last; )
     {
      next = current->ivNext;
      if (0 < ivOps.functionComparison ((void *)comparisonFunction,
				         current, current->ivNext)) {
	// exchange next and current
	current->ivNext = next->ivNext;
	next->ivPrevious = current->ivPrevious;
	if (current->ivNext != 0) current->ivNext->ivPrevious = current;
	if (next->ivPrevious != 0) next->ivPrevious->ivNext = next;
	current->ivPrevious = next;
	next->ivNext = current;
	if (next == last)
         {
	  last = current;
	  if (next == ivLast)
           {
	    ivLast = current;
           }
         }
	if (current == ivFirst)
         {
           ivFirst = next;
         }
      }
      else
       {
	current = next;
       }
     }
    // maximum elements sorted in last .. ivLast
   }
}

#endif
