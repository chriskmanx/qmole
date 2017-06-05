///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSIndexedModel.H>
#include <MSTypes/MSIndexVector.H>
#include <MSTypes/MSIndexedEvent.H>

void MSIndexedModel::sendIndexedEvent(const MSIndexVector& aIndexVector_) 
{
  MSIndexedEvent aEvent(aIndexVector_);
  sendEvent(aEvent);
}

void MSIndexedModel::sendIndexedEvent(unsigned int index_)
{
  MSIndexedEvent aEvent(index_);
  sendEvent(aEvent);
}


MSIndexedModel::MSIndexedModel()
{
}


MSIndexedModel::~MSIndexedModel()
{
}


const MSIndexVector& MSIndexedModel::nullIndexVector()
{ return MSIndexVector::nullVector(); }
