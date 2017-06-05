///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSVector.H>
#include <MSTypes/MSIndexVector.H>

MSVector::MSVector()  {}

MSVector::~MSVector() {}

unsigned int MSVector::getLength() const
{ return 0; }

MSIndexVector MSVector::gradeUp(void) const
{ return MSIndexVector(); }
MSIndexVector MSVector::gradeDown(void) const
{ return MSIndexVector(); }

void MSVector::permute(const MSIndexVector&)
{}

void MSVector::processAppendUpdate (unsigned length_, unsigned numAppended_)
{
  // optimize for length 1 append, most common case
  if (numAppended_==1) changed(length_-1);  
  else
   {
     MSIndexVector index(numAppended_);
     changed(index.series(numAppended_,length_-numAppended_));     
   }
}


