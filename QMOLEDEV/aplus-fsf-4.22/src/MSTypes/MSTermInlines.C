#ifndef MSTermINLINES
#define MSTermINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif


INLINELINKAGE MSTerm MSTerm::operator+(const MSTerm& term_) const
{ return MSTerm(*this,term_); }

INLINELINKAGE MSBoolean MSTerm::operator==(const MSTerm &term_) const
{ return MSBoolean(_years==term_._years&&_months==term_._months&&_days==term_._days); }

INLINELINKAGE MSBoolean MSTerm::operator!=(const MSTerm &term_) const 
{ return MSBoolean(_years!=term_._years||_months!=term_._months||_days!=term_._days); }

INLINELINKAGE MSBoolean MSTerm::operator<(const MSTerm &term_) const
{ return MSBoolean(compare(term_)==-1); }
INLINELINKAGE MSBoolean MSTerm::operator<=(const MSTerm &term_) const
{ return MSBoolean(compare(term_)<=0); }
INLINELINKAGE MSBoolean MSTerm::operator>(const MSTerm &term_) const
{ return MSBoolean(compare(term_)==1); }
INLINELINKAGE MSBoolean MSTerm::operator>=(const MSTerm &term_) const
{ return MSBoolean(compare(term_)>=0); }

#endif
