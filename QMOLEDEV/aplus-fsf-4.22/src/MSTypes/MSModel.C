///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSModel.H>
#include <MSTypes/MSString.H>

MSModel::~MSModel(void) {} 

const MSSymbol& MSModel::symbol(void)      
{ return MSSymbol::nullSymbol(); }

MSModel *MSModel::create(void) const
{ return (MSModel *)0; }

void MSModel::assign(const MSModel&)
{}

long MSModel::compare(const MSModel&) const
{ return 0; }


// The functions below, dbg_asString() and dbg_asDebugInfo() are provided for debugging purposes only.
// Their only usage is to be called from a debugger.  This provides a universal interface for accessing
// data of any MSModel subclass from any debugger (assuming that any debugger has the basic capability
// to call a global, non-overloaded function which returns a character string.
//
const char *dbg_asString(const MSModel& model_)
{
  static MSString buf;
  buf = model_.asString();
  return buf.string();
}

const char *dbg_asDebugInfo(const MSModel& model_)
{
  static MSString buf;
  buf = model_.asDebugInfo();
  return buf.string();
}
