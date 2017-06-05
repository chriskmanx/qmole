#ifndef MSModelHEADER
#define MSModelHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSEnum.H>
#include <MSTypes/MSSymbol.H>
#include <MSTypes/MSEventSender.H>
#include <MSTypes/MSEventReceiver.H>
#include <MSTypes/MSError.H>

class MSString;

class MSTypesExport MSModel : public MSEventSender
{
public:
  MSModel(void) {}
  MSModel(const MSModel&) {}
  ~MSModel(void);

  // this protocol needs to be implemented by each MSModel subclass that intends to
  // be instantiable
  virtual MSString asString(void) const=0;
  virtual MSString asDebugInfo(void) const=0;
  virtual MSString asMSF(void) const=0;
  virtual MSString className(void) const=0;
  virtual const MSSymbol& type(void) const=0;
  virtual MSError::ErrorStatus set(const char *)=0;
  virtual MSError::ErrorStatus setFromMSF(const char *)=0;
  virtual MSModel *clone(void) const=0;

  // these are additions to the MSModel protocol - they will become
  // pure virtual
  virtual MSModel *create(void) const;
  virtual void assign(const MSModel&); 
  virtual long compare(const MSModel&) const;

  static const MSSymbol& symbol(void);
};

#endif









