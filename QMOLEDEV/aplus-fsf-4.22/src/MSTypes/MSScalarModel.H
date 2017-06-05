#ifndef MSScalarModelHEADER
#define MSScalarModelHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSModel.H>
#include <MSTypes/MSNullEvent.H>

class MSTypesExport MSScalarModel : public MSModel
{
public:
  MSScalarModel(void) {}
  virtual ~MSScalarModel(void);

protected:
  void changed(void);
  void sendNullEvent(void);
};

inline void MSScalarModel::sendNullEvent(void)
{
  MSNullEvent aEvent;
  sendEvent((MSEvent&)aEvent);
}

inline void MSScalarModel::changed(void)
{ if (receiverList()!=0) sendNullEvent(); }

#ifndef MSStringHEADER
#include <MSTypes/MSString.H>
#endif

#endif









