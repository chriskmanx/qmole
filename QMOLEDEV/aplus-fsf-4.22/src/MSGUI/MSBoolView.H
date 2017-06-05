#ifndef MSBoolViewHEADER
#define MSBoolViewHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 199-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSEventReceiver.H>
#include <MSTypes/MSBool.H>

class MSBoolView : public MSEventReceiver
{
public:
  MSBoolView(void);
  virtual ~MSBoolView(void);
  void boolModel(MSBool &);
  MSBool &boolModel(void);

protected:
  MSBool  _internalBoolModel;
  MSBool *_boolModel;

  virtual void receiveEvent(MSEvent&);
  virtual void removeSenderNotify(MSEventSender *);

  virtual void newBoolModelNotify(void)=0;
  virtual void boolModelChanged(MSEvent &)=0;
};


#endif
