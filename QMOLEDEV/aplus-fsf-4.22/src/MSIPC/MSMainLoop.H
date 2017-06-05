#ifndef MSMainLoopHEADER
#define MSMainLoopHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSEnum.H>

class MSIPCExport MSMainLoop
{
public:
  MSMainLoop(void);
  virtual ~MSMainLoop(void);

  virtual void loop(void);
  virtual void flushAndProcess(MSBoolean blocking_=MSFalse);
  
  static void continueLoop(MSBoolean continueLoop_);
  static MSBoolean continueLoop(void);
  static void quit(void);
  
protected:  
  virtual MSBoolean processTimers(void);  
  virtual MSBoolean flush(void);
  virtual void selectAndProcess(void);
  virtual void outerLoop(void);
  virtual void innerLoop(MSBoolean blocking_=MSTrue);
  virtual void innerLoopFlush(MSBoolean blocking_);  
  virtual void userProcessing(void);
  virtual void terminateLoop(void);

  static MSBoolean zeroTimeOut(void);
  static void zeroTimeOut(MSBoolean zeroTimeOut_);

private:
  static MSBoolean  _zeroTimeOut;
  static MSBoolean  _continueLoop;
};

#endif
