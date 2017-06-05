#ifndef __TimrConnection_H
#define __TimrConnection_H

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <AipcService.H>

#include <MSIPC/MSAbsoluteTimer.H>
#include <MSIPC/MSRegularTimer.H>

#define TIMR_ABSOLUTE       0x01

class TimrConnection : public AipcService
{
 public:
  static I ServiceType;

  enum OnExpire{Destroy=0, Restart, Hold, Interval};

 protected:
  static A SetableAttrs;
  static A NonsetableAttrs;
  static S OnExpireSymbols[10];
  static MSBoolean StaticsInitialized;
  
 protected:
  MSTimer *_timer;
  A _aExpiry;
  int _secs;
  int _usecs;
  int _flags;
  S _eventSymbol;
  A _aEventData;
  OnExpire _onExpire;

 public:
  // TimrConnection Constructors and Destructor
  TimrConnection(A aTimeout_, A cbfunc_);
  virtual ~TimrConnection(void);

 private:
  void init(A aTimeout_);
  void initStatics(void);

 protected:
  void goOff(void);

  // M:Flags
  MSBoolean isPending(void){return (_timer==0)?MSFalse:MSTrue;}

  void turnAbsoluteOn(void){_flags |= TIMR_ABSOLUTE;}
  void turnAbsoluteOff(void){_flags &= ~TIMR_ABSOLUTE;}
  MSBoolean isAbsolute(void){return (_flags&TIMR_ABSOLUTE)?MSTrue:MSFalse;}

  MSBoolean setOnExpire(A aOnExpire);
  A getOnExpire(void);
  MSBoolean setExpiry(A newExpiry_);
  A getExpiry(void){return(A)ic(_aExpiry);}
  MSBoolean setEventSymbol(A newEventSymbol_);
  A getEventSymbol(void);
  MSBoolean setEventData(A newEventData_);
  A getEventData(void){return(A)ic(_aEventData);}

 public:
  void close(void);
  void open(void);

  // M: from AipcAttributes;
 protected:
  A timrSetableAttrs(void){return SetableAttrs;}
  A timrNonsetableAttrs(void){return NonsetableAttrs;}
  int timrSetAttrIndex(C *attr_);
  int timrNonsetAttrIndex(C *attr_);

  // M:Attribute interface;
 public:
  A setableAttrlist(void);
  A getableAttrlist(void);
  MSBoolean setAttr(C *attr_,A aobj);
  A getAttr(C *attr_);

  // M:Other methods
  virtual I serviceType(void) {return TimrConnection::ServiceType;}
  virtual const C *protocol(void){ return _eventSymbol->n;}

};

#endif

