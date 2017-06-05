#ifndef __AipcAttributes_H
#define __AipcAttributes_H

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <a/k.h>
#include <a/fncdcls.h>
#include <a/arthur.h>
#include <dap/Warn.h>

#include <MSTypes/MSEnum.H>

// ACFLAG_ macros are individual bits within the _flagsAttr integer.
// Bits equal or below 0x80 are reserved for AipcConnection.  Bits
// equal or above 0x010000 may be defined and used by subclasses of this
// class.

#define ACFLAG_NODELAY     0x01
#define ACFLAG_READPAUSE   0x02
#define ACFLAG_WRITEPAUSE  0x04
#define ACFLAG_RETRY       0x08
#define ACFLAG_DEBUG       0x10

class AipcAttributes
{
 protected:
  static A SetableAttrs;
  static A NonsetableAttrs;

  A _clientData;
  int _flagAttrs;
  int _readPriority;
  int _writePriority;
  int _readBufsize;
  int _writeBufsize;
  int _listener;

  // private methods
  void init(void);
  
 public:

  // AipcAttributes (Service Control Block) Constructors
  AipcAttributes(void);
  AipcAttributes(AipcAttributes &src_);
  virtual ~AipcAttributes(void);

  // M:Flags
  void noDelay(MSBoolean b_){if(b_)_flagAttrs|=ACFLAG_NODELAY;
    else _flagAttrs&=~ACFLAG_NODELAY;}
  MSBoolean noDelay(void) {R (_flagAttrs&ACFLAG_NODELAY)?MSTrue:MSFalse;}

  void readPause(MSBoolean b_){if(b_)_flagAttrs|=ACFLAG_READPAUSE;
    else _flagAttrs&=~ACFLAG_READPAUSE;}
  MSBoolean readPause(void){R (_flagAttrs&ACFLAG_READPAUSE)?MSTrue:MSFalse;}
  
  void writePause(MSBoolean b_){if(b_)_flagAttrs|=ACFLAG_WRITEPAUSE;
    else _flagAttrs&=~ACFLAG_WRITEPAUSE;}
  MSBoolean writePause(void){R (_flagAttrs&ACFLAG_WRITEPAUSE)?MSTrue:MSFalse;}
  
  void retry(MSBoolean b_){if(b_)_flagAttrs|=ACFLAG_RETRY;
    else _flagAttrs&=~ACFLAG_RETRY;}
  MSBoolean retry(void){R (_flagAttrs&ACFLAG_RETRY)?MSTrue:MSFalse;}
  
  void debug(MSBoolean b_){if(b_)_flagAttrs|=ACFLAG_DEBUG;
    else _flagAttrs&=~ACFLAG_DEBUG;}
  MSBoolean debug(void){R (_flagAttrs&ACFLAG_DEBUG)?MSTrue:MSFalse;}
  
  // M:Other Attributes
  void readPriority(int val_){_readPriority=val_;}
  int readPriority(void){return _readPriority;}
  void writePriority(int val_){_writePriority=val_;}
  int writePriority(void){return _writePriority;}
  void readBufsize(int val_){_readBufsize=val_;}
  int readBufsize(void){return _readBufsize;}
  void writeBufsize(int val_){_writeBufsize=val_;}
  int writeBufsize(void){return _writeBufsize;}
  void clientData(A aobj_){dc(_clientData);_clientData=aobj_;}
  A clientData(void){return _clientData;}
  void listener(int val_){_listener=val_;}
  int listener(void){return _listener;}

  // M:Attribute interface
  A setableAttrs(void){R SetableAttrs;}
  A nonsetableAttrs(void){R NonsetableAttrs;}
  int setAttrIndex(C *attr_);
  int nonsetAttrIndex(C *attr_);
};


#endif


