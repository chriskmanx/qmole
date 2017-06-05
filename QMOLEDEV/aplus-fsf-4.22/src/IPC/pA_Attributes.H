#ifndef __pA_Attributes_H
#define __pA_Attributes_H

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <a/k.h>
#include <a/fncdcls.h>
#include <dap/Warn.h>

#include <MSTypes/MSEnum.H>

#define PAFLAG_BURSTMODE  0x01

class pA_Attributes
{
 protected:
  static A SetableAttrs;
  static A NonsetableAttrs;

  int _pA_flags;

  // private methods
  void init(void);
  
 public:

  // AipcAttributes (Service Control Block) Constructors
  pA_Attributes(void);

  // M:Flags
  void burstMode(MSBoolean b_){if(b_)_pA_flags|=PAFLAG_BURSTMODE;
    else _pA_flags&=~PAFLAG_BURSTMODE;}
  MSBoolean burstMode(void){
    return (_pA_flags&PAFLAG_BURSTMODE)?MSTrue:MSFalse;}
  
  
  // M:Other Attributes

  // M:Attribute interface
  A setableAttrs(void){return SetableAttrs;}
  A nonsetableAttrs(void){return NonsetableAttrs;}
  int setAttrIndex(C *attr_);
  int nonsetAttrIndex(C *attr_);
};


#endif


