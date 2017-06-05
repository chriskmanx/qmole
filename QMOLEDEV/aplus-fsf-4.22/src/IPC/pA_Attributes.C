///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
//
// pA_Attributes
//
// This class contains the setable attributes of an pA_Connection.
// It is a separate class to facilitate its enclosure in a pA_Listener,
// from which newly spawned connecitons can be initialized.
// 

#include <pA_Attributes.H>
#include <IPCUtilities.H>

// Static members

A pA_Attributes::SetableAttrs=(A)0;
A pA_Attributes::NonsetableAttrs=(A)0;

pA_Attributes::pA_Attributes(void)
{
  ipcWarn(0,"%t pA_Attributes::pA_Attributes\n");
  init();
}

void pA_Attributes::init(void)
{
  ipcWarn(0,"%t pA_Attributes::init\n");
  _pA_flags=0;
  if(0==SetableAttrs)
    SetableAttrs=gvi(Et,1,MS(si("burstMode")));
  if(0==NonsetableAttrs)
    NonsetableAttrs=gvi(Et,0);
}

// M:Attribute interface

int pA_Attributes::setAttrIndex(C *attr_)
{
  int idx;
  A attrs=setableAttrs();
  I attrsym=MS(si(attr_));
  for(idx=0;idx<attrs->n;++idx)if(attrsym==attrs->p[idx])break;
  return (idx==attrs->n)?-1:idx;
}

int pA_Attributes::nonsetAttrIndex(C *attr_)
{
  int idx;
  A attrs=nonsetableAttrs();
  I attrsym=MS(si(attr_));
  for(idx=0;idx<attrs->n;++idx)if(attrsym==attrs->p[idx])break;
  return (idx==attrs->n)?-1:idx;
}
