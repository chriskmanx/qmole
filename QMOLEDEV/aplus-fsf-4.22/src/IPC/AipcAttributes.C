///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
//
// AipcAttributes
//
// This class contains the setable attributes of an AipcConnection.
// It is a separate calss to facilitate its enclosure in a AipcListener,
// from which newly spawned connecitons can be initialized.
// 

#include <AipcAttributes.H>
#include <IPCUtilities.H>

// Static members

A AipcAttributes::SetableAttrs=(A)0;
A AipcAttributes::NonsetableAttrs=(A)0;

AipcAttributes::AipcAttributes(void)
{
  ipcWarn(0,"%t AipcAttributes::AipcAttributes\n");
  init();
}

AipcAttributes::AipcAttributes(AipcAttributes &src_)
  : _clientData((A)ic(src_.clientData())),
    _flagAttrs(src_._flagAttrs),
    _readPriority(src_._readPriority),
    _writePriority(src_._writePriority),
    _readBufsize(src_._readBufsize),
    _writeBufsize(src_._writeBufsize),
    _listener(src_._listener)
{}

AipcAttributes::~AipcAttributes(void){dc(_clientData);}

void AipcAttributes::init(void)
{
  ipcWarn(0,"%t AipcAttributes::init\n");
  _flagAttrs=_readPriority=_writePriority=_readBufsize=_writeBufsize=
    _listener=0;
  _clientData=(A)0;
  retry(MSTrue);
  if(0==SetableAttrs)
    SetableAttrs=gvi(Et,10,MS(si("noDelay")),
		     MS(si("readPause")),MS(si("writePause")),
		     MS(si("readPriority")),MS(si("writePriority")),
		     MS(si("readBufsize")),MS(si("writeBufsize")),
		     MS(si("retry")),MS(si("clientData")),MS(si("debug")));
  if(0==NonsetableAttrs)
    NonsetableAttrs=gvi(Et,5,MS(si("fd")),MS(si("port")),
			MS(si("writeStatus")),MS(si("readStatus")),
			MS(si("listener")));
}

// M:Attribute interface

int AipcAttributes::setAttrIndex(C *attr_)
{
  int idx;
  A attrs=setableAttrs();
  I attrsym=MS(si(attr_));
  for(idx=0;idx<attrs->n;++idx)if(attrsym==attrs->p[idx])break;
  return (idx==attrs->n)?-1:idx;
}

int AipcAttributes::nonsetAttrIndex(C *attr_)
{
  int idx;
  A attrs=nonsetableAttrs();
  I attrsym=MS(si(attr_));
  for(idx=0;idx<attrs->n;++idx)if(attrsym==attrs->p[idx])break;
  return (idx==attrs->n)?-1:idx;
}









