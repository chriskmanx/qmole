///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/AplusCallback.H>

AplusCallback::AplusCallback(AClientData *ac_)
{ _ac = ac_; }
AplusCallback::~AplusCallback(void)
{ delete _ac;}

void AplusCallback::process(void)
{
//  MSDeleteQueue::allowDelete(MSFalse);

  if(AScbTraceHook::function()) 
    {
      AScbTraceHook::run(ac()->function(),(I)ac()->data(),0,0,0,ac()->aplusVar());
    }

  A r=af4(ac()->function(),(I)ac()->data(),0,0,0,ac()->aplusVar());
  if (r==0) showError(qs);
  else dc(r);
//  MSDeleteQueue::allowDelete(MSTrue);
}
