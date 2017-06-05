///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <MSTypes/MSUnsignedLongVector.H>
#include <AplusGUI/AplusCommon.H>
#include <AplusGUI/AplusUpdateQueue.H>

AplusUpdateQueue updateQueue;

AplusUpdate::AplusUpdate(V v_,A data_,A index_,A pick_,I ravel_)
{
  _aplusVar=v_;
  _data=(data_!=0)?(A)ic(data_):data_;   
  _index=(index_!=0)?(A)ic(index_):index_;   
  _pick=(pick_!=0)?(A)ic(pick_):pick_;   
  _ravel=ravel_;   
}

AplusUpdate::~AplusUpdate(void)
{
  if (_data!=0)  dc(_data);
  if (_index!=0) dc(_index);
  if (_pick!=0)  dc(_pick);  
}

void AplusUpdate::send(void)
{
  AVariableData *varData=pAVarDataFromV(_aplusVar);
  if (varData!=0&&varData->pWidgetView()!=0) 
    {
      AplusEvent ae(_index, _pick, _ravel);
      // we can call receiveEvent() on the widget-view directly instead of
      // calling sendEvent() on the model, but for that we need to
      // cast the widget-view to MSEventReceiver, since MSView::receiveEvent()
      // is protected
      ((MSEventReceiver *)varData->pWidgetView())->receiveEvent(ae);
    }
}  


AplusUpdateQueue::AplusUpdateQueue(void)
{
}


AplusUpdateQueue::~AplusUpdateQueue(void)
{
  unsigned int len=_queue.length();
  AplusUpdate *upd;

  for (int i=0; i<len; ++i)
    {
      upd = (AplusUpdate *)_queue(i);
      delete upd;
    }
}


void AplusUpdateQueue::process(void)
{
  unsigned int i=0, len=_queue.length();
  AplusUpdate *upd;
  //
  // _queue may change while this loop is running; additional elements may get pushed
  // onto the queue if updating involves any dependencies.  Therefore, we provide an
  // external "do-while" loop, on top of the simple iterating "for" loop, to process those
  // additional elements (if any)
  //
  do
    {
      for (; i<len; ++i)
	{
	  upd = (AplusUpdate *)_queue(i);
	  upd->send(); // send the update
	  delete upd;
	}

      len = _queue.length();	// get the new length to see if any elements have been added
    }
  while (i<len);	// i is now equal to the old length; compare it with the new one

  _queue.removeAll();	// clean up the queue
}
