#ifndef AplusUpdateQueueHEADER
#define AplusUpdateQueueHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <a/fncdcls.h>

inline void addToUpdateQueue(V,A,A,A,I);
inline void processUpdateQueue(void);

// FIFO queue

class AplusUpdate
{
private:
  V _aplusVar;
  A _data;
  A _index;
  A _pick;
  I _ravel;
 
public:
  AplusUpdate(V v_,A data_,A index_,A pick_,I ravel_);
  ~AplusUpdate(void);

  void send(void);
};

// forward declaration for MSUnsignedLongVector
template <class Type> class MSTypeVector;
typedef MSTypeVector<unsigned long> MSUnsignedLongVector;

class AplusUpdateQueue
{
protected:
  MSUnsignedLongVector _queue;

public:
  AplusUpdateQueue(void);
  ~AplusUpdateQueue(void);

  inline void push(AplusUpdate *update_);
  void process(void);
};

extern AplusUpdateQueue updateQueue;

inline void AplusUpdateQueue::push(AplusUpdate *pUpdate_)  { _queue.append((unsigned long)pUpdate_); }

inline void addToUpdateQueue(V v_,A data_,A index_,A pick_,I ravel_)
{ updateQueue.push(new AplusUpdate(v_,data_,index_,pick_,ravel_)); }

inline void processUpdateQueue(void)  { updateQueue.process(); }

#endif  //AplusUpdateQueueHEADER
