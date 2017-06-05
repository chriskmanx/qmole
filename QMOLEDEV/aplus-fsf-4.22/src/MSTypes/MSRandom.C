///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <sys/types.h>
#include <sys/time.h>
#include <MSTypes/MSRandom.H>
#include <MSTypes/MSMutex.H>

#if defined(MS_MULTI_THREAD)
static MSMutex defaultGenMutex;
#endif //MS_MULTI_THREAD

MSRandom::MSRandom(void) : _useDefaultGen(MSTrue) 	// dispatch to default generator
{
}
  

MSRandom::MSRandom(unsigned long seed_) : _useDefaultGen(MSFalse) 	// use private generator
{
  if (seed_==0)
    {
      struct timeval now;
      gettimeofday(&now,(struct timezone *)(0));
      seed_ = now.tv_sec;
    }

  _seed = seed_;
}

MSRandom::~MSRandom(void) {}

void MSRandom::seed(unsigned long seed_)
{
  // switch to private generator
  _useDefaultGen = MSFalse;
  _seed = seed_;
}


unsigned long MSRandom::random(unsigned long limit_) 
{
  if (_useDefaultGen==MSTrue)	// use the global generator
    {
      MSGUARD(defaultGenMutex);
      return defaultGen().random(limit_);
    }
  else	// use own generator
    {
      _seed=_seed*5709421UL+1UL;
      return ((_seed>>16)%limit_);
    }
}


MSRandom& MSRandom::defaultGen(void)
{
  // initialize default generator with a seed of 0, which means that
  // gettimeofday() will be used as the seed
  static MSRandom rn(0);
  return rn;
}
