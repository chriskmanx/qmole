#ifndef MSRandomHEADER
#define MSRandomHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


// The MSRandom class defines a psedo-random number generator
// that uses the linear congruential method (rand,srand).

#include <stdlib.h>
#include <MSTypes/MSEnum.H>

// Most applications should NOT have to create their own random number generators.
// Instead, they should use the default generator, accessible via MSRandom::defaultGen()
// method.  To preserve backward compatibility with MStk version <=2.4, the old MSRandom
// interface, where the application would create an MSRandom object whenever it needed
// to generate random numbers, is still supported.  However, under the covers this will
// dispatch everything to the global (default) generator, unless the application sets
// the seed explicitly (either through the constructor or through seed() method), in
// which case the MSRandom object will switch to its private generator.

class MSTypesExport MSRandom
{
protected:
  unsigned long _seed;
  MSBoolean     _useDefaultGen;

public:
  MSRandom(void);
  MSRandom(unsigned long seed_);
  ~MSRandom(void);

  inline unsigned long seed(void) const;
  void seed(unsigned long seed_);

  // iterator - get next random number between 0 and limit-1
  unsigned long random(unsigned long limit_);
  inline unsigned long operator() (unsigned long limit_);

  static MSRandom & defaultGen(void);
};

inline unsigned long MSRandom::seed(void) const
{ return (_useDefaultGen==MSTrue) ? defaultGen().seed() : _seed; }

inline unsigned long MSRandom::operator() (unsigned long limit_)  { return random(limit_); }

#endif  //MSRandomHEADER
