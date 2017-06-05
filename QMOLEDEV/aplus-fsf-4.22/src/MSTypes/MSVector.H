#ifndef MSVectorHEADER
#define MSVectorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <assert.h>
#include <MSTypes/MSEnum.H>
#include <MSTypes/MSIndexedModel.H>
#include <MSTypes/MSString.H>

#if HAVE_IOSTREAM
#include <iostream>
// #include <fstream>
using namespace std;
#else
class ostream;
#endif

class MSIndexVector;  // indexing vector

//const unsigned MSVectorDefaultMinimumAllocationSize=8;

class MSTypesExport MSVector : public MSIndexedModel 
{
protected:
  void processAppendUpdate(unsigned int, unsigned int);
  void appendUpdate(unsigned int, unsigned int);
  
  virtual unsigned int getLength() const;

public:
  MSVector();
  virtual ~MSVector();
  
  inline unsigned int length() const;
  inline unsigned rows(void)   const;
  
  virtual MSIndexVector gradeUp(void) const;
  virtual MSIndexVector gradeDown(void) const;
  virtual void permute(const MSIndexVector&);
};

inline void MSVector::appendUpdate (unsigned int oldLength_, unsigned int numAppended_)
{ if (doChanged()==MSTrue) processAppendUpdate(oldLength_,numAppended_); }

inline unsigned int MSVector::length() const
{ return getLength(); }

inline unsigned MSVector::rows() const
{ return length(); }

#endif  //MSVectorHEADER

