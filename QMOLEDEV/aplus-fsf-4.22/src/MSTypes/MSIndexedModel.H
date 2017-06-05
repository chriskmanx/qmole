#ifndef MSIndexedModelHEADER
#define MSIndexedModelHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSModel.H>

class MSIndexVector; // indexing vector for updates

class MSTypesExport MSIndexedModel : public MSModel
{
public:
  MSIndexedModel();
  virtual ~MSIndexedModel();

protected:
  static const MSIndexVector& nullIndexVector(void);
  MSBoolean doChanged(void);
  void changed(const MSIndexVector& indices_=MSIndexedModel::nullIndexVector());
  void changed(unsigned int index_);
  void sendIndexedEvent(const MSIndexVector& indices_=MSIndexedModel::nullIndexVector());
  void sendIndexedEvent(unsigned int index_);
};

inline MSBoolean MSIndexedModel::doChanged(void) 
{ return MSBoolean(receiverList()!=0); }

inline void MSIndexedModel::changed(const MSIndexVector& aIndexVector_) 
{ if (receiverList()!=0) sendIndexedEvent(aIndexVector_); }

inline void MSIndexedModel::changed(unsigned int index_)
{ if (receiverList()!=0) sendIndexedEvent(index_); }

#endif









