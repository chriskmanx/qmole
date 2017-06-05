#ifndef VaraibleHEADER
#define VaraibleHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSString.H>
#include <MSTypes/MSModel.H>

class MSTypesExport MSVariable
{
public:  
  MSModel *_pModel;
  MSString _name;

  MSVariable(void) :
  _pModel(0) {}
  MSVariable(const MSVariable& aVariable_) :
  _pModel(aVariable_._pModel),_name(aVariable_._name) {}
  MSVariable(MSModel *pModel_,const char *name_) :
  _pModel(pModel_),_name(name_) {}

  MSVariable& operator=(const MSVariable& aVariable_)
  {
    if (this!=&aVariable_) _pModel=aVariable_._pModel,_name=aVariable_._name;
    return *this;
  }
};

inline const MSString& key(const MSVariable& aVariable_)
{ return aVariable_._name; }

#endif
