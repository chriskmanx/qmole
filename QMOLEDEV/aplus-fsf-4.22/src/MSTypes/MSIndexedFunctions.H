#ifndef MSIndexedFunctionHEADER
#define MSIndexedFunctionHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSFloatVector.H>
#include <MSTypes/MSIntVector.H>

class MSTypesExport MSIndexedFunctions
{
public:
  enum Mode {Total=2,Minimum=4,Maximum=8,Average=16};
  MSIndexedFunctions(void);
  ~MSIndexedFunctions(void);

  static MSIndexedFunctions& indexedFuncs(void) {return _indexedFuncs; }
  const MSFloatVector& defaultWeights(void)     {return _defaultWeights;}
  MSIndexVector& defaultIndex(void)             {return _defaultIndex;}
  void defaultWeights(MSFloatVector& x_)        {_defaultWeights=x_;}
  void defaultIndex(MSIndexVector& x_)          {_defaultIndex=x_;}
  
  static double computeIndexedFunction(const MSFloatVector& model_,int mode_=MSIndexedFunctions::Total);
  static double computeIndexedFunction(const MSFloatVector& model_,
				       const MSFloatVector& weights_=indexedFuncs().defaultWeights(),
				       int mode_=MSIndexedFunctions::Total);
  static double computeIndexedFunction(const MSFloatVector& model_,
				       const MSFloatVector& weights_=indexedFuncs().defaultWeights(),
				       MSIndexVector& index_=indexedFuncs().defaultIndex(),
				       int mode_=MSIndexedFunctions::Total);
  static double computeTotal(const MSFloatVector&,MSIndexVector&);
  static double computeMinimum(const MSFloatVector&,MSIndexVector&);
  static double computeMaximum(const MSFloatVector&,MSIndexVector&);
  static double computeAverage(const MSFloatVector&,const MSFloatVector&,MSIndexVector&);

  static double computeIndexedFunction(const MSIntVector& model_,int mode_=MSIndexedFunctions::Total);
  static double computeIndexedFunction(const MSIntVector& model_,
				       const MSFloatVector& weights_=indexedFuncs().defaultWeights(),
				       int mode_=MSIndexedFunctions::Total);
  static double computeIndexedFunction(const MSIntVector& model_,
				       const MSFloatVector& weights_=indexedFuncs().defaultWeights(),
				       MSIndexVector& index_=indexedFuncs().defaultIndex(),
				       int mode_=MSIndexedFunctions::Total);
  static double computeTotal(const MSIntVector&,MSIndexVector&);
  static double computeMinimum(const MSIntVector&,MSIndexVector&);
  static double computeMaximum(const MSIntVector&,MSIndexVector&);
  static double computeAverage(const MSIntVector&,const MSFloatVector&,MSIndexVector&);
protected:
  static MSIndexVector computeIndex(const MSVector&,MSIndexVector&);
  
private:
  static MSIndexedFunctions    _indexedFuncs;
  MSIndexVector                _defaultIndex;
  MSFloatVector                _defaultWeights;
};

#endif
