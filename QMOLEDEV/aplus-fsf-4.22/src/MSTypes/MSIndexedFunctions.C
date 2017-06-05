///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <float.h>
#include <MSTypes/MSIndexedFunctions.H>
#include <MSTypes/MSMessageLog.H>

MSIndexedFunctions MSIndexedFunctions::_indexedFuncs;

MSIndexedFunctions::MSIndexedFunctions(void) {}
MSIndexedFunctions::~MSIndexedFunctions(void) {}

MSIndexVector MSIndexedFunctions::computeIndex(const MSVector& model_,MSIndexVector& index_)
{
  MSIndexVector index;
  unsigned start=0,stop=model_.length();
  if (index_.length()>1) 
   {
     MSIndexVector t(MSIndexVector::take(index_,-2));
     start=t(0);
     stop=t(1)>stop?stop:t(1);
   }
  return index<<start<<stop;
}

//
// MSFloatVector methods
//

double MSIndexedFunctions::computeIndexedFunction(const MSFloatVector& model_,int mode_)
{
  return MSIndexedFunctions::computeIndexedFunction(model_,indexedFuncs().defaultWeights(),
						   indexedFuncs().defaultIndex(),mode_);
}

double MSIndexedFunctions::computeIndexedFunction(const MSFloatVector& model_,
						  const MSFloatVector& weights_,int mode_)
{return MSIndexedFunctions::computeIndexedFunction(model_,weights_,indexedFuncs().defaultIndex(),mode_);}

double MSIndexedFunctions::computeIndexedFunction(const MSFloatVector& model_,
						  const MSFloatVector& weights_,
						  MSIndexVector& index_,int mode_)
{
  switch(mode_)
   {
   default:   
   case Total:   return computeTotal(model_,index_);
   case Minimum: return computeMinimum(model_,index_);
   case Maximum: return computeMaximum(model_,index_);
   case Average: return computeAverage(model_,weights_,index_);
   }
}

double MSIndexedFunctions::computeTotal(const MSFloatVector& model_,MSIndexVector& index_)
{
  double val=0;
  MSIndexVector index(computeIndex(model_,index_));
  for (unsigned i=index(0);i<index(1);i++) val+=model_(i);
  return val;
}

double MSIndexedFunctions::computeMinimum(const MSFloatVector& model_,MSIndexVector& index_)
{
  double val=DBL_MAX;
  MSIndexVector index(computeIndex(model_,index_));
  for (unsigned i=index(0);i<index(1);i++) val=val<model_(i)?val:model_(i);
  return val;
}

double MSIndexedFunctions::computeMaximum(const MSFloatVector& model_,MSIndexVector& index_)
{
  double val=-DBL_MAX;
  MSIndexVector index(computeIndex(model_,index_));
  for (unsigned i=index(0);i<index(1);i++) val=val>model_(i)?val:model_(i);
  return val;
}

double MSIndexedFunctions::computeAverage(const MSFloatVector& model_,const MSFloatVector& weights_,
					  MSIndexVector& index_)
{
  static MSFloatVector *pModel=0,*pWeights=0;
  double val=0,weightTotal=0.0;
  int mLen,wLen;
  MSIndexVector index(computeIndex(model_,index_));
  mLen=model_.length();
  wLen=weights_.length();
  for (unsigned i=index(0);i<index(1);i++)
   {
     double factor=mLen==wLen?weights_(i):1;
     val+=model_(i)*factor;
     weightTotal+=factor;
   }
  val/=weightTotal>0.0?weightTotal:1;
  if (wLen>0&&wLen!=mLen&&(pModel!=&model_&&pWeights!=&weights_))
   {
     pModel=(MSFloatVector*)&model_;
     pWeights=(MSFloatVector*)&weights_;
     MSMessageLog::warningMessage("MSIndexedFunctions::computeAverage error: Weights length %d != model length %d\n",
                                  wLen,mLen);
   }
  return val;
}

//
// MSIntVector methods
//

double MSIndexedFunctions::computeIndexedFunction(const MSIntVector& model_,int mode_)
{return MSIndexedFunctions::computeIndexedFunction(model_,indexedFuncs().defaultWeights(),
						   indexedFuncs().defaultIndex(),mode_);}

double MSIndexedFunctions::computeIndexedFunction(const MSIntVector& model_,
						  const MSFloatVector& weights_,int mode_)
{return MSIndexedFunctions::computeIndexedFunction(model_,weights_,indexedFuncs().defaultIndex(),mode_);}

double MSIndexedFunctions::computeIndexedFunction(const MSIntVector& model_,
						  const MSFloatVector& weights_,
						  MSIndexVector& index_,int mode_)
{
  switch(mode_)
   {
   default:   
   case Total:   return computeTotal(model_,index_);
   case Minimum: return computeMinimum(model_,index_);
   case Maximum: return computeMaximum(model_,index_);
   case Average: return computeAverage(model_,weights_,index_);
   }
}

double MSIndexedFunctions::computeTotal(const MSIntVector& model_,MSIndexVector& index_)
{
  double val=0;
  MSIndexVector index(computeIndex(model_,index_));
  for (unsigned i=index(0);i<index(1);i++) val+=model_(i);
  return val;
}

double MSIndexedFunctions::computeMinimum(const MSIntVector& model_,MSIndexVector& index_)
{
  double val=DBL_MAX;
  MSIndexVector index(computeIndex(model_,index_));
  for (unsigned i=index(0);i<index(1);i++) val=val<model_(i)?val:model_(i);
  return val;
}

double MSIndexedFunctions::computeMaximum(const MSIntVector& model_,MSIndexVector& index_)
{
  double val=-DBL_MAX;
  MSIndexVector index(computeIndex(model_,index_));
  for (unsigned i=index(0);i<index(1);i++) val=val>model_(i)?val:model_(i);
  return val;
}

double MSIndexedFunctions::computeAverage(const MSIntVector& model_,const MSFloatVector& weights_,
					  MSIndexVector& index_)
{
  static MSIntVector *pModel=0;
  static MSFloatVector *pWeights=0;
  double val=0,weightTotal=0.0;
  int mLen,wLen;
  mLen=model_.length();
  wLen=weights_.length();
  
  MSIndexVector index(computeIndex(model_,index_));
  for (unsigned i=index(0);i<index(1);i++)
   {
     double factor=mLen==wLen?weights_(i):1;
     val+=model_(i)*factor;
     weightTotal+=factor;
   }
  val/=weightTotal>0.0?weightTotal:1;
  if (wLen>0&&wLen!=mLen&&(pModel!=&model_&&pWeights!=&weights_))
   {
     pModel=(MSIntVector*)&model_;
     pWeights=(MSFloatVector*)&weights_;
     MSMessageLog::warningMessage("MSIndexedFunctions::computeAverage warning: Weights length %d != model length %d\n",
                                  wLen,mLen);
   }
  return val;
}
