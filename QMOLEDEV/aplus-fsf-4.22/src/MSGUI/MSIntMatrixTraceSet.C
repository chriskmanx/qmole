///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSIntMatrixTraceSet.H>

MSIntMatrixTraceSet::MSIntMatrixTraceSet(MSGraph *owner_,const char *legends_,const MSSymbol& tag_):
MSTraceSet(owner_,legends_,tag_)
{
  format(MSFormat(MSInt::WithoutCommas));
}

MSIntMatrixTraceSet::MSIntMatrixTraceSet(MSGraph *owner_,const MSStringVector& legends_,const MSSymbol& tag_):
MSTraceSet(owner_,legends_,tag_)
{
  format(MSFormat(MSInt::WithoutCommas));
}

MSIntMatrixTraceSet::MSIntMatrixTraceSet(MSGraph *owner_,MSIntMatrix& m_,const char *legends_,const MSSymbol& tag_):
MSTraceSet(owner_,legends_,tag_)
{
  format(MSFormat(MSInt::WithoutCommas));
  model(m_);
}

MSIntMatrixTraceSet::MSIntMatrixTraceSet(MSGraph *owner_,MSIntMatrix& m_,const MSStringVector& legends_,const MSSymbol& tag_):
MSTraceSet(owner_,legends_,tag_)
{
  format(MSFormat(MSInt::WithoutCommas));
  model(m_);
}

MSIntMatrixTraceSet::~MSIntMatrixTraceSet(void) {} 

int MSIntMatrixTraceSet::dataCount(void) const
{ return MSView::model()!=0?matrix().rows():0; }     

double MSIntMatrixTraceSet::x(int i_) const
{ return MSView::model()!=0?matrix()(i_,0):0; }     

double MSIntMatrixTraceSet::y(int r_,int c_) const
{ return MSView::model()!=0?matrix()(r_,c_+1):0; }     

void MSIntMatrixTraceSet::model(MSIntMatrix& model_)
{ couple(&model_); }

void MSIntMatrixTraceSet::model(const MSIntMatrix& model_)
{ constCouple(&model_); }

int MSIntMatrixTraceSet::numColumns(void) const
{ return MSView::model()!=0?matrix().columns()>1?matrix().columns():0:0; }

void MSIntMatrixTraceSet::validate(int r_,int c_,double x_,double y_)
{
  if (MSView::model()!=0) 
   { 
     if (constraint()!=HoldX) matrix()(r_,0)=(int)x_; 
     matrix()(r_,c_+1)=(int)y_; 
   }
}

const char *MSIntMatrixTraceSet::formatOutput(MSString &buffer_,unsigned row_,unsigned col_)
{
  if (MSView::model()!=0)
   {
     MSInt aInt(matrix()(row_,col_));
     aInt.format(buffer_,format());
   }
  return buffer_.string();
}

MSBoolean MSIntMatrixTraceSet::moveTraceValidate(double xOffset_,double yOffset_)
{
  if (MSView::model()!=0) 
   {
     MSIntMatrix& fm=matrix();
     MSIntVector fv(fm.columnAt(0));
     fm-=(int)yOffset_;
     fv+=(int)xOffset_;
     fm.assignColumn(0,fv);
     return MSTrue;  
   }
  return MSFalse;
}

MSTraceSet* MSIntMatrixTraceSet::copyTraceValidate(MSGraph * owner_,const char *legends_,const MSSymbol& tag_)
{
  MSIntMatrix* model=(MSIntMatrix*)matrix().clone();
  MSIntMatrixTraceSet* ts=new MSIntMatrixTraceSet(owner_,*model,legends_,tag_);
  ts->internalModel(MSTrue);
  return ts;
}

MSFloatMatrix MSIntMatrixTraceSet::asFloatMatrix(void) const
{
  MSFloatMatrix fm(matrix().rows(),matrix().columns());
  int n=matrix().count();
  for(int i=0;i<n;i++)
   {
     fm.set(i,(double)matrix()(i)); // using absolute matrix index.
   }
  return fm;
}
