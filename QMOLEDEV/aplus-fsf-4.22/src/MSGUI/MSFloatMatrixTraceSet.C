///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSFloatMatrixTraceSet.H>

MSFloatMatrixTraceSet::MSFloatMatrixTraceSet(MSGraph *owner_,const char *legends_,const MSSymbol& tag_):
MSTraceSet(owner_,legends_,tag_)
{
  format(MSFormat(MSFloat::Decimal2));
}

MSFloatMatrixTraceSet::MSFloatMatrixTraceSet(MSGraph *owner_,const MSStringVector& legends_,const MSSymbol& tag_):
MSTraceSet(owner_,legends_,tag_)
{
  format(MSFormat(MSFloat::Decimal2));
}

MSFloatMatrixTraceSet::MSFloatMatrixTraceSet(MSGraph *owner_,MSFloatMatrix& m_,const char *legends_,const MSSymbol& tag_):
MSTraceSet(owner_,legends_,tag_)
{
  format(MSFormat(MSFloat::Decimal2));
  model(m_);
}

MSFloatMatrixTraceSet::MSFloatMatrixTraceSet(MSGraph *owner_,MSFloatMatrix& m_,const MSStringVector& legends_,const MSSymbol& tag_):
MSTraceSet(owner_,legends_,tag_)
{
  format(MSFormat(MSFloat::Decimal2));
  model(m_);
}

MSFloatMatrixTraceSet::~MSFloatMatrixTraceSet(void) {} 

int MSFloatMatrixTraceSet::dataCount(void) const
{ return MSView::model()!=0?matrix().rows():0; }     

double MSFloatMatrixTraceSet::x(int i_) const
{ return MSView::model()!=0?matrix()(i_,0):0.0; }     

double MSFloatMatrixTraceSet::y(int r_,int c_) const
{ return MSView::model()!=0?matrix()(r_,c_+1):0.0; }     

void MSFloatMatrixTraceSet::model(MSFloatMatrix& model_)
{ couple(&model_); }

void MSFloatMatrixTraceSet::model(const MSFloatMatrix& model_)
{ constCouple(&model_); }

int MSFloatMatrixTraceSet::numColumns(void) const
{ return MSView::model()!=0?matrix().columns()>1?matrix().columns():0:0; }

void MSFloatMatrixTraceSet::validate(int r_,int c_,double x_,double y_)
{
  if (MSView::model()!=0) 
   { 
     if (constraint()!=HoldX) matrix()(r_,0)=x_; 
     matrix()(r_,c_+1)=y_;
   }
}

const char *MSFloatMatrixTraceSet::formatOutput(MSString &buffer_,unsigned row_,unsigned col_)
{
  if (MSView::model()!=0)
   {
     MSFloat aFloat(matrix()(row_,col_));
     aFloat.format(buffer_,format());
   }
  return buffer_.string();
}

MSBoolean MSFloatMatrixTraceSet::moveTraceValidate(double xOffset_,double yOffset_)
{
  if (MSView::model()!=0) 
   {
     MSFloatMatrix& fm=matrix();
     MSFloatVector fv(fm.columnAt(0));
     fm-=yOffset_;
     fv+=xOffset_;
     fm.assignColumn(0,fv);
     return MSTrue;  
   }
  return MSFalse;
}

MSTraceSet* MSFloatMatrixTraceSet::copyTraceValidate(MSGraph * owner_,const char *legends_,const MSSymbol& tag_)
{
  MSFloatMatrix* model=(MSFloatMatrix*)matrix().clone();
  MSFloatMatrixTraceSet* ts=new MSFloatMatrixTraceSet(owner_,*model,legends_,tag_);
  ts->internalModel(MSTrue);
  return ts;
}

MSFloatMatrix MSFloatMatrixTraceSet::asFloatMatrix(void) const
{
  return matrix();
}


