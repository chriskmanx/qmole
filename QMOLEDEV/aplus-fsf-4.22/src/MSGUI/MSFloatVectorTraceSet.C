///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSFloatVectorTraceSet.H>

MSFloatVectorTraceSet::MSFloatVectorTraceSet(MSGraph *owner_,const char *legends_,const MSSymbol& tag_):
MSTraceSet(owner_,legends_,tag_)
{
  format(MSFormat(MSFloat::Decimal2));
}

MSFloatVectorTraceSet::MSFloatVectorTraceSet(MSGraph *owner_,const MSStringVector& legends_,const MSSymbol& tag_):
MSTraceSet(owner_,legends_,tag_)
{
  format(MSFormat(MSFloat::Decimal2));
}

MSFloatVectorTraceSet::MSFloatVectorTraceSet(MSGraph *owner_,MSFloatVector& m_,const char *legends_,const MSSymbol& tag_):
MSTraceSet(owner_,legends_,tag_)
{
  format(MSFormat(MSFloat::Decimal2));
  model(m_);
}

MSFloatVectorTraceSet::MSFloatVectorTraceSet(MSGraph *owner_,MSFloatVector& m_,const MSStringVector& legends_,const MSSymbol& tag_):
MSTraceSet(owner_,legends_,tag_)
{
  format(MSFormat(MSFloat::Decimal2));
  model(m_);
}

MSFloatVectorTraceSet::~MSFloatVectorTraceSet(void) {} 

int MSFloatVectorTraceSet::dataCount(void) const
{ return MSView::model()!=0?vector().length():0; }     

double MSFloatVectorTraceSet::y(int i_,int) const
{ return MSView::model()!=0?vector()(i_):0; }     

void MSFloatVectorTraceSet::validate(int r_,int,double,double y_)
{ if (MSView::model()!=0) vector().set(r_,y_); }

void MSFloatVectorTraceSet::model(MSFloatVector& model_) 
{ couple(&model_); }

void MSFloatVectorTraceSet::model(const MSFloatVector& model_) 
{ constCouple(&model_); }

void MSFloatVectorTraceSet::selectable(MSBoolean x_)
{ MSTraceSet::selectable(x_); } 

MSBoolean MSFloatVectorTraceSet::selectable(void) const
{ return MSTraceSet::selectable(); } 

void MSFloatVectorTraceSet::constraint(unsigned long x_)
{ MSTraceSet::constraint(x_&HoldX); }

unsigned long MSFloatVectorTraceSet::constraint(void) const
{ return MSTraceSet::constraint(); }

const char *MSFloatVectorTraceSet::formatOutput(MSString &buffer_,unsigned row_,unsigned)
{
  if (MSView::model()!=0)
   {
     MSFloat aFloat(vector()(row_));
     aFloat.format(buffer_,format());
   }
  return buffer_.string();
}

MSBoolean MSFloatVectorTraceSet::moveTraceValidate(double,double yOffset_)
{
  if (MSView::model()!=0) 
   {
     vector()-=yOffset_;
     return MSTrue;  
   }
  return MSFalse;
}

MSTraceSet* MSFloatVectorTraceSet::copyTraceValidate(MSGraph * owner_,const char *legends_,const MSSymbol& tag_)
{
  MSFloatVector* model=(MSFloatVector*)vector().clone();
  MSFloatVectorTraceSet* ts=new MSFloatVectorTraceSet(owner_,*model,legends_,tag_);
  ts->internalModel(MSTrue);
  return ts;
}

MSFloatMatrix MSFloatVectorTraceSet::asFloatMatrix(void) const
{
  MSFloatMatrix fm(vector().length(),0);
  fm.appendColumn(vector());
  return fm;
}
