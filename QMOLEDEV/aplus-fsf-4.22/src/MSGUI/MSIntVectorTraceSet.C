///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSIntVectorTraceSet.H>

MSIntVectorTraceSet::MSIntVectorTraceSet(MSGraph *owner_,const char *legends_,const MSSymbol& tag_):
MSTraceSet(owner_,legends_,tag_)
{
  format(MSFormat(MSInt::WithoutCommas));
}

MSIntVectorTraceSet::MSIntVectorTraceSet(MSGraph *owner_,const MSStringVector& legends_,const MSSymbol& tag_):
MSTraceSet(owner_,legends_,tag_)
{
  format(MSFormat(MSInt::WithoutCommas));
}

MSIntVectorTraceSet::MSIntVectorTraceSet(MSGraph *owner_,MSIntVector& m_,const char *legends_,const MSSymbol& tag_):
MSTraceSet(owner_,legends_,tag_)
{
  format(MSFormat(MSInt::WithoutCommas));
  model(m_);
}

MSIntVectorTraceSet::MSIntVectorTraceSet(MSGraph *owner_,MSIntVector& m_,const MSStringVector& legends_,const MSSymbol& tag_):
MSTraceSet(owner_,legends_,tag_)
{
  format(MSFormat(MSInt::WithoutCommas));
  model(m_);
}

MSIntVectorTraceSet::~MSIntVectorTraceSet(void) {} 

int MSIntVectorTraceSet::dataCount(void) const
{ return MSView::model()!=0?vector().length():0; }     

double MSIntVectorTraceSet::y(int i_,int) const
{ return (double)(MSView::model()!=0?vector()(i_):0); }     

void MSIntVectorTraceSet::validate(int r_,int,double,double y_)
{ if (MSView::model()!=0) vector().set(r_,(int)y_);}

void MSIntVectorTraceSet::model(MSIntVector& model_)
{ couple(&model_); }

void MSIntVectorTraceSet::model(const MSIntVector& model_)
{ constCouple(&model_); }

void MSIntVectorTraceSet::selectable(MSBoolean x_)
{ MSTraceSet::selectable(x_); } 

MSBoolean MSIntVectorTraceSet::selectable(void) const
{ return MSTraceSet::selectable(); } 

void MSIntVectorTraceSet::constraint(unsigned long x_)
{ MSTraceSet::constraint(x_&HoldX); }

unsigned long MSIntVectorTraceSet::constraint(void) const
{ return MSTraceSet::constraint(); }

const char *MSIntVectorTraceSet::formatOutput(MSString &buffer_,unsigned row_,unsigned) 
{
  if (MSView::model()!=0)
   {
     MSInt aInt(vector()(row_));
     aInt.format(buffer_,format());
   }
  return buffer_.string();
}

MSBoolean MSIntVectorTraceSet::moveTraceValidate(double,double yOffset_)
{
  if (MSView::model()!=0) 
   {
     vector()-=(int)yOffset_;
     return MSTrue;  
   }
  return MSFalse;
}


MSTraceSet* MSIntVectorTraceSet::copyTraceValidate(MSGraph * owner_,const char *legends_,const MSSymbol& tag_)
{
  MSIntVector* model=(MSIntVector*)vector().clone();
  MSIntVectorTraceSet* ts=new MSIntVectorTraceSet(owner_,*model,legends_,tag_);
  ts->internalModel(MSTrue);
  return ts;
}

MSFloatMatrix MSIntVectorTraceSet::asFloatMatrix(void) const
{
  int n=vector().length();
  MSFloatMatrix fm(n,1);
  for(int i=0;i<n;i++)
   {
     fm.set(i,vector()(i)); // using absolute matrix index.
   }
  return fm;
}

