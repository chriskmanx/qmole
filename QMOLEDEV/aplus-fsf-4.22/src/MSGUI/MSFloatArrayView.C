///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSFloatArrayView.H>

MSFloatArrayView::MSFloatArrayView(MSWidget *owner_,const char *title_) : 
MSNumericArrayView(owner_,title_)
{}

MSFloatArrayView::MSFloatArrayView(MSWidget *owner_,const MSStringVector& title_) : 
MSNumericArrayView(owner_,title_)
{}

MSFloatArrayView::MSFloatArrayView(MSWidget *owner_,MSFloatVector& model_,const char *title_) : 
MSNumericArrayView(owner_,title_)
{ model(model_); }

MSFloatArrayView::MSFloatArrayView(MSWidget *owner_,MSFloatVector& model_,const MSStringVector& title_) : 
MSNumericArrayView(owner_,title_)
{ model(model_); }

MSFloatArrayView::~MSFloatArrayView(void)
{}

void MSFloatArrayView::model(MSFloatVector& model_)
{ couple(&model_); }

void MSFloatArrayView::model(const MSFloatVector& model_)
{ constCouple(&model_); }

void MSFloatArrayView::array(const MSFloatVector& aVector_)
{ if (MSView::model()!=0) array()=aVector_; }

MSBoolean MSFloatArrayView::validate(const char *pString_,unsigned row_,unsigned)
{ return (MSView::model()!=0)?((array().set(row_,pString_)==MSError::MSSuccess)?MSTrue:MSFalse):MSTrue; }

const char *MSFloatArrayView::formatOutput(MSString &buffer_,unsigned row_,unsigned) 
{
  if (MSView::model()!=0) formatValue(buffer_,array()(row_));
  return buffer_.string();
}

unsigned MSFloatArrayView::numColumns(void) const
{ return 1; }

unsigned MSFloatArrayView::numRows(void) const
{ return (MSView::model()!=0)?array().length():0; }

void MSFloatArrayView::moveRow(int from_, int to_)
{
  if(MSView::model()!=0)
   {
     MSFloatVector& vectorModel = array();
     double item = vectorModel(from_);
     vectorModel.removeAt(from_);
     if (to_>=vectorModel.length()) vectorModel.append(item);
     else vectorModel.insertAt(to_,item);
   }
}

