///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSIntArrayView.H>

MSIntArrayView::MSIntArrayView(MSWidget *owner_,const char *title_) : 
MSNumericArrayView(owner_,title_)
{}

MSIntArrayView::MSIntArrayView(MSWidget *owner_,const MSStringVector& title_) : 
MSNumericArrayView(owner_,title_)
{}

MSIntArrayView::MSIntArrayView(MSWidget *owner_,MSIntVector& model_,const char *title_) : 
MSNumericArrayView(owner_,title_)
{ model(model_); }

MSIntArrayView::MSIntArrayView(MSWidget *owner_,MSIntVector& model_,const MSStringVector& title_) : 
MSNumericArrayView(owner_,title_)
{ model(model_); }

MSIntArrayView::~MSIntArrayView(void)
{}

void MSIntArrayView::model(MSIntVector& model_)
{ couple(&model_); }

void MSIntArrayView::model(const MSIntVector& model_)
{ constCouple(&model_); }

void MSIntArrayView::array(const MSIntVector& aVector_)
{ if (MSView::model()!=0) array()=aVector_; }

MSBoolean MSIntArrayView::validate(const char *pString_,unsigned row_,unsigned) 
{ return (MSView::model()!=0)?((array().set(row_,pString_)==MSError::MSSuccess)?MSTrue:MSFalse):MSTrue; }

const char *MSIntArrayView::formatOutput(MSString &buffer_,unsigned row_,unsigned) 
{
  if (MSView::model()!=0) formatValue(buffer_,array()(row_));
  return buffer_.string();
}

unsigned MSIntArrayView::numColumns(void) const
{ return 1; }

unsigned MSIntArrayView::numRows(void) const
{ return (MSView::model()!=0)?array().length():0; }


void MSIntArrayView::moveRow(int from_, int to_)
{
  if(MSView::model()!=0)
   {
     MSIntVector& vectorModel = array();
     int item = vectorModel(from_);
     vectorModel.removeAt(from_);
     if (to_>=vectorModel.length()) vectorModel.append(item);
     else vectorModel.insertAt(to_,item);
   }
}


