///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSFloatMatrixView.H>

MSFloatMatrixView::MSFloatMatrixView(MSWidget *owner_,const char *title_) : 
MSNumericArrayView(owner_,title_)
{}

MSFloatMatrixView::MSFloatMatrixView(MSWidget *owner_,const MSStringVector& title_) : 
MSNumericArrayView(owner_,title_)
{}

MSFloatMatrixView::MSFloatMatrixView(MSWidget *owner_,MSFloatMatrix& model_,const char *title_) : 
MSNumericArrayView(owner_,title_)
{ model(model_); }

MSFloatMatrixView::MSFloatMatrixView(MSWidget *owner_,MSFloatMatrix& model_,const MSStringVector& title_) : 
MSNumericArrayView(owner_,title_)
{ model(model_); }

MSFloatMatrixView::~MSFloatMatrixView(void)
{}

void MSFloatMatrixView::model(MSFloatMatrix& model_)
{ couple(&model_); }

void MSFloatMatrixView::model(const MSFloatMatrix& model_)
{ constCouple(&model_); }

void MSFloatMatrixView::matrix(const MSFloatMatrix& aMatrix_)
{ if (MSView::model()!=0) matrix()=aMatrix_; }

MSBoolean MSFloatMatrixView::validate(const char *pString_,unsigned row_,unsigned column_) 
{
  if (MSView::model()!=0)
   {
     unsigned ravelIndex=matrix().index(row_,column_);
     return (matrix().set(ravelIndex,pString_)==MSError::MSSuccess)?MSTrue:MSFalse;
   }
  return MSTrue;
}

const char *MSFloatMatrixView::formatOutput(MSString &buffer_,unsigned row_,unsigned column_) 
{
  if (MSView::model()!=0) formatValue(buffer_,matrix()(row_,column_));
  return buffer_.string();
}

unsigned MSFloatMatrixView::numColumns(void) const
{ return (MSView::model()!=0)?matrix().columns():0; }

unsigned MSFloatMatrixView::numRows(void) const
{ return (MSView::model()!=0)?matrix().rows():0; }

void MSFloatMatrixView::moveRow(int from_, int to_)
{
  if(MSView::model()!=0)
   {
     MSFloatMatrix& matrixModel = matrix();
     MSFloatVector vector = matrixModel.rowAt(from_);
     matrixModel.removeRow(from_);
     if (to_>=matrixModel.rows()) matrixModel.appendRow(vector);
     else matrixModel.insertRowBefore(to_,vector);
   }
}



