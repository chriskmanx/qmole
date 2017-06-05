///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSIntMatrixView.H>

MSIntMatrixView::MSIntMatrixView(MSWidget *owner_,const char *title_) : 
MSNumericArrayView(owner_,title_)
{}

MSIntMatrixView::MSIntMatrixView(MSWidget *owner_,const MSStringVector& title_) : 
MSNumericArrayView(owner_,title_)
{}

MSIntMatrixView::MSIntMatrixView(MSWidget *owner_,MSIntMatrix& model_,const char *title_) : 
MSNumericArrayView(owner_,title_)
{ model(model_); }

MSIntMatrixView::MSIntMatrixView(MSWidget *owner_,MSIntMatrix& model_,const MSStringVector& title_) : 
MSNumericArrayView(owner_,title_)
{ model(model_); }

MSIntMatrixView::~MSIntMatrixView(void)
{}

void MSIntMatrixView::model(MSIntMatrix& model_)
{ couple(&model_); }

void MSIntMatrixView::matrix(const MSIntMatrix& aMatrix_)
{ if (MSView::model()!=0) matrix()=aMatrix_; }

MSBoolean MSIntMatrixView::validate(const char *pString_,unsigned row_,unsigned column_) 
{
  if (MSView::model()!=0)
   {
     unsigned ravelIndex=matrix().index(row_,column_);
     return (matrix().set(ravelIndex,pString_)==MSError::MSSuccess)?MSTrue:MSFalse;
   }
  return MSTrue;
}

const char *MSIntMatrixView::formatOutput(MSString &buffer_,unsigned row_,unsigned column_) 
{
  if (MSView::model()!=0) formatValue(buffer_,matrix()(row_,column_));
  return buffer_.string();
}

unsigned MSIntMatrixView::numColumns(void) const
{ return (MSView::model()!=0)?matrix().columns():0; }

unsigned MSIntMatrixView::numRows(void) const
{ return (MSView::model()!=0)?matrix().rows():0; }

void MSIntMatrixView::moveRow(int from_, int to_)
{
  if(MSView::model()!=0)
   {
     MSIntMatrix& matrixModel = matrix();
     MSIntVector vector = matrixModel.rowAt(from_);
     matrixModel.removeRow(from_);
     if (to_>=matrixModel.rows()) matrixModel.appendRow(vector);
     else matrixModel.insertRowBefore(to_,vector);
   }
}

