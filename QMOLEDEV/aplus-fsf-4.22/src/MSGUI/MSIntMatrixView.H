#ifndef MSIntMatrixViewHEADER
#define MSIntMatrixViewHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSNumericArrayView.H>
#include <MSTypes/MSIntMatrix.H>

class MSGUIExport MSIntMatrixView : public MSNumericArrayView
{
protected:
  virtual unsigned numColumns(void) const;
  virtual unsigned numRows(void) const;

  virtual const char *formatOutput(MSString &,unsigned,unsigned);
  virtual MSBoolean validate(const char *,unsigned,unsigned);
  virtual void moveRow(int,int);

  MSIntMatrix& matrix(void) { return (MSIntMatrix&)*_model; }
  const MSIntMatrix& matrix(void) const { return (MSIntMatrix&)*_model; }  
  void matrix(const MSIntMatrix&);
  
public:
  MSIntMatrixView(MSWidget *,const char *title_=0);
  MSIntMatrixView(MSWidget *,const MSStringVector&);
  MSIntMatrixView(MSWidget *,MSIntMatrix&,const char *title_=0);
  MSIntMatrixView(MSWidget *,MSIntMatrix&,const MSStringVector&);
  ~MSIntMatrixView(void);

  void model(MSIntMatrix&);

  MSIntMatrix& viewModel(void)             { return *((MSIntMatrix*)_model); }
  const MSIntMatrix& viewModel(void) const { return *((MSIntMatrix*)_model); }
  MSIntMatrixView& operator=(const MSIntMatrix& matrix_) { matrix(matrix_); return *this; }
  operator MSIntMatrix&() const { return *((MSIntMatrix *)_model); } 
};

#endif



