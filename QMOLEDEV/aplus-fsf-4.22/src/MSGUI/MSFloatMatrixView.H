#ifndef MSFloatMatrixViewHEADER
#define MSFloatMatrixViewHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSNumericArrayView.H>
#include <MSTypes/MSFloatMatrix.H>

class MSGUIExport MSFloatMatrixView : public MSNumericArrayView
{
protected:
  virtual unsigned numColumns(void) const;
  virtual unsigned numRows(void) const;

  virtual const char *formatOutput(MSString &,unsigned,unsigned);
  virtual MSBoolean validate(const char *,unsigned,unsigned);
  virtual void moveRow(int, int);

  MSFloatMatrix& matrix(void) { return (MSFloatMatrix&)*_model; }
  const MSFloatMatrix& matrix(void) const { return (MSFloatMatrix&)*_model; }  
  void matrix(const MSFloatMatrix&);
  
public:
  MSFloatMatrixView(MSWidget *,const char *title_=0);
  MSFloatMatrixView(MSWidget *,const MSStringVector&);
  MSFloatMatrixView(MSWidget *,MSFloatMatrix&,const char *title_=0);
  MSFloatMatrixView(MSWidget *,MSFloatMatrix&,const MSStringVector&);
  ~MSFloatMatrixView(void);

  void model(MSFloatMatrix&);
  void model(const MSFloatMatrix&);

  MSFloatMatrix& viewModel(void)             { return *((MSFloatMatrix*)_model); }
  const MSFloatMatrix& viewModel(void) const { return *((MSFloatMatrix*)_model); }
  MSFloatMatrixView& operator=(const MSFloatMatrix& matrix_) { matrix(matrix_); return *this; }
  operator MSFloatMatrix&() const { return *((MSFloatMatrix *)_model); } 
};

#endif



