#ifndef MSIntMatrixTraceSetHEADER
#define MSIntMatrixTraceSetHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSIntMatrix.H>
#include <MSGUI/MSTraceSet.H>

class MSGUIExport MSIntMatrixTraceSet:public MSTraceSet
{
public:
  MSIntMatrixTraceSet(MSGraph *,const char *legends_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSIntMatrixTraceSet(MSGraph *,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSIntMatrixTraceSet(MSGraph *,MSIntMatrix&,const char *legends_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSIntMatrixTraceSet(MSGraph *,MSIntMatrix&,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSIntMatrixTraceSet(void);

  void model(MSIntMatrix&);
  void model(const MSIntMatrix&);

  MSIntMatrix& viewModel(void)             { return *((MSIntMatrix*)_model); }
  const MSIntMatrix& viewModel(void) const { return *((MSIntMatrix*)_model); }
  MSIntMatrixTraceSet& operator=(const MSIntMatrix& matrix_)  {matrix(matrix_); return *this;}
  operator MSIntMatrix&() const {return *((MSIntMatrix *)_model);} 

protected:
  MSIntMatrix& matrix(void)                   {return *((MSIntMatrix *)_model); }
  const MSIntMatrix& matrix(void)       const {return *((MSIntMatrix *)_model); }  
  void matrix(const MSIntMatrix& v_)          {if (MSView::model()!=0) matrix()=v_;}
  virtual int dataCount(void) const;
  virtual double x(int) const;
  virtual double y(int,int) const;
  virtual int numColumns(void) const;   
  virtual void validate(int,int,double,double);
  virtual MSBoolean moveTraceValidate(double,double);
  virtual MSTraceSet* copyTraceValidate(MSGraph*,const char*, const MSSymbol&);
  virtual MSFloatMatrix asFloatMatrix(void) const;
  virtual const char *formatOutput(MSString &,unsigned,unsigned);
};

#endif
