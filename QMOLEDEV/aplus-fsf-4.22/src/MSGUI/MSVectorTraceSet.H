#ifndef MSVectorTraceSetHEADER
#define MSVectorTraceSetHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSBuiltinVector.H>
#include <MSTypes/MSTypeVector.H>
#include <MSGUI/MSTraceSet.H>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSVectorTraceSet.C")
#endif

template<class Type>
class MSVectorTraceSet : public MSTraceSet
{
public:
  MSVectorTraceSet(MSGraph *,const char *title_,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSVectorTraceSet(MSGraph *,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSVectorTraceSet(MSGraph *,MSTypeVector<Type>&,const char *,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSVectorTraceSet(MSGraph *,MSTypeVector<Type>&,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSVectorTraceSet(void);
  
  void model(MSTypeVector<Type>&);
  void model(const MSTypeVector<Type>&);

  MSTypeVector<Type>& viewModel(void)             { return *((MSTypeVector<Type>*)_model); }
  const MSTypeVector<Type>& viewModel(void) const { return *((MSTypeVector<Type>*)_model); }
  
protected:
  MSTypeVector<Type>& vector(void);
  
  // this member function now inlined, because of bug in Visual C++ 2.0
  const MSTypeVector<Type>& vector(void) const
  {return *((MSTypeVector<Type> *)_model); }
  
  virtual int dataCount(void) const;
  virtual double y(int row_,int) const;
  void vector(const MSTypeVector<Type>& aVector_);
  virtual void validate(int r_,int c_,double x_,double y_);
  virtual const char *formatOutput(MSString &,unsigned,unsigned);
  
  MSVectorTraceSet<Type>& operator=(const MSTypeVector<Type>& aVector_);
  operator MSTypeVector<Type>&() const;
};

#endif
