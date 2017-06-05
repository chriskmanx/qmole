#ifndef MSDependentTableColumnHEADER
#define MSDependentTableColumnHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSTableColumn.H>
#include <MSTypes/MSStringVector.H>
#include <MSTypes/MSFormat.H>
#include <MSTypes/MSDependentVector.H>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSDependentTableColumn.C")
#endif

template<class Type>
class MSDependentTableColumn : public MSTableColumn
{
public:
  MSDependentTableColumn(MSReportTable *,const char *title_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSDependentTableColumn(MSReportTable *,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSDependentTableColumn(MSReportTable *,MSDependentVector<Type>&,const char *title_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSDependentTableColumn(MSReportTable *,MSDependentVector<Type>&,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSDependentTableColumn(void);

  void model(MSDependentVector<Type>&);

  MSDependentVector<Type>& viewModel(void)             { return *((MSDependentVector<Type>*)_model); }
  const MSDependentVector<Type>& viewModel(void) const { return *((MSDependentVector<Type>*)_model); }

  // this member function inlined for Visual C++ 2.0
  operator MSDependentVector<Type>&() const
  { return *((MSDependentVector<Type> *)_model); }

  void format(const MSFormat&);
  inline const MSFormat& format(void) const;

  virtual MSIndexVector gradeUp(void) const;
  virtual MSIndexVector gradeDown(void) const;
  virtual void permute(const MSIndexVector &);
  virtual void range(MSIndexVector &start_,MSIndexVector &end_);
  virtual MSIndexVector rangeGradeUp(const MSIndexVector &start_,const MSIndexVector &end_);
  virtual MSIndexVector rangeGradeDown(const MSIndexVector &start_,const MSIndexVector &end_);
  virtual unsigned numRows(void) const;
  virtual MSBoolean isDuplicate(unsigned);
  virtual MSBoolean breakCriteria(unsigned);
  virtual const char *formatOutput(MSString &,unsigned);
  virtual const char *formatBreak(MSString &,unsigned,unsigned);

protected:
  void init(void);
  virtual void breakProcess(MSIndexVector&);
  virtual MSBoolean validate(const char *,unsigned);

  inline MSDependentVector<Type>& vector(void);
  inline const MSDependentVector<Type>& vector(void) const;  

  static const Type&  getType(void);
};

template<class Type>
inline MSDependentVector<Type>& MSDependentTableColumn<Type>::vector(void)
{ return (MSDependentVector<Type>&)*_model; }

template<class Type>
inline const MSDependentVector<Type>& MSDependentTableColumn<Type>::vector(void) const
{ return (MSDependentVector<Type>&)*_model; }

template<class Type>
inline const MSFormat& MSDependentTableColumn<Type>::format(void) const
{ return _format; }

#endif
