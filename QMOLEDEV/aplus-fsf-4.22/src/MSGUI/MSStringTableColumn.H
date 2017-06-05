#ifndef MSStringTableColumnHEADER
#define MSStringTableColumnHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSTableColumn.H>
#include <MSTypes/MSStringVector.H>

class MSGUIExport MSStringTableColumn : public MSTableColumn
{
public:
  MSStringTableColumn(MSReportTable *,const char *heading_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSStringTableColumn(MSReportTable *,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSStringTableColumn(MSReportTable *,MSStringVector&,const char *heading_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSStringTableColumn(MSReportTable *,MSStringVector&,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSStringTableColumn(void);

  void model(MSStringVector&);
  void model(const MSStringVector&);

  MSStringVector& viewModel(void)             { return (MSStringVector&)*_model; }
  const MSStringVector& viewModel(void) const { return (MSStringVector&)*_model; }

  MSStringTableColumn& operator=(const MSStringVector& vector_);
  operator MSStringVector&() const;

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

  virtual void set(MSAttrValueList& avList_);
  virtual MSAttrValueList& get(MSAttrValueList& avList_);

protected:
  void init(void);
  virtual MSBoolean validate(const char *,unsigned);
  virtual void moveRow(int, int);

  MSStringVector& vector(void) { return (MSStringVector&)*_model; }
  const MSStringVector& vector(void) const { return (MSStringVector&)*_model; }
  void vector(const MSStringVector&);
};

inline MSStringTableColumn& MSStringTableColumn::operator=(const MSStringVector& vector_)
{ vector(vector_); return *this; }
inline MSStringTableColumn::operator MSStringVector&() const
{ return *((MSStringVector *)_model); }

#endif









