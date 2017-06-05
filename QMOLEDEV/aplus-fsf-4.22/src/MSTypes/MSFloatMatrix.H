#ifndef MSFloatMatrixHEADER
#define MSFloatMatrixHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDefines.H>

#if defined(MS_TEMPLATE_BEFORE_SPECIALIZATION)
#include <MSTypes/MSTypeMatrix.H>
#endif

#include <MSTypes/MSMatrix.H>
#include <MSTypes/MSBinaryMatrix.H>
#include <MSTypes/MSTypeData.H>
#include <MSTypes/MSFloatVector.H>
#include <MSTypes/MSAllocator.H>

class MSBinaryVector;
template<class Type> class MSTypeMatrix;
template<class Type> class MSTypeVector;
template<class Type> class MSMatrixSTypePick; // MSTypeMatrix indexed by an unsigned int

#if !defined(MS_NO_PREDECLARE_SPECIALIZATION)
class MSMatrixSTypePick<double>; 
#endif


#if !defined(MS_MSFloatMatrix_INSTANTIATE) && defined(MS_STD_TEMPLATE_SPECIALIZATION)
template <> class MSTypeMatrix<double> : public MSMatrix
#else
template <> class MSTypesExport MSTypeMatrix<double> : public MSMatrix
#endif
{
public:
  MSTypeMatrix(void);
  MSTypeMatrix(unsigned rows_,unsigned columns_);
  MSTypeMatrix(unsigned rows_,unsigned columns_,double fill_);
  MSTypeMatrix(const MSTypeMatrix<double>&);
  MSTypeMatrix(MSTypeData<double,MSAllocator<double> > *dp_,unsigned rows_,unsigned columns_);
  MSTypeMatrix(const double *pElements_,unsigned rows_,unsigned columns_);
  ~MSTypeMatrix(void);

  virtual MSString asString(void) const;
  virtual MSString asMSF(void) const;
  virtual MSString asDebugInfo(void) const;
  virtual MSString className(void) const;
  virtual const MSSymbol& type(void) const;
  virtual MSModel *clone(void) const;
  virtual MSModel *create(void) const;
  virtual void assign(const MSModel&);
  virtual long compare(const MSModel&) const;
  virtual MSError::ErrorStatus set(const char *pString_);
  virtual MSError::ErrorStatus setFromMSF(const char *pString_);
  static const MSSymbol& symbol(void);

  MSError::ErrorStatus set(unsigned index_,const char *pString_); 
  MSError::ErrorStatus set(unsigned index_,double); 

  unsigned indexOf(double aValue_,unsigned startPos_=0) const;
  unsigned lastIndexOf(double aValue_,unsigned startPos_=UINT_MAX-1) const;

  inline const double& elementAt(unsigned) const;
  inline const double& elementAt(unsigned,unsigned) const;

  MSTypeMatrix<double>& series(double offset_=0.0);

  MSTypeMatrix<double>& random(unsigned long limit_=0);
  double min(void) const;
  double max(void) const;
  double sum(void) const;

  MSTypeMatrix<double>& operator=(const MSTypeMatrix<double>&);
  MSTypeMatrix<double>& operator=(const MSMatrixSTypePick<double>&);
  MSTypeMatrix<double>& operator=(double);

  inline double operator()(unsigned index_) const;
  inline double operator[](unsigned index_) const;
  inline double operator()(unsigned row_,unsigned column_) const;

  inline MSMatrixSTypePick<double> operator()(unsigned index_);
  inline MSMatrixSTypePick<double> operator[](unsigned index_);
  inline MSMatrixSTypePick<double> operator()(unsigned row_,unsigned column_);
  
  friend MSTypesExport ostream& operator<<(ostream&,const MSTypeMatrix<double>&);

  friend MSTypesExport MSTypeMatrix<double> operator-(const MSTypeMatrix<double>&);
  MSTypeMatrix<double>& operator++();       // Prefix
  MSTypeMatrix<double>& operator--();
  MSTypeMatrix<double> operator++(int);    // Postfix
  MSTypeMatrix<double> operator--(int);

  long compare(const MSTypeMatrix<double>&) const;
  //inline friend MSTypesExport long compare(const MSTypeMatrix<double>&,const MSTypeMatrix<double>&);

  MSBinaryMatrix binaryCompare(const MSTypeMatrix<double>&,MSComparison) const;
  MSBinaryMatrix binaryCompare(double,MSComparison) const;
  MSBoolean scalarCompare(double,MSComparison) const;

  inline MSBoolean operator==(const MSTypeMatrix<double>&) const;
  inline MSBoolean operator!=(const MSTypeMatrix<double>&) const;

  inline friend MSTypesExport MSBoolean operator<(const MSTypeMatrix<double>&,const MSTypeMatrix<double>&);
  inline friend MSTypesExport MSBoolean operator>(const MSTypeMatrix<double>&,const MSTypeMatrix<double>&);
  inline friend MSTypesExport MSBoolean operator<=(const MSTypeMatrix<double>&,const MSTypeMatrix<double>&);
  inline friend MSTypesExport MSBoolean operator>=(const MSTypeMatrix<double>&,const MSTypeMatrix<double>&);

  inline friend MSTypesExport MSBoolean operator<(const MSTypeMatrix<double>&,double);
  inline friend MSTypesExport MSBoolean operator<(double,const MSTypeMatrix<double>&);
  inline friend MSTypesExport MSBoolean operator>(const MSTypeMatrix<double>&,double);
  inline friend MSTypesExport MSBoolean operator>(double,const MSTypeMatrix<double>&);
  inline friend MSTypesExport MSBoolean operator<=(const MSTypeMatrix<double>&,double);
  inline friend MSTypesExport MSBoolean operator<=(double,const MSTypeMatrix<double>&);
  inline friend MSTypesExport MSBoolean operator>=(const MSTypeMatrix<double>&,double);
  inline friend MSTypesExport MSBoolean operator>=(double,const MSTypeMatrix<double>&);
  inline friend MSTypesExport MSBoolean operator==(const MSTypeMatrix<double>&,double);
  inline friend MSTypesExport MSBoolean operator==(double,const MSTypeMatrix<double>&);
  inline friend MSTypesExport MSBoolean operator!=(const MSTypeMatrix<double>&,double);
  inline friend MSTypesExport MSBoolean operator!=(double,const MSTypeMatrix<double>&);

  friend MSTypesExport MSTypeMatrix<double> operator+(const MSTypeMatrix<double>&,double);
  friend MSTypesExport MSTypeMatrix<double> operator+(double,const MSTypeMatrix<double>&);
  friend MSTypesExport MSTypeMatrix<double> operator+(const MSTypeMatrix<double>&,const MSTypeMatrix<double>&);
  MSTypeMatrix<double>& operator+=(double);

  friend MSTypesExport MSTypeMatrix<double> operator-(const MSTypeMatrix<double>&,double);
  friend MSTypesExport MSTypeMatrix<double> operator-(const MSTypeMatrix<double>&,const MSTypeMatrix<double>&);
  MSTypeMatrix<double>& operator-=(double);

  friend MSTypesExport MSTypeMatrix<double> operator*(const MSTypeMatrix<double>&,double);
  friend MSTypesExport MSTypeMatrix<double> operator*(double,const MSTypeMatrix<double>&);
  friend MSTypesExport MSTypeMatrix<double> operator*(const MSTypeMatrix<double>&,const MSTypeMatrix<double>&);
  MSTypeMatrix<double>& operator*=(double);

  friend MSTypesExport MSTypeMatrix<double> operator/(const MSTypeMatrix<double>&,double);
  friend MSTypesExport MSTypeMatrix<double> operator/(const MSTypeMatrix<double>&,const MSTypeMatrix<double>&);
  MSTypeMatrix<double>& operator/=(double);

  // The following operators are equivalent to the apl operation m_ (x @1 0) v_
  friend MSTypesExport MSTypeMatrix<double> operator+(const MSTypeMatrix<double>& m_,const MSTypeVector<double>& v_);
  friend MSTypesExport MSTypeMatrix<double> operator-(const MSTypeMatrix<double>& m_,const MSTypeVector<double>& v_);
  friend MSTypesExport MSTypeMatrix<double> operator*(const MSTypeMatrix<double>& m_,const MSTypeVector<double>& v_);
  friend MSTypesExport MSTypeMatrix<double> operator/(const MSTypeMatrix<double>& m_,const MSTypeVector<double>& v_);

  // The following operators are equivalent to the apl operation v_ (x @0 1) m_
  friend MSTypesExport MSTypeMatrix<double> operator+(const MSTypeVector<double>& v_,const MSTypeMatrix<double>& m_);
  friend MSTypesExport MSTypeMatrix<double> operator*(const MSTypeVector<double>& v_,const MSTypeMatrix<double>& m_);

  // matrix multiplication (inner product)
  friend MSTypesExport MSTypeMatrix<double> multiply(const MSTypeMatrix<double>&,const MSTypeMatrix<double>&);

  // stack matrices vertically
  MSTypeMatrix<double>& stack(const MSTypeMatrix<double>&);
  friend MSTypesExport MSTypeMatrix<double> stack(const MSTypeMatrix<double>&,const MSTypeMatrix<double>&);

  // adjoin matrices horizontally
  MSTypeMatrix<double>& adjoin(const MSTypeMatrix<double>&);
  friend MSTypesExport MSTypeMatrix<double> adjoin(const MSTypeMatrix<double>&,const MSTypeMatrix<double>&);

  MSTypeMatrix<double>& appendColumn(const MSTypeVector<double>&);
  MSTypeMatrix<double>& appendColumns(unsigned columns_,double fill_=0);
  MSTypeMatrix<double>& appendRow(const MSTypeVector<double>&);
  MSTypeMatrix<double>& appendRows(unsigned rows_,double fill_=0);
  MSTypeMatrix<double>& exchangeColumns(unsigned a_,unsigned b_);
  MSTypeMatrix<double>& exchangeRows(unsigned a_,unsigned b_);
  MSTypeMatrix<double>& insertColumnBefore(unsigned column_,double fill_=0);
  MSTypeMatrix<double>& insertColumnBefore(unsigned column_,const MSTypeVector<double>&);
  MSTypeMatrix<double>& insertColumnAfter(unsigned column_,double fill_=0);
  MSTypeMatrix<double>& insertColumnAfter(unsigned column_,const MSTypeVector<double>&);
  MSTypeMatrix<double>& insertRowBefore(unsigned row_,double fill_=0);
  MSTypeMatrix<double>& insertRowBefore(unsigned row_,const MSTypeVector<double>&);
  MSTypeMatrix<double>& insertRowAfter(unsigned row_,double fill_=0);
  MSTypeMatrix<double>& insertRowAfter(unsigned row_,const MSTypeVector<double>&);
  MSTypeMatrix<double>& reshape(unsigned rows_,unsigned columns_);
  MSTypeMatrix<double>& removeAll(void);
  MSTypeMatrix<double>& removeAllRows(void);
  MSTypeMatrix<double>& removeAllColumns(void);
  MSTypeMatrix<double>& assignRow(unsigned row_, double scalar_);
  MSTypeMatrix<double>& assignColumn(unsigned column_, double scalar_);
  MSTypeMatrix<double>& assignColumn(unsigned column_, const MSTypeVector<double>&);
  MSTypeMatrix<double>& assignRow(unsigned row_, const MSTypeVector<double>&);
  MSTypeMatrix<double>& removeRow(unsigned row_);
  MSTypeMatrix<double>& removeColumn(unsigned column_);
  MSTypeMatrix<double>& compressRows(const MSBinaryVector&);
  MSTypeMatrix<double>& compressColumns(const MSBinaryVector&);
  MSTypeMatrix<double>& reverseRows(void);
  MSTypeMatrix<double>& reverseColumns(void);
  MSTypeMatrix<double>& transpose(void);
  MSTypeMatrix<double>& rotateRows(int position_);
  MSTypeMatrix<double>& rotateColumns(int position_);
  MSTypeMatrix<double>& takeRows(int numberOfRows_);
  MSTypeMatrix<double>& takeColumns(int numberOfColumns_);
  MSTypeMatrix<double>& dropRows(int numberOfRows_);
  MSTypeMatrix<double>& dropColumns(int numberOfColumns_);

  MSTypeVector<double> rowAt(unsigned row_)  const;
  MSTypeVector<double> columnAt(unsigned column_) const;
  
  MSTypeMatrix<double>& allElementsDo(ElementWiseFunction,void *clientData_=0);
  MSTypeMatrix<double>& allElementsDo(MathFunction);
  friend MSTypesExport MSTypeMatrix<double> allElementsDo(const MSTypeMatrix<double>&,MathFunction);
  friend MSTypesExport MSTypeMatrix<double> allElementsDo(const MSTypeMatrix<double>&,
					     ElementWiseFunction,
					     void *clientData_=0);
  MSTypeMatrix<double>& sin(void);
  MSTypeMatrix<double>& sinh(void);
  MSTypeMatrix<double>& cos(void);
  MSTypeMatrix<double>& cosh(void);
  MSTypeMatrix<double>& tan(void);
  MSTypeMatrix<double>& tanh(void);
  MSTypeMatrix<double>& asin(void);
  MSTypeMatrix<double>& acos(void);
  MSTypeMatrix<double>& atan(void);
  MSTypeMatrix<double>& exp(void);
  MSTypeMatrix<double>& log(void);
  MSTypeMatrix<double>& sqrt(void);
  MSTypeMatrix<double>& ceil(void);
  MSTypeMatrix<double>& floor(void);
  MSTypeMatrix<double>& abs(void);

  MSTypeMatrix<double>& pow(double);

  friend MSTypesExport MSTypeMatrix<double> sin(const MSTypeMatrix<double>&);
  friend MSTypesExport MSTypeMatrix<double> sinh(const MSTypeMatrix<double>&);
  friend MSTypesExport MSTypeMatrix<double> cos(const MSTypeMatrix<double>&);
  friend MSTypesExport MSTypeMatrix<double> cosh(const MSTypeMatrix<double>&);
  friend MSTypesExport MSTypeMatrix<double> tan(const MSTypeMatrix<double>&);
  friend MSTypesExport MSTypeMatrix<double> tanh(const MSTypeMatrix<double>&);
  friend MSTypesExport MSTypeMatrix<double> asin(const MSTypeMatrix<double>&);
  friend MSTypesExport MSTypeMatrix<double> acos(const MSTypeMatrix<double>&);
  friend MSTypesExport MSTypeMatrix<double> atan(const MSTypeMatrix<double>&);
  friend MSTypesExport MSTypeMatrix<double> exp(const MSTypeMatrix<double>&);
  friend MSTypesExport MSTypeMatrix<double> log(const MSTypeMatrix<double>&);
  friend MSTypesExport MSTypeMatrix<double> sqrt(const MSTypeMatrix<double>&);
  friend MSTypesExport MSTypeMatrix<double> ceil(const MSTypeMatrix<double>&);
  friend MSTypesExport MSTypeMatrix<double> floor(const MSTypeMatrix<double>&);
  friend MSTypesExport MSTypeMatrix<double> abs(const MSTypeMatrix<double>&);

  friend MSTypesExport MSTypeMatrix<double> pow(const MSTypeMatrix<double>&,double);

  void error(const char *) const;
  inline double *data(void) const;
  inline unsigned size(void) const;
  inline unsigned length(void) const;

protected:
  friend class MSMatrixSTypePick<double>;

  static double           _badData; // used as return value for index errors
  MSTypeData<double,MSAllocator<double> >     *_pData;
//  unsigned                _size;
  MSBoolean               _blocked;

  inline MSTypeData<double,MSAllocator<double> > *pData(void) const;
  inline MSBoolean blocked(void) const;
  inline void blocked(MSBoolean);

  void reserve(unsigned);
  void blockLeft(unsigned target_,unsigned moveCount_);
  void blockRight(unsigned target_,unsigned moveCount_);  
  void freeData(void);
  void allocData(unsigned);
  void makeUniqueCopy(void);
  void prepareToChange(void);
  
  inline MSTypeData<double,MSAllocator<double> > *incrementCount(void);
  inline void decrementCount(void);

  inline double& elementAt(unsigned);
  inline double& elementAt(unsigned,unsigned);

  // ElementWiseFunction that returns x**y
  static double _pow(double x_,void *y_);
};
  
#if !defined(MS_MSFloatMatrix_INSTANTIATE) && defined(MS_STD_TEMPLATE_SPECIALIZATION)
template <> class MSMatrixSTypePick<double>
#else
template <> class MSTypesExport MSMatrixSTypePick<double>
#endif
{
public:
  inline MSMatrixSTypePick<double>& operator=(double);

  MSMatrixSTypePick<double>& operator+=(double);
  MSMatrixSTypePick<double>& operator-=(double);
  MSMatrixSTypePick<double>& operator*=(double);
  MSMatrixSTypePick<double>& operator/=(double);

  MSMatrixSTypePick<double>& operator++(int);
  MSMatrixSTypePick<double>& operator--(int);

  inline operator double() const;

  inline MSTypeMatrix<double> *pMatrix(void) const;
  inline double value(void) const;
  inline unsigned index(void) const;

private:
#ifdef MS_TEMPLATE_FRIEND_CLASS_BUG
  friend MSTypeMatrix<double>;
#else
  friend class MSTypeMatrix<double>;
#endif
  inline MSMatrixSTypePick(const MSTypeMatrix<double>&,unsigned);
  inline MSMatrixSTypePick(const MSMatrixSTypePick<double>&);

  MSTypeMatrix<double>    *_pMatrix;
  unsigned               _index;
};

//------------------------------------------------------------
// MSTypeMatrix inlines
//------------------------------------------------------------
inline MSTypeData<double,MSAllocator<double> > *MSTypeMatrix<double>::pData(void) const
{ return _pData; }

inline unsigned MSTypeMatrix<double>::size(void) const 
{ return _pData->size(); }
//{ return _size; }

inline unsigned MSTypeMatrix<double>::length(void) const 
{ return _count; }

inline MSBoolean MSTypeMatrix<double>::blocked(void) const 
{ return _blocked; }

inline void MSTypeMatrix<double>::blocked(MSBoolean blocked_)
{ _blocked=blocked_; }

inline double& MSTypeMatrix<double>::elementAt(unsigned index_)
{
#if !defined(MS_NO_INDEX_ERROR)
  return (index_<_count)?_pData->elementAt(index_):(indexError(index_,length()),_badData);
#else
  return _pData->elementAt(index_);
#endif
}

inline const double& MSTypeMatrix<double>::elementAt(unsigned index_) const
{
#if !defined(MS_NO_INDEX_ERROR)
//  return (index_<_count)?_pData->get(index_):(indexError(index_,length()),_badData);
  return (index_<_count)?_pData->elementAt(index_):(indexError(index_,length()),_badData);
#else
  return _pData->elementAt(index_);  
#endif
}

inline double& MSTypeMatrix<double>::elementAt(unsigned row_,unsigned column_)
{
#if !defined(MS_NO_INDEX_ERROR)
  unsigned ravel=index(row_,column_);
  return (ravel<_count)?_pData->elementAt(ravel):(indexError(ravel,length()),_badData);
#else
  return _pData->elementAt(index(row_,column_));
#endif
}

inline const double& MSTypeMatrix<double>::elementAt(unsigned row_,unsigned column_) const
{
#if !defined(MS_NO_INDEX_ERROR)
  unsigned ravel=index(row_,column_);
  return (ravel<_count)?_pData->elementAt(ravel):(indexError(ravel,length()),_badData);
#else
  return _pData->elementAt(index(row_,column_));  
#endif
}

inline double MSTypeMatrix<double>::operator()(unsigned index_) const
{ return elementAt(index_); }

inline double MSTypeMatrix<double>::operator[](unsigned index_) const
{ return elementAt(index_); }

inline double MSTypeMatrix<double>::operator()(unsigned row_,unsigned column_) const
{ return elementAt(index(row_,column_)); }

inline MSTypeData<double,MSAllocator<double> > *MSTypeMatrix<double>::incrementCount(void)
{ return (_pData!=0)?_pData->incrementCount():0; }

inline void MSTypeMatrix<double>::decrementCount(void)
{ if (_pData!=0) _pData->decrementCount(); }

inline double *MSTypeMatrix<double>::data(void) const
{ return (_pData!=0)?_pData->elements():0; }

// // inline long compare(const MSTypeMatrix<double>& aTypeMatrix_,const MSTypeMatrix<double>& bTypeMatrix_)
// { return aTypeMatrix_.compare(bTypeMatrix_); }

inline MSBoolean MSTypeMatrix<double>::operator==(const MSTypeMatrix<double>& aTypeMatrix_) const
{ return MSBoolean(compare(aTypeMatrix_)==0); }
inline MSBoolean MSTypeMatrix<double>::operator!=(const MSTypeMatrix<double>& aTypeMatrix_) const
{ return MSBoolean(compare(aTypeMatrix_)!=0); }

inline MSBoolean operator<(const MSTypeMatrix<double>& aTypeMatrix_,const MSTypeMatrix<double>& bTypeMatrix_)
{ return MSBoolean(aTypeMatrix_.compare(bTypeMatrix_)>0); }
inline MSBoolean operator>(const MSTypeMatrix<double>& aTypeMatrix_,const MSTypeMatrix<double>& bTypeMatrix_)
{ return MSBoolean(aTypeMatrix_.compare(bTypeMatrix_)<0); }
inline MSBoolean operator<=(const MSTypeMatrix<double>& aTypeMatrix_,const MSTypeMatrix<double>& bTypeMatrix_)
{ return MSBoolean(aTypeMatrix_.compare(bTypeMatrix_)>=0); }
inline MSBoolean operator>=(const MSTypeMatrix<double>& aTypeMatrix_,const MSTypeMatrix<double>& bTypeMatrix_)
{ return MSBoolean(aTypeMatrix_.compare(bTypeMatrix_)<=0); }

inline MSBoolean operator<(const MSTypeMatrix<double>& aTypeMatrix_,double aType_)
{ return aTypeMatrix_.scalarCompare(aType_,MSLessThan); }
inline MSBoolean operator<(double aType_,const MSTypeMatrix<double>& aTypeMatrix_)
{ return aTypeMatrix_.scalarCompare(aType_,MSGreaterThan); }

inline MSBoolean operator>(const MSTypeMatrix<double>& aTypeMatrix_,double aType_)
{ return aTypeMatrix_.scalarCompare(aType_,MSGreaterThan); }
inline MSBoolean operator>(double aType_,const MSTypeMatrix<double>& aTypeMatrix_)
{ return aTypeMatrix_.scalarCompare(aType_,MSLessThan); }

inline MSBoolean operator<=(const MSTypeMatrix<double>& aTypeMatrix_,double aType_)
{ return aTypeMatrix_.scalarCompare(aType_,MSLessThanOrEqualTo); }
inline MSBoolean operator<=(double aType_,const MSTypeMatrix<double>& aTypeMatrix_)
{ return aTypeMatrix_.scalarCompare(aType_,MSGreaterThanOrEqualTo); }

inline MSBoolean operator>=(const MSTypeMatrix<double>& aTypeMatrix_,double aType_)
{ return aTypeMatrix_.scalarCompare(aType_,MSGreaterThanOrEqualTo); }
inline MSBoolean operator>=(double aType_,const MSTypeMatrix<double>& aTypeMatrix_)
{ return aTypeMatrix_.scalarCompare(aType_,MSLessThanOrEqualTo); }

inline MSBoolean operator==(const MSTypeMatrix<double>& aTypeMatrix_,double aType_)
{ return aTypeMatrix_.scalarCompare(aType_,MSEqualTo); }
inline MSBoolean operator==(double aType_,const MSTypeMatrix<double>& aTypeMatrix_)
{ return aTypeMatrix_.scalarCompare(aType_,MSEqualTo); }

inline MSBoolean operator!=(const MSTypeMatrix<double>& aTypeMatrix_,double aType_)
{ return aTypeMatrix_.scalarCompare(aType_,MSNotEqualTo); }
inline MSBoolean operator!=(double aType_,const MSTypeMatrix<double>& aTypeMatrix_)
{ return aTypeMatrix_.scalarCompare(aType_,MSNotEqualTo); }

//------------------------------------------------------------
// MSMatrixSTypePick inlines
//------------------------------------------------------------
inline MSMatrixSTypePick<double>::MSMatrixSTypePick(const MSTypeMatrix<double>& aTypeMatrix_,unsigned index_)
{ _pMatrix=&(MSTypeMatrix<double>&)aTypeMatrix_; _index=index_; }

inline MSMatrixSTypePick<double>::MSMatrixSTypePick(const MSMatrixSTypePick<double>& aPick_)
{ _pMatrix=aPick_.pMatrix(); _index=aPick_.index(); }

inline MSMatrixSTypePick<double>& MSMatrixSTypePick<double>::operator=(double aType_)
{ _pMatrix->set(_index,aType_); return *this; }

inline MSTypeMatrix<double> *MSMatrixSTypePick<double>::pMatrix(void) const
{ return _pMatrix; }

inline unsigned MSMatrixSTypePick<double>::index(void) const
{ return _index; }

inline MSMatrixSTypePick<double>::operator double() const
{ return _pMatrix->elementAt(_index); }

inline double MSMatrixSTypePick<double>::value(void) const
{ return _pMatrix->elementAt(_index); }

//------------------------------------------------------------
// Pick Indexing operators - define here to avoid inline problem
//------------------------------------------------------------
inline MSMatrixSTypePick<double> MSTypeMatrix<double>::operator()(unsigned index_)    
{ return MSMatrixSTypePick<double>(*this,index_); }

inline MSMatrixSTypePick<double> MSTypeMatrix<double>::operator[](unsigned index_)
{ return MSMatrixSTypePick<double>(*this,index_); }

inline MSMatrixSTypePick<double> MSTypeMatrix<double>::operator()(unsigned row_,unsigned column_) 
{ return MSMatrixSTypePick<double>(*this,index(row_,column_)); }

typedef MSTypeMatrix<double> MSFloatMatrix;

#ifndef MS_MSFloatMatrix_INSTANTIATE
#ifdef MS_DECLARE_TEMPLATE_SPECIALIZATION
template <> ostream& operator<<(ostream&,const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> operator-(const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> operator+(const MSTypeMatrix<double>&,double);
template <> MSTypeMatrix<double> operator+(double,const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> operator+(const MSTypeMatrix<double>&,const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> operator-(const MSTypeMatrix<double>&,double);
template <> MSTypeMatrix<double> operator-(const MSTypeMatrix<double>&,const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> operator*(const MSTypeMatrix<double>&,double);
template <> MSTypeMatrix<double> operator*(double,const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> operator*(const MSTypeMatrix<double>&,const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> operator/(const MSTypeMatrix<double>&,double);
template <> MSTypeMatrix<double> operator/(const MSTypeMatrix<double>&,const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> operator+(const MSTypeMatrix<double>&,const MSTypeVector<double>&);
template <> MSTypeMatrix<double> operator-(const MSTypeMatrix<double>&,const MSTypeVector<double>&);
template <> MSTypeMatrix<double> operator*(const MSTypeMatrix<double>&,const MSTypeVector<double>&);
template <> MSTypeMatrix<double> operator/(const MSTypeMatrix<double>&,const MSTypeVector<double>&);
template <> MSTypeMatrix<double> operator+(const MSTypeVector<double>&,const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> operator*(const MSTypeVector<double>&,const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> multiply(const MSTypeMatrix<double>&,const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> stack(const MSTypeMatrix<double>&,const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> adjoin(const MSTypeMatrix<double>&,const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> allElementsDo(const MSTypeMatrix<double>&,MathFunction);
template <> MSTypeMatrix<double> allElementsDo(const MSTypeMatrix<double>&,ElementWiseFunction,void *clientData_);
template <> MSTypeMatrix<double> sin(const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> sinh(const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> cos(const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> cosh(const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> tan(const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> tanh(const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> asin(const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> acos(const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> atan(const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> exp(const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> log(const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> sqrt(const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> ceil(const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> floor(const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> abs(const MSTypeMatrix<double>&);
template <> MSTypeMatrix<double> pow(const MSTypeMatrix<double>&,double);
#endif
#endif

// We provide provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSTYPESLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSAllocator<double>
#pragma do_not_instantiate MSTypeData<double, MSAllocator<double> >
#endif // !__MSTYPESLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif
