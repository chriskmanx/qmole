#ifndef MSTypeMatrixHEADER
#define MSTypeMatrixHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSMatrix.H>
#include <MSTypes/MSBinaryMatrix.H>
#include <MSTypes/MSTypeData.H>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSTypeMatrix.C")
#endif

// These are specializations of MSTypeMatrix
// and need to be included before MSTypeMatrix
// is defined or else lcc cannot deal correctly
// with automatic template instantiation.
// Same for Visual C++
#if defined(MS_NEED_TEMPLATE_SPECIALIZATION_BUG)
#if !defined(__MSTYPESLIBRARY_BUILD__)
#include <MSTypes/MSFloatMatrix.H>
#endif
#endif

template<class Type> class MSTypeMatrix;
template<class Type> class MSTypeVector;
template<class Type> class MSMatrixSTypePick; // MSTypeMatrix indexed by an unsigned int

class MSBinaryVector;

#if defined(MS_PREDECLARE_TEMPLATE_FRIENDS)
template<class Type> class MSTypeMatrix;
template<class Type> ostream& operator<<(ostream&, const MSTypeMatrix<Type>&);
template<class Type> MSTypeMatrix<Type> operator-(const MSTypeMatrix<Type>&);
template<class Type> inline MSBoolean operator<(const MSTypeMatrix<Type>&, const MSTypeMatrix<Type>&);
template<class Type> inline MSBoolean operator>(const MSTypeMatrix<Type>&, const MSTypeMatrix<Type>&);
template<class Type> inline MSBoolean operator<=(const MSTypeMatrix<Type>&, const MSTypeMatrix<Type>&);
template<class Type> inline MSBoolean operator>=(const MSTypeMatrix<Type>&, const MSTypeMatrix<Type>&);
template<class Type> inline MSBoolean operator<(const MSTypeMatrix<Type>&, Type);
template<class Type> inline MSBoolean operator<(Type, const MSTypeMatrix<Type>&);
template<class Type> inline MSBoolean operator>(const MSTypeMatrix<Type>&, Type);
template<class Type> inline MSBoolean operator>(Type, const MSTypeMatrix<Type>&);
template<class Type> inline MSBoolean operator<=(const MSTypeMatrix<Type>&, Type);
template<class Type> inline MSBoolean operator<=(Type, const MSTypeMatrix<Type>&);
template<class Type> inline MSBoolean operator>=(const MSTypeMatrix<Type>&, Type);
template<class Type> inline MSBoolean operator>=(Type, const MSTypeMatrix<Type>&);

template<class Type> inline MSBoolean operator==(const MSTypeMatrix<Type>&, Type);
template<class Type> inline MSBoolean operator==(Type, const MSTypeMatrix<Type>&);
template<class Type> inline MSBoolean operator!=(const MSTypeMatrix<Type>&, Type);
template<class Type> inline MSBoolean operator!=(Type, const MSTypeMatrix<Type>&);
template<class Type> inline MSBoolean operator==(const MSTypeMatrix<Type>&, const MSTypeMatrix<Type>&);
template<class Type> inline MSBoolean operator!=(const MSTypeMatrix<Type>&, const MSTypeMatrix<Type>&);

template<class Type> MSTypeMatrix<Type> operator+(const MSTypeMatrix<Type>&, Type);
template<class Type> MSTypeMatrix<Type> operator+(Type, const MSTypeMatrix<Type>&);
template<class Type> MSTypeMatrix<Type> operator+(const MSTypeMatrix<Type>&, const MSTypeMatrix<Type>&);
template<class Type> MSTypeMatrix<Type> operator-(const MSTypeMatrix<Type>&, Type);
template<class Type> MSTypeMatrix<Type> operator-(const MSTypeMatrix<Type>&, const MSTypeMatrix<Type>&);
template<class Type> MSTypeMatrix<Type> operator*(const MSTypeMatrix<Type>&, Type);
template<class Type> MSTypeMatrix<Type> operator*(Type, const MSTypeMatrix<Type>&);
template<class Type> MSTypeMatrix<Type> operator*(const MSTypeMatrix<Type>&, const MSTypeMatrix<Type>&);
template<class Type> MSTypeMatrix<Type> operator/(const MSTypeMatrix<Type>&, Type);
template<class Type> MSTypeMatrix<Type> operator/(const MSTypeMatrix<Type>&, const MSTypeMatrix<Type>&);
template<class Type> MSTypeMatrix<Type> operator+(const MSTypeMatrix<Type>&, const MSTypeVector<Type>&);
template<class Type> MSTypeMatrix<Type> operator-(const MSTypeMatrix<Type>&, const MSTypeVector<Type>&);
template<class Type> MSTypeMatrix<Type> operator*(const MSTypeMatrix<Type>&, const MSTypeVector<Type>&);
template<class Type> MSTypeMatrix<Type> operator/(const MSTypeMatrix<Type>&, const MSTypeVector<Type>&);
template<class Type> MSTypeMatrix<Type> operator+(const MSTypeVector<Type>&, const MSTypeMatrix<Type>&);
template<class Type> MSTypeMatrix<Type> operator*(const MSTypeVector<Type>&, const MSTypeMatrix<Type>&);
#endif // MS_PREDECLARE_TEMPLATE_FRIENDS

template<class Type> class MSTypeMatrix : public MSMatrix
{
public:
  MSTypeMatrix(void);
  MSTypeMatrix(unsigned rows_,unsigned columns_);
  MSTypeMatrix(unsigned rows_,unsigned columns_,Type fill_);
  MSTypeMatrix(const MSTypeMatrix<Type>&);
  MSTypeMatrix(MSTypeData<Type,MSAllocator<Type> > *dp_,unsigned rows_,unsigned columns_);
  MSTypeMatrix(const Type *, unsigned rows_, unsigned columns_);
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
  MSError::ErrorStatus set(unsigned index_,Type); 

  unsigned indexOf(Type aType_,unsigned startPos_=0) const;
  unsigned lastIndexOf(Type aType_,unsigned startPos_=UINT_MAX-1) const;

  inline const Type& elementAt(unsigned) const;
  inline const Type& elementAt(unsigned,unsigned) const;

  MSTypeMatrix<Type>& series(Type offset_=0);

  MSTypeMatrix<Type>& random(unsigned long limit_=0);
  Type min(void) const;
  Type max(void) const;
  double sum(void) const;

  MSTypeMatrix<Type>& operator=(const MSTypeMatrix<Type>&);
  MSTypeMatrix<Type>& operator=(const MSMatrixSTypePick<Type>&);
  MSTypeMatrix<Type>& operator=(Type);

  inline Type operator()(unsigned index_) const;
  inline Type operator[](unsigned index_) const;
  inline Type operator()(unsigned row_,unsigned column_) const;

  inline MSMatrixSTypePick<Type> operator()(unsigned index_);
  inline MSMatrixSTypePick<Type> operator[](unsigned index_);
  inline MSMatrixSTypePick<Type> operator()(unsigned row_,unsigned column_);
  
  friend MSTypesExport ostream& operator<< MS_FRIEND_TEMPLATEID (ostream&,const MSTypeMatrix<Type>&);

  friend MSTypesExport MSTypeMatrix<Type> operator- MS_FRIEND_TEMPLATEID (const MSTypeMatrix<Type>&);
  MSTypeMatrix<Type>& operator++();       // Prefix
  MSTypeMatrix<Type>& operator--();
  MSTypeMatrix<Type> operator++(int);    // Postfix
  MSTypeMatrix<Type> operator--(int);

  long compare(const MSTypeMatrix<Type>&) const;
  //inline friend MSTypesExport long compare(const MSTypeMatrix<Type>&,const MSTypeMatrix<Type>&);

  MSBinaryMatrix binaryCompare(const MSTypeMatrix<Type>&,MSComparison) const;
  MSBinaryMatrix binaryCompare(Type,MSComparison) const;
  MSBoolean scalarCompare(Type,MSComparison) const;

  INLINELINKAGE2 friend MSTypesExport MSBoolean operator< MS_FRIEND_TEMPLATEID (const MSTypeMatrix<Type>&,
                                                                        const MSTypeMatrix<Type>&);
  INLINELINKAGE2 friend MSTypesExport MSBoolean operator> MS_FRIEND_TEMPLATEID (const MSTypeMatrix<Type>&,
                                                                        const MSTypeMatrix<Type>&);
  INLINELINKAGE2 friend MSTypesExport MSBoolean operator<= MS_FRIEND_TEMPLATEID (const MSTypeMatrix<Type>&,
                                                                         const MSTypeMatrix<Type>&);
  INLINELINKAGE2 friend MSTypesExport MSBoolean operator>= MS_FRIEND_TEMPLATEID (const MSTypeMatrix<Type>&,
                                                                         const MSTypeMatrix<Type>&);

  INLINELINKAGE2 friend MSTypesExport MSBoolean operator< MS_FRIEND_TEMPLATEID (const MSTypeMatrix<Type>&,Type);
  INLINELINKAGE2 friend MSTypesExport MSBoolean operator< MS_FRIEND_TEMPLATEID (Type,const MSTypeMatrix<Type>&);
  INLINELINKAGE2 friend MSTypesExport MSBoolean operator> MS_FRIEND_TEMPLATEID (const MSTypeMatrix<Type>&,Type);
  INLINELINKAGE2 friend MSTypesExport MSBoolean operator> MS_FRIEND_TEMPLATEID (Type,const MSTypeMatrix<Type>&);
  INLINELINKAGE2 friend MSTypesExport MSBoolean operator<= MS_FRIEND_TEMPLATEID (const MSTypeMatrix<Type>&,Type);
  INLINELINKAGE2 friend MSTypesExport MSBoolean operator<= MS_FRIEND_TEMPLATEID (Type,const MSTypeMatrix<Type>&);
  INLINELINKAGE2 friend MSTypesExport MSBoolean operator>= MS_FRIEND_TEMPLATEID (const MSTypeMatrix<Type>&,Type);
  INLINELINKAGE2 friend MSTypesExport MSBoolean operator>= MS_FRIEND_TEMPLATEID (Type,const MSTypeMatrix<Type>&);
  INLINELINKAGE2 friend MSTypesExport MSBoolean operator== MS_FRIEND_TEMPLATEID(const MSTypeMatrix<Type>&,Type);
  INLINELINKAGE2 friend MSTypesExport MSBoolean operator== MS_FRIEND_TEMPLATEID (Type,const MSTypeMatrix<Type>&);
  INLINELINKAGE2 friend MSTypesExport MSBoolean operator!= MS_FRIEND_TEMPLATEID (const MSTypeMatrix<Type>&,Type);
  INLINELINKAGE2 friend MSTypesExport MSBoolean operator!= MS_FRIEND_TEMPLATEID(Type,const MSTypeMatrix<Type>&);
  INLINELINKAGE2 friend MSTypesExport MSBoolean operator== MS_FRIEND_TEMPLATEID(const MSTypeMatrix<Type>&,const MSTypeMatrix<Type>&);
  INLINELINKAGE2 friend MSTypesExport MSBoolean operator!= MS_FRIEND_TEMPLATEID(const MSTypeMatrix<Type>&,const MSTypeMatrix<Type>&);

  friend MSTypesExport MSTypeMatrix<Type> operator+ MS_FRIEND_TEMPLATEID (const MSTypeMatrix<Type>&,Type);
  friend MSTypesExport MSTypeMatrix<Type> operator+ MS_FRIEND_TEMPLATEID (Type,const MSTypeMatrix<Type>&);
  friend MSTypesExport MSTypeMatrix<Type> operator+ MS_FRIEND_TEMPLATEID (const MSTypeMatrix<Type>&,
                                                                          const MSTypeMatrix<Type>&);
  MSTypeMatrix<Type>& operator+=(Type);

  friend MSTypesExport MSTypeMatrix<Type> operator- MS_FRIEND_TEMPLATEID (const MSTypeMatrix<Type>&,Type);
  friend MSTypesExport MSTypeMatrix<Type> operator- MS_FRIEND_TEMPLATEID (const MSTypeMatrix<Type>&,
                                                                          const MSTypeMatrix<Type>&);
  MSTypeMatrix<Type>& operator-=(Type);

  friend MSTypesExport MSTypeMatrix<Type> operator* MS_FRIEND_TEMPLATEID (const MSTypeMatrix<Type>&,Type);
  friend MSTypesExport MSTypeMatrix<Type> operator* MS_FRIEND_TEMPLATEID (Type,const MSTypeMatrix<Type>&);
  friend MSTypesExport MSTypeMatrix<Type> operator* MS_FRIEND_TEMPLATEID (const MSTypeMatrix<Type>&,
                                                                          const MSTypeMatrix<Type>&);
  MSTypeMatrix<Type>& operator*=(Type);

  friend MSTypesExport MSTypeMatrix<Type> operator/ MS_FRIEND_TEMPLATEID (const MSTypeMatrix<Type>&,Type);
  friend MSTypesExport MSTypeMatrix<Type> operator/ MS_FRIEND_TEMPLATEID (const MSTypeMatrix<Type>&,
                                                                          const MSTypeMatrix<Type>&);
  MSTypeMatrix<Type>& operator/=(Type);

  // The following operators are equivalent to the apl operation m_ (x @1 0) v_
  friend MSTypesExport MSTypeMatrix<Type> operator+ MS_FRIEND_TEMPLATEID (const MSTypeMatrix<Type>& m_,
                                                                          const MSTypeVector<Type>& v_);
  friend MSTypesExport MSTypeMatrix<Type> operator- MS_FRIEND_TEMPLATEID (const MSTypeMatrix<Type>& m_,
                                                                          const MSTypeVector<Type>& v_);
  friend MSTypesExport MSTypeMatrix<Type> operator* MS_FRIEND_TEMPLATEID (const MSTypeMatrix<Type>& m_,
                                                                          const MSTypeVector<Type>& v_);
  friend MSTypesExport MSTypeMatrix<Type> operator/ MS_FRIEND_TEMPLATEID (const MSTypeMatrix<Type>& m_,
                                                                          const MSTypeVector<Type>& v_);

  // The following operators are equivalent to the apl operation v_ (x @0 1) m_
  friend MSTypesExport MSTypeMatrix<Type> operator+ MS_FRIEND_TEMPLATEID (const MSTypeVector<Type>& v_,
                                                                          const MSTypeMatrix<Type>& m_);
  friend MSTypesExport MSTypeMatrix<Type> operator* MS_FRIEND_TEMPLATEID (const MSTypeVector<Type>& v_,
                                                                          const MSTypeMatrix<Type>& m_);

  // matrix multiplication (inner product)
  static MSTypeMatrix<Type> multiply(const MSTypeMatrix<Type>&,
                                     const MSTypeMatrix<Type>&);

  // stack matrices vertically
  MSTypeMatrix<Type>& stack(const MSTypeMatrix<Type>&);
  static MSTypeMatrix<Type> stack(const MSTypeMatrix<Type>&, 
				  const MSTypeMatrix<Type>&);

  // adjoin matrices horizontally
  MSTypeMatrix<Type>& adjoin(const MSTypeMatrix<Type>&);
  static MSTypeMatrix<Type> adjoin(const MSTypeMatrix<Type>&,
                                   const MSTypeMatrix<Type>&);

  MSTypeMatrix<Type>& appendColumn(const MSTypeVector<Type>&);
  MSTypeMatrix<Type>& appendColumns(unsigned columns_, Type fill_=0);
  MSTypeMatrix<Type>& appendRow(const MSTypeVector<Type>&);
  MSTypeMatrix<Type>& appendRows(unsigned rows_, Type fill_=0);
  MSTypeMatrix<Type>& exchangeColumns(unsigned a_,unsigned b_);
  MSTypeMatrix<Type>& exchangeRows(unsigned a_,unsigned b_);
  MSTypeMatrix<Type>& insertColumnBefore(unsigned column_,Type fill_=0);
  MSTypeMatrix<Type>& insertColumnBefore(unsigned column_,const MSTypeVector<Type>&);
  MSTypeMatrix<Type>& insertColumnAfter(unsigned column_,Type fill_=0);
  MSTypeMatrix<Type>& insertColumnAfter(unsigned column_,const MSTypeVector<Type>&);
  MSTypeMatrix<Type>& insertRowBefore(unsigned row_,Type fill_=0);
  MSTypeMatrix<Type>& insertRowBefore(unsigned row_,const MSTypeVector<Type>&);
  MSTypeMatrix<Type>& insertRowAfter(unsigned row_,Type fill_=0);
  MSTypeMatrix<Type>& insertRowAfter(unsigned row_,const MSTypeVector<Type>&);
  MSTypeMatrix<Type>& reshape(unsigned rows_,unsigned columns_);
  MSTypeMatrix<Type>& removeAll(void);
  MSTypeMatrix<Type>& removeAllRows(void);
  MSTypeMatrix<Type>& removeAllColumns(void);
  MSTypeMatrix<Type>& assignRow(unsigned row_, Type scalar_);
  MSTypeMatrix<Type>& assignColumn(unsigned column_, Type scalar_);
  MSTypeMatrix<Type>& assignColumn(unsigned column_,const MSTypeVector<Type>&);
  MSTypeMatrix<Type>& assignRow(unsigned row_,const MSTypeVector<Type>&);
  MSTypeMatrix<Type>& removeRow(unsigned row_);
  MSTypeMatrix<Type>& removeColumn(unsigned column_);
  MSTypeMatrix<Type>& compressRows(const MSBinaryVector&);
  MSTypeMatrix<Type>& compressColumns(const MSBinaryVector&);
  MSTypeMatrix<Type>& reverseRows(void);
  MSTypeMatrix<Type>& reverseColumns(void);
  MSTypeMatrix<Type>& transpose(void);
  MSTypeMatrix<Type>& rotateRows(int position_);
  MSTypeMatrix<Type>& rotateColumns(int position_);
  MSTypeMatrix<Type>& takeRows(int numberOfRows_);
  MSTypeMatrix<Type>& takeColumns(int numberOfColumns_);
  MSTypeMatrix<Type>& dropRows(int numberOfRows_);
  MSTypeMatrix<Type>& dropColumns(int numberOfColumns_);

  MSTypeVector<Type> rowAt(unsigned row_) const;
  MSTypeVector<Type> columnAt(unsigned column_) const;

  void error(const char *) const;
  inline Type *data(void) const;
  inline unsigned size(void) const;
  inline unsigned length(void) const;
  inline MSTypeData<Type,MSAllocator<Type> > *pData(void) const;

protected:
  friend class MSMatrixSTypePick<Type>;

  static Type           _badData; // used as return value for index errors
  MSTypeData<Type,MSAllocator<Type> >     *_pData;
//  unsigned              _size;
  MSBoolean             _blocked;

//  inline MSTypeData<Type,MSAllocator<Type> > *pData(void) const;
  inline MSBoolean blocked(void) const;
  inline void blocked(MSBoolean);

  void reserve(unsigned);
  void blockLeft(unsigned target_,unsigned moveCount_);
  void blockRight(unsigned target_,unsigned moveCount_);  
  void freeData(void);
  void allocData(unsigned);
  void makeUniqueCopy(void);
  void prepareToChange(void);
  
  inline MSTypeData<Type,MSAllocator<Type> > *incrementCount(void);
  inline void decrementCount(void);

  inline Type& elementAt(unsigned);
  inline Type& elementAt(unsigned,unsigned);
};
  
template<class Type> class MSMatrixSTypePick
{
public:
  inline MSMatrixSTypePick<Type>& operator=(Type);

  MSMatrixSTypePick<Type>& operator+=(Type);
  MSMatrixSTypePick<Type>& operator-=(Type);
  MSMatrixSTypePick<Type>& operator*=(Type);
  MSMatrixSTypePick<Type>& operator/=(Type);

  MSMatrixSTypePick<Type>& operator++(int);
  MSMatrixSTypePick<Type>& operator--(int);

  inline operator Type() const;

  inline MSTypeMatrix<Type> *pMatrix(void) const;
  inline Type value(void) const;
  inline unsigned index(void) const;

private:
  friend class MSTypeMatrix<Type>;

  inline MSMatrixSTypePick(const MSTypeMatrix<Type>&,unsigned);
  inline MSMatrixSTypePick(const MSMatrixSTypePick<Type>&);

  MSTypeMatrix<Type>    *_pMatrix;
  unsigned               _index;
};

//------------------------------------------------------------
// MSTypeMatrix inlines
//------------------------------------------------------------
template<class Type>
inline MSTypeData<Type,MSAllocator<Type> > *MSTypeMatrix<Type>::pData(void) const
{ return _pData; }

template<class Type>
inline unsigned MSTypeMatrix<Type>::size(void) const 
{ return _pData->size(); }
//{ return _size; }

template<class Type>
inline unsigned MSTypeMatrix<Type>::length(void) const 
{ return _count; }

template<class Type>
inline MSBoolean MSTypeMatrix<Type>::blocked(void) const 
{ return _blocked; }

template<class Type>
inline void MSTypeMatrix<Type>::blocked(MSBoolean blocked_)
{ _blocked=blocked_; }

template<class Type>
inline Type& MSTypeMatrix<Type>::elementAt(unsigned index_)
{
#if !defined (MS_NO_INDEX_ERROR)
  if(index_<_count) return _pData->elementAt(index_); else return indexError(index_,length()),_badData;
#else
  return _pData->elementAt(index_);
#endif
}

template<class Type>
inline const Type& MSTypeMatrix<Type>::elementAt(unsigned index_) const
{
#if !defined (MS_NO_INDEX_ERROR)
  if(index_<_count) return _pData->elementAt(index_); else return indexError(index_,length()),_badData;
#else
  return _pData->elementAt(index_);  
#endif
}

template<class Type>
inline Type& MSTypeMatrix<Type>::elementAt(unsigned row_,unsigned column_)
{
#if !defined (MS_NO_INDEX_ERROR)
  unsigned ravel=index(row_,column_);
  if(ravel<_count) return _pData->elementAt(ravel); else return indexError(ravel,length()),_badData; 
#else
  return _pData->elementAt(index(row_,column_));
#endif
}

template<class Type>
inline const Type& MSTypeMatrix<Type>::elementAt(unsigned row_,unsigned column_) const
{
#if !defined (MS_NO_INDEX_ERROR)
  unsigned ravel=index(row_,column_);
  if(ravel<_count) return _pData->elementAt(ravel); else return indexError(ravel,length()),_badData;
#else
  return _pData->elementAt(index(row_,column_));  
#endif
}

template<class Type>
inline Type MSTypeMatrix<Type>::operator()(unsigned index_) const
{ return elementAt(index_); }

template<class Type>
inline Type MSTypeMatrix<Type>::operator[](unsigned index_) const
{ return elementAt(index_); }

template<class Type>
inline Type MSTypeMatrix<Type>::operator()(unsigned row_,unsigned column_) const
{ return elementAt(index(row_,column_)); }

template<class Type>
inline MSTypeData<Type,MSAllocator<Type> > *MSTypeMatrix<Type>::incrementCount(void)
{ return (_pData!=0)?_pData->incrementCount():0; }

template<class Type>
inline void MSTypeMatrix<Type>::decrementCount(void)
{ if (_pData!=0) _pData->decrementCount(); }

template<class Type>
inline Type *MSTypeMatrix<Type>::data(void) const
{ return (_pData!=0)?_pData->elements():0; }


template<class Type>
inline MSBoolean operator<(const MSTypeMatrix<Type>& aTypeMatrix_, const MSTypeMatrix<Type>& bTypeMatrix_)
{ return MSBoolean(aTypeMatrix_.compare(bTypeMatrix_)>0); }

template<class Type>
inline MSBoolean operator>(const MSTypeMatrix<Type>& aTypeMatrix_, const MSTypeMatrix<Type>& bTypeMatrix_)
{ return MSBoolean(aTypeMatrix_.compare(bTypeMatrix_)<0); }

template<class Type>
inline MSBoolean operator<=(const MSTypeMatrix<Type>& aTypeMatrix_, const MSTypeMatrix<Type>& bTypeMatrix_)
{ return MSBoolean(aTypeMatrix_.compare(bTypeMatrix_)>=0); }

template<class Type>
inline MSBoolean operator>=(const MSTypeMatrix<Type>& aTypeMatrix_ ,const MSTypeMatrix<Type>& bTypeMatrix_)
{ return MSBoolean(aTypeMatrix_.compare(bTypeMatrix_)<=0); }

template<class Type>
inline MSBoolean operator<(const MSTypeMatrix<Type>& aTypeMatrix_, Type aType_)
{ return aTypeMatrix_.scalarCompare(aType_,MSLessThan); }

template<class Type>
inline MSBoolean operator<(Type aType_,const MSTypeMatrix<Type>& aTypeMatrix_)
{ return aTypeMatrix_.scalarCompare(aType_,MSGreaterThan); }

template<class Type>
inline MSBoolean operator>(const MSTypeMatrix<Type>& aTypeMatrix_,Type aType_)
{ return aTypeMatrix_.scalarCompare(aType_,MSGreaterThan); }

template<class Type>
inline MSBoolean operator>(Type aType_,const MSTypeMatrix<Type>& aTypeMatrix_)
{ return aTypeMatrix_.scalarCompare(aType_,MSLessThan); }

template<class Type>
inline MSBoolean operator<=(const MSTypeMatrix<Type>& aTypeMatrix_,Type aType_)
{ return aTypeMatrix_.scalarCompare(aType_,MSLessThanOrEqualTo); }

template<class Type>
inline MSBoolean operator<=(Type aType_,const MSTypeMatrix<Type>& aTypeMatrix_)
{ return aTypeMatrix_.scalarCompare(aType_,MSGreaterThanOrEqualTo); }

template<class Type>
inline MSBoolean operator>=(const MSTypeMatrix<Type>& aTypeMatrix_,Type aType_)
{ return aTypeMatrix_.scalarCompare(aType_,MSGreaterThanOrEqualTo); }

template<class Type>
inline MSBoolean operator>=(Type aType_,const MSTypeMatrix<Type>& aTypeMatrix_)
{ return aTypeMatrix_.scalarCompare(aType_,MSLessThanOrEqualTo); }

template<class Type>
inline MSBoolean operator==(const MSTypeMatrix<Type>& aTypeMatrix_,Type aType_)
{ return aTypeMatrix_.scalarCompare(aType_,MSEqualTo); }

template<class Type>
inline MSBoolean operator==(Type aType_,const MSTypeMatrix<Type>& aTypeMatrix_)
{ return aTypeMatrix_.scalarCompare(aType_,MSEqualTo); }

template<class Type>
inline MSBoolean operator!=(const MSTypeMatrix<Type>& aTypeMatrix_,Type aType_)
{ return aTypeMatrix_.scalarCompare(aType_,MSNotEqualTo); }

template<class Type>
inline MSBoolean operator!=(Type aType_,const MSTypeMatrix<Type>& aTypeMatrix_)
{ return aTypeMatrix_.scalarCompare(aType_,MSNotEqualTo); }

template<class Type>
inline MSBoolean operator==(const MSTypeMatrix<Type>& a_, const MSTypeMatrix<Type>& b_)
{ return MSBoolean(a_.compare(b_)==0);}

template<class Type>
inline MSBoolean operator!=(const MSTypeMatrix<Type>& a_, const MSTypeMatrix<Type>& b_)
{ return MSBoolean(a_.compare(b_)!=0);}


//------------------------------------------------------------
// MSMatrixSTypePick inlines
//------------------------------------------------------------
template<class Type>
inline MSMatrixSTypePick<Type>::MSMatrixSTypePick(const MSTypeMatrix<Type>& aTypeMatrix_,unsigned index_)
{ _pMatrix=&(MSTypeMatrix<Type>&)aTypeMatrix_; _index=index_; }

template<class Type>
inline MSMatrixSTypePick<Type>::MSMatrixSTypePick(const MSMatrixSTypePick<Type>& aPick_)
{ _pMatrix=aPick_.pMatrix(); _index=aPick_.index(); }

template<class Type>
inline MSMatrixSTypePick<Type>& MSMatrixSTypePick<Type>::operator=(Type aType_)
{ _pMatrix->set(_index,aType_); return *this; }

template<class Type>
inline MSTypeMatrix<Type> *MSMatrixSTypePick<Type>::pMatrix(void) const
{ return _pMatrix; }

template<class Type>
inline unsigned MSMatrixSTypePick<Type>::index(void) const
{ return _index; }

template<class Type>
inline MSMatrixSTypePick<Type>::operator Type() const
{ return _pMatrix->elementAt(_index); }

template<class Type>
inline Type MSMatrixSTypePick<Type>::value(void) const
{ return _pMatrix->elementAt(_index); }

//------------------------------------------------------------
// Pick Indexing operators - define here to avoid inline problem
//------------------------------------------------------------
template<class Type>
inline MSMatrixSTypePick<Type> MSTypeMatrix<Type>::operator()(unsigned index_)    
{ return MSMatrixSTypePick<Type>(*this,index_); }

template<class Type>
inline MSMatrixSTypePick<Type> MSTypeMatrix<Type>::operator[](unsigned index_)
{ return MSMatrixSTypePick<Type>(*this,index_); }

template<class Type>
inline MSMatrixSTypePick<Type> MSTypeMatrix<Type>::operator()(unsigned row_,unsigned column_) 
{ return MSMatrixSTypePick<Type>(*this,index(row_,column_)); }

#endif
