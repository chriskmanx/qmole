#ifndef MSBinaryMatrixHEADER
#define MSBinaryMatrixHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSMatrix.H>
#include <MSTypes/MSTypeData.H>
#include <MSTypes/MSAllocator.H>

class MSBinaryVector;
class MSBinaryMatrixSTypePick; // MSBinaryMatrix indexed by an unsigned int

class MSTypesExport MSBinaryMatrix : public MSMatrix
{
public:
  MSBinaryMatrix(void);
  MSBinaryMatrix(unsigned rows_,unsigned columns_);
  MSBinaryMatrix(unsigned rows_,unsigned columns_,unsigned char fill_);
  MSBinaryMatrix(const MSBinaryMatrix&);
  MSBinaryMatrix(MSTypeData<unsigned char,MSAllocator<unsigned char> > *dp_,unsigned rows_,unsigned columns_);
  MSBinaryMatrix(const unsigned char *pElements_, unsigned rows_, unsigned cols_);
  ~MSBinaryMatrix(void);

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
  inline static const MSSymbol& symbol(void);

  MSError::ErrorStatus set(unsigned index_,const char *pString_); 
  MSError::ErrorStatus set(unsigned index_,unsigned char); 

  unsigned indexOf(unsigned char aBinaryValue_,unsigned startPos_=0) const;
  unsigned lastIndexOf(unsigned char aBinaryValue_,unsigned startPos_=UINT_MAX-1) const;

  inline const unsigned char& elementAt(unsigned) const;
  inline const unsigned char& elementAt(unsigned,unsigned) const;

  MSBinaryMatrix& random(void);	  // limit is always 2; nothing else makes sense
  unsigned char min(void) const;
  unsigned char max(void) const;
  unsigned long sum(void) const;

  MSBinaryMatrix& operator=(const MSBinaryMatrix&);
  MSBinaryMatrix& operator=(const MSBinaryMatrixSTypePick&);
  MSBinaryMatrix& operator=(unsigned char);

  inline unsigned char operator()(unsigned index_) const;
  inline unsigned char operator[](unsigned index_) const;
  inline unsigned char operator()(unsigned row_,unsigned column_) const;

  inline MSBinaryMatrixSTypePick operator()(unsigned index_); 
  inline MSBinaryMatrixSTypePick operator[](unsigned index_); 
  inline MSBinaryMatrixSTypePick operator()(unsigned row_,unsigned column_); 
  
  friend MSTypesExport ostream& operator<<(ostream&,const MSBinaryMatrix&);

  long compare(const MSBinaryMatrix&) const;
  inline long compare(const MSBinaryMatrix&,const MSBinaryMatrix&);

  MSBinaryMatrix binaryCompare(const MSBinaryMatrix&,MSComparison) const;
  MSBinaryMatrix binaryCompare(unsigned char,MSComparison) const;
  MSBoolean scalarCompare(unsigned char,MSComparison) const;

  inline MSBoolean operator==(const MSBinaryMatrix&) const;
  inline MSBoolean operator!=(const MSBinaryMatrix&) const;

  inline friend MSTypesExport MSBoolean operator<(const MSBinaryMatrix&,const MSBinaryMatrix&);
  inline friend MSTypesExport MSBoolean operator>(const MSBinaryMatrix&,const MSBinaryMatrix&);
  inline friend MSTypesExport MSBoolean operator<=(const MSBinaryMatrix&,const MSBinaryMatrix&);
  inline friend MSTypesExport MSBoolean operator>=(const MSBinaryMatrix&,const MSBinaryMatrix&);
	    
  inline friend MSTypesExport MSBoolean operator<(const MSBinaryMatrix&,unsigned char);
  inline friend MSTypesExport MSBoolean operator<(unsigned char,const MSBinaryMatrix&);
  inline friend MSTypesExport MSBoolean operator>(const MSBinaryMatrix&,unsigned char);
  inline friend MSTypesExport MSBoolean operator>(unsigned char,const MSBinaryMatrix&);
  inline friend MSTypesExport MSBoolean operator<=(const MSBinaryMatrix&,unsigned char);
  inline friend MSTypesExport MSBoolean operator<=(unsigned char,const MSBinaryMatrix&);
  inline friend MSTypesExport MSBoolean operator>=(const MSBinaryMatrix&,unsigned char);
  inline friend MSTypesExport MSBoolean operator>=(unsigned char,const MSBinaryMatrix&);
  inline friend MSTypesExport MSBoolean operator==(const MSBinaryMatrix&,unsigned char);
  inline friend MSTypesExport MSBoolean operator==(unsigned char,const MSBinaryMatrix&);
  inline friend MSTypesExport MSBoolean operator!=(const MSBinaryMatrix&,unsigned char);
  inline friend MSTypesExport MSBoolean operator!=(unsigned char,const MSBinaryMatrix&);

  friend MSTypesExport MSBinaryMatrix operator!(const MSBinaryMatrix&);
  friend MSTypesExport MSBinaryMatrix operator~(const MSBinaryMatrix&);

  friend MSTypesExport MSBinaryMatrix operator&(const MSBinaryMatrix&,unsigned char);
  friend MSTypesExport MSBinaryMatrix operator&(unsigned char,const MSBinaryMatrix&);
  friend MSTypesExport MSBinaryMatrix operator&(const MSBinaryMatrix&,const MSBinaryMatrix&);

  MSBinaryMatrix& operator&=(const MSBinaryMatrix&);
  MSBinaryMatrix& operator&=(unsigned char);

  friend MSTypesExport MSBinaryMatrix operator|(const MSBinaryMatrix&,unsigned char);
  friend MSTypesExport MSBinaryMatrix operator|(unsigned char,const MSBinaryMatrix&);
  friend MSTypesExport MSBinaryMatrix operator|(const MSBinaryMatrix&,const MSBinaryMatrix&);

  MSBinaryMatrix& operator|=(const MSBinaryMatrix&);
  MSBinaryMatrix& operator|=(unsigned char);

  friend MSTypesExport MSBinaryMatrix operator^(const MSBinaryMatrix&,unsigned char);
  friend MSTypesExport MSBinaryMatrix operator^(unsigned char,const MSBinaryMatrix&);
  friend MSTypesExport MSBinaryMatrix operator^(const MSBinaryMatrix&,const MSBinaryMatrix&);

  MSBinaryMatrix& operator^=(const MSBinaryMatrix&);
  MSBinaryMatrix& operator^=(unsigned char);

  // stack matrices vertically
  MSBinaryMatrix& stack(const MSBinaryMatrix&);
  friend MSTypesExport MSBinaryMatrix stack(const MSBinaryMatrix&,const MSBinaryMatrix&);

  // adjoin matrices horizontally
  MSBinaryMatrix& adjoin(const MSBinaryMatrix&);
  friend MSTypesExport MSBinaryMatrix adjoin(const MSBinaryMatrix&,const MSBinaryMatrix&);

  MSBinaryMatrix& appendColumn(const MSBinaryVector&);
  MSBinaryMatrix& appendColumns(unsigned columns_, unsigned char fill_=0);
  MSBinaryMatrix& appendRow(const MSBinaryVector&);
  MSBinaryMatrix& appendRows(unsigned rows_, unsigned char fill_=0);
  MSBinaryMatrix& exchangeColumns(unsigned a_,unsigned b_);
  MSBinaryMatrix& exchangeRows(unsigned a_,unsigned b_);
  MSBinaryMatrix& insertColumnBefore(unsigned column_,unsigned char fill_=0);
  MSBinaryMatrix& insertColumnBefore(unsigned column_,const MSBinaryVector&);
  MSBinaryMatrix& insertColumnAfter(unsigned column_,unsigned char fill_=0);
  MSBinaryMatrix& insertColumnAfter(unsigned column_,const MSBinaryVector&);
  MSBinaryMatrix& insertRowBefore(unsigned row_,unsigned char fill_=0);
  MSBinaryMatrix& insertRowBefore(unsigned row_,const MSBinaryVector&);
  MSBinaryMatrix& insertRowAfter(unsigned row_,unsigned char fill_=0);
  MSBinaryMatrix& insertRowAfter(unsigned row_,const MSBinaryVector&);
  MSBinaryMatrix& reshape(unsigned rows_,unsigned columns_);
  MSBinaryMatrix& removeAll(void);
  MSBinaryMatrix& removeAllRows(void);
  MSBinaryMatrix& removeAllColumns(void);
  MSBinaryMatrix& assignRow(unsigned row_, unsigned char scalar_);
  MSBinaryMatrix& assignColumn(unsigned column_, unsigned char scalar_);
  MSBinaryMatrix& assignColumn(unsigned column_, const MSBinaryVector&);
  MSBinaryMatrix& assignRow(unsigned row_, const MSBinaryVector&);
  MSBinaryMatrix& removeRow(unsigned row_);
  MSBinaryMatrix& removeColumn(unsigned column_);
  MSBinaryMatrix& compressRows(const MSBinaryVector&);
  MSBinaryMatrix& compressColumns(const MSBinaryVector&);
  MSBinaryMatrix& reverseRows(void);
  MSBinaryMatrix& reverseColumns(void);
  MSBinaryMatrix& transpose(void);
  MSBinaryMatrix& rotateRows(int position_);
  MSBinaryMatrix& rotateColumns(int position_);
  MSBinaryMatrix& takeRows(int numberOfRows_);
  MSBinaryMatrix& takeColumns(int numberOfColumns_);
  MSBinaryMatrix& dropRows(int numberOfRows_);
  MSBinaryMatrix& dropColumns(int numberOfColumns_);

  MSBinaryVector rowAt(unsigned row_)  const;
  MSBinaryVector columnAt(unsigned column_) const;

  void error(const char *) const;
  inline unsigned char *data(void) const;
  inline unsigned size(void) const;
  inline unsigned length(void) const;

protected:
  friend class MSBinaryMatrixSTypePick;

  static unsigned char       _badData; // used as return value for index errors
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *_pData;
//  unsigned                   _size;
  MSBoolean                  _blocked;

  inline MSTypeData<unsigned char,MSAllocator<unsigned char> > *pData(void) const;
  inline MSBoolean blocked(void) const;
  inline void blocked(MSBoolean);

  void reserve(unsigned);
  void blockLeft(unsigned target_,unsigned moveCount_);
  void blockRight(unsigned target_,unsigned moveCount_);  
  void freeData(void);
  void allocData(unsigned);
  void makeUniqueCopy(void);
  void prepareToChange(void);
  
  inline MSTypeData<unsigned char,MSAllocator<unsigned char> > *incrementCount(void);
  inline void decrementCount(void);

  inline unsigned char& elementAt(unsigned);
  inline unsigned char& elementAt(unsigned,unsigned);
 
  void appendString(const char *);
};
  
class MSTypesExport MSBinaryMatrixSTypePick
{
public:
  inline MSBinaryMatrixSTypePick& operator=(unsigned char);
  inline operator unsigned char() const;

  inline MSBinaryMatrix *pMatrix(void) const;
  inline unsigned char value(void) const;
  inline unsigned index(void) const;

private:
  friend class MSBinaryMatrix;

  inline MSBinaryMatrixSTypePick(const MSBinaryMatrix&,unsigned);
  inline MSBinaryMatrixSTypePick(const MSBinaryMatrixSTypePick&);

  MSBinaryMatrix    *_pMatrix;
  unsigned           _index;
};

//------------------------------------------------------------
// MSBinaryMatrix inlines
//------------------------------------------------------------
inline MSTypeData<unsigned char,MSAllocator<unsigned char> > *MSBinaryMatrix::pData(void) const
{ return _pData; }

inline unsigned MSBinaryMatrix::size(void) const 
{ return _pData->size(); }
//{ return _size; }

inline unsigned MSBinaryMatrix::length(void) const 
{ return _count; }

inline MSBoolean MSBinaryMatrix::blocked(void) const 
{ return _blocked; }

inline void MSBinaryMatrix::blocked(MSBoolean blocked_)
{ _blocked=blocked_; }

inline unsigned char& MSBinaryMatrix::elementAt(unsigned index_)
{
#if defined(MS_NO_INDEX_ERROR)
  return _pData->elementAt(index_);
#else
  if (index_<_count) return _pData->elementAt(index_);
  else return (indexError(index_,length()),_badData);
#endif
}

inline const unsigned char& MSBinaryMatrix::elementAt(unsigned index_) const
{
#if defined(MS_NO_INDEX_ERROR)
  return _pData->elementAt(index_);  
#else
  if (index_<_count) return _pData->elementAt(index_);
  else return  (indexError(index_,length()),_badData);
#endif
}

inline unsigned char& MSBinaryMatrix::elementAt(unsigned row_,unsigned column_)
{
#if defined(MS_NO_INDEX_ERROR)
  return _pData->elementAt(index(row_,column_));
#else
  unsigned ravel=index(row_,column_);
  if (ravel<_count) return _pData->elementAt(ravel);
  else return (indexError(ravel,length()),_badData);
#endif
}

inline const unsigned char& MSBinaryMatrix::elementAt(unsigned row_,unsigned column_) const
{
#if defined(MS_NO_INDEX_ERROR)
  return _pData->elementAt(index(row_,column_));  
#else
  unsigned ravel=index(row_,column_);
  if (ravel<_count) return _pData->elementAt(ravel);
  else return (indexError(ravel,length()),_badData);
#endif
}

inline unsigned char MSBinaryMatrix::operator()(unsigned index_) const
{ return elementAt(index_); }

inline unsigned char MSBinaryMatrix::operator[](unsigned index_) const
{ return elementAt(index_); }

inline unsigned char MSBinaryMatrix::operator()(unsigned row_,unsigned column_) const
{ return elementAt(index(row_,column_)); }

inline MSTypeData<unsigned char,MSAllocator<unsigned char> > *MSBinaryMatrix::incrementCount(void)
{ return (_pData!=0)?_pData->incrementCount():0; }

inline void MSBinaryMatrix::decrementCount(void)
{ if (_pData!=0) _pData->decrementCount(); }

inline unsigned char *MSBinaryMatrix::data(void) const
{ return (_pData!=0)?_pData->elements():0; }

inline long compare(const MSBinaryMatrix& aBinaryMatrix_,const MSBinaryMatrix& bBinaryMatrix_)
{ return aBinaryMatrix_.compare(bBinaryMatrix_); }

inline MSBoolean MSBinaryMatrix::operator==(const MSBinaryMatrix& aBinaryMatrix_) const
{ return MSBoolean(compare(aBinaryMatrix_)==0); }

inline MSBoolean MSBinaryMatrix::operator!=(const MSBinaryMatrix& aBinaryMatrix_) const
{ return MSBoolean(compare(aBinaryMatrix_)!=0); }

inline MSBoolean operator<(const MSBinaryMatrix& aBinaryMatrix_,const MSBinaryMatrix& bBinaryMatrix_)
{ return MSBoolean(aBinaryMatrix_.compare(bBinaryMatrix_)>0); }
inline MSBoolean operator>(const MSBinaryMatrix& aBinaryMatrix_,const MSBinaryMatrix& bBinaryMatrix_)
{ return MSBoolean(aBinaryMatrix_.compare(bBinaryMatrix_)<0); }
inline MSBoolean operator<=(const MSBinaryMatrix& aBinaryMatrix_,const MSBinaryMatrix& bBinaryMatrix_)
{ return MSBoolean(aBinaryMatrix_.compare(bBinaryMatrix_)>=0); }
inline MSBoolean operator>=(const MSBinaryMatrix& aBinaryMatrix_,const MSBinaryMatrix& bBinaryMatrix_)
{ return MSBoolean(aBinaryMatrix_.compare(bBinaryMatrix_)<=0); }

inline MSBoolean operator<(const MSBinaryMatrix& aBinaryMatrix_,unsigned char aBinaryValue_)
{ return aBinaryMatrix_.scalarCompare(aBinaryValue_,MSLessThan); }
inline MSBoolean operator<(unsigned char aBinaryValue_,const MSBinaryMatrix& aBinaryMatrix_)
{ return aBinaryMatrix_.scalarCompare(aBinaryValue_,MSGreaterThan); }

inline MSBoolean operator>(const MSBinaryMatrix& aBinaryMatrix_,unsigned char aBinaryValue_)
{ return aBinaryMatrix_.scalarCompare(aBinaryValue_,MSGreaterThan); }
inline MSBoolean operator>(unsigned char aBinaryValue_,const MSBinaryMatrix& aBinaryMatrix_)
{ return aBinaryMatrix_.scalarCompare(aBinaryValue_,MSLessThan); }

inline MSBoolean operator<=(const MSBinaryMatrix& aBinaryMatrix_,unsigned char aBinaryValue_)
{ return aBinaryMatrix_.scalarCompare(aBinaryValue_,MSLessThanOrEqualTo); }
inline MSBoolean operator<=(unsigned char aBinaryValue_,const MSBinaryMatrix& aBinaryMatrix_)
{ return aBinaryMatrix_.scalarCompare(aBinaryValue_,MSGreaterThanOrEqualTo); }

inline MSBoolean operator>=(const MSBinaryMatrix& aBinaryMatrix_,unsigned char aBinaryValue_)
{ return aBinaryMatrix_.scalarCompare(aBinaryValue_,MSGreaterThanOrEqualTo); }
inline MSBoolean operator>=(unsigned char aBinaryValue_,const MSBinaryMatrix& aBinaryMatrix_)
{ return aBinaryMatrix_.scalarCompare(aBinaryValue_,MSLessThanOrEqualTo); }

inline MSBoolean operator==(const MSBinaryMatrix& aBinaryMatrix_,unsigned char aBinaryValue_)
{ return aBinaryMatrix_.scalarCompare(aBinaryValue_,MSEqualTo); }
inline MSBoolean operator==(unsigned char aBinaryValue_,const MSBinaryMatrix& aBinaryMatrix_)
{ return aBinaryMatrix_.scalarCompare(aBinaryValue_,MSEqualTo); }

inline MSBoolean operator!=(const MSBinaryMatrix& aBinaryMatrix_,unsigned char aBinaryValue_)
{ return aBinaryMatrix_.scalarCompare(aBinaryValue_,MSNotEqualTo); }
inline MSBoolean operator!=(unsigned char aBinaryValue_,const MSBinaryMatrix& aBinaryMatrix_)
{ return aBinaryMatrix_.scalarCompare(aBinaryValue_,MSNotEqualTo); }

//------------------------------------------------------------
// MSBinaryMatrixSTypePick inlines
//------------------------------------------------------------
inline MSBinaryMatrixSTypePick::MSBinaryMatrixSTypePick(const MSBinaryMatrix& aTypeMatrix_,unsigned index_)
{ _pMatrix=&(MSBinaryMatrix&)aTypeMatrix_; _index=index_; }

inline MSBinaryMatrixSTypePick::MSBinaryMatrixSTypePick(const MSBinaryMatrixSTypePick& aPick_)
{ _pMatrix=aPick_.pMatrix(); _index=aPick_.index(); }

inline MSBinaryMatrixSTypePick& MSBinaryMatrixSTypePick::operator=(unsigned char aBinaryValue_)
{ _pMatrix->set(_index,aBinaryValue_); return *this; }

inline MSBinaryMatrix *MSBinaryMatrixSTypePick::pMatrix(void) const
{ return _pMatrix; }

inline unsigned MSBinaryMatrixSTypePick::index(void) const
{ return _index; }

inline MSBinaryMatrixSTypePick::operator unsigned char() const
{ return _pMatrix->elementAt(_index); }

inline unsigned char MSBinaryMatrixSTypePick::value(void) const
{ return _pMatrix->elementAt(_index); }

//------------------------------------------------------------
// Pick Indexing operators - define here to avoid inline problem
//------------------------------------------------------------
inline MSBinaryMatrixSTypePick MSBinaryMatrix::operator()(unsigned index_)    
{ return MSBinaryMatrixSTypePick(*this,index_); }

inline MSBinaryMatrixSTypePick MSBinaryMatrix::operator[](unsigned index_)
{ return MSBinaryMatrixSTypePick(*this,index_); }

inline MSBinaryMatrixSTypePick MSBinaryMatrix::operator()(unsigned row_,unsigned column_) 
{ return MSBinaryMatrixSTypePick(*this,index(row_,column_)); }

// We provide provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSTYPESLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSAllocator<unsigned char>
#pragma do_not_instantiate MSTypeData<unsigned char, MSAllocator<unsigned char> >
#endif // !__MSTYPESLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif
