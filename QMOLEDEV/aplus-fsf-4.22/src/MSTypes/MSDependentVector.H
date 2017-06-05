#ifndef MSDependentVectorHEADER
#define MSDependentVectorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSSpecializedVector.H>
#include <MSTypes/MSTypeVector.H>
#include <MSTypes/MSIndexVector.H>
#include <MSTypes/MSIndexedEvent.H>

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSDependentVector.C")
#endif

#if defined(MS_PREDECLARE_TEMPLATE_FRIENDS)
template<class Type> class MSDependentVector;
template <class Type> ostream& operator<<(ostream&, const MSDependentVector<Type>&);
#endif  //MS_PREDECLARE_TEMPLATE_FRIENDS

template<class Type>
class MSDependentVector : public MSIndexedModel, public MSEventReceiver
{
public:
  class MSTypesExport Pick
  {
  protected:
    MSTypeVector<Type> *_pVector;
    unsigned            _index;

  public:
    Pick(MSTypeVector<Type> *pVector_,unsigned index_) : _pVector(pVector_),_index(index_) {}
    
    MSTypeVector<Type>& vector(void) { return *_pVector; }
    unsigned index(void)             { return  _index; }

    Pick& operator=(const Type&);
  };

  MSDependentVector(const MSDependentVector<Type>&);  
  MSDependentVector(MSTypeVector<Type>&,MSIndexVector&);

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

  friend MSTypesExport ostream& operator<<(ostream&,const MSDependentVector<Type>&);

  MSError::ErrorStatus set(unsigned index_,const char *pString_); 
  inline Pick operator[](unsigned);

  inline const Type& operator()(unsigned) const;

  MSDependentVector<Type>& operator=(const MSDependentVector<Type>&);

  long compare(const MSDependentVector<Type>&) const;
  inline const Type& elementAt(unsigned) const;
  inline unsigned length(void) const;

  unsigned indexOf(const Type&,unsigned startPos_=0) const;
  unsigned lastIndexOf(const Type&,unsigned startPos_=UINT_MAX-1) const;

  virtual void receiveEvent(MSEvent&);

  MSIndexVector gradeUp(void) const;
  MSIndexVector gradeDown(void) const;
  void permute(const MSIndexVector&);
  MSDependentVector<Type>& sortUp(void);
  MSDependentVector<Type>& sortDown(void);

  // the following method is equivalent to dyadic iota in apl (indexOf on vectors)
  MSIndexVector indicesOf(const MSDependentVector<Type>&) const;
  // the following method is equivalent to membership in apl
  MSBinaryVector memberOf(const MSDependentVector<Type>&) const;

  static MSDependentVector<Type> select(const MSDependentVector<Type>&,
					const MSIndexVector&,MSIndexVector&);
  static MSDependentVector<Type> compress(const MSDependentVector<Type>&,
					  const MSBinaryVector&,MSIndexVector&);

  MSTypeVector<Type> *sourceVector(void) const { return _sourceVector; }
  MSIndexVector *indexVector(void)       const { return _indexVector; }  

protected:
  MSTypeVector<Type> *_sourceVector;
  MSIndexVector      *_indexVector;
  
  static unsigned mergeSortUp(unsigned n_,MSDependentVector<Type> *sp_,
			      unsigned *p_,unsigned low_,unsigned high_);
  static unsigned mergeSortDown(unsigned n_,MSDependentVector<Type> *sp_,
				unsigned *p_,unsigned low_,unsigned high_);
  static unsigned indexCompareUp(MSDependentVector<Type> *p_,unsigned i_,unsigned j_);
  static unsigned indexCompareDown(MSDependentVector<Type> *p_,unsigned i_,unsigned j_);

private:
  static MSSymbol initializeType(void);
};

template<class Type>
inline MSDependentVector<Type>::Pick MSDependentVector<Type>::operator[](unsigned index_)
{ return Pick(_sourceVector,(*_indexVector)(index_)); }

template<class Type>
inline MSDependentVector<Type>::Pick& MSDependentVector<Type>::Pick::operator=(const Type& aType_)
{
  _pVector->set(_index,aType_);
  return *this;
}

template<class Type>
inline MSError::ErrorStatus MSDependentVector<Type>::set(unsigned index_,const char *pString_)
{ return _sourceVector->set((*_indexVector)(index_),pString_); }


template<class Type>
inline const Type& MSDependentVector<Type>::elementAt(unsigned index_) const
// cannot use operator() for the integral types - it returns by value
// { return (*_sourceVector)((*_indexVector)(index_)); }
{ return ((const MSTypeVector<Type>&)(*_sourceVector)).elementAt((*_indexVector)(index_)); }

template<class Type>
inline const Type& MSDependentVector<Type>::operator()(unsigned index_) const
{ return elementAt(index_); }

template<class Type>
inline unsigned MSDependentVector<Type>::length(void) const
{ return _indexVector->length(); }

template<class Type>
inline long compare(const MSDependentVector<Type>& a_,const MSDependentVector<Type>& b_)
{ return a_.compare(b_); }

#endif
