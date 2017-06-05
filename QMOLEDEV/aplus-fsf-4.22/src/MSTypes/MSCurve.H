#ifndef MSCurveHEADER
#define MSCurveHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSBuiltinVector.H>
#include <MSTypes/MSTypeVector.H>
#include <MSTypes/MSIndexVector.H>
#include <MSTypes/MSIndexedModel.H>
#include <MSTypes/MSIndexedEvent.H>
#include <MSTypes/MSEventReceiver.H>
#include <MSTypes/MSPoint.H>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSCurve.C")
#endif

template<class X,class Y> class MSIPointPick;    // MSCurve indexed by MSIndexVector
template<class X,class Y> class MSBPointPick;    // MSCurve indexed by MSBinaryVector   
template<class X,class Y> class MSSPointPick;    // MSCurve indexed by a MSPoint

template<class X,class Y> class MSCurveInterpolater;

#if defined(MS_PREDECLARE_TEMPLATE_FRIENDS)
template<class X,class Y> class MSCurve;
template<class X,class Y> ostream& operator<<(ostream& aStream_, const MSCurve<X,Y>& aCurve_);
#endif  //MS_PREDECLARE_TEMPLATE_FRIENDS

template<class X,class Y> 
class MSCurve : public MSIndexedModel, public MSEventReceiver
{
friend MSIPointPick<X,Y>;
friend MSBPointPick<X,Y>;

public:
  enum Method {Linear,Quadratic,Cubic,CubicSpline,Exponential,Polynomial};

  virtual MSString asString(void) const;
  virtual MSString asDebugInfo(void) const;
  virtual MSString asMSF(void) const;
  virtual MSString className(void) const;
  virtual const MSSymbol& type(void) const;
  virtual MSError::ErrorStatus set(const char *);
  virtual MSError::ErrorStatus setFromMSF(const char *);
  virtual MSModel *clone(void) const;  
  virtual MSModel *create(void) const; 
  virtual void assign(const MSModel&);
  virtual long compare(const MSModel&) const;
  static const MSSymbol& symbol(void);

  // -------------------------------------------------------------
  // INSTANCE CREATION
  // -------------------------------------------------------------
  MSCurve(unsigned numPoints_=0);
  MSCurve(const MSCurve<X,Y>&);
  MSCurve(const MSTypeVector<X>&,const MSTypeVector<Y>&);
  ~MSCurve();

  virtual void receiveEvent(MSEvent&);

  // -------------------------------------------------------------
  // ASSIGNMENT
  // -------------------------------------------------------------

  MSCurve<X,Y>& operator=(const MSCurve<X,Y>&);
  MSCurve<X,Y>& operator=(const MSIPointPick<X,Y>&);
  MSCurve<X,Y>& operator=(const MSBPointPick<X,Y>&);
  MSCurve<X,Y>& operator=(const MSPoint<X,Y>&);

  void setXValue(unsigned index_,X value_);
  void setYValue(unsigned index_,Y value_);
  void replaceAt(unsigned index_,const MSPoint<X,Y>& point_);
  void replaceAt(unsigned index_,const X& xValue_,const Y& yValue_);

  // -------------------------------------------------------------
  // ACCESSING
  // -------------------------------------------------------------

  inline MSTypeVector<X>& xVector(void);
  inline MSTypeVector<Y>& yVector(void);
  inline const MSTypeVector<X>& xVector(void) const;
  inline const MSTypeVector<Y>& yVector(void) const;

  friend ostream& operator<<(ostream& aStream_,const MSCurve<X,Y>& aCurve_);

  inline MSPoint<X,Y> operator()(unsigned index_) const;
  inline MSPoint<X,Y> operator[](unsigned index_) const;
  inline MSSPointPick<X,Y> operator()(unsigned index_);
  inline MSSPointPick<X,Y> operator[](unsigned index_);

  inline MSIPointPick<X,Y> operator[](const MSIndexVector&);
  inline const MSIPointPick<X,Y> operator[](const MSIndexVector&) const;

  inline MSBPointPick<X,Y> operator[](const MSBinaryVector&);
  inline const MSBPointPick<X,Y> operator[](const MSBinaryVector&) const;
  
  inline MSPoint<X,Y> firstElement(void) const;
  inline MSPoint<X,Y> lastElement(void) const;
  inline MSPoint<X,Y> elementAt(unsigned index_) const;
  inline unsigned length(void) const;

  // -------------------------------------------------------------
  // UPDATING
  // -------------------------------------------------------------

  MSCurve<X,Y>& removeAll(void);
  MSCurve<X,Y>& append(const X&,const Y&);
  MSCurve<X,Y>& append(const MSPoint<X,Y>&);
  MSCurve<X,Y>& append(const MSTypeVector<X>&,const MSTypeVector<Y>&);
  MSCurve<X,Y>& insertAt(unsigned index_,const MSPoint<X,Y>&);
  MSCurve<X,Y>& insertAt(unsigned index_,const MSTypeVector<X>&,const MSTypeVector<Y>&);
  MSCurve<X,Y>& removeAt(unsigned index_);
  MSCurve<X,Y>& removeAt(unsigned startPos_,unsigned numElements_);
  MSCurve<X,Y>& remove(const MSIndexVector&);
  MSCurve<X,Y>& remove(const MSBinaryVector&);
  MSCurve<X,Y>& exchange(unsigned,unsigned);

  MSCurve<X,Y>& select(const MSIndexVector&);
  MSCurve<X,Y>& compress(const MSBinaryVector&); 

  // -------------------------------------------------------------
  // QUERY
  // -------------------------------------------------------------

  inline unsigned numberOfPoints(void) const;
  inline MSBoolean isEmpty(void) const;

  // -------------------------------------------------------------
  // INTERPOLATION
  // -------------------------------------------------------------

  Y operator()(const X&,MSCurveInterpolater<X,Y> *pInterpolater_) const;
  Y operator()(const X&,MSCurveInterpolater<X,Y>& pInterpolater_) const;
  MSTypeVector<Y> operator()(const MSTypeVector<X>&,MSCurveInterpolater<X,Y> *pInterpolater_) const;
  MSTypeVector<Y> operator()(const MSTypeVector<X>&,MSCurveInterpolater<X,Y>& pInterpolater_) const;

  Y interpolate(const X&,MSCurveInterpolater<X,Y> *pInterpolater_) const;
  Y interpolate(const X&,MSCurveInterpolater<X,Y>& pInterpolater_) const;
  MSTypeVector<Y> interpolate(const MSTypeVector<X>&,MSCurveInterpolater<X,Y> *pInterpolater_) const;
  MSTypeVector<Y> interpolate(const MSTypeVector<X>&,MSCurveInterpolater<X,Y>& pInterpolater_) const;
  
  // -------------------------------------------------------------
  // ARITHMETIC 
  // -------------------------------------------------------------

  // -------------------------------------------------------------
  // BOOLEAN
  // -------------------------------------------------------------

  //inline friend long compare(const MSCurve<X,Y>&,const MSCurve<X,Y>&);
  
  // -------------------------------------------------------------
  // STATISTICS
  // -------------------------------------------------------------

  // -------------------------------------------------------------
  // DEPENDENCY
  // -------------------------------------------------------------

protected:
  MSTypeVector<X>  _xVector;
  MSTypeVector<Y>  _yVector;
  MSBoolean        _blocked;

  void init(void);
  inline MSBoolean blocked(void);
  inline void blocked(MSBoolean);
};

//--------------------
// MSCurve inlines
//--------------------
template<class X,class Y> 
inline MSTypeVector<X>& MSCurve<X,Y>::xVector(void)
{ return _xVector; }

template<class X,class Y> 
inline MSTypeVector<Y>& MSCurve<X,Y>::yVector(void)
{ return _yVector; }

template<class X,class Y> 
inline const MSTypeVector<X>& MSCurve<X,Y>::xVector(void) const
{ return _xVector; }

template<class X,class Y> 
inline const MSTypeVector<Y>& MSCurve<X,Y>::yVector(void) const
{ return _yVector; }

template<class X,class Y> 
inline MSBoolean MSCurve<X,Y>::blocked(void)
{ return _blocked; }

template<class X,class Y>
inline void MSCurve<X,Y>::blocked(MSBoolean aBool_)
{ _blocked=aBool_; }

template<class X,class Y> 
inline unsigned MSCurve<X,Y>::length(void) const
{ return _xVector.length(); }

template<class X,class Y> 
inline MSPoint<X,Y> MSCurve<X,Y>::firstElement(void) const
{ return MSPoint<X,Y>(xVector()(0),yVector()(0)); }

template<class X,class Y> 
inline MSPoint<X,Y> MSCurve<X,Y>::lastElement(void) const
{ return MSPoint<X,Y>(xVector()(length()-1),yVector()(length()-1)); }

template<class X,class Y> 
inline MSPoint<X,Y> MSCurve<X,Y>::elementAt(unsigned index_) const
{ return MSPoint<X,Y>(xVector()(index_),yVector()(index_)); }

template<class X,class Y>
inline unsigned MSCurve<X,Y>::numberOfPoints(void) const
{ return length(); }

template<class X,class Y>
inline MSBoolean MSCurve<X,Y>::isEmpty(void) const
{ return length()==0?MSTrue:MSFalse; }

// template<class X,class Y>
// inline long compare(const MSCurve<X,Y>& aCurve_,const MSCurve<X,Y>& bCurve_)
// { return aCurve_.xVector().compare(bCurve_.xVector()); }

//-------------------------
// MSIPointPick declaration
//-------------------------
template<class X,class Y>
class MSIPointPick
{
public:
  MSIPointPick<X,Y>& operator=(const MSCurve<X,Y>&);
  MSIPointPick<X,Y>& operator=(const MSIPointPick<X,Y>&);
  MSIPointPick<X,Y>& operator=(const MSBPointPick<X,Y>&);
  MSIPointPick<X,Y>& operator=(const MSPoint<X,Y>&);

  inline MSCurve<X,Y> *pCurve(void) const;
  inline const MSIndexVector *pIndexVector(void) const;

private:
  friend MSCurve<X,Y>;
  friend MSIndexVector;

  inline MSIPointPick(const MSCurve<X,Y>&,const MSIndexVector&);
  inline MSIPointPick(const MSIPointPick<X,Y>&);

  void changed(void);

  MSCurve<X,Y>         *_pCurve;
  const MSIndexVector  *_pIndexVector;
};

//----------------------
// MSIPointPick inlines
//----------------------
template <class X,class Y>
inline MSIPointPick<X,Y>::MSIPointPick(const MSCurve<X,Y>& aCurve_,const MSIndexVector& aIndexVector_)
{ _pCurve=&(MSCurve<X,Y>&)aCurve_; _pIndexVector=&aIndexVector_; }

template <class X,class Y>
inline MSIPointPick<X,Y>::MSIPointPick(const MSIPointPick<X,Y>& aPick_)
{ _pCurve=aPick_.pCurve(); _pIndexVector=aPick_.pIndexVector(); }

template <class X,class Y>
inline MSCurve<X,Y> *MSIPointPick<X,Y>::pCurve(void) const
{ return _pCurve; }

template <class X,class Y>
inline const MSIndexVector *MSIPointPick<X,Y>::pIndexVector(void) const
{ return _pIndexVector; }

//-------------------------
// MSBPointPick declaration
//-------------------------
template<class X,class Y>
class MSBPointPick
{
public:
  MSBPointPick<X,Y>& operator=(const MSCurve<X,Y>&);
  MSBPointPick<X,Y>& operator=(const MSBPointPick<X,Y>&);
  MSBPointPick<X,Y>& operator=(const MSIPointPick<X,Y>&);
  MSBPointPick<X,Y>& operator=(const MSPoint<X,Y>&);

  inline MSCurve<X,Y> *pCurve(void) const;
  inline const MSBinaryVector *pBinaryVector(void) const;

private:
  friend MSCurve<X,Y>;
  friend MSBinaryVector;

  inline MSBPointPick(const MSCurve<X,Y>&,const MSBinaryVector&);
  inline MSBPointPick(const MSBPointPick<X,Y>&);

  void changed(void);

  MSCurve<X,Y>         *_pCurve;
  const MSBinaryVector *_pBinaryVector;
};

//-------------------------
// MSBPointPick inlines
//-------------------------
template <class X,class Y>
inline MSCurve<X,Y> *MSBPointPick<X,Y>::pCurve(void) const 
{ return _pCurve; }

template <class X,class Y>
inline const MSBinaryVector *MSBPointPick<X,Y>::pBinaryVector(void) const 
{ return _pBinaryVector; }

template <class X,class Y> inline 
MSBPointPick<X,Y>::MSBPointPick(const MSCurve<X,Y>& aCurve_,const MSBinaryVector& aBinaryVector_) 
{ 
  _pCurve=&(MSCurve<X,Y>&)aCurve_; 
  _pBinaryVector=&aBinaryVector_; 
}

template<class X,class Y>
inline MSBPointPick<X,Y>::MSBPointPick(const MSBPointPick<X,Y>& aPick_) 
{
  _pCurve=aPick_.pCurve(); 
  _pBinaryVector=aPick_.pBinaryVector(); 
}

//-------------------------
// MSSPointPick declaration
//-------------------------
template <class X,class Y>
class MSSPointPick
{
public:
  MSSPointPick<X,Y>& operator=(const MSPoint<X,Y>&);

  inline MSCurve<X,Y> *pCurve(void) const;
  inline unsigned index(void) const;

private:
  friend MSCurve<X,Y>;

  inline MSSPointPick(const MSCurve<X,Y>& aCurve_,unsigned index_);
  inline MSSPointPick(const MSSPointPick<X,Y>& aPoint_);

  MSCurve<X,Y>      *_pCurve;
  unsigned           _index; 
};

//----------------------
// MSSPointPick inlines
//----------------------
template <class X,class Y>
MSSPointPick<X,Y>::MSSPointPick(const MSCurve<X,Y>& aCurve_,unsigned index_)
{ _pCurve=&(MSCurve<X,Y>&)aCurve_; _index=index_; }

template <class X,class Y>
MSSPointPick<X,Y>::MSSPointPick(const MSSPointPick<X,Y>& aPoint_)
{ _pCurve=aPoint_.pCurve(); _index=aPoint_.index(); }

template <class X,class Y>
MSCurve<X,Y> *MSSPointPick<X,Y>::pCurve(void) const
{ return _pCurve; }

template <class X,class Y>
unsigned MSSPointPick<X,Y>::index(void) const
{ return _index; }

//----------------------------------------------------------------------
// Pick Indexing operators - define here to avoid inline problem
//----------------------------------------------------------------------
template <class X,class Y>
inline MSIPointPick<X,Y> MSCurve<X,Y>::operator[](const MSIndexVector& aIndexVector_)
{ return MSIPointPick<X,Y>(*this,aIndexVector_); }

template <class X,class Y>
inline const MSIPointPick<X,Y> MSCurve<X,Y>::operator[](const MSIndexVector& aIndexVector_) const
{ return MSIPointPick<X,Y>(*this,aIndexVector_); }

template <class X,class Y>
inline MSBPointPick<X,Y> MSCurve<X,Y>::operator[](const MSBinaryVector& aBinaryVector_)
{ return MSBPointPick<X,Y>(*this,aBinaryVector_); }

template <class X,class Y>
inline const MSBPointPick<X,Y> MSCurve<X,Y>::operator[](const MSBinaryVector& aBinaryVector_) const
{ return MSBPointPick<X,Y>(*this,aBinaryVector_); }

template<class X,class Y> 
inline MSSPointPick<X,Y> MSCurve<X,Y>::operator[](unsigned index_)
{ return MSSPointPick<X,Y>(*this,index_); }

template <class X,class Y>
inline MSSPointPick<X,Y> MSCurve<X,Y>::operator()(unsigned index_)
{ return MSSPointPick<X,Y>(*this,index_); }

template<class X,class Y> 
inline MSPoint<X,Y> MSCurve<X,Y>::operator[](unsigned index_) const
{ return elementAt(index_); }

template <class X,class Y>
inline MSPoint<X,Y> MSCurve<X,Y>::operator()(unsigned index_) const
{ return elementAt(index_); }

#endif
