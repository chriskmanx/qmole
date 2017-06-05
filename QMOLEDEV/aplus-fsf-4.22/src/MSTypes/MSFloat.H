#ifndef MSFloatHEADER
#define MSFloatHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSMathDeclare.H>
#include <MSTypes/MSError.H>
#include <MSTypes/MSInt.H>
#include <MSTypes/MSSymbol.H>

#if HAVE_SVR4
#include <ieeefp.h>
#endif

#if defined(MS_C_LINKAGE_INCOMPATIBILITY)
extern "C"
{
  typedef double (*FloatMathFunction)(double);
}
#else
typedef double (*FloatMathFunction)(double);
#endif

class MSFormat;

class MSTypesExport MSFloat : public MSScalarModel
{
public:
  
  // Formats for formats. Note that since MSRate and MSMoney
  // are derived from MSFloat,these formats are "inherited".
  enum MSFloatFormat
  {
    Decimal0=200,         // 1234
    Decimal1=201,         // 1234.2
    Decimal2=202,         // 1234.17
    Decimal3=203,         // 1234.171
    Decimal4=204,         // 1234.1712
    Decimal5=205,         // 1234.17123
    Decimal6=206,         // 1234.171234
    Decimal7=207,         // 1234.1712345
    Decimal8=208,         // 1234.17123456
    CommaDecimal0=209,    // 1,234
    CommaDecimal1=210,    // 1,234.2
    CommaDecimal2=211,    // 1,234.17
    CommaDecimal3=212,    // 1,234.171
    CommaDecimal4=213,    // 1,234.1712
    CommaDecimal5=214,    // 1,234.17123
    CommaDecimal6=215,    // 1,234.171234
    CommaDecimal7=216,    // 1,234.1712345
    CommaDecimal8=217,    // 1,234.17123456
    MaximumPrecision=218,
    Default=219
  };

  // These constructors and destructors are *very* heavily used.  When
  // they were out-of-line they amounted to around 5% of total execution
  // time for one project.
  MSFloat(void) : _flags(MSFloat::Valid),_real(0.0) {}
  MSFloat(double r_) : _flags(MSFloat::Set),_real(r_) {if (isfinite(r_)>0) _flags|=MSFloat::Valid;}
  MSFloat(int i_) : _real(i_),_flags(MSFloat::Set|MSFloat::Valid) {}
  MSFloat(const MSFloat& f_) : _real(f_._real),_flags(f_._flags) {}
  MSFloat(const MSInt& i_) : _real(int(i_)),_flags((i_.isSet()==MSTrue) ? Set|Valid : Valid) {}
  virtual ~MSFloat(void);
  
  inline friend MSTypesExport unsigned long hash(const MSFloat&,unsigned long size_);
  
  inline long compare(const MSFloat&) const;
  inline long compare(const MSFloat*) const;

  virtual MSString asString(void) const;
  virtual MSString asDebugInfo(void) const;
  virtual MSString asMSF(void) const;
  virtual MSString className(void) const;
  virtual const MSSymbol& type(void) const;
  virtual MSModel *clone(void) const;
  virtual MSModel *create(void) const; 
  virtual void assign(const MSModel&); 
  virtual long compare(const MSModel&) const;
  virtual MSError::ErrorStatus set(const char *);
  virtual MSError::ErrorStatus setFromMSF(const char*);  
  static const MSSymbol& symbol(void);

  virtual MSError::ErrorStatus set(const MSString *);
  virtual MSError::ErrorStatus set(const MSString &);
  virtual MSError::ErrorStatus set(double);
  const char *format(MSString *) const;
  const char *format(MSString &) const;
  const char *format(MSString *,MSFloatFormat) const;
  const char *format(MSString &,MSFloatFormat) const;
  const char *format(MSString*,const MSFormat&) const;
  const char *format(MSString&,const MSFormat&) const;
  
  // operators
  // Most of the time, MSFloat will be automatically converted to
  // a double, the built-in double operators will be used, and the
  // result will be converted back into a MSFloat automatically.
  operator double()   const { return _real; }
  double operator()() const { return _real; }      // function call operator

  MSFloat& operator=(const MSFloat&);
  MSFloat& operator=(int value_)           { set((double)value_); return *this; }
  MSFloat& operator=(double value_)        { set(value_); return *this; }
  MSFloat& operator=(const char *value_)   { set(value_); return *this; }

  MSFloat& sin(void)   { return apply(::sin); }
  MSFloat& sinh(void)  { return apply(::sinh); }
  MSFloat& cos(void)   { return apply(::cos); }
  MSFloat& cosh(void)  { return apply(::cosh); }
  MSFloat& tan(void)   { return apply(::tan); }
  MSFloat& tanh(void)  { return apply(::tanh); }
  MSFloat& asin(void)  { return apply(::asin); }
  MSFloat& acos(void)  { return apply(::acos); }
  MSFloat& atan(void)  { return apply(::atan); }
  MSFloat& exp(void)   { return apply(::exp); }
  MSFloat& log(void)   { return apply(::log); }
  MSFloat& sqrt(void)  { return apply(::sqrt); }
  MSFloat& ceil(void)  { return apply(::ceil); }
  MSFloat& floor(void) { return apply(::floor); }
  MSFloat& abs(void)   { return apply(::fabs); }

  // The equality operators return MSTrue if the difference between
  // the values is less than MSFloatDelta (see MSFloat.C)
  MSBoolean operator==(const MSFloat &r_) const { return *this==r_._real; }
  MSBoolean operator==(int i_)            const { return *this==(double)i_; }
  MSBoolean operator==(const MSInt& i_)   const { return *this==(double)((int)i_); }
  MSBoolean operator==(double d_)         const;

  MSBoolean operator!=(const MSFloat &r_) const { return (*this==r_._real)?MSFalse:MSTrue; }
  MSBoolean operator!=(double r_)         const { return (_real==r_)?MSFalse:MSTrue; }
  MSBoolean operator!=(int i_)            const { return *this!=(double)i_; }
  MSBoolean operator!=(const MSInt& i_)   const { return *this!=(double)((int)i_); }
  
  friend MSTypesExport MSFloat operator+(const MSFloat&,const MSFloat&);
  friend MSTypesExport MSFloat operator-(const MSFloat&,const MSFloat&);
  friend MSTypesExport MSFloat operator*(const MSFloat&,const MSFloat&);
  friend MSTypesExport MSFloat operator/(const MSFloat&,const MSFloat&);

  friend MSTypesExport MSFloat operator+(const MSFloat&,const MSInt&);
  friend MSTypesExport MSFloat operator-(const MSFloat&,const MSInt&);
  friend MSTypesExport MSFloat operator*(const MSFloat&,const MSInt&);
  friend MSTypesExport MSFloat operator/(const MSFloat&,const MSInt&);

  friend MSTypesExport MSFloat operator+(const MSInt&,const MSFloat&);
  friend MSTypesExport MSFloat operator-(const MSInt&,const MSFloat&);
  friend MSTypesExport MSFloat operator*(const MSInt&,const MSFloat&);
  friend MSTypesExport MSFloat operator/(const MSInt&,const MSFloat&);

  friend MSTypesExport MSFloat operator+(double,const MSFloat&);
  friend MSTypesExport MSFloat operator-(double,const MSFloat&);
  friend MSTypesExport MSFloat operator*(double,const MSFloat&);
  friend MSTypesExport MSFloat operator/(double,const MSFloat&);

  friend MSTypesExport MSFloat operator+(const MSFloat&,double);
  friend MSTypesExport MSFloat operator-(const MSFloat&,double);
  friend MSTypesExport MSFloat operator*(const MSFloat&,double);
  friend MSTypesExport MSFloat operator/(const MSFloat&,double);

  friend MSTypesExport MSFloat operator+(int,const MSFloat&);
  friend MSTypesExport MSFloat operator-(int,const MSFloat&);
  friend MSTypesExport MSFloat operator*(int,const MSFloat&);
  friend MSTypesExport MSFloat operator/(int,const MSFloat&);

  friend MSTypesExport MSFloat operator+(const MSFloat&,int);
  friend MSTypesExport MSFloat operator-(const MSFloat&,int);
  friend MSTypesExport MSFloat operator*(const MSFloat&,int);
  friend MSTypesExport MSFloat operator/(const MSFloat&,int);

  MSFloat& operator+=(const MSFloat&);
  MSFloat& operator+=(const MSInt&);
  MSFloat& operator+=(double r_);
  MSFloat& operator+=(int i_);
  
  MSFloat& operator-=(const MSFloat&);
  MSFloat& operator-=(const MSInt&);
  MSFloat& operator-=(double r_);
  MSFloat& operator-=(int i_);
  
  MSFloat& operator*=(const MSFloat&);
  MSFloat& operator*=(const MSInt&);
  MSFloat& operator*=(double r_);
  MSFloat& operator*=(int i_);
  
  MSFloat& operator/=(const MSFloat&);
  MSFloat& operator/=(const MSInt&);
  MSFloat& operator/=(double r_);
  MSFloat& operator/=(int i_);
  
  MSFloat operator-() { return -_real; }
  
  MSBoolean isSet(void)   const;
  MSBoolean isValid(void) const;
  void setInvalid(void);
  void unset(void);
  
  friend MSTypesExport ostream& operator<<(ostream& aStream_,const MSFloat&);
  friend MSTypesExport istream& operator>>(istream& aStream_,MSFloat&);
  
protected:
  enum BitFlag {Valid=0x1,Set=0x2};
  
  double           _real;
  unsigned         _flags;
  
  MSError::ErrorStatus internalSet(const char *);
  // nuts-and-bolts of formatted format function
  const char *formatReal(MSString&,MSFloatFormat,unsigned long modifier_,double value) const;
  MSFloat& apply(FloatMathFunction func_) { set(func_(_real)); return *this; }

  enum FloatInitialization {Uninitialized};
  MSFloat(FloatInitialization) {}  // optimization - we want an unitialized float

  //
  // The following methods are used to maintain the state bits for
  // the object
  //
  MSBoolean bitState(MSFloat::BitFlag) const;
  void setToValid(void);
  void setToInValid(void);  
  void setBit(MSFloat::BitFlag);
  void unsetBit(MSFloat::BitFlag);
  
  // special protected constructors - they allow us to take advantage of the
  // return value optimization (RV0) - see section 12.1c of the ARM
  enum FloatOperator{Plus,Minus,Multiply,Divide};
  MSFloat(const MSFloat&,const MSFloat&,FloatOperator);
  MSFloat(const MSFloat&,const MSInt&,FloatOperator);
  MSFloat(const MSInt&,const MSFloat&,FloatOperator);
  MSFloat(const MSFloat&,double,FloatOperator);
  MSFloat(double,const MSFloat&,FloatOperator);
  MSFloat(const MSFloat&,int,FloatOperator);
  MSFloat(int,const MSFloat&,FloatOperator);
};


// Note that "equivalence" has a fudge factor, so comparisons
// have to check for "equality" first
inline long MSFloat::compare(const MSFloat& aFloat_) const
{ return aFloat_==_real?0:(_real>aFloat_._real?1:-1); }

inline long MSFloat::compare(const MSFloat *pFloat_) const 
{ return compare(*pFloat_); }

inline long compare(const MSFloat& aFloat_,const MSFloat& bFloat_)
{ return aFloat_.compare(bFloat_); }

inline unsigned long hash(const MSFloat& aFloat_,unsigned long size_)
{ return int(aFloat_._real)%size_; }

#ifndef MS_NO_INLINES
#ifndef MSFloatINLINES
#include <MSTypes/MSFloatInlines.C>
#endif
#endif

#endif
