#ifndef MSRateHEADER
#define MSRateHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSError.H>
#include <MSTypes/MSFloat.H>
#include <MSTypes/MSSymbol.H>

class MSFormat;

class MSTypesExport MSRate: public MSFloat 
{
public:

  enum MSRateFormat
  {
    Decimal0=MSFloat::Decimal0,	    //  1234
    Decimal1=MSFloat::Decimal1,	    //  1234.2
    Decimal2=MSFloat::Decimal2,	    //  1234.17
    Decimal3=MSFloat::Decimal3,	    //  1234.171
    Decimal4=MSFloat::Decimal4,	    //  1234.1712
    Decimal5=MSFloat::Decimal5,	    //  1234.17123
    Decimal6=MSFloat::Decimal6,	    //  1234.171234
    Decimal7=MSFloat::Decimal7,	    //  1234.1712345
    Decimal8=MSFloat::Decimal8,	    //  1234.17123456
    CommaDecimal0=MSFloat::CommaDecimal0, // 1,234
    CommaDecimal1=MSFloat::CommaDecimal1, // 1,234.2
    CommaDecimal2=MSFloat::CommaDecimal2, // 1,234.17
    CommaDecimal3=MSFloat::CommaDecimal3, // 1,234.171
    CommaDecimal4=MSFloat::CommaDecimal4, // 1,234.1712
    CommaDecimal5=MSFloat::CommaDecimal5, // 1,234.17123
    CommaDecimal6=MSFloat::CommaDecimal6, // 1,234.171234
    CommaDecimal7=MSFloat::CommaDecimal7, // 1,234.1712345
    CommaDecimal8=MSFloat::CommaDecimal8, // 1,234.17123456
    
    Percent0=400,	// 17%
    Percent1=401,	// 17.1%
    Percent2=402,	// 17.12%
    Percent3=403,	// 17.123%
    Percent4=404,	// 17.1234%
    Percent5=405,	// 17.12345%

    BasisPoint=406	// 17bp
  };
  
  MSRate(void) {}
  MSRate(const MSRate& aRate_) : MSFloat(aRate_) {}
  MSRate(const MSFloat& f_) : MSFloat(f_) {}
  MSRate(const MSInt& i_) : MSFloat(i_) {}
  MSRate(int i_) : MSFloat(i_) {}
  MSRate(double d_) : MSFloat(d_) {}
  ~MSRate(void);

  virtual MSString asString(void) const;
  virtual MSString asDebugInfo(void) const;
  virtual MSString className(void) const;
  virtual const MSSymbol& type(void) const;
  virtual MSModel *clone(void) const;
  virtual MSModel *create(void) const; 
  virtual void assign(const MSModel&);
  virtual long compare(const MSModel&) const; 
  static const MSSymbol& symbol(void);

  // On entry, if the string has a trailing percent sign the number is
  // divided by 100 and then stored; if the string has a trailing bp
  // (for "basis points"), the number is divided by 10000 and then stored;
  // otherwise, the number itself is stored.  For example, 
  // entering ".0725", "7.25%", and "725bp" format you the same thing.
  virtual MSError::ErrorStatus set(const char*);
  MSError::ErrorStatus set(const MSString*);
  MSError::ErrorStatus set(const MSString&);
  MSError::ErrorStatus set(double);
  const char *format(MSString*) const;
  const char *format(MSString&) const;
  const char *format(MSString*,MSRateFormat) const;
  const char *format(MSString&,MSRateFormat) const;
  const char *format(MSString*,const MSFormat&) const;
  const char *format(MSString&,const MSFormat&) const;

  friend MSTypesExport ostream& operator<<(ostream&,const MSRate&);

  operator double() const { return _real; }

};

inline long compare(const MSRate& aRate_,const MSRate& bRate_)
{ return compare((MSFloat&)aRate_,(MSFloat&)bRate_); }

// nothing in this file as 25 Feb 96 - JMB
// #ifndef MS_NO_INLINES
// #ifndef MSRateINLINES
// #include <MSTypes/MSRateInlines.C>
// #endif
// #endif

#endif
