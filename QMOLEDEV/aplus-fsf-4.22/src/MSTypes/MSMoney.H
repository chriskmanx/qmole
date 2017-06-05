#ifndef MSMoneyHEADER
#define MSMoneyHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSError.H>
#include <MSTypes/MSFloat.H>
#include <MSTypes/MSTime.H>
#include <MSTypes/MSSymbol.H>

class MSRate;
class MSFormat;
struct CurrencyData;

class MSTypesExport MSMoney : public MSFloat 
{
public:
  
  enum Currency 
  {
    DefaultCurrency,	// currency is the one in _defaultCurrency
    USDollar,           USDollars=USDollar,              // the enums in 2nd column are aliases provided for
    CanadianDollar,     CanadianDollars=CanadianDollar,  // backward compatibility only
    BritishPound,       Pound=BritishPound,
    JapaneseYen,        Yen=JapaneseYen,
    DeutscheMark,       Deutchmark=DeutscheMark,
    FrenchFranc,
    SwissFranc,
    DutchGuilder,       Gilder=DutchGuilder,
    AustralianDollar,   Aus=AustralianDollar,
    ItalianLira,        Lira=ItalianLira,
    SpanishPeseta,      Peseta=SpanishPeseta,
    Euro,
    ArgentinianPeso,
    AustrianSchilling,
    BelgianFranc,
    BrazilianReal,
    BulgarianLev,
    ChileanPeso,
    ChineseRenminbi,
    CzechKoruna,
    DanishKrone,
    FinnishMarkka,
    GreekDrachma,
    HongKongDollar,
    HungarianForint,
    IcelandKrona,
    IndianRupee,
    IrishPunt,
    IsraeliShekel,
    MexicanPeso,
    NewZealandDollar,
    NorwegianKrone,
    PolishZloty,
    PortugueseEscudo,
    RomanianLeu,
    RussianRouble,
    SingaporeDollar,
    SlovakianKoruna,
    SouthKoreanWon,
    SwedishKrona,
    TaiwanDollar,
    TurkishLira,
    VenezuelanBolivar
  };

  enum MSMoneyFormat 
  {
    // "inherited" formats from MSFloat
    Decimal0=MSFloat::Decimal0,	          //  1234
    Decimal1=MSFloat::Decimal1,	          //  1234.2
    Decimal2=MSFloat::Decimal2,	          //  1234.17
    Decimal3=MSFloat::Decimal3,	          //  1234.171
    Decimal4=MSFloat::Decimal4,	          //  1234.1712
    Decimal5=MSFloat::Decimal5,	          //  1234.17123
    Decimal6=MSFloat::Decimal6,	          //  1234.171234
    Decimal7=MSFloat::Decimal7,	          //  1234.1712345
    Decimal8=MSFloat::Decimal8,	          //  1234.17123456
    CommaDecimal0=MSFloat::CommaDecimal0, // 1,234
    CommaDecimal1=MSFloat::CommaDecimal1, // 1,234.2
    CommaDecimal2=MSFloat::CommaDecimal2, // 1,234.17
    CommaDecimal3=MSFloat::CommaDecimal3, // 1,234.171
    CommaDecimal4=MSFloat::CommaDecimal4, // 1,234.1712
    CommaDecimal5=MSFloat::CommaDecimal5, // 1,234.17123
    CommaDecimal6=MSFloat::CommaDecimal6, // 1,234.171234
    CommaDecimal7=MSFloat::CommaDecimal7, // 1,234.1712345
    CommaDecimal8=MSFloat::CommaDecimal8, // 1,234.17123456
    
    // Use precision appropriate to currency
    CurrencyPrecision=300,	// default if not specified
    CommaCurrencyPrecision=301, // same as above with commas
    
    Eights=302,
    Sixteenths=303,
    ThirtySeconds=304,
    SixtyForths=305,
    OneTwentyEights=306,
    TwoFiftySixths=307,
    Tenths=Decimal1,
    Hundredths=Decimal2,
    Thousandths=Decimal3,
    TenThousandths=Decimal4,
    HundredThousandths=Decimal5,
    Millionths=Decimal6,
    TenMillionths=Decimal7,
    HundredMillionths=Decimal8
  };
  
  enum SymbolLocation { SymbolAtStart, SymbolAtEnd, DefaultLocation };
  enum SymbolType { ISOSymbol, LocalSymbol, NoCurrencySymbol };

  MSMoney(void);
  MSMoney(const MSMoney& m_);
  MSMoney(double d_,Currency c_=DefaultCurrency);
  MSMoney(int i_,Currency c_=DefaultCurrency);
  MSMoney(const MSFloat& f_,Currency c_=DefaultCurrency);
  MSMoney(const MSInt& i_,Currency c_=DefaultCurrency);
  ~MSMoney(void);

  virtual MSString asString(void) const;
  virtual MSString asMSF(void) const;
  virtual MSString asDebugInfo(void) const;
  virtual MSString className(void) const;
  virtual const MSSymbol& type(void) const;
  virtual MSModel *clone(void) const;
  virtual MSModel *create(void) const; 
  virtual void assign(const MSModel&);
  virtual long compare(const MSModel&) const; 
  virtual MSError::ErrorStatus setFromMSF(const char *);
  virtual MSError::ErrorStatus set(const char *);  

  static const MSSymbol& symbol(void);

  MSError::ErrorStatus set(const MSString *);
  MSError::ErrorStatus set(const MSString &);
  MSError::ErrorStatus set(double);
  const char *format(MSString*) const;
  const char *format(MSString&) const;
  const char *format(MSString*,MSMoneyFormat,MSBoolean justify=MSFalse) const;
  const char *format(MSString&,MSMoneyFormat,MSBoolean justify=MSFalse) const;
  const char *format(MSString*,const MSFormat&,MSBoolean justify=MSFalse) const;
  const char *format(MSString&,const MSFormat&,MSBoolean justify=MSFalse) const;
  const char *format(MSString*, MSMoneyFormat, SymbolType, SymbolLocation =DefaultLocation,
		     MSBoolean justify=MSFalse) const;
  const char *format(MSString&, MSMoneyFormat, SymbolType, SymbolLocation =DefaultLocation,
		     MSBoolean justify=MSFalse) const;

  void currency(Currency);
  Currency currency(void) const;
  const char *isoCurrency (void) const;
  Currency isoConvert(const char *pisoSymbol_) const;
  
  // Currency conversion
  // Returns this value converted to "dest" currency, optionally using
  // rate at passed time instead of current rate.
  MSMoney convert(Currency) const;
  MSMoney convert(Currency,const MSTime&) const;

  operator double() const;

  // Combining money is only allowed if currencies match.
  // Note that money*money=real, but money*real=money and
  // real/money=real. operator=/+/- (MSFloat) is explicitly
  // left out, to catch incorrect types in the applications.
  MSMoney& operator=(const MSMoney&);
  MSMoney& operator=(const MSFloat&);
  MSMoney& operator=(const MSInt&);
  MSMoney& operator=(double);
  MSMoney& operator=(int);
  
  MSMoney operator+(const MSMoney&) const;
  MSMoney operator-(const MSMoney&) const;
  double  operator/(const MSMoney&) const;
  //  MSMoney operator*(const MSMoney& m_)  -- generate compile time error - makes no sense

  INLINELINKAGE friend MSTypesExport MSMoney operator+(const MSMoney&,const MSFloat&);
  INLINELINKAGE friend MSTypesExport MSMoney operator-(const MSMoney&,const MSFloat&);
  INLINELINKAGE friend MSTypesExport MSMoney operator*(const MSMoney&,const MSFloat&);
  INLINELINKAGE friend MSTypesExport MSMoney operator/(const MSMoney&,const MSFloat&);
  
  INLINELINKAGE friend MSTypesExport MSMoney operator+(const MSMoney&,const MSInt&);
  INLINELINKAGE friend MSTypesExport MSMoney operator-(const MSMoney&,const MSInt&);
  INLINELINKAGE friend MSTypesExport MSMoney operator*(const MSMoney&,const MSInt&);
  INLINELINKAGE friend MSTypesExport MSMoney operator/(const MSMoney&,const MSInt&);
  
  INLINELINKAGE friend MSTypesExport MSMoney operator+(const MSMoney&,double);
  INLINELINKAGE friend MSTypesExport MSMoney operator-(const MSMoney&,double);
  INLINELINKAGE friend MSTypesExport MSMoney operator*(const MSMoney&,double);
  INLINELINKAGE friend MSTypesExport MSMoney operator/(const MSMoney&,double);
   
  INLINELINKAGE friend MSTypesExport MSMoney operator+(const MSMoney&,int); 
  INLINELINKAGE friend MSTypesExport MSMoney operator-(const MSMoney&,int); 
  INLINELINKAGE friend MSTypesExport MSMoney operator*(const MSMoney&,int); 
  INLINELINKAGE friend MSTypesExport MSMoney operator/(const MSMoney&,int); 
  
  INLINELINKAGE friend MSTypesExport MSMoney operator+(const MSFloat&,const MSMoney&);   
  INLINELINKAGE friend MSTypesExport MSMoney operator-(const MSFloat&,const MSMoney&);   
  INLINELINKAGE friend MSTypesExport MSMoney operator*(const MSFloat&,const MSMoney&);
  INLINELINKAGE friend MSTypesExport double  operator/(const MSFloat&,const MSMoney&);
  
  INLINELINKAGE friend MSTypesExport MSMoney operator+(const MSInt&,const MSMoney&);
  INLINELINKAGE friend MSTypesExport MSMoney operator-(const MSInt&,const MSMoney&);
  INLINELINKAGE friend MSTypesExport MSMoney operator*(const MSInt&,const MSMoney&);
  INLINELINKAGE friend MSTypesExport double  operator/(const MSInt&,const MSMoney&);
  
  INLINELINKAGE friend MSTypesExport MSMoney operator+(double,const MSMoney&);
  INLINELINKAGE friend MSTypesExport MSMoney operator-(double,const MSMoney&);
  INLINELINKAGE friend MSTypesExport MSMoney operator*(double,const MSMoney&);
  INLINELINKAGE friend MSTypesExport double  operator/(double,const MSMoney&);
  
  INLINELINKAGE friend MSTypesExport MSMoney operator+(int,const MSMoney&);
  INLINELINKAGE friend MSTypesExport MSMoney operator-(int,const MSMoney&);
  INLINELINKAGE friend MSTypesExport MSMoney operator*(int,const MSMoney&);
  INLINELINKAGE friend MSTypesExport double  operator/(int,const MSMoney&);
    
  // Be sure to preserve the currency
  MSMoney operator-();
  
  MSMoney& operator+=(const MSMoney&);
  MSMoney& operator-=(const MSMoney&);

  INLINELINKAGE MSMoney& operator+=(const MSFloat&);
  INLINELINKAGE MSMoney& operator+=(const MSInt&);
  INLINELINKAGE MSMoney& operator+=(double);
  INLINELINKAGE MSMoney& operator+=(int);

  INLINELINKAGE MSMoney& operator-=(const MSFloat&);
  INLINELINKAGE MSMoney& operator-=(const MSInt&);
  INLINELINKAGE MSMoney& operator-=(double);
  INLINELINKAGE MSMoney& operator-=(int);

  //  MSMoney& operator*=(const MSMoney&);  -- generate compile time error - makes no sense 
  INLINELINKAGE MSMoney& operator*=(const MSFloat&);
  INLINELINKAGE MSMoney& operator*=(const MSInt&);
  INLINELINKAGE MSMoney& operator*=(double);
  INLINELINKAGE MSMoney& operator*=(int);

  //  MSMoney& operator/=(const MSMoney&);  -- generate compile time error - makes no sense 
  INLINELINKAGE MSMoney& operator/=(const MSFloat&);
  INLINELINKAGE MSMoney& operator/=(const MSInt&);
  INLINELINKAGE MSMoney& operator/=(double);
  INLINELINKAGE MSMoney& operator/=(int);

  friend MSTypesExport ostream& operator<<(ostream&,const MSMoney&);
  
  static void     declareConversionRate(Currency a_,Currency b_,const MSRate& r_)
                   { declareConversionRate(a_,b_,r_,MSTime()); }
  static void     declareConversionRate(Currency,Currency,const MSRate&,const MSTime&) {}
  static void     declareRecipConversionRate(Currency a_,Currency b_,const MSRate& r_)
                   { declareRecipConversionRate(a_,b_,r_,MSTime()); }
  static void     declareRecipConversionRate(Currency,Currency,const MSRate&,const MSTime&) {}
  static void     setDefaultCurrency(Currency);
  static Currency defaultCurrency() { return _defaultCurrency; }
  
protected:
  Currency         _currency;
  static Currency  _defaultCurrency; // initially USD

  // special protected constructors - they allow us to take advantage of the
  // return value optimization (RV0) - see section 12.1c of the ARM
  MSMoney(const MSMoney&,const MSMoney&,FloatOperator);
  MSMoney(const MSMoney&,const MSInt&,FloatOperator);
  MSMoney(const MSMoney&,const MSFloat&,FloatOperator);
  MSMoney(const MSMoney&,double,FloatOperator);
  MSMoney(const MSMoney&,int,FloatOperator);
  MSMoney(const MSFloat&,const MSMoney&,FloatOperator);
  MSMoney(const MSInt&,const MSMoney&,FloatOperator);
  MSMoney(int,const MSMoney&,FloatOperator);
  MSMoney(double,const MSMoney&,FloatOperator);

  static CurrencyData *findCurrency(const char *);
  static int iscurr(int);
};

inline long compare(const MSMoney& aMoney_,const MSMoney& bMoney_)
{ return compare((MSFloat&)aMoney_,(MSFloat&)bMoney_); }

#ifndef MS_NO_INLINES
#ifndef MSMoneyINLINES
#include <MSTypes/MSMoneyInlines.C>
#endif
#endif

#endif  //MSMoneyHEADER
