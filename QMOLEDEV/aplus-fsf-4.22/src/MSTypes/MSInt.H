#ifndef MSIntHEADER
#define MSIntHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSError.H>
#include <MSTypes/MSScalarModel.H>
#include <MSTypes/MSString.H>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

class MSFormat;

#if defined (MS_PREDECLARE_NONTEMPLATE_FRIEND_FUNCTIONS)
class MSInt;
inline long compare (const MSInt &, const MSInt &);
#endif

class MSTypesExport MSInt : public MSScalarModel
{
public:

  enum MSIntFormat
  {
    WithoutCommas=50,   // 1234    
    WithCommas=51       // 1,234
  };
  enum {MaximumLength=10};
  
  MSInt(void);
  MSInt(int);
  MSInt(const MSInt&);

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
  virtual MSError::ErrorStatus set(int);
  const char *format(MSString*) const;
  const char *format(MSString&) const;
  const char *format(MSString*,MSIntFormat) const;
  const char *format(MSString&,MSIntFormat) const;
  const char *format(MSString*,const MSFormat&) const;
  const char *format(MSString&,const MSFormat&) const;
  
  operator int() const;
  operator double() const;
  int operator()() const; // function call operator

  MSInt& operator=(const MSInt&);
  MSInt& operator=(int);
  MSInt& operator=(const char *);

  inline friend MSTypesExport long compare(const MSInt&,const MSInt&);
  inline friend MSTypesExport unsigned long hash(const MSInt&,unsigned long size_);

  MSBoolean operator==(const MSInt&) const;
  MSBoolean operator!=(const MSInt&) const;
  MSBoolean operator< (const MSInt&) const;
  MSBoolean operator<=(const MSInt&) const;
  MSBoolean operator> (const MSInt&) const;
  MSBoolean operator>=(const MSInt&) const;
  
  MSBoolean operator==(int) const;
  MSBoolean operator!=(int) const;
  MSBoolean operator< (int) const;
  MSBoolean operator<=(int) const;
  MSBoolean operator> (int) const;
  MSBoolean operator>=(int) const;
  
  // Prefix - add/subtract one, then return result
  MSInt& operator++();
  MSInt& operator--();

  // Postfix - add/subtract one, then return the initial value
  MSInt operator++(int);
  MSInt operator--(int);

  MSInt& operator+=(const MSInt&);
  MSInt& operator+=(int);
  
  MSInt& operator-=(const MSInt&);
  MSInt& operator-=(int);
  
  MSInt& operator*=(const MSInt&);
  MSInt& operator*=(int);
  
  MSInt& operator/=(const MSInt&);
  MSInt& operator/=(int);
  
  MSInt operator-();
  
  friend MSTypesExport int operator+(const MSInt&,int);
  friend MSTypesExport int operator+(int,const MSInt&);
  friend MSTypesExport int operator+(const MSInt& d1,const MSInt&);

  friend MSTypesExport int operator-(const MSInt&,int);
  friend MSTypesExport int operator-(int,const MSInt&);
  friend MSTypesExport int operator-(const MSInt& d1,const MSInt&);
  
  friend MSTypesExport int operator*(const MSInt&,int);
  friend MSTypesExport int operator*(int,const MSInt&);
  friend MSTypesExport int operator*(const MSInt& d1,const MSInt&);
  
  friend MSTypesExport int operator/(const MSInt&,int);
  friend MSTypesExport int operator/(int,const MSInt&);
  friend MSTypesExport int operator/(const MSInt&,const MSInt&);

  friend MSTypesExport int operator%(const MSInt&,int);
  friend MSTypesExport int operator%(int,const MSInt&);
  friend MSTypesExport int operator%(const MSInt&,const MSInt&);
  
  friend MSTypesExport MSBoolean operator==(int,const MSInt&);
  friend MSTypesExport MSBoolean operator!=(int,const MSInt&);
  friend MSTypesExport MSBoolean operator< (int,const MSInt&);
  friend MSTypesExport MSBoolean operator<=(int,const MSInt&);
  friend MSTypesExport MSBoolean operator> (int,const MSInt&);
  friend MSTypesExport MSBoolean operator>=(int,const MSInt&);

  friend MSTypesExport ostream& operator<<(ostream& aStream_,const MSInt&);
  friend MSTypesExport istream& operator>>(istream& aStream_,MSInt&);

  MSBoolean isSet(void) const;
  void unset(void);

protected:
  const char *formatInt(MSString& aString_,MSIntFormat format_,unsigned long modifier_,int value_) const;

private:
  int             _int; 
  MSBoolean       _isSet;
};

inline long compare(const MSInt& aInt_,const MSInt& bInt_)
{ return (aInt_._int-bInt_._int); }

inline unsigned long hash(const MSInt& aInt_,unsigned long size_)
{ return aInt_._int%size_; }

#ifndef MS_NO_INLINES
#ifndef MSIntINLINES
#include <MSTypes/MSIntInlines.C>
#endif
#endif

#endif


