#ifndef MSUnsignedHEADER
#define MSUnsignedHEADER

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
#include <MSTypes/MSInt.H>			// for MSIntFormat

class MSFormat;

#if defined (MS_PREDECLARE_NONTEMPLATE_FRIEND_FUNCTIONS)
class MSUnsigned;
inline long compare (const MSUnsigned &, const MSUnsigned &);
#endif

class MSTypesExport MSUnsigned : public MSScalarModel
{
public:

  enum {MaximumLength=10};
  
  MSUnsigned(void);
  MSUnsigned(unsigned);
  MSUnsigned(const MSUnsigned&);

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
  virtual MSError::ErrorStatus set(unsigned);
  const char *format(MSString*) const;
  const char *format(MSString&) const;
  const char *format(MSString*,MSInt::MSIntFormat) const;
  const char *format(MSString&,MSInt::MSIntFormat) const;
  const char *format(MSString*,const MSFormat&) const;
  const char *format(MSString&,const MSFormat&) const;
  
  operator unsigned() const;
  operator double() const;
  unsigned operator()() const; // function call operator

  MSUnsigned& operator=(const MSUnsigned&);
  MSUnsigned& operator=(unsigned);
  MSUnsigned& operator=(const char *);

  inline friend MSTypesExport long compare(const MSUnsigned&,const MSUnsigned&);
  inline friend MSTypesExport unsigned long hash(const MSUnsigned&,unsigned long size_);

  MSBoolean operator==(const MSUnsigned&) const;
  MSBoolean operator!=(const MSUnsigned&) const;
  MSBoolean operator< (const MSUnsigned&) const;
  MSBoolean operator<=(const MSUnsigned&) const;
  MSBoolean operator> (const MSUnsigned&) const;
  MSBoolean operator>=(const MSUnsigned&) const;
  
  MSBoolean operator==(unsigned) const;
  MSBoolean operator!=(unsigned) const;
  MSBoolean operator< (unsigned) const;
  MSBoolean operator<=(unsigned) const;
  MSBoolean operator> (unsigned) const;
  MSBoolean operator>=(unsigned) const;
  
  // Prefix - add/subtract one, then return result
  MSUnsigned& operator++();
  MSUnsigned& operator--();

  // Postfix - add/subtract one, then return the initial value
  MSUnsigned operator++(int);
  MSUnsigned operator--(int);

  MSUnsigned& operator+=(const MSUnsigned&);
  MSUnsigned& operator+=(unsigned);
  
  MSUnsigned& operator-=(const MSUnsigned&);
  MSUnsigned& operator-=(unsigned);
  
  MSUnsigned& operator*=(const MSUnsigned&);
  MSUnsigned& operator*=(unsigned);
  
  MSUnsigned& operator/=(const MSUnsigned&);
  MSUnsigned& operator/=(unsigned);
  
  MSUnsigned operator-();
  
  inline friend MSTypesExport unsigned operator+(const MSUnsigned&,unsigned);
  inline friend MSTypesExport unsigned operator+(unsigned,const MSUnsigned&);
  inline friend MSTypesExport unsigned operator+(const MSUnsigned& d1,const MSUnsigned&);

  inline friend MSTypesExport unsigned operator-(const MSUnsigned&,unsigned);
  inline friend MSTypesExport unsigned operator-(unsigned,const MSUnsigned&);
  inline friend MSTypesExport unsigned operator-(const MSUnsigned& d1,const MSUnsigned&);
  
  inline friend MSTypesExport unsigned operator*(const MSUnsigned&,unsigned);
  inline friend MSTypesExport unsigned operator*(unsigned,const MSUnsigned&);
  inline friend MSTypesExport unsigned operator*(const MSUnsigned& d1,const MSUnsigned&);
  
  inline friend MSTypesExport unsigned operator/(const MSUnsigned&,unsigned);
  inline friend MSTypesExport unsigned operator/(unsigned,const MSUnsigned&);
  inline friend MSTypesExport unsigned operator/(const MSUnsigned&,const MSUnsigned&);

  inline friend MSTypesExport unsigned operator%(const MSUnsigned&,unsigned);
  inline friend MSTypesExport unsigned operator%(unsigned,const MSUnsigned&);
  inline friend MSTypesExport unsigned operator%(const MSUnsigned&,const MSUnsigned&);
  
  inline friend MSTypesExport MSBoolean operator==(unsigned,const MSUnsigned&);
  inline friend MSTypesExport MSBoolean operator!=(unsigned,const MSUnsigned&);
  inline friend MSTypesExport MSBoolean operator< (unsigned,const MSUnsigned&);
  inline friend MSTypesExport MSBoolean operator<=(unsigned,const MSUnsigned&);
  inline friend MSTypesExport MSBoolean operator> (unsigned,const MSUnsigned&);
  inline friend MSTypesExport MSBoolean operator>=(unsigned,const MSUnsigned&);

  friend MSTypesExport ostream& operator<<(ostream& aStream_,const MSUnsigned&);
  friend MSTypesExport istream& operator>>(istream& aStream_,MSUnsigned&);

  MSBoolean isSet(void) const;
  void unset(void);
  
protected:
  const char *formatUnsigned(MSString& aString_,MSInt::MSIntFormat format_,unsigned long modifier_,unsigned value_) const;

private:
  unsigned        _unsigned; 
  MSBoolean       _isSet;
};

inline long compare(const MSUnsigned& aUnsigned_,const MSUnsigned& bUnsigned_)
{ return (aUnsigned_._unsigned-bUnsigned_._unsigned); }

inline unsigned long hash(const MSUnsigned& aUnsigned_,unsigned long size_)
{ return aUnsigned_._unsigned%size_; }

#ifndef MS_NO_INLINES
#ifndef MSUnsignedINLINES
#include <MSTypes/MSUnsignedInlines.C>
#endif
#endif

inline unsigned operator+(const MSUnsigned& a_,unsigned b_)
{ return a_._unsigned+b_; }
inline unsigned operator+(unsigned a_,const MSUnsigned& b_)
{ return a_+b_._unsigned; }
inline unsigned operator+(const MSUnsigned& a_,const MSUnsigned& b_)
{ return a_._unsigned+b_._unsigned; }

inline unsigned operator-(const MSUnsigned& a_,unsigned b_)
{ return a_._unsigned-b_; }
inline unsigned operator-(unsigned a_,const MSUnsigned& b_)
{ return a_-b_._unsigned; }
inline unsigned operator-(const MSUnsigned& a_,const MSUnsigned& b_)
{ return a_._unsigned-b_._unsigned; }

inline unsigned operator*(const MSUnsigned& a_,unsigned b_)
{ return a_._unsigned*b_; }
inline unsigned operator*(unsigned a_,const MSUnsigned& b_)
{ return a_*b_._unsigned; }
inline unsigned operator*(const MSUnsigned& a_,const MSUnsigned& b_)
{ return a_._unsigned*b_._unsigned; }

inline unsigned operator/(const MSUnsigned& a_,unsigned b_)
{ return a_._unsigned/b_; }
inline unsigned operator/(unsigned a_,const MSUnsigned& b_)
{ return a_/b_._unsigned; }
inline unsigned operator/(const MSUnsigned& a_,const MSUnsigned& b_)
{ return a_._unsigned/b_._unsigned; }

inline unsigned operator%(const MSUnsigned& a_,unsigned b_)
{ return a_._unsigned%b_; }
inline unsigned operator%(unsigned a_,const MSUnsigned& b_)
{ return a_%b_._unsigned; }
inline unsigned operator%(const MSUnsigned& a_,const MSUnsigned& b_)
{ return a_._unsigned%b_._unsigned; }

inline MSBoolean operator==(unsigned a_,const MSUnsigned& b_)
{ return MSBoolean(a_==b_._unsigned); } 
inline MSBoolean operator!=(unsigned a_,const MSUnsigned& b_)
{ return MSBoolean(a_!=b_._unsigned); } 
inline MSBoolean operator< (unsigned a_,const MSUnsigned& b_)
{ return MSBoolean(a_< b_._unsigned); } 
inline MSBoolean operator<=(unsigned a_,const MSUnsigned& b_)
{ return MSBoolean(a_<=b_._unsigned); } 
inline MSBoolean operator> (unsigned a_,const MSUnsigned& b_)
{ return MSBoolean(a_> b_._unsigned); } 
inline MSBoolean operator>=(unsigned a_,const MSUnsigned& b_)
{ return MSBoolean(a_>=b_._unsigned); }

#endif


