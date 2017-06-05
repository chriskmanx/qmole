#ifndef MSBoolHEADER
#define MSBoolHEADER

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
#include <MSTypes/MSSymbol.H>

class MSFormat;

#if defined (MS_PREDECLARE_NONTEMPLATE_FRIEND_FUNCTIONS)
class MSBool;
inline long compare (const MSBool &, const MSBool &);
#endif

class MSTypesExport MSBool: public MSScalarModel
{
public:
  
  enum MSBoolFormat {YesAndNo=0,TrueAndFalse=1,Binary=2};
  
  MSBool(MSBoolean);
  MSBool(void);

  virtual MSString asString(void) const;
  virtual MSString asDebugInfo(void) const;
  virtual MSString asMSF(void) const;
  virtual MSString className(void) const;
  virtual const MSSymbol& type(void) const;
  virtual MSModel *clone(void) const;
  virtual MSModel *create(void) const; 
  virtual void assign(const MSModel&);
  virtual long compare(const MSModel&) const; 
  virtual MSError::ErrorStatus set(const char*);
  virtual MSError::ErrorStatus setFromMSF(const char*);  
  static const MSSymbol& symbol(void);

  virtual MSError::ErrorStatus set(const MSString*);
  virtual MSError::ErrorStatus set(const MSString&);
  virtual MSError::ErrorStatus set(int);
  const char *format(MSString*) const;
  const char *format(MSString&) const;
  const char *format(MSString*,MSBoolFormat) const;
  const char *format(MSString&,MSBoolFormat) const;
  const char *format(MSString*,const MSFormat&) const;
  const char *format(MSString&,const MSFormat&) const;

  MSBoolean isSet(void) const;
  MSBoolean isTrue(void) const;
  MSBoolean isFalse(void) const;

  void unset(void);
  
  operator int() const;
  operator MSBoolean() const;

  inline friend MSTypesExport long compare (const MSBool &, const MSBool &);

  MSBoolean operator==(const MSBool&) const;
  MSBoolean operator==(int) const;
  MSBoolean operator!=(const MSBool&) const;
  MSBoolean operator!=(int) const;

#if !defined MS_ENUM_COMPARE_BUG
  MSBoolean operator!=(MSBoolean) const;
  MSBoolean operator==(MSBoolean) const;
  friend MSTypesExport MSBoolean operator==(MSBoolean,const MSBool&);
  friend MSTypesExport MSBoolean operator!=(MSBoolean,const MSBool&);
#endif

  MSBoolean operator <(const MSBool&) const;
  MSBoolean operator >(const MSBool&) const;
  MSBoolean operator<=(const MSBool&) const;
  MSBoolean operator>=(const MSBool&) const;
  
  friend MSTypesExport MSBoolean operator==(int,const MSBool&);
  friend MSTypesExport MSBoolean operator!=(int,const MSBool&);

  friend MSTypesExport ostream& operator<<(ostream& os_,const MSBool&);
  friend MSTypesExport istream& operator>>(istream& aStream_,MSBool&);

  MSBool& operator=(const MSBool&);
  MSBool& operator=(MSBoolean);
  MSBool& operator=(int);

private:
  MSBoolean       _bool;
  MSBoolean       _isSet;
};

inline long compare(const MSBool & aBool_,const MSBool & bBool_)
{ return (aBool_._bool==bBool_._bool)?0:(aBool_._bool>bBool_._bool)?1:-1; }

#ifndef MS_NO_INLINES
#include <MSTypes/MSBoolInlines.C>
#endif

#endif
