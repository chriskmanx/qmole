#ifndef MSMBSDateHEADER
#define MSMBSDateHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#include <MSTypes/MSDate.H>

class MSTypesExport MSMBSDate : public MSDate 
{
public:
  MSMBSDate(void);
  MSMBSDate(const char *);
  MSMBSDate(const MSMBSDate&);
  MSMBSDate(const MSDate&);
  MSMBSDate(const MSTime&,MSTime::MSTimeZone=MSTime::Local);
  MSMBSDate(const MSString&);
  MSMBSDate(MSJulian);
  MSMBSDate(int month_,int day_,int year_);
  MSMBSDate(const MSString&,const char* strptimeFormat_);
  ~MSMBSDate(void);

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
  virtual MSError::ErrorStatus set(const char *month_,const char *day_,const char *year_);
  virtual MSError::ErrorStatus set(int month_,int day_,int year_);

  virtual const char *format(MSString*) const;
  virtual const char *format(MSString&) const;
  virtual const char *format(MSString*,MSDateFormat) const;
  virtual const char *format(MSString&,MSDateFormat) const;
  virtual const char *format(MSString*,const MSFormat&) const;
  virtual const char *format(MSString&,const MSFormat&) const;
  virtual const char *format(MSString*,const char *format_) const;
  virtual const char *format(MSString&,const char *format_) const;

  virtual void setToday(void);
  virtual time_t asCalendarTime(MSTime::MSTimeZone=MSTime::Local) const;

  // return represenation of this date as a julian number
  virtual MSJulian date(void) const;

  // Date-specific functions
  virtual MSBoolean between(const MSDate&,const MSDate&) const;
  virtual MSDay dayOfYear(void) const;     // 1-366
  virtual MSDay dayOfMonth(void) const;    // 1-31
  virtual MSDay daysInMonth(void) const;   // 1-31
  virtual MSDay daysInYear(void) const;    // 1-366
  virtual unsigned weekOfYear(void) const; // 1-52
  
  virtual MSDay firstDayOfMonth(void)    const;
  virtual MSDay firstDayOfMonth(MSMonth) const;
  virtual void  setFirstDayOfMonth(void);
  
  virtual MSDay lastDayOfMonth(void)    const;
  virtual MSDay lastDayOfMonth(MSMonth) const;
  virtual void  setLastDayOfMonth(void);
  
  virtual MSBoolean   isSet(void)       const;
  virtual MSBoolean   isValid(void)     const;
  virtual MSBoolean   isLeapYear(void)  const;
  virtual MSBoolean   leap(void)        const;
  virtual const char *nameOfDay(void)   const;
  virtual const char *nameOfMonth(void) const;

  virtual MSDate  max(const MSDate&) const;
  virtual MSDate  min(const MSDate&) const;
  virtual MSMonth month(void) const;
  virtual MSDate  previous(const char* dayName) const;
  virtual MSDate  previous(MSDay) const; 
  virtual MSDay   weekDay(void) const;
  virtual MSYear  year(void) const;
  virtual void    asMonthDayYear(MSMonth&,MSDay&,MSYear&) const;
  
  virtual void nextWeekday(void);
  virtual void prevWeekday(void);
  virtual MSBoolean isWeekday(void) const;
  virtual MSBoolean isWeekend(void) const;

  friend MSTypesExport ostream& operator<<(ostream&,const MSMBSDate&);
  friend MSTypesExport istream& operator>>(istream&,MSMBSDate&);

  operator double() const;
  operator unsigned long() const;

  MSMBSDate& operator=(const MSMBSDate&);

  INLINELINKAGE MSBoolean operator <(const MSMBSDate&) const;
  INLINELINKAGE MSBoolean operator >(const MSMBSDate&) const;
  INLINELINKAGE MSBoolean operator<=(const MSMBSDate&) const;
  INLINELINKAGE MSBoolean operator>=(const MSMBSDate&) const;
  INLINELINKAGE MSBoolean operator==(const MSMBSDate&) const;
  INLINELINKAGE MSBoolean operator!=(const MSMBSDate&) const;

  inline friend MSTypesExport unsigned long hash(const MSMBSDate&,unsigned long size_);

  // date - date returns difference in days.
  int operator-(const MSMBSDate&) const;
  
  // date + term = date 
  MSMBSDate  operator+(const MSTerm&) const;
  MSMBSDate& operator+=(const MSTerm&);

  // date + days = date
  MSMBSDate operator+(int) const;
  MSMBSDate& operator+=(int);
  
  // date + NormYr = date 
  MSMBSDate  operator+(const MSNormalizedYears&) const;
  MSMBSDate& operator+=(const MSNormalizedYears&);
  
  // Note: (Date + Term) - Term != Date in some cases
  // 3/30/90 + 1 month = 4/30/90
  // 4/30/90 - 1 month = 3/31/90
  // Avoid using (date - term) if possible
  MSMBSDate  operator -(const MSTerm&) const;
  MSMBSDate& operator-=(const MSTerm&);

  // date - days = date
  MSMBSDate  operator -(int) const;
  MSMBSDate& operator-=(int);
  
  // date - NormYr = date 
  MSMBSDate  operator -(const MSNormalizedYears&) const;
  MSMBSDate& operator-=(const MSNormalizedYears&);
  
  friend MSTypesExport MSMBSDate operator+(int,const MSMBSDate&);
  friend MSTypesExport MSMBSDate operator+(const MSNormalizedYears&,const MSMBSDate&);
  friend MSTypesExport MSMBSDate operator+(const MSTerm&,const MSMBSDate&);

  MSMBSDate& operator++();        // Prefix
  MSMBSDate& operator--();
  MSMBSDate operator++(int);     // Postfix
  MSMBSDate operator--(int);

  static const char* dayName(MSDay);
  static MSDay       dayOfWeek(const char*);
  static MSBoolean   dayWithinMonth(MSMonth,MSDay,MSYear);
  static MSDay       daysInMonth(MSMonth,MSYear);
  static MSDay       daysInYear(MSYear);
  static MSMonth     indexOfMonth(const char*);
  static MSJulian    asJulianNumber(MSMonth,MSDay,MSYear);
  static MSJulian    as30_360(MSMonth,MSDay,MSYear);
  static MSBoolean   leapYear(MSYear);
  static const char* monthName(MSMonth);
  static int         dcb30_360(const MSMBSDate&,const MSMBSDate&);
  static int         findMatch(const char*,const char**,int);
  static MSMBSDate   today(void);
  static MSJulian    currentDate(void);
  static MSJulian    nullDate(void);

  static MSDateFormat     defaultFormat(void);
  static void             defaultFormat(MSDateFormat);
  static const MSString&  strftimeDefaultFormat(void);
  static void             strftimeDefaultFormat(MSString&);
  static void             strftimeDefaultFormat(const char *);
  static void             defaultConstructToToday(MSBoolean);
  static MSBoolean        defaultConstructToToday(void);
  
protected:
  static MSDateFormat _defaultFormat;
  static MSString     _strftimeDefaultFormat;
  static MSBoolean    _defaultConstructToToday;

  static MSBoolean assertWeekDayNumber(MSDay);
  static MSBoolean assertIndexOfMonth(MSMonth);

  void normalizeAndSet(int month,int day,int year,int lastOfMonth);
  enum Operator {Plus,Minus};
  MSMBSDate(const MSMBSDate&,const MSTerm&,Operator);
  MSMBSDate(const MSMBSDate&,const MSNormalizedYears&,Operator);
};

inline long compare(const MSMBSDate& aDate_,const MSMBSDate& bDate_) 
{ return aDate_==bDate_?0:(aDate_>bDate_?1:-1); }

inline unsigned long hash(const MSMBSDate& aDate_,unsigned long size_) 
{ return aDate_._date%size_; }

#ifndef MS_NO_INLINES
#ifndef MSMBSDateINLINES
#include <MSTypes/MSMBSDateInlines.C>
#endif
#endif

#endif
