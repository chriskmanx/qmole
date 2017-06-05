#ifndef MSDateHEADER
#define MSDateHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSError.H>
#include <MSTypes/MSScalarModel.H>
#include <MSTypes/MSTerm.H>
#include <MSTypes/MSTime.H>
#include <MSTypes/MSNormalizedYears.H>
#include <MSTypes/MSSymbol.H>
#include <time.h>
#include <sys/time.h>

class MSFormat;

typedef unsigned	MSDay;
typedef unsigned	MSMonth;
typedef unsigned	MSYear;
typedef unsigned long	MSJulian;

class MSTypesExport MSDate : public MSScalarModel 
{
public:

  enum MSDateFormat
  {
    Slash=100,	        // Format as 11/15/92 (American)
    Slash4=101,	        // Format as 11/15/1992 (American)
    Terse=102, 	        // Format as 11-Nov-92
    Terse4=103, 	// Format as 11-Nov-1992
    Long=104,	        // Format as November 15, 1992
    MonthYear=105,	// Format as Nov 92
    YearMonthDay=106,   // Format as 1992/11/15
    Year2MonthDay=107,  // Format as 92/11/15
    EuropeanDot=108,    // Format as 15.11.92
    EuropeanDot4=109,   // Format as 15.11.1992
    Database=110,	// Format as 19921115 (yyyymmdd)
    DataBase=Database,	// Same as above (preserved for backward compatibility only)
    Julian=111,	        // Format as a Julian day number
    Strftime=112	// Format as Standard C Library strftime format
  };
  
  // order of printing fields in the above formats 
  // used also for scanning fields on input
  enum MSDateLocale {European,American,Japanese};
  enum MSWeekDays   {Monday=1,Tuesday=2,Wednesday=3,Thursday=4,Friday=5,Saturday=6,Sunday=7};
    
  MSDate(void);
  MSDate(const char *);
  MSDate(const MSDate&);
  MSDate(const MSTime&,MSTime::MSTimeZone=MSTime::Local);
  MSDate(const MSString&);
  MSDate(MSJulian);
  MSDate(int month_,int day_,int year_);
  MSDate(const MSString&,const char* strptimeFormat_);
  ~MSDate(void);

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

  // return internal representation of this date
  MSJulian asInternal(void) const;
  
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

  virtual MSDate   max(const MSDate&) const;
  virtual MSDate   min(const MSDate&) const;
  virtual MSMonth  month(void) const;
  virtual MSDate   previous(const char* dayName) const;
  virtual MSDate   previous(MSDay) const; 
  virtual MSDay    weekDay(void) const;
  virtual MSYear   year(void) const;
  virtual void     asMonthDayYear(MSMonth&,MSDay&,MSYear&) const;

  virtual void nextWeekday(void);
  virtual void prevWeekday(void);
  virtual MSBoolean isWeekday(void) const;
  virtual MSBoolean isWeekend(void) const;

  virtual void unset(void);
  
  friend MSTypesExport ostream& operator<<(ostream&,const MSDate&);
  friend MSTypesExport istream& operator>>(istream&,MSDate&);

  operator double() const;
  operator unsigned long() const;

  MSDate& operator=(const MSDate&);

  INLINELINKAGE MSBoolean operator <(const MSDate&) const;
  INLINELINKAGE MSBoolean operator >(const MSDate&) const;
  INLINELINKAGE MSBoolean operator<=(const MSDate&) const;
  INLINELINKAGE MSBoolean operator>=(const MSDate&) const;
  INLINELINKAGE MSBoolean operator==(const MSDate&) const;
  INLINELINKAGE MSBoolean operator!=(const MSDate&) const;

  inline friend MSTypesExport unsigned long hash(const MSDate&,unsigned long size_);

  // date - date returns difference in days.
  int operator-(const MSDate&) const;
  
  // date + term = date 
  MSDate  operator+(const MSTerm&) const;
  MSDate& operator+=(const MSTerm&);

  // date + days = date
  MSDate operator+(int) const;
  MSDate& operator+=(int);
  
  // date + NormYr = date 
  MSDate  operator+(const MSNormalizedYears&) const;
  MSDate& operator+=(const MSNormalizedYears&);
  
  // Note: (Date + Term) - Term != Date in some cases
  // 3/30/90 + 1 month = 4/30/90
  // 4/30/90 - 1 month = 3/31/90
  // Avoid using (date - term) if possible
  MSDate  operator -(const MSTerm&) const;
  MSDate& operator-=(const MSTerm&);

  // date - days = date
  MSDate  operator -(int) const;
  MSDate& operator-=(int);
  
  // date - NormYr = date 
  MSDate  operator -(const MSNormalizedYears&) const;
  MSDate& operator-=(const MSNormalizedYears&);
  
  friend MSTypesExport MSDate operator+(int,const MSDate&);
  friend MSTypesExport MSDate operator+(const MSNormalizedYears&,const MSDate&);
  friend MSTypesExport MSDate operator+(const MSTerm&,const MSDate&);

  MSDate& operator++();        // Prefix
  MSDate& operator--();
  MSDate operator++(int);     // Postfix
  MSDate operator--(int);

  static const char* dayName(MSDay);
  static MSDay       dayOfWeek(const char*);
  static MSBoolean   dayWithinMonth(MSMonth,MSDay,MSYear);
  static MSDay       daysInMonth(MSMonth,MSYear);
  static MSDay       daysInYear(MSYear);
  static MSMonth     indexOfMonth(const char*);
  static MSJulian    asJulianNumber(MSMonth,MSDay,MSYear);
  static MSBoolean   leapYear(MSYear);
  static const char* monthName(MSMonth);
  static int         dcb30_360(const MSDate&,const MSDate&);
  static int         findMatch(const char*,const char**,int);
  static MSDate      today(void);
  static MSJulian    currentDate(void);
  static MSJulian    nullDate(void);

  static MSDateFormat      defaultFormat(void);
  static void              defaultFormat(MSDateFormat);
  static const MSString&   strftimeDefaultFormat(void);
  static void              strftimeDefaultFormat(MSString&);
  static void              strftimeDefaultFormat(const char *);
  static void              defaultConstructToToday(MSBoolean);
  static MSBoolean         defaultConstructToToday(void);

  static MSDateLocale locale(void);
  static void locale(MSDateLocale);
  
protected:
  MSJulian              _date;      // Julian Day Number (Not same as Julian date.)
  static MSDateLocale	_locale;
  static MSJulian       _override;
  static int		_useOverride;
  static int		_firstTime;
  static MSDateFormat   _defaultFormat;
  static MSString       _strftimeDefaultFormat;
  static MSBoolean      _defaultConstructToToday;
  static MSJulian       _nullDate;

  static const unsigned char _daysInMonth[];
  static const MSDay _firstDayOfEachMonth[];
  static const char *_monthNames[];
  static const char *_ucMonthNames[];
  static const char *_weekDayNames[];
  static const char *_ucWeekDayNames[];

  static MSBoolean assertWeekDayNumber(MSDay);
  static MSBoolean assertIndexOfMonth(MSMonth);

  void normalizeAndSet(int month,int day,int year,int lastOfMonth);
  enum Operator {Plus,Minus};
  MSDate(const MSDate&,const MSTerm&,Operator);
  MSDate(const MSDate&,const MSNormalizedYears&,Operator);

  static MSDateLocale initLocale(void);
};

inline long compare(const MSDate& aDate_,const MSDate& bDate_) 
{ return aDate_==bDate_?0:(aDate_>bDate_?1:-1); }

inline unsigned long hash(const MSDate& aDate_,unsigned long size_) 
{ return aDate_._date%size_; }

#ifndef MS_NO_INLINES
#ifndef MSDateINLINES
#include <MSTypes/MSDateInlines.C>
#endif
#endif

#endif
