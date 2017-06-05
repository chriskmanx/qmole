#ifndef MSTimeHEADER
#define MSTimeHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <time.h>
#include <MSTypes/MSError.H>
#include <MSTypes/MSScalarModel.H>
#include <MSTypes/MSTerm.H>
#include <MSTypes/MSSymbol.H>

class MSDate;
class MSMBSDate;
class MSFormat;
class MSHashTable;

class MSTypesExport MSTime : public MSScalarModel
{
public:
  friend class MSDate;
  friend class MSMBSDate;

  enum MSTimeZone
  {
    A=0,N,B,O,C,P,D,Q,E,R,F,S,G,T,H,U,I,V,K,W,L,X,M,Y,Z,
    BST,EET,EET_DST,EEST=EET_DST,MET,MET_DST,MEST=MET_DST,WET,WET_DST,WEST=WET_DST,MSK,MSD,
    JST,HKT,SGT,KST,KDT,AEST,AESST,ACST,ACSST,AWST,NZST,NZDT,
    NST,NDT,AST,ADT,EST,EDT,CST,CDT,MST,MDT,PST,PDT,AKST,YST=AKST,AKDT,YDT=AKDT,HAST,HADT,HST,
    UT,GMT,Local
  };
  
  enum MSTimeFormat 
  { 
    HoursMinutesSeconds=600,            // Format as 12:30:28
    MonthDayYear=601,                   // Format as 02/28/94
    HoursMinutesSecondsSlash=602,       // Format as 12:30:28 02/28/94
    HoursMinutesSecondsSlashZone=603,   // Format as 12:30:28 02/28/94 PST
    HoursMinutesSecondsSlash4=604,      // Format as 12:30:28 02/28/1994
    HoursMinutesSecondsSlash4Zone=605,  // Format as 12:30:28 02/28/1994 PST
    CalendarTime=606,	                // Format as seconds since 1970 GMT, plus the source time zone
    Strftime=607	                // Format as Standard C Library strftime format
  };

  INLINELINKAGE MSTime(void);
  INLINELINKAGE MSTime(const MSTime&);
  INLINELINKAGE MSTime(const char *, MSTimeZone =Local);
  INLINELINKAGE MSTime(const MSString&, MSTimeZone =Local);
  MSTime(const MSString&,const char* strptimeFormat_,MSTimeZone=Local);
  MSTime(const MSDate&,MSTimeZone=Local);
  INLINELINKAGE MSTime(time_t);
  INLINELINKAGE ~MSTime(void);

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
  virtual MSError::ErrorStatus setFromMSF(const char *);
  static const MSSymbol& symbol(void);

  MSError::ErrorStatus set(const char *, MSTimeZone);
  MSError::ErrorStatus set(const MSString*, MSTimeZone =Local);
  MSError::ErrorStatus set(const MSString&, MSTimeZone =Local);
  const char *format(MSString*) const;
  const char *format(MSString&) const;
  const char *format(MSString*,MSTimeFormat,MSTimeZone=MSTime::Local) const;
  const char *format(MSString&,MSTimeFormat,MSTimeZone=MSTime::Local) const;
  const char *format(MSString*,MSTimeFormat,const char *timeZone_) const;
  const char *format(MSString&,MSTimeFormat,const char *timeZone_) const;
  const char *format(MSString*,const MSFormat&,MSTimeZone=MSTime::Local) const;
  const char *format(MSString&,const MSFormat&,MSTimeZone=MSTime::Local) const;
  const char *format(MSString*,const char* strftimeFormat_) const;
  const char *format(MSString&,const char* strftimeFormat_) const;
  
  MSError::ErrorStatus setNow(void);
  INLINELINKAGE void setInvalid(void);

  INLINELINKAGE MSTime max(const MSTime&) const;
  INLINELINKAGE MSTime min(const MSTime&) const;
  INLINELINKAGE MSBoolean between(const MSTime&,const MSTime&) const;

  INLINELINKAGE MSTime atMidnight(MSTimeZone=Local) const;
  INLINELINKAGE MSBoolean isAtMidnight(MSTimeZone=Local) const;
  unsigned long differenceInDays(const MSTime&,MSTimeZone=Local) const;

  INLINELINKAGE unsigned long secondsPastMinute(MSTimeZone=Local) const;
  INLINELINKAGE unsigned long minutesPastHour(MSTimeZone=Local) const;
  INLINELINKAGE unsigned long hoursPastMidnight(MSTimeZone=Local) const;

  INLINELINKAGE MSBoolean isSet(void)   const;
  INLINELINKAGE MSBoolean isValid(void) const;
  void unset(void);
  
  INLINELINKAGE operator double() const;
  INLINELINKAGE operator time_t() const;
  
  INLINELINKAGE MSBoolean operator==(const MSTime&) const;
  INLINELINKAGE MSBoolean operator!=(const MSTime&) const;
  INLINELINKAGE MSBoolean operator<=(const MSTime&) const;
  INLINELINKAGE MSBoolean operator>=(const MSTime&) const;
  INLINELINKAGE MSBoolean operator< (const MSTime&) const;
  INLINELINKAGE MSBoolean operator> (const MSTime&) const;
  
  INLINELINKAGE MSTime& operator=(const MSTime&);
  INLINELINKAGE MSTime& operator=(const MSString&);
  INLINELINKAGE MSTime& operator=(const char *);
  INLINELINKAGE MSTime& operator=(time_t);

  inline friend MSTypesExport unsigned long hash(const MSTime&,unsigned long size_);

  // Add seconds.
  MSTime  operator+(long) const;
  INLINELINKAGE MSTime& operator+=(long);
  friend MSTypesExport MSTime operator+(long,const MSTime&);

  // Subtract seconds.
  MSTime  operator-(long) const;
  INLINELINKAGE MSTime& operator-=(long);

  // time + term = time
  MSTime  operator+(const MSTerm&) const;
  MSTime& operator+=(const MSTerm&);
  friend MSTypesExport MSTime operator+(const MSTerm&,const MSTime&);

  // Note: (Time + Term) - Time != time in some cases
  // 00:00:00 3/30/90 + 1 month = 00:00:00 4/30/90
  // 00:00:00 4/30/90 - 1 month = 00:00:00 3/31/90
  // Avoid using (time - term) if possible
  MSTime  operator-(const MSTerm&) const;
  MSTime& operator-=(const MSTerm&);

  INLINELINKAGE MSTime& operator++();      // Prefix
  INLINELINKAGE MSTime& operator--();
  INLINELINKAGE MSTime operator++(int);   // Postfix
  INLINELINKAGE MSTime operator--(int);

  static MSTimeZone setLocalTimeZone(void);
  static INLINELINKAGE MSTimeZone localTimeZone(void);
  static time_t currentTime(void);
  static INLINELINKAGE time_t nullTime(void);
  static INLINELINKAGE MSTime now(void);

  static INLINELINKAGE MSTimeFormat defaultFormat(void);
  static INLINELINKAGE void defaultFormat(MSTimeFormat);
  static INLINELINKAGE const MSString& strftimeDefaultFormat(void);
  static INLINELINKAGE void strftimeDefaultFormat(MSString&);
  static INLINELINKAGE void strftimeDefaultFormat(const char *);
  static INLINELINKAGE void defaultConstructToNow(MSBoolean);
  static INLINELINKAGE MSBoolean defaultConstructToNow(void);
  
  friend MSTypesExport ostream& operator<<(ostream&,const MSTime&);

protected:
  time_t              _time; // Seconds since 1970 GMT
  static MSHashTable *_zoneHashTable;
  static MSTimeZone   _localTimeZone;
  static MSTimeFormat _defaultFormat;
  static MSString     _strftimeDefaultFormat;
  static MSBoolean    _defaultConstructToNow;
  static time_t       _nullTime;
  
  static const long SECS_IN_MIN;
  static const long SECS_IN_HOUR;
  static const long SECS_IN_DAY;

  time_t convertDate(const MSDate&) const;
  long zoneOffset(const char*) const;
  INLINELINKAGE long zoneOffset(MSTimeZone) const;
  static long zoneOffset(time_t, MSTimeZone);
  static long zoneOffset(const struct tm *);
  static const char *zoneName(const struct tm *);
  
  MSError::ErrorStatus internalSet(const char *, MSTimeZone =Local);
  void strftimeFormat(MSString&,const char*,const char*) const;
  void strftimeFormat(MSString&,const char*,MSTimeZone,MSBoolean =MSFalse) const;
  long scanTimeZone(const char *);
  static MSHashTable *zoneHashTable(void) { return _zoneHashTable; }
  static MSHashTable *initZoneHashTable(void);
  static long greenwichMeanTime(struct tm*,MSTimeZone);
};

inline long compare(const MSTime& aTime_,const MSTime& bTime_) 
{ return aTime_==bTime_?0:(aTime_>bTime_?1:-1); }

inline unsigned long hash(const MSTime& aTime_,unsigned long size_) 
{ return aTime_._time%size_; }

#ifndef MS_NO_INLINES
#ifndef MSTimeINLINES
#include <MSTypes/MSTimeInlines.C>
#endif
#endif

#endif
