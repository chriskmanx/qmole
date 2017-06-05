///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSMBSDate.H>
#include <MSTypes/MSString.H>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <time.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <MSTypes/MSUtil.H>
#include <MSTypes/MSFormat.H>
#include <MSTypes/MSMessageLog.H>

// static constants
const unsigned long JAN_01_1900=1900; // Reference date for 30/360 calculations
static const char *NullString="";

MSDate::MSDateFormat MSMBSDate::_defaultFormat(MSDate::Slash4);
MSString MSMBSDate::_strftimeDefaultFormat("%D");
MSBoolean MSMBSDate::_defaultConstructToToday(MSFalse);

#ifdef MS_NO_INLINES
#include <MSTypes/MSMBSDateInlines.C>
#endif

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// other constructors
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MSMBSDate::MSMBSDate(const MSDate& aDate_)
{
  MSMonth m; MSDay d; MSYear y;
  aDate_.asMonthDayYear(m,d,y);
  if (d==31) d--;
  _date=as30_360(m,d,y);
}

MSMBSDate::MSMBSDate(const MSTime &aDate_,MSTime::MSTimeZone zone_)
: MSDate(MSJulian(0))
{
  MSString s;
  (void)aDate_.format(&s,MSTime::MonthDayYear,zone_);
  set(&s);
}

#if defined(MS_NEED_STRPTIME_DECLARATION)
extern "C" char *strptime(const char*, const char*, struct tm*);
#endif

MSMBSDate::MSMBSDate(const MSString& aString_,const char* strptimeFormat_)
: MSDate(MSJulian(0))
{
  struct tm aTimeStruct={0};
#if defined(MS_NO_STRPTIME)
  MSMessageLog::errorMessage("MSMBSDate: strptime() not supported for this platform.\n");
  _date=0;
#else
  char *pString=strptime((char *)aString_.string(),(char *)strptimeFormat_,&aTimeStruct);
  if (pString==0) _date=0;
  else _date=as30_360(aTimeStruct.tm_mon+1,aTimeStruct.tm_mday,aTimeStruct.tm_year+1900);
#endif
}

MSMBSDate::~MSMBSDate(void) {}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// MSModel virtual methods
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MSString MSMBSDate::asString(void) const
{ MSString aString; return MSString(format(aString,MSDate::defaultFormat())); }

MSString MSMBSDate::asDebugInfo(void) const
{
  MSString result("MSMBSDate(@");
  result+=MSString((unsigned long)this).d2x().lowerCase();
  result+=",_date=";
  result+=MSString((unsigned long)asInternal());
  result+=",_locale=";
  result+=MSString((int)_locale);
  result+=",_override=";
  result+=MSString((unsigned long)_override);
  result+=",_useOverride=";
  result+=MSString((int)_useOverride);
  result+=",_firstTime=";
  result+=MSString((int)_firstTime);
  result+=",_defaultFormat=";
  result+=MSString((int)_defaultFormat);
  result+=",_strftimeDefaultFormat=";
  result+=_strftimeDefaultFormat;
  result+=",_defaultConstructToToday=";
  result+=(defaultConstructToToday()==MSTrue?"MSTrue":"MSFalse");
  result+=",_type=";
  result+=type().symbolName();
  result+=")";
  return MSString(result);
}

MSString MSMBSDate::className(void) const
{ return MSString("MSMBSDate"); }

const MSSymbol& MSMBSDate::type(void) const
{ return symbol(); }

MSModel *MSMBSDate::clone(void) const
{ return new MSMBSDate(*this); }

MSModel *MSMBSDate::create(void) const
{ return new MSMBSDate(); }

void MSMBSDate::assign(const MSModel& aModel_)
{ *this=(MSMBSDate&)aModel_; }

long MSMBSDate::compare(const MSModel& aModel_) const
{ return ::compare(*this,(MSMBSDate&)aModel_); }

MSString MSMBSDate::asMSF(void) const
{
  MSString buf;
  if (isSet()==MSTrue) (void)format(&buf,MSDate::Database);
  return buf;
}

MSError::ErrorStatus MSMBSDate::setFromMSF(const char *pString_)
{
  int code;
  if (pString_!=0) code=set(pString_);
  else code=MSError::BadMSFString;
  return (MSError::ErrorStatus)code;
}

const MSSymbol& MSMBSDate::symbol(void) 
{
  static MSSymbol sym ("MSMBSDate");
  return sym;
}

MSError::ErrorStatus MSMBSDate::set(const MSString *pString_) { return set(pString_->string()); }
MSError::ErrorStatus MSMBSDate::set(const MSString& aString_) { return set(aString_.string()); }

MSError::ErrorStatus MSMBSDate::set(const char *pString_)
{
  MSError::ErrorStatus rc=MSError::MSSuccess;
  char *f1,*f2,*f3;
  char buf[80];
  char *cp;

  if (*pString_=='\0'||strcmp(pString_,NullString)==0||strcmp(pString_,"/  /")==0)
   {
     _date=nullDate(); // handle null string
   }
  else if (MSUtil::hasAlpha(pString_))
   {
     // No alpha input formats are currently allowed
     // in the future we could allow any of the
     // output formats, e.g., 11-Nov-1992
     rc=MSError::BadDate;
   }
  else
   {
     // Look for slashes
     strncpy(buf,pString_,sizeof(buf));
     f1=buf;		// point to the start of string
     cp=strchr(buf,'/');
     if (cp)
      {
	*cp='\0'; 	// nul-terminate first field
	cp++;		// point to the start of second field
	f2=cp;
	cp=strchr(f2,'/');
	if (cp)
	 {
	   *cp='\0';	// nul-terminate second field
	   cp++;        // point to the start of third field
	   f3=cp;
	   switch (_locale)
	    {
	      // set expects month, day, year
	    case European: return set(f2,f1,f3);
	    case American: return set(f1,f2,f3);
	    case Japanese: return set(f2,f3,f1);
            default: MSMessageLog::warningMessage("MSMBSDate: invalid value of order flag\n");
	    }
	 }
	else rc=MSError::BadDate;
      }
     cp=strchr(buf,'.');
     if(cp!=0)
      {
 	// check if format = dd.mm.yyyy
	*cp='\0'; 	// nul-terminate first field
	cp++;           // point to the start of second field
	f2=cp;
	cp=strchr(f2,'.');
	if (cp!=0)
	 {
	   *cp='\0';	// nul-terminate second field
	   cp++;        // point to the start of third field
	   f3=cp;
	   // set expects month, day, year
	   return set(f2,f1,f3);
	 }
	else rc=MSError::BadDate;
      }
     cp=strchr(buf,'-');
     if (cp)
      {
	*cp='\0'; 	// nul-terminate first field
	cp++;		// point to the start of second field
	f2=cp;
	cp=strchr(f2,'-');
	if (cp)
	 {
	   *cp='\0';	// nul-terminate second field
	   cp++;        // point to the start of third field
	   f3=cp;
	   switch (_locale)
	    {
	      // set expects month, day, year
	    case European: return set(f2,f1,f3);
	    case American: return set(f1,f2,f3);
	    case Japanese: return set(f2,f3,f1);
            default: MSMessageLog::warningMessage("MSMBSDate: invalid value of order flag\n");
	    }
	 }
	else rc=MSError::BadDate;
      }
     else
      {
	// no slashes, no period, no dashes, no alpha - must be internal fmt (digits)
	if (MSUtil::isNumeric(buf)==MSFalse)
	 {
	   if (MSUtil::isSpace(buf)==MSTrue) _date=nullDate(); // May be full of spaces
	   else rc=MSError::BadDate;
	 }
	else
	 {
           // format of yyyymmdd
	   if(strlen(pString_)>7)
	    {
              f1=f2=f3=buf;
              strncpy(buf,pString_,4);
              f1[4]='\0'; // strncpy won't insert null if not encountered
              f2+=5;
              strncpy(f2,pString_+4,2);
              f2[2]='\0';
              f3+=8;
              strncpy(f3,pString_+6,2);
              f3[2]='\0';
              // set expects month, day, year
	      return set(f2,f3,f1);
	    }
	   else rc=MSError::BadDate;
	 }
      }
   }
  if (rc==MSError::MSSuccess) changed();
  return (rc);
}

MSError::ErrorStatus MSMBSDate::set(const char *m_,const char *d_,const char *y_)
{
  if ((MSUtil::isNumeric(m_)==MSFalse)||
      (MSUtil::isNumeric(d_)==MSFalse)||
      (MSUtil::isNumeric(y_)==MSFalse))
   {
     _date=nullDate();
     changed();
     return MSError::BadDate;
   }
  MSDay day=atoi(d_);
  if (day==31) day--;
  MSJulian date=as30_360(atoi(m_),day,atoi(y_));
  if (date==nullDate()) return MSError::BadDate;
  _date=date;
  changed();
  return (MSError::MSSuccess);
}

MSError::ErrorStatus MSMBSDate::set(int m_,int d_,int y_)
{
  MSDay d=(d_==31?30:d_);
  _date=as30_360(m_,d,y_);
  changed();
  return (_date!=nullDate())?MSError::MSSuccess:MSError::BadDate;
}

const char *MSMBSDate::format(MSString *pString_) const
{ return format(pString_,defaultFormat()); }

const char *MSMBSDate::format(MSString& aString_) const
{ return format(&aString_,defaultFormat()); }

const char *MSMBSDate::format(MSString *pString_,const char *format_) const
{ return format(*pString_,format_); }

const char *MSMBSDate::format(MSString& aString_,const char *format_) const
{
  #ifdef MS_THREAD_SAFE_FUNCTIONS
  struct tm tms;
  #endif //MS_THREAD_SAFE_FUNCTIONS

  MSString aString(0,((format_!=0)?strlen(format_):0)+3);
  // __tm_zone, a member of struct tm for the sun architecture
  // must be initialized if the strftime format %Z is used. Can
  // get this value from localtime.
  time_t now=time(0);
  struct tm *pCalendarTime=MS_LOCALTIME(&now,&tms);
  MSMonth m; MSDay d; MSYear y;
  asMonthDayYear(m,d,y);
  pCalendarTime->tm_sec=0;
  pCalendarTime->tm_min=0;
  pCalendarTime->tm_hour=0;
  pCalendarTime->tm_mday=d;
  pCalendarTime->tm_mon=m-1;
  pCalendarTime->tm_year=y-1900;
  pCalendarTime->tm_wday=weekDay()==7?0:weekDay();
  pCalendarTime->tm_yday=dayOfYear()-1;
  pCalendarTime->tm_isdst=-1;
  int numberOfChars;
  while ((numberOfChars=strftime((char*)aString.string(),aString.length(),format_,pCalendarTime))==0)
   {
     aString=MSString(0,2*aString.length());
   }
  // remove the terminating null from strftime. strftime returns the number of characters
  // produced not including the terminating null character.
  aString=MSString(aString.string(),numberOfChars);
  aString_=aString.string();
  return aString_;
}

const char *MSMBSDate::format(MSString& aString_,const MSFormat& aFormat_) const
{ return (aFormat_.formatType()==MSFormat::Date)?format(aString_,aFormat_.dateFormat()):format(aString_); }

const char *MSMBSDate::format(MSString *pString_,const MSFormat& aFormat_) const
{ return format(*pString_,aFormat_); }

const char *MSMBSDate::format(MSString *pString_,MSDateFormat format_) const
{ return format(*pString_,format_); }

const char *MSMBSDate::format(MSString& aString_,MSDateFormat format_) const
{
  char buf[64];
  MSMonth m; MSDay d; MSYear y;
  // Don't want to call asMonthDayYear if the format is Strftime.
  // It gets called in the strftime format method.
  if (format_!=Strftime) asMonthDayYear(m,d,y);
  aString_.removeAll();
  buf[0]='\0';
  switch (format_)
   {
   case Slash:
     if (_date==nullDate()) aString_=NullString;
     else
      {
	// Format as 11/15/92
	switch (_locale)
	 {
	 case European:
	   sprintf(buf,"%02d/%02d/%02d",d,m,y%100);
	   break;
	 case American:
	   sprintf(buf,"%02d/%02d/%02d",m,d,y%100);
	   break;
	 case Japanese:
	   sprintf(buf,"%02d/%02d/%02d",y%100,m,d);
	   break;
	 default:
           MSMessageLog::warningMessage("MSMBSDate: invalid value of order flag\n");
           break;
	 }
	aString_+=buf;
      }
     break;
   case Slash4:
     if (_date==nullDate()) aString_=NullString;
     else
      {
	// Format as 11/15/1992
	switch (_locale)
	 {
	 case European:
	   sprintf(buf,"%02d/%02d/%04d",d,m,y);
	   break;
	 case American:
	   sprintf(buf,"%02d/%02d/%04d",m,d,y);
	   break;
	 case Japanese:
	   sprintf(buf,"%04d/%02d/%02d",y,m,d);
	   break;
	 default:
           MSMessageLog::warningMessage("MSMBSDate: invalid value of order flag\n");
           break;
	 }
	aString_+=buf;
      }
     break;
   case Terse:
     if (_date==nullDate()) aString_="  -   -  ";
     else
      {
	// Format as 11-Nov-92
	switch (_locale)
	 {
	 case European:
	   sprintf(buf,"%02d-%3.3s-%02d",d,_monthNames[m-1],y%100);
	   break;
	 case American:
	   sprintf(buf,"%02d-%3.3s-%02d",d,_monthNames[m-1],y%100);
	   break;
	 case Japanese:
	   sprintf(buf,"%02d-%3.3s-%02d",y%100,_monthNames[m-1],d);
	   break;
	 default:
           MSMessageLog::warningMessage("MSMBSDate: invalid value of order flag\n");
           break;
	 }
	aString_+=buf;
      }
     break;
   case Terse4:
     if (_date==nullDate()) aString_="  -   -  ";
     else
      {
	// Format as 11-Nov-1992
	switch (_locale)
	 {
	 case European:
	   sprintf(buf,"%02d-%3.3s-%04d",d,_monthNames[m-1],y);
	   break;
	 case American:
	   sprintf(buf,"%02d-%3.3s-%04d",d,_monthNames[m-1],y);
	   break;
	 case Japanese:
	   sprintf(buf,"%04d-%3.3s-%02d",y,_monthNames[m-1],d);
	   break;
	 default:
           MSMessageLog::warningMessage("MSMBSDate: invalid value of order flag\n");
           break;
         }
	aString_+=buf;
      }
     break;
   case Long:
     if (_date==nullDate()) aString_="      ,     ";
     else
      {
	// Format as November 15, 1992
	switch (_locale)
	 {
	 case European:
	   sprintf(buf,"%s %d, %4d",_monthNames[m-1],d,y);
	   break;
	 case American:
	   sprintf(buf,"%s %d, %4d",_monthNames[m-1],d,y);
	   break;
	 case Japanese:
	   sprintf(buf,"%4d %s %d",y,_monthNames[m-1],d);
	   break;
	 default:
           MSMessageLog::warningMessage("MSMBSDate: invalid value of order flag\n");
           break;
	 }
	aString_+=buf;
      }
     break;
   case MonthYear:
     if (_date==nullDate()) aString_="      ";
     else
      {
	// Format as Nov 92
	sprintf(buf,"%3.3s %02d",_monthNames[m-1],y%100);
	aString_+=buf;
      }
     break;
   case YearMonthDay:
     if (_date==nullDate()) aString_="    /  /  ";
     else
      {
	sprintf(buf,"%04d/%02d/%02d",y,m,d);
	aString_+=buf;
      }
     break;
   case Year2MonthDay:
     if (_date==nullDate()) aString_="  /  /  ";
     else
      {
	sprintf(buf,"%02d/%02d/%02d",y%100,m,d);
	aString_+=buf;
      }
     break;
   case EuropeanDot:
     if (_date==nullDate()) aString_="  .   .  ";
     else
      {
	// Format as dd.mm.yy
        sprintf(buf,"%02d.%02d.%02d",d,m,y%100);
	aString_+=buf;
      }
     break;
   case EuropeanDot4:
     if (_date==nullDate()) aString_="  .   .  ";
     else
      {
	// Format as dd.mm.yyyy
        sprintf(buf,"%02d.%02d.%04d",d,m,y);
	aString_+=buf;
      }
     break;
   case Database:
     if (_date==nullDate()) aString_=NullString;
     else
      {
	// Format as yyyymmdd
        sprintf(buf,"%04d%02d%02d",y,m,d);
	aString_+=buf;
      }
     break;
   case Julian:
     if (_date==nullDate()) aString_=NullString;
     else
      {
        sprintf(buf,"%d",(_date));
        aString_+=buf;
      }
     break;
   case Strftime:
     if (_date==nullDate()) aString_=NullString;
     else format(aString_,(const char *)strftimeDefaultFormat());
     break;
   default:
     MSMessageLog::warningMessage("MSMBSDate: invalid value of format\n");
     break;
   }
  return aString_.string();
}

void MSMBSDate::setToday(void)
{
  _date = currentDate();
  changed();
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// static member functions
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Convert Gregorian calendar date to the corresponding 30/360 day number.
// Gregorian calendar started on Sep. 14, 1752.
// Returns 0 if the date is invalid.
MSJulian MSMBSDate::as30_360(MSMonth month_,MSDay day_,MSYear year_)
{
  if (year_<=99) year_+=year_<=MS_PIVOT_YEAR?2000:1900;
  if (year_<JAN_01_1900) return MSJulian(0);
  if (dayWithinMonth(month_,day_,year_)==MSFalse) return MSJulian(0);
  return (month_-1)*30+day_+(year_-JAN_01_1900)*360;
}



// This algorithm is in accordance with Public Securities Association (PSA) standard specification
// for mortgage-backed and related securities (6/1/1990).
// It's a modified version of MSDate's algorithm from "Standard Security Calculation Methods"
// by Spence, Graudenz, and Lynch (SIA).
//
int MSMBSDate::dcb30_360(const MSMBSDate& d1_,const MSMBSDate& d2_)
{
  MSDate   dt1=d1_,dt2=d2_;
  MSMonth  Tm1,Tm2;
  MSDay	   Td1,Td2;
  MSYear   Ty1,Ty2;
  int	   m1,m2,d1,d2,y1,y2;
  int	   result;
  // order the dates
  long compareResult=::compare(d1_,d2_);
  
  if (compareResult>0) { dt1=d2_; dt2=d1_; }
  else if (compareResult==0) return 0;
  
  // format m, d, y and convert to signed ints
  dt1.asMonthDayYear(Tm1,Td1,Ty1);
  m1=Tm1; d1=Td1; y1=Ty1;
  dt2.asMonthDayYear(Tm2,Td2,Ty2);
  m2=Tm2; d2=Td2; y2=Ty2;

  // The algorithm is...
  //
  if (m1==2)	// if February
    {
      // modification from MSDate's algorithm - normalize the last day of Februrary
      // (based on request for change from Dan Evison (2/7/97)
      //
      MSDay lastFebDay = (MSDate::leapYear(Ty1)==MSTrue) ? _daysInMonth[m1]+1 : _daysInMonth[m1];
      if (d1==lastFebDay)
	{
	  d1=30;
	}
    }

  if (d1==31) d1=30;
  if (d2==31&&(d1==30||d1==31)) d2=30;
  result=(y2-y1)*360+(m2-m1)*30+(d2-d1);
  return result;
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Member Functions
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// return represenation of this date as a julian number
MSJulian MSMBSDate::date(void) const
{
  MSMonth m; MSDay d; MSYear y;
  asMonthDayYear(m,d,y);
  return asJulianNumber(m,d,y);
}

MSBoolean MSMBSDate::between(const MSDate& d1_,const MSDate& d2_) const
{ return MSBoolean(_date>=d1_.asInternal()&&_date<=d2_.asInternal()); }

MSDay MSMBSDate::dayOfYear(void) const
{ return _date-as30_360(12,31,year()-1); }

// Returns the day of the month of this MSMBSDate.
MSDay MSMBSDate::dayOfMonth(void) const
{
  MSMonth m; MSDay d; MSYear y;
  asMonthDayYear(m,d,y);
  return d;
}

MSDay MSMBSDate::daysInMonth(void) const
{ return 30; }

MSDay MSMBSDate::daysInYear(void) const
{ return 360; }

unsigned MSMBSDate::weekOfYear(void) const
{ return (dayOfYear()/7)+1; }

// Return the number of the first day of a given month
// Return 0 if "month" is outside of the range 1 through 12, inclusive.
MSDay MSMBSDate::firstDayOfMonth(void) const
{ return firstDayOfMonth(month()); }

MSDay MSMBSDate::firstDayOfMonth(MSMonth month_) const
{
  if (assertIndexOfMonth(month_)==MSFalse) return 0;
  return (month_-1)*30+1;
}

void MSMBSDate::setFirstDayOfMonth(void)
{
  MSMonth m; MSDay d; MSYear y;
  asMonthDayYear(m,d,y);
  _date-=(d-1);
  changed();
}

// Return the number of the last day of a given month.
// Return 0 if "month" is outside of the range 1 through 12, inclusive.
MSDay MSMBSDate::lastDayOfMonth(void) const
{ return lastDayOfMonth(month()); }

MSDay MSMBSDate::lastDayOfMonth(MSMonth month_) const
{
  MSDay answer=firstDayOfMonth(month_+1);
  return answer?answer-1:0;
}

void MSMBSDate::setLastDayOfMonth(void)
{
  MSMonth m; MSDay d; MSYear y;
  asMonthDayYear(m,d,y);
  _date+=30-d;
  changed();
}

MSBoolean MSMBSDate::isSet() const
{ return _date==nullDate()?MSFalse:MSTrue; }

MSBoolean MSMBSDate::isValid(void) const
{ return MSBoolean(_date>0); }

MSBoolean MSMBSDate::isLeapYear(void) const
{ return leapYear(year()); }

MSBoolean MSMBSDate::leap(void) const
{ return leapYear(year()); }

const char* MSMBSDate::nameOfDay(void) const
{ return dayName(weekDay());}

const char* MSMBSDate::nameOfMonth(void) const
{ return monthName(month());}

MSDate MSMBSDate::min(const MSDate& d_) const
{
  if (d_.asInternal()<_date) return d_;
  else return *this;
}

MSDate MSMBSDate::max(const MSDate& d_) const
{
  if (d_.asInternal()>_date) return d_;
  else return *this;
}

// Returns the month of this MSMBSDate.
MSMonth MSMBSDate::month() const
{
  MSMonth m; MSDay d; MSYear y;
  asMonthDayYear(m,d,y);
  return m;
}

MSDate MSMBSDate::previous(const char *dayName) const
{ return previous(dayOfWeek(dayName)); }

MSDate MSMBSDate::previous(MSDay desiredDayOfWeek_) const
{
  // Renumber the desired and current day of week to start at 0 (Monday)
  // and end at 6 (Sunday).
  desiredDayOfWeek_--;
  MSDay thisDayOfWeek=weekDay()-1;
  MSJulian j=_date;

  // Have to determine how many days difference from current day back to
  // desired, if any.  Special calculation under the 'if' statement to
  // effect the wraparound counting from Monday (0) back to Sunday (6).
  if (desiredDayOfWeek_>thisDayOfWeek) thisDayOfWeek+=7-desiredDayOfWeek_;
  else thisDayOfWeek-=desiredDayOfWeek_;
  j-=thisDayOfWeek; // Adjust j to set it at desired day of week.
  return MSMBSDate(j);
}

MSDay MSMBSDate::weekDay(void) const
{
  MSDate d(*this);
  return d.weekDay();
}

// Returns the year of this MSMBSDate.
MSYear MSMBSDate::year(void) const
{
  MSMonth m; MSDay d; MSYear y;
  asMonthDayYear(m,d,y);
  return y;
}

// Convert a 30/360 day number to its corresponding Gregorian calendar date.
void MSMBSDate::asMonthDayYear(MSMonth& month_,MSDay& day_,MSYear& year_) const
{
  month_=day_=year_=0;
  if (_date!=nullDate())
   {
     div_t year=div(_date,360);
     div_t month=div(year.rem,30);
     year_=year.quot+JAN_01_1900;
     month_=month.quot+1;
     day_=month.rem;
     if (month.rem==0)
      {
	day_=30,month_--;
	if (month.quot==0) month_=12,year_--;
      }
   }
}

void MSMBSDate::nextWeekday(void)
{
  switch (weekDay())
   {
   case Saturday: _date+=2; changed(); break;
   case Sunday:   _date++;  changed(); break;
   default:                            break;
   }
}

void MSMBSDate::prevWeekday(void)
{
  switch (weekDay())
   {
   case Saturday: _date--;  changed(); break;
   case Sunday:   _date-=2; changed(); break;
   default:                            break;
   }
}

MSBoolean MSMBSDate::isWeekday(void) const
{
  MSBoolean rc;
  switch (weekDay())
   {
   case Monday: case Tuesday: case Wednesday: case Thursday: case Friday: rc=MSTrue; break;
   case Saturday: case Sunday: rc=MSFalse; break;
   default: rc=MSFalse; break;
   }
  return rc;
}

MSBoolean MSMBSDate::isWeekend(void) const
{
  MSBoolean rc;
  switch (weekDay())
   {
   case Monday: case Tuesday: case Wednesday: case Thursday: case Friday: rc=MSFalse; break;
   case Saturday: case Sunday: rc=MSTrue; break;
   default: rc=MSFalse; break;
   }
  return rc;
}

// This is the tricky part.  Adding a term to a date.
// NormalizeAndSet is an internal function to
// set the date to the given month, day, and year, forcing it to
// be the last day of the month if lastOfMonth is MSTrue.  Day must be
// 1-31, and lastOfMonth must be true if day is 31.  Month may be negative.
void MSMBSDate::normalizeAndSet(int m_, int d_,int y_,int lastOfMonth_)
{
  // normalize month
  if (m_>12)
   {
     m_-=1;
     y_+=m_/12;
     m_%=12;
     m_+=1;
   }
  else if (m_<1)
   {
     // must be careful because (-14/-12) can be -1 or -2,
     // and (-14%-12) can be -2 or 10.  Let's keep everything positive.
     int yearsBack=((-m_)/12)+1;
     m_+=yearsBack*12;
     y_-=yearsBack;
   }

  // Now fix up the day of the month.
  // If the start date was the end of the month, the end date is the
  // end of the month.  Otherwise, if the start date was not the end
  // of the month, but its day of month is illegal in the end month
  // (which can only happen if the end month is Feb.), set the end date
  // to the end of the month
  if (lastOfMonth_) d_=30;
  _date=as30_360(m_,d_,y_);
  changed();
}

MSMBSDate& MSMBSDate::operator+=(const MSTerm& t_)
{
  if ((t_.years()!=0)||(t_.months()!=0))
   {
     int m; int d; int y;
     MSMonth mT; MSDay dT; MSYear yT;
     asMonthDayYear(mT,dT,yT);
     m=int(mT); d=int(dT); y=int(yT);
     int wasLastOfMonth=(((m==2)&&leapYear(y))?d==29:d==_daysInMonth[m]);
     y+=t_.years();
     m+=t_.months();
     normalizeAndSet(m,d,y,wasLastOfMonth);
   }
  // now add in the number of days in term
  _date+=t_.days();
  changed();
  return *this;
}

MSMBSDate& MSMBSDate::operator-=(const MSTerm& t_)
{
  if ((t_.years() != 0) || (t_.months() != 0))
   {
     int m; int d; int y;
     MSMonth mT; MSDay dT; MSYear yT;
     asMonthDayYear(mT,dT,yT);
     m=int(mT); d=int(dT); y=int(yT);
     int wasLastOfMonth=(((m==2)&&(leapYear(y)==MSTrue))?d==29:d==_daysInMonth[m]);
     y-=t_.years();
     m-=t_.months();
     normalizeAndSet(m,d,y,wasLastOfMonth);
   }
  // now subtract out the number of days in term
  _date-=t_.days();
  changed();
  return *this;
}

// Special private constructors. They allow us to take advantage of the
// return value optimization (RV0). See section 12.1c of the ARM.
MSMBSDate::MSMBSDate(const MSMBSDate& date_,const MSTerm& term_,MSMBSDate::Operator operator_)
{
  _date=date_._date;
  switch(operator_)
   {
   case Plus:   *this+=term_; break;
   case Minus:  *this-=term_; break;
   default:                   break;
   }
}

MSMBSDate::MSMBSDate(const MSMBSDate& date_,const MSNormalizedYears& ny_,MSMBSDate::Operator operator_)
{
  _date=date_._date;
  switch(operator_)
   {
   case Plus:   _date+=ny_.days(); break;
   case Minus:  _date-=ny_.days(); break;
   default:                        break;
   }
}

int MSMBSDate::operator-(const MSMBSDate& d_) const
{ return int(long(_date)-long(d_._date)); }
MSMBSDate MSMBSDate::operator+(const MSTerm& term_) const
{ return MSMBSDate(*this,term_,MSMBSDate::Plus); }
MSMBSDate MSMBSDate::operator+(const MSNormalizedYears& ny_) const
{ return MSMBSDate(*this,ny_,MSMBSDate::Plus); }
MSMBSDate MSMBSDate::operator-(const MSTerm& term_) const
{ return MSMBSDate(*this,term_,MSMBSDate::Minus); }
MSMBSDate MSMBSDate::operator-(const MSNormalizedYears& ny_) const
{ return MSMBSDate(*this,ny_,MSMBSDate::Minus); }
MSMBSDate MSMBSDate::operator+(int d_) const
{ return MSMBSDate(_date+d_); }
MSMBSDate MSMBSDate::operator-(int d_) const
{ return MSMBSDate(_date-d_); }
MSMBSDate& MSMBSDate::operator+=(int d_)
{ _date+=d_; changed(); return *this; }
MSMBSDate& MSMBSDate::operator-=(int d_)
{ _date-=d_; changed(); return *this; }
MSMBSDate& MSMBSDate::operator+=(const MSNormalizedYears& ny_)
{ *this+=ny_.days(); return *this; }
MSMBSDate& MSMBSDate::operator-=(const MSNormalizedYears& ny_)
{ *this-=ny_.days(); return *this; }

MSMBSDate operator+(int i_,const MSMBSDate& d_)
{ return d_+i_; }
MSMBSDate operator+(const MSTerm& t_,const MSMBSDate& d_)
{ return d_+t_; }
MSMBSDate operator+(const MSNormalizedYears& y_,const MSMBSDate& d_)
{ return d_+y_.days(); }

ostream& operator<<(ostream& aStream_,const MSMBSDate& aDate_)
{ MSString s; return aStream_<<aDate_.format(s)<<flush; }

istream& operator>>(istream& aStream_,MSMBSDate& aDate_)
{
  MSString aString;
  aStream_>>aString;
  aDate_.set(aString);
  return aStream_;
}


MSJulian MSMBSDate::currentDate(void)
{
  static MSBoolean firstTime=MSTrue;
  static MSJulian override=0;
  static MSBoolean useOverride=MSFalse;

  if (firstTime==MSTrue)
    {
      firstTime=MSFalse;
      char *cp=getenv("TB_DATE_OVERRIDE");
      if (cp)
	{
	  MSMBSDate d;
	  if (d.set(cp)==MSError::MSSuccess)
	    {
	      override=d._date;
	      useOverride=MSTrue;
	    }
	  else
	    {
	      MSMessageLog::errorMessage("MSMBSDate: TB_DATE_OVERRIDE contains an invalid date\n");
	      MSMessageLog::errorMessage("MSMBSDate: ignoring attempt to override\n");
	      useOverride=MSFalse;
	    }
	}
      else
	{
	  useOverride=MSFalse;
	}
    }
  
  if (useOverride==MSTrue)
    {
      return override;
    }
  else
    {
      #ifdef MS_THREAD_SAFE_FUNCTIONS
      struct tm tms;
      #endif //MS_THREAD_SAFE_FUNCTIONS

      time_t clk=time(0);
      struct tm *now=MS_LOCALTIME(&clk,&tms);
      return as30_360(now->tm_mon+1,now->tm_mday,now->tm_year+1900);
    }
}

// converts this MSMBSDate to calendar time - time in seconds
// since 00:00:00, January 1, 1970, with a correction for the time
// zone.  The time is set to midnight of this date.
time_t MSMBSDate::asCalendarTime(MSTime::MSTimeZone zone_) const
{
  time_t rc=-1;
  if (asInternal()!=nullDate())
    {
      struct tm aTimeStruct={0};
      MSMonth m; MSDay d; MSYear y;
      asMonthDayYear(m,d,y);
      aTimeStruct.tm_mday=d;
      aTimeStruct.tm_mon=m-1;
      aTimeStruct.tm_year=y;
      aTimeStruct.tm_isdst=-1;
      // tm_wday and tm_yday fields are ignored
      rc = MSTime::greenwichMeanTime(&aTimeStruct, zone_);
   }

  return rc;
}
