///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDate.H>
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
const unsigned long SEP_14_1752=2361222; // first day of Gregorian calendar
static const char *NullString="";

// months 1-12, 0 is padding
const unsigned char MSDate::_daysInMonth[13]={0,31,28,31,30,31,30,31,31,30,31,30,31};
const MSDay MSDate::_firstDayOfEachMonth[13]={0,1,32,60,91,121,152,182,213,244,274,305,335};
const char *MSDate::_monthNames[12]={"January","February","March","April","May","June","July",
                            "August","September","October","November","December"};
const char *MSDate::_ucMonthNames[12]={"JANUARY","FEBRUARY","MARCH","APRIL","MAY","JUNE","JULY",
                              "AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER"};
const char *MSDate::_weekDayNames[7]={"Monday","Tuesday","Wednesday","Thursday","Friday",
                               "Saturday","Sunday"};
const char *MSDate::_ucWeekDayNames[7]={"MONDAY","TUESDAY","WEDNESDAY","THURSDAY","FRIDAY",
                                      "SATURDAY","SUNDAY" };

MSDate::MSDateFormat MSDate::_defaultFormat=MSDate::Slash4;
MSString MSDate::_strftimeDefaultFormat="%D";
MSJulian MSDate::_nullDate=0;
MSBoolean MSDate::_defaultConstructToToday=MSFalse;

#ifdef MS_NO_INLINES
#include <MSTypes/MSDateInlines.C>
#endif

MSDate::MSDate(const MSTime &time_,MSTime::MSTimeZone zone_)
{
  MSString s;
  (void)time_.format(&s,MSTime::MonthDayYear,zone_);
  set(&s);
}

#if defined(MS_NEED_STRPTIME_DECLARATION)   
extern "C" char *strptime(const char*, const char*, struct tm*);
#endif
 
MSDate::MSDate(const MSString& aString_,const char* strptimeFormat_)
{
  struct tm aTimeStruct={0};
#if defined(MS_NO_STRPTIME)
  MSMessageLog::errorMessage("MSDate: strptime() not supported for this platform.\n");
  _date=0;
#else
  char *pString=strptime((char *)aString_.string(),(char *)strptimeFormat_,&aTimeStruct);
  if (pString==0) _date=0;
  else _date=asJulianNumber(aTimeStruct.tm_mon+1,aTimeStruct.tm_mday,aTimeStruct.tm_year+1900);
#endif
}

MSDate::~MSDate(void) {}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// MSModel virtual methods
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MSString MSDate::asString(void) const
{ MSString aString; return MSString(format(aString,MSDate::defaultFormat())); }

MSString MSDate::asDebugInfo(void) const
{
  MSString result("MSDate(@");
  result+=MSString((unsigned long)this).d2x().lowerCase();
  result+=",_date=";
  result+=MSString((unsigned long)date());
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
  
const MSSymbol& MSDate::symbol(void)   
{
  static MSSymbol sym("MSDate");
  return sym;
}

MSString MSDate::className(void) const
{ return MSString("MSDate"); }

const MSSymbol& MSDate::type(void) const
{ return symbol(); }

MSModel *MSDate::clone(void) const
{ return new MSDate(*this); }

MSModel *MSDate::create(void) const
{ return new MSDate(); }

void MSDate::assign(const MSModel& aModel_)
{ *this=(MSDate&)aModel_; }

long MSDate::compare(const MSModel& aModel_) const
{ return ::compare(*this,(MSDate&)aModel_); }

MSString MSDate::asMSF(void) const
{ 
  MSString buf;
  if (isSet()==MSTrue)  (void) format(&buf,MSDate::Database);
  return buf;
}

MSError::ErrorStatus MSDate::setFromMSF(const char *pString_)
{ 
  int code;
  if (pString_!=0) code=set(pString_);
  else code=MSError::BadMSFString;
  return (MSError::ErrorStatus)code;
}

void MSDate::unset(void)
{
  if (isSet()==MSTrue)
   {
     _date=nullDate();
     changed();
   }
}

MSError::ErrorStatus MSDate::set(const MSString *pString_)
{ return set(pString_->string()); }
MSError::ErrorStatus MSDate::set(const MSString& aString_)
{ return set(aString_.string()); }
MSError::ErrorStatus MSDate::set(const char *pString_)
{
  MSError::ErrorStatus rc=MSError::MSSuccess;
  char *f1,*f2,*f3;
  char buf[80];
  char *cp;
  static char separators[] = { '/', '.', '-' };	// possible separators between day, month, and year

  if (pString_==0) rc=MSError::BadDate;
  else
   {
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
	strncpy(buf,pString_,sizeof(buf));
	f1=buf;		// point to the start of string   
	//
	// see if different parts of the date are separated by one of the items in separators[] array
	//
	for (unsigned int i=0; i<sizeof(separators); ++i)
	  {
	    // Look for the separator
	    cp=strchr(buf,separators[i]);
	    if (cp) 
	      {
		*cp='\0'; 	// nul-terminate first field
		cp++;		// point to the start of second field
		f2=cp;
		cp=strchr(f2,separators[i]);
		if (cp)
		  {
		    *cp='\0';	// null-terminate second field
		    cp++;        // point to the start of third field
		    f3=cp;
		    switch (_locale)
		      {
			// set expects month, day, year
		      case European: return set(f2,f1,f3);
		      case American: return set(f1,f2,f3);
		      case Japanese: return set(f2,f3,f1);
		      default: MSMessageLog::warningMessage("MSDate: invalid value of order flag\n");
		      }
		  }
		else
		  {
		    rc=MSError::BadDate;
		  }
	      }
	  }

	// no slashes, no period, no dashes, no alpha - must be internal fmt (digits)
	if (MSUtil::isNumeric(buf)==MSFalse)
	  {
	    if (MSUtil::isSpace(buf)==MSTrue) _date=nullDate(); // May be full of spaces
	    else rc=MSError::BadDate;
	  }
	else 
	  {
	    // format of yyyymmdd
	    if (strlen(pString_)>7)
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
  if (rc!=MSError::MSSuccess) _date=nullDate(); 
  return changed(),rc;
}

MSError::ErrorStatus MSDate::set(const char *m_,const char *d_,const char *y_)
{
  if ((MSUtil::isNumeric(m_)==MSFalse)||
      (MSUtil::isNumeric(d_)==MSFalse)||
      (MSUtil::isNumeric(y_)==MSFalse))
   {
     _date=nullDate();
     return changed(),MSError::BadDate;
   }
  MSJulian date=asJulianNumber(atoi(m_),atoi(d_),atoi(y_)); 
  if (date==nullDate()||date<SEP_14_1752)
   {
     _date=nullDate();
     return changed(),MSError::BadDate;
   }
  _date=date;
  return changed(),MSError::MSSuccess;
}

MSError::ErrorStatus MSDate::set(int m_,int d_,int y_)
{
  _date=asJulianNumber(m_,d_,y_);
  changed();
  return (_date!=nullDate())?MSError::MSSuccess:MSError::BadDate;
}

const char *MSDate::format(MSString *pString_) const
{ return format(pString_,defaultFormat()); }

const char *MSDate::format(MSString& aString_) const
{ return format(&aString_,defaultFormat()); }

const char *MSDate::format(MSString *pString_,const char *format_) const
{ return format(*pString_,format_); }

const char *MSDate::format(MSString& aString_,const char *format_) const
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

const char *MSDate::format(MSString& aString_,const MSFormat& aFormat_) const
{ return (aFormat_.formatType()==MSFormat::Date)?format(aString_,aFormat_.dateFormat()):format(aString_); }

const char *MSDate::format(MSString *pString_,const MSFormat& aFormat_) const
{ return format(*pString_,aFormat_); }

const char *MSDate::format(MSString *pString_,MSDateFormat format_) const
{ return format(*pString_,format_); }

const char *MSDate::format(MSString& aString_,MSDateFormat format_) const
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
           MSMessageLog::warningMessage("MSDate: invalid value of order flag\n");
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
           MSMessageLog::warningMessage("MSDate: invalid value of order flag\n");
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
           MSMessageLog::warningMessage("MSDate: invalid value of order flag\n");
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
           MSMessageLog::warningMessage("MSDate: invalid value of order flag\n");
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
           MSMessageLog::warningMessage("MSDate: invalid value of order flag\n");
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
     if (_date==nullDate()) aString_="  .  .  ";
     else
      {
	// Format as dd.mm.yy
        sprintf(buf,"%02d.%02d.%02d",d,m,y%100);
	aString_+=buf;
      }
     break;
   case EuropeanDot4:
     if (_date==nullDate()) aString_="  .  .  ";
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
        sprintf(buf,"%d",(_date-SEP_14_1752));
        aString_+=buf;
      }
     break;
   case Strftime:
     if (_date==nullDate()) aString_=NullString;
     else format(aString_,(const char *)strftimeDefaultFormat());
     break;
   default:
     MSMessageLog::warningMessage("MSDate: invalid value of format\n");
     break;
   }
  return aString_.string();
}

void MSDate::setToday(void)
{
  _date = currentDate();
  changed();
}

// static member functions
// Returns a string name for the weekday number.
// Monday == 1, ... , Sunday == 7
// Return 0 for weekday number out of range
const char *MSDate::dayName(MSDay d_)
{ return assertWeekDayNumber(d_)==MSTrue?_weekDayNames[d_-1]:0; }

// Return the number, 1-7, of the day of the week named nameOfDay.
// Return 0 if name doesn't match.
MSDay MSDate::dayOfWeek(const char *d_)
{ return MSDay(findMatch(d_,_ucWeekDayNames,7)+1); }

// Is a day (1-31) within a given month?
MSBoolean MSDate::dayWithinMonth(MSMonth month_,MSDay day_,MSYear year_)
{
  if (day_<=0||assertIndexOfMonth(month_)==MSFalse) return MSFalse;
  unsigned d=_daysInMonth[month_];
  if (month_==2&&leapYear(year_)==MSTrue) d++;
  return MSBoolean(day_<=d);
}

MSDay MSDate::daysInMonth(MSMonth month_,MSYear year_) 
{ 
  if (assertIndexOfMonth(month_)==MSFalse) return 0;
  return (month_==2&&leapYear(year_)==MSTrue)?_daysInMonth[month_]+1:_daysInMonth[month_];
}

MSDay MSDate::daysInYear(MSYear year_) 
{ return leapYear(year_)==MSTrue?366:365; }

// Returns the number, 1-12, of the month named nameOfMonth.
// Return 0 for no match.
MSMonth MSDate::indexOfMonth(const char *c_)
{ return MSMonth(findMatch(c_,_ucMonthNames,12)+1); }

// Convert Gregorian calendar date to the corresponding Julian day
// number.  Algorithm 199 from Communications of the ACM, Volume 6, No.
// 8, (Aug. 1963), p. 444.  Gregorian calendar started on Sep. 14, 1752.
// This function not valid before that.
// Returns 0 if the date is invalid.
MSJulian MSDate::asJulianNumber(MSMonth month_,MSDay day_,MSYear year_)
{
  unsigned long c,ya;
  if (year_<=99) year_+=year_<=MS_PIVOT_YEAR?2000:1900;
  if (dayWithinMonth(month_,day_,year_)==MSFalse) return MSJulian(0);
  if (month_>2) month_-=3;
  else { month_+=9; year_--; } 
  c=year_/100;
  ya=year_-100*c;
  return ((146097*c)>>2)+((1461*ya)>>2)+(153*month_+2)/5+day_+1721119;
} 

// Algorithm from K & R, "The C Programming Language", 1st ed.
MSBoolean MSDate::leapYear(MSYear y_) { return MSBoolean((y_&3)==0&&y_%100!=0||y_%400==0); }

// Returns a string name for the month number.
// Return 0 if invalid month number.
const char *MSDate::monthName(MSMonth m_)
{ return assertIndexOfMonth(m_)==MSTrue?_monthNames[m_-1]:0; }

// Algorithm from "Standard Security Calculation Methods
// by Spence, Graudenz, and Lynch (SIA) Forward is dated April 19, 1973
int MSDate::dcb30_360(const MSDate& d1_,const MSDate& d2_)
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
  if (d1==31) d1=30;
  if (d2==31&&(d1==30||d1==31)) d2=30;
  result=(y2-y1)*360+(m2-m1)*30+(d2-d1);
  return result;
}

// Return index of case-insensitive match; -1 if no match.
int MSDate::findMatch(const char *str_,const char **candidates_,int icand_)
{
  unsigned len=strlen(str_);
#if !defined(MS_HAS_STRNICMP)
  register unsigned N=len;
  register char *up;
  char *p=up=new char[len+1];
  while(N--) {*up++=islower(*str_)?toupper(*str_):*str_;str_++;}
  *up='\0';
  while(icand_--) 
   { if (strncmp(p,candidates_[icand_],len)==0) break; }
  delete [] p;
#else
  while(icand_--)
   { if (strnicmp(str_,candidates_[icand_],len)==0) break; }
#endif
  return icand_;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Member Functions
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MSJulian MSDate::date(void) const 
{ return _date; }

MSBoolean MSDate::between(const MSDate& d1_,const MSDate& d2_) const
{ return MSBoolean(_date>=d1_._date&&_date<=d2_._date); }

MSDay MSDate::dayOfYear(void) const
{ return _date-asJulianNumber(12,31,year()-1); }

// Returns the day of the month of this MSDate.
MSDay MSDate::dayOfMonth(void) const
{
  MSMonth m; MSDay d; MSYear y;
  asMonthDayYear(m,d,y);
  return d;
}

MSDay MSDate::daysInMonth(void) const 
{
  MSMonth m; MSDay d; MSYear y;
  asMonthDayYear(m,d,y);
  return daysInMonth(m,y);
}

MSDay MSDate::daysInYear(void) const
{
  MSMonth m; MSDay d; MSYear y;
  asMonthDayYear(m,d,y);
  return leapYear(y)==MSTrue?366:365;
}

unsigned MSDate::weekOfYear(void) const
{ return (dayOfYear()/7)+1; }

// Return the number of the first day of a given month
// Return 0 if "month" is outside of the range 1 through 12, inclusive.
MSDay MSDate::firstDayOfMonth(void) const 
{ return firstDayOfMonth(month()); }

MSDay MSDate::firstDayOfMonth(MSMonth month) const
{
  if (assertIndexOfMonth(month)==MSFalse) return 0;
  unsigned firstDay=_firstDayOfEachMonth[month];
  if (month>2&&leap()==MSTrue)firstDay++;
  return firstDay;
}
void MSDate::setFirstDayOfMonth(void)
{
  MSMonth m; MSDay d; MSYear y;
  asMonthDayYear(m,d,y);
  _date-=(d-1);
  changed();
}

// Return the number of the last day of a given month by
// looking up the first day of the next month and subtracting
// 1 from it.
// Return 0 if "month" is outside of the range 1 through 12, inclusive.
MSDay MSDate::lastDayOfMonth(void) const 
{ return lastDayOfMonth(month()); }

MSDay MSDate::lastDayOfMonth(MSMonth month_) const
{
  MSDay answer;
  if (month_==12) return leap()==MSTrue?MSDay(366):MSDay(365);
  else answer=firstDayOfMonth(month_+1);
  return answer?answer-1:0;
}

void MSDate::setLastDayOfMonth(void)
{
  MSMonth m; MSDay d; MSYear y;
  asMonthDayYear(m,d,y);
  _date+=((leapYear(y)==MSTrue)&&(m==2))?29-d:_daysInMonth[m]-d;
  changed();
}

MSBoolean MSDate::isValid(void) const 
{ return MSBoolean(_date>0); }
MSBoolean MSDate::isLeapYear(void) const 
{ return leapYear(year()); } 
MSBoolean MSDate::leap(void) const 
{ return leapYear(year()); } 
const char* MSDate::nameOfDay(void) const 
{ return dayName(weekDay());}
const char* MSDate::nameOfMonth(void) const 
{ return monthName(month());}

MSDate MSDate::min(const MSDate& d_) const 
{ return d_._date<_date?d_:*this; }
MSDate MSDate::max(const MSDate& d_) const
{ return d_._date>_date?d_:*this; }

// Returns the month of this MSDate.
MSMonth MSDate::month() const
{
  MSMonth m; MSDay d; MSYear y;
  asMonthDayYear(m,d,y);
  return m;
}

MSDate MSDate::previous(const char *dayName) const
{ return previous(dayOfWeek(dayName)); }

MSDate MSDate::previous(MSDay desiredDayOfWeek_) const
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
  return MSDate(j);
}

MSDay MSDate::weekDay(void) const { return ((((_date+1)%7)+6)%7)+1; }

// Returns the year of this MSDate.
MSYear MSDate::year(void) const
{
  MSMonth m; MSDay d; MSYear y;
  asMonthDayYear(m,d,y);
  return y;
}

// Convert a Julian day number to its corresponding Gregorian calendar
// date.  Algorithm 199 from Communications of the ACM, Volume 6, No. 8,
// (Aug. 1963), p. 444.  Gregorian calendar started on Sep. 14, 1752.
// This function not valid before that.  
void MSDate::asMonthDayYear(MSMonth& month_,MSDay& day_,MSYear& year_) const
{
  unsigned long d;
  MSJulian j=_date-1721119;
  year_=(MSYear) (((j<<2)-1)/146097);
  j=(j<<2)-1-146097*year_;
  d=(j>>2);
  j=((d<<2)+3)/1461;
  d=(d<<2)+3-1461*j;
  d=(d+4)>>2;
  month_=(MSMonth)(5*d-3)/153;
  d=5*d-3-153*month_;
  day_=(MSDay)((d+5)/5);
  year_=(MSYear)(100*year_+j);
  if (month_<10) month_+=3;
  else { month_-=9; year_++; } 
} 

void MSDate::nextWeekday(void)
{
  switch (weekDay())
   {
   case Saturday: _date+=2; changed(); break;     
   case Sunday:   _date++;  changed(); break;
   default:                            break;
   }
}

void MSDate::prevWeekday(void)
{
  switch (weekDay()) 
   {
   case Saturday: _date--;  changed(); break;
   case Sunday:   _date-=2; changed(); break;
   default:                            break;
   }
}

MSBoolean MSDate::isWeekday(void) const
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

MSBoolean MSDate::isWeekend(void) const
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
void MSDate::normalizeAndSet(int m_, int d_,int y_,int lastOfMonth_)
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
  if (lastOfMonth_) d_=(m_==2)&&(leapYear(y_)==MSTrue)?29:_daysInMonth[m_];
  else if ((d_>28)&&(m_==2)) d_=leapYear(y_)==MSTrue?29:28;
  _date=asJulianNumber(m_,d_,y_);
  changed();
}

MSDate& MSDate::operator+=(const MSTerm& t_)
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

MSDate& MSDate::operator-=(const MSTerm& t_)
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
MSDate::MSDate(const MSDate& date_,const MSTerm& term_,MSDate::Operator operator_)
{
  _date=date_._date;
  switch(operator_)
   {
   case Plus:   *this+=term_; break;
   case Minus:  *this-=term_; break;
   default:                   break;
   }
}

MSDate::MSDate(const MSDate& date_,const MSNormalizedYears& ny_,MSDate::Operator operator_)
{
  _date=date_._date;
  switch(operator_)
   {
   case Plus:   _date+=ny_.days(); break;
   case Minus:  _date-=ny_.days(); break;
   default:                        break;
   }
}

int MSDate::operator-(const MSDate& d_) const 
{ return int(long(_date)-long(d_._date)); }
MSDate MSDate::operator+(const MSTerm& term_) const
{ return MSDate(*this,term_,MSDate::Plus); }
MSDate MSDate::operator+(const MSNormalizedYears& ny_) const
{ return MSDate(*this,ny_,MSDate::Plus); }
MSDate MSDate::operator-(const MSTerm& term_) const
{ return MSDate(*this,term_,MSDate::Minus); }
MSDate MSDate::operator-(const MSNormalizedYears& ny_) const
{ return MSDate(*this,ny_,MSDate::Minus); }
MSDate MSDate::operator+(int d_) const
{ return MSDate(_date+d_); }
MSDate MSDate::operator-(int d_) const
{ return MSDate(_date-d_); }
MSDate& MSDate::operator+=(int d_)
{ _date+=d_; changed(); return *this; }
MSDate& MSDate::operator-=(int d_)
{ _date-=d_; changed(); return *this; }
MSDate& MSDate::operator+=(const MSNormalizedYears& ny_)
{ *this+=ny_.days(); return *this; }
MSDate& MSDate::operator-=(const MSNormalizedYears& ny_)
{ *this-=ny_.days(); return *this; }

MSDate operator+(int i_,const MSDate& d_)
{ return d_+i_; }
MSDate operator+(const MSTerm& t_,const MSDate& d_)
{ return d_+t_; }
MSDate operator+(const MSNormalizedYears& y_,const MSDate& d_)
{ return d_+y_.days(); }

ostream& operator<<(ostream& aStream_,const MSDate& aDate_)
{ MSString s; return aStream_<<aDate_.format(s)<<flush; }

istream& operator>>(istream& aStream_,MSDate& aDate_)
{
  MSString aString;
  aStream_>>aString;
  aDate_.set(aString);
  return aStream_;
}

MSBoolean MSDate::isSet() const { return _date==nullDate()?MSFalse:MSTrue; }

MSDate::MSDateLocale MSDate::initLocale(void)
{
  char *cp=getenv("LC_TIME");
  if (cp) 
    {
      if (strcmp(cp,"japanese")==0) return MSDate::Japanese;
      else if (strcmp(cp,"european")==0) return MSDate::European;
      else return MSDate::American;
    }
  else
    {
      return MSDate::American;
    }
}

MSDate::MSDateLocale MSDate::_locale=MSDate::initLocale();
MSJulian MSDate::_override=0;
int MSDate::_useOverride=MSFalse;
int MSDate::_firstTime=MSTrue;

MSJulian MSDate::currentDate(void)
{
  if (_firstTime) 
    {
      _firstTime=MSFalse;
      char *cp=getenv("TB_DATE_OVERRIDE");
      if (cp) 
	{
	  MSDate d;
	  if (d.set(cp)==MSError::MSSuccess)
	    {
	      _override=d._date;
	      _useOverride=MSTrue;
	    }
	  else
	    {
	      MSMessageLog::errorMessage("MSDate: TB_DATE_OVERRIDE contains an invalid date\n");
	      MSMessageLog::errorMessage("MSDate: ignoring attempt to override\n");
	      _useOverride=MSFalse;
	    }
	}
      else
	{
	  _useOverride=MSFalse;
	}
    }
  
  if (_useOverride)
    {
      return _override;
    }
  else
    {
      #ifdef MS_THREAD_SAFE_FUNCTIONS
      struct tm tms;
      #endif //MS_THREAD_SAFE_FUNCTIONS

      time_t clk=time(0);
      struct tm *now=MS_LOCALTIME(&clk,&tms);
      return asJulianNumber(now->tm_mon+1,now->tm_mday,now->tm_year+1900);
    }
}

// converts this MSDate to GMT calendar time - time in seconds
// since 00:00:00 January 1, 1970, with a correction for the time
// zone.  The time is set to midnight of this date.
time_t MSDate::asCalendarTime(MSTime::MSTimeZone zone_) const
{
  time_t rc=-1;
  if (date()!=nullDate())
    {
      struct tm aTimeStruct={0};
      MSMonth m; MSDay d; MSYear y; 
      asMonthDayYear(m,d,y);
      aTimeStruct.tm_mday=d;
      aTimeStruct.tm_mon=m-1;
      //Do not substruct 1900 from the year as greenwichMeanTime will add it
      //back on anyway if the year is smaller than 100. Otherwise it will do
      //nothing which is good. 
      aTimeStruct.tm_year=y;   
      aTimeStruct.tm_isdst=-1;
      // tm_wday and tm_yday fields are ignored
      rc = MSTime::greenwichMeanTime(&aTimeStruct, zone_);
    }

  return rc;
}


MSDate::MSDateLocale MSDate::locale(void) { return _locale; }
void MSDate::locale(MSDate::MSDateLocale locale_) { _locale=locale_; }
