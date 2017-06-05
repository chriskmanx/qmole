///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSHashTable.H>
#include <MSTypes/MSTime.H>
#include <MSTypes/MSDate.H>
#include <MSTypes/MSString.H>
#include <MSTypes/MSFormat.H>
#include <MSTypes/MSMessageLog.H>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <stdlib.h> // for getenv
#include <stdio.h>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

// these two constants are no longer used
//char *const MSStandardTimeFormat="%H:%M:%S %m/%d/%Y %Z";
//char *const MSStandardTimeDefinition="  :  :     /  /        ";
const int MSTimeBufferSize=64;
const char ZoneLength=10;

MSTime::MSTimeFormat MSTime::_defaultFormat=MSTime::HoursMinutesSeconds;
MSString MSTime::_strftimeDefaultFormat="%D %T %Z";
time_t MSTime::_nullTime=0;
MSBoolean MSTime::_defaultConstructToNow=MSFalse;
const long MSTime::SECS_IN_MIN =60;
const long MSTime::SECS_IN_HOUR=3600;
const long MSTime::SECS_IN_DAY =86400;

#ifdef MS_NO_INLINES
#include <MSTypes/MSTimeInlines.C>
#endif

MSString MSTime::asString(void) const
{ MSString aString; return MSString(format(aString)); }

MSString MSTime::asDebugInfo(void) const
{
  MSString result("MSTime(@");
  result+=MSString((unsigned long)this).d2x().lowerCase();
  result+=",_time=";
  result+=MSString(_time);
  result+=",_zoneHashTable=";
  result+=MSString((unsigned long)_zoneHashTable).d2x().lowerCase();
  result+=",_localTimeZone=";
  result+=MSString((int)_localTimeZone);
  result+=",_defaultFormat=";
  result+=MSString((int)_defaultFormat);
  result+=",_strftimeDefaultFormat=";
  result+=_strftimeDefaultFormat;
  result+=",_defaultConstructToNow=";
  result+=(defaultConstructToNow()==MSTrue?"MSTrue":"MSFalse");
  result+=",_type=";
  result+=type().symbolName();
  result+=")";
  return MSString(result);
}

MSTime::MSTime(const MSDate& aDate_,MSTimeZone aTimeZone_)
{
  _time=aDate_.asCalendarTime(aTimeZone_);
  if (_time<0)  _time=0;
}

#if defined(MS_NEED_STRPTIME_DECLARATION)
extern "C" char *strptime(const char*, const char*, struct tm*);
#endif

MSTime::MSTime(const MSString& aString_,const char* strptimeFormat_,MSTimeZone zone_)
{
#if defined(MS_NO_STRPTIME)
  MSMessageLog::errorMessage("MSTime: strptime() not supported for this platform.\n");
  _time = 0;
#else
  struct tm tms={0};
  tms.tm_mon=tms.tm_mday=tms.tm_year=tms.tm_isdst=-1;
  char *pStr;

#if !defined(MS_NO_CONSTCHAR_STRPTIME)
  pStr = strptime(aString_.string(),strptimeFormat_,&tms);
#else
  pStr = strptime((char *)aString_.string(),(char *)strptimeFormat_,&tms);
#endif  

  if (pStr==0) // if the string could not be parsed
    {
      _time = 0;
      return;
    }

  int isDaySet=0, isMonthSet=0, isYearSet=0;

#if defined(MS_TMS_RESET_BY_STRPTIME)
  // On some systems, strptime() resets the tm structure, setting to 0 all fields not mentioned in the format string.
  // This make it hard for us to determine if a field has been set or not because 0 is a valid value for some of the
  // fields.  One assumption we are making here is that if strptime() zeroes out the unset fields, it will make sure
  // that the 2-digit years are handled correctly (that is, add 100 if it's <38).  This assumption is currently true,
  // except on SGI platforms, where we have submitted a bug report and are waiting for a fix.
  //
  isDaySet = tms.tm_mday>0;	// valid range is [1,31]
  //
  // If all unset fields are 0, it is impossible to determine if the month has not been set or has been set to 0, since
  // tm_mon's valid range is [0,11].  Therefore, we are doing some guess work here:  if tm_mon>0, then it is definitely
  // set; otherwise (if tm_mon==0), we will consider it in pair with tm_mday -- either both are set (i.e., if tm_mday
  // is set and tm_mon==0, then tm_mon is considered set as well) or both are unset (i.e., if tm_mday is unset and
  // tm_mon==0, then tm_mon is considered unset as well).  This makes sense since a format containing just the month,
  // without the day of the month, is considered invalid in construction of an MSTime object.
  //
  isMonthSet = (tms.tm_mon>0 || (tms.tm_mon==0 && isDaySet));  // valid range is [0,11]
  isYearSet = tms.tm_year>0;	// valid range is [70,138]
  tms.tm_isdst = -1;
#else
  isDaySet = tms.tm_mday>0;	// valid range is [1,31]
  isMonthSet = tms.tm_mon>=0;	// valid range is [0,11]
  isYearSet = tms.tm_year>=0;	// valid range is [0,38] and [70,138]
  //
  // 2-digit years <=MS_PIVOT_YEAR (really <=38, since 2038 is the limit of time_t type) are considered to be
  // in 21st century; however, strptime() on some systems does not adjust the year by 100; thus, we do it here
  // ourselves
  //
  if (isYearSet && tms.tm_year<=MS_PIVOT_YEAR) tms.tm_year+=100;
#endif //MS_TMS_RESET_BY_STRPTIME
  //
  // if only the time is specified (no date), then today's date is taken;
  // if the date is specified (both tm_mon and tm_mday) but without the year, the current year is taken
  //
  if (!isYearSet)	// the year is not set
    {
      if (!isDaySet && !isMonthSet) // neither month nor day is set
	{
	  #ifdef MS_THREAD_SAFE_FUNCTIONS
	  struct tm tmp;
	  #endif //MS_THREAD_SAFE_FUNCTIONS

	  time_t curTime = time(0);
	  struct tm *pCur_tm;

	  if (zone_==Local)	// for local time zone, we cannot determine DST presence without calling localtime()
	    {
	      pCur_tm = MS_LOCALTIME(&curTime,&tmp);
	    }
	  else	// for a specific time zone, we can get adjust the time with a cached offset and call gmtime()
	    {
	      curTime += zoneOffset(zone_);
	      pCur_tm = MS_GMTIME(&curTime,&tmp);
	    }

	  tms.tm_year = pCur_tm->tm_year;
	  tms.tm_mon  = pCur_tm->tm_mon;
	  tms.tm_mday = pCur_tm->tm_mday;
	}
      else if (isDaySet && isMonthSet)	// both month and day are set
	{
	  #ifdef MS_THREAD_SAFE_FUNCTIONS
	  struct tm tmp;
	  #endif //MS_THREAD_SAFE_FUNCTIONS

	  time_t curTime = time(0);
	  if (zone_==Local)	// for local time zone, we cannot determine DST presence without calling localtime()
	    {
	      tms.tm_year = MS_LOCALTIME(&curTime,&tmp)->tm_year;
	    }
	  else	// for a specific time zone, we can adjust the time with a cached offset and call gmtime()
	    {
	      curTime += zoneOffset(zone_);
	      tms.tm_year = MS_GMTIME(&curTime,&tmp)->tm_year;
	    }
	}
    }
  else	// the year is set
    {
      if (!isDaySet)   tms.tm_mday=1;
      if (!isMonthSet) tms.tm_mon=0;
    }

  _time = greenwichMeanTime(&tms,zone_);
#endif	//MS_NO_STRPTIME
}

MSString MSTime::className(void) const
{ return MSString("MSTime"); }

const MSSymbol& MSTime::type(void) const
{ return symbol(); }

MSModel *MSTime::clone(void) const
{ return new MSTime(*this); }

MSModel *MSTime::create(void) const
{ return new MSTime(); }

void MSTime::assign(const MSModel& aModel_)
{ *this=(MSTime&)aModel_; }

long MSTime::compare(const MSModel& aModel_) const
{ return ::compare(*this,(MSTime&)aModel_); }

MSString MSTime::asMSF(void) const
{ 
  MSString buf;
  (void) format (&buf,MSTime::HoursMinutesSecondsSlash4);
  return buf.encodeMSF();
}

MSError::ErrorStatus MSTime::setFromMSF(const char *pString_)
{ 
  MSError::ErrorStatus code=MSError::BadMSFString;
  if (pString_!=0)
   {
     MSString buf(pString_);
     buf.decodeMSF();
     code=set(buf);
   }
  return code;
}

const MSSymbol& MSTime::symbol(void)    
{
  static MSSymbol sym ("MSTime");
  return sym;
}

struct zoneoff 
{
  char	             *zname;
  MSTime::MSTimeZone  zone;
  int	              hr_off;
  int	              mn_off;
};

static int mtbl[]={ 0,31,59,90,120,151,181,212,243,273,304,334 };

static struct zoneoff TimeZones[]=
{
    // Military Time 
    { "A",MSTime::A,	      1,  0 },	{ "N",MSTime::N,          -1,  0 },
    { "B",MSTime::B,	      2,  0 },	{ "O",MSTime::O,          -2,  0 },
    { "C",MSTime::C,	      3,  0 },	{ "P",MSTime::P,          -3,  0 },
    { "D",MSTime::D,	      4,  0 },	{ "Q",MSTime::Q,          -4,  0 },
    { "E",MSTime::E,	      5,  0 },	{ "R",MSTime::R,          -5,  0 },
    { "F",MSTime::F,	      6,  0 },	{ "S",MSTime::S,          -6,  0 },
    { "G",MSTime::G,	      7,  0 },	{ "T",MSTime::T,          -7,  0 },
    { "H",MSTime::H,	      8,  0 },	{ "U",MSTime::U,          -8,  0 },
    { "I",MSTime::I,	      9,  0 },	{ "V",MSTime::V,          -9,  0 },
    { "K",MSTime::K,	     10,  0 },	{ "W",MSTime::W,         -10,  0 },
    { "L",MSTime::L,	     11,  0 },	{ "X",MSTime::X,         -11,  0 },
    { "M",MSTime::M,	     12,  0 },	{ "Y",MSTime::Y,         -12,  0 },
    { "Z",MSTime::Z,	      0,  0 },
    // Europe
    { "BST",MSTime::BST,      1,  0 },                                       // British Summer 
    { "EET",MSTime::EET,      2,  0 },	{ "EET DST",MSTime::EET_DST, 3, 0 }, // Eastern Europe
    { "MET",MSTime::MET,      1,  0 },	{ "MET DST",MSTime::MET_DST, 2, 0 }, // Middle Europe
    { "WET",MSTime::WET,      0,  0 },	{ "WET DST",MSTime::WET_DST, 1, 0 }, // Western Europe
    { "MSK",MSTime::MSK,      3,  0 },  { "MSD",MSTime::MSD,         4, 0 }, // Russia (Moscow)
    // Asia and Australasia
    { "JST",MSTime::JST,      9,  0 },                                       // Japan 
    { "HKT",MSTime::HKT,      8,  0 },                                       // Hong Kong
    { "SGT",MSTime::SGT,      8,  0 },                                       // Singapore
    { "KST",MSTime::KST,      9,  0 },  { "KDT",MSTime::KDT,      10,  0 },  // South Korea
    { "AEST",MSTime::AEST,   10,  0 },	{ "AESST",MSTime::AESST,  11,  0 },  // Eastern Australia
    { "ACST",MSTime::ACST,    9, 30 },	{ "ACSST",MSTime::ACSST,  10, 30 },  // Central Australia
    { "AWST",MSTime::AWST,    8,  0 },                                       // Western Australia
    { "NZST",MSTime::NZST,   12,  0 },  { "NZDT",MSTime::NZDT,    13,  0 },  // New Zealand
    // North America
    { "NST",MSTime::NST,     -3,-30 },  { "NDT",MSTime::NDT,      -2,-30 },  // Newfoundland 
    { "AST",MSTime::AST,     -4,  0 },	{ "ADT",MSTime::ADT,      -3,  0 },  // Atlantic 
    { "EST",MSTime::EST,     -5,  0 },	{ "EDT",MSTime::EDT,      -4,  0 },  // Eastern 
    { "CST",MSTime::CST,     -6,  0 },	{ "CDT",MSTime::CDT,      -5,  0 },  // Central 
    { "MST",MSTime::MST,     -7,  0 },	{ "MDT",MSTime::MDT,      -6,  0 },  // Mountain 
    { "PST",MSTime::PST,     -8,  0 },	{ "PDT",MSTime::PDT,      -7,  0 },  // Pacific 
    { "AKST",MSTime::AKST,   -9,  0 },  { "AKDT",MSTime::AKDT,    -8,  0 },  // Alaska
    { "HAST",MSTime::HAST,  -10,  0 },	{ "HADT",MSTime::HADT,    -9,  0 },  // Aleutian
    { "HST",MSTime::HST,    -10,  0 },                                       // Hawaii 
    // Universal Time 
    { "UT",MSTime::UT,	      0,  0 },
    { "GMT",MSTime::GMT,      0,  0 },
    { "Local",MSTime::Local,  0,  0 },
    // Also legal is +/- followed by hhmm offset from UT 
    { 0,MSTime::A,0,0 }
};

MSHashTable *MSTime::_zoneHashTable=MSTime::initZoneHashTable();
MSTime::MSTimeZone MSTime::_localTimeZone=MSTime::setLocalTimeZone();

void MSTime::unset(void)
{
  if (isSet()==MSTrue)
   {
     _time=nullTime();
     changed();
   }
}

// Values that can be passed to ::set()
//    1) A value of "0", which causes the time to be set to nullTime().
//    2) A value in the form "secs timezone" (i.e. "701296172 GMT+9:00").
//    3) A user-supplied value which is assumed to be in locale-specific format.
MSError::ErrorStatus MSTime::set(const MSString *pString_, MSTimeZone zone_)
{ return set(pString_->string(),zone_); }
MSError::ErrorStatus MSTime::set(const MSString& aString_, MSTimeZone zone_)
{ return set(aString_.string(),zone_); }
MSError::ErrorStatus MSTime::set(const char *pString_)
{ return set(pString_,Local); }
MSError::ErrorStatus MSTime::set(const char *pString_, MSTimeZone zone_)
{ 
  MSError::ErrorStatus status=internalSet(pString_,zone_);
  return changed(),status;
}

MSError::ErrorStatus MSTime::internalSet(const char *cp_, MSTimeZone zone_)
{
  static char          *fname="MSTime::set";
  struct tm             tmStruct={0},*now;
  char                  buf[ZoneLength];
  time_t                aTime;
  MSError::ErrorStatus  rc=MSError::MSSuccess;
  #ifdef MS_THREAD_SAFE_FUNCTIONS
  struct tm tmp;
  #endif //MS_THREAD_SAFE_FUNCTIONS

  tmStruct.tm_isdst=-1;	  // when tm_isdst<0, libc functions will determine the correct time zone

  if (cp_==0) rc=MSError::MSFailure;
  else
    {
      if (*cp_=='\0'||(cp_[0]=='0'&&cp_[1] =='\0')) 
	{
	  _time=nullTime();
	  return MSError::MSSuccess;
	}
      
      int assigned=sscanf(cp_,"%d:%d:%d %d/%d/%d %s",
			  &tmStruct.tm_hour,&tmStruct.tm_min,&tmStruct.tm_sec,
			  &tmStruct.tm_mon,&tmStruct.tm_mday,&tmStruct.tm_year,buf);
      
      switch (assigned) 
	{
	case 1: // format of "seconds GMT+hh:mm" or a valid date format
	  // check for valid date format
	  aTime=MSDate(cp_).asCalendarTime(zone_);
	  if (aTime>=0)
	    {
	      _time=aTime;
	    }
	  // check for format of "seconds GMT+hh:mm"
	  else
	    {
	      assigned=sscanf(cp_,"%d %s",&aTime,buf);
	      if (assigned==1)	// simply seconds, without a time zone
		{
		  _time=aTime-zoneOffset(zone_);
		}
	      else if (assigned==2)  // seconds plus time zone (in which case it overrides the zone_ argument)
		{
		  _time=aTime-scanTimeZone(buf);
		}
	      else 
		{
		  MSMessageLog::errorMessage("Invalid format in %s: \"%s\"\n",fname,cp_);
		  rc=MSError::Internal;
		}
	    }
	  break;
	case 2:
	  // i.e. format of hh:mm"
	  tmStruct.tm_sec=0;
	  // FALL THRU
	case 3:
	  // i.e. format of hh:mm:ss"
	  (void)time(&_time);
	  if (zone_==Local)	// cannot determine DST presence => have to use localtime()
	    {
	      now=MS_LOCALTIME(&_time,&tmp);
	    }
	  else	// any specific time zone => can use gmtime()
	    {
	      time_t t=_time+zoneOffset(zone_);
	      now=MS_GMTIME(&t,&tmp);
	    }
	  //
	  // _time is now set to current time; we need to set it to the given number of hours, minutes, and seconds
	  // of the *same* day.  Thus, instead of calling greenwichMeanTime() with now pointer and recomputing
	  // everything from scratch, we can simply adjust _time to the desired time of day.
	  //
	  _time += SECS_IN_HOUR*(tmStruct.tm_hour-now->tm_hour) +
	           SECS_IN_MIN*(tmStruct.tm_min-now->tm_min) +
	           tmStruct.tm_sec-now->tm_sec;
	  break;
	case 6:
	  // i.e. format of "hh:mm:ss mm/dd/yyyy" or "hh:mm:ss mm/dd/yy"
	  --tmStruct.tm_mon;	// tm_mon is 0-based
	  if (tmStruct.tm_year<100)
	    {
	      // this should be consistent with MSDate:  2-digit years <=MS_PIVOT_YEAR (defined in MSDefines.H)
	      // are considered to be in 21st century (e.g., 3/10/29 is 3/10/2029) while those >MS_PIVOT_YEAR
	      // are in the 20th century (3/10/79 is 3/10/1979)
	      //
	      tmStruct.tm_year += (tmStruct.tm_year<=MS_PIVOT_YEAR) ? 2000 : 1900;
	    }
	  _time=greenwichMeanTime(&tmStruct,zone_);
	  break;
	case 7:
	  {
	    // i.e. format of "hh:mm:ss mm/dd/yyyy GMT+9:00" or "hh:mm:ss mm/dd/yy GMT+9:00";
	    // the time zone in the string overrides the zone_ argument
	    //
	    if (strlen(buf)>=ZoneLength) 
	      {
		MSMessageLog::errorMessage("Cannot store buf=\"%s\" in %s(): too large\n",buf,fname);
		rc=MSError::Internal;
		break;
	      }
	    MSString zone;
	    time_t offset = scanTimeZone(buf);
	    --tmStruct.tm_mon;	// tm_mon is 0-based
	    if (tmStruct.tm_year<100)
	      {
		// this should be consistent with MSDate:  2-digit years <=MS_PIVOT_YEAR (defined in MSDefines.H)
		// are considered to be in 21st century (e.g., 3/10/29 is 3/10/2029) while those >MS_PIVOT_YEAR
		// are in the 20th century (3/10/79 is 3/10/1979)
		//
		tmStruct.tm_year += (tmStruct.tm_year<=MS_PIVOT_YEAR) ? 2000 : 1900;
	      }
	    _time=greenwichMeanTime(&tmStruct,GMT)-offset;
	  }
	  break;
	default:
	  rc=MSError::MSFailure;
	  break;
	}
    }
  return rc;
}

const char *MSTime::format(MSString *pString_) const
{ return format(*pString_,defaultFormat()); }

const char *MSTime::format(MSString& aString_) const
{ return format(aString_,defaultFormat()); }

const char *MSTime::format(MSString *pString_,const char *format_) const
{ return format(*pString_,format_); }

const char *MSTime::format(MSString& aString_,const char *format_) const
{
  #ifdef MS_THREAD_SAFE_FUNCTIONS
  struct tm tmp;
  #endif //MS_THREAD_SAFE_FUNCTIONS

  MSString aString(0,((format_!=0)?strlen(format_):0)+6);
  struct tm *pCalendarTime=MS_LOCALTIME((time_t *)&_time,&tmp);
  int numberOfChars;
  while ((numberOfChars=strftime((char*)aString.string(),aString.length(),format_,pCalendarTime))==0)
   {
     aString=MSString(0,2*aString.length());
   }
  //
  // remove the terminating null from strftime. strftime returns the number of characters
  // produced not including the terminating null character.
  //
  aString_=MSString(aString.string(),numberOfChars);
  return aString_;

}

const char *MSTime::format(MSString& aString_,const MSFormat& aFormat_,MSTime::MSTimeZone zone_) const
{ return (aFormat_.formatType()==MSFormat::Time)?format(aString_,aFormat_.timeFormat(),zone_):format(aString_); }

const char *MSTime::format(MSString *pString_,const MSFormat& aFormat_,MSTime::MSTimeZone zone_) const
{ return format(*pString_,aFormat_,zone_); }

const char *MSTime::format(MSString *pString_,MSTimeFormat format_,MSTime::MSTimeZone zone_) const
{ return format(*pString_,format_,zone_); }

const char *MSTime::format(MSString& aString_,MSTimeFormat format_,MSTime::MSTimeZone zone_) const
{
  aString_.removeAll(); 
  if (_time==nullTime()) aString_="0";
  else
   {
     switch (format_) 
      {
      case HoursMinutesSeconds:
	strftimeFormat(aString_,"%H:%M:%S",zone_);
	break;
      case MonthDayYear:
	strftimeFormat(aString_,"%m/%d/%Y",zone_);
        break;
      case HoursMinutesSecondsSlash: 
	strftimeFormat(aString_,"%H:%M:%S %m/%d/%y",zone_);
	break;
      case HoursMinutesSecondsSlashZone: 
	strftimeFormat(aString_,"%H:%M:%S %m/%d/%y ",zone_,MSTrue);
	break;
      case HoursMinutesSecondsSlash4:
	strftimeFormat(aString_,"%H:%M:%S %m/%d/%Y",zone_);
	break;
      case HoursMinutesSecondsSlash4Zone:
	strftimeFormat(aString_,"%H:%M:%S %m/%d/%Y ",zone_,MSTrue);
	break;
      case CalendarTime:
	{  
	  char buf[MSTimeBufferSize];
	  const char *zname;
	  time_t t;
	  if (zone_==Local) // can't determine DST presence for local time zone => use localtime() to find zone name and offset
	    {
	      #ifdef MS_THREAD_SAFE_FUNCTIONS
	      struct tm tmp;
	      #endif //MS_THREAD_SAFE_FUNCTIONS

	      struct tm *pTm=MS_LOCALTIME(&_time,&tmp);
	      zname=zoneName(pTm);
	      t=_time+zoneOffset(pTm);
	    }
	  else	// specific time zone => use the cached name and offset values from the time zone table
	    {
	      zname=TimeZones[zone_].zname;
	      t=_time+zoneOffset(zone_);
	    }
	  sprintf(buf,"%d %s",t,zname);
	  aString_=buf;
	  break;
	}
      case Strftime:
        format(aString_,(const char *)strftimeDefaultFormat());
	break;     
      default:
	MSMessageLog::warningMessage("MSTime: invalid value of format\n");
	break;
      }
   }
  return aString_.string();
}

void MSTime::strftimeFormat(MSString& aString_,const char *format_,MSTime::MSTimeZone zone_,
			    MSBoolean appendZoneName_) const
{
  char buf[MSTimeBufferSize];
  const char *zname;
  struct tm *now;

  #ifdef MS_THREAD_SAFE_FUNCTIONS
  struct tm tmp;
  #endif //MS_THREAD_SAFE_FUNCTIONS

  if (zone_==Local)	// cannot determine if DST is on or not; have to use localtime()
    {
      now=MS_LOCALTIME(&_time,&tmp);
      zname=zoneName(now);
    }
  else	// a specific, non-local time zone -> can use the cached values from the table
    {
      long src=_time+zoneOffset(zone_);
      now=MS_GMTIME((time_t *)&src,&tmp);
      zname=TimeZones[zone_].zname;
    }
  
  strftime(buf,sizeof(buf),format_,now);
  aString_=buf;
  if (appendZoneName_==MSTrue)
    {
      aString_+=zname;
    }
}

const char *MSTime::format(MSString *pString_,MSTimeFormat format_,const char *zone_) const
{ return format(*pString_,format_,zone_); }

const char *MSTime::format(MSString& aString_,MSTimeFormat format_,const char *zone_) const
{
  aString_.removeAll(); 
  if (_time==nullTime()) aString_="0";
  else
   {
     switch (format_) 
      {
      case HoursMinutesSeconds:
	strftimeFormat(aString_,"%H:%M:%S",zone_);
	break;
      case MonthDayYear:
	strftimeFormat(aString_,"%m/%d/%Y",zone_);
	break;
      case HoursMinutesSecondsSlash: 
	strftimeFormat(aString_,"%H:%M:%S %m/%d/%y",zone_);
	break;
      case HoursMinutesSecondsSlashZone: 
	strftimeFormat(aString_,"%H:%M:%S %m/%d/%y ",zone_);
	aString_+=zone_;
	break;
      case HoursMinutesSecondsSlash4:
	strftimeFormat(aString_,"%H:%M:%S %m/%d/%Y",zone_);
	break;
      case HoursMinutesSecondsSlash4Zone:
	strftimeFormat(aString_,"%H:%M:%S %m/%d/%Y ",zone_);
	aString_+=zone_;
	break;
      case CalendarTime:
	{
	  char buf[MSTimeBufferSize];
	  long src=_time+zoneOffset(zone_);
	  sprintf(buf,"%d %s",src,zone_);
	  aString_=buf;
	  break;
	}
      case Strftime:
        format(aString_,(const char *)strftimeDefaultFormat());
	break;     
      default:
	MSMessageLog::warningMessage("MSTime: invalid value of format\n");
	break;
      }
   }
  return aString_.string();
}

void MSTime::strftimeFormat(MSString& aString_,const char *format_,const char *zone_) const
{
  #ifdef MS_THREAD_SAFE_FUNCTIONS
  struct tm tmp;
  #endif //MS_THREAD_SAFE_FUNCTIONS

  char buf[MSTimeBufferSize];
  long src=_time+zoneOffset(zone_);
  struct tm *now=MS_GMTIME((time_t *)&src,&tmp);
  strftime(buf,sizeof(buf),format_,now);  
  aString_=buf;
}

// this uses an MSDate to set the time
MSError::ErrorStatus MSTime::setNow(void)
{
  _time = currentTime();
  changed();
  return MSError::MSSuccess;
}


unsigned long MSTime::differenceInDays(const MSTime& aTime_,MSTimeZone zone_) const
{
  time_t t1offset=_time+zoneOffset(zone_);
  time_t t2offset=aTime_._time+aTime_.zoneOffset(zone_);
  time_t t1=(t1offset-t1offset%SECS_IN_DAY);
  time_t t2=(t2offset-t2offset%SECS_IN_DAY);
  if (t2>=t1) return (t2-t1)/SECS_IN_DAY;
  else  return (t1-t2)/SECS_IN_DAY;
}


long MSTime::scanTimeZone(const char *zone_)
{
  if (zone_==0 || *zone_=='\0')
    {
      return 0;
    }

  MSString zone(zone_);
  unsigned signIndex = zone.indexOfAnyOf("+-");
  int adjust=0;

  if (signIndex<zone.length())	// if found a '+' or '-' => format is "TZ+hh:mm" or "TZ-hh:mm"
    {
      int hours, mins;
      switch (sscanf(zone.string()+signIndex,"%d:%d",&hours,&mins)) 
	{
	case 1:
	  adjust=hours*SECS_IN_HOUR;
	  break;
	case 2:
	  adjust=hours*SECS_IN_HOUR+mins*SECS_IN_MIN;
	  break;
	default:
	  MSMessageLog::errorMessage("Bad time zone specification in MSTime::scanTimeZone(): \"%s\"\n",zone_);
	  adjust=0;
	  break;
	}

      if (zone(signIndex)=='+')
	{
	  adjust=-adjust;
	}
	   
      zone.take(signIndex);	// cut off the adjustment portion, leaving only the time zone name
    }

  zone.upper();
  return zoneOffset(zone)+adjust;
}


long MSTime::zoneOffset(const char *zoneName_) const
{
  // This method looks for an exact match of zoneName_ to one of the time zone names in
  // the _zoneHashTable; thus, zoneName_ has to be in the same case (upper) and without
  // any adjustment portions - all this should have already been done in the calling function.
  //
  struct zoneoff *zp = (struct zoneoff *)zoneHashTable()->lookup(zoneName_);
  if (zp==(struct zoneoff *)zoneHashTable()->notFound())
    {
      MSMessageLog::warningMessage("Invalid timezone:  \"%s\"\n",zoneName_);
      return 0;
    }
  else
    {
      return zoneOffset(zp->zone);
    }
}


long MSTime::zoneOffset(time_t t_, MSTime::MSTimeZone zone_)
{
  if (zone_==Local)	// cannot properly determine if DST is on - have to call localtime()
    {
      #ifdef MS_THREAD_SAFE_FUNCTIONS
      struct tm tmp;
      #endif //MS_THREAD_SAFE_FUNCTIONS

      return zoneOffset(MS_LOCALTIME(&t_,&tmp));
    }
  else
    {
      return (TimeZones[zone_].hr_off*SECS_IN_HOUR+TimeZones[zone_].mn_off*SECS_IN_MIN);
    }
}


long MSTime::zoneOffset(const struct tm *pTm_)
{
  if (pTm_==0)
    {
      return 0;
    }

#if !defined(MS_STRUCT_TM_TZ)
  if (pTm_->tm_isdst<=0)	// if DST is not in effect or information is not available
    {
      return -timezone;
    }
  else	// DST is in effect
    {
#if defined(MS_NO_ALTZONE)
      return (-timezone+SECS_IN_HOUR);	// add 1 hour to the opposite of offset
#else  
      return -altzone;
#endif // MS_NO_ALTZONE
    }
#else  // MS_STRUCT_TM_TZ
  return pTm_->tm_gmtoff;
#endif // MS_STRUCT_TM_TZ
}


const char *MSTime::zoneName(const struct tm *pTm_)
{
  if (pTm_==0)
    {
      return "";
    }
  
#if !defined(MS_STRUCT_TM_TZ)
  if (pTm_->tm_isdst<=0)	// if DST is not in effect or information is not available
    {
      return tzname[0];
    }
  else	// DST is in effect
    {
      return tzname[1];
    }
#else  // MS_STRUCT_TM_TZ
  return pTm_->tm_zone;
#endif // MS_STRUCT_TM_TZ
}

// Kind of the reverse of localtime() and gmtime() -- converts a struct tm
// to time in seconds since 1970.  Valid until 2038.
// If the "zone" argument is present, it modifies the return value.
long MSTime::greenwichMeanTime(struct tm *time_, MSTimeZone zone_)
{
  // The following rules are used for determining the year:
  // 
  // - the tm structure cannot represent any dates before 1900 or after 2038
  //
  // - if the year is >=1900, then it's taken to be an "absolute" year - that is, it's
  //   assumed to be represented as a full 4-digit number (e.g., 1997 or 2047)
  //
  // - if the year is <1900, then it's taken to be a "relative" year - that is, it's the
  //   real year minus 1900; thus, 96 represents (1900+96)=1996 and 136 represents
  //   (1900+136)=2036; of course, the valid range for a "relative" year is 0-138
  // 
  if (zone_==Local)	// cannot determine if DST is on or not; have to use libc functions
    {
      if (time_->tm_year>=1900)	// if the year is "absolute" (see above)
	{
	  time_->tm_year-=1900;	 // make the year relative to 1900 so that libc functions work correctly
	}
#if defined(MS_HAS_MKTIME) || defined(HAVE_MKTIME)
      time_t t=mktime(time_);   // mktime() assumes that the tm structure it gets is relative to the local time zone
#else
      time_t t=timelocal(time_);
#endif  //MS_HAS_MKTIME
      return (t<0) ? 0 : t;
    }

  long julian, year;

  if (time_->tm_year<1900)  // if the year is "relative" to 1900
    {
      year=time_->tm_year+1900;
    }
  else	// if the year is absolute, i.e., represented by all 4 digits
    {
      year=time_->tm_year;
    }

  julian=365*(year-1970)+(int)((year-1970+1)/4)+mtbl[time_->tm_mon]+time_->tm_mday-1;
  // time_->tm_yday might not be valid 
  if (time_->tm_mon>1&&year%4==0&&(year%100!=0||year%400==0)) julian++;
  // convert to seconds 
  julian*=SECS_IN_DAY;
  julian+=(time_->tm_hour*SECS_IN_MIN+time_->tm_min)*SECS_IN_MIN+time_->tm_sec;
  return julian-zoneOffset(julian,zone_);
}

MSTime::MSTimeZone MSTime::setLocalTimeZone(void)
{
  //
  // There is a possibility that the local time zone returned by localtime() is not in our TimeZones table
  // and, correspondingly, not in the _zoneHashTable.  Then we have to add it to both tables so that we are
  // able to use the local time zone, even though we haven't accounted for it originally.
  //
  static char buf[MSTimeBufferSize];  
  struct zoneoff *zp=&TimeZones[Local];	  // zoneoff structure for the local time zone

  #ifdef MS_THREAD_SAFE_FUNCTIONS
  struct tm tmp;
  #endif //MS_THREAD_SAFE_FUNCTIONS

  time_t now;
  (void)time(&now);
  struct tm *pTime=MS_LOCALTIME(&now,&tmp);

  long gmtoff = zoneOffset(pTime);
  strcpy(buf,zoneName(pTime));
  zp->hr_off=gmtoff/SECS_IN_HOUR;
  zp->mn_off=(gmtoff%SECS_IN_HOUR)/SECS_IN_MIN;
  zp->zname=buf;

  if (zoneHashTable()->lookup(buf)==(void *)zoneHashTable()->notFound()) // if local zone is not in the table yet
    {
      zoneHashTable()->add(buf,(void *)zp);   // add it to the hash table
    }
      
  return zp->zone;
}


time_t MSTime::currentTime(void)
{
  static int	checked=0;
  static int	override=0;
  static MSDate	date;
  time_t timeNow;
  
  if (checked++==0) 
    {
      char *ep=getenv("TB_DATE_OVERRIDE");    
      if (ep!=0) 
	{
	  date.setToday();
	  override=1;
	}
    }
  
  (void)time(&timeNow);

  if (override)
    {
      #ifdef MS_THREAD_SAFE_FUNCTIONS
      struct tm tmp;
      #endif //MS_THREAD_SAFE_FUNCTIONS

      struct tm	 *pTmStruct;
      pTmStruct=MS_LOCALTIME(&timeNow,&tmp);
      pTmStruct->tm_mday=date.dayOfMonth();
      pTmStruct->tm_mon=date.month()-1;
      pTmStruct->tm_year=date.year();  // we don't need to subtract 1900, since greenwichMeanTime() will handle that
      //
      // We have to call greenwichMeanTime() with Local (instead of trying to find out the zone offset from pTmStruct)
      // because the current time and the override date may be on different sides of daylight savings "barrier".
      //
      return greenwichMeanTime(pTmStruct,Local);
    }
  else
    {
      return timeNow;
    }
}

MSTime MSTime::operator+(long seconds_) const
{ return MSTime(_time+seconds_); }
MSTime MSTime::operator-(long seconds_) const
{ return MSTime(_time-seconds_); }
MSTime operator+(long seconds_,const MSTime &aTime_)
{ return MSTime(aTime_._time+seconds_); }

// This is an internal method that returns the unix time
// of the given MSDate offset by the hours, minutes,
// and seconds of this object.
time_t MSTime::convertDate(const MSDate& aDate_) const
{
  #ifdef MS_THREAD_SAFE_FUNCTIONS
  struct tm tmp;
  #endif //MS_THREAD_SAFE_FUNCTIONS

  time_t t=aDate_.asCalendarTime();
  struct tm *pTimeStruct=MS_LOCALTIME(&_time,&tmp);
  unsigned long s=pTimeStruct->tm_sec;
  unsigned long m=pTimeStruct->tm_min*SECS_IN_MIN;
  unsigned long h=pTimeStruct->tm_hour*SECS_IN_HOUR;
  return t+s+m+h;
}

MSTime MSTime::operator+(const MSTerm& aTerm_) const
{
  MSDate d(*this);
  d+=aTerm_;
  return MSTime(convertDate(d));
}

MSTime operator+(const MSTerm& aTerm_,const MSTime &aTime_)
{
  MSDate d(aTime_);
  d+=aTerm_;
  return MSTime(aTime_.convertDate(d));
}

MSTime& MSTime::operator+=(const MSTerm& aTerm_)
{
  MSDate d(*this);
  d+=aTerm_;
  _time=convertDate(d);
  return changed(),*this;
}

MSTime MSTime::operator-(const MSTerm& aTerm_) const
{
  MSDate d(*this);
  d-=aTerm_;
  return MSTime(convertDate(d));
}

MSTime& MSTime::operator-=(const MSTerm& aTerm_)
{
  MSDate d(*this);
  d-=aTerm_;
  _time=convertDate(d);
  return changed(),*this;
}

MSHashTable *MSTime::initZoneHashTable(void)
{
  MSHashTable *ht=new MSHashTable(64);
  ht->notFound(0x0);
  for (int i=0;TimeZones[i].zname!=0;i++) ht->add(TimeZones[i].zname,(void *)&TimeZones[i]);
  return ht;
}

ostream& operator<<(ostream& os_,const MSTime& aTime_)
{ MSString s; return os_<<aTime_.format(s)<<flush; }
