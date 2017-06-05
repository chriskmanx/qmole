#ifndef MSTimeINLINES
#define MSTimeINLINES

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


INLINELINKAGE MSTime::~MSTime(void) {}

INLINELINKAGE time_t MSTime::nullTime(void)                     
{ return _nullTime; }

INLINELINKAGE void MSTime::defaultConstructToNow(MSBoolean aBoolean_)
{ _defaultConstructToNow=aBoolean_; }
INLINELINKAGE MSBoolean MSTime::defaultConstructToNow(void)
{ return _defaultConstructToNow; }

INLINELINKAGE MSTime::MSTime(void)
: _time(0)
{ if (defaultConstructToNow()==MSTrue) _time=currentTime(); }
INLINELINKAGE MSTime::MSTime(const MSTime& aTime_)
{ _time=aTime_._time; }
INLINELINKAGE MSTime::MSTime(const MSString& aString_, MSTimeZone zone_)
{ _time=nullTime(),internalSet((const char *)aString_,zone_); }
INLINELINKAGE MSTime::MSTime(const char *pString_, MSTimeZone zone_)
{ _time=nullTime(),internalSet(pString_,zone_); }
INLINELINKAGE MSTime::MSTime(time_t aTime_)
{ _time=aTime_; }

INLINELINKAGE MSTime MSTime::now(void)                     
{ return MSTime(currentTime()); }

INLINELINKAGE MSTime::MSTimeZone MSTime::localTimeZone(void)
{ return _localTimeZone; }
INLINELINKAGE MSTime::MSTimeFormat MSTime::defaultFormat(void)
{ return _defaultFormat; }
INLINELINKAGE void MSTime::defaultFormat(MSTime::MSTimeFormat format_)
{ _defaultFormat=format_; }

INLINELINKAGE const MSString& MSTime::strftimeDefaultFormat(void)
{ return _strftimeDefaultFormat; }
INLINELINKAGE void MSTime::strftimeDefaultFormat(MSString& format_)
{ _strftimeDefaultFormat=format_; }
INLINELINKAGE void MSTime::strftimeDefaultFormat(const char *pFormat_)
{ _strftimeDefaultFormat=pFormat_; }

INLINELINKAGE void MSTime::setInvalid(void) 
{ _time=nullTime(); changed(); }

INLINELINKAGE MSTime MSTime::max(const MSTime& aTime_) const 
{ return aTime_._time>_time?aTime_:*this; }
INLINELINKAGE MSTime MSTime::min(const MSTime& aTime_) const 
{ return aTime_._time<_time?aTime_:*this; }
INLINELINKAGE MSBoolean MSTime::between(const MSTime& t1_,const MSTime& t2_) const
{ return MSBoolean(_time>=t1_._time&&_time<=t2_._time); }

INLINELINKAGE MSBoolean MSTime::isSet(void)   const 
{ return MSBoolean(_time!=nullTime()); }
INLINELINKAGE MSBoolean MSTime::isValid(void) const 
{ return MSBoolean(_time!=nullTime()); }

INLINELINKAGE MSTime::operator double() const 
{ return _time; }
INLINELINKAGE MSTime::operator time_t() const 
{ return _time; }

INLINELINKAGE MSTime MSTime::atMidnight(MSTimeZone zone_) const
{ return MSTime(_time-(_time+zoneOffset(zone_))%SECS_IN_DAY); }

INLINELINKAGE MSBoolean MSTime::isAtMidnight(MSTimeZone zone_) const
{ return ((_time+zoneOffset(zone_))%SECS_IN_DAY)==0?MSTrue:MSFalse; }

INLINELINKAGE unsigned long MSTime::secondsPastMinute(MSTimeZone zone_) const
{ return (_time+zoneOffset(zone_))%SECS_IN_MIN; }

INLINELINKAGE unsigned long MSTime::minutesPastHour(MSTimeZone zone_) const
{ return ((_time+zoneOffset(zone_))%SECS_IN_HOUR)/SECS_IN_MIN; }

INLINELINKAGE unsigned long MSTime::hoursPastMidnight(MSTimeZone zone_) const
{ return ((_time+zoneOffset(zone_))%SECS_IN_DAY)/SECS_IN_HOUR; }

INLINELINKAGE long MSTime::zoneOffset(MSTimeZone zone_) const
{ return zoneOffset(_time, zone_); }

// Prefix - add/subtract one, then return result
INLINELINKAGE MSTime& MSTime::operator++() 
{ return ++_time,changed(),*this; }
INLINELINKAGE MSTime& MSTime::operator--() 
{ return --_time,changed(),*this; }

// Postfix - add/subtract one, then return the initial value
INLINELINKAGE MSTime MSTime::operator++(int) 
{ time_t t=_time++; return changed(),MSTime(t); }
INLINELINKAGE MSTime MSTime::operator--(int) 
{ time_t t=_time--; return changed(),MSTime(t); }

INLINELINKAGE MSTime& MSTime::operator+=(long seconds_) 
{ _time+=seconds_; return changed(),*this; }
INLINELINKAGE MSTime& MSTime::operator-=(long seconds_) 
{ _time-=seconds_; return changed(),*this; }

INLINELINKAGE MSBoolean MSTime::operator==(const MSTime &aTime_) const 
{ return MSBoolean(_time==aTime_._time); }
INLINELINKAGE MSBoolean MSTime::operator!=(const MSTime &aTime_) const 
{ return MSBoolean(_time!=aTime_._time); }
INLINELINKAGE MSBoolean MSTime::operator<=(const MSTime &aTime_) const 
{ return MSBoolean(_time<=aTime_._time); }
INLINELINKAGE MSBoolean MSTime::operator>=(const MSTime &aTime_) const 
{ return MSBoolean(_time>=aTime_._time); }
INLINELINKAGE MSBoolean MSTime::operator< (const MSTime &aTime_) const 
{ return MSBoolean(_time<aTime_._time); }
INLINELINKAGE MSBoolean MSTime::operator> (const MSTime &aTime_) const 
{ return MSBoolean(_time>aTime_._time); }

INLINELINKAGE MSTime& MSTime::operator=(const MSTime& aTime_)   
{ _time=aTime_._time; return changed(),*this; }
INLINELINKAGE MSTime& MSTime::operator=(const MSString& aString_) 
{ set(aString_,Local); return *this; }
INLINELINKAGE MSTime& MSTime::operator=(const char *pString_)
{ set(pString_,Local); return *this; }
INLINELINKAGE MSTime& MSTime::operator=(time_t aTime_)          
{ _time=aTime_; return changed(),*this; }

#endif
