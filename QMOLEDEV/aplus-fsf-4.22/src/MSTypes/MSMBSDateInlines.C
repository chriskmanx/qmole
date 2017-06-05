#ifndef MSMBSDateINLINES
#define MSMBSDateINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


INLINELINKAGE MSMBSDate::MSMBSDate(void)
: MSDate((MSJulian)0)
{ if (_defaultConstructToToday==MSTrue) _date=currentDate(); }
INLINELINKAGE MSMBSDate::MSMBSDate(const MSString& aString_)         
{ _date=_nullDate,set((const char *)aString_); }
INLINELINKAGE MSMBSDate::MSMBSDate(const char *pString_)         
{ _date=_nullDate,set(pString_); }
INLINELINKAGE MSMBSDate::MSMBSDate(int m_,int d_,int y_)
{ _date=as30_360(m_,d_,y_); }
INLINELINKAGE MSMBSDate::MSMBSDate(const MSMBSDate& d_)	 
{ _date=d_.asInternal(); }
INLINELINKAGE MSMBSDate::MSMBSDate(MSJulian j_)            
{ _date=j_; }

INLINELINKAGE MSMBSDate MSMBSDate::today(void)
{ return MSMBSDate(currentDate()); }

INLINELINKAGE MSMBSDate::operator double() const 
{ return _date; }
INLINELINKAGE MSMBSDate::operator unsigned long() const 
{ return _date; }

// Prefix - add/subtract one, then return result
INLINELINKAGE MSMBSDate& MSMBSDate::operator++()    
{ return ++_date,changed(),*this; }
INLINELINKAGE MSMBSDate& MSMBSDate::operator--()
{ return --_date,changed(),*this; }

// Postfix - add/subtract one, then return the initial value
INLINELINKAGE MSMBSDate MSMBSDate::operator++(int) 
{ MSJulian d=_date++; return changed(),MSMBSDate(d); }
INLINELINKAGE MSMBSDate MSMBSDate::operator--(int) 
{ MSJulian d=_date--; return changed(),MSMBSDate(d); }  

INLINELINKAGE MSMBSDate& MSMBSDate::operator=(const MSMBSDate& date_) 
{_date=date_._date; return changed(),*this; }

INLINELINKAGE MSBoolean MSMBSDate::operator <(const MSMBSDate& date_) const 
{ return MSBoolean(_date<date_._date); }
INLINELINKAGE MSBoolean MSMBSDate::operator >(const MSMBSDate& date_) const 
{ return MSBoolean(_date>date_._date); }
INLINELINKAGE MSBoolean MSMBSDate::operator<=(const MSMBSDate& date_) const 
{ return MSBoolean(_date<=date_._date); }
INLINELINKAGE MSBoolean MSMBSDate::operator>=(const MSMBSDate& date_) const 
{ return MSBoolean(_date>=date_._date); }
INLINELINKAGE MSBoolean MSMBSDate::operator==(const MSMBSDate& date_) const 
{ return MSBoolean(_date==date_._date); }
INLINELINKAGE MSBoolean MSMBSDate::operator!=(const MSMBSDate& date_) const 
{ return MSBoolean(_date!=date_._date); }

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// static inlines
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
INLINELINKAGE const char* MSMBSDate::dayName(MSDay day_)
{ return MSDate::dayName(day_); }
INLINELINKAGE MSDay MSMBSDate::dayOfWeek(const char *pString_)
{ return MSDate::dayOfWeek(pString_); }
INLINELINKAGE MSBoolean MSMBSDate::dayWithinMonth(MSMonth m_,MSDay d_,MSYear y_)
{ return MSDate::dayWithinMonth(m_,d_,y_); }
INLINELINKAGE MSDay MSMBSDate::daysInMonth(MSMonth,MSYear)
{ return 30; }
INLINELINKAGE MSDay MSMBSDate::daysInYear(MSYear y_)
{ return 360; }
INLINELINKAGE MSMonth MSMBSDate::indexOfMonth(const char* pString_)
{ return MSDate::indexOfMonth(pString_); }
INLINELINKAGE MSJulian MSMBSDate::asJulianNumber(MSMonth m_,MSDay d_,MSYear y_)
{ return MSDate::asJulianNumber(m_,d_,y_); }
INLINELINKAGE MSBoolean MSMBSDate::leapYear(MSYear y_)
{ return MSDate::leapYear(y_); }
INLINELINKAGE const char *MSMBSDate::monthName(MSMonth m_)
{ return MSDate::monthName(m_); }
INLINELINKAGE int MSMBSDate::findMatch(const char* pString_,const char** candidates_,int icand_)
{ return MSDate::findMatch(pString_,candidates_,icand_); }
INLINELINKAGE MSJulian MSMBSDate::nullDate(void)
{ return MSDate::nullDate(); }
INLINELINKAGE MSDate::MSDateFormat MSMBSDate::defaultFormat(void)
{ return _defaultFormat; }
INLINELINKAGE void MSMBSDate::defaultFormat(MSDate::MSDateFormat aFormat_)
{ _defaultFormat=aFormat_; }
INLINELINKAGE const MSString& MSMBSDate::strftimeDefaultFormat(void)
{ return _strftimeDefaultFormat; }
INLINELINKAGE void MSMBSDate::strftimeDefaultFormat(MSString& aString_)
{ _strftimeDefaultFormat=aString_; }
INLINELINKAGE void MSMBSDate::strftimeDefaultFormat(const char *pString_)
{ _strftimeDefaultFormat=pString_; }
INLINELINKAGE void MSMBSDate::defaultConstructToToday(MSBoolean flag_)
{ _defaultConstructToToday=flag_; }
INLINELINKAGE MSBoolean MSMBSDate::defaultConstructToToday(void)
{ return _defaultConstructToToday; }

INLINELINKAGE MSBoolean MSMBSDate::assertWeekDayNumber(MSDay d_)  
{ return MSBoolean(d_>=1&&d_<=7); }
INLINELINKAGE MSBoolean MSMBSDate::assertIndexOfMonth(MSMonth m_) 
{ return MSBoolean(m_>=1&&m_<=12); }

#endif
