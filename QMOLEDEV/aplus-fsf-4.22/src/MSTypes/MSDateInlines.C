#ifndef MSDateINLINES
#define MSDateINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


INLINELINKAGE MSDate::MSDateFormat MSDate::defaultFormat(void)
{ return _defaultFormat; }
INLINELINKAGE void MSDate::defaultFormat(MSDate::MSDateFormat format_)
{ _defaultFormat=format_; }

INLINELINKAGE void MSDate::defaultConstructToToday(MSBoolean aBoolean_)
{ _defaultConstructToToday=aBoolean_; }
INLINELINKAGE MSBoolean MSDate::defaultConstructToToday(void)
{ return _defaultConstructToToday; }

INLINELINKAGE const MSString& MSDate::strftimeDefaultFormat(void)
{ return _strftimeDefaultFormat; }
INLINELINKAGE void MSDate::strftimeDefaultFormat(MSString& format_)
{ _strftimeDefaultFormat=format_; }
INLINELINKAGE void MSDate::strftimeDefaultFormat(const char *pFormat_)
{ _strftimeDefaultFormat=pFormat_; }

INLINELINKAGE MSBoolean MSDate::assertWeekDayNumber(MSDay d_)  
{ return MSBoolean(d_>=1&&d_<=7); }
INLINELINKAGE MSBoolean MSDate::assertIndexOfMonth(MSMonth m_) 
{ return MSBoolean(m_>=1&&m_<=12); }

INLINELINKAGE MSJulian MSDate::nullDate(void)                     
{ return _nullDate; }

INLINELINKAGE MSDate::MSDate(void)
: _date(0)
{ if (defaultConstructToToday()==MSTrue) _date=currentDate(); }
INLINELINKAGE MSDate::MSDate(const MSString& aString_)         
{ _date=nullDate(),set((const char *)aString_); }
INLINELINKAGE MSDate::MSDate(const char *pString_)         
{ _date=nullDate(),set(pString_); }
INLINELINKAGE MSDate::MSDate(int m_,int d_,int y_)   
{ _date=asJulianNumber(m_,d_,y_); }
INLINELINKAGE MSDate::MSDate(const MSDate& d_)	 
{ _date=d_.date(); }
INLINELINKAGE MSDate::MSDate(MSJulian j_)            
{ _date=j_; }

INLINELINKAGE MSDate MSDate::today(void)                     
{ return MSDate(currentDate()); }

INLINELINKAGE MSJulian MSDate::asInternal(void) const 
{ return _date; }

INLINELINKAGE MSDate::operator double() const 
{ return _date; }
INLINELINKAGE MSDate::operator unsigned long() const 
{ return _date; }

// Prefix - add/subtract one, then return result
INLINELINKAGE MSDate& MSDate::operator++()    
{ return ++_date,changed(),*this; }
INLINELINKAGE MSDate& MSDate::operator--()
{ return --_date,changed(),*this; }

// Postfix - add/subtract one, then return the initial value
INLINELINKAGE MSDate MSDate::operator++(int) 
{ MSJulian d=_date++; return changed(),MSDate(d); }
INLINELINKAGE MSDate MSDate::operator--(int) 
{ MSJulian d=_date--; return changed(),MSDate(d); }  

INLINELINKAGE MSDate& MSDate::operator=(const MSDate& date_) 
{_date=date_.date(); return changed(),*this; }

INLINELINKAGE MSBoolean MSDate::operator <(const MSDate& date_) const 
{ return MSBoolean(_date<date_._date); }
INLINELINKAGE MSBoolean MSDate::operator >(const MSDate& date_) const 
{ return MSBoolean(_date>date_._date); }
INLINELINKAGE MSBoolean MSDate::operator<=(const MSDate& date_) const 
{ return MSBoolean(_date<=date_._date); }
INLINELINKAGE MSBoolean MSDate::operator>=(const MSDate& date_) const 
{ return MSBoolean(_date>=date_._date); }
INLINELINKAGE MSBoolean MSDate::operator==(const MSDate& date_) const 
{ return MSBoolean(_date==date_._date); }
INLINELINKAGE MSBoolean MSDate::operator!=(const MSDate& date_) const 
{ return MSBoolean(_date!=date_._date); }

#endif
