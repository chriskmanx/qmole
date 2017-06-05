/********************************************************************************
*                                                                               *
*                            D a t e   C l a s s                                *
*                                                                               *
*********************************************************************************
* Copyright (C) 2005,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* This library is free software; you can redistribute it and/or                 *
* modify it under the terms of the GNU Lesser General Public                    *
* License as published by the Free Software Foundation; either                  *
* version 2.1 of the License, or (at your option) any later version.            *
*                                                                               *
* This library is distributed in the hope that it will be useful,               *
* but WITHOUT ANY WARRANTY; without even the implied warranty of                *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU             *
* Lesser General Public License for more details.                               *
*                                                                               *
* You should have received a copy of the GNU Lesser General Public              *
* License along with this library; if not, write to the Free Software           *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.    *
*********************************************************************************
* $Id: FXDate.cpp,v 1.11 2006/02/03 00:44:32 fox Exp $                          *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXDate.h"


/*
  Notes:
  - Henry F. Fliegel and Thomas C. Van Flandern, "A Machine Algorithm for
    Processing Calendar Dates". CACM, Vol. 11, No. 10, October 1968, pp 657.
*/


using namespace FX;

/*******************************************************************************/

namespace FX {

// Short month names
const FXchar FXDate::shortMonthName[12][4]={
 "Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"
  };


// Long month names
const FXchar FXDate::longMonthName[12][10]={
 "January","February","March","April","May","June","July","August","September","October","November","December"
  };


// Short week day name
const FXchar FXDate::shortWeekDay[7][4]={
  "Sun","Mon","Tue","Wed","Thu","Fri","Sat"
  };


// Long week day name
const FXchar FXDate::longWeekDay[7][10]={
  "Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"
  };


// Number of days in nomimal month
static const FXuchar monthDays[13]={
  0,31,28,31,30,31,30,31,31,30,31,30,31
  };


// Convert gregorian date to julian day number
void FXDate::greg2jul(FXuint& jd,FXint y,FXint m,FXint d){
  if(m<1 || m>12 || d<1 || d>31){ fxerror("FXDate:: bad argument\n"); }
  jd=(1461*(y+4800+(m-14)/12))/4+(367*(m-2-12*((m-14)/12)))/12-(3*((y+4900+(m-14)/12)/100))/4+d-32075;
  }


// Convert julian day number to gregorian date
void FXDate::jul2greg(FXuint jd,FXint& y,FXint& m,FXint& d){
  register FXint l,n,i,j;
  l=jd+68569;
  n=(4*l)/146097;
  l=l-(146097*n+3)/4;
  i=(4000*(l+1))/1461001;
  l=l-(1461*i)/4+31;
  j=(80*l)/2447;
  d=l-(2447*j)/80;
  l=j/11;
  m=j+2-(12*l);
  y=100*(n-49)+i+l;
  }


// Initialize with year, month, and day
FXDate::FXDate(FXint y,FXint m,FXint d){
  greg2jul(julian,y,m,d);
  }


// Set to year, month, and day
void FXDate::setDate(FXint y,FXint m,FXint d){
  greg2jul(julian,y,m,d);
  }


// Get year, month, and day
void FXDate::getDate(FXint& y,FXint& m,FXint& d) const {
  jul2greg(julian,y,m,d);
  }


// is value a leap year?
bool FXDate::leapYear(FXint y){
  return ((y%4==0) && (y%100!=0)) || (y%400==0);
  }


// Return day of the month
FXint FXDate::day() const {
  FXint d,m,y;
  jul2greg(julian,y,m,d);
  return d;
  }


// Return month
FXint FXDate::month() const {
  FXint d,m,y;
  jul2greg(julian,y,m,d);
  return m;
  }


// Return year
FXint FXDate::year() const {
  FXint d,m,y;
  jul2greg(julian,y,m,d);
  return y;
  }


// Return day of the week, starting with sunday
FXint FXDate::dayOfWeek() const {
//  return (((julian+1)%7)+6)%7;        // Monday is day 0 of week
  return (julian+1)%7;                  // Sunday is day 0 of week
  }


// number of days in the months
FXint FXDate::daysInMonth() const {
  FXint y,m,d;
  jul2greg(julian,y,m,d);
  if(m==2 && leapYear(y)) return 29;
  return monthDays[m];
  }


// Return day of year
FXint FXDate::dayOfYear() const {
  FXint y,m,d; FXuint jd;
  jul2greg(julian,y,m,d);
  greg2jul(jd,y,1,1);
  return julian-jd+1;
  }


// Return true if leap year
bool FXDate::leapYear() const {
  FXint d,m,y;
  jul2greg(julian,y,m,d);
  return leapYear(y);
  }


// Return current local date
FXDate FXDate::localDate(){
  FXDate date;
#ifndef WIN32
#if defined(FOX_THREAD_SAFE) && !defined(__FreeBSD__) && !defined(__OpenBSD__)
  struct tm result,*t;
  time_t ltime;
  time(&ltime);
  t=localtime_r(&ltime,&result);
#else
  struct tm *t;
  time_t ltime;
  time(&ltime);
  t=localtime(&ltime);
#endif
  greg2jul(date.julian,t->tm_year+1900,t->tm_mon+1,t->tm_mday);
#else
  SYSTEMTIME t;
  GetLocalTime(&t);
  greg2jul(date.julian,t.wYear,t.wMonth,t.wDay);
#endif
  return date;
  }


// Return current UTC (Zulu) date
FXDate FXDate::zuluDate(){
  FXDate date;
#ifndef WIN32
#if defined(FOX_THREAD_SAFE) && !defined(__FreeBSD__) && !defined(__OpenBSD__)
  struct tm result,*t;
  time_t ltime;
  time(&ltime);
  t=gmtime_r(&ltime,&result);
#else
  struct tm *t;
  time_t ltime;
  time(&ltime);
  t=gmtime(&ltime);
#endif
  greg2jul(date.julian,t->tm_year+1900,t->tm_mon+1,t->tm_mday);
#else
  SYSTEMTIME t;
  GetSystemTime(&t);
  greg2jul(date.julian,t.wYear,t.wMonth,t.wDay);
#endif
  return date;
  }


// save to store
FXStream& operator<<(FXStream& store,const FXDate& d){
  store << d.julian;
  return store;
  }


// load from store
FXStream& operator>>(FXStream& store,FXDate& d){
  store >> d.julian;
  return store;
  }

}

