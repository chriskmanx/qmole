///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSTerm.H>
#include <MSTypes/MSDate.H>
#include <MSTypes/MSUtil.H>
#include <MSTypes/MSFormat.H>
#include <MSTypes/MSTypes_MSF.H>
#include <MSTypes/MSMessageLog.H>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <ctype.h>

static const char MSTermDelimiter=',';
static const double DaysInYear=365.25;

ostream& operator<<(ostream& aStream_,const MSTerm& aTerm_)
{ MSString aString; return aStream_<<aTerm_.format(aString); }

#ifdef MS_NO_INLINES
#include <MSTypes/MSTermInlines.C>
#endif

MSString MSTerm::asString(void) const
{ MSString aString; return MSString(format(aString)); }

MSString MSTerm::asDebugInfo(void) const
{
  MSString result("MSTerm(@");
  result+=MSString((unsigned long)this).d2x().lowerCase();
  result+=",_years=";
  result+=MSString(_years);
  result+=",_months=";
  result+=MSString(_months);
  result+=",_days=";
  result+=MSString(_days);
  result+=",_isSet=";
  if (isSet()==MSTrue) result+="MSTrue"; else result+="MSFalse";
  result+=",_type=";
  result+=type().symbolName();
  result+=")";
  return MSString(result);
}

MSString MSTerm::className(void) const
{ return MSString("MSTerm"); }

const MSSymbol& MSTerm::type(void) const
{ return symbol(); }

MSModel *MSTerm::clone(void) const
{ return new MSTerm(*this); }

MSModel *MSTerm::create(void) const
{ return new MSTerm(); }

void MSTerm::assign(const MSModel& aModel_)
{ *this=(MSTerm&)aModel_; }

long MSTerm::compare(const MSModel& aModel_) const
{ return ::compare(*this,(MSTerm&)aModel_); }

MSString MSTerm::asMSF(void) const
{
  MSString aString;
  if (isSet()==MSTrue)
   {
     aString+=MSString(_years);
     aString+='y';
     aString+=MSMSF_SPACE;
     aString+=MSString(_months);
     aString+='m';
     aString+=MSMSF_SPACE;
     aString+=MSString(_days);
     aString+='d';
   }
  return aString;
}

MSError::ErrorStatus MSTerm::setFromMSF(const char *pString_)
{
  int code;
  if (pString_!=0)
   {
     MSString s(pString_);
     s.decodeMSF();
     code=set(s);
   }
  else code=MSError::BadMSFString;
  return (MSError::ErrorStatus)code;
}

const MSSymbol& MSTerm::symbol(void)   
{
  static MSSymbol sym ("MSTerm");
  return sym;
}

MSTerm::MSTerm(void) :
_isSet(MSFalse),_years(0),_months(0),_days(0)
{}

MSTerm::MSTerm(const MSTerm& aTerm_) :
_isSet(aTerm_._isSet),_years(aTerm_._years),_months(aTerm_._months),_days(aTerm_._days)
{}

MSTerm::MSTerm(const MSTerm& a_,const MSTerm& b_) :
_isSet(MSBoolean(a_._isSet&&b_._isSet)),
_years(a_.years()+b_.years()),
_months(a_.months()+b_.months()),
_days(a_.days()+b_.days())
{}

MSTerm::MSTerm(int y_,int m_,int d_) :
_isSet(MSTrue),_years(y_),_months(m_),_days(d_)
{}

MSTerm::MSTerm(const MSDate& a_,const MSDate& b_)
  : _isSet(MSTrue)
{
  MSDate min=a_,max=b_;
  if (a_>b_) { min=b_; max=a_; }

  MSYear year; MSMonth month; MSDay day;
  int minY,maxY,minM,maxM;

  min.asMonthDayYear(month,day,year);
  minY=year; minM=month;

  max.asMonthDayYear(month,day,year);
  maxY=year; maxM=month;

  _years=maxY-minY;
  _months=maxM-minM;
  // earlier month in later year
  if (_months<0) { _years--; _months+=12; }
  _days=0;

  MSDate test=min+*this;

  // last-of-month to last-of-month, or day-of-month to same day-of-month
  if (test==max) return;

  // day-of-month to earlier day of later month
  if (test>max)
   {
     _months--;
     // day-of-month to earlier day of same month in later year
     if (_months<0) { _years--; _months+=12; }
     test=min+*this;
   }

  // test had better be <= max at this point
  _days=max-test;
}

MSTerm& MSTerm::operator=(const MSTerm& aTerm_)
{
  if (this!=&aTerm_)
   {
     _years=aTerm_._years;
     _months=aTerm_._months;
     _days=aTerm_._days;
     _isSet=aTerm_._isSet;
     changed();
   }
  return *this;
}

void MSTerm::getToken(const char *pString_,int *pos_,Token *token_,int *value_)
{
  // Skip white space and commas.
  while (isspace(pString_[*pos_])||pString_[*pos_]==',') (*pos_)++;

  // Are we at the end?
  if (*pos_>=strlen(pString_)) { *token_=END; return; }

  // What is it?
  int start=*pos_;

  // Must have at least one digit.
  if (!isdigit(pString_[*pos_])) { *token_=BAD; return; }

  while (isdigit(pString_[*pos_])) (*pos_)++;

  *value_=atoi(&pString_[start]);

  if (pString_[*pos_]=='y'||pString_[*pos_]=='Y')      { (*pos_)++; *token_=YEARS;  return; }
  else if (pString_[*pos_]=='m'||pString_[*pos_]=='M') { (*pos_)++; *token_=MONTHS; return; }
  else if (pString_[*pos_]=='w'||pString_[*pos_]=='W') { (*pos_)++; *token_=WEEKS;  return; }
  else if (pString_[*pos_]=='d'||pString_[*pos_]=='D') { (*pos_)++; *token_=DAYS;   return; }
  else                                                 { *token_=BAD; return; }
}

void MSTerm::unset(void)
{
  if (isSet()==MSTrue)
   {
     _isSet=MSFalse;     
     _years=0;
     _months=0;
     _days=0;
     changed();
   }
}

MSError::ErrorStatus MSTerm::set(const char *pString_)
{
  // Allow input of Ny Nm Nw Nd in any order,with commas treated
  // as white space,and any of the pieces optional. Weeks are
  // translated into 7 seven days.

  int y,m,d;
  if (sscanf(pString_,"%d,%d,%d",&y,&m,&d)==3)
   {
     _years=y;
     _months=m;
     _days=d;
     _isSet=MSTrue;
     return changed(),MSError::MSSuccess;
   }

  // Okay. Now let's go to the more free-format inputs. We'll read stuff
  // char by char, skipping white space and commas, and look for a match
  // of Ny, Nm, Nw, or Nd in any order. Repeats are considered an error.
  int pos=0;
  int value;
  Token token;
  MSBoolean yearsDone,monthsDone,weeksDone,daysDone;

  yearsDone=monthsDone=weeksDone=daysDone=MSFalse;
  y=m=d=0;

  // The odd looking "for" statement below is due to the fact that the DEC
  // compiler will not allow the private type Token to be a return value.
  for (getToken(pString_,&pos,&token,&value);token!=END;getToken(pString_,&pos,&token,&value))
   {
     if (token==BAD) { return changed(),MSError::BadTerm; }
     else if (token==YEARS)
      {
	if (yearsDone) return changed(),MSError::BadTerm;
	else { y=value; yearsDone=MSTrue; }
      }
     else if (token==MONTHS)
      {
	if (monthsDone) return changed(),MSError::BadTerm;
	else { m=value; monthsDone=MSTrue; }
      }
     else if (token==WEEKS)
      {
	if (weeksDone) return changed(),MSError::BadTerm;
	else { d+=(value*7); weeksDone=MSTrue; }
      }
     else if (token==DAYS)
      {
        if (daysDone) return changed(),MSError::BadTerm;
	else { d+=value; daysDone=MSTrue; }
      }
   }

  _years=y,_months=m,_days=d, _isSet=MSTrue;
  return changed(),MSError::MSSuccess;
}

MSError::ErrorStatus MSTerm::set(int y_,int m_,int d_)
{
  _isSet=MSTrue,_years=y_,_months=m_,_days=d_;
  return changed(),MSError::MSSuccess;
}

MSError::ErrorStatus MSTerm::set(const MSString *sp_)
{ return set(sp_->string()); }
MSError::ErrorStatus MSTerm::set(const MSString& sp_)
{ return set(sp_.string()); }

const char *MSTerm::format(MSString *pString_) const
{ return format(*pString_); }

const char *MSTerm::format(MSString& aString_) const
{
  // This is the default (unformated) version of format: Y,M,D.
  char buf[64];
  sprintf(buf,"%d%c%d%c%d",_years,MSTermDelimiter,_months,MSTermDelimiter,_days);
  aString_=buf;
  return aString_.string();
}

long MSTerm::compare(const MSTerm *pTerm_) const
{
  double diff=this->normalizedYears()-pTerm_->normalizedYears();
  if (diff<0) diff=-1;
  else if (diff>0) diff=1;
  return((long)diff);
}

long MSTerm::compare(const MSTerm& aTerm_) const
{
  double diff=this->normalizedYears()-aTerm_.normalizedYears();
  if (diff<0) diff=-1;
  else if (diff>0) diff=1;
  return((long)diff);
}

double MSTerm::normalizedYears(void) const
{ return double(_days)/DaysInYear+double(_months)/12.0+double(_years); }

const char *MSTerm::format(MSString *pString_,MSTermFormat format_) const
{ return format(*pString_,format_); }

const char *MSTerm::format(MSString& aString_,MSTermFormat format_) const
{
  // This is the formatted format.
  char buf[64];
  switch (format_)
   {
   case YearMonthDay:
     // example: 1y 3d
     // suppresses empty terms
     aString_.removeAll();
     if (_years!=0)
      {
	sprintf(buf,"%dy ",_years);
	aString_+=buf;
      }
     if (_months!=0)
      {
	sprintf(buf,"%dm ",_months);
	aString_+=buf;
      }
     if (_days!=0)
      {
	sprintf(buf,"%dd",_days);
	aString_+=buf;
      }
     break;
   case YearMonthDayPad:
     // example:  1y  0m  3d
     // aligns columns (assumes max of 2 digits)
     sprintf(buf,"%2dy %2dm %2dd",_years,_months,_days);
     aString_=buf;
     break;
   case YearMonthDayNoPad:
     // example: 1y 2m 3d
     sprintf(buf,"%dy %dm %dd",_years,_months,_days);
     aString_=buf;
     break;
   default:
     MSMessageLog::warningMessage("MSTerm: invalid value of format enum\n");
   }
  return aString_.string();
}

const char *MSTerm::format(MSString& aString_,const MSFormat& aFormat_) const
{
  return (aFormat_.formatType()==MSFormat::Term)?
  format(aString_,aFormat_.termFormat()):format(aString_);
}
const char *MSTerm::format(MSString *pString_,const MSFormat& aFormat_) const
{ return format(*pString_,aFormat_); }

MSTerm& MSTerm::operator+=(const MSTerm& aTerm_)
{
  _years+=aTerm_._years;
  _months+=aTerm_._months;
  _days+=aTerm_._days;
  _isSet=MSBoolean(_isSet&&aTerm_._isSet);
  return changed(),*this;
}
