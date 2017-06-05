///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <math.h>
#include <stdio.h>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#if HAVE_SSTREAM && HAVE_IOSFWD
#include <sstream>
# if HAVE_IOSFWD
#include <iosfwd>
# else
# error "Must have iosfwd with sstream"
# endif
#else
#include <strstream.h>
#endif
#include <limits.h>
#include <time.h>
#include <AplusFormatter.H>

const int EnumHashTableSize=128;
const int DefaultOutputPrecision=6;

int        AplusFormatter::_count=0;
AplusHashTable *AplusFormatter::_enumHashTable=0;
AplusHashTable *AplusFormatter::_stringEnumHashTable=0;
struct tm  AplusFormatter::_aTimeStruct;

AplusFormatter::AplusFormatter(void)
{
  if (count()==0) 
   {
     initHashTable();
     initStringHashTable();
   }
  _count++;
}

AplusFormatter::~AplusFormatter(void)
{
  _count--;
  if (count()==0) 
   {
     delete _enumHashTable;
     delete _stringEnumHashTable;
     _enumHashTable=0;
     _stringEnumHashTable=0;
   }
}

AplusFormatter::OutputFormat AplusFormatter::format(const char *str_)
{
  if (str_!=0)
   {
     if (str_[0]=='\0') return NoFormat;
     else return (OutputFormat)(unsigned long)enumHashTable()->lookup(str_);  
   }
  else return BadFormat;
}

char *AplusFormatter::formatEnumString(int fmt_)
{
  return (char *)stringEnumHashTable()->lookup((OutputFormat)fmt_);  
}

AplusFormatter::OutputFormat AplusFormatter::format(A str_)
{
  if (QS(str_)) return format((char *)XS(str_)->n);
  else if (str_->t==Ct) return format((char *)str_->p);
  else if (str_->t==Et&&str_->n>0&&QS(*str_->p)) return format((char *)XS(*str_->p)->n);
  else return BadFormat;
}

A AplusFormatter::formats(void)
{
  return stringEnumHashTable()->listAllEntries();
}

void AplusFormatter::initHashTable(void)
{
  _enumHashTable=new AplusHashTable(EnumHashTableSize);
  enumHashTable()->notFound((unsigned long)BadFormat);
  enumHashTable()->add("none",(void*)NoFormat);
  enumHashTable()->add("float",(void*)Float);
  enumHashTable()->add("fixed",(void*)Fixed);
  enumHashTable()->add("frac",(void*)Fraction);
  enumHashTable()->add("32nd",(void*)Price32);
  enumHashTable()->add("64th",(void*)Price64);
  enumHashTable()->add("128th",(void*)Price128);
  enumHashTable()->add("320th",(void*)Price320);
  enumHashTable()->add("328th",(void*)Price328);
  enumHashTable()->add("hr",(void*)hh);
  enumHashTable()->add("hr24",(void*)hh24);
  enumHashTable()->add("min",(void*)mm);
  enumHashTable()->add("sec",(void*)ss);
  enumHashTable()->add("hrmin",(void*)hhmm);
  enumHashTable()->add("hrmin24",(void*)hhmm24);
  enumHashTable()->add("minsec",(void*)mmss);
  enumHashTable()->add("hrminsec",(void*)hhmmss);
  enumHashTable()->add("hrminsec24",(void*)hhmmss24);

  enumHashTable()->add("day7_u",(void*)D_u);
  enumHashTable()->add("day365_u",(void*)DDD_u);
  enumHashTable()->add("day31_u",(void*)DD_u);
  enumHashTable()->add("m12_u",(void*)MM_u);
  enumHashTable()->add("m_u",(void*)MO_u);
  enumHashTable()->add("y2_u",(void*)Y2_u);
  enumHashTable()->add("y4_u",(void*)Y4_u);
  enumHashTable()->add("mdy_u",(void*)DMDDYYYY_u);
  enumHashTable()->add("dmy2_u",(void*)DDMMYY_u);
  enumHashTable()->add("dmy4_u",(void*)DDMMYYYY_u);
  enumHashTable()->add("mdy2_u",(void*)MMDDYY_u);
  enumHashTable()->add("mdy4_u",(void*)MMDDYYYY_u);

  enumHashTable()->add("day7",(void*)D);
  enumHashTable()->add("day365",(void*)DDD);
  enumHashTable()->add("day31",(void*)DD);
  enumHashTable()->add("m12",(void*)MM);
  enumHashTable()->add("m",(void*)MO);
  enumHashTable()->add("y2",(void*)Y2);
  enumHashTable()->add("y4",(void*)Y4);
  enumHashTable()->add("mdy",(void*)DMDDYYYY);
  enumHashTable()->add("dmy2",(void*)DDMMYY);
  enumHashTable()->add("dmy4",(void*)DDMMYYYY);
  enumHashTable()->add("mdy2",(void*)MMDDYY);
  enumHashTable()->add("mdy4",(void*)MMDDYYYY);
}

void AplusFormatter::initStringHashTable(void)
{
  _stringEnumHashTable=new AplusHashTable(EnumHashTableSize);
  stringEnumHashTable()->notFound((int)0);
  stringEnumHashTable()->add(NoFormat,(void*)"none");
  stringEnumHashTable()->add(Float,(void*)"float");
  stringEnumHashTable()->add(Fixed,(void*)"fixed");
  stringEnumHashTable()->add(Fraction,(void*)"frac");
  stringEnumHashTable()->add(Price32,(void*)"32nd");
  stringEnumHashTable()->add(Price64,(void*)"64th");
  stringEnumHashTable()->add(Price128,(void*)"128th");
  stringEnumHashTable()->add(Price320,(void*)"320th");
  stringEnumHashTable()->add(Price328,(void*)"328th");
  stringEnumHashTable()->add(hh,(void*)"hr");
  stringEnumHashTable()->add(hh24,(void*)"hr24");
  stringEnumHashTable()->add(mm,(void*)"min");
  stringEnumHashTable()->add(ss,(void*)"sec");
  stringEnumHashTable()->add(hhmm,(void*)"hrmin");
  stringEnumHashTable()->add(hhmm24,(void*)"hrmin24");
  stringEnumHashTable()->add(mmss,(void*)"minsec");
  stringEnumHashTable()->add(hhmmss,(void*)"hrminsec");
  stringEnumHashTable()->add(hhmmss24,(void*)"hrminsec24");

  stringEnumHashTable()->add(D_u,(void*)"day7_u");
  stringEnumHashTable()->add(DDD_u,(void*)"day365_u");
  stringEnumHashTable()->add(DD_u,(void*)"day31_u");
  stringEnumHashTable()->add(MM_u,(void*)"m12_u");
  stringEnumHashTable()->add(MO_u,(void*)"m_u");
  stringEnumHashTable()->add(Y2_u,(void*)"y2_u");
  stringEnumHashTable()->add(Y4_u,(void*)"y4_u");
  stringEnumHashTable()->add(DMDDYYYY_u,(void*)"mdy_u");
  stringEnumHashTable()->add(DDMMYY_u,(void*)"dmy2_u");
  stringEnumHashTable()->add(DDMMYYYY_u,(void*)"dmy4_u");
  stringEnumHashTable()->add(MMDDYY_u,(void*)"mdy2_u");
  stringEnumHashTable()->add(MMDDYYYY_u,(void*)"mdy4_u");
  
  stringEnumHashTable()->add(D,(void*)"day7");
  stringEnumHashTable()->add(DDD,(void*)"day365");
  stringEnumHashTable()->add(DD,(void*)"day31");
  stringEnumHashTable()->add(MM,(void*)"m12");
  stringEnumHashTable()->add(MO,(void*)"m");
  stringEnumHashTable()->add(Y2,(void*)"y2");
  stringEnumHashTable()->add(Y4,(void*)"y4");
  stringEnumHashTable()->add(DMDDYYYY,(void*)"mdy");
  stringEnumHashTable()->add(DDMMYY,(void*)"dmy2");
  stringEnumHashTable()->add(DDMMYYYY,(void*)"dmy4");
  stringEnumHashTable()->add(MMDDYY,(void*)"mdy2");
  stringEnumHashTable()->add(MMDDYYYY,(void*)"mdy4");  
}

extern "C" A ep_fmt(unsigned char *,A);
int AplusFormatter::defaultPrecision(void) 
{ return DefaultOutputPrecision; }
A AplusFormatter::fmt(A str_,A data_)      
{ return ep_fmt((unsigned char *)str_->p,data_); }

A AplusFormatter::thorn(A fmt_,A data_)    
{ 
  static char buf[128];
  if (QA(fmt_)&&fmt_->t==It&&fmt_->n==2) 
   {
     P p;p.i=data_->p;
     buf[0]='\0';
     sprintf(buf,"%*.*f",(int)fmt_->p[0],(int)fmt_->p[1],(data_->t==Ft)?p.f[0]:(double)p.i[0]);
     return (A)gsv(0,buf);
   }
  else return aplus_nl;
}

const char *AplusFormatter::formatOutput(OutputFormat fmt_,double v_,int p_,MSBoolean showPos_)
{
  static char microSymbol[] ={' ','1','2','3','+','5','6','7'};
  static const char *day[]  ={"Sun","Mon","Tue","Wed","Thu","Fri","Sat"};
  static const char *month[]={"Jan","Feb","Mar","Apr","May","Jun",
			      "Jul","Aug","Sep","Oct","Nov" ,"Dec"};
#if HAVE_SSTREAM
  static string bufstr;
  static ostringstream os(bufstr,ios::out);
#else
  static char 	      buf[64];
  static ostrstream   os(buf,64,ios::out);
#endif
  static char         plus[1];
  static char         minus[1];
  int    	      i,j;
  int    	      denom,num,whole;
  int    	      found=0;
  int    	      frac,small,tiny;
  time_t	      date;
  double 	      fraction,r;
  double              value=v_;
  int                 precision=p_;
  struct tm 	     *when;
#ifdef APLUS_THREAD_SAFE_FUNCTIONS
  struct tm tmp;
#endif // APLUS_THREAD_SAFE_FUNCTIONS

  AplusFormatter::OutputFormat format=fmt_;
  
  plus[0]=(value>=0.0&&showPos_==MSTrue)?'+':'\0';
  minus[0]=(value<0.0)?'-':'\0';

  os.precision(precision);
  os.setf(ios::right,ios::adjustfield);
  os.seekp(ios::beg);

  if (format>=D&&format<=MMDDYYYY) 
   {
     when=parseDate(value);
     os.fill('0');
   }
  else if (format>=Price32&&format<=Price328&&value<0.0)
   {
     value*=-1;
   }
  else if (format>Price328)
   {
     os.fill('0');
     if (value>=0&&value<LONG_MAX)// Jan 5,2038 max date
      {
	date=long(value);
	when=APLUS_GMTIME(&date,&tmp);
	i=(when->tm_hour>=12)?1:0;
	j=when->tm_hour%12;
      }
     else if (format>D_u) 
      {
	when=julianDay(value);
      }
     else format=Float;
   }
  else os.fill(' ');

  switch (format)
   {
   case BadFormat:
   case NoFormat:
     os.setf(ios::fixed,ios::floatfield);
     os<<value<<ends;
     break;
   case Float:
     os.setf(ios::scientific,ios::floatfield);
     os<<plus<<value<<ends;
     break;
   case Fixed:
     os.setf(ios::fixed,ios::floatfield);
     os<<plus<<value<<ends;
     break;
   case Fraction: 
     whole=(int)(value+.0000001);
     fraction=fabs(value-(double)whole);
     if (fraction<.0000001)
      {
	found=1;
	num=0;
      }
     // Test bases until one which goes in evenly is found
     for (r=2.;!found&&r<=256.;r*=2.)
      {
	num=(int)(fraction*r+.00000001);
	if (fabs(((double)num)-fraction*r)<.0000000001)
	 {
	   denom=(int)r;
	   found=1;
	 }
      }
     for (r=5.;!found&&r<=25.;r+=5.)
      {
	num=(int)(fraction*r+.0000001);
	if (fabs(((double)num)-fraction*r)<.0000000001)
	 {
	   denom=(int)r;
	   found=1;
	 }
      }
     for (r=3.;!found&&r<=9.;r+=3.)
      {
	num=(int)(fraction*r+.0000001);
	if (fabs(((double)num)-fraction*r)<.0000000001)
	 {
	   denom=(int)r;
	   found=1;
	 }
      }
     for (r=10.;!found&&r<=100.;r+=10.)
      {
	num=(int)(fraction*r+.0000001);
	if (fabs(((double)num)-fraction*r)<.0000000001)
	 {
	   denom=(int)r;
	   found=1;
	 }
      }
     if (found)// express as fraction,otherwise decimal
      {
	if (whole==0)
	 {
	   if (num==0)os<<whole<<ends;
	   else os<<num<<"/"<<denom<<ends;
	 }
	else
	 {
	   if (num==0)os<<whole<<ends;
	   else os<<whole<<" "<<num<<"/"<<denom<<ends;
	 }
      }
     else os<<plus<<value<<ends;
     break;
   case Price32:
     whole=(int) fabs(value*32.);
     small=whole%32;
     whole/=32;
     os<<minus<<whole<<"/";
     os.fill('0');
     os.width(2);
     os<<small<<ends;
     break;
   case Price64:
     whole=(int) fabs(value*64.);
     small=whole%64;
     whole/=64;
     os<<minus<<whole<<"\\";
     os.fill('0');
     os.width(2);
     os<<small<<ends;
     break;
   case Price128:
     whole=(int) fabs(value*128.);
     small=whole%128;
     whole/=128;
     os<<minus<<whole<<"=";
     os.fill('0');
     os.width(3);
     os<<small<<ends;
     break;
   case Price320:
     whole=(int) fabs(value*320.);
     small=whole%320;
     whole/=320;
     os<<minus<<whole<<"'";
     os.fill('0');
     os.width(3);
     os<<small<<ends;
     break;
   case Price328:
     whole=(int) fabs(value*32.);
     small=whole%32;
     whole/=32;
     frac=(int) fabs((value-small/32.)*256.);
     tiny=frac%256;
     os<<minus<<whole<<"-";
     os.fill('0');
     os.width(2);
     os<<small;
     os<<tiny<<ends;
     break;
   case hh:
     os.fill('0');
     os.precision(2);
     os<<((j==0)?12:j)<<((i)?"pm":"am")<<ends;
     break;
   case hh24:
     os.fill('0');
     os.width(2);
     os.precision(2);
     os<<when->tm_hour<<ends;
     break;
   case mm:
     os.fill('0');
     os.precision(2);
     os.width(2);
     os<<when->tm_min<<ends;
     break;
   case ss:
     os.fill('0');
     os.width(2);
     os.precision(2);
     os<<when->tm_sec<<ends;
     break;
   case hhmm:
     os.fill('0');
     os.width(2);
     os.precision(2);
     os<<((j==0)?12:j)<<":";
     os.width(2);
     os<<when->tm_min;
     os<<((i==1)?"pm":"am")<<ends;
     break;
   case hhmm24:
     os.fill('0');
     os.width(2);
     os.precision(2);
     os<<when->tm_hour<<":";
     os.width(2);
     os<<when->tm_min<<ends;
     break;
   case mmss:
     os.fill('0');
     os.precision(2);
     os.width(2);
     os<<when->tm_min<<":" ;
     os.width(2);
     os<<when->tm_sec<<ends;
     break;
   case hhmmss:
     os.fill('0');
     os.precision(2);
     os.width(2);
     os<<((j==0)?12:j)<<":" ;
     os.width(2);
     os<<when->tm_min<<":" ;
     os.width(2);
     os<<when->tm_sec;
     os<<((i==1)?"pm":"am")<<ends;
     break;
   case hhmmss24:
     os.fill('0');
     os.width(2);
     os.precision(2);
     os<<when->tm_hour<<":";
     os.width(2);
     os<<when->tm_min<<":";
     os.width(2);
     os<<when->tm_sec<<ends;
     break;
   case D_u:
     os<<day[when->tm_wday]<<ends;
     break;
   case DDD_u:
     os.width(3);
     os<<when->tm_yday+1<<ends;
     break;
   case DD_u:
     os.width(2);
     os<<when->tm_mday<<ends;
     break;
   case MM_u:
     os.width(2);
     os<<when->tm_mon+1<<ends;
     break;
   case MO_u:
     os<<month[when->tm_mon]<<ends;
     break;
   case Y2_u:
     os.width(2);
     os<<(when->tm_year%100)<<ends;
     break;
   case Y4_u:
     os<<when->tm_year+1900<<ends;
     break;
   case DMDDYYYY_u:
     os<<day[when->tm_wday]<<" ";
     os<<month[when->tm_mon]<<" ";
     os.width(2);
     os<<when->tm_mday<<", ";
     os<<when->tm_year+1900<<ends;
     break;
   case DDMMYY_u:
     os.width(2);
     os<<when->tm_mday<<"/";
     os.width(2);
     os<<when->tm_mon+1<<"/";
     os.width(2);
     os<<(when->tm_year%100)<<ends;
     break;
   case DDMMYYYY_u:
     os.width(2);
     os<<when->tm_mday<<"/";
     os.width(2);
     os<<when->tm_mon+1<<"/";
     os<<when->tm_year+1900<<ends;
     break;
   case MMDDYY_u:
     os.width(2);
     os<< when->tm_mon+1<<"/";
     os.width(2);
     os<<when->tm_mday<<"/";
     os.width(2);
     os<<(when->tm_year%100)<<ends;
     break;
   case MMDDYYYY_u:
     os.width(2);
     os<< when->tm_mon+1<<"/";
     os.width(2);
     os<<when->tm_mday<<"/";
     os<<when->tm_year+1900<<ends;
     break;
   case D:
     os<<day[(when->tm_wday>=0&&when->tm_wday<7)?when->tm_wday:0]<<ends;
     break;
   case DDD:
     os.width(3);
     os<<when->tm_yday<<ends;
     break;
   case DD:
     os.width(2);
     os<<when->tm_mday<<ends;
     break;
   case MM:
     os.width(2);
     os<<when->tm_mon+1<<ends;
     break;
   case MO:
     os<<month[(when->tm_mon>=0&&when->tm_mon<12)?when->tm_mon:0]<<ends;
     break;
   case Y2:
     os.width(2);
     os<<(when->tm_year%100)<<ends; 
     break;
   case Y4:
     os.width(4);
     os<<when->tm_year<<ends;
     break;
   case DMDDYYYY:
     os<<day[(when->tm_wday>=0&&when->tm_wday<7)?when->tm_wday:0]<<" ";
     os<<month[(when->tm_mon>=0&&when->tm_mon<12)?when->tm_mon:0]<<" ";
     os.width(2);
     os<<when->tm_mday<<", ";
     os.width(4);
     os<<when->tm_year<<ends;
     break;
   case DDMMYY:
     os.width(2);
     os<<when->tm_mday<<"/";
     os.width(2);
     os<<when->tm_mon+1<<"/";
     os.width(2);
       os<< (when->tm_year%100)<<ends;
     break;
   case DDMMYYYY:
     os.width(2);   
     os<<when->tm_mday<<"/";
     os.width(2);
     os<<when->tm_mon+1<<"/";
     os.width(4);
     os<<when->tm_year<<ends;
     break;
   case MMDDYY:
     os.width(2);   
     os<<when->tm_mon+1<<"/";
     os.width(2);
     os<<when->tm_mday<<"/";
     os.width(2);
     os<< (when->tm_year%100)<<ends;
     break;
   case MMDDYYYY:
     os.width(2);   
     os<<when->tm_mon+1<<"/";
     os.width(2);
     os<<when->tm_mday<<"/";
     os.width(4);
     os<<when->tm_year<<ends;
     break;
   }
#if HAVE_SSTREAM
  return bufstr.data();
#else
  return buf;
#endif
}


A AplusFormatter::formatOutput(A fmt_,A data_)
{
  double        value;
  OutputFormat  format;
  int           precision;
  
  if (QA(fmt_)&&fmt_->t==It&&fmt_->n==2) 
   {
     format=(OutputFormat)fmt_->p[0];
     precision=(int)fmt_->p[1];
   }
  else return aplus_nl;
  if (QA(data_)&&data_->t<=Ft) 
   {
     P p;p.i=data_->p;
     value=(data_->t==Ft)?p.f[0]:(double)p.i[0];
   }
  else return aplus_nl;
  return (A)gsv(0,(char *)formatOutput(format,value,precision,MSFalse));
}

A AplusFormatter::sfmt(A fmt_,A data_)
{
  if (QA(fmt_))
   {
     if (fmt_->t==Ct) return fmt(fmt_,data_);
     else if (fmt_->t==It||fmt_->t==Ft)
      {
        P p; p.i=fmt_->p;
        double dfmt=(fmt_->t==Ft)?(double)p.f[0]:(double)p.i[0];
        int ifmt=(int)(10.0*dfmt);
        A fmt=(A)gv(It,2);
        int width=(int)floor(dfmt);
	int precision=(ifmt-(width*10));
        fmt->p[0]=(I)width;
	fmt->p[1]=(I)precision;
        A r=(data_->t==It||data_->t==Ft)?thorn(fmt,data_):0;
        dc(fmt);
        if (r==0) q=6;
        return r;
      }
     else if (fmt_->t==Et&&fmt_->n>0)
      {
        P p; p.i=fmt_->p;
	if (QS(p.a[0])||
	    (QA(p.a[0])&&(p.a[0]->t==Et&&p.a[0]->n>0&&QS(*p.a[0]->p))))
	{
	  A r=0;
	  if (QA(data_)&&data_->t<=Ft) 
	  {
	    P pp;pp.i=data_->p;
	    double value=(data_->t==Ft)?pp.f[0]:(double)pp.i[0];
	    int pr=(fmt_->n==2&&QA(p.a[1])&&It==p.a[1]->t)?
	      (int)p.a[1]->p[0]:defaultPrecision();
	      r=(A)gsv(0,(char *)formatOutput(format(p.a[0]),value,pr,MSFalse));
	  }
	  if (r==0) q=9; // domain error
	    return r;
	}
      }
   }
  q=6; // type error
  return 0;
}

struct tm *AplusFormatter::parseDate(double ymd_)
{
  static int mdays[]={31,28,31,30,31,30,31,31,30,31,30,31};
  MSBoolean    leapyr=MSFalse;
  int yr=(int)(ymd_*0.0001);
  double temp=(ymd_-yr*10000);
  int mmx=(int)(temp*0.01);
  int yday=0;
  int dd=(int)((mmx>=1 && mmx <=12)?temp-(mmx*100):0);
  if ((yr%4==0&&yr%100!=0)||yr%400==0) leapyr=MSTrue;
  int century, wday;
  if (mmx >= 1 &&  mmx <= 12 &&
      dd >= 1 && dd <= ((leapyr==MSTrue && mmx==2) ? 29 : mdays[mmx-1]) )
   {
     for (int i=0;i<mmx-1;i++) yday+=mdays[i];
     if (leapyr==MSTrue&&mmx>2) yday++;
     yday+=dd;
     century=2001>yr?0:(yr-2001)/100;
     wday=(yday+(yr-1900)+((yr-1901)/4)-(century-century/4))%7;
   }
  else  // An invalid date will return 00/00/0000
   {
     yr = mmx = dd = yday = wday = 0;
   }
  
  _aTimeStruct.tm_sec=0;
  _aTimeStruct.tm_min=0;
  _aTimeStruct.tm_hour=0;
  _aTimeStruct.tm_mday=dd;
  _aTimeStruct.tm_mon=mmx-1;
  _aTimeStruct.tm_yday=yday;
  _aTimeStruct.tm_wday=wday;
  _aTimeStruct.tm_year=yr;

  return &_aTimeStruct;
}

struct tm *AplusFormatter::julianDay(double value_)
{
  _aTimeStruct.tm_sec=0;
  _aTimeStruct.tm_min=0;
  _aTimeStruct.tm_hour=0;
  int offset=2440588;  // offset from sept 14 1752
  // convert value (unix sec) to julian day
  // Convert Gregorian calendar date to the corresponding Julian day
  // number j.  Algorithm 199 from Communications of the ACM, Volume 6, No.
  // 8, (Aug. 1963), p. 444.  Gregorian calendar started on Sep. 14, 1752.
  unsigned long jday= (unsigned long)(offset+value_/86400-1721119);
  int wday=(int)((((jday+3)%7)+6)%7);
  int yr=(int)(((jday<<2)-1)/146097);
  jday=(jday<<2)-1-146097*yr;
  unsigned long d=(jday>>2);
  jday=((d<<2)+3)/1461;
  d=(d<<2)+3-1461*jday;
  d=(d+4)>>2;
  int month=(int)(5*d-3)/153;
  d=5*d-3-153*month;
  yr=(int)(100*yr+jday);
  if (month<10) month+=3;
  else { month-=9; yr++; }
  _aTimeStruct.tm_wday=wday==7?0:wday;
  _aTimeStruct.tm_mday=(int)((d+5)/5);
  _aTimeStruct.tm_mon=month-1;
  _aTimeStruct.tm_year=yr-1900;
  return &_aTimeStruct;
}
