///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSLabelFormat.H>
#include <MSGUI/MSLabelOut.H>

static const double protStdUp[]    ={ 10.,1.,2.,2.5,5.,0. };
static const double protStdDn[]    ={ .1,.5,.25,.2,.1,0. };
static const double protFracDn[]   ={ .5,1.,0. };
static const double protPrice32[]  ={ 0.,.03125,.0625,.125,.25,.5,0. };
static const double protPrice64[]  ={ 0.,.015625,.03125,.0625,.125,.25,.5,0. };
static const double protPrice128[] ={ 0.,.0078125,.015625,.03125,.0625,.125,.25,.5,0. };
static const double protPrice320[] ={ 0.,.001953125,.00390625,.015625,.03125,.0625,
				      .125,.25,.5,0. };
static const double protTime[]     ={ 0.0,1.,2.,5.,10.,15.,30.,60. ,120.,
				      300.,600.,900.,1800.,3600.,7200.,
				      10800.,14400.,21600.,43200.,0.0 };
static const double protDate[]     ={ 0.0,86400.,172800.,259200.,345600.,604800.,
				      1209600.,2678400.,5356800.,8035200.,
				      10713600.,16070400.,31622400.,0.0 };
static const double *protocolFloat[]    ={ protStdUp,protStdDn,0 };
static const double *protocolFrac[]     ={ protStdUp,protStdDn,protFracDn,0 };
static const double *protocolPrice32[]  ={ protStdUp,protPrice32,0 };
static const double *protocolPrice64[]  ={ protStdUp,protPrice64,0 };
static const double *protocolPrice128[] ={ protStdUp,protPrice128,0 };
static const double *protocolPrice320[] ={ protStdUp,protPrice320,0 };
static const double *protocolTime[]     ={ protTime,protDate,0 };
static const double *protocolDate[]     ={ protTime,protDate,0 };

MSLabelFormat::MSLabelFormat(void) {}
MSLabelFormat::~MSLabelFormat(void) {}

unsigned long MSLabelFormat::nextWeekday(unsigned long now_)
{
  const int  secondsPerDay=86400;
  struct tm *tm;
#ifdef MS_THREAD_SAFE_FUNCTIONS
  struct tm tmStruct;
#endif
  time_t now=(time_t)now_;

  tm=MS_GMTIME(&now,&tmStruct);
  if(tm->tm_wday==0)now+=secondsPerDay;         // Sunday so move up one day
  else if(tm->tm_wday==6)now+=2*secondsPerDay;  // Saturday so move up two days
  now_=(unsigned long)now;
  
  return(now_);
}

double MSLabelFormat::snapTime(double start_,double increment_)
{
  unsigned long newTime=(unsigned long)increment_;
  if (fabs(start_)<LONG_MAX)
   {
     const int   		secondsPerDay=86400;
     const unsigned long 	zeroYear    =31622400; // January 1st 
     const unsigned long 	halfYear    =16070400; // half year 
     const unsigned long 	thirdYear   =10713600; // third year 
     const unsigned long 	quarterYear =8035200;  // quarter year 
     const unsigned long 	sixthYear   =5356800;  // sixth year 
     const unsigned long 	month       =2678400;  // Month 
     const unsigned long 	monday      =604800;   // Monday 
     time_t 			start       =(long)start_;
     struct tm 		*tm;
#ifdef MS_THREAD_SAFE_FUNCTIONS
     struct tm tmStruct;
#endif
     
     tm=MS_GMTIME(&start,&tmStruct);
     tm->tm_sec=0;       // Midnight
     tm->tm_min=0;
     tm->tm_hour=0;

     if (increment_>=zeroYear)// Snap to January 1st 
      {
	tm->tm_mday=1;
	tm->tm_mon=0;
	newTime=nextWeekday(timegm(tm));
      }
     else if (increment_>=halfYear) // Snap to half year 
      {
	tm->tm_mday=1;
	tm->tm_mon -=tm->tm_mon % 6;
	newTime=nextWeekday(timegm(tm));
      }
     else if (increment_>=thirdYear) // Snap to third year 
      {
	tm->tm_mday=1;
	tm->tm_mon -=tm->tm_mon % 4;
	newTime=nextWeekday(timegm(tm));
      }
     else if (increment_>=quarterYear) // Snap to quarter year 
      {
	tm->tm_mday=1;
	tm->tm_mon -=tm->tm_mon % 3;
	newTime=nextWeekday(timegm(tm));
      }
     else if (increment_>=sixthYear) // Snap to sixth year 
      {
	tm->tm_mday=1;
	tm->tm_mon -=tm->tm_mon % 2;
	newTime=nextWeekday(timegm(tm));
      }
     else if (increment_>=month)// Snap to Month 
      {
	tm->tm_mday=1;
	newTime=nextWeekday(timegm (tm));
      }
     else if (increment_>=monday)// Snap to Monday 
      {
	newTime=start-(secondsPerDay*tm->tm_wday)-
	start % secondsPerDay+secondsPerDay;
      }
     else
      {
	newTime=start-start%(long)increment_;  // Use the fallback 
      }
   }
  return (double)newTime;
}

const double **MSLabelFormat::findProtocol(const MSLabelOut& out_)
{
  const double   **protocol=protocolFloat;

  if (out_.format().formatType()==MSFormat::Time) protocol=protocolTime;
  else if (out_.format().formatType()==MSFormat::Money)
   {
     switch(out_.format().moneyFormat())
      {
      case MSMoney::ThirtySeconds:
	protocol=protocolPrice32;
	break;
	
      case MSMoney::SixtyForths:
	protocol=protocolPrice64;
	break;
	
      case MSMoney::OneTwentyEights:
	protocol=protocolPrice128;
	break;
	
//   case Price320:
//     protocol=protocolPrice320;
//     break;
	
//   case Price328:
//     protocol=protocolPrice320;
//     break;
      }
   }
  return protocol;
}

double MSLabelFormat::snapNumber(double in_,const MSLabelOut& out_)
{
  int 			i,j,p;
  double 		out  =-1.;
  double 		best =-1.;
  double 		factor=1.;
  double 		sign =1.;
  const double	      **protocol=findProtocol(out_);
  
  if (protocol!=0)
   {
     if (in_<0.0)
      {
	sign=-1.;
	in_=-in_;
      }
     // Loop through the various protocols and find the smallest number each finds
     for (p=0; protocol[p]!=0; p++)
      {
	out=-1.;
	factor=1.;
	// Handle a finite series protocol
	if (protocol[p][0]<=0.0)
	 {
	   for (i=1; protocol[p][i]>0.; i++)
	    {
	      if (protocol[p][i]*factor>=in_)
	       {
		 out=protocol[p][i]*factor;
		 break;
	       }
	    }
	 }
	// Handle a decreasing series protocol
	else if (protocol[p][0]<=1.0)
	 {
	   j=1;
	   do
	    {
	      for (i=1; protocol[p][i]>0.; i++)
	       {
		 if (protocol[p][i]*factor>=in_) out=protocol[p][i]*factor;
		 else
		  {
		    j=0;
		    break;
		  }
	       }
	      factor*=protocol[p][0];
	    } while (j!=0);
	 }
	// Handle an increasing series protocol
	else do
	 {
	   for (i=1; protocol[p][i]>0.; i++)
	    {
	      if (protocol[p][i]*factor>=in_)
	       {
		 out=protocol[p][i]*factor;
		 break;
	       }
	    }
	   factor*=protocol[p][0];
	 } while (out<=0.);
	if (best<=0.||(out>=0.&&out<best)) best=out;
      }
     if (best>0.0) return (best*sign);
     else return (in_*sign);  // Couldn't find a nice values so return the old one 
   }
  return in_;
}

double MSLabelFormat::snipNumber(double in_,const MSLabelOut& out_)
{
  int 		    i,j,p;
  double 	    out=-1.;
  double 	    best=-1.;
  double 	    factor=1.;
  double 	    last;
  double 	    sign=1.;
  const double 	  **protocol=findProtocol(out_);

  if (protocol!=0)
   {
     if (in_<0.0)
      {
	sign=-1.;
	in_=-in_;
      }
     // Loop through the various protocols and find the largest number each finds
     for (p=0; protocol[p]!=0; p++)
      {
	out=-1.0;
	factor=1.0;
	// Handle a finite series protocol
	if (protocol[p][0]<=0.0)
	 {
	   for (i=1; protocol[p][i]>0.0; i++)
	    {
	      if (protocol[p][i]==in_)
	       {
		 out=protocol[p][i];
		 break;
	       }
	      else if (protocol[p][i]>in_)
	       {
		 if (i>1)out=protocol[p][i-1];
		 break;
	       }
	    }
	 }
	// Handle a decreasing series protocol
	else if (protocol[p][0]<=1.0)
	 {
	   j=1;
	   do
	    {
	      for (i=1; protocol[p][i]>0.; i++)
	       {
		 if (protocol[p][i]*factor<=in_)
		  {
		    out=protocol[p][i]*factor;
		    j=0;
		    break;
		  }
	       }
	      factor*=protocol[p][0];
	    } while (j);
	 }
	// Handle an increasing series protocol
	else
	 {
	   last=0.;
	   do
	    {
	      for (i=1; protocol[p][i]>0.; i++)
	       {
		 if (protocol[p][i]*factor==in_)
		  {
		    out=protocol[p][i]*factor;
		    break;
		  }
		 else if (protocol[p][i]*factor>in_)
		  {
		    out=last;
		    break;
		  }
		 last=(double)protocol[p][i]*factor;
	       }
	      factor*=protocol[p][0];
	    } while (out<0.);
	 }
	if (best<=0.0||(out>0.0&&out>best))
	best=out;
      }
     if (best>0.0) return(best*sign);
   }
  return in_;
}

double MSLabelFormat::minimumNumber(const MSLabelOut& out_)
{
  int 			i;
  int 			found=0;
  double 		value=0.;
  const double	      **protocol=findProtocol(out_);

  if (protocol==0)return 0.0;
  for (i=0; protocol[i]!=0; i++)
   {
     if (protocol[i][0]<=0.0||protocol[i][0]>1.0)
      {
	if (!found||protocol[i][1]<value)
	 {
	   found=1;
	   value=protocol[i][1];
	 }
      }
     else if (protocol[i][0]<1.)return(0.0);
   }
  if (found)return(value);
  else return(0.0);
}

