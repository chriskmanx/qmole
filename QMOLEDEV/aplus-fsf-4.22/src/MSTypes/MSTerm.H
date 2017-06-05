#ifndef MSTermHEADER
#define MSTermHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSError.H>
#include <MSTypes/MSScalarModel.H>
#include <MSTypes/MSString.H>
#include <MSTypes/MSSymbol.H>

/*---------------------------------------------------------------------------------|
|    DESCRIPTION                                                              	   | 
|                                                                             	   | 
|    MSTerm represents a span of time, given in days, calendar months, and    	   | 
|    calendar years. It is primarily used to calculate what date a term       	   | 
|    would end on, given a specific starting date. Given a MSDate which is    	   | 
|    set to the starting date, you can simply add the MSTerm to it (using     	   | 
|    the MSDate::operator+(const MSTerm&) function) and the result will       	   | 
|    be a MSDate set to the date the term ends on. We generally describe      	   | 
|    how MSTerms work by describing what MSDate is produced when they are     	   | 
|    added to a given MSDate.                                                 	   | 
|                                                                             	   | 
|    In general, the number of days in a MSTerm is not a constant because     	   | 
|    the number of days in a month (or days in a year) in not a constant;     	   | 
|    the length of a term depends on the date that you start from.  In the    	   | 
|    common case, if you add 1 month to a date you will end on the same day   	   | 
|    of the month in the next month, e.g. Jan. 15 plus 1 month yields Feb.    	   | 
|    15 (for a term of 31 days), Feb. 15 plus 1 month yields Mar. 15 (for a   	   | 
|    term of 28 days, or 29 days in a leap year).                             	   | 
|										   | 
|    There are some special cases, though. If you start on the last day of    	   | 
|    the month and add some number of months (and no days), you always will   	   | 
|    end on the last day of a month. For example, one month from March 31     	   | 
|    is April 30 (30 days), while one month from April 30 is May 31 (31       	   | 
|    days). Also, adding a month to a date will never actually skip a         	   | 
|    month. For example, if you add one month to Jan. 29, you will not end    	   | 
|    on a date sometime in March, but rather you will end on Feb. 28 (or           | 
|    Feb. 29 in a leap year).                                                 	   | 
|    										   | 
|    Because of the special way months (and years) are handled with a         	   | 
|    MSTerm, you need to be careful about defining terms; a term of 90        	   | 
|    days is always 90 days, while a term of 3 calendar months can be         	   | 
|    anywhere from 89 to 92 days.                                             	   | 
|										   | 
|    Terms which have 0 years and 0 months do not do anything special with    	   | 
|    the days. Terms are, by convention, never negative periods of time,      	   | 
|    but you can subtract them from dates (see "Warnings"). There is no       	   | 
|    checking on the days, months, or years to see if they are negative,      	   | 
|    so be careful when setting them. Adding terms toformather produces terms 	   | 
|    which are the sums of their periods:        |				   | 
|										   | 
|    1year 9months 5days + 1year 6months 28days =  2years 15months 33days     	   | 
|										   | 
|    Note that MSTerms are not "inclusive" when used with MSDates; when       	   | 
|    you add one month to Jan. 1 you format Feb. 1, not Jan. 31. Also, because	   | 
|    of the special handling of months, (date + term1) + term2 is not  		   | 
|    necessarily the same as date + (term1 + term2). You really need to		   | 
|    think carefully about what you want to do.					   | 
|										   | 
|    Two MSTerms are == only if they have identical years, months, and             |
|    days. In general, you should not be comparing terms. To see which of	   |
|    two terms is longer, pick a starting date, add each of the terms to	   |
|    it, and see which produces the latest result (see which MSDate is >	   |
|    the other). The compare() function is provided for sorting terms by	   |
|    length, and is approximately correct (it assumes equal length months	   |
|    and equal length years); it is fine for ordering lists of similar		   |
|    kinds of terms, but it should not be relied on for critical date		   |
|    calculations.								   |
|										   |
|----------------------------------------------------------------------------------|
|    OTHER WAYS 								   |
|										   |
|    There are other ways to represent time periods, and you need to think	   |
|    about which way to do it.:							   |
|										   |
|    MSDate gives you a time span in days when you subtract one date from	   |
|    another; this is the best method for representing a definite period of	   |
|    time and should always be used if possible.                                   |
|										   |
|    The MSNormalizedYears object represents a time span based on a		   |
|    365.25-day year and is only for use with the analytics library;		   |
|    applications should always use dates, days, and terms. If you want the	   |
|    difference between two random dates, in years (for analytics), use the	   |
|    MSNormalizedYears(const MSDate&, const MSDate&) constructor, which		   |
|    gives you the absolute (never negative) difference between the two		   |
|    dates, in years.  See MSNormalizedYears for more details about		   |
|    normalized years.								   |
|										   |
|    Use MSTerms only when the real-world specification is given in months	   |
|    or years; for example, to find out what date is 3 months from today,	   |
|    create a term of 3 months and add it to today.				   |
|										   |
|----------------------------------------------------------------------------------|
|    EXAMPLE DATE CALCULATIONS							   |
|										   |
|    MSDate + MSTerm gives MSDate: 1/31/90 + 2yrs1mo0day gives 2/29/92             |
|										   |
|    MSDate + int (days) gives MSDate: 1/31/92 + 30 give 3/1/92			   |
|										   |
|    To convert MSNormalizedYears to a number of days use MSNormalizedYears.days() |
|										   |
|    MSDate + MSNormalizedYears == MSDate + MSNormalizedYears.days() gives MSDate  |
|										   |
|    MSNormalizedYears between two dates: MSNormalizedYears(MSDate, MSDate)	   |
|										   |
|    Days between two dates: MSDate - MSDate (can be negative);			   |
|										   |
|    Term of given number of days: MSTerm(0,0,days) 				   |
|										   |
|----------------------------------------------------------------------------------|
|    CONVERTING A MSTerm TO SOMETHING ELSE					   |
|										   |
|    You cannot directly convert a MSTerm to any other time span, since		   |
|    terms are varying lengths of time based on the starting date. If you          |
|    have the real starting date, use it; otherwise, you can format an		   |
|    approximate answer by using today as a starting date:			   |
|										   |
|    Days in term: ((MSDate + MSTerm) - MSDate). 				   |
|										   |
|    Normalized years in term: MSNormalizedYears((MSDate + MSTerm), MSDate). 	   |
|										   |
|----------------------------------------------------------------------------------|
|    TERM BETWEEN TWO DATES							   |
|										   |
|    You usually don't want to represent the time between two given dates	   |
|    as a term. What you usually want is to determine if some date is more	   |
|    than some term away from some other date. Use (MSDate + MSTerm) and	   |
|    compare to other MSDate.							   |
|										   |
|    Example, is date_x more than 2 years after date_y? 			   |
|										   |
|	 date_x > (MSTerm(2,0,0) + date_y)                                         |
|										   |
|    If you have to, you can construct a MSTerm from two MSDates, which		   |
|    will give you a term that, when added to the earlier date, will give	   |
|    you the later date, and that has years >= 0, 12 > months >= 0, 30 >=	   |
|    days >= 0. The only reason to do this is when you need to display a	   |
|    time span as a term; otherwise, it is much better to use the number of	   |
|    days between the two dates. As discussed under "Warnings," there can	   |
|    be several different ways of describing the term between two dates,	   |
|    so this method of constructing a MSTerm is somewhat unreliable. The	   |
|    number of days between two dates is unique and always accurate.		   |
|										   |
|----------------------------------------------------------------------------------|
|			       WARNINGS 					   |
|----------------------------------------------------------------------------------|
|										   |
|    There are fundamental asymmetries in calendar arithmetic. For example,	   |
|    3/30/90 + 1 month is 4/30/90, but 3/31/90 + 1 month is 4/30/90, so		   |
|    what is 4/30/90 - 1 month? MSDate says that 4/30/90 - 1 month is              |
|    3/31/90. Similarly, there can be many different terms that, when added	   |
|    to a date, give the same final date. Some of these will, when		   |
|    subtracted from the final date, give the original start date, and some	   |
|    will not. There is no way to tell which of the terms you really want	   |
|    when constructing a term from two dates, so MSTerm cannot guarantee	   |
|    that MSTerm(date, date + term) will give you back your original term.	   |
|    And since two terms are equal only if they have identical years,		   |
|    months, and days, it is common for MSTerm(date, date + term) != term.	   |
|										   |
|    When constructing a MSTerm from two dates, MSTerm tries to minimize	   |
|    the number of days (as opposed to months) in the term; given a choice	   |
|    between a term of 30 days and a term of 1 month, MSTerm will create a	   |
|    term of 1 month. MSTerm promises that min(date1,date2) +			   |
|    MSTerm(date1, date2) is always equal to max(date1,date2), but will		   |
|    not promise that max(date1,date2) - MSTerm(date2, date2) is equal to	   |
|    min(date1,date2). Going back to our example, MSTerm(3/30/90, 4/30/90)	   |
|    is 1 month, so the addition works but the subtraction fails. We could	   |
|    insure that both addition and subtraction would work for any date             |
|    pair, but this would mean that the term between 3/30/90 and 4/30/90 is	   |
|    31 days, while the term between 3/29/90 and 4/29/90 is one month. We	   |
|    have chosen one irregularity over the other.				   |
|										   |
|    Be careful when using MSNormalizedYears. Just calculate them and pass	   |
|    them to analytics.  Don't try to use them on your own; use the real	   |
|    number of days instead.							   |
|										   |
|----------------------------------------------------------------------------------|
|    WHERE ARE THE OPERATORS?							   |
|										   |
|    The actual MSDate + MSTerm operators are declared in <MSTypes/MSDate.H> and   |
|    are documented (briefly) in the MSDate documentation. The			   |
|    description of what they actually do is in the paragraphs above.		   |
|										   |
|---------------------------------------------------------------------------------*/

class MSDate;	
class MSFormat;

// The "smart" date + term operator is in MSDate, becuase MSDate
// has all the tables of calendar data.
class MSTypesExport MSTerm: public MSScalarModel
{
public:

  enum MSTermFormat 
  {	
    // YearMonthDayPad pads y,m,d if needed so they line
    // up, YearMonthDayNoPad uses no extra padding
    YearsMonthsDays=500,                    // Format as "11y 3d"
    YearsMonthsDaysPad=501,                 // Format as "11y  0m  0w  3d"
    YearsMonthsDaysNoPad=502,               // Format as "11y 0m 0w 3d"
    // for backward compatibility
    YearMonthDay=YearsMonthsDays,           // Format as "11y 3d"
    YearMonthDayPad=YearsMonthsDaysPad,     // Format as "11y  0m  0w  3d"
    YearMonthDayNoPad=YearsMonthsDaysNoPad  // Format as "11y 0m 0w 3d"
  };
  
  MSTerm(void);
  MSTerm(const MSTerm&);
  MSTerm(int y,int m=0,int d=0);
  MSTerm(const char *s)	{set(s); }
  // Term between two dates.  Use only for display,otherwise use
  // number of days.  Order of dates not important.  See Warning above.
  MSTerm(const MSDate&,const MSDate&);
  
  virtual MSString asString(void) const;
  virtual MSString asDebugInfo(void) const;
  virtual MSString asMSF(void) const;
  virtual MSString className(void) const;
  virtual const MSSymbol& type(void) const;
  virtual MSModel *clone(void) const;
  virtual MSModel *create(void) const;
  virtual void assign(const MSModel&);
  virtual long compare(const MSModel&) const;
  virtual MSError::ErrorStatus set(const char *);
  virtual MSError::ErrorStatus setFromMSF(const char *);
  static const MSSymbol& symbol(void);

  MSTerm& operator=(const MSTerm&);

  MSError::ErrorStatus set(const MSString *);
  MSError::ErrorStatus set(const MSString &);
  MSError::ErrorStatus set(int y,int m,int d);
  const char *format(MSString *) const;
  const char *format(MSString &) const;
  const char *format(MSString *,MSTermFormat) const;
  const char *format(MSString &,MSTermFormat) const;
  const char *format(MSString*,const MSFormat&) const;
  const char *format(MSString&,const MSFormat&) const;

  long compare(const MSTerm *) const;
  long compare(const MSTerm &) const;
  
  int years(void)       const { return _years; }
  int months(void)      const { return _months; }
  int days(void)        const { return _days; }
  MSBoolean isSet(void) const { return _isSet; }
  void unset(void);
  
  MSTerm operator+(const MSTerm&) const;
  MSTerm& operator+=(const MSTerm&);
  
  MSBoolean operator==(const MSTerm&) const;
  MSBoolean operator!=(const MSTerm&) const;
  MSBoolean operator<(const MSTerm&) const;
  MSBoolean operator<=(const MSTerm&) const;
  MSBoolean operator>(const MSTerm&) const;
  MSBoolean operator>=(const MSTerm&) const;
  
  friend MSTypesExport ostream& operator<<(ostream&,const MSTerm&);
  
private:
  int             _years;
  int             _months;
  int             _days;
  MSBoolean       _isSet;

  double normalizedYears(void) const;
  
  MSTerm(const MSTerm&,const MSTerm&); // use to add two terms

  // The following is used for parsing input.
  enum Token {BAD,YEARS,MONTHS,WEEKS,DAYS,END};

  unsigned totalDays(void);
  static void getToken(const char*,int*,Token*,int*);
};

inline long compare(const MSTerm& aTerm_,const MSTerm& bTerm_)
{ return aTerm_.compare(bTerm_); }

#ifndef MS_NO_INLINES
#ifndef MSTermINLINES
#include <MSTypes/MSTermInlines.C>
#endif
#endif

#endif
