#ifndef MSCalendarHEADER
#define MSCalendarHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSResourceCodeSet.H>
#include <MSTypes/MSHoliday.H>
#include <MSTypes/MSIHashKeySet.H>
#include <MSTypes/MSDefines.H>

#if defined (MS_PREDECLARE_TEMPLATE_OP_FUNCTIONS)
class MSResourceHolidaySet;
class MSResourceCodeDesc;
inline MSResourceCode const& key(MSResourceHolidaySet const& holidaySet_);
inline MSResourceCode const& key(MSResourceCode const& resourceCode_); 
inline MSResourceCode const& key(MSResourceCode & resourceCode_);
inline MSResourceCode const& key(MSResourceCodeDesc const& rcd_);
#endif

DECLARE_ELEMENT_FOR_OPS_TYPE(MSHoliday)

class MSTypesExport MSResourceHolidaySet : public MSIHashKeySet<MSHoliday,MSDate> 
{
public:
  MSResourceHolidaySet() {}
  MSResourceHolidaySet(const MSResourceCode&);
  MSResourceHolidaySet(const MSResourceHolidaySet&);
  MSResourceHolidaySet& operator=(const MSResourceHolidaySet&);
  friend MSTypesExport ostream& operator<<(ostream&,const MSResourceHolidaySet&);
  
  const MSResourceCode& resourceCode(void) const;

private:
  MSResourceCode _resourceCode;
};

class MSTypesExport MSResourceCodeDesc
{
public:
  MSResourceCodeDesc() : _index(0) {}
  MSResourceCodeDesc(const MSResourceCode &,const MSString &);
  MSResourceCodeDesc(const MSResourceCodeDesc &);
  MSResourceCodeDesc& operator=(const MSResourceCodeDesc &);
  MSBoolean operator==(const MSResourceCodeDesc &) const;
  ~MSResourceCodeDesc() {};
  
  unsigned int index()                  const {return _index;}
  const MSResourceCode & resourceCode() const {return _resourceCode;}
  const MSString & description()        const {return _description;}
  void dump(void) const;
private:
  MSResourceCode        _resourceCode;
  unsigned int          _index;    // from 1 to 52
  MSString              _description;
  static unsigned int   _count;
};

DECLARE_ELEMENT_FOR_OPS_TYPE(MSResourceCodeDesc)
DECLARE_ELEMENT_FOR_OPS_TYPE(MSResourceCodeHolidaySet)

typedef MSIHashKeySet<MSResourceHolidaySet,MSResourceCode> MSHolidaySet;
typedef MSIHashKeySet<MSResourceCodeDesc,MSResourceCode> MSResourceCodeDescriptionSet;
typedef MSIHashKeySet<MSResourceCodeDesc,MSResourceCode>::Cursor MSResourceCodeDescriptionSetCursor;

class MSTypesExport MSCalendar 
{
public:
  MSCalendar(void);
  ~MSCalendar(void);

  // returns list of holiday dates for a resource.
  static MSResourceHolidaySet holidayDateList(const MSResourceCode& resourceCode_) ;
  // returns true if date is a holiday for specific resource - if no
  // resource is supplied,the user's local country could be used.
  static MSBoolean isHoliday(const MSDate& aDate_="",const MSResourceCode& resourceCode_="");
  static MSBoolean isHoliday(const MSDate& aDate_,const MSResourceCodeSet& resourceCodeSet_);

  // checks if valid trade date for supplied date and resource
  static MSBoolean isValidTradeDate(const MSDate& aDate_=MSDate::today(),
				    const MSResourceCode& resourceCode_="");
  static MSBoolean isValidTradeDate(const MSDate& aDate_,const MSResourceCodeSet & resourceCodeSet_);

  // return next/prev valid business date from the passed date argument
  // and resource codes
  static MSDate nextTradeDate(const MSDate& aDate_=MSDate::today(),const MSResourceCode& resourceCode_="");
  static MSDate nextTradeDate(const MSDate& aDate_,const MSResourceCodeSet& resourceCodeSet_);

  static MSDate prevTradeDate(const MSDate& aDate_=MSDate::today(),const MSResourceCode& resourceCode_="");
  static MSDate prevTradeDate(const MSDate& aDate_,const MSResourceCodeSet& resourceCodeSet_);

  // calculate the forward date from the passed string argument
  // (ex: 1M - one month) - if resourceCodes are supplied holidays for
  // those resources are taken into account

  static MSDate calcForwardDate(const MSTerm& aTerm_,
				const MSResourceCode& resourceCode_,
				const MSDate& startingDate_=MSDate::today());

  static MSDate calcForwardDate(const MSTerm& aTerm_,
				const MSResourceCodeSet& resourceCodeSet_,
				const MSDate& startingDate_=MSDate::today());

  // installs holidays for all resources a specified database
  static MSBoolean installHolidaySetFrom(const MSString&);

  // installs holidays for all resources from database
  static MSBoolean installHolidaySet(void);

  // installs holidays for a resource from database - used for demand loading
  static MSBoolean installHolidaySet(const MSString& resourceCode_);  
  static MSBoolean installHolidaySet(const MSResourceCodeSet& resourceCodeSet_);  

  // find the holiday set for a given resource code(s),if not in,load from
  // database
  static MSBoolean locateOrInstallHolidaySet(const MSResourceCode& resourceCode_);
  static MSBoolean locateOrInstallHolidaySet(const MSResourceCodeSet& resourceCodeSet_);
 
  // adds a single holiday for a given resource code
  static MSBoolean addHoliday(const MSHoliday& aHoliday_);
  static MSDate nextNTradeDate(const MSDate &,int,const MSResourceCodeSet &);
  static MSDate prevNTradeDate(const MSDate &,int,const MSResourceCodeSet &);
  
  friend MSTypesExport ostream& operator<<(ostream&,const MSCalendar&);

  static const MSString& defaultHolidayFile(void);
  static void defaultHolidayFile(const MSString&);

  static const MSString& defaultHolidayDescriptionFile(void);
  static void defaultHolidayDescriptionFile(const MSString&);

  static MSDate lastTradeDateOfMonth(const MSDate& aDate_,const MSResourceCodeSet & rCodeSet_);
  static MSDate lastTradeDateOfPrevMonth(const MSDate& aDate_,const MSResourceCodeSet & );
  static MSDate firstTradeDateOfMonth(const MSDate& aDate_,const MSResourceCodeSet & rCodeSet_);
  
  static MSBoolean isLastTradeDateOfMonth(const MSDate & aDate_,const MSResourceCodeSet & rCodeSet_)
    { return (aDate_==lastTradeDateOfMonth(aDate_,rCodeSet_) ? MSTrue : MSFalse); }
  static MSBoolean isFirstTradeDateOfMonth(const MSDate & aDate_,const MSResourceCodeSet & rCodeSet_)
    { return (aDate_==firstTradeDateOfMonth(aDate_,rCodeSet_) ? MSTrue : MSFalse); }

  static MSBoolean isValidResourceCode(const MSResourceCode &);
  static unsigned int numberOfResourceCodes() ;
  static const MSString& descriptionOfResourceCode(const MSResourceCode & rCode_) ;
  static void dumpDescriptionSet(void);
  
private:
  static MSResourceCodeDescriptionSet       _descSet;
  static MSResourceCodeDescriptionSetCursor _descSetCursor;
  static MSString                           _defaultHolidayFile;
  static MSString                           _defaultHolidayDescriptionFile;
  static MSHolidaySet                       _holidaySet;
  static MSBoolean                          _isDescriptionDataInstalled;
  static MSString                           _invalidResourceCodeMessage;

  static MSBoolean installHolidayDescriptionData(void);
  static MSHolidaySet& holidaySet(void) { return _holidaySet; }
  static MSBoolean hasDescriptionDataInstalled(void);
  static MSBoolean isResourceCodeInDescSet(const MSResourceCode &);
  
  static MSBoolean parseAndAddHolidaySet(const MSString& holidayString_,
					 const MSResourceCode& resourceCode_);
  static MSBoolean isHoliday(const MSDate& aDate_,const MSResourceHolidaySet&);
  static MSBoolean isValidTradeDate(const MSDate& aDate_,const MSResourceHolidaySet&);
  static MSDate lastTradeDateOfMonth(const MSDate& aDate_,
				     const MSResourceHolidaySet& aSet1_,
				     const MSResourceHolidaySet& aSet2_);
};


// MSResourceHolidaySet inlines
inline const MSResourceCode& MSResourceHolidaySet::resourceCode(void) const
{ return _resourceCode; }

inline MSResourceCode const& key(MSResourceHolidaySet const& holidaySet_)
{ return holidaySet_.resourceCode(); }

inline MSResourceCode const& key(MSResourceCode const& resourceCode_) 
{ return resourceCode_; }

inline MSResourceCode const& key(MSResourceCode & resourceCode_) 
{ return resourceCode_; }

// MSResourceCodeDesc inlines
inline const MSResourceCode & key(const MSResourceCodeDesc & rcd_)
{ return rcd_.resourceCode(); }

// We provide provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSTYPESLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSIHashKeySet<MSHoliday,MSDate>
#pragma do_not_instantiate MSIHashKeySetNode<MSHoliday>
#pragma do_not_instantiate MSIHashKeySetCursor<MSHoliday,MSDate>

#pragma do_not_instantiate MSIHashKeySet<MSResourceHolidaySet,MSResourceCode>
#pragma do_not_instantiate MSIHashKeySetNode<MSResourceHolidaySet>
#pragma do_not_instantiate MSIHashKeySetCursor<MSResourceHolidaySet,MSResourceCode>

#pragma do_not_instantiate MSIHashKeySet<MSResourceCodeDesc,MSResourceCode>
#pragma do_not_instantiate MSIHashKeySetNode<MSResourceCodeDesc>
#pragma do_not_instantiate MSIHashKeySetCursor<MSResourceCodeDesc,MSResourceCode>
#endif // !__MSTYPESLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

DECLARE_ELEMENT_FOR_OPS_TYPE(MSHoliday)
DECLARE_ELEMENT_FOR_OPS_TYPE(MSResourceCodeDesc)
DECLARE_ELEMENT_FOR_OPS_TYPE(MSResourceHolidaySet)


#endif

