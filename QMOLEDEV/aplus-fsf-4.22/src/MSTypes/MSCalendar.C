///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#if HAVE_FSTREAM
#include <fstream>
#else
#include <fstream.h>
#endif
#include <MSTypes/MSCalendar.H>
#include <MSTypes/MSMessageLog.H>
#include <MSTypes/MSMutex.H>

#if defined(MSTK_MANUAL_INSTANTIATION)
#include <MSTypes/MSIHashKeySet.C>

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define(MSIHashKeySet<MSHoliday,MSDate>)
#pragma define(MSIHashKeySet<MSResourceHolidaySet,MSResourceCode>)
#pragma define(MSIHashKeySet<MSResourceCodeDesc,MSResourceCode>)
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSIHashKeySet<MSHoliday,MSDate>
#pragma instantiate MSIHashKeySet<MSResourceHolidaySet,MSResourceCode>
#pragma instantiate MSIHashKeySet<MSResourceCodeDesc,MSResourceCode>
#endif

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSIHashKeySet<MSHoliday,MSDate>;
template class MSIHashKeySet<MSResourceHolidaySet,MSResourceCode>;
template class MSIHashKeySet<MSResourceCodeDesc,MSResourceCode>;
#endif

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSIHashKeySet<MSHoliday,MSDate>;
template MSIHashKeySet<MSResourceHolidaySet,MSResourceCode>;
template MSIHashKeySet<MSResourceCodeDesc,MSResourceCode>;
#endif

#endif //MSTK_MANUAL_INSTANTIATION

//#######################################################
// MSCalendar
//#######################################################

MSString MSCalendar::_defaultHolidayFile("./holidays.dat");
MSString MSCalendar::_defaultHolidayDescriptionFile("./holidays.des");
MSBoolean MSCalendar::_isDescriptionDataInstalled=MSFalse;
unsigned int MSResourceCodeDesc::_count=0;
MSString MSCalendar::_invalidResourceCodeMessage="INVALID MSResourceCode !! ";

MSResourceCodeDescriptionSet MSCalendar::_descSet;
MSResourceCodeDescriptionSetCursor MSCalendar::_descSetCursor(MSCalendar::_descSet);

MSHolidaySet MSCalendar::_holidaySet;

#if defined(MS_MULTI_THREAD)
static MSMutex descSetMutex;
static MSMutex holidaySetMutex;
#endif //MS_MULTI_THREAD

// The functions below implement layering technique in MSCalendar.  The idea
// behind layering is to separate internal (implementation) and external
// (interface) methods of a class.  Interface methods will be called by the
// user and will never be called internally by other class functions.
// Interface methods will call implementation methods to do the actual work.
// Implementation methods can call other implementation methods.  This
// separation of the code into different layers is especially useful when
// implementing thread safety of a class.  Implementation methods *never*
// do any locking; locking is full responsibility of interface methods.
// This helps prevent deadlocks (e.g., a method acquires a lock, and then calls
// another method, which tries to acquire the same lock), which would be quite
// difficult if the code were not organized this way.
//
// By convention, all functions whose names start with an underscore ('_') are
// internal (implementation) functions.  Regularly, they would be protected
// member functions of the class; however, since these changes are for a patch
// release (of 2.6), we cannot make any header file changes - thus, we have to
// make the functions global.
//
static MSBoolean _installHolidaySet(MSHolidaySet&, const MSResourceCode&, MSHolidaySet::Cursor&);
static MSBoolean _parseAndAddHolidaySet(MSHolidaySet&, const MSString&, const MSResourceCode, MSHolidaySet::Cursor&);
static MSBoolean _locateOrInstallHolidaySet(MSHolidaySet&, const MSResourceCode&, MSHolidaySet::Cursor&);
inline MSBoolean _isHoliday(const MSDate&, const MSResourceHolidaySet&);
static MSBoolean _isHoliday(const MSDate&, MSHolidaySet&, const MSResourceCode&);
static MSBoolean _isHoliday(const MSDate&, MSHolidaySet&, const MSResourceCodeSet&);
static MSBoolean _isValidTradeDate(const MSDate&, const MSResourceHolidaySet&);
static MSBoolean _isValidTradeDate(const MSDate&, MSHolidaySet&, const MSResourceCode&);
static MSBoolean _isValidTradeDate(const MSDate&, MSHolidaySet&, const MSResourceCodeSet&);
static MSDate _nextTradeDate(const MSDate&, MSHolidaySet&, const MSResourceCode&);
static MSDate _nextTradeDate(const MSDate&, MSHolidaySet&, const MSResourceCodeSet&);

MSCalendar::MSCalendar(void)
{}

MSCalendar::~MSCalendar(void)
{}

const MSString& MSCalendar::defaultHolidayFile(void)
{ return _defaultHolidayFile; }

void MSCalendar::defaultHolidayFile(const MSString& file_)
{ _defaultHolidayFile=file_; }

const MSString& MSCalendar::defaultHolidayDescriptionFile(void)
{ return _defaultHolidayDescriptionFile; }

void MSCalendar::defaultHolidayDescriptionFile(const MSString& file_)
{ _defaultHolidayDescriptionFile=file_; }

// format of holiday file: 
// 
// resourceCode date,"description" date,"description"  .... eol
// resourceCode date,"description" date,"description"  .... eol
// eof
//
// returns MSTrue if the holiday set for resourceCode_ parsed and added successfully
MSBoolean MSCalendar::parseAndAddHolidaySet(const MSString& holidayString_,const MSResourceCode& resourceCode_)
{
  MSHolidaySet::Cursor c(holidaySet());
  return _parseAndAddHolidaySet(holidaySet(),holidayString_,resourceCode_,c);
}


// this will removeAll resources in the holiday set and
// rebuild the calendar from the database
//
MSBoolean MSCalendar::installHolidaySet(void)
{ return installHolidaySetFrom(defaultHolidayFile()); }

MSBoolean MSCalendar::installHolidaySetFrom(const MSString& holidayFile_)
{
  MSGUARD(holidaySetMutex);

  ifstream fin(holidayFile_); 
  if (fin.fail()==ios::goodbit)
   {
     MSString line;
     unsigned position;
     MSHolidaySet::Cursor c(holidaySet());

     holidaySet().removeAll();
     while (!fin.eof())
      {
	line=MSString::lineFrom(fin);
	if (line.length()>0)
	 {
	   position=line.indexOf(' ');
	   if (position<line.length()) _parseAndAddHolidaySet(holidaySet(),line,line.subString(0,position),c);
	 }
      }
     return MSTrue;
   }
  else
   {
     MSMessageLog::errorMessage("MSCalendar: unable to open holiday file - %s - calendar not loaded\n",holidayFile_.string());
   }
  return MSFalse;
}

// returns MSTrue if the holiday set for resourceCode_ is installed successfully
// this method supports the demand loading capability for MSCalendar
MSBoolean MSCalendar::installHolidaySet(const MSResourceCode& resourceCode_)
{
  MSGUARD(holidaySetMutex);
  MSHolidaySet::Cursor c(holidaySet());
  return _installHolidaySet(holidaySet(),resourceCode_,c);
}


MSBoolean MSCalendar::installHolidaySet(const MSResourceCodeSet& rCodeSet_)
{
  MSBoolean installOK=MSTrue;
  unsigned len=rCodeSet_.numberOfElements();
  MSGUARD(holidaySetMutex);
  MSHolidaySet::Cursor c(holidaySet());

  for(unsigned int i=0; i<len; ++i)
    {
      const MSResourceCode& resourceCode=rCodeSet_.elementAt(i);
      
      if (_installHolidaySet(holidaySet(),resourceCode,c)==MSFalse)
	{
	  installOK=MSFalse;
	}
    }

  return installOK;
}

// returns MSTrue if the holiday set is already installed or
// if it is installed successfully
MSBoolean MSCalendar::locateOrInstallHolidaySet(const MSString& resourceCode_)
{
  MSGUARD(holidaySetMutex);
  
  MSHolidaySet::Cursor c(holidaySet());
  return _locateOrInstallHolidaySet(holidaySet(),resourceCode_,c);
}

MSBoolean MSCalendar::locateOrInstallHolidaySet(const MSResourceCodeSet& rCodeSet_)
{
  MSBoolean installOK=MSTrue;
  unsigned len=rCodeSet_.numberOfElements();
  MSHolidaySet::Cursor c(holidaySet());
  MSGUARD(holidaySetMutex);

  for(unsigned int i=0; i<len; ++i)
    {
      const MSResourceCode& resourceCode=rCodeSet_.elementAt(i);
      if (_locateOrInstallHolidaySet(holidaySet(),resourceCode,c)==MSFalse)
	{
	  installOK = MSFalse;
	}
    }

  return installOK;
}

MSBoolean MSCalendar::addHoliday(const MSHoliday& aHoliday_)
{
  MSHolidaySet::Cursor c(holidaySet());
  MSGUARD(holidaySetMutex);
  
  if (_locateOrInstallHolidaySet(holidaySet(),aHoliday_.resourceCode(),c)==MSTrue)  
    {
      MSResourceHolidaySet& aSet=holidaySet().elementAt(c);
      aSet.addOrReplaceElementWithKey(aHoliday_);
    }
  else
    {
      MSResourceHolidaySet aResourceHolidaySet(aHoliday_.resourceCode());
      aResourceHolidaySet.addOrReplaceElementWithKey(aHoliday_);
      holidaySet().add(aResourceHolidaySet);
    }

  return MSTrue;
}


MSBoolean MSCalendar::isHoliday(const MSDate& aDate_,const MSResourceHolidaySet& aResourceHolidaySet_)
{
  // This method does not access _holidaySet - therefore, no locking is necessary
  //
  return _isHoliday(aDate_,aResourceHolidaySet_);
}
  

MSBoolean MSCalendar::isHoliday(const MSDate& aDate_,const MSResourceCode& resourceCode_)
{
  MSGUARD(holidaySetMutex);
  return _isHoliday(aDate_,holidaySet(),resourceCode_);
}


MSBoolean MSCalendar::isHoliday(const MSDate& aDate_,const MSResourceCodeSet& rCodeSet_)
{
  MSGUARD(holidaySetMutex);
  return _isHoliday(aDate_,holidaySet(),rCodeSet_);
}


MSBoolean MSCalendar::isValidTradeDate(const MSDate& aDate_,const MSResourceHolidaySet& aResourceHolidaySet_)
{
  // This method does not access _holidaySet - therefore, no locking is necessary
  //
  return _isValidTradeDate(aDate_,aResourceHolidaySet_);
}


MSBoolean MSCalendar::isValidTradeDate(const MSDate& aDate_,const MSString& resourceCode_)
{
  MSGUARD(holidaySetMutex);
  return _isValidTradeDate(aDate_,holidaySet(),resourceCode_);
}


MSBoolean MSCalendar::isValidTradeDate(const MSDate& aDate_,const MSResourceCodeSet & rCodeSet_)
{
  MSGUARD(holidaySetMutex);
  return _isValidTradeDate(aDate_,holidaySet(),rCodeSet_);
}


MSResourceHolidaySet MSCalendar::holidayDateList(const MSResourceCode& resourceCode_) 
{
  MSHolidaySet::Cursor c(holidaySet());
  MSGUARD(holidaySetMutex);

  if (holidaySet().locateElementWithKey(resourceCode_,c)==MSTrue)
    {
      return MSResourceHolidaySet(c.element());
    }
  else
    {
      return MSResourceHolidaySet("");
    }
}

 
// nextTradeDate rolls forward one trade date
// taking holidays into consideration for the supplied resource.
MSDate MSCalendar::nextTradeDate(const MSDate& aDate_,const MSResourceCode& resourceCode_)
{
  MSGUARD(holidaySetMutex);
  return _nextTradeDate(aDate_,holidaySet(),resourceCode_);
}


MSDate MSCalendar::nextTradeDate(const MSDate& aDate_,const MSResourceCodeSet& rCodeSet_)
{
  MSGUARD(holidaySetMutex);
  return _nextTradeDate(aDate_,holidaySet(),rCodeSet_);
}


// prevTradeDate rolls backward one trade date
// taking holidays into consideration for the supplied resource.
MSDate MSCalendar::prevTradeDate(const MSDate& aDate_,const MSResourceCode& resourceCode_)
{
  if(aDate_.isSet()==MSFalse) return MSDate::nullDate();

  MSHolidaySet::Cursor c(holidaySet());
  MSGUARD(holidaySetMutex);

  if (_locateOrInstallHolidaySet(holidaySet(),resourceCode_,c)==MSTrue)  
    {
      MSDate aDate(aDate_);
      MSDay day;
      const MSResourceHolidaySet& aSet=c.element();
      while (MSTrue)
	{
	  aDate--;
	  day=aDate.weekDay();
	  if (day!=MSDate::Saturday&&day!=MSDate::Sunday)
	    {
	      if (aSet.containsElementWithKey(aDate)==MSFalse) return aDate;
	    }
	}
    }

  return MSDate();
}

MSDate MSCalendar::prevTradeDate(const MSDate& aDate_,const MSResourceCodeSet& rCodeSet_)
{
  if(aDate_.isSet()==MSFalse) return MSDate::nullDate();

  MSDate aDate(aDate_);
  MSGUARD(holidaySetMutex);

  do
    {
      --aDate;
    }
  while (_isValidTradeDate(aDate,holidaySet(),rCodeSet_)!=MSTrue);
  return aDate;
}


MSDate MSCalendar::lastTradeDateOfMonth(const MSDate& aDate_, const MSResourceHolidaySet& aSet1_,
					const MSResourceHolidaySet& aSet2_)
{
  // This method does not access _holidaySet - therefore, no locking is necessary
  //
  if(aDate_.isSet()==MSFalse) return MSDate::nullDate();
  MSDate tradeDate(aDate_);
  MSDate firstDayOfMonth(aDate_);  
  tradeDate.setLastDayOfMonth();
  firstDayOfMonth.setFirstDayOfMonth();
  while (tradeDate>=firstDayOfMonth)
   {
     if (_isValidTradeDate(tradeDate,aSet1_)==MSTrue &&
	 _isValidTradeDate(tradeDate,aSet2_)==MSTrue) return tradeDate;
     tradeDate--;
   }
  return tradeDate;
}


MSDate MSCalendar::lastTradeDateOfMonth(const MSDate& aDate_, const MSResourceCodeSet& rCodeSet_)
{
  if(aDate_.isSet()==MSFalse) return MSDate::nullDate();
  MSDate tradeDate(aDate_);
  MSDate firstDayOfMonth(aDate_);  
  tradeDate.setLastDayOfMonth();
  firstDayOfMonth.setFirstDayOfMonth();

  MSGUARD(holidaySetMutex);

  while (tradeDate>=firstDayOfMonth)
    {
      if (_isValidTradeDate(tradeDate,holidaySet(),rCodeSet_)==MSTrue)
	{
	  return tradeDate;
	}

      tradeDate--;
    }

  return tradeDate;
}


MSDate MSCalendar::firstTradeDateOfMonth(const MSDate& aDate_, const MSResourceCodeSet& rCodeSet_)
{
  if(aDate_.isSet()==MSFalse) return MSDate::nullDate();
  MSDate tradeDate(aDate_);
  MSDate lastDayOfMonth(aDate_);  
  tradeDate.setFirstDayOfMonth();
  lastDayOfMonth.setLastDayOfMonth();

  MSGUARD(holidaySetMutex);

  while (tradeDate<=lastDayOfMonth)
    {
      if (_isValidTradeDate(tradeDate,holidaySet(),rCodeSet_)==MSTrue)
	{
	  return tradeDate;
	}

      tradeDate++;
    }

  return tradeDate;
}
  

MSDate MSCalendar::lastTradeDateOfPrevMonth(const MSDate& aDate_, const MSResourceCodeSet& rCodeSet_)
{
  if(aDate_.isSet()==MSFalse) return MSDate::nullDate();
  MSDate aDayOfPrevMonth(aDate_-MSTerm(0,1,0));
  return (lastTradeDateOfMonth(aDayOfPrevMonth, rCodeSet_));
}


// calcForwardDate
// parameters:
// MSTerm   aTerm          - e.g. '1M', '14M', '1Y'
// MSDate   startingDate   - represents the date which you should start with when calculating the
//                           forward date.
// MSString resourceCode1  -
// MSString resourceCode2  -
//
// calcForwardDate does calendar-smart arithmetic, i.e. it skips weekends
//   and holidays.
//
// it computes the forward date by adding the term to starting date.
// if it is a common trade date, return forward date
// else return next trade date of forward date
//
// business-unit specific rules will be handled in MSBusinessDate/Time
//   and their subclasses.
//

MSDate MSCalendar::calcForwardDate(const MSTerm& aTerm_, const MSResourceCode& resourceCode_,
				   const MSDate& startingDate_)
{
  if(startingDate_.isSet()==MSFalse) return MSDate::nullDate();

  MSHolidaySet::Cursor c(holidaySet());
  MSGUARD(holidaySetMutex);

  if (_locateOrInstallHolidaySet(holidaySet(),resourceCode_,c)==MSTrue)
    {
      const MSResourceHolidaySet& aSet=c.element();
      MSDate forwardDate(startingDate_+aTerm_);

      if (_isValidTradeDate(forwardDate,aSet)==MSTrue) return forwardDate;
      else
	{
	  do { forwardDate++; }
	  while (_isValidTradeDate(forwardDate,aSet)!=MSTrue);
	  return forwardDate;
	}
    }

  return MSDate(MSDate::nullDate());
}


MSDate MSCalendar::calcForwardDate(const MSTerm& aTerm_,  const MSResourceCodeSet& rCodeSet_,
				   const MSDate& startingDate_)
{
  if(startingDate_.isSet()==MSFalse) return MSDate::nullDate();
  unsigned len=rCodeSet_.numberOfElements();
  MSHolidaySet::Cursor c(holidaySet());
  MSGUARD(holidaySetMutex);

  for(unsigned int i=0; i<len; ++i) 
    {
      const MSResourceCode& resourceCode=rCodeSet_.elementAt(i);
      if (_locateOrInstallHolidaySet(holidaySet(),resourceCode,c)==MSFalse)
	{
	  return MSDate(MSDate::nullDate());
	}
    }

  MSDate forwardDate(startingDate_+aTerm_);

  if (_isValidTradeDate(forwardDate,holidaySet(),rCodeSet_)==MSTrue)
    {
      // if forward date is a trade date, return it
      //
      return forwardDate;
    }
  else
    {
      // if not, find next trade date
      //
      return _nextTradeDate(forwardDate,holidaySet(),rCodeSet_);
    }
}


// this function is added to avoid overhead of multiple function calls
MSDate MSCalendar::nextNTradeDate(const MSDate & aDate_, int numOfDay_,
				       const MSResourceCodeSet & rCodeSet_)
{
  if(aDate_.isSet()==MSFalse) return MSDate::nullDate();
  
  MSDate aDate(aDate_);
  MSGUARD(holidaySetMutex);

  while (numOfDay_ > 0)
    {
      do
	{
	  ++aDate;
	}
      while (_isValidTradeDate(aDate,holidaySet(),rCodeSet_)!=MSTrue);
      --numOfDay_;
    }

  return aDate;
}

MSDate MSCalendar::prevNTradeDate(const MSDate & aDate_, int numOfDay_,
				  const MSResourceCodeSet & rCodeSet_)
{
  if(aDate_.isSet()==MSFalse) return MSDate::nullDate();
  
  MSDate aDate(aDate_);
  MSGUARD(holidaySetMutex);

  while (numOfDay_ > 0)
    {
      do
	{
	  --aDate;
	}
      while (_isValidTradeDate(aDate,holidaySet(),rCodeSet_)!=MSTrue);
      --numOfDay_;
    }

  return aDate;
}


ostream& operator<<(ostream& aStream_,const MSCalendar& aCalendar_)
{
  MSGUARD(holidaySetMutex);
  MSHolidaySet::Cursor holidaySetCursor(aCalendar_.holidaySet());
  forCursor(holidaySetCursor) aStream_<<holidaySetCursor.element();
  return aStream_;
}


MSBoolean MSCalendar::installHolidayDescriptionData()
{
  // This function is called only if hasDescriptionDataInstalled()==MSFalse, which
  // constitutes the first check in the double-check pattern.  Therefore, we can
  // lock the mutex at once here.
  //
  MSGUARD(descSetMutex);
  //
  // check if another thread has just installed the description data - in that case,
  // we don't have to do anything
  //
  if (_isDescriptionDataInstalled==MSTrue)
    {
      return MSTrue;
    }

  ifstream fin(defaultHolidayDescriptionFile()); 
  if (fin.fail()==ios::goodbit)
    {
      MSString line;
      unsigned position, length;
      while (!fin.eof())
	{
	  line=MSString::lineFrom(fin);
	  position=line.indexOf(' ');
	  length=line.length();
	  if (position<length)
	    {
	      _descSet.add(MSResourceCodeDesc(line.subString(0,position),
					      line.subString(line.indexOfWord(1), length)));
	    }
	}

      _isDescriptionDataInstalled=MSTrue;
    }
  else
    {
      MSMessageLog::errorMessage("MSCalendar: unable to open holiday description file - %s\n",
				 defaultHolidayDescriptionFile().string());
    }

  return MSFalse;
}

// is the a valid resource code
// check if it is in the description set in description file has been
//       installed.
// else load the file and check against it.
MSBoolean MSCalendar::isValidResourceCode(const MSResourceCode & rCode_)
{
  if (hasDescriptionDataInstalled()==MSFalse) installHolidayDescriptionData();
  return isResourceCodeInDescSet(rCode_);
  
}

MSBoolean MSCalendar::hasDescriptionDataInstalled(void)
{
  return _isDescriptionDataInstalled;
}

MSBoolean MSCalendar::isResourceCodeInDescSet(const MSResourceCode & rCode_)
{
  if (hasDescriptionDataInstalled()==MSFalse)
    {
      installHolidayDescriptionData();
    }

  MSResourceCodeDescriptionSetCursor cursor(_descSet);
  return _descSet.locateElementWithKey(rCode_,cursor);
}


MSResourceCodeDesc::MSResourceCodeDesc(const MSResourceCode & rCode_, const MSString & _desc)
: _resourceCode(rCode_), _description(_desc), _index(++_count)
{}

MSResourceCodeDesc::MSResourceCodeDesc(const MSResourceCodeDesc & rCodeDesc_)
: _resourceCode(rCodeDesc_._resourceCode), _description(rCodeDesc_._description), _index(rCodeDesc_._index)
{}

MSResourceCodeDesc& MSResourceCodeDesc::operator=(const MSResourceCodeDesc & rCodeDesc_)
{
  _resourceCode=rCodeDesc_._resourceCode;
  _description=rCodeDesc_._description;
  _index=rCodeDesc_._index;
  return *this;
}


unsigned int MSCalendar::numberOfResourceCodes() 
{
  if (hasDescriptionDataInstalled()==MSFalse)
    {
      installHolidayDescriptionData();
    }

  return _descSet.numberOfElements();
}

const MSString & MSCalendar::descriptionOfResourceCode(const MSResourceCode & rCode_) 
{
  if (hasDescriptionDataInstalled()==MSFalse)
    {
      installHolidayDescriptionData();
    }

  MSResourceCodeDescriptionSetCursor cursor(_descSet);
  if (_descSet.locateElementWithKey(rCode_,cursor)==MSFalse)
    {
      return _invalidResourceCodeMessage;
    }
  else	// resource code found
    {
      return _descSet.elementAt(cursor).description();
    }
}

void MSCalendar::dumpDescriptionSet(void)
{
  if (hasDescriptionDataInstalled()==MSFalse)
    {
      installHolidayDescriptionData();
    }
  
  MSResourceCodeDescriptionSetCursor cursor(_descSet);
  forCursor(cursor)
    {
      _descSet.elementAt(cursor).dump();
    }
}

//#######################################
// MSCalendar's implementation functions
//#######################################

MSBoolean _installHolidaySet(MSHolidaySet& set_, const MSResourceCode& rCode_, MSHolidaySet::Cursor& c_)
{
  if (MSCalendar::isValidResourceCode(rCode_)==MSFalse)
    {
      MSMessageLog::errorMessage("MSCalendar:  UNRECOGNIZED resource code found\n");
      return MSFalse;
    }
  
  ifstream fin(MSCalendar::defaultHolidayFile()); 
  if (fin.fail()==ios::goodbit)
    {
      MSString line;
      unsigned position;
      while (!fin.eof())
	{
	  line=MSString::lineFrom(fin);
	  position=line.indexOf(' ');
	  if (position<line.length())
	    {
	      MSString resourceCode=line.subString(0,position);
	      if (resourceCode==rCode_)
		{
		  return _parseAndAddHolidaySet(set_,line,resourceCode,c_);
		}
	    }
	}
    }
  else
    {
      MSMessageLog::errorMessage("MSCalendar: unable to open holiday file - %s\n"\
				 "cannot load holiday information for: \n",
				 MSCalendar::defaultHolidayFile().string(),rCode_.string());
    }

  return MSFalse;
}


MSBoolean _parseAndAddHolidaySet(MSHolidaySet& set_, const MSString& holidayString_,
				 const MSResourceCode rCode_, MSHolidaySet::Cursor& c_)
{
  MSResourceCode resourceCode,description,holiday;
  unsigned position,lastPosition=0;
  if (holidayString_.length()>0)
    {
      position=holidayString_.indexOf(' ');
      if (position<holidayString_.length())
	{
	  resourceCode=holidayString_.subString(0,position);
	  lastPosition=holidayString_.indexOfAnyBut(' ',position+1);
	  if (resourceCode.length()>0&&resourceCode==rCode_)
	    {
	      MSResourceHolidaySet aResourceHolidaySet(resourceCode);
	      while ((position=holidayString_.indexOf(',',lastPosition))<holidayString_.length())
		{
		  unsigned length=position-int(lastPosition);
		  MSString dateString(holidayString_.subString(lastPosition,length));
		  MSDate aDate(dateString.string());
		  unsigned startDescriptionPosition=holidayString_.indexOf('"',position+1);
		  unsigned endDescriptionPosition=holidayString_.indexOf('"',startDescriptionPosition+1);
		  length=endDescriptionPosition-int(startDescriptionPosition);
		  if (endDescriptionPosition>startDescriptionPosition&&length>1)
		    {
		      description=holidayString_.subString(startDescriptionPosition+1,length-1);
		    }
		  else description="";
		  aResourceHolidaySet.add(MSHoliday(aDate,resourceCode,description));
		  lastPosition=holidayString_.indexOf(' ',endDescriptionPosition)+1;
		}
	      if (set_.addOrReplaceElementWithKey(aResourceHolidaySet,c_)==MSTrue) return MSTrue;
	    }
	}
    }
  return MSFalse;
}


MSBoolean _locateOrInstallHolidaySet(MSHolidaySet& set_, const MSResourceCode& rCode_,
				     MSHolidaySet::Cursor& cursor_)
{
  if (set_.locateElementWithKey(rCode_,cursor_)==MSFalse)
    {
      return _installHolidaySet(set_,rCode_,cursor_);
    }

  return MSTrue;
}


inline MSBoolean _isHoliday(const MSDate& date_, const MSResourceHolidaySet& set_)
{
  return set_.containsElementWithKey(date_);
}


MSBoolean _isHoliday(const MSDate& date_, MSHolidaySet& set_, const MSResourceCode& rCode_)
{
  MSHolidaySet::Cursor c(set_);
  if (_locateOrInstallHolidaySet(set_,rCode_,c)==MSTrue)
    {
      const MSResourceHolidaySet& aSet=c.element();
      return _isHoliday(date_,aSet);
    }
  else
    {
      return MSFalse;
    }
}
  

MSBoolean _isHoliday(const MSDate& date_, MSHolidaySet& set_, const MSResourceCodeSet& rCodeSet_)
{
  if (rCodeSet_.isEmpty()==MSTrue || date_.isSet()==MSFalse)
    {
      return MSFalse;
    }

  unsigned len=rCodeSet_.numberOfElements();

  for(unsigned int i=0; i<len; ++i)
    {
      if (_isHoliday(date_,set_,rCodeSet_.elementAt(i))==MSTrue)
	{
	  return MSTrue;
	}
    }

  return MSFalse;
}


MSBoolean _isValidTradeDate(const MSDate& date_, const MSResourceHolidaySet& set_)
{
  if(date_.isSet()==MSFalse) return MSFalse;  
  MSDay day=date_.weekDay();
  if (day==MSDate::Saturday||day==MSDate::Sunday) return MSFalse;
  else return (_isHoliday(date_,set_)==MSTrue)?MSFalse:MSTrue;
}


MSBoolean _isValidTradeDate(const MSDate& date_, MSHolidaySet& set_, const MSResourceCode& rCode_)
{
  if(date_.isSet()==MSFalse) return MSFalse;  
  MSDay day=date_.weekDay();
  if (day==MSDate::Saturday||day==MSDate::Sunday) return MSFalse;  
  else return (_isHoliday(date_,set_,rCode_)==MSTrue)?MSFalse:MSTrue;
}


MSBoolean _isValidTradeDate(const MSDate& date_, MSHolidaySet& set_, const MSResourceCodeSet& rCodeSet_)
{
  if(date_.isSet()==MSFalse) return MSFalse;  
  MSDay day=date_.weekDay();
  if (day==MSDate::Saturday||day==MSDate::Sunday) return MSFalse;  
  else return (_isHoliday(date_,set_,rCodeSet_)==MSTrue)?MSFalse:MSTrue;
}


MSDate _nextTradeDate(const MSDate& date_, MSHolidaySet& set_, const MSResourceCode& rCode_)
{
  if(date_.isSet()==MSFalse) return MSDate::nullDate();  

  MSHolidaySet::Cursor c(set_);

  if (_locateOrInstallHolidaySet(set_,rCode_,c)==MSTrue)  
    {
      MSDate aDate(date_);
      MSDay day;
      const MSResourceHolidaySet& aSet=c.element();
      while (MSTrue)
	{
	  aDate++;
	  day=aDate.weekDay();
	  if (day!=MSDate::Saturday&&day!=MSDate::Sunday)
	    {
	      if (aSet.containsElementWithKey(aDate)==MSFalse)
		{
		  return aDate;
		}
	    }
	}
    }

  return MSDate();
}


MSDate _nextTradeDate(const MSDate& date_, MSHolidaySet& set_, const MSResourceCodeSet& rCodeSet_)
{
  if(date_.isSet()==MSFalse) return MSDate::nullDate();
  
  MSDate aDate(date_);

  do
    {
      ++aDate;
    }
  while (_isValidTradeDate(aDate,set_,rCodeSet_)!=MSTrue);
  return aDate;
}


void MSResourceCodeDesc::dump(void) const
{
  cerr << _resourceCode << " :: " << _index << " :: " << _description << endl;
}


MSBoolean MSResourceCodeDesc::operator==(const MSResourceCodeDesc& r_) const
{
  return (_resourceCode==r_._resourceCode && _index==r_._index && _description==r_._description) ? MSTrue : MSFalse;
}

//#######################################################
// MSResourceHolidaySet
//#######################################################

MSResourceHolidaySet::MSResourceHolidaySet(const MSResourceCode& resourceCode_) :
_resourceCode(resourceCode_)
{}

MSResourceHolidaySet::MSResourceHolidaySet(const MSResourceHolidaySet& aResourceHolidaySet_) :
_resourceCode(aResourceHolidaySet_.resourceCode())
{ addAllFrom(aResourceHolidaySet_); }

ostream& operator<<(ostream& aStream_,const MSResourceHolidaySet& aResourceHolidaySet_)
{
  MSResourceHolidaySet::Cursor cursor(aResourceHolidaySet_);
  aStream_<<"Holidays for: "<<aResourceHolidaySet_.resourceCode()<<endl;
  forCursor(cursor) aStream_<<cursor.element()<<endl;
  return aStream_<<endl;
}


MSResourceHolidaySet& MSResourceHolidaySet::operator=(const MSResourceHolidaySet& aResourceHolidaySet_)
{
  _resourceCode=aResourceHolidaySet_.resourceCode();
  removeAll();
  addAllFrom(aResourceHolidaySet_);
  return *this;
}
