#ifndef	MSRegexpHEADER
#define MSRegexpHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <MSTypes/MSEnum.H>
#include <MSTypes/MSRange.H>
#include <MSTypes/MSRefCount.H>


// Definitions etc. for regexp(3) routines.
// Caveat:  this is V8 regexp(3) [actually, a reimplementation thereof],
// not the System V one.


#define NSUBEXP  10

struct regexp;

class MSTypesExport MSRegexp
{
public:
  enum CaseFlag {ConsiderCase=0,IgnoreCase=1};
    
private:
  class MSTypesExport RegexpData : public MSRefCount
  {
  public:
    RegexpData(const char *pattern_,CaseFlag caseFlag_=ConsiderCase);
    ~RegexpData(void);

    regexp     *_regularExpression;
    char       *_pattern;
    char       *_target; // only used as a base address to get an offset
    MSBoolean   _status;
    CaseFlag    _caseFlag;
    int         _numGroups;
   
    regexp *regularExpression(void) const {return _regularExpression;}
    MSBoolean isMatch(const char *pattern_);
    MSRange groupRange(unsigned) const;
    void strlwr(char *pString_) const
     {while(*pString_) {*pString_=tolower(*pString_),pString_++;}}
  };

  RegexpData *_regexpData;

  RegexpData *regexpData(void) const {return _regexpData;}
  CaseFlag caseFlag(void)      const {return _regexpData->_caseFlag;}

  void init(const char *pattern_,CaseFlag);

public:
  MSRegexp(void);
  MSRegexp(const char *pattern_,CaseFlag caseFlag_=ConsiderCase);
  MSRegexp(const MSRegexp&);
  ~MSRegexp(void);

  MSBoolean match(const char *target_);    // old API, replaced by isMatch
  MSBoolean isMatch(const char *target_);  

  MSBoolean isCaseSensitive(void)     const {return MSBoolean(caseFlag()==ConsiderCase);}
  const char *pattern(void)           const {return _regexpData->_pattern;}
  MSBoolean isValid(void)             const {return _regexpData->_status;}
  unsigned numberOfGroups(void)       const {return _regexpData->_numGroups;}
  MSRange groupRange(unsigned index_) const {return _regexpData->groupRange(index_);}
  
  MSRegexp& operator=(const MSRegexp&);

  friend MSTypesExport MSBoolean operator==(MSRegexp& aRegexp_,const char *target_)
   { return aRegexp_.isMatch(target_); }
  friend MSTypesExport MSBoolean operator==(const char *target_,MSRegexp& aRegexp_)
   { return aRegexp_.isMatch(target_); }
  friend MSTypesExport MSBoolean operator!=(MSRegexp& aRegexp_,const char *target_)
   { return (aRegexp_.isMatch(target_)==MSTrue)?MSFalse:MSTrue; }
  friend MSTypesExport MSBoolean operator!=(const char *target_,MSRegexp& aRegexp_)
   { return (aRegexp_.isMatch(target_)==MSTrue)?MSFalse:MSTrue; }
};

#endif









