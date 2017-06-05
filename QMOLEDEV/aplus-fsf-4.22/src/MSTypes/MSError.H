#ifndef MSErrorHEADER
#define MSErrorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDefines.H>

class MSTypesExport MSError
{
public:
  enum ErrorStatus 
  {
    MSSuccess=0,         //  Successful completion  
    MSFailure=1,         //  General failure
    Internal=2,          //  Internal error 
    BadPrice=3,          // *Invalid price string 
    BadString=4,         // *Invalid string format  
    BadDate=5,           //  Invalid date; a valid date is 01/31/92 
    BadTerm=6,           //  Invalid term; a valid term is 3,11,25 or 3y, 11m, 25d 
    BadRate=7,           //  Invalid rate; a valid rate is .725 or 7.25% 
    BadInt=8,            //  Invalid integer; an example of an integer is 7231 
    BadReal=9,           //  Invalid real; an example of a real is 7231.295 
    BadTime=10,          // *Invalid time; an example of a time is 15:30:00 04/13/1992 JST 
    BadBool=11,          // *Invalid boolean; an example of a boolean is 1 or 0 
    BadMoney=12,         // *Invalid money amount; an example of a valid money amount is 3257.29 
    IntTooBig=13,        //  Integer amount is greater than the maximum allowed by the system 
    BadMSFString=14,     //  The MSFString cannot be converted to the specified object
    BadMSDNFieldName=15, //  Invalid MSF field name
    MSDNcfdcError=16,    //  Encountered error in MSDN cfdc layer
    MSDNctlError=17,     //  Encountered error in MSDN ctl layer
    StringTooBig=18,     //  String is too big, can not be copied
    NoMemory=19,         //  Unable to allocate memory
    TimeOut=20,          //  Time-out occurred
    OldObject=21,        //  New object is older than current object
    NoPrototype=22,      //  Class Prototype not found
    NoMSOid=23,          //  Object does not contain MSOid
    MatchExpParse=24,    //  Match expression parsing error
    BadItem=25,          //  Invalid MSDNItem
    BadClone=26,         //  Bad clone, can't clone object
    BadClient=27,        //  Bad Client, client not found
    BadCriteria=28       //  Bad Criteria
  };
  
  static int error(ErrorStatus status,const char *function,const char *format_, ...);
  
};

#endif
