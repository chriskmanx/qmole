#ifndef AtHEADER
#define AtHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#include <MSTypes/MSString.H>
#include <MSGUI/MSGUIEnum.H>
  
class MSGUIExport At 
{
public:
  enum {Default=0,
        Left=1,               // "l"
        Right=2,              // "r"
        Top=4,                // "t"
        Bottom=8,             // "b"
        MinimizeWidth=16,     // "w"
        MinimizeHeight=32,    // "h"
        MaintainWidth=64,     // "W" 
        MaintainHeight=128    // "H" 
       }; 
private:
  int           _row;
  int           _column;
  int           _rowSpan;
  int           _columnSpan;
  unsigned long _constraints;

  MSString parseConstraints(unsigned long) const;
  unsigned long parseConstraints(const char *) const;

  void parseString(const char *, int);

public:
  At(void);
  At(int row_,int col_,int rowSpan_,int columnSpan_,unsigned long constraints_=0);
  At(int row_,int col_,int rowSpan_,int columnSpan_,const char *constraints_);
  At(const At&);
  At(const MSString&);  // "row col rowSpan colSpan whHW"
  At(const char *);
  ~At(void);

  int row(void) const;
  int column(void) const;
  int rowSpan(void) const;
  int columnSpan(void) const;
  unsigned long constraints(void) const;
  MSString parsedConstraints(void) const;
  MSString asString(void) const;

  void row(int row_);
  void column(int col_);
  void rowSpan(int span_);
  void columnSpan(int span_);
  void constraints(unsigned long constraints_);
  void constraints(const char *constraints_);

  void at(int row_,int col_,int rowSpan_,int columnSpan_,unsigned long constraints_=0);
  void at(int row_,int col_,int rowSpan_,int columnSpan_,const char *constraints_);

  MSBoolean operator==(const At&);
  MSBoolean operator!=(const At&);
  At& operator=(const At&);
};

#endif

