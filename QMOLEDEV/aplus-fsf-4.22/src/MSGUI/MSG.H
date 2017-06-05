#ifndef MSGHEADER
#define MSGHEADER

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

#if defined(MS_HAS_MSG)
// MSG is already used in <windows.h>
#define MSG __MSGraphMSG
#endif

class MSG
{
public:
  MSG(void) {}
  ~MSG(void) {}

  enum GraphStyles  {LastValue=256,Outside=512,Inside=1024};
  enum AxisStyle    {Axis=1,Std=2,Box=64};
  enum AxisMode     {Ascending=0,Descending=1};
  enum Symbol       {Cross=1,X=2,Square=4,Circle=8,Diamond=16,Triangle=32};
  enum Style        {Line=1,Outline=2,Area=4,Bar=8,Stack=16,Step=32,Scatter=64,
		     Fill=128,Segment=256,Text=512,Pie=1024,Osc=2048,Close=4096,Candle=8192,
		     HL=16384,HLOC=32768,HLC=65536,MarketProfile=131072,ColorProfile=262144};
  enum Constraint   {HoldX=1,HoldY=2};
  enum Orientation  {Vertical=64,Horizontal=128};
};

#endif
