#ifndef MSGUIEnumHEADER
#define MSGUIEnumHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSEnum.H>

enum MSAlignment           {MSNone=0,MSCenter=2,MSLeft=4,MSRight=8,MSTop=16,MSBottom=32};
enum MSStatus              {MSFailure=0,MSSuccess=1}; 
enum MSShadowStyle         {MSRaised,MSSunken,MSEtchedIn,MSEtchedOut,MSFlat};
enum MSLineStyle           {MSSolid=0,MSDot=1,MSDash=2};
enum MSWidgetIterationType {MSBreadthFirst,MSDepthFirst};
enum MSSelectionMode       {MSSingle,MSBrowse,MSMultiple,MSToggle};
enum MSCycleColorMode      {MSForeground,MSBackground,MSReverseVideo};
enum MSClipMode            {MSNoClipping,MSClipStars,MSClipIndicator};



#endif







