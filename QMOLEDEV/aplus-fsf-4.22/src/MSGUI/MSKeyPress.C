///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1998-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSKeyPress.H>
#include <string.h>

static char *keyTokens[]={"None","Shift","Lock","Ctrl","Meta","NumLck"};
static int  keyMasks[]={MSKeyPress::NoneKeyMask,
			MSKeyPress::ShiftKeyMask,
			MSKeyPress::LockKeyMask,
			MSKeyPress::ControlKeyMask,
                        MSKeyPress::MetaKeyMask,
			MSKeyPress::NumLockMask};

	
static const int NumTokens=6;
static char *tokenString="<Key>";


MSKeyPress::MSKeyPress(const char*  pString_)
{ 
  unsigned int mask;
  translate(pString_,_keysym,mask,_state);
}

MSKeyPress::MSKeyPress(KeySym keysym_,unsigned int state_): 
  _state(state_),
  _keysym(keysym_){}

MSKeyPress::~MSKeyPress(){}

MSBoolean MSKeyPress::isMatch(KeySym keysym_, unsigned int state_) const
{ return ( keysym_ == _keysym && state_ == _state)? MSTrue:MSFalse;}

MSBoolean MSKeyPress::isMatch(const char* pString_) const
{
  KeySym keySym;
  unsigned int mask, flag;
  translate(pString_,keySym,mask,flag);
  return isMatch(keysym(),state(),keySym,mask,flag);
}

void MSKeyPress::translate(const char *pString_,KeySym &keysym_,
					    unsigned int &mask_, unsigned int &flag_) 
{
  mask_=0;
  flag_=0;
  keysym_=0;

  if (pString_!=0)
   {
     char *pString;
     for (int i=0;i<NumTokens;i++)
      {
	// cast is for bug in Borland
	if ((pString=strstr((char*)(void*)pString_,keyTokens[i]))!= 0)
	 {
	   mask_+=keyMasks[i];
	   if (pString==pString_) flag_+=keyMasks[i]; // check for beginning of string
	   else if (*(pString-1)!='~') flag_+=keyMasks[i]; // check for negation character
	 }
      }
     if (pString_[0]=='!') mask_=ExactMask;  // require an exact match
     // cast is for bug in Borland
     if ((pString=strstr((char*)(void*)pString_,tokenString))!=0)
       {
	 keysym_=XStringToKeysym(pString+5);
	 if (keysym_>=0x61&&keysym_<=0x7a&&(mask_&ShiftKeyMask)==ShiftKeyMask)
	   {
	     keysym_-=0x20;  // uppercase-X will send 'Q' not 'q' with Shift down
	   }
       }
     else   mask_+=ALL;
   }
}

MSBoolean MSKeyPress::isMatch(KeySym keysym1_,unsigned int state_, 
				  KeySym keysym2_,unsigned int mask_, unsigned int flag_)
{
  if (mask_&ExactMask) return (state_==( flag_ & ~(ALL)) &&
				((ALL&mask_)||keysym1_==keysym2_))?MSTrue:MSFalse;
  else return ((state_&mask_)==flag_&&
	       ((ALL&mask_)||keysym1_==keysym2_))?MSTrue:MSFalse;
}
