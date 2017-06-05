/********************************************************************************
*                                                                               *
*                   A c c e l e r a t o r   T a b l e   C l a s s               *
*                                                                               *
*********************************************************************************
* Copyright (C) 1998,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* This library is free software; you can redistribute it and/or                 *
* modify it under the terms of the GNU Lesser General Public                    *
* License as published by the Free Software Foundation; either                  *
* version 2.1 of the License, or (at your option) any later version.            *
*                                                                               *
* This library is distributed in the hope that it will be useful,               *
* but WITHOUT ANY WARRANTY; without even the implied warranty of                *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU             *
* Lesser General Public License for more details.                               *
*                                                                               *
* You should have received a copy of the GNU Lesser General Public              *
* License along with this library; if not, write to the Free Software           *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.    *
*********************************************************************************
* $Id: FXAccelTable.cpp,v 1.51.2.1 2007/03/07 14:30:27 fox Exp $                    *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxkeys.h"
#include "fxascii.h"
#include "fxunicode.h"
#include "FXHash.h"
#include "FXThread.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXObject.h"
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXApp.h"
#include "FXAccelTable.h"
#include "fxpriv.h"

/*
  Notes:
  - We also dropped the "fx" since we now have namespaces to keep stuff out of each
    other's hair.
  - Turned parseAccel() cum suis to simple global functions.  The rules for friend
    declarations have change in GCC 4.1 so declaring them as friends is no longer
    possible.
  - We need to deal with X11 unicode keysyms (with 0x01000000 flag) in some way.
*/

#define EMPTYSLOT       0xfffffffe   // Previously used, now empty slot
#define UNUSEDSLOT      0xffffffff   // Unsused slot marker


using namespace FX;

/*******************************************************************************/

namespace FX {


// Map
FXDEFMAP(FXAccelTable) FXAccelTableMap[]={
  FXMAPFUNC(SEL_KEYPRESS,0,FXAccelTable::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXAccelTable::onKeyRelease),
  };


// Object implementation
FXIMPLEMENT(FXAccelTable,FXObject,FXAccelTableMap,ARRAYNUMBER(FXAccelTableMap))


// Make empty accelerator table
FXAccelTable::FXAccelTable(){
  FXTRACE((100,"%p->FXAccelTable::FXAccelTable\n",this));
  FXMALLOC(&key,FXAccelKey,1);
  key[0].code=UNUSEDSLOT;
  key[0].target=NULL;
  key[0].messagedn=0;
  key[0].messageup=0;
  max=0;
  num=0;
  }


// Resize hash table, and rehash old stuff into it
void FXAccelTable::resize(FXuint m){
  register FXuint p,i,c;
  FXAccelKey *newkey;
  FXMALLOC(&newkey,FXAccelKey,m+1);
  for(i=0; i<=m; i++){
    newkey[i].code=UNUSEDSLOT;
    newkey[i].target=NULL;
    newkey[i].messagedn=0;
    newkey[i].messageup=0;
    }
  for(i=0; i<=max; i++){
    if((c=key[i].code)>=EMPTYSLOT) continue;
    p=(c*13)&m;
    while(newkey[p].code!=UNUSEDSLOT) p=(p+1)&m;
    newkey[p]=key[i];
    }
  FXFREE(&key);
  key=newkey;
  max=m;
  }


// Add (or replace) accelerator
void FXAccelTable::addAccel(FXHotKey hotkey,FXObject* target,FXSelector seldn,FXSelector selup){
  if(hotkey){
    FXTRACE((150,"%p->FXAccelTable::addAccel: code=%04x state=%04x\n",this,(FXushort)hotkey,(FXushort)(hotkey>>16)));
    register FXuint p=(hotkey*13)&max;
    register FXuint c;
    FXASSERT(hotkey!=UNUSEDSLOT);
    FXASSERT(hotkey!=EMPTYSLOT);
    while((c=key[p].code)!=UNUSEDSLOT){ // Check if in table already
      if(c==hotkey) goto x;
      p=(p+1)&max;
      }
    ++num;
    if(max<(num<<1)) resize((max<<1)+1);
    FXASSERT(num<=max);
    p=(hotkey*13)&max;                  // Locate first unused or empty slot
    while((c=key[p].code)<EMPTYSLOT){
      p=(p+1)&max;
      }
x:  key[p].code=hotkey;                 // Add or replace accelerator info
    key[p].target=target;
    key[p].messagedn=seldn;
    key[p].messageup=selup;
    }
  }


// Remove accelerator.
// When removed, the slot may still be in a chain of probe
// positions, unless it is demonstrably the last item in a chain.
void FXAccelTable::removeAccel(FXHotKey hotkey){
  if(hotkey){
    FXTRACE((150,"%p->FXAccelTable::removeAccel: code=%04x state=%04x\n",this,(FXushort)hotkey,(FXushort)(hotkey>>16)));
    register FXuint p=(hotkey*13)&max;
    register FXuint c;
    FXASSERT(hotkey!=UNUSEDSLOT);
    FXASSERT(hotkey!=EMPTYSLOT);
    while((c=key[p].code)!=hotkey){
      if(c==UNUSEDSLOT) return;
      p=(p+1)&max;
      }
    if(key[(p+1)&max].code==UNUSEDSLOT){// Last in chain
      key[p].code=UNUSEDSLOT;
      }
    else{                               // Middle of chain
      key[p].code=EMPTYSLOT;
      }
    key[p].target=NULL;
    key[p].messagedn=0;
    key[p].messageup=0;
    if(max>=(num<<2)) resize(max>>1);
    --num;
    FXASSERT(num<=max);
    }
  }


// See if accelerator exists
bool FXAccelTable::hasAccel(FXHotKey hotkey) const {
  if(hotkey){
    register FXuint p=(hotkey*13)&max;
    register FXuint c;
    FXASSERT(hotkey!=UNUSEDSLOT);
    FXASSERT(hotkey!=EMPTYSLOT);
    while((c=key[p].code)!=hotkey){
      if(c==UNUSEDSLOT) return false;
      p=(p+1)&max;
      }
    return true;
    }
  return false;
  }


// Return target object of the given accelerator
FXObject* FXAccelTable::targetOfAccel(FXHotKey hotkey) const {
  if(hotkey){
    register FXuint p=(hotkey*13)&max;
    register FXuint c;
    FXASSERT(hotkey!=UNUSEDSLOT);
    FXASSERT(hotkey!=EMPTYSLOT);
    while((c=key[p].code)!=hotkey){
      if(c==UNUSEDSLOT) return NULL;
      p=(p+1)&max;
      }
    return key[p].target;
    }
  return NULL;
  }


// Keyboard press; forward to accelerator target
long FXAccelTable::onKeyPress(FXObject* sender,FXSelector,void* ptr){
  FXTRACE((200,"%p->FXAccelTable::onKeyPress keysym=0x%04x state=%04x\n",this,((FXEvent*)ptr)->code,((FXEvent*)ptr)->state));
  register FXEvent* event=(FXEvent*)ptr;
  register FXuint code=MKUINT(event->code,event->state&(SHIFTMASK|CONTROLMASK|ALTMASK|METAMASK));
  register FXuint p=(code*13)&max;
  register FXuint c;
  FXASSERT(code!=UNUSEDSLOT);
  FXASSERT(code!=EMPTYSLOT);
  while((c=key[p].code)!=code){
    if(c==UNUSEDSLOT) return 0;
    p=(p+1)&max;
    }
  if(key[p].target && key[p].messagedn){
    key[p].target->tryHandle(sender,key[p].messagedn,ptr);
    }
  return 1;
  }


// Keyboard release; forward to accelerator target
long FXAccelTable::onKeyRelease(FXObject* sender,FXSelector,void* ptr){
  FXTRACE((200,"%p->FXAccelTable::onKeyRelease keysym=0x%04x state=%04x\n",this,((FXEvent*)ptr)->code,((FXEvent*)ptr)->state));
  register FXEvent* event=(FXEvent*)ptr;
  register FXuint code=MKUINT(event->code,event->state&(SHIFTMASK|CONTROLMASK|ALTMASK|METAMASK));
  register FXuint p=(code*13)&max;
  register FXuint c;
  FXASSERT(code!=UNUSEDSLOT);
  FXASSERT(code!=EMPTYSLOT);
  while((c=key[p].code)!=code){
    if(c==UNUSEDSLOT) return 0;
    p=(p+1)&max;
    }
  if(key[p].target && key[p].messageup){
    key[p].target->tryHandle(sender,key[p].messageup,ptr);
    }
  return 1;
  }


// Unparse hot key back into a string
FXString unparseAccel(FXHotKey key){
  FXuint mods=(key&0xffff0000)>>16;
  FXuint code=(key&0xffff);
  FXchar buffer[64];
  FXString s;

  // Handle modifier keys
  if(mods&CONTROLMASK) s+="Ctrl+";
  if(mods&ALTMASK) s+="Alt+";
  if(mods&SHIFTMASK) s+="Shift+";
  if(mods&METAMASK) s+="Meta+";

  // Handle some special keys
  switch(code){
    case KEY_Home:
      s+="Home";
      break;
    case KEY_End:
      s+="End";
      break;
    case KEY_Page_Up:
      s+="PgUp";
      break;
    case KEY_Page_Down:
      s+="PgDn";
      break;
    case KEY_Left:
      s+="Left";
      break;
    case KEY_Right:
      s+="Right";
      break;
    case KEY_Up:
      s+="Up";
      break;
    case KEY_Down:
      s+="Down";
      break;
    case KEY_Insert:
      s+="Ins";
      break;
    case KEY_Delete:
      s+="Del";
      break;
    case KEY_Escape:
      s+="Esc";
      break;
    case KEY_Tab:
      s+="Tab";
      break;
    case KEY_Return:
      s+="Return";
      break;
    case KEY_BackSpace:
      s+="Back";
      break;
    case KEY_space:
      s+="Space";
      break;
    case KEY_F1:
    case KEY_F2:
    case KEY_F3:
    case KEY_F4:
    case KEY_F5:
    case KEY_F6:
    case KEY_F7:
    case KEY_F8:
    case KEY_F9:
    case KEY_F10:
    case KEY_F11:
    case KEY_F12:
    case KEY_F13:
    case KEY_F14:
    case KEY_F15:
    case KEY_F16:
    case KEY_F17:
    case KEY_F18:
    case KEY_F19:
    case KEY_F20:
    case KEY_F21:
    case KEY_F22:
    case KEY_F23:
    case KEY_F24:
    case KEY_F25:
    case KEY_F26:
    case KEY_F27:
    case KEY_F28:
    case KEY_F29:
    case KEY_F30:
    case KEY_F31:
    case KEY_F32:
    case KEY_F33:
    case KEY_F34:
    case KEY_F35:
      sprintf(buffer,"F%d",code-KEY_F1+1);
      s+=buffer;
      break;
    default:
      if(Ascii::isPrint(code)){
        if(mods&SHIFTMASK)
          s+=Ascii::toUpper(code);
        else
          s+=Ascii::toLower(code);
        }
      else{
        sprintf(buffer,"#%04x",code);
        s+=buffer;
        }
      break;
    }
  return s;
  }


// Parse accelerator from menu
FXHotKey parseAccel(const FXString& string){
  register FXuint code=0,mods=0;
  register FXint pos=0;

  // Parse leading space
  while(pos<string.length() && Ascii::isSpace(string[pos])) pos++;

  // Parse modifiers
  while(pos<string.length()){

    // Modifier
    if(comparecase(&string[pos],"ctl",3)==0){ mods|=CONTROLMASK; pos+=3; }
    else if(comparecase(&string[pos],"ctrl",4)==0){ mods|=CONTROLMASK; pos+=4; }
    else if(comparecase(&string[pos],"alt",3)==0){ mods|=ALTMASK; pos+=3; }
    else if(comparecase(&string[pos],"meta",4)==0){ mods|=METAMASK; pos+=4; }
    else if(comparecase(&string[pos],"shift",5)==0){ mods|=SHIFTMASK; pos+=5; }
    else break;

    // Separator
    if(string[pos]=='+' || string[pos]=='-' || Ascii::isSpace(string[pos])) pos++;
    }

  // Test for some special keys
  if(comparecase(&string[pos],"home",4)==0){
    code=KEY_Home;
    }
  else if(comparecase(&string[pos],"end",3)==0){
    code=KEY_End;
    }
  else if(comparecase(&string[pos],"pgup",4)==0){
    code=KEY_Page_Up;
    }
  else if(comparecase(&string[pos],"pgdn",4)==0){
    code=KEY_Page_Down;
    }
  else if(comparecase(&string[pos],"left",4)==0){
    code=KEY_Left;
    }
  else if(comparecase(&string[pos],"right",5)==0){
    code=KEY_Right;
    }
  else if(comparecase(&string[pos],"up",2)==0){
    code=KEY_Up;
    }
  else if(comparecase(&string[pos],"down",4)==0){
    code=KEY_Down;
    }
  else if(comparecase(&string[pos],"ins",3)==0){
    code=KEY_Insert;
    }
  else if(comparecase(&string[pos],"del",3)==0){
    code=KEY_Delete;
    }
  else if(comparecase(&string[pos],"esc",3)==0){
    code=KEY_Escape;
    }
  else if(comparecase(&string[pos],"tab",3)==0){
    code=KEY_Tab;
    }
  else if(comparecase(&string[pos],"return",6)==0){
    code=KEY_Return;
    }
  else if(comparecase(&string[pos],"enter",5)==0){
    code=KEY_Return;
    }
  else if(comparecase(&string[pos],"back",4)==0){
    code=KEY_BackSpace;
    }
  else if(comparecase(&string[pos],"spc",3)==0){
    code=KEY_space;
    }
  else if(comparecase(&string[pos],"space",5)==0){
    code=KEY_space;
    }

  // Test for function keys
  else if(Ascii::toLower(string[pos])=='f' && Ascii::isDigit(string[pos+1])){
    if(Ascii::isDigit(string[pos+2])){
      code=KEY_F1+10*(string[pos+1]-'0')+(string[pos+2]-'0')-1;
      }
    else{
      code=KEY_F1+string[pos+1]-'1';
      }
    }

  // Test if hexadecimal code designator
  else if(string[pos]=='#'){
    code=strtoul(&string[pos+1],NULL,16);
    }

  // Test if its a single character accelerator
  else if(Ascii::isPrint(string[pos])){
    if(mods&SHIFTMASK)
      code=Ascii::toUpper(string[pos])+KEY_space-' ';
    else
      code=Ascii::toLower(string[pos])+KEY_space-' ';
    }

  FXTRACE((110,"parseAccel(%s) = code=%04x mods=%04x\n",string.text(),code,mods));
  return MKUINT(code,mods);
  }


// Parse hot key from string
FXHotKey parseHotKey(const FXString& string){
  register FXuint code=0,mods=0;
  register FXint pos=0;
  register FXwchar w;
  while(pos<string.length()){
    if(string[pos]=='&'){
      if(string[pos+1]!='&'){
        w=string.wc(pos+1);
        if(Unicode::isAlphaNumeric(w)){
          mods=ALTMASK;
          code=fxucs2keysym(Unicode::toLower(w));
          }
        break;
        }
      pos++;
      }
    pos++;
    }
  FXTRACE((110,"parseHotKey(%s) = code=%04x mods=%04x\n",string.text(),code,mods));
  return MKUINT(code,mods);
  }

// Obtain hot key offset in string
FXint findHotKey(const FXString& string){
  register FXint pos=0;
  register FXint n=0;
  while(pos<string.length()){
    if(string[pos]=='&'){
      if(string[pos+1]!='&'){
        return n;
        }
      pos++;
      }
    pos++;
    n++;
    }
  return -1;
  }


// Strip hot key from string
FXString stripHotKey(const FXString& string){
  FXString result=string;
  register FXint len=result.length();
  register FXint i,j;
  for(i=j=0; j<len; j++){
    if(result[j]=='&'){
      if(result[j+1]!='&') continue;
      j++;
      }
    result[i++]=result[j];
    }
  result.trunc(i);
  return result;
  }


// Save data
void FXAccelTable::save(FXStream& store) const {
  register FXuint i;
  FXObject::save(store);
  store << max;
  store << num;
  for(i=0; i<=max; i++){
    store << key[i].code;
    store << key[i].target;
    store << key[i].messagedn;
    store << key[i].messageup;
    }
  }


// Load data
void FXAccelTable::load(FXStream& store){
  register FXuint i;
  FXObject::load(store);
  store >> max;
  store >> num;
  FXRESIZE(&key,FXAccelKey,max+1);
  for(i=0; i<=max; i++){
    store >> key[i].code;
    store >> key[i].target;
    store >> key[i].messagedn;
    store >> key[i].messageup;
    }
  }


// Destroy table
FXAccelTable::~FXAccelTable(){
  FXTRACE((100,"%p->FXAccelTable::~FXAccelTable\n",this));
  FXFREE(&key);
  key=(FXAccelKey*)-1L;
  }


}
