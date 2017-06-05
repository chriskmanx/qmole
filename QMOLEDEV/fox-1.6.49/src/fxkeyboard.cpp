/********************************************************************************
*                                                                               *
*                         K e y b o a r d   H a n d l i n g                     *
*                                                                               *
*********************************************************************************
* Copyright (C) 1997,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: fxkeyboard.cpp,v 1.14.2.2 2006/04/14 01:21:01 fox Exp $                      *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxkeys.h"
#include "fxpriv.h"
#include "FXHash.h"
#include "FXThread.h"
#include "FXStream.h"
#include "FXObject.h"
#include "FXDict.h"
#include "FXString.h"
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXApp.h"
#include "FXId.h"
#include "FXDrawable.h"
#include "FXWindow.h"

/*
  Notes:

  - Translate key cap codes to FOX key symbols on MS-Windows.
  - Windows code first written by Daniel Gehriger <gehriger@linkcad.com.
*/

using namespace FX;

/*******************************************************************************/

namespace FX {


#ifdef WIN32

//-------------------------------------------------------------------------
//
// Notes (Daniel Gehriger <gehriger@linkcad.com>:
//
//
//  [In the comments below, WM_KEYx refers to WM_KEYDOWN|WM_SYSKEYDOWN|
//   WM_KEYUP|WM_SYSKEYUP; WM_CHAR refers to WM_CHAR and WM_IME_CHAR]
//
//  Notes
//  =====
//
//  1. MapVirtualKeyEx and ToAsciiEx don't work in Korean Win98 (although
//     they're fine in other Korean Windows products); MapVirtualKey and
//     ToAscii work fine.
//
//
//  2. Some keyboard layouts use the Right-ALT key as a 2nd level
//     shift key, and label it as AltGr. With these layouts, it is IMPOSSIBLE
//     to distinguish between "Ctrl + AltGr + Key" and "AltGr + Key" during the
//     WM_KEYDOWN messages. Neither the keyboard state nor the messages differ
//     between the two key sequences.
//
//     Implications: suppose some menu command has been bound to "Ctrl + }". On my
//     Swiss keyboard, I have to type "AltGr + $" to get the right bracket. This means
//     that FOX receives the sames messages if I type "}" or "Left-Ctrl + }" !
//
//     Solution: I wrote a function winkeyCheckLayout() that checks if a keyboard
//     layout uses the Right-Alt key as AltGr. If it does, I interpret any combination
//     of "Left-Ctrl + AltGr" as "AltGr", i.e. I drop the Left-Ctrl key. Keyboard
//     accelerators can still be reached with the "Right-Ctrl".
//
//
//  3. Related to the previous comment: how can we distinguish between left
//     and right Alt / Ctrl / Shift ? Windows 95/98/Me do not set the
//     corresponding VK_L*/VK_R* keyboard states, as NT does. We must look
//     at bit 24, the "extended bit" of lParam in the window messages. The code
//     below goes to great lenghts to correctly determine the state of the
//     left / right keys on these systems.
//
//
//  4. The Win32 function ToAscii() has internal state. This means that calling this
//     function twice with the same arguments does NOT generally yield the same result.
//     For instance, if the circumflex (^) is used to compose characters in the
//     selected keyboard layout, and one types '^' followed by 'e', calling
//     ToAscii() to translate the 'e' yields 'ê' (e-grave). Calling ToAscii() again,
//     however, yields a raw 'e'. Also, I found that the Win32 function
//     TranslateMessage(), which is used to have Win32 generate WM_CHAR messages,
//     calls ToAscii() internally.
//
//     The implications of this are :
//
//     1. Either use TranslateMessage() OR use ToAscii() when processing the above
//        messages. Calling both breaks keyboard processing !!!!
//
//     2. Only call ToAscii() when a WM_KEYUP/WM_KEYDOWN/WM_SYSKEYUP/WM_SYSKEYDOWN
//        message has been received.
//
//
//  5. Keyboard accelerators: they are used to trigger some command by typing
//     a character sequence. Some accelerator sequences include the Shift key:
//
//     	  Example:  "Ctrl + {"   versus   "Ctrl + Shift + {".
//
//     However, on some keyboards, some of the characters in the accelerator
//     sequences themselves require the Shift key as a character modifier.
//
//         Example: the US keyboard layout requires "Shift + [" to type "{".
//
//     There is NO workaround, except not using Shift in accelerator sequences
//     that include non-letters.
//
//-------------------------------------------------------------------------

// Keyboard map for control keys
static const FXuint keymapCtl[] = {
  VK_PRIOR,     KEY_Prior,
  VK_NEXT,      KEY_Next,
  VK_END,       KEY_End,
  VK_HOME,      KEY_Home,
  VK_LEFT,      KEY_Left,
  VK_UP,        KEY_Up,
  VK_RIGHT,     KEY_Right,
  VK_DOWN,      KEY_Down,
  VK_INSERT,    KEY_Insert,
  VK_DELETE,    KEY_Delete,
  VK_HELP,      KEY_Help,
  VK_F1,        KEY_F1,
  VK_F2,        KEY_F2,
  VK_F3,        KEY_F3,
  VK_F4,        KEY_F4,
  VK_F5,        KEY_F5,
  VK_F6,        KEY_F6,
  VK_F7,        KEY_F7,
  VK_F8,        KEY_F8,
  VK_F9,        KEY_F9,
  VK_F10,       KEY_F10,
  VK_F11,       KEY_F11,
  VK_F12,       KEY_F12,
  VK_F13,       KEY_F13,
  VK_F14,       KEY_F14,
  VK_F15,       KEY_F15,
  VK_F16,       KEY_F16,
  VK_F17,       KEY_F17,
  VK_F18,       KEY_F18,
  VK_F19,       KEY_F19,
  VK_F20,       KEY_F20,
  VK_F21,       KEY_F21,
  VK_F22,       KEY_F22,
  VK_F23,       KEY_F23,
  VK_F24,       KEY_F24,
  VK_SCROLL,    KEY_Scroll_Lock,
  VK_CLEAR,     KEY_Begin,
  VK_CAPITAL,   KEY_Caps_Lock,
  VK_NUMLOCK,   KEY_Num_Lock,
  VK_SNAPSHOT,  KEY_Print,
  VK_CANCEL,    KEY_Break,
  VK_PAUSE,     KEY_Pause,
  VK_BACK,      KEY_BackSpace,
  VK_TAB,       KEY_Tab,
  VK_ESCAPE,    KEY_Escape,
  VK_SPACE,     KEY_space,
  VK_MULTIPLY,  KEY_KP_Multiply,
  VK_ADD,       KEY_KP_Add,
  VK_SEPARATOR, KEY_KP_Separator,
  VK_SUBTRACT,  KEY_KP_Subtract,
  VK_DECIMAL,   KEY_KP_Decimal,
  VK_DIVIDE,    KEY_KP_Divide,
  VK_NUMPAD0,   KEY_KP_0,
  VK_NUMPAD1,   KEY_KP_1,
  VK_NUMPAD2,   KEY_KP_2,
  VK_NUMPAD3,   KEY_KP_3,
  VK_NUMPAD4,   KEY_KP_4,
  VK_NUMPAD5,   KEY_KP_5,
  VK_NUMPAD6,   KEY_KP_6,
  VK_NUMPAD7,   KEY_KP_7,
  VK_NUMPAD8,   KEY_KP_8,
  VK_NUMPAD9,   KEY_KP_9,
  VK_LWIN,      KEY_Super_L,
  VK_RWIN,      KEY_Super_R
  };


#define KEYDOWN(ks,vk)    (((ks)[vk]&0x80)!=0)
#define KEYUP(ks,vk)      (((ks)[vk]&0x80)==0)
#define KEYTOGGLED(ks,vk) (((ks)[vk]&0x01)!=0)


// True if OS does not distinguish between left & right Alt/Ctrl/Shift keys
static FXbool bNoLR=FALSE;
static BYTE ksRShft=0;
static BYTE ksRCtrl=0;
static BYTE ksLMenu=0;
static BYTE ksRMenu=0;


// Retrieves the current input code page
UINT wkbGetCodePage(){
  static HKL hklOld=NULL;
  static UINT uCPID=0;
  HKL hkl=GetKeyboardLayout(0);
  if(hklOld!=hkl || uCPID==0){
    hklOld=hkl;
    char lpLCData[256];
    if(GetLocaleInfoA(LANGIDFROMLCID(LOWORD(hkl)),LOCALE_IDEFAULTANSICODEPAGE,lpLCData,sizeof(lpLCData))==0) return CP_ACP;
    uCPID=atoi(lpLCData);
    }
  return uCPID;
  }


// Checks if the right-hand ALT key is used as a 2nd shift key
static FXbool wkbAltGrDown(PBYTE ks){
  static FXbool bHasAltGr=FALSE;
  static HKL hklOld = NULL;
  HKL hkl=GetKeyboardLayout(0);
  if(hklOld!=hkl){
    hklOld=hkl;
    bHasAltGr=FALSE;
    for(FXuint ch=0x20; ch<=0xff ; ++ch){
      // <MSDN>
      // For keyboard layouts that use the right-hand ALT key as a shift key
      // (for example, the French keyboard layout), the shift state is
      // represented by the value 6, because the right-hand ALT key is
      // converted internally into CTRL+ALT.
      // </MSDN>
      if(HIBYTE(VkKeyScanEx(ch,hkl))==6){
        bHasAltGr=TRUE;
        break;
        }
      }
    }
  if(bNoLR)
    return bHasAltGr && KEYDOWN(ks,VK_MENU) && ksRMenu;
  else
    return bHasAltGr /* && KEYDOWN(ks, VK_LCONTROL) */ && KEYDOWN(ks,VK_RMENU);
  }


// Return the current state of the modifier keys and mouse buttons
unsigned int fxmodifierkeys(){
  FXuint state=0;
  BYTE ks[256];
  GetKeyboardState(ks);
  if(KEYDOWN(ks,VK_SHIFT)) state|=SHIFTMASK;
  if(KEYTOGGLED(ks,VK_CAPITAL)) state|=CAPSLOCKMASK;
  if(KEYTOGGLED(ks,VK_NUMLOCK)) state|=NUMLOCKMASK;
  if(KEYTOGGLED(ks,VK_SCROLL)) state|=SCROLLLOCKMASK;
  if(KEYDOWN(ks,VK_LBUTTON)) state|=LEFTBUTTONMASK;
  if(KEYDOWN(ks,VK_MBUTTON)) state|=MIDDLEBUTTONMASK;
  if(KEYDOWN(ks,VK_RBUTTON)) state|=RIGHTBUTTONMASK;
  if(KEYDOWN(ks,VK_LWIN)) state|=METAMASK;		// Added JVZ
  if(KEYDOWN(ks,VK_RWIN)) state|=METAMASK;
  if(wkbAltGrDown(ks)){
    // Left-Ctrl + Right-Alt = AltGr is used to compose characters;
    // If AltGr is pressed, only allow Right-Control & Left-Alt
    if(ksRCtrl) state|=CONTROLMASK;
    if(ksLMenu) state|=ALTMASK;
    }
  else{
    if(KEYDOWN(ks,VK_CONTROL)) state|=CONTROLMASK;
    if(KEYDOWN(ks,VK_MENU)) state|=ALTMASK;
    }
  return state;
  }

//VK_LWIN (5B)
//Left Windows key (Microsoft Natural keyboard)

//VK_RWIN (5C)
//Right Windows key (Natural keyboard)

// Map Win32 virtual key codes to FOX key codes
FXuint wkbMapKeyCode(UINT iMsg,WPARAM uVirtKey,LPARAM lParam){
  BYTE ks[256];
  char c;

  // Get keyboard state
  if(GetKeyboardState(ks)!=0){

    // Determine left/right key states
    BYTE ksOldRShft=ksRShft;
    BYTE ksOldRCtrl=ksRCtrl;
    BYTE ksOldRMenu=ksRMenu;

    FXuint xt=HIWORD(lParam)&KF_EXTENDED;

    if(!bNoLR && iMsg==WM_KEYDOWN){
      if(uVirtKey==VK_CONTROL) bNoLR|=(KEYDOWN(ks,xt ? VK_RCONTROL : VK_LCONTROL) ^ KEYDOWN(ks,VK_CONTROL));
      if(uVirtKey==VK_MENU) bNoLR|=(KEYDOWN(ks,xt ? VK_RMENU : VK_LMENU) ^ KEYDOWN(ks,VK_MENU));
      }

    // OS does not save correct left/right key states (most Win95/98/Me)
    if(bNoLR){
      ksRShft = ks[VK_RSHIFT];
      ksRCtrl = (KEYDOWN(ks,VK_CONTROL) ? (uVirtKey==VK_CONTROL&&xt) ? 0x80 : ksRCtrl : 0x00);
      ksRMenu = (KEYDOWN(ks,VK_MENU) ? (uVirtKey==VK_MENU && xt) ? 0x80 : ksRMenu : 0x00);
      ksLMenu = (KEYDOWN(ks,VK_MENU) ? (uVirtKey==VK_MENU && !xt) ? 0x80 : ksLMenu : 0x00);
      }

    // OS saves correct left/right key states
    else{
      ksRShft = KEYDOWN(ks,VK_RSHIFT);
      ksRCtrl = KEYDOWN(ks,VK_RCONTROL);
      ksRMenu = KEYDOWN(ks,VK_RMENU);
      ksLMenu = KEYDOWN(ks,VK_LMENU);
      }

    // Map virtual key code
    switch(uVirtKey){
      case VK_SHIFT:
        if(iMsg==WM_KEYDOWN && HIWORD(lParam)&KF_REPEAT)
          return KEY_VoidSymbol;
        else
          return (ksRShft^ksOldRShft) ? KEY_Shift_R : KEY_Shift_L;

      case VK_CONTROL:
        if(iMsg==WM_KEYDOWN && HIWORD(lParam)&KF_REPEAT)
          return KEY_VoidSymbol;
        else
          return (ksRCtrl^ksOldRCtrl) ? KEY_Control_R : KEY_Control_L;

      case VK_MENU:
        if((iMsg==WM_KEYDOWN || iMsg==WM_SYSKEYDOWN) && (HIWORD(lParam)&KF_REPEAT))     // Patch Jon Sargeant <delta17@cox.net>
          return KEY_VoidSymbol;
        else
          return (ksRMenu^ksOldRMenu) ? KEY_Alt_R : KEY_Alt_L;

      case VK_RETURN:
        if(uVirtKey==VK_RETURN && (HIWORD(lParam)&KF_EXTENDED))
          return KEY_KP_Enter;
        else
          return KEY_Return;

      default:

        // Map remaining special keys
        for(FXuint i=0; i<ARRAYNUMBER(keymapCtl)/2; ++i){
          if(keymapCtl[2*i]==uVirtKey){
            return keymapCtl[2*i+1];
            }
          }

        // Map characters (they come in as uppercase,
        // but FOX wants them converted according the current shift state...
        if('A'<=uVirtKey && uVirtKey<='Z'){
          return (FXuint)(KEYDOWN(ks,VK_SHIFT) ? uVirtKey : uVirtKey-'A'+KEY_a);
          }

        // Ask Windows to map remaining characters
        c=(char)LOWORD(MapVirtualKeyEx(uVirtKey,2,GetKeyboardLayout(0))); // FIXME ";" and ":" map to same keysym; this is wrong!
        if(c) return c;
      }
    }
  return KEY_VoidSymbol;
  }


// Windows 9x don't support ToUnicodeEx (but Windows 98/Me export the function
// as a no-op from User32.DLL. The following code determines upon the first call
// to fxToUnicodeEx if the OS supports ToUnicodeEx and fixes the function pointer
// to send subsequent calls directly to the OS. Otherwise, a stub is used.
int WINAPI wkbToUnicodeExStub(UINT,UINT,const BYTE*,LPWSTR,int,UINT,HKL);
int WINAPI wkbToUnicodeExWin9x(UINT,UINT,const BYTE*,LPWSTR,int,UINT,HKL);

typedef int (WINAPI *PFN_TOUNICODEEX)(UINT,UINT,const BYTE*,LPWSTR,int,UINT,HKL);

PFN_TOUNICODEEX ToUnicodeEx=wkbToUnicodeExStub;

// Stub function for first call to fxToUnicodeEx()
int WINAPI wkbToUnicodeExStub(UINT uVirtKey,UINT uScanCode,const BYTE* lpKeyState,LPWSTR pwszBuff,int cchBuff,UINT wFlags,HKL dwhkl){
  OSVERSIONINFOA osinfo={sizeof(OSVERSIONINFOA)};
  GetVersionExA(&osinfo);
  if(osinfo.dwPlatformId==VER_PLATFORM_WIN32_WINDOWS){
    // Windows 9x/Me => use stub
    ToUnicodeEx = wkbToUnicodeExWin9x;
    }
  else{
    // Windows NT => forward to OS
    HMODULE user32Dll=LoadLibraryA("user32");
    ToUnicodeEx=(PFN_TOUNICODEEX)GetProcAddress(user32Dll,"ToUnicodeEx");
    if(!ToUnicodeEx){ToUnicodeEx=wkbToUnicodeExWin9x;}
    }
  return ToUnicodeEx(uVirtKey, uScanCode, lpKeyState, pwszBuff, cchBuff, wFlags, dwhkl);
  }


// Adapter function for Windows 9x/Me
int WINAPI wkbToUnicodeExWin9x(UINT uVirtKey,UINT uScanCode,const BYTE* lpKeyState,LPWSTR pwszBuff,int cchBuff,UINT wFlags,HKL dwhkl){
  WORD c;
  int cnt=ToAsciiEx(uVirtKey,uScanCode,(BYTE*)lpKeyState,&c,wFlags,dwhkl);
  if(cnt<=0) return cnt;
  return MultiByteToWideChar(CP_ACP,0,(LPCSTR)&c,cnt,pwszBuff,cchBuff);
  }


#endif

}
