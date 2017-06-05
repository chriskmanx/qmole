/********************************************************************************
*                                                                               *
*                  F O X   D e s k t o p   C a l c u l a t o r                  *
*                                                                               *
*********************************************************************************
* Copyright (C) 2001,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* This program is free software; you can redistribute it and/or modify          *
* it under the terms of the GNU General Public License as published by          *
* the Free Software Foundation; either version 2 of the License, or             *
* (at your option) any later version.                                           *
*                                                                               *
* This program is distributed in the hope that it will be useful,               *
* but WITHOUT ANY WARRANTY; without even the implied warranty of                *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                 *
* GNU General Public License for more details.                                  *
*                                                                               *
* You should have received a copy of the GNU General Public License             *
* along with this program; if not, write to the Free Software                   *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.    *
*********************************************************************************
* $Id: Calculator.cpp,v 1.58 2006/01/22 18:01:12 fox Exp $                      *
********************************************************************************/
#include "fx.h"
#include "fxkeys.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <signal.h>
#ifndef WIN32
#include <unistd.h>
#endif
#include <ctype.h>
#include "icons.h"
#include "Calculator.h"
#include "Preferences.h"
#include "HelpWindow.h"


#define BINARY_LIMIT      32                      // 32 bits
#define OCTAL_LIMIT       11                      // 11 digits
#define DECIMAL_LIMIT     16                      // +1.234567890123456E-308
#define HEXADECIMAL_LIMIT 8                       // 8 hexadecimal digits

#define GOLDEN            1.6180339887498948482045868343        // Golden ratio
#define INVGOLDEN         (1.0/GOLDEN)                          // Inverse golden ratio

#define	DEG2RAD(x)	  (((2.0*PI)/360.0)*(x))  // Degrees to radians
#define	GRA2RAD(x)	  ((PI/200.0)*(x))        // Grad to radians
#define	RAD2DEG(x)	  ((360.0/(2.0*PI))*(x))  // Radians to degrees
#define	RAD2GRA(x)	  ((200.0/PI)*(x))        // Radians to grad


/*
  Notes:

  - On window enter, direct keyboard focus to numeric input buttons
  - When clicking on the display focus should stay on buttons
  - On resize I'd like x-y aspect to remain the same
  - Would be nice if font size more or less follows window size
  - History option in display to retrieve back earlier results
    (stored when pressing '=')
  - Add button to return largest prime smaller than x.
  - Add buttons for lcm/gcd.
*/

/*******************************************************************************/



// Map
FXDEFMAP(Calculator) CalculatorMap[]={
  FXMAPFUNCS(SEL_COMMAND,Calculator::ID_MODE,Calculator::ID_GRA,Calculator::onCmdAngle),
  FXMAPFUNCS(SEL_UPDATE,Calculator::ID_MODE,Calculator::ID_GRA,Calculator::onUpdAngle),
  FXMAPFUNCS(SEL_COMMAND,Calculator::ID_BASE,Calculator::ID_HEX,Calculator::onCmdBase),
  FXMAPFUNCS(SEL_UPDATE,Calculator::ID_BASE,Calculator::ID_HEX,Calculator::onUpdBase),
  FXMAPFUNCS(SEL_UPDATE,Calculator::ID_0,Calculator::ID_F,Calculator::onUpdDigit),
  FXMAPFUNCS(SEL_COMMAND,Calculator::ID_0,Calculator::ID_F,Calculator::onCmdDigit),
  FXMAPFUNCS(SEL_COMMAND,Calculator::ID_COLOR_DISPLAY,Calculator::ID_COLOR_CLEAR,Calculator::onCmdColor),
  FXMAPFUNCS(SEL_CHANGED,Calculator::ID_COLOR_DISPLAY,Calculator::ID_COLOR_CLEAR,Calculator::onCmdColor),
  FXMAPFUNCS(SEL_UPDATE,Calculator::ID_COLOR_DISPLAY,Calculator::ID_COLOR_CLEAR,Calculator::onUpdColor),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_EXPONENT_ALWAYS,Calculator::onCmdExponent),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_EXPONENT_NEVER,Calculator::onCmdExponent),
  FXMAPFUNC(SEL_UPDATE,Calculator::ID_EXPONENT_ALWAYS,Calculator::onUpdExponent),
  FXMAPFUNC(SEL_UPDATE,Calculator::ID_EXPONENT_NEVER,Calculator::onUpdExponent),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_PRECISION,Calculator::onCmdPrecision),
  FXMAPFUNC(SEL_UPDATE,Calculator::ID_PRECISION,Calculator::onUpdPrecision),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_QUESTION,Calculator::onCmdQuestion),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_BEEP,Calculator::onCmdBeep),
  FXMAPFUNC(SEL_UPDATE,Calculator::ID_BEEP,Calculator::onUpdBeep),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_FONT,Calculator::onCmdFont),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_PREFERENCES,Calculator::onCmdPreferences),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_CLEAR,Calculator::onCmdClear),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_CLEARALL,Calculator::onCmdClearAll),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_INV,Calculator::onCmdInverse),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_HYP,Calculator::onCmdHyper),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_MEM_REC,Calculator::onCmdMemRec),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_MEM_ADD,Calculator::onCmdMemAdd),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_MEM_SUB,Calculator::onCmdMemSub),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_MEM_CLR,Calculator::onCmdMemClr),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_PNT,Calculator::onCmdPoint),
  FXMAPFUNC(SEL_UPDATE,Calculator::ID_PNT,Calculator::onUpdPoint),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_EXP,Calculator::onCmdExp),
  FXMAPFUNC(SEL_UPDATE,Calculator::ID_EXP,Calculator::onUpdExp),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_DELETE,Calculator::onCmdDelete),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_SIN,Calculator::onCmdSin),
  FXMAPFUNC(SEL_UPDATE,Calculator::ID_SIN,Calculator::onUpdSin),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_COS,Calculator::onCmdCos),
  FXMAPFUNC(SEL_UPDATE,Calculator::ID_COS,Calculator::onUpdCos),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_TAN,Calculator::onCmdTan),
  FXMAPFUNC(SEL_UPDATE,Calculator::ID_TAN,Calculator::onUpdTan),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_LOG,Calculator::onCmdLog),
  FXMAPFUNC(SEL_UPDATE,Calculator::ID_LOG,Calculator::onUpdLog),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_LN,Calculator::onCmdLn),
  FXMAPFUNC(SEL_UPDATE,Calculator::ID_LN,Calculator::onUpdLn),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_PI,Calculator::onCmdPi),
  FXMAPFUNC(SEL_UPDATE,Calculator::ID_PI,Calculator::onUpdPi),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_FAC,Calculator::onCmdFac),
  FXMAPFUNC(SEL_UPDATE,Calculator::ID_FAC,Calculator::onUpdFac),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_PER,Calculator::onCmdPer),
  FXMAPFUNC(SEL_UPDATE,Calculator::ID_PER,Calculator::onUpdPer),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_COM,Calculator::onCmdCom),
  FXMAPFUNC(SEL_UPDATE,Calculator::ID_COM,Calculator::onUpdCom),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_RECIP,Calculator::onCmdRecip),
  FXMAPFUNC(SEL_UPDATE,Calculator::ID_RECIP,Calculator::onUpdRecip),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_PLUSMIN,Calculator::onCmdPlusMin),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_XTOY,Calculator::onCmdXToY),
  FXMAPFUNC(SEL_UPDATE,Calculator::ID_XTOY,Calculator::onUpdXToY),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_SQRT,Calculator::onCmdSqrt),
  FXMAPFUNC(SEL_UPDATE,Calculator::ID_SQRT,Calculator::onUpdSqrt),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_SHL,Calculator::onCmdShl),
  FXMAPFUNC(SEL_UPDATE,Calculator::ID_SHL,Calculator::onUpdShl),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_SHR,Calculator::onCmdShr),
  FXMAPFUNC(SEL_UPDATE,Calculator::ID_SHR,Calculator::onUpdShr),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_2LOG,Calculator::onCmd2Log),
  FXMAPFUNC(SEL_UPDATE,Calculator::ID_2LOG,Calculator::onUpd2Log),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_LPAR,Calculator::onCmdLPar),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_RPAR,Calculator::onCmdRPar),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_AND,Calculator::onCmdAnd),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_OR,Calculator::onCmdOr),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_XOR,Calculator::onCmdXor),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_NOT,Calculator::onCmdNot),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_MUL,Calculator::onCmdMul),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_DIV,Calculator::onCmdDiv),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_MOD,Calculator::onCmdMod),
  FXMAPFUNC(SEL_UPDATE,Calculator::ID_MOD,Calculator::onUpdMod),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_ADD,Calculator::onCmdAdd),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_SUB,Calculator::onCmdSub),
  FXMAPFUNC(SEL_COMMAND,Calculator::ID_ENTER,Calculator::onCmdEnter),
  };


// Implementation
FXIMPLEMENT(Calculator,FXMainWindow,CalculatorMap,ARRAYNUMBER(CalculatorMap))


// Use a trick to get a nan
#if FOX_BIGENDIAN
static const FXuint   nanny[2]={0x7fffffff,0xffffffff};
#else
static const FXuint   nanny[2]={0xffffffff,0x7fffffff};
#endif

// Double precision nan
static const FXdouble& dblnan=*((FXdouble*)nanny);


// Operator priorities
static const FXuchar priority[]={
  1,  // DY_OR
  1,  // DY_XOR
  1,  // DY_AND
  2,  // DY_SUB
  2,  // DY_ADD
  3,  // DY_MOD
  3,  // DY_IDIV
  3,  // DY_DIV
  3,  // DY_MUL
  4,  // DY_XTOY
  4,  // DY_XTOINVY
  5,  // DY_PER
  5,  // DY_COM
  8,  // DY_LPAR
  8,  // DY_RPAR
  };



/*******************************************************************************/

// Construct calculator dialog
Calculator::Calculator(FXApp* a):FXMainWindow(a,"FOX Calculator",NULL,NULL,DECOR_ALL, 0,0,0,0, 0,0){

  // Default font used by default, duh!
  font=NULL;

  // Make some icons
  bigicon=new FXGIFIcon(getApp(),bigcalc);
  smallicon=new FXGIFIcon(getApp(),tinycalc);
  cmem=new FXBMPIcon(getApp(),constmem);
  quest=new FXGIFIcon(getApp(),question);

  // Application icons
  setIcon(bigicon);
  setMiniIcon(smallicon);

  // Interior
  FXVerticalFrame *vert=new FXVerticalFrame(this,LAYOUT_FILL_X,0,0,0,0, 8,8,8,4, 1,1);
  FXHorizontalFrame *displayframe=new FXHorizontalFrame(vert,LAYOUT_FILL_X,0,0,0,0, 0,0,0,0);
  new FXButton(displayframe,"FOX Calculator",bigicon,this,ID_PREFERENCES,ICON_BEFORE_TEXT|JUSTIFY_LEFT|LAYOUT_FILL_Y,0,0,0,0, 4,4,2,2);
  new FXButton(displayframe,FXString::null,quest,this,ID_QUESTION,ICON_BEFORE_TEXT|JUSTIFY_LEFT|LAYOUT_FILL_Y,0,0,0,0, 4,4,2,2);
  display=new FXTextField(displayframe,16,this,ID_TEXT,TEXTFIELD_READONLY|FRAME_SUNKEN|FRAME_THICK|JUSTIFY_RIGHT|LAYOUT_FILL_X|LAYOUT_FILL_Y, 0,0,0,0, 4,4,1,1);
  new FXLabel(vert,FXString::null,cmem,LAYOUT_RIGHT,0,0,0,0, 0,0,0,0);

  FXHorizontalFrame *modeframe=new FXHorizontalFrame(this,LAYOUT_FILL_X,0,0,0,0, 8,8,0,4, 8,8);

  FXHorizontalFrame *baseframe=new FXHorizontalFrame(modeframe,FRAME_SUNKEN|LAYOUT_FILL_X|LAYOUT_FILL_Y|PACK_UNIFORM_WIDTH, 0,0,0,0, 0,0,0,0 ,0,0);

  numbase[0]=new FXButton(baseframe,"&Hex",NULL,this,ID_HEX,FRAME_RAISED|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  numbase[1]=new FXButton(baseframe,"D&ec",NULL,this,ID_DEC,FRAME_RAISED|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  numbase[2]=new FXButton(baseframe,"&Oct",NULL,this,ID_OCT,FRAME_RAISED|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  numbase[3]=new FXButton(baseframe,"&Bin",NULL,this,ID_BIN,FRAME_RAISED|LAYOUT_FILL_X|LAYOUT_FILL_Y);

  FXHorizontalFrame *degframe=new FXHorizontalFrame(modeframe,FRAME_SUNKEN|LAYOUT_FILL_X|LAYOUT_FILL_Y|PACK_UNIFORM_WIDTH, 0,0,0,0, 0,0,0,0 ,0,0);

  angmode[0]=new FXButton(degframe,"&Deg",NULL,this,ID_DEG,FRAME_RAISED|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 2,2,2,2);
  angmode[1]=new FXButton(degframe,"&Rad",NULL,this,ID_RAD,FRAME_RAISED|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 2,2,2,2);
  angmode[2]=new FXButton(degframe,"&Gra",NULL,this,ID_GRA,FRAME_RAISED|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 2,2,2,2);

  // Frame for button blocks
  FXHorizontalFrame *buttonframe=new FXHorizontalFrame(this,LAYOUT_FILL_X|LAYOUT_FILL_Y|PACK_UNIFORM_WIDTH, 0,0,0,0, 4,4,4,4, 0,0);

  // Functions block
  FXMatrix *funcblock=new FXMatrix(buttonframe,6,MATRIX_BY_ROWS|LAYOUT_FILL_X|LAYOUT_FILL_Y|PACK_UNIFORM_WIDTH|PACK_UNIFORM_HEIGHT);
  inverse=new FXButton(funcblock,"inv",NULL,this,ID_INV,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);
  functions[0]=new FXButton(funcblock,"+/-",NULL,this,ID_PLUSMIN,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);
  functions[1]=new FXButton(funcblock,"1/x",NULL,this,ID_RECIP,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);
  functions[2]=new FXButton(funcblock,"x^y",NULL,this,ID_XTOY,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);
  functions[3]=new FXButton(funcblock,"sqrt",NULL,this,ID_SQRT,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);
  functions[4]=new FXButton(funcblock,"2log",NULL,this,ID_2LOG,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);

  functions[5]=new FXButton(funcblock,"pi",NULL,this,ID_PI,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);
  functions[6]=new FXButton(funcblock,"SHL",NULL,this,ID_SHL,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);
  functions[7]=new FXButton(funcblock,"SHR",NULL,this,ID_SHR,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);
  functions[8]=new FXButton(funcblock,"x!",NULL,this,ID_FAC,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);
  functions[9]=new FXButton(funcblock,"nPr",NULL,this,ID_PER,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);
  functions[10]=new FXButton(funcblock,"nCr",NULL,this,ID_COM,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);

  hyper2=new FXButton(funcblock,"hyp",NULL,this,ID_HYP,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);
  functions[11]=new FXButton(funcblock,"sin",NULL,this,ID_SIN,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);
  functions[12]=new FXButton(funcblock,"cos",NULL,this,ID_COS,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);
  functions[13]=new FXButton(funcblock,"tan",NULL,this,ID_TAN,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);
  functions[14]=new FXButton(funcblock,"log",NULL,this,ID_LOG,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);
  functions[15]=new FXButton(funcblock,"ln",NULL,this,ID_LN,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);

  digit[10]=new FXButton(funcblock,"A",NULL,this,ID_A,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);
  digit[11]=new FXButton(funcblock,"B",NULL,this,ID_B,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);
  digit[12]=new FXButton(funcblock,"C",NULL,this,ID_C,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);
  digit[13]=new FXButton(funcblock,"D",NULL,this,ID_D,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);
  digit[14]=new FXButton(funcblock,"E",NULL,this,ID_E,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);
  digit[15]=new FXButton(funcblock,"F",NULL,this,ID_F,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 8,8,1,1);

  // Main block
  FXMatrix *mainblock=new FXMatrix(buttonframe,5,MATRIX_BY_ROWS|LAYOUT_FILL_X|LAYOUT_FILL_Y|PACK_UNIFORM_WIDTH|PACK_UNIFORM_HEIGHT);
  memory[0]=new FXButton(mainblock,"MR",NULL,this,ID_MEM_REC,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  digit[7]=new FXButton(mainblock,"7",NULL,this,ID_7,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  digit[4]=new FXButton(mainblock,"4",NULL,this,ID_4,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  digit[1]=new FXButton(mainblock,"1",NULL,this,ID_1,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  digit[0]=new FXButton(mainblock,"0",NULL,this,ID_0,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);

  memory[1]=new FXButton(mainblock,"M+",NULL,this,ID_MEM_ADD,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  digit[8]=new FXButton(mainblock,"8",NULL,this,ID_8,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  digit[5]=new FXButton(mainblock,"5",NULL,this,ID_5,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  digit[2]=new FXButton(mainblock,"2",NULL,this,ID_2,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  operators[0]=new FXButton(mainblock,".",NULL,this,ID_PNT,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);

  memory[2]=new FXButton(mainblock,"M-",NULL,this,ID_MEM_SUB,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  digit[9]=new FXButton(mainblock,"9",NULL,this,ID_9,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  digit[6]=new FXButton(mainblock,"6",NULL,this,ID_6,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  digit[3]=new FXButton(mainblock,"3",NULL,this,ID_3,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  operators[1]=new FXButton(mainblock,"EXP",NULL,this,ID_EXP,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);

  memory[3]=new FXButton(mainblock,"MC",NULL,this,ID_MEM_CLR,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  operators[2]=new FXButton(mainblock,"(",NULL,this,ID_LPAR,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  operators[3]=new FXButton(mainblock,"*",NULL,this,ID_MUL,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  operators[4]=new FXButton(mainblock,"+",NULL,this,ID_ADD,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  operators[5]=new FXButton(mainblock,"=",NULL,this,ID_ENTER,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);

  clearbtn=new FXButton(mainblock,"C",NULL,this,ID_CLEAR,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  operators[6]=new FXButton(mainblock,")",NULL,this,ID_RPAR,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  operators[7]=new FXButton(mainblock,"/",NULL,this,ID_DIV,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  operators[8]=new FXButton(mainblock,"-",NULL,this,ID_SUB,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  operators[9]=new FXButton(mainblock,"mod",NULL,this,ID_MOD,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);

  clearallbtn=new FXButton(mainblock,"AC",NULL,this,ID_CLEARALL,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  operators[10]=new FXButton(mainblock,"AND",NULL,this,ID_AND,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  operators[11]=new FXButton(mainblock,"OR",NULL,this,ID_OR,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  operators[12]=new FXButton(mainblock,"XOR",NULL,this,ID_XOR,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);
  operators[13]=new FXButton(mainblock,"NOT",NULL,this,ID_NOT,BUTTON_NORMAL|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 1,1,2,2);

  // Hot keys for digits
  digit[0]->addHotKey(MKUINT(KEY_0,0));
  digit[0]->addHotKey(MKUINT(KEY_KP_0,0));
  digit[1]->addHotKey(MKUINT(KEY_1,0));
  digit[1]->addHotKey(MKUINT(KEY_KP_1,0));
  digit[2]->addHotKey(MKUINT(KEY_2,0));
  digit[2]->addHotKey(MKUINT(KEY_KP_2,0));
  digit[3]->addHotKey(MKUINT(KEY_3,0));
  digit[3]->addHotKey(MKUINT(KEY_KP_3,0));
  digit[4]->addHotKey(MKUINT(KEY_4,0));
  digit[4]->addHotKey(MKUINT(KEY_KP_4,0));
  digit[5]->addHotKey(MKUINT(KEY_5,0));
  digit[5]->addHotKey(MKUINT(KEY_KP_5,0));
  digit[6]->addHotKey(MKUINT(KEY_6,0));
  digit[6]->addHotKey(MKUINT(KEY_KP_6,0));
  digit[7]->addHotKey(MKUINT(KEY_7,0));
  digit[7]->addHotKey(MKUINT(KEY_KP_7,0));
  digit[8]->addHotKey(MKUINT(KEY_8,0));
  digit[8]->addHotKey(MKUINT(KEY_KP_8,0));
  digit[9]->addHotKey(MKUINT(KEY_9,0));
  digit[9]->addHotKey(MKUINT(KEY_KP_9,0));

  // Hot keys for hex
  digit[10]->addHotKey(MKUINT(KEY_a,0));
  digit[11]->addHotKey(MKUINT(KEY_b,0));
  digit[12]->addHotKey(MKUINT(KEY_c,0));
  digit[13]->addHotKey(MKUINT(KEY_d,0));
  digit[14]->addHotKey(MKUINT(KEY_e,0));
  digit[15]->addHotKey(MKUINT(KEY_f,0));

  // Hot keys for operators
  operators[0]->addHotKey(MKUINT(KEY_period,0));
  operators[0]->addHotKey(MKUINT(KEY_KP_Decimal,0));
  operators[1]->addHotKey(MKUINT(KEY_E,SHIFTMASK));
  operators[2]->addHotKey(MKUINT(KEY_parenleft,SHIFTMASK));
  operators[3]->addHotKey(MKUINT(KEY_asterisk,SHIFTMASK));
  operators[3]->addHotKey(MKUINT(KEY_KP_Multiply,0));
  operators[4]->addHotKey(MKUINT(KEY_plus,SHIFTMASK));
  operators[4]->addHotKey(MKUINT(KEY_KP_Add,0));
  operators[5]->addHotKey(MKUINT(KEY_equal,0));
  operators[6]->addHotKey(MKUINT(KEY_parenright,SHIFTMASK));
  operators[7]->addHotKey(MKUINT(KEY_slash,0));
  operators[7]->addHotKey(MKUINT(KEY_KP_Divide,0));
  operators[8]->addHotKey(MKUINT(KEY_minus,0));
  operators[8]->addHotKey(MKUINT(KEY_KP_Subtract,0));
  operators[9]->addHotKey(MKUINT(KEY_percent,SHIFTMASK));
  operators[10]->addHotKey(MKUINT(KEY_ampersand,SHIFTMASK));
  operators[11]->addHotKey(MKUINT(KEY_bar,SHIFTMASK));
  operators[12]->addHotKey(MKUINT(KEY_asciicircum,SHIFTMASK));
  operators[13]->addHotKey(MKUINT(KEY_asciitilde,SHIFTMASK));

  // Shifting
  functions[6]->addHotKey(MKUINT(KEY_less,SHIFTMASK));
  functions[7]->addHotKey(MKUINT(KEY_greater,SHIFTMASK));
  functions[8]->addHotKey(MKUINT(KEY_exclam,SHIFTMASK));

  inverse->addHotKey(MKUINT(KEY_i,0));
  hyper2->addHotKey(MKUINT(KEY_h,0));

  // Add accelerators
  getAccelTable()->addAccel(MKUINT(KEY_Q,0),this,FXSEL(SEL_COMMAND,ID_CLOSE));
  getAccelTable()->addAccel(MKUINT(KEY_q,0),this,FXSEL(SEL_COMMAND,ID_CLOSE));
  getAccelTable()->addAccel(MKUINT(KEY_q,CONTROLMASK),this,FXSEL(SEL_COMMAND,ID_CLOSE));
  getAccelTable()->addAccel(MKUINT(KEY_Escape,0),this,FXSEL(SEL_COMMAND,ID_CLEAR));
  getAccelTable()->addAccel(MKUINT(KEY_BackSpace,0),this,FXSEL(SEL_COMMAND,ID_DELETE));
  getAccelTable()->addAccel(MKUINT(KEY_Delete,0),this,FXSEL(SEL_COMMAND,ID_DELETE));
  getAccelTable()->addAccel(MKUINT(KEY_KP_Delete,0),this,FXSEL(SEL_COMMAND,ID_DELETE));
  getAccelTable()->addAccel(MKUINT(KEY_Return,0),this,FXSEL(SEL_COMMAND,ID_ENTER));
  getAccelTable()->addAccel(MKUINT(KEY_KP_Enter,0),this,FXSEL(SEL_COMMAND,ID_ENTER));

  // Initialize stuff
  display->setText("0");
  recall=0.0;
  numstack[0]=0.0;
  numsp=0;
  opsp=-1;
  limit=DECIMAL_LIMIT;
  digits=1;
  base=NUM_DEC;
  angles=ANG_RAD;
  precision=16;
  exponent=MAYBE;
  beep=TRUE;
  parens=0;
  modifiers=0;
  }


// Create and show window
void Calculator::create(){
  readRegistry();
  FXMainWindow::create();
  show();
  }


// Destroy calculator dialog
Calculator::~Calculator(){
  delete font;
  delete bigicon;
  delete smallicon;
  delete cmem;
  delete quest;
  }


/*******************************************************************************/


// Set digit color
void Calculator::setDigitColor(FXColor clr){
  FXColor hilite=makeHiliteColor(clr);
  FXColor shadow=makeShadowColor(clr);
  for(FXuint i=0; i<10; i++){
    digit[i]->setBackColor(clr);
    digit[i]->setHiliteColor(hilite);
    digit[i]->setShadowColor(shadow);
    }
  }


// Get digit color
FXColor Calculator::getDigitColor() const {
  return digit[0]->getBackColor();
  }


// Set digit color
void Calculator::setHexDigitColor(FXColor clr){
  FXColor hilite=makeHiliteColor(clr);
  FXColor shadow=makeShadowColor(clr);
  for(FXuint i=10; i<16; i++){
    digit[i]->setBackColor(clr);
    digit[i]->setHiliteColor(hilite);
    digit[i]->setShadowColor(shadow);
    }
  }


// Get digit color
FXColor Calculator::getHexDigitColor() const {
  return digit[10]->getBackColor();
  }


// Set operator color
void Calculator::setOperatorColor(FXColor clr){
  FXColor hilite=makeHiliteColor(clr);
  FXColor shadow=makeShadowColor(clr);
  for(FXuint i=0; i<14; i++){
    operators[i]->setBackColor(clr);
    operators[i]->setHiliteColor(hilite);
    operators[i]->setShadowColor(shadow);
    }
  }


// Get operator color
FXColor Calculator::getOperatorColor() const {
  return operators[0]->getBackColor();
  }


// Set function color
void Calculator::setFunctionColor(FXColor clr){
  FXColor hilite=makeHiliteColor(clr);
  FXColor shadow=makeShadowColor(clr);
  for(FXuint i=0; i<16; i++){
    functions[i]->setBackColor(clr);
    functions[i]->setHiliteColor(hilite);
    functions[i]->setShadowColor(shadow);
    }
  }


// Get function color
FXColor Calculator::getFunctionColor() const {
  return functions[0]->getBackColor();
  }


// Set memory color
void Calculator::setMemoryColor(FXColor clr){
  FXColor hilite=makeHiliteColor(clr);
  FXColor shadow=makeShadowColor(clr);
  for(FXuint i=0; i<4; i++){
    memory[i]->setBackColor(clr);
    memory[i]->setHiliteColor(hilite);
    memory[i]->setShadowColor(shadow);
    }
  }


// Get memory color
FXColor Calculator::getMemoryColor() const {
  return memory[0]->getBackColor();
  }


// Set inverse color
void Calculator::setInverseColor(FXColor clr){
  inverse->setBackColor(clr);
  inverse->setHiliteColor(makeHiliteColor(clr));
  inverse->setShadowColor(makeShadowColor(clr));
  }


// Get inverse color
FXColor Calculator::getInverseColor() const {
  return inverse->getBackColor();
  }


// Set hyp color
void Calculator::setHyperColor(FXColor clr){
  hyper2->setBackColor(clr);
  hyper2->setHiliteColor(makeHiliteColor(clr));
  hyper2->setShadowColor(makeShadowColor(clr));
  }


// Get hyp color
FXColor Calculator::getHyperColor() const {
  return hyper2->getBackColor();
  }


// Set clear color
void Calculator::setClearColor(FXColor clr){
  clearbtn->setBackColor(clr);
  clearbtn->setHiliteColor(makeHiliteColor(clr));
  clearbtn->setShadowColor(makeShadowColor(clr));
  }


// Get clear color
FXColor Calculator::getClearColor() const {
  return clearbtn->getBackColor();
  }


// Set clear all color
void Calculator::setClearAllColor(FXColor clr){
  clearallbtn->setBackColor(clr);
  clearallbtn->setHiliteColor(makeHiliteColor(clr));
  clearallbtn->setShadowColor(makeShadowColor(clr));
  }


// Get clear all color
FXColor Calculator::getClearAllColor() const {
  return clearallbtn->getBackColor();
  }


// Set display color
void Calculator::setDisplayColor(FXColor clr){
  display->setBackColor(clr);
  display->setSelTextColor(clr);
  display->setHiliteColor(makeHiliteColor(clr));
  display->setShadowColor(makeShadowColor(clr));
  }


// Get display color
FXColor Calculator::getDisplayColor() const {
  return display->getBackColor();
  }


// Set display color
void Calculator::setDisplayNumberColor(FXColor clr){
  display->setTextColor(clr);
  display->setSelBackColor(clr);
  }


// Get display color
FXColor Calculator::getDisplayNumberColor() const {
  return display->getTextColor();
  }


// Set numeric base color
void Calculator::setBaseColor(FXColor clr){
  FXColor hilite=FXRGB(FXREDVAL(clr)+((255-FXREDVAL(clr))*3)/8,
                       FXGREENVAL(clr)+((255-FXGREENVAL(clr))*3)/8,
                       FXBLUEVAL(clr)+((255-FXBLUEVAL(clr))*3)/8);
  FXColor shadow=makeShadowColor(clr);
  for(FXuint i=0; i<4; i++){
    numbase[i]->setBackColor(clr);
    numbase[i]->setHiliteColor(hilite);
    numbase[i]->setShadowColor(shadow);
    }
  }


// Get numeric base  color
FXColor Calculator::getBaseColor() const {
  return numbase[0]->getBackColor();
  }


// Set angle mode color
void Calculator::setAngleColor(FXColor clr){
  FXColor hilite=FXRGB(FXREDVAL(clr)+((255-FXREDVAL(clr))*3)/8,
                       FXGREENVAL(clr)+((255-FXGREENVAL(clr))*3)/8,
                       FXBLUEVAL(clr)+((255-FXBLUEVAL(clr))*3)/8);
  FXColor shadow=makeShadowColor(clr);
  for(FXuint i=0; i<3; i++){
    angmode[i]->setBackColor(clr);
    angmode[i]->setHiliteColor(hilite);
    angmode[i]->setShadowColor(shadow);
    }
  }


// Get angle mode color
FXColor Calculator::getAngleColor() const {
  return angmode[0]->getBackColor();
  }


// Set display font
void Calculator::setDisplayFont(FXFont* font){
  display->setFont(font);
  }


// Return display font
FXFont* Calculator::getDisplayFont() const {
  return display->getFont();
  }


// Read registry
void Calculator::readRegistry(){
  FXString fontspec;

  // Position
  FXint xx=getApp()->reg().readIntEntry("SETTINGS","x",50);
  FXint yy=getApp()->reg().readIntEntry("SETTINGS","y",50);
  FXint ww=getApp()->reg().readIntEntry("SETTINGS","w",0);
  FXint hh=getApp()->reg().readIntEntry("SETTINGS","h",0);

  // Read colors
  FXColor digitclr=getApp()->reg().readColorEntry("SETTINGS","digitcolor",FXRGB(94,209,204));
  FXColor hexdigitclr=getApp()->reg().readColorEntry("SETTINGS","hexdigitcolor",FXRGB(151,189,206));
  FXColor operatorclr=getApp()->reg().readColorEntry("SETTINGS","operatorcolor",FXRGB(255,222,163));
  FXColor functionclr=getApp()->reg().readColorEntry("SETTINGS","functioncolor",FXRGB(158,213,188));
  FXColor memoryclr=getApp()->reg().readColorEntry("SETTINGS","memorycolor",FXRGB(181,207,227));
  FXColor inverseclr=getApp()->reg().readColorEntry("SETTINGS","inversecolor",FXRGB(224,222,69));
  FXColor hyperclr=getApp()->reg().readColorEntry("SETTINGS","hypercolor",FXRGB(224,222,69));
  FXColor clearclr=getApp()->reg().readColorEntry("SETTINGS","clearcolor",FXRGB(238,148,0));
  FXColor clearallclr=getApp()->reg().readColorEntry("SETTINGS","clearallcolor",FXRGB(238,118,0));
  FXColor displayclr=getApp()->reg().readColorEntry("SETTINGS","displaycolor",FXRGB(255,255,255));
  FXColor numberclr=getApp()->reg().readColorEntry("SETTINGS","displaynumbercolor",FXRGB(0,0,0));
  FXColor numbaseclr=getApp()->reg().readColorEntry("SETTINGS","numbasecolor",FXRGB(203,203,203));
  FXColor angmodeclr=getApp()->reg().readColorEntry("SETTINGS","anglemodecolor",FXRGB(203,203,203));

  // Number base
  FXint numbase=getApp()->reg().readIntEntry("SETTINGS","base",NUM_DEC);

  // Angle type
  FXint angmode=getApp()->reg().readIntEntry("SETTINGS","angles",ANG_RAD);

  // Exponent mode
  FXbool expmode=(FXExponent)getApp()->reg().readIntEntry("SETTINGS","exponent",MAYBE);

  // Precision
  FXint prec=getApp()->reg().readIntEntry("SETTINGS","precision",10);

  // Beep
  FXbool noise=getApp()->reg().readIntEntry("SETTINGS","beep",TRUE);

  // Memory cell
  recall=getApp()->reg().readRealEntry("SETTINGS","memory",0.0);

  // Font
  fontspec=getApp()->reg().readStringEntry("SETTINGS","displayfont","");
  if(!fontspec.empty()){
    font=new FXFont(getApp(),fontspec);
    setDisplayFont(font);
    }

  setDigitColor(digitclr);
  setHexDigitColor(hexdigitclr);
  setOperatorColor(operatorclr);
  setFunctionColor(functionclr);
  setMemoryColor(memoryclr);
  setInverseColor(inverseclr);
  setHyperColor(hyperclr);
  setClearColor(clearclr);
  setClearAllColor(clearallclr);
  setDisplayColor(displayclr);
  setDisplayNumberColor(numberclr);
  setBaseColor(numbaseclr);
  setAngleColor(angmodeclr);

  // Number base
  setBase(numbase);
  setAngles(angmode);
  setPrecision(prec);
  setExponentMode(expmode);
  setBeep(noise);

  setX(xx);
  setY(yy);
  setWidth(ww);
  setHeight(hh);
  }


// Write registry
void Calculator::writeRegistry(){
  FXString fontspec;

  // Position
  getApp()->reg().writeIntEntry("SETTINGS","x",getX());
  getApp()->reg().writeIntEntry("SETTINGS","y",getY());
  getApp()->reg().writeIntEntry("SETTINGS","w",getWidth());
  getApp()->reg().writeIntEntry("SETTINGS","h",getHeight());

  // Write colors
  getApp()->reg().writeColorEntry("SETTINGS","digitcolor",getDigitColor());
  getApp()->reg().writeColorEntry("SETTINGS","hexdigitcolor",getHexDigitColor());
  getApp()->reg().writeColorEntry("SETTINGS","operatorcolor",getOperatorColor());
  getApp()->reg().writeColorEntry("SETTINGS","functioncolor",getFunctionColor());
  getApp()->reg().writeColorEntry("SETTINGS","memorycolor",getMemoryColor());
  getApp()->reg().writeColorEntry("SETTINGS","inversecolor",getInverseColor());
  getApp()->reg().writeColorEntry("SETTINGS","hypercolor",getHyperColor());
  getApp()->reg().writeColorEntry("SETTINGS","clearcolor",getClearColor());
  getApp()->reg().writeColorEntry("SETTINGS","clearallcolor",getClearAllColor());
  getApp()->reg().writeColorEntry("SETTINGS","displaycolor",getDisplayColor());
  getApp()->reg().writeColorEntry("SETTINGS","displaynumbercolor",getDisplayNumberColor());
  getApp()->reg().writeColorEntry("SETTINGS","numbasecolor",getBaseColor());
  getApp()->reg().writeColorEntry("SETTINGS","anglemodecolor",getAngleColor());

  // Number base
  getApp()->reg().writeIntEntry("SETTINGS","base",getBase());

  // Angle type
  getApp()->reg().writeIntEntry("SETTINGS","angles",getAngles());

  // Exponent mode
  getApp()->reg().writeIntEntry("SETTINGS","exponent",getExponentMode());

  // Precision
  getApp()->reg().writeIntEntry("SETTINGS","precision",getPrecision());

  // Beep
  getApp()->reg().writeIntEntry("SETTINGS","beep",getBeep());

  // Memory contents
  getApp()->reg().writeRealEntry("SETTINGS","memory",recall);

  // Font
  fontspec=getDisplayFont()->getFont();
  getApp()->reg().writeStringEntry("SETTINGS","displayfont",fontspec.text());
  }


/*******************************************************************************/

// Get display text
FXString Calculator::getDisplayText() const {
  return display->getText();
  }


// Display text
void Calculator::setDisplayText(const FXString& txt){
  display->setText(txt);
  }


// Get displayed value
FXdouble Calculator::getDisplayValue() const {
  FXdouble val;
  if(base==10)
    val=FXDoubleVal(getDisplayText());
  else
    val=(FXdouble)FXUIntVal(getDisplayText(),base);
  return val;
  }


// Redisplay new value
void Calculator::setDisplayValue(FXdouble val){
  FXint fp=fxieeedoubleclass(val);
  if(fp==-2 || fp==+2){
    setDisplayText("ERROR");
    if(beep) getApp()->beep();
    }
  else if(fp==-1){
    setDisplayText("-INF");
    if(beep) getApp()->beep();
    }
  else if(fp==+1){
    setDisplayText("+INF");
    if(beep) getApp()->beep();
    }
  else if(base==10){
    if(val==0.0) val=0.0;       // Don't ever print out -0 instead of 0
    setDisplayText(FXStringVal(val,precision,exponent));
    }
  else{
    setDisplayText(FXStringVal((FXuint)floor(val),base));
    }
  }


/*******************************************************************************/

// Push to number stack
FXdouble Calculator::pushnum(FXdouble num){
  FXASSERT(numsp<STACKLEN);
  numstack[++numsp]=num;
  return num;
  }


// Replace number on top of stack
FXdouble Calculator::setnum(FXdouble num){
  FXASSERT(0<=numsp && numsp<STACKLEN);
  numstack[numsp]=num;
  return num;
  }


// Pop number of stack
FXdouble Calculator::popnum(){
  FXASSERT(0<=numsp);
  return numstack[numsp--];
  }


// Return number on top of stack
FXdouble Calculator::getnum(){
  FXASSERT(0<=numsp && numsp<STACKLEN);
  return numstack[numsp];
  }


// Set number base
void Calculator::setBase(FXint b){
  switch(b){
    case  2: base=2;  limit=BINARY_LIMIT; break;
    case  8: base=8;  limit=OCTAL_LIMIT; break;
    case 10: base=10; limit=DECIMAL_LIMIT; break;
    case 16: base=16; limit=HEXADECIMAL_LIMIT; break;
    }
  setDisplayValue(getnum());
  modifiers=0;
  }


// Set exponent mode
void Calculator::setExponentMode(FXbool expmode){
  exponent=expmode;
  setDisplayValue(getnum());
  modifiers=0;
  }


// Set precision
void Calculator::setPrecision(FXint prec){
  precision=prec;
  setDisplayValue(getnum());
  modifiers=0;
  }


// Argument to sine, cosine, etc
FXdouble Calculator::trigarg(FXdouble ang) const {
  switch(angles){
    case ANG_DEG: return DEG2RAD(ang);
    case ANG_GRA: return GRA2RAD(ang);
    case ANG_RAD: return ang;
    }
  return ang;
  }


// Result from arcsine, arccosine, etc
FXdouble Calculator::trigres(FXdouble res) const {
  switch(angles){
    case ANG_DEG: return RAD2DEG(res);
    case ANG_GRA: return RAD2GRA(res);
    case ANG_RAD: return res;
    }
  return res;
  }


// Factorial
//
// result = n!
//
static FXdouble factorial(FXdouble n){
  FXdouble num=floor(n);
  FXdouble result=1.0;
  if(0.0<=num && num==n){
    while(num>0.0){
      if(fxieeedoubleclass(result)>0) break;
      result=result*num;
      num=num-1.0;
      }
    return result;
    }
  return dblnan;
  }


// Permutations
//
//             n!
// result =  ------
//            (n-r)!
//
static FXdouble permutations(FXdouble n,FXdouble r){
  FXdouble num=floor(n);
  FXdouble den=floor(r);
  FXdouble result=1.0;
  if(0.0<=num && 0.0<=den && den<=num && num==n && den==r){
    while(den>0.0){
      if(fxieeedoubleclass(result)>0) break;
      result=result*num;
      num=num-1.0;
      den=den-1.0;
      }
    return result;
    }
  return dblnan;
  }


// Combinations
//
//               n!
// result =  ----------
//            r! (n-r)!
//
static FXdouble combinations(FXdouble n,FXdouble r){
  FXdouble num=floor(n);
  FXdouble den=floor(r);
  FXdouble res1=1.0;
  FXdouble res2=1.0;
  if(0.0<=num && 0.0<=den && den<=num && num==n && den==r){
    while(den>0.0){
      if(fxieeedoubleclass(res1)>0) break;
      res1=res1*num;
      res2=res2*den;
      num=num-1.0;
      den=den-1.0;
      }
    return res1/res2;
    }
  return dblnan;
  }


// Reset calculator
void Calculator::clearAll(){
  setDisplayValue(0.0);
  numstack[0]=0.0;
  numsp=0;
  opsp=-1;
  parens=0;
  modifiers=0;
  }


// Clear calculator
void Calculator::clear(){
  setDisplayValue(0.0);
  setnum(0.0);
  modifiers=0;
  }


// Perform unary operator
void Calculator::unary(FXuchar op){
  FXdouble acc,val;
  FXASSERT(0<=numsp);
  val=getnum();
  acc=0.0;
  switch(op){
    case UN_NOT:
      acc=(FXdouble) (~((FXuint)floor(val)));
      break;
    case UN_NEG:
      acc=-val;
      break;
    case UN_SHL:
      acc=(FXdouble) (((FXuint)floor(val))<<1);
      break;
    case UN_SHR:
      acc=(FXdouble) (((FXuint)floor(val))>>1);
      break;
    case UN_SAR:
      acc=(FXdouble) ((((FXuint)floor(val))>>1) | (((FXuint)floor(val))&0x80000000));
      break;
    case UN_RECIP:
      acc=1.0/val;
      break;
    case UN_FAC:
      acc=factorial(val);
      break;
    case UN_SQRT:
      acc=sqrt(val);
      break;
    case UN_QUAD:
      acc=val*val;
      break;
    case UN_2LOG:
      acc=log(val)/log(2.0);
      break;
    case UN_2TOX:
      acc=pow(2.0,val);
      break;
    case UN_LOG:
      acc=log10(val);
      break;
    case UN_10TOX:
      acc=pow(10.0,val);
      break;
    case UN_LN:
      acc=log(val);
      break;
    case UN_EXP:
      acc=exp(val);
      break;
    case UN_SIN:
      acc=sin(trigarg(val));
      break;
    case UN_COS:
      acc=cos(trigarg(val));
      break;
    case UN_TAN:
      acc=tan(trigarg(val));
      break;
    case UN_ASIN:
      acc=trigres(asin(val));
      break;
    case UN_ACOS:
      acc=trigres(acos(val));
      break;
    case UN_ATAN:
      acc=trigres(atan(val));
      break;
    case UN_SINH:
      acc=sinh(val);
      break;
    case UN_COSH:
      acc=cosh(val);
      break;
    case UN_TANH:
      acc=tanh(val);
      break;
    case UN_ASINH:
      acc=log(val+sqrt(val*val+1.0));   // Tired of #ifdef's:- just expand definitions (Abramowitz & Stegun, pp. 87)
      break;
    case UN_ACOSH:
      acc=log(val+sqrt(val*val-1.0));   // Same here
      break;
    case UN_ATANH:
      acc=0.5*log((1.0+val)/(1.0-val)); // And here
    default:
      break;
    }
  setnum(acc);
  setDisplayValue(acc);
  modifiers=0;
  }


// Perform operator
void Calculator::dyop(FXuchar op){
  FXdouble acc,val;
  FXASSERT(0<=numsp);
  val=popnum();
  FXASSERT(0<=numsp);
  acc=getnum();
  switch(op){
    case DY_OR:
      acc=(FXdouble) (((FXuint)floor(acc)) | ((FXuint)floor(val)));
      break;
    case DY_XOR:
      acc=(FXdouble) (((FXuint)floor(acc)) ^ ((FXuint)floor(val)));
      break;
    case DY_AND:
      acc=(FXdouble) (((FXuint)floor(acc)) & ((FXuint)floor(val)));
      break;
    case DY_SUB:
      acc=acc-val;
      break;
    case DY_ADD:
      acc=acc+val;
      break;
    case DY_MOD:                // Theo Veenker <Theo.Veenker@let.uu.nl> suggested this new definition of "mod":
      val=fabs(val);            // x = a div |b|        ; with a round toward 0
      acc=fmod(acc,val);        // y = a mod |b|
      break;                    // a = x * |b| + y
    case DY_IDIV:
      modf(acc/val,&acc);
      break;
    case DY_DIV:
      acc=acc/val;
      break;
    case DY_MUL:
      acc=acc*val;
      break;
    case DY_XTOY:
      acc=pow(acc,val);
      break;
    case DY_XTOINVY:
      acc=pow(acc,1.0/val);
      break;
    case DY_PER:
      acc=permutations(acc,val);
      break;
    case DY_COM:
      acc=combinations(acc,val);
    default:
      break;
    }
  setnum(acc);
  setDisplayValue(acc);
  modifiers=0;
  }


// Enter operator
void Calculator::dyadic(FXuchar op){
  if(opsp<0 || opstack[opsp]==DY_LPAR || priority[op]>priority[opstack[opsp]]){
    pushnum(getnum());
    opstack[++opsp]=op;
    }
  else{
    dyop(opstack[opsp]);
    pushnum(getnum());
    opstack[opsp]=op;
    }
  modifiers=0;
  }


// Enter evaluate
void Calculator::evaluate(){
  register FXuchar op;
  while(0<=opsp){
    op=opstack[opsp--];
    if(op!=DY_LPAR)
      dyop(op);
    else
      parens--;
    }
  setDisplayValue(getnum());
  modifiers=0;
  }


// Left parentheses
void Calculator::lparen(){
  opstack[++opsp]=DY_LPAR;
  setnum(0.0);
  setDisplayValue(0.0);
  parens++;
  modifiers=0;
  }


// Right parentheses
void Calculator::rparen(){
  register FXuchar op;
  while(0<=opsp){
    op=opstack[opsp--];
    if(op==DY_LPAR){ parens--; break; }
    dyop(op);
    }
  setDisplayValue(getnum());
  modifiers=0;
  }


/*******************************************************************************/

// Close the window and save registry
FXbool Calculator::close(FXbool notify){
  writeRegistry();
  return FXMainWindow::close(notify);
  }


// Change preferences
long Calculator::onCmdPreferences(FXObject*,FXSelector,void*){
  Preferences preferences(this);
  preferences.setX(getX()+80);
  preferences.setY(getY()+80);
  preferences.execute(PLACEMENT_DEFAULT);
  return 1;
  }


// Change colors
long Calculator::onCmdColor(FXObject*,FXSelector sel,void* ptr){
  FXColor clr=(FXColor)(FXuval)ptr;
  switch(FXSELID(sel)){
    case ID_COLOR_DISPLAY: setDisplayColor(clr); break;
    case ID_COLOR_DISPLAYNUMBER: setDisplayNumberColor(clr); break;
    case ID_COLOR_DIGITS: setDigitColor(clr); break;
    case ID_COLOR_HEXDIGITS: setHexDigitColor(clr); break;
    case ID_COLOR_OPERATORS: setOperatorColor(clr); break;
    case ID_COLOR_FUNCTIONS: setFunctionColor(clr); break;
    case ID_COLOR_MEMORY: setMemoryColor(clr); break;
    case ID_COLOR_BASE: setBaseColor(clr); break;
    case ID_COLOR_ANGLES: setAngleColor(clr); break;
    case ID_COLOR_INVERT: setInverseColor(clr); break;
    case ID_COLOR_HYPER: setHyperColor(clr); break;
    case ID_COLOR_CLEARALL: setClearAllColor(clr); break;
    case ID_COLOR_CLEAR: setClearColor(clr); break;
    }
  return 1;
  }


// Update colors
long Calculator::onUpdColor(FXObject* sender,FXSelector sel,void*){
  FXColor clr;
  switch(FXSELID(sel)){
    case ID_COLOR_DISPLAY: clr=getDisplayColor(); break;
    case ID_COLOR_DISPLAYNUMBER: clr=getDisplayNumberColor(); break;
    case ID_COLOR_DIGITS: clr=getDigitColor(); break;
    case ID_COLOR_HEXDIGITS: clr=getHexDigitColor(); break;
    case ID_COLOR_OPERATORS: clr=getOperatorColor(); break;
    case ID_COLOR_FUNCTIONS: clr=getFunctionColor(); break;
    case ID_COLOR_MEMORY: clr=getMemoryColor(); break;
    case ID_COLOR_BASE: clr=getBaseColor(); break;
    case ID_COLOR_ANGLES: clr=getAngleColor(); break;
    case ID_COLOR_INVERT: clr=getInverseColor(); break;
    case ID_COLOR_HYPER: clr=getHyperColor(); break;
    case ID_COLOR_CLEARALL: clr=getClearAllColor(); break;
    case ID_COLOR_CLEAR: clr=getClearColor(); break;
    }
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&clr);
  return 1;
  }


// Change font
long Calculator::onCmdFont(FXObject*,FXSelector,void*){
  FXFontDialog fontdlg(this,"Change Display Font",DECOR_BORDER|DECOR_TITLE);
  FXFontDesc fontdesc;
  getDisplayFont()->getFontDesc(fontdesc);
  fontdlg.setFontSelection(fontdesc);
  if(fontdlg.execute()){
    FXFont *oldfont=font;
    fontdlg.getFontSelection(fontdesc);
    font=new FXFont(getApp(),fontdesc);
    font->create();
    setDisplayFont(font);
    delete oldfont;
    }
  return 1;
  }


// Change exponential notation
long Calculator::onCmdExponent(FXObject*,FXSelector sel,void* ptr){
  if(FXSELID(sel)==ID_EXPONENT_ALWAYS && ptr) setExponentMode(TRUE);
  else if(FXSELID(sel)==ID_EXPONENT_NEVER && ptr) setExponentMode(FALSE);
  else setExponentMode(MAYBE);
  return 1;
  }


// Update exponential notation
long Calculator::onUpdExponent(FXObject* sender,FXSelector sel,void*){
  if(FXSELID(sel)==ID_EXPONENT_ALWAYS && exponent==TRUE)
    sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
  else if(FXSELID(sel)==ID_EXPONENT_NEVER && exponent==FALSE)
    sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
  else
    sender->handle(this,FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Change precision
long Calculator::onCmdPrecision(FXObject* sender,FXSelector,void*){
  FXint prec=16;
  sender->handle(this,FXSEL(SEL_COMMAND,ID_GETINTVALUE),(void*)&prec);
  setPrecision(prec);
  return 1;
  }


// Update precision
long Calculator::onUpdPrecision(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&precision);
  return 1;
  }


// Change beep mode
long Calculator::onCmdBeep(FXObject*,FXSelector,void*){
  beep=!beep;
  return 1;
  }


// Update beep mode
long Calculator::onUpdBeep(FXObject* sender,FXSelector,void*){
  sender->handle(this,beep ? FXSEL(SEL_COMMAND,ID_CHECK) : FXSEL(SEL_COMMAND,ID_UNCHECK), NULL);
  return 1;
  }




// Popup help
long Calculator::onCmdQuestion(FXObject*,FXSelector,void*){
  HelpWindow helpwindow(this,"Calculator Help");
  helpwindow.setHelp(help);
  helpwindow.setX(getX()+80);
  helpwindow.setY(getY()+80);
  helpwindow.execute(PLACEMENT_DEFAULT);
  return 1;
  }


// Change angle mode
long Calculator::onCmdAngle(FXObject*,FXSelector sel,void*){
  angles=(FXSELID(sel)-ID_MODE);
  return 1;
  }


// Update radio button for angle mode
long Calculator::onUpdAngle(FXObject* sender,FXSelector sel,void*){
  sender->handle(this,angles==(FXSELID(sel)-ID_MODE) ? FXSEL(SEL_COMMAND,ID_CHECK) : FXSEL(SEL_COMMAND,ID_UNCHECK), NULL);
  return 1;
  }


// Change angle mode
long Calculator::onCmdBase(FXObject*,FXSelector sel,void*){
  setBase(FXSELID(sel)-ID_BASE);
  return 1;
  }


// Update radio button for angle mode
long Calculator::onUpdBase(FXObject* sender,FXSelector sel,void*){
  sender->handle(this,base==(FXSELID(sel)-ID_BASE) ? FXSEL(SEL_COMMAND,ID_CHECK) : FXSEL(SEL_COMMAND,ID_UNCHECK), NULL);
  return 1;
  }


// Update digits based on base
long Calculator::onUpdDigit(FXObject* sender,FXSelector sel,void*){
  sender->handle(this,(FXSELID(sel)-ID_0)<base ? FXSEL(SEL_COMMAND,ID_ENABLE) : FXSEL(SEL_COMMAND,ID_DISABLE), NULL);
  return 1;
  }


// Update digits based on base
long Calculator::onCmdDigit(FXObject*,FXSelector sel,void*){
  FXString text=getDisplayText();
  FXint pos;
  if(!(modifiers&MOD_ENT)){ text=""; digits=0; }
  if((base==10) && (pos=text.find('E'))>=0){
    pos++;                                          // Skip 'E'
    if(text[pos]=='-' || text[pos]=='+') pos++;     // Skip sign
    if(text[pos]=='0' || (text[pos] && text[pos+1] && text[pos+2])){
      while(text[pos+1]){ text[pos]=text[pos+1]; pos++; }
      text[pos]=FXString::HEX[FXSELID(sel)-ID_0];
      }
    else{
      text.append(FXString::HEX[FXSELID(sel)-ID_0]);
      }
    }
  else if(digits<limit){
    text+=FXString::HEX[FXSELID(sel)-ID_0];
    digits++;
    }
  setDisplayText(text);
  setnum(getDisplayValue());
  modifiers|=MOD_ENT;
  return 1;
  }


// Decimal point
long Calculator::onCmdPoint(FXObject*,FXSelector,void*){
  FXString text=getDisplayText();
  if(!(modifiers&MOD_ENT)){ text="0"; digits=1; }
  if(base==10 && text.find('.')<0 && text.find('E')<0) text+='.';
  setDisplayText(text);
  setnum(getDisplayValue());
  modifiers|=MOD_ENT;
  return 1;
  }


// Update decimal point
long Calculator::onUpdPoint(FXObject* sender,FXSelector,void*){
  sender->handle(this,(base==10) ? FXSEL(SEL_COMMAND,ID_ENABLE) : FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Exponent
long Calculator::onCmdExp(FXObject*,FXSelector,void*){
  FXString text=getDisplayText();
  if(!(modifiers&MOD_ENT)){ text="0"; digits=1; }
  if(base==10 && text.find('E')<0) text+="E+0";
  setDisplayText(text);
  setnum(getDisplayValue());
  modifiers|=MOD_ENT;
  return 1;
  }


// Update exponent
long Calculator::onUpdExp(FXObject* sender,FXSelector,void*){
  sender->handle(this,(base==10) ? FXSEL(SEL_COMMAND,ID_ENABLE) : FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Plus minus +/-
long Calculator::onCmdPlusMin(FXObject*,FXSelector,void*){
  FXString text=getDisplayText();
  FXint pos;
  if(modifiers&MOD_ENT){
    if((base==10) && (pos=text.find('E'))>=0){
      if(text[pos+1]=='+') text[pos+1]='-';
      else if(text[pos+1]=='-') text[pos+1]='+';
      else text.insert(pos+1,'-');
      }
    else{
      if(text[0]=='-') text.erase(0);
      else if(text[0]=='+') text[0]='-';
      else if(text!="0") text.prepend('-');
      }
    setDisplayText(text);
    setnum(getDisplayValue());
    }
  else{
    unary(UN_NEG);
    }
  return 1;
  }


// Delete last character
long Calculator::onCmdDelete(FXObject*,FXSelector,void*){
  FXString text=getDisplayText();
  FXint len;
  if(modifiers&MOD_ENT){
    len=text.length();
    if(0<len){
      if(base==10 && text.find('E')>=0){
        len--;
        if(0<len && (text[len-1]=='+' || text[len-1]=='-')) len--;
        if(0<len && text[len-1]=='E') len--;
        }
      else{
        len--;
        if(isdigit(text[len])) digits--;
        if(0<len && (text[len-1]=='+' || text[len-1]=='-')) len--;
        if(0<len && text[len-1]=='.') len--;
        }
      text.trunc(len);
      }
    if(len<=0){
      text="0";
      modifiers&=~MOD_ENT;
      digits=1;
      }
    setDisplayText(text);
    setnum(getDisplayValue());
    }
  else{
    clear();
    }
  return 1;
  }


// Clear entry
long Calculator::onCmdClear(FXObject*,FXSelector,void*){
  clear();
  return 1;
  }


// Clear entry
long Calculator::onCmdClearAll(FXObject*,FXSelector,void*){
  clearAll();
  return 1;
  }


// Inverse
long Calculator::onCmdInverse(FXObject*,FXSelector,void*){
  modifiers^=MOD_INV;
  return 1;
  }


// Hyper
long Calculator::onCmdHyper(FXObject*,FXSelector,void*){
  modifiers^=MOD_HYP;
  return 1;
  }


// Sine button
long Calculator::onCmdSin(FXObject*,FXSelector,void*){
  unary(UN_SIN+(modifiers&(MOD_INV|MOD_HYP)));
  return 1;
  }


// Update sine button
long Calculator::onUpdSin(FXObject* sender,FXSelector,void*){
  FXString label="sin";
  if(modifiers&MOD_INV) label.prepend('a');
  if(modifiers&MOD_HYP) label.append('h');
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&label);
  sender->handle(this,(base==10) ? FXSEL(SEL_COMMAND,ID_ENABLE) : FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Cosine button
long Calculator::onCmdCos(FXObject*,FXSelector,void*){
  unary(UN_COS+(modifiers&(MOD_INV|MOD_HYP)));
  return 1;
  }


// Update cosine button
long Calculator::onUpdCos(FXObject* sender,FXSelector,void*){
  FXString label="cos";
  if(modifiers&MOD_INV) label.prepend('a');
  if(modifiers&MOD_HYP) label.append('h');
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&label);
  sender->handle(this,(base==10) ? FXSEL(SEL_COMMAND,ID_ENABLE) : FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Tangent button
long Calculator::onCmdTan(FXObject*,FXSelector,void*){
  unary(UN_TAN+(modifiers&(MOD_INV|MOD_HYP)));
  return 1;
  }


// Update tangent button
long Calculator::onUpdTan(FXObject* sender,FXSelector,void*){
  FXString label="tan";
  if(modifiers&MOD_INV) label.prepend('a');
  if(modifiers&MOD_HYP) label.append('h');
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&label);
  sender->handle(this,(base==10) ? FXSEL(SEL_COMMAND,ID_ENABLE) : FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Log button
long Calculator::onCmdLog(FXObject*,FXSelector,void*){
  unary(UN_LOG+(modifiers&MOD_INV));
  return 1;
  }


// Update Log button
long Calculator::onUpdLog(FXObject* sender,FXSelector,void*){
  FXString label=(modifiers&MOD_INV)?"10^x":"log";
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&label);
  sender->handle(this,(base==10) ? FXSEL(SEL_COMMAND,ID_ENABLE) : FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Ln button
long Calculator::onCmdLn(FXObject*,FXSelector,void*){
  unary(UN_LN+(modifiers&MOD_INV));
  return 1;
  }


// Update Ln button
long Calculator::onUpdLn(FXObject* sender,FXSelector,void*){
  FXString label=(modifiers&MOD_INV)?"e^x":"ln";
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&label);
  sender->handle(this,(base==10) ? FXSEL(SEL_COMMAND,ID_ENABLE) : FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Update PI button
long Calculator::onCmdPi(FXObject*,FXSelector,void*){
  setnum((modifiers&MOD_HYP)?((modifiers&MOD_INV)?INVGOLDEN:GOLDEN):((modifiers&MOD_INV)?EULER:PI));
  setDisplayValue(getnum());
  modifiers=0;
  return 1;
  }


// Update PI button
long Calculator::onUpdPi(FXObject* sender,FXSelector,void*){
  FXString label=(modifiers&MOD_HYP) ? (modifiers&MOD_INV) ? "1/phi" : "phi" : (modifiers&MOD_INV) ? "e" : "pi";
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&label);
  sender->handle(this,(base==10) ? FXSEL(SEL_COMMAND,ID_ENABLE) : FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Factorial
long Calculator::onCmdFac(FXObject*,FXSelector,void*){
  unary(UN_FAC);
  return 1;
  }


// Update factorial
long Calculator::onUpdFac(FXObject* sender,FXSelector,void*){
  sender->handle(this,(base==10) ? FXSEL(SEL_COMMAND,ID_ENABLE) : FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Permutations
long Calculator::onCmdPer(FXObject*,FXSelector,void*){
  dyadic(DY_PER);
  return 1;
  }


// Update permutations
long Calculator::onUpdPer(FXObject* sender,FXSelector,void*){
  sender->handle(this,(base==10) ? FXSEL(SEL_COMMAND,ID_ENABLE) : FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Combinations
long Calculator::onCmdCom(FXObject*,FXSelector,void*){
  dyadic(DY_COM);
  return 1;
  }


// Update combinations
long Calculator::onUpdCom(FXObject* sender,FXSelector,void*){
  sender->handle(this,(base==10) ? FXSEL(SEL_COMMAND,ID_ENABLE) : FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Reciprocal
long Calculator::onCmdRecip(FXObject*,FXSelector,void*){
  unary(UN_RECIP);
  return 1;
  }


// Update reciprocal
long Calculator::onUpdRecip(FXObject* sender,FXSelector,void*){
  sender->handle(this,(base==10) ? FXSEL(SEL_COMMAND,ID_ENABLE) : FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// X ^ Y
long Calculator::onCmdXToY(FXObject*,FXSelector,void*){
  dyadic(DY_XTOY+(modifiers&MOD_INV));
  return 1;
  }


// Update X ^ Y
long Calculator::onUpdXToY(FXObject* sender,FXSelector,void*){
  FXString label=(modifiers&MOD_INV)?"x^1/y":"x^y";
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&label);
  sender->handle(this,(base==10) ? FXSEL(SEL_COMMAND,ID_ENABLE) : FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Sqrt
long Calculator::onCmdSqrt(FXObject*,FXSelector,void*){
  unary(UN_SQRT+(modifiers&MOD_INV));
  return 1;
  }


// Update Sqrt
long Calculator::onUpdSqrt(FXObject* sender,FXSelector,void*){
  FXString label=(modifiers&MOD_INV)?"x^2":"sqrt";
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&label);
  sender->handle(this,(base==10) ? FXSEL(SEL_COMMAND,ID_ENABLE) : FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Shift left
long Calculator::onCmdShl(FXObject*,FXSelector,void*){
  unary(UN_SHL);
  return 1;
  }


// Update Shift left
long Calculator::onUpdShl(FXObject* sender,FXSelector,void*){
  FXString label="SHL";
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&label);
  return 1;
  }


// Shift right
long Calculator::onCmdShr(FXObject*,FXSelector,void*){
  unary(UN_SHR+(modifiers&MOD_INV));
  return 1;
  }


// Update Shift right
long Calculator::onUpdShr(FXObject* sender,FXSelector,void*){
  FXString label=(modifiers&MOD_INV)?"SAR":"SHR";
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&label);
  return 1;
  }


// Base 2 log
long Calculator::onCmd2Log(FXObject*,FXSelector,void*){
  unary(UN_2LOG+(modifiers&MOD_INV));
  return 1;
  }


// Update Base 2 log
long Calculator::onUpd2Log(FXObject* sender,FXSelector,void*){
  FXString label=(modifiers&MOD_INV)?"2^x":"2log";
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&label);
  sender->handle(this,(base==10) ? FXSEL(SEL_COMMAND,ID_ENABLE) : FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Left parenth
long Calculator::onCmdLPar(FXObject*,FXSelector,void*){
  lparen();
  return 1;
  }


// Right parenth
long Calculator::onCmdRPar(FXObject*,FXSelector,void*){
  rparen();
  return 1;
  }


// Bitwise AND
long Calculator::onCmdAnd(FXObject*,FXSelector,void*){
  dyadic(DY_AND);
  return 1;
  }


// Bitwise OR
long Calculator::onCmdOr(FXObject*,FXSelector,void*){
  dyadic(DY_OR);
  return 1;
  }


// Bitwise XOR
long Calculator::onCmdXor(FXObject*,FXSelector,void*){
  dyadic(DY_XOR);
  return 1;
  }


// Bitwise NOT
long Calculator::onCmdNot(FXObject*,FXSelector,void*){
  unary(UN_NOT);
  return 1;
  }


// Multiply
long Calculator::onCmdMul(FXObject*,FXSelector,void*){
  dyadic(DY_MUL);
  return 1;
  }


// Divide
long Calculator::onCmdDiv(FXObject*,FXSelector,void*){
  dyadic(DY_DIV);
  return 1;
  }


// Modulo
long Calculator::onCmdMod(FXObject*,FXSelector,void*){
  dyadic(DY_MOD+(modifiers&MOD_INV));
  return 1;
  }


// Update mod
long Calculator::onUpdMod(FXObject* sender,FXSelector,void*){
  FXString label=(modifiers&MOD_INV)?"div":"mod";
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&label);
  sender->handle(this,(base==10) ? FXSEL(SEL_COMMAND,ID_ENABLE) : FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Add
long Calculator::onCmdAdd(FXObject*,FXSelector,void*){
  dyadic(DY_ADD);
  return 1;
  }


// Sub
long Calculator::onCmdSub(FXObject*,FXSelector,void*){
  dyadic(DY_SUB);
  return 1;
  }


// Enter
long Calculator::onCmdEnter(FXObject*,FXSelector,void*){
  evaluate();
  return 1;
  }


// Recall from memory
long Calculator::onCmdMemRec(FXObject*,FXSelector,void*){
  setnum(recall);
  setDisplayValue(recall);
  modifiers=0;
  return 1;
  }


// Add to memory
long Calculator::onCmdMemAdd(FXObject*,FXSelector,void*){
  recall+=getnum();
  modifiers=0;
  return 1;
  }


// Substract from memory
long Calculator::onCmdMemSub(FXObject*,FXSelector,void*){
  recall-=getnum();
  modifiers=0;
  return 1;
  }


// Clear memory
long Calculator::onCmdMemClr(FXObject*,FXSelector,void*){
  recall=0.0;
  modifiers=0;
  return 1;
  }


