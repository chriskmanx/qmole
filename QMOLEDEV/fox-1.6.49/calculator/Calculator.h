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
* $Id: Calculator.h,v 1.27 2006/01/22 18:01:12 fox Exp $                        *
********************************************************************************/
#ifndef CALCULATOR_H
#define CALCULATOR_H


/*******************************************************************************/



// Mini application object
class Calculator : public FXMainWindow {
  FXDECLARE(Calculator)
protected:
  enum{STACKLEN=32};
protected:
  FXTextField *display;             // Number display
  FXFont      *font;                // Display font
  FXButton    *digit[16];           // Digit buttons
  FXButton    *memory[4];           // Memory buttons
  FXButton    *clearbtn;            // Clear entry button
  FXButton    *clearallbtn;         // Clear all button
  FXButton    *inverse;             // Inverse function
  FXButton    *hyper2;              // Hyper (for trig) button
  FXButton    *functions[16];       // Function buttons
  FXButton    *operators[14];       // Operator buttons
  FXButton    *numbase[4];          // Numeric base
  FXButton    *angmode[3];          // Angle mode (deg, rad, grad)
  FXIcon      *bigicon;             // Big application icon
  FXIcon      *smallicon;           // Small application icon
  FXIcon      *cmem;		    // Label
  FXIcon      *quest;		    // Question mark
  FXdouble     numstack[STACKLEN];  // Evaluation stack
  FXint        numsp;               // Evaluation stack pointer
  FXuchar      opstack[STACKLEN];   // Operator stack
  FXint        opsp;                // Operator stack pointer
  FXdouble     recall;              // Memory cell
  FXint        limit;               // Maximum displayed digits
  FXint        digits;              // Number of digits entered
  FXint        base;                // Number base
  FXint        angles;              // Angle mode
  FXint        precision;	    // How many digits to show
  FXbool       exponent;     	    // Exponential notation mode
  FXbool       beep;		    // Beep on error
  FXint        parens;              // Count of ( and )
  FXuchar      modifiers;           // Invert, hyperbolic, entry modifiers
  static const char help[];
protected:
  Calculator(){}
private:
  Calculator(const Calculator&);
  Calculator &operator=(const Calculator&);
  void setDisplayText(const FXString& txt);
  FXString getDisplayText() const;
  void setDisplayValue(FXdouble val);
  FXdouble getDisplayValue() const;
  FXdouble trigarg(FXdouble ang) const;
  FXdouble trigres(FXdouble res) const;
  FXdouble pushnum(FXdouble num);
  FXdouble setnum(FXdouble num);
  FXdouble popnum();
  FXdouble getnum();
  void dyop(FXuchar op);
  void clear();
  void unary(FXuchar op);
  void dyadic(FXuchar op);
  void evaluate();
  void lparen();
  void rparen();
public:
  long onCmdAngle(FXObject*,FXSelector,void*);
  long onUpdAngle(FXObject*,FXSelector,void*);
  long onCmdBase(FXObject*,FXSelector,void*);
  long onUpdBase(FXObject*,FXSelector,void*);
  long onUpdDigit(FXObject*,FXSelector,void*);
  long onCmdDigit(FXObject*,FXSelector,void*);
  long onCmdPoint(FXObject*,FXSelector,void*);
  long onUpdPoint(FXObject*,FXSelector,void*);
  long onCmdExp(FXObject*,FXSelector,void*);
  long onUpdExp(FXObject*,FXSelector,void*);
  long onCmdDelete(FXObject*,FXSelector,void*);
  long onCmdClear(FXObject*,FXSelector,void*);
  long onCmdClearAll(FXObject*,FXSelector,void*);
  long onCmdInverse(FXObject*,FXSelector,void*);
  long onCmdHyper(FXObject*,FXSelector,void*);
  long onCmdMemRec(FXObject*,FXSelector,void*);
  long onCmdMemAdd(FXObject*,FXSelector,void*);
  long onCmdMemSub(FXObject*,FXSelector,void*);
  long onCmdMemClr(FXObject*,FXSelector,void*);
  long onCmdSin(FXObject*,FXSelector,void*);
  long onUpdSin(FXObject*,FXSelector,void*);
  long onCmdCos(FXObject*,FXSelector,void*);
  long onUpdCos(FXObject*,FXSelector,void*);
  long onCmdTan(FXObject*,FXSelector,void*);
  long onUpdTan(FXObject*,FXSelector,void*);
  long onCmdLog(FXObject*,FXSelector,void*);
  long onUpdLog(FXObject*,FXSelector,void*);
  long onCmdLn(FXObject*,FXSelector,void*);
  long onUpdLn(FXObject*,FXSelector,void*);
  long onCmdPi(FXObject*,FXSelector,void*);
  long onUpdPi(FXObject*,FXSelector,void*);
  long onCmdFac(FXObject*,FXSelector,void*);
  long onUpdFac(FXObject*,FXSelector,void*);
  long onCmdPer(FXObject*,FXSelector,void*);
  long onUpdPer(FXObject*,FXSelector,void*);
  long onCmdCom(FXObject*,FXSelector,void*);
  long onUpdCom(FXObject*,FXSelector,void*);
  long onCmdRecip(FXObject*,FXSelector,void*);
  long onUpdRecip(FXObject*,FXSelector,void*);
  long onCmdPlusMin(FXObject*,FXSelector,void*);
  long onCmdXToY(FXObject*,FXSelector,void*);
  long onUpdXToY(FXObject*,FXSelector,void*);
  long onCmdSqrt(FXObject*,FXSelector,void*);
  long onUpdSqrt(FXObject*,FXSelector,void*);
  long onCmdShl(FXObject*,FXSelector,void*);
  long onUpdShl(FXObject*,FXSelector,void*);
  long onCmdShr(FXObject*,FXSelector,void*);
  long onUpdShr(FXObject*,FXSelector,void*);
  long onCmd2Log(FXObject*,FXSelector,void*);
  long onUpd2Log(FXObject*,FXSelector,void*);
  long onCmdLPar(FXObject*,FXSelector,void*);
  long onCmdRPar(FXObject*,FXSelector,void*);
  long onCmdAnd(FXObject*,FXSelector,void*);
  long onCmdOr(FXObject*,FXSelector,void*);
  long onCmdXor(FXObject*,FXSelector,void*);
  long onCmdNot(FXObject*,FXSelector,void*);
  long onCmdMul(FXObject*,FXSelector,void*);
  long onCmdDiv(FXObject*,FXSelector,void*);
  long onCmdMod(FXObject*,FXSelector,void*);
  long onUpdMod(FXObject*,FXSelector,void*);
  long onCmdAdd(FXObject*,FXSelector,void*);
  long onCmdSub(FXObject*,FXSelector,void*);
  long onCmdEnter(FXObject*,FXSelector,void*);
  long onCmdPreferences(FXObject*,FXSelector,void*);
  long onUpdColor(FXObject*,FXSelector,void*);
  long onCmdColor(FXObject*,FXSelector,void*);
  long onCmdFont(FXObject*,FXSelector,void*);
  long onCmdExponent(FXObject*,FXSelector,void*);
  long onUpdExponent(FXObject*,FXSelector,void*);
  long onCmdPrecision(FXObject*,FXSelector,void*);
  long onUpdPrecision(FXObject*,FXSelector,void*);
  long onCmdBeep(FXObject*,FXSelector,void*);
  long onUpdBeep(FXObject*,FXSelector,void*);
  long onCmdQuestion(FXObject*,FXSelector,void*);
public:
  enum {
    MOD_INV=1,        // Modes
    MOD_HYP=2,
    MOD_ENT=4
    };
  enum {
    ANG_DEG,          // Angle modes
    ANG_RAD,
    ANG_GRA
    };
  enum {
    NUM_BIN=2,        // Number bases
    NUM_OCT=8,
    NUM_DEC=10,
    NUM_HEX=16
    };
  enum {
    DY_OR,            // Dyadics
    DY_XOR,
    DY_AND,
    DY_SUB,
    DY_ADD,
    DY_MOD,
    DY_IDIV,
    DY_DIV,
    DY_MUL,
    DY_XTOY,
    DY_XTOINVY,
    DY_PER,
    DY_COM,
    DY_LPAR,
    DY_RPAR
    };
  enum {
    UN_NOT,           // Unaries
    UN_NEG,
    UN_SHL,
    UN_SHR,
    UN_SAR,
    UN_RECIP,
    UN_FAC,
    UN_SQRT,
    UN_QUAD,
    UN_2LOG,
    UN_2TOX,
    UN_LOG,
    UN_10TOX,
    UN_LN,
    UN_EXP,
    UN_SIN,
    UN_ASIN,
    UN_SINH,
    UN_ASINH,
    UN_COS,
    UN_ACOS,
    UN_COSH,
    UN_ACOSH,
    UN_TAN,
    UN_ATAN,
    UN_TANH,
    UN_ATANH
    };
public:
  enum {
    ID_TEXT=FXMainWindow::ID_LAST,
    ID_PREFERENCES,
    ID_COLOR_DISPLAY,
    ID_COLOR_DISPLAYNUMBER,
    ID_COLOR_DIGITS,
    ID_COLOR_HEXDIGITS,
    ID_COLOR_OPERATORS,
    ID_COLOR_FUNCTIONS,
    ID_COLOR_MEMORY,
    ID_COLOR_BASE,
    ID_COLOR_ANGLES,
    ID_COLOR_INVERT,
    ID_COLOR_HYPER,
    ID_COLOR_CLEARALL,
    ID_COLOR_CLEAR,
    ID_EXPONENT_ALWAYS,
    ID_EXPONENT_NEVER,
    ID_PRECISION,
    ID_QUESTION,
    ID_BEEP,
    ID_FONT,
    ID_BASE,
    ID_BIN=ID_BASE+NUM_BIN,
    ID_OCT=ID_BASE+NUM_OCT,
    ID_DEC=ID_BASE+NUM_DEC,
    ID_HEX=ID_BASE+NUM_HEX,
    ID_MODE,
    ID_DEG=ID_MODE+ANG_DEG,
    ID_RAD=ID_MODE+ANG_RAD,
    ID_GRA=ID_MODE+ANG_GRA,
    ID_MEM_REC,
    ID_MEM_ADD,
    ID_MEM_SUB,
    ID_MEM_CLR,
    ID_PLUSMIN,
    ID_ENTER,
    ID_CLEAR,
    ID_CLEARALL,
    ID_DELETE,
    ID_INV,
    ID_HYP,
    ID_EXP,
    ID_PNT,
    ID_PI,
    ID_0,
    ID_1,
    ID_2,
    ID_3,
    ID_4,
    ID_5,
    ID_6,
    ID_7,
    ID_8,
    ID_9,
    ID_A,
    ID_B,
    ID_C,
    ID_D,
    ID_E,
    ID_F,
    ID_OR,
    ID_XOR,
    ID_AND,
    ID_SUB,
    ID_ADD,
    ID_MOD,
    ID_DIV,
    ID_MUL,
    ID_XTOY,
    ID_PER,
    ID_COM,
    ID_LPAR,
    ID_RPAR,
    ID_NOT,
    ID_SHL,
    ID_SHR,
    ID_RECIP,
    ID_FAC,
    ID_SQRT,
    ID_2LOG,
    ID_LOG,
    ID_LN,
    ID_SIN,
    ID_COS,
    ID_TAN,
    ID_LAST
    };
public:

  /// Construct calculator dialog
  Calculator(FXApp* a);

  // Close the window and save registry
  virtual FXbool close(FXbool notify=FALSE);

  /// Create
  virtual void create();

  /// Set digit color
  void setDigitColor(FXColor clr);
  FXColor getDigitColor() const;

  /// Set digit color
  void setHexDigitColor(FXColor clr);
  FXColor getHexDigitColor() const;

  /// Set operator color
  void setOperatorColor(FXColor clr);
  FXColor getOperatorColor() const;

  /// Set function color
  void setFunctionColor(FXColor clr);
  FXColor getFunctionColor() const;

  /// Set memory color
  void setMemoryColor(FXColor clr);
  FXColor getMemoryColor() const;

  /// Set inverse color
  void setInverseColor(FXColor clr);
  FXColor getInverseColor() const;

  /// Set hyp color
  void setHyperColor(FXColor clr);
  FXColor getHyperColor() const;

  /// Set clear color
  void setClearColor(FXColor clr);
  FXColor getClearColor() const;

  /// Set clear all color
  void setClearAllColor(FXColor clr);
  FXColor getClearAllColor() const;

  /// Set display color
  void setDisplayColor(FXColor clr);
  FXColor getDisplayColor() const;

  /// Set display number color
  void setDisplayNumberColor(FXColor clr);
  FXColor getDisplayNumberColor() const;

  /// Set numeric base color
  void setBaseColor(FXColor clr);
  FXColor getBaseColor() const;

  /// Set numeric base color
  void setAngleColor(FXColor clr);
  FXColor getAngleColor() const;

  /// Set number base
  void setBase(FXint base);
  FXint getBase() const { return base; }

  /// Set angle mode
  void setAngles(FXint ang){ angles=ang; }
  FXint getAngles() const { return angles; }

  /// Set exponent mode
  void setExponentMode(FXbool expmode);
  FXbool getExponentMode() const { return exponent; }

  /// Set precision
  void setPrecision(FXint prec);
  FXint getPrecision() const { return precision; }

  /// Beep on error
  void setBeep(FXbool on){ beep=on; }
  FXbool getBeep() const { return beep; }

  /// Set display font
  void setDisplayFont(FXFont* font);
  FXFont* getDisplayFont() const;

  /// Clear the calculator
  void clearAll();

  /// Read/write registry
  void readRegistry();
  void writeRegistry();

  /// Destroy calculator
  virtual ~Calculator();
  };

#endif
