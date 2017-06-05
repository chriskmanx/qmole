/********************************************************************************
*                                                                               *
*                      E x p r e s s i o n   E v a l u a t o r                  *
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
* $Id: FXExpression.cpp,v 1.30.2.1 2006/07/24 15:33:14 fox Exp $                    *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxascii.h"
#include "fxunicode.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXExpression.h"


/*
  Notes:
  - Old as night, but recently rediscovered ;-).
  - Better treatment of identifiers needed [user-supplied names].
  - Maintain stack-depth during compile phase for possible limit check.
*/

#define MAXSTACKDEPTH 128

using namespace FX;

/*******************************************************************************/

namespace {

// Tokens
enum {
  TK_EOF        = 0,
  TK_INT        = 1,
  TK_INT_HEX    = 2,
  TK_INT_BIN    = 3,
  TK_INT_OCT    = 4,
  TK_REAL       = 5,
  TK_PLUS       = 6,
  TK_MINUS      = 7,
  TK_TIMES      = 8,
  TK_DIVIDE     = 9,
  TK_MODULO     = 10,
  TK_POWER      = 11,
  TK_LPAR       = 12,
  TK_RPAR       = 13,
  TK_LESS       = 14,
  TK_GREATER    = 15,
  TK_LESSEQ     = 16,
  TK_GREATEREQ  = 17,
  TK_EQUAL      = 18,
  TK_NOTEQUAL   = 19,
  TK_AND        = 20,
  TK_OR         = 21,
  TK_XOR        = 22,
  TK_NOT        = 23,
  TK_SHIFTLEFT  = 24,
  TK_SHIFTRIGHT = 25,
  TK_COMMA      = 26,
  TK_PI         = 27,
  TK_EULER      = 28,
  TK_RAN        = 29,
  TK_ERROR      = 30,
  TK_ABS        = 108848,
  TK_ACOS       = 3592862,
  TK_ACOSH      = 118564406,
  TK_ASIN       = 3610325,
  TK_ASINH      = 119140637,
  TK_ATAN       = 3615258,
  TK_ATANH      = 119303474,
  TK_CEIL       = 3523203,
  TK_COS        = 107103,
  TK_COSH       = 3534423,
  TK_EXP        = 114029,
  TK_FLOOR      = 122360152,
  TK_LOG        = 114052,
  TK_LOG10      = 124204261,
  TK_SIN        = 124308,
  TK_SINH       = 4102268,
  TK_SQRT       = 4076772,
  TK_TAN        = 123227,
  TK_TANH       = 4066515,
  TK_MAX        = 121748,
  TK_MIN        = 121482,
  TK_POW        = 119176,
  TK_ATAN2      = 119303528
  };


// Opcodes
enum {
  OP_END,
  OP_NUM,
  OP_VAR,
  OP_PI,
  OP_EULER,
  OP_RAND,

  OP_NOT,
  OP_NEG,

  OP_MUL,
  OP_DIV,
  OP_MOD,
  OP_ADD,
  OP_SUB,
  OP_AND,
  OP_OR,
  OP_XOR,
  OP_SHL,
  OP_SHR,
  OP_LT,
  OP_GT,
  OP_LE,
  OP_GE,
  OP_EQ,
  OP_NE,

  OP_ABS,
  OP_ACOS,
  OP_ACOSH,
  OP_ASIN,
  OP_ASINH,
  OP_ATAN,
  OP_ATANH,
  OP_CEIL,
  OP_COS,
  OP_COSH,
  OP_EXP,
  OP_FLOOR,
  OP_LOG,
  OP_LOG10,
  OP_SIN,
  OP_SINH,
  OP_SQRT,
  OP_TAN,
  OP_TANH,

  OP_MAX,
  OP_MIN,
  OP_POW,
  OP_ATAN2
  };


// Compile class
struct FXCompile {
  const FXchar *head;
  const FXchar *tail;
  const FXchar *vars;
  FXuchar      *code;
  FXuchar      *pc;
  FXuint        token;
  FXExpressionError compile();
  FXExpressionError expression();
  FXExpressionError compexp();
  FXExpressionError shiftexp();
  FXExpressionError addexp();
  FXExpressionError mulexp();
  FXExpressionError powexp();
  FXExpressionError primary();
  FXExpressionError element();
  FXint lookup(const FXchar *list);
  void opcode(FXuchar op);
  void operand(FXint n);
  void operand(FXdouble num);
  void gettok();
  };


/*******************************************************************************/

#ifndef NDEBUG

// Dump program
void dump(FXuchar *prog){
  FXint op;
  fxmessage("\n");
  fxmessage("Program:\n");
  fxmessage("%-10p SIZE   %d\n",prog,*((FXint*)prog));
  prog+=4;
  while(1){
    fxmessage("%-10p ",prog);
    op=*prog++;
    switch(op){
      case OP_END:
        fxmessage("OP_END\n");
        goto x;
      case OP_NUM:
        fxmessage("OP_NUM %.10g\n",*((FXdouble*)prog));
        prog+=8;
        break;
      case OP_VAR:
        fxmessage("OP_VAR %d\n",*prog);
        prog++;
        break;
      case OP_PI:
        fxmessage("OP_PI\n");
        break;
      case OP_EULER:
        fxmessage("OP_EULER\n");
        break;
      case OP_RAND:
        fxmessage("OP_RAND\n");
        break;
      case OP_NOT:
        fxmessage("OP_NOT\n");
        break;
      case OP_NEG:
        fxmessage("OP_NEG\n");
        break;
      case OP_SIN:
        fxmessage("OP_SIN\n");
        break;
      case OP_COS:
        fxmessage("OP_COS\n");
        break;
      case OP_TAN:
        fxmessage("OP_TAN\n");
        break;
      case OP_ASIN:
        fxmessage("OP_ASIN\n");
        break;
      case OP_ACOS:
        fxmessage("OP_ACOS\n");
        break;
      case OP_ATAN:
        fxmessage("OP_ATAN\n");
        break;
      case OP_SINH:
        fxmessage("OP_SINH\n");
        break;
      case OP_COSH:
        fxmessage("OP_COSH\n");
        break;
      case OP_TANH:
        fxmessage("OP_TANH\n");
        break;
      case OP_ASINH:
        fxmessage("OP_ASINH\n");
        break;
      case OP_ACOSH:
        fxmessage("OP_ACOSH\n");
        break;
      case OP_ATANH:
        fxmessage("OP_ATANH\n");
        break;
      case OP_SQRT:
        fxmessage("OP_SQRT\n");
        break;
      case OP_ABS:
        fxmessage("OP_ABS\n");
        break;
      case OP_CEIL:
        fxmessage("OP_CEIL\n");
        break;
      case OP_FLOOR:
        fxmessage("OP_FLOOR\n");
        break;
      case OP_EXP:
        fxmessage("OP_EXP\n");
        break;
      case OP_LOG:
        fxmessage("OP_LOG\n");
        break;
      case OP_LOG10:
        fxmessage("OP_LOG10\n");
        break;
      case OP_MUL:
        fxmessage("OP_MUL\n");
        break;
      case OP_DIV:
        fxmessage("OP_DIV\n");
        break;
      case OP_MOD:
        fxmessage("OP_MOD\n");
        break;
      case OP_ADD:
        fxmessage("OP_ADD\n");
        break;
      case OP_SUB:
        fxmessage("OP_SUB\n");
        break;
      case OP_AND:
        fxmessage("OP_AND\n");
        break;
      case OP_OR:
        fxmessage("OP_OR\n");
        break;
      case OP_XOR:
        fxmessage("OP_XOR\n");
        break;
      case OP_LT:
        fxmessage("OP_LT\n");
        break;
      case OP_GT:
        fxmessage("OP_GT\n");
        break;
      case OP_LE:
        fxmessage("OP_LE\n");
        break;
      case OP_GE:
        fxmessage("OP_GE\n");
        break;
      case OP_EQ:
        fxmessage("OP_EQ\n");
        break;
      case OP_NE:
        fxmessage("OP_NE\n");
        break;
      case OP_POW:
        fxmessage("OP_POW\n");
        break;
      case OP_MAX:
        fxmessage("OP_MAX\n");
        break;
      case OP_MIN:
        fxmessage("OP_MIN\n");
        break;
      case OP_ATAN2:
        fxmessage("OP_ATAN2\n");
        break;
      default:
        fxmessage("OP_%d: error\n",op);
        goto x;
      }
    }
x:fxmessage("end\n");
  }

#endif

/*******************************************************************************/

// Compile expression
FXExpressionError FXCompile::compile(){
  FXExpressionError err;
  if(token==TK_EOF) return EXPRERR_EMPTY;
  err=expression();
  if(err!=EXPRERR_OK) return err;
  if(token!=TK_EOF) return EXPRERR_TOKEN;
  opcode(OP_END);
  return EXPRERR_OK;
  }


// Expression
FXExpressionError FXCompile::expression(){
  FXExpressionError err=compexp();
  if(err!=EXPRERR_OK) return err;
  while(TK_AND<=token && token<=TK_XOR){
    FXuint t=token;
    gettok();
    err=compexp();
    if(err!=EXPRERR_OK) return err;
    if(t==TK_AND) opcode(OP_AND);
    else if(t==TK_OR) opcode(OP_OR);
    else opcode(OP_XOR);
    }
  return EXPRERR_OK;
  }


// Compare expression
FXExpressionError FXCompile::compexp(){
  FXExpressionError err=shiftexp();
  if(err!=EXPRERR_OK) return err;
  if(TK_LESS<=token && token<=TK_NOTEQUAL){
    FXuint t=token;
    gettok();
    err=shiftexp();
    if(err!=EXPRERR_OK) return err;
    if(t==TK_LESS) opcode(OP_LT);
    else if(t==TK_LESSEQ) opcode(OP_LE);
    else if(t==TK_GREATER) opcode(OP_GT);
    else if(t==TK_GREATEREQ) opcode(OP_GE);
    else if(t==TK_EQUAL) opcode(OP_EQ);
    else opcode(OP_NE);
    }
  return EXPRERR_OK;
  }


// Shift expression
FXExpressionError FXCompile::shiftexp(){
  FXExpressionError err=addexp();
  if(err!=EXPRERR_OK) return err;
  while(TK_SHIFTLEFT<=token && token<=TK_SHIFTRIGHT){
    FXuint t=token;
    gettok();
    err=mulexp();
    if(err!=EXPRERR_OK) return err;
    if(t==TK_SHIFTLEFT) opcode(OP_SHL);
    else opcode(OP_SHR);
    }
  return EXPRERR_OK;
  }


// Add expression
FXExpressionError FXCompile::addexp(){
  FXExpressionError err=mulexp();
  if(err!=EXPRERR_OK) return err;
  while(TK_PLUS<=token && token<=TK_MINUS){
    FXuint t=token;
    gettok();
    err=mulexp();
    if(err!=EXPRERR_OK) return err;
    if(t==TK_MINUS) opcode(OP_SUB);
    else opcode(OP_ADD);
    }
  return EXPRERR_OK;
  }


// Mul expression
FXExpressionError FXCompile::mulexp(){
  FXExpressionError err=powexp();
  if(err!=EXPRERR_OK) return err;
  while(TK_TIMES<=token && token<=TK_MODULO){
    FXuint t=token;
    gettok();
    err=powexp();
    if(err!=EXPRERR_OK) return err;
    if(t==TK_TIMES) opcode(OP_MUL);
    else if(t==TK_DIVIDE) opcode(OP_DIV);
    else opcode(OP_MOD);
    }
  return EXPRERR_OK;
  }


// Power expression
FXExpressionError FXCompile::powexp(){
  FXExpressionError err=primary();
  if(err!=EXPRERR_OK) return err;
  if(token==TK_POWER){
    gettok();
    err=powexp();
    if(err!=EXPRERR_OK) return err;
    opcode(OP_POW);
    }
  return EXPRERR_OK;
  }


// Primary
FXExpressionError FXCompile::primary(){
  FXExpressionError err;
  if(token==TK_PLUS || token==TK_MINUS || token==TK_NOT){
    FXuint t=token;
    gettok();
    err=primary();
    if(err!=EXPRERR_OK) return err;
    if(t==TK_MINUS) opcode(OP_NEG);
    else if(t==TK_NOT) opcode(OP_NOT);
    }
  else{
    err=element();
    if(err!=EXPRERR_OK) return err;
    }
  return EXPRERR_OK;
  }


// Element
FXExpressionError FXCompile::element(){
  FXExpressionError err;
  FXdouble num;
  FXuchar op;
  FXint v;
  switch(token){
    case TK_LPAR:
      gettok();
      err=expression();
      if(err!=EXPRERR_OK) return err;
      if(token!=TK_RPAR) return EXPRERR_PAREN;
      break;
    case TK_INT_HEX:
      num=(FXdouble)strtol(head+2,NULL,16);
      opcode(OP_NUM);
      operand(num);
      break;
    case TK_INT_BIN:
      num=(FXdouble)strtol(head+2,NULL,2);
      opcode(OP_NUM);
      operand(num);
      break;
    case TK_INT_OCT:
      num=(FXdouble)strtol(head+1,NULL,8);
      opcode(OP_NUM);
      operand(num);
      break;
    case TK_INT:
      num=(FXdouble)strtol(head,NULL,10);
      opcode(OP_NUM);
      operand(num);
      break;
    case TK_REAL:
      num=strtod(head,NULL);
      opcode(OP_NUM);
      operand(num);
      break;
    case TK_PI:
      opcode(OP_PI);
      break;
    case TK_EULER:
      opcode(OP_EULER);
      break;
    case TK_RAN:
      opcode(OP_RAND);
      break;
    case TK_MAX:
      op=OP_MAX;
      goto dyad;
    case TK_MIN:
      op=OP_MIN;
      goto dyad;
    case TK_POW:
      op=OP_POW;
      goto dyad;
    case TK_ATAN2:
      op=OP_ATAN2;
dyad: gettok();
      if(token!=TK_LPAR) return EXPRERR_PAREN;
      gettok();
      err=expression();
      if(err!=EXPRERR_OK) return err;
      if(token!=TK_COMMA) return EXPRERR_COMMA;
      gettok();
      err=expression();
      if(err!=EXPRERR_OK) return err;
      if(token!=TK_RPAR) return EXPRERR_PAREN;
      opcode(op);
      break;
    case TK_ABS:
      op=OP_ABS;
      goto mono;
    case TK_ACOS:
      op=OP_ACOS;
      goto mono;
    case TK_ACOSH:
      op=OP_ACOS;
      goto mono;
    case TK_ASIN:
      op=OP_ASIN;
      goto mono;
    case TK_ASINH:
      op=OP_ASINH;
      goto mono;
    case TK_ATAN:
      op=OP_ATAN;
      goto mono;
    case TK_ATANH:
      op=OP_ATANH;
      goto mono;
    case TK_CEIL:
      op=OP_CEIL;
      goto mono;
    case TK_COS:
      op=OP_COS;
      goto mono;
    case TK_COSH:
      op=OP_COSH;
      goto mono;
    case TK_EXP:
      op=OP_EXP;
      goto mono;
    case TK_FLOOR:
      op=OP_FLOOR;
      goto mono;
    case TK_LOG:
      op=OP_LOG;
      goto mono;
    case TK_LOG10:
      op=OP_LOG10;
      goto mono;
    case TK_SIN:
      op=OP_SIN;
      goto mono;
    case TK_SINH:
      op=OP_SINH;
      goto mono;
    case TK_SQRT:
      op=OP_SQRT;
      goto mono;
    case TK_TAN:
      op=OP_TAN;
      goto mono;
    case TK_TANH:
      op=OP_TANH;
mono: gettok();
      if(token!=TK_LPAR) return EXPRERR_PAREN;
      gettok();
      err=expression();
      if(err!=EXPRERR_OK) return err;
      if(token!=TK_RPAR) return EXPRERR_PAREN;
      opcode(op);
      break;
    default:
      v=lookup(vars);
      if(v<0) return EXPRERR_IDENT;
      opcode(OP_VAR);
      opcode(v);
      break;
    case TK_EOF:
    case TK_TIMES:
    case TK_DIVIDE:
    case TK_MODULO:
    case TK_POWER:
    case TK_RPAR:
    case TK_LESS:
    case TK_GREATER:
    case TK_LESSEQ:
    case TK_GREATEREQ:
    case TK_EQUAL:
    case TK_NOTEQUAL:
    case TK_AND:
    case TK_OR:
    case TK_XOR:
    case TK_SHIFTLEFT:
    case TK_SHIFTRIGHT:
    case TK_COMMA:
    case TK_ERROR:
      return EXPRERR_TOKEN;
    }
  gettok();
  return EXPRERR_OK;
  }


// Lookup current token in list
FXint FXCompile::lookup(const FXchar *list){
  if(list){
    FXint which=0;
    while(*list){
      const FXchar *q;
      for(q=head; q<tail && *q==*list; q++,list++);
      if(q==tail && (*list=='\0' || *list==',')) return which;
      while(*list && *list!=',') list++;
      if(*list==','){ which++; list++; }
      }
    }
  return -1;
  }


// Obtain next token from input
void FXCompile::gettok(){
  register FXchar c;
  head=tail;
  while((c=*tail)!='\0'){
    switch(c){
      case ' ':
      case '\b':
      case '\t':
      case '\v':
      case '\f':
      case '\r':
      case '\n':
        head=++tail;
        break;
      case '=':
        token=TK_ERROR; tail++;
        if(*tail=='='){ token=TK_EQUAL; tail++; }
        return;
      case '<':
        token=TK_LESS; tail++;
        if(*tail=='='){ token=TK_LESSEQ; tail++; }
        else if(*tail=='<'){ token=TK_SHIFTLEFT; tail++; }
        return;
      case '>':
        token=TK_GREATER;
        tail++;
        if(*tail=='='){ token=TK_GREATEREQ; tail++; }
        else if(*tail=='>'){ token=TK_SHIFTRIGHT; tail++; }
        return;
      case '|':
        token=TK_OR; tail++;
        return;
      case '&':
        token=TK_AND; tail++;
        return;
      case '^':
        token=TK_XOR; tail++;
        return;
      case '~':
        token=TK_NOT; tail++;
        return;
      case '-':
        token=TK_MINUS; tail++;
        return;
      case '+':
        token=TK_PLUS; tail++;
        return;
      case '*':
        token=TK_TIMES; tail++;
        if(*tail=='*'){ token=TK_POWER; tail++; }
        return;
      case '/':
        token=TK_DIVIDE; tail++;
        return;
      case '%':
        token=TK_MODULO; tail++;
        return;
      case '!':
        token=TK_ERROR; tail++;
        if(*tail=='='){ token=TK_NOTEQUAL; tail++; }
        return;
      case '(':
        token=TK_LPAR; tail++;
        return;
      case ')':
        token=TK_RPAR; tail++;
        return;
      case ',':
        token=TK_COMMA; tail++;
        return;
      case '.':
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
        token=TK_INT;
        if(c=='0'){
          tail++;
          if(*tail=='x' || *tail=='X'){
            tail++;
            if(!Ascii::isHexDigit(*tail)){ token=TK_ERROR; return; }
            tail++;
            while(Ascii::isHexDigit(*tail)) tail++;
            token=TK_INT_HEX;
            return;
            }
          if(*tail=='b' || *tail=='B'){
            tail++;
            if(*tail!='0' && *tail!='1'){ token=TK_ERROR; return; }
            tail++;
            while(*tail=='0' || *tail=='1') tail++;
            token=TK_INT_BIN;
            return;
            }
          if('0'<=*tail && *tail<='7'){
            tail++;
            while('0'<=*tail && *tail<='7') tail++;
            if('7'<=*tail && *tail<='9'){
              token=TK_ERROR;
              return;
              }
            token=TK_INT_OCT;
            return;
            }
          }
        while(Ascii::isDigit(*tail)) tail++;
        if(*tail=='.'){
          token=TK_REAL;
          tail++;
          while(Ascii::isDigit(*tail)) tail++;
          }
        if(*tail=='e' || *tail=='E'){
          token=TK_REAL;
          tail++;
          if(*tail=='-' || *tail=='+') tail++;
          if(!Ascii::isDigit(*tail)){ token=TK_ERROR; return; }
          tail++;
          while(Ascii::isDigit(*tail)) tail++;
          }
        return;
      case 'e':
        if(Ascii::isAlphaNumeric(tail[1])) goto ident;
        token=TK_EULER;
        tail+=1;
        return;
      case 'p':
        if(tail[1]!='i') goto ident;
        if(Ascii::isAlphaNumeric(tail[2])) goto ident;
        token=TK_PI;
        tail+=2;
        return;
      case 'r':
        if(tail[1]!='a') goto ident;
        if(tail[2]!='n') goto ident;
        if(Ascii::isAlphaNumeric(tail[3])) goto ident;
        token=TK_RAN;
        tail+=3;
        return;
      default:
ident:  token=TK_ERROR;
        if(Ascii::isLetter(*tail)){
          token=*tail++;
          while(Ascii::isAlphaNumeric(*tail)){
            token=((token<<5)+token)^*tail++;
            }
          }
        return;
      }
    }
  token=TK_EOF;
  }


// Emit opcode
void FXCompile::opcode(FXuchar op){
  if(code){
    pc[0]=op;
    }
  pc++;
  }


// Emit integer operand
void FXCompile::operand(FXint n){
  if(code){
#if defined(__i386__) || defined(__x86_64__) || defined(WIN32)
    ((FXint*)pc)[0]=n;
#else
    pc[0]=((const FXuchar*)&n)[0];
    pc[1]=((const FXuchar*)&n)[1];
    pc[2]=((const FXuchar*)&n)[2];
    pc[3]=((const FXuchar*)&n)[3];
#endif
    }
  pc+=4;
  }


// Emit double operand
void FXCompile::operand(FXdouble n){
  if(code){
#if defined(__i386__) || defined(__x86_64__) || defined(WIN32)
    ((FXdouble*)pc)[0]=n;
#else
    pc[0]=((const FXuchar*)&n)[0];
    pc[1]=((const FXuchar*)&n)[1];
    pc[2]=((const FXuchar*)&n)[2];
    pc[3]=((const FXuchar*)&n)[3];
    pc[4]=((const FXuchar*)&n)[4];
    pc[5]=((const FXuchar*)&n)[5];
    pc[6]=((const FXuchar*)&n)[6];
    pc[7]=((const FXuchar*)&n)[7];
#endif
    }
  pc+=8;
  }

}

/*******************************************************************************/

namespace FX {

#if FOX_BIGENDIAN == 1
const FXuchar FXExpression::initial[]={0,0,0,14,OP_NUM,0,0,0,0,0,0,0,0,OP_END};
#endif
#if FOX_BIGENDIAN == 0
const FXuchar FXExpression::initial[]={14,0,0,0,OP_NUM,0,0,0,0,0,0,0,0,OP_END};
#endif


// Table of error messages
const FXchar *const FXExpression::errors[]={
  "OK",
  "Empty expression",
  "Out of memory",
  "Unmatched parenthesis",
  "Illegal token",
  "Expected comma",
  "Unknown identifier"
  };


// Construct empty expression object
FXExpression::FXExpression():code((FXuchar*)initial){
  }


// Copy regex object
FXExpression::FXExpression(const FXExpression& orig):code((FXuchar*)initial){
  if(orig.code!=initial){
    FXMEMDUP(&code,orig.code,FXuchar,*((FXint*)orig.code));
    }
  }


// Compile expression from pattern; fail if error
FXExpression::FXExpression(const FXchar* expression,const FXchar* variables,FXExpressionError* error):code((FXuchar*)initial){
  FXExpressionError err=parse(expression,variables);
  if(error){ *error=err; }
  }


// Compile expression from pattern; fail if error
FXExpression::FXExpression(const FXString& expression,const FXString& variables,FXExpressionError* error):code((FXuchar*)initial){
  FXExpressionError err=parse(expression.text(),variables.text());
  if(error){ *error=err; }
  }


// Assignment
FXExpression& FXExpression::operator=(const FXExpression& orig){
  if(code!=orig.code){
    if(code!=initial) FXFREE(&code);
    code=(FXuchar*)initial;
    if(orig.code!=initial){
      FXMEMDUP(&code,orig.code,FXuchar,*((FXint*)orig.code));
      }
    }
  return *this;
  }


// Parse expression, return error code if syntax error is found
FXExpressionError FXExpression::parse(const FXchar* expression,const FXchar* variables){
  FXExpressionError err=EXPRERR_EMPTY;
  FXint size=0;
  FXCompile cs;

  // Free old code, if any
  if(code!=initial) FXFREE(&code);
  code=(FXuchar*)initial;

  // If not empty, parse expression
  if(expression){

    // Fill in compile data
    cs.tail=expression;
    cs.vars=variables;
    cs.code=NULL;
    cs.pc=NULL;
    cs.token=TK_EOF;

    // Get first token
    cs.gettok();

    // Emit unknown size
    cs.operand(0);

    // Parse to check syntax and determine size
    err=cs.compile();

    // Was OK?
    if(err==EXPRERR_OK){

      // Allocate new code
      size=cs.pc-cs.code;
      if(!FXMALLOC(&code,FXuchar,size)){
        code=(FXuchar*)initial;
        return EXPRERR_MEMORY;
        }

      // Fill in compile data
      cs.tail=expression;
      cs.code=code;
      cs.pc=code;
      cs.token=TK_EOF;

      // Get first token
      cs.gettok();

      // Emit code size
      cs.operand(size);

      // Generate program
      err=cs.compile();

      // Dump for debugging
#ifndef NDEBUG
      if(fxTraceLevel>100) dump(code);
#endif
      }
    }
  return err;
  }


// Parse expression, return error code if syntax error is found
FXExpressionError FXExpression::parse(const FXString& expression,const FXString& variables){
  return parse(expression.text(),variables.text());
  }


// Evaluate expression
FXdouble FXExpression::evaluate(const FXdouble *args){
  FXdouble stack[MAXSTACKDEPTH];
  register const FXuchar *pc=code+4;
  register FXdouble *sp=stack-1;
  while(1){
    switch(*pc++){
      case OP_END:   return *sp;
#if defined(__i386__) || defined(__x86_64__) || defined(WIN32)
      case OP_NUM:   *++sp=*((FXdouble*)pc); pc+=8; break;
#else
      case OP_NUM:   ++sp; ((FXuchar*)sp)[0]=*pc++; ((FXuchar*)sp)[1]=*pc++; ((FXuchar*)sp)[2]=*pc++; ((FXuchar*)sp)[3]=*pc++; ((FXuchar*)sp)[4]=*pc++; ((FXuchar*)sp)[5]=*pc++; ((FXuchar*)sp)[6]=*pc++; ((FXuchar*)sp)[7]=*pc++; break;
#endif
      case OP_VAR:   *++sp=args[*pc++]; break;
      case OP_PI:    *++sp=3.1415926535897932384626433833; break;
      case OP_EULER: *++sp=2.7182818284590452353602874713; break;
#ifndef WIN32
      case OP_RAND:  *++sp=drand48(); break;
#else
      case OP_RAND:  *++sp=(FXdouble)rand()/(FXdouble)RAND_MAX; break;
#endif
      case OP_NOT:   *sp=(FXdouble)(~((FXint)*sp)); break;
      case OP_NEG:   *sp=-*sp; break;
      case OP_SIN:   *sp=sin(*sp); break;
      case OP_COS:   *sp=cos(*sp); break;
      case OP_TAN:   *sp=tan(*sp); break;
      case OP_ASIN:  *sp=asin(*sp); break;
      case OP_ACOS:  *sp=acos(*sp); break;
      case OP_ATAN:  *sp=atan(*sp); break;
      case OP_SINH:  *sp=sinh(*sp); break;
      case OP_COSH:  *sp=cosh(*sp); break;
      case OP_TANH:  *sp=tanh(*sp); break;
#ifndef WIN32
      case OP_ASINH: *sp=asinh(*sp); break;
      case OP_ACOSH: *sp=acosh(*sp); break;
      case OP_ATANH: *sp=atanh(*sp); break;
#else
      case OP_ASINH: *sp=log(*sp + sqrt(*sp * *sp + 1.0)); break;
      case OP_ACOSH: *sp=log(*sp + sqrt(*sp * *sp - 1.0)); break;
      case OP_ATANH: *sp=0.5 * log((1.0 + *sp)/(1.0 - *sp)); break;
#endif
      case OP_SQRT:  *sp=sqrt(*sp); break;
      case OP_ABS:   *sp=fabs(*sp); break;
      case OP_CEIL:  *sp=ceil(*sp); break;
      case OP_FLOOR: *sp=floor(*sp); break;
      case OP_EXP:   *sp=exp(*sp); break;
      case OP_LOG:   *sp=log(*sp); break;
      case OP_LOG10: *sp=log10(*sp); break;
      case OP_MUL:   *(sp-1)=*(sp-1) * *sp; --sp; break;
      case OP_DIV:   *(sp-1)=*(sp-1) / *sp; --sp; break;
      case OP_MOD:   *(sp-1)=fmod(*(sp-1),*sp); --sp; break;
      case OP_ADD:   *(sp-1)=*(sp-1) + *sp; --sp; break;
      case OP_SUB:   *(sp-1)=*(sp-1) - *sp; --sp; break;
      case OP_AND:   *(sp-1)=(FXdouble)(((FXint)*(sp-1)) & ((FXint)*sp)); --sp; break;
      case OP_OR:    *(sp-1)=(FXdouble)(((FXint)*(sp-1)) | ((FXint)*sp)); --sp; break;
      case OP_XOR:   *(sp-1)=(FXdouble)(((FXint)*(sp-1)) ^ ((FXint)*sp)); --sp; break;
      case OP_SHL:   *(sp-1)=(FXdouble)(((FXint)*(sp-1)) << ((FXint)*sp)); --sp; break;
      case OP_SHR:   *(sp-1)=(FXdouble)(((FXint)*(sp-1)) >> ((FXint)*sp)); --sp; break;
      case OP_LT:    *(sp-1)=(FXdouble)(*(sp-1) < *sp); --sp; break;
      case OP_GT:    *(sp-1)=(FXdouble)(*(sp-1) > *sp); --sp; break;
      case OP_LE:    *(sp-1)=(FXdouble)(*(sp-1) <= *sp); --sp; break;
      case OP_GE:    *(sp-1)=(FXdouble)(*(sp-1) >= *sp); --sp; break;
      case OP_EQ:    *(sp-1)=(FXdouble)(*(sp-1) == *sp); --sp; break;
      case OP_NE:    *(sp-1)=(FXdouble)(*(sp-1) != *sp); --sp; break;
      case OP_POW:   *(sp-1)=pow(*(sp-1),*sp); --sp; break;
      case OP_MAX:   *(sp-1)=FXMAX(*(sp-1),*sp); --sp; break;
      case OP_MIN:   *(sp-1)=FXMIN(*(sp-1),*sp); --sp; break;
      case OP_ATAN2: *(sp-1)=atan2(*(sp-1),*sp); --sp; break;
      }
    }
  return 0.0;
  }


// Clean up
FXExpression::~FXExpression(){
  if(code!=initial) FXFREE(&code);
  }

}
