/********************************************************************************
*                                                                               *
*                   V a r a r g s   S c a n f   R o u t i n e s                 *
*                                                                               *
*********************************************************************************
* Copyright (C) 2002,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: vsscanf.cpp,v 1.20.2.2 2007/01/29 20:22:29 fox Exp $                         *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxascii.h"


/*
  Notes:
  - Needs checking for conformance with standard scanf.
  - Some POSIX finesse:
    u,d is equivalent to strtol (strtoul) with base = 10.
    x   is equivalent to strtoul with base = 16
    o   is equivalent to strtoul with base = 8
    i   is equivalent to strtol with base = 0 (which means either
        octal, hex, or decimal as determined by leading 2 characters).
  - Rewrite in terms of strtol, strtoul strtod; these are available since we're
    already using them and have heard no complaints.
*/

using namespace FX;


#ifndef HAVE_VSSCANF

// API
extern "C" int vfscanf(FILE *stream, const char *format, va_list arg_ptr);


// API
extern "C" int vscanf(const char *format, va_list arg_ptr);


// API
extern "C" int vsscanf(const char* str, const char* format, va_list arg_ptr);


/*******************************************************************************/


struct arg_scanf {
  void *data;
  int (*getch)(void*);
  int (*putch)(int,void*);
  };

struct str_data {
  unsigned char* str;
  };


static int sgetc(struct str_data* sd){
  register unsigned int ret = *sd->str++;
  return ret ? (int)ret : -1;
  }

static int sputc(int c,struct str_data* sd){
  return (*--sd->str==c)?c:-1;
  }


#define A_GETC(fn)	(++consumed,(fn)->getch((fn)->data))
#define A_PUTC(c,fn)	(--consumed,(fn)->putch((c),(fn)->data))


static int __v_scanf(arg_scanf* fn,const char *format,va_list arg_ptr){
  int flag_not,flag_dash,flag_width,flag_discard,flag_half,flag_long,flag_longlong;
  unsigned int ch,_div,consumedsofar,consumed;
  unsigned long v;
  double d,factor;
  int width,n,neg,exp,prec,tpch;
  char cset[256],*s;

  /* arg_ptr tmps */
  double *pd;
  float  *pf;
  long   *pl;
  short  *ph;
  int    *pi;

  consumed=0;
  n=0;

  /* get one char */
  tpch=A_GETC(fn);

  while(tpch!=-1 && *format){
    ch=(unsigned int)*format++;
    switch(ch){
      case 0:                                           // End of the format string
        return 0;
      case ' ':                                         // Skip blanks
      case '\f':
      case '\t':
      case '\v':
      case '\n':
      case '\r':
        while(*format && Ascii::isSpace(*format)) ++format;
        while(Ascii::isSpace(tpch)) tpch=A_GETC(fn);
        break;
      case '%':                                         // Format string %
        _div=0;
        width=-1;
        flag_width=0;
        flag_discard=0;
        flag_half=0;
        flag_long=0;
        flag_longlong=0;
in_scan:ch=*format++;
        switch(ch){
          case 0:                                       // End of the format string
            return 0;
          case '%':                                     // Just a % sign
            if((unsigned char)tpch != ch) goto err_out;
            tpch=A_GETC(fn);
            break;
          case '*':                                     // Discard, i.e. don't convert
            flag_discard=1;
            goto in_scan;
          case 'h':                                     // Argument is short
            flag_half=1;
            goto in_scan;
          case 'l':                                     // Argument is long or long long
            if(flag_long) flag_longlong=1;
            flag_long=1;
            goto in_scan;
          case 'q':                                     // Argument is long long
          case 'L':
            flag_longlong=1;
            goto in_scan;
          case '0':                                     // Width specification
          case '1':
          case '2':
          case '3':
          case '4':
          case '5':
          case '6':
          case '7':
          case '8':
          case '9':
            width=strtol(format-1,&s,10);
            format=s;
            flag_width=1;
            goto in_scan;
          case 'p':                                     // Pointer (hex)
          case 'X':
          case 'x':                                     // Hexadecimal
            _div+=6;
          case 'd':                                     // Decimal
          case 'u':
            _div+=2;
          case 'o':                                     // Octal
            _div+=8;
          case 'i':                                     // 'i' may be decimal, octal, or hex
            v=0;
            consumedsofar=consumed;
            while(Ascii::isSpace(tpch)) tpch=A_GETC(fn);
            neg=0;
            if(tpch=='-'){
              tpch=A_GETC(fn);
              neg=1;
              }
            else if(tpch=='+'){
              tpch=A_GETC(fn);
              }
            if((_div==16) && (tpch=='0')) goto scan_hex;
            if(!_div){
              _div=10;
              if(tpch=='0'){
                _div=8;
scan_hex:       tpch=A_GETC(fn);
                if((tpch|32)=='x'){
                  tpch=A_GETC(fn);
                  _div=16;
                  }
                }
              }
            while(tpch!=-1){
              register unsigned long c=tpch&0xff;
              register unsigned long d=c|0x20;
              c=(d>='a'?d-'a'+10:c<='9'?c-'0':0xff);
              if(c>=_div) break;
              d=v*_div;
              v=(d<v)?ULONG_MAX:d+c;
              tpch=A_GETC(fn);
              }
            if((ch|0x20)<'p'){
              if(v>=0-((unsigned long)LONG_MIN)){
                v=(neg)?LONG_MIN:LONG_MAX;
                }
              else{
                if(neg) v*=-1L;
                }
              }
            if(!flag_discard){
              if(flag_long){
                pl=(long *)va_arg(arg_ptr,long*);
                *pl=v;
                }
              else if(flag_half){
                ph=(short*)va_arg(arg_ptr,short*);
                *ph=(short)v;
                }
              else{     // FIXME handle flag_longlong
                pi=(int*)va_arg(arg_ptr,int*);
                *pi=(int)v;
                }
              if(consumedsofar<consumed) ++n;
              }
            break;
          case 'e':                                     // Floating point
          case 'E':
          case 'f':
          case 'g':
            d=0.0;
            while(Ascii::isSpace(tpch)) tpch=A_GETC(fn);
            neg=0;
            if(tpch=='-'){
              tpch=A_GETC(fn);
              neg=1;
              }
            else if(tpch=='+'){
              tpch=A_GETC(fn);
              }
            while(Ascii::isDigit(tpch)){
              d=d*10.0+(tpch-'0');
              tpch=A_GETC(fn);
              }
            if(tpch=='.'){
              factor=.1;
              tpch=A_GETC(fn);
              while(Ascii::isDigit(tpch)){
                d=d+(factor*(tpch-'0'));
                factor/=10;
                tpch=A_GETC(fn);
                }
              }
            if((tpch|0x20)=='e'){
              exp=0;
              prec=tpch;
              tpch=A_GETC(fn);
              factor=10;
              if(tpch=='-'){
                factor=0.1;
                tpch=A_GETC(fn);
                }
              else if(tpch=='+'){
                tpch=A_GETC(fn);
                }
              else{
                d=0;
                if(tpch!=-1) A_PUTC(tpch,fn);
                tpch=prec;
                goto exp_out;
                }
              while(Ascii::isDigit(tpch)){
                exp=exp*10+(tpch-'0');
                tpch=A_GETC(fn);
                }
              while(exp){	/* as in strtod: XXX: this introduces rounding errors */
                d*=factor;
                --exp;
                }
              }
exp_out:    if(neg) d=-d;
            if(!flag_discard){
              if(flag_long){
                pd=(double *)va_arg(arg_ptr,double*);
                *pd=d;
                }
              else {
                pf=(float *)va_arg(arg_ptr,float*);
                *pf=(float)d;
                }
              ++n;
              }
            break;
          case 'c':                                     // Character
            if(!flag_discard){
              s=(char *)va_arg(arg_ptr,char*);
              ++n;
              }
            if(!flag_width) width=1;
            while(width && (tpch!=-1)){
              if(!flag_discard) *s++=tpch;
              --width;
              tpch=A_GETC(fn);
              }
            break;
          case 's':                                     // String
            if(!flag_discard) s=(char *)va_arg(arg_ptr,char*);
            while(Ascii::isSpace(tpch)) tpch=A_GETC(fn);
            while (width && (tpch!=-1) && (!Ascii::isSpace(tpch))){
              if(!flag_discard) *s=tpch;
              if(tpch) ++s; else break;
              --width;
              tpch=A_GETC(fn);
              }
            if(!flag_discard){ *s=0; n++; }
            break;
          case 'n':                                     // Total characters so far, not including %n's
            if(!flag_discard){
              s=(char *)va_arg(arg_ptr,char*);
              }
            if(!flag_discard) *s++=consumed-1;
            break;
          case '[':                                     // Character set
            memset(cset,0,sizeof(cset));
            ch=*format;
            flag_not=0;
            if(ch=='^'){                                // Negated character set
              flag_not=1;
              ch=*++format;
              }
            if(ch=='-' || ch==']'){                     // Special case if first is - or ]
              cset[ch]=1;
              ch=*++format;
              }
            flag_dash=0;
            for( ; *format && *format!=']'; ++format){  // Parse set
              if(flag_dash){
                for( ; ch<=(unsigned int)*format; ++ch) cset[ch]=1;   // Set characters
                flag_dash=0;
                ch=*format;
                }
              else if(*format=='-'){                    // Character range
                flag_dash=1;
                }
              else{                                     // Set single character
                cset[ch]=1;
                ch=*format;
                }
              }
            if(flag_dash)                               // Last character
              cset[(int)'-']=1;
            else
              cset[ch]=1;
            if(!flag_discard){                          // Copy string if not discarded
              s=(char *)va_arg(arg_ptr,char*);
              ++n;
              }
            while(width && (tpch>=0) && (cset[tpch]^flag_not)){
              if(!flag_discard) *s=tpch;
              if(tpch) ++s; else break;
              --width;
              tpch=A_GETC(fn);
              }
            if(!flag_discard) *s=0;
            ++format;
            break;
          default:
            goto err_out;
          }
        break;
      default:                                          // Other characters must match format string
        if((unsigned char)tpch != ch) goto err_out;
        tpch=A_GETC(fn);
        break;
      }
    }
err_out:
  if(tpch<0 && n==0) return EOF;
  A_PUTC(tpch,fn);
  return n;
  }


// API
int vfscanf(FILE *stream,const char *format,va_list arg_ptr){
  arg_scanf farg={(void*)stream,(int(*)(void*))fgetc,(int(*)(int,void*))ungetc};
  return __v_scanf(&farg,format,arg_ptr);
  }


// API
int vscanf(const char *format,va_list arg_ptr){
  return vfscanf(stdin,format,arg_ptr);
  }


// API
int vsscanf(const char* str,const char* format,va_list arg_ptr){
  str_data fdat={(unsigned char*)str};
  arg_scanf farg={(void*)&fdat,(int(*)(void*))sgetc,(int(*)(int,void*))sputc};
  return __v_scanf(&farg,format,arg_ptr);
  }

#endif
