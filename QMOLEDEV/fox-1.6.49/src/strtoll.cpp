/********************************************************************************
*                                                                               *
*             S t r t o l l  a n d   S t r t o u l l   R o u t i n e s          *
*                                                                               *
*********************************************************************************
* Copyright (C) 2005,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: strtoll.cpp,v 1.9.2.2 2007/11/16 14:50:40 fox Exp $                          *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxascii.h"


/*
  Notes:
  - Furnish our own versions of these functions if not available.
*/

//#undef HAVE_STRTOLL
//#undef HAVE_STRTOULL


// Systems that like LL for 64-bit long constant suffix
#if defined(__CYGWIN__) || defined(__MINGW32__) || defined(__SC__) || !defined(WIN32)
#ifndef ULLONG_MAX
#define ULLONG_MAX 18446744073709551615ULL
#endif
#ifndef LLONG_MAX
#define LLONG_MAX  9223372036854775807LL
#endif
#ifndef LLONG_MIN
#define LLONG_MIN  (-LLONG_MAX-1LL)
#endif

// Systems with L for 64-bit long constant suffix
#else
#ifndef ULLONG_MAX
#define ULLONG_MAX 18446744073709551615UL
#endif
#ifndef LLONG_MAX
#define LLONG_MAX  9223372036854775807L
#endif
#ifndef LLONG_MIN
#define LLONG_MIN  (-LLONG_MAX-1L)
#endif
#endif


/*******************************************************************************/

using namespace FX;

#ifndef HAVE_STRTOLL

extern "C" FXlong strtoll(const char *nptr,char **endptr,int base);

FXlong strtoll(const char *nptr,char **endptr,int base){
  register FXulong cutoff,cutlim,value;
  register FXchar *s=(FXchar*)nptr;
  register FXuchar neg=0;
  register FXuchar any=0;
  register FXuchar ovf=0;
  register FXuchar c;

  // Initialize
  value=0;

  // Assume no conversion
  if(endptr) *endptr=s;

  // Skip spaces
  while(*s!='\0' && Ascii::isSpace(*s)) s++;

  // Process sign
  if(*s=='-'){
    neg=1;
    s++;
    }
  else if(*s=='+'){
    s++;
    }

  // Figure base if not given
  if(base<2){
    base=10;
    if(s[0]=='0'){
      base=8;
      if(s[1]=='x' || s[1]=='X') base=16;
      }
    }

  // Process 0x if hexadecimal
  if(base==16 && s[0]=='0' && (s[1]=='x' || s[1]=='X')) s+=2;

  // Overflow detection
  cutoff=neg?LLONG_MAX+1:LLONG_MAX;
  cutlim=cutoff%base;
  cutoff=cutoff/base;

  // Process digits
  while((c=*s++)!='\0'){

    // Digit value
    if(Ascii::isDigit(c)){
      c=c-'0';
      }
    else if(Ascii::isLetter(c)){
      c=Ascii::toUpper(c)-'A'+10;
      }
    else
      break;

    // Digit value out of range
    if(c>=base) break;

    // Check for overflow
    if(ovf || value>cutoff || (value==cutoff && c>cutlim)){ ovf=1; continue; }

    // Accumulate
    value*=base;
    value+=c;
    any=1;
    }

  // Overflow
  if(ovf){
    value = neg ? LLONG_MIN : LLONG_MAX;
    errno=ERANGE;
    }

  // Negative
  else if(neg){
    value=-value;
    }

  // Return end of number
  if(any){
    if(endptr) *endptr=s;
    }

  // Done
  return value;
  }

#endif



#ifndef HAVE_STRTOULL

extern "C" FXulong strtoull(const char *nptr,char **endptr,int base);


FXulong strtoull(const char *nptr,char **endptr,int base){
  register FXulong cutoff,cutlim,value;
  register FXchar *s=(FXchar*)nptr;
  register FXuchar neg=0;
  register FXuchar any=0;
  register FXuchar ovf=0;
  register FXuchar c;

  // Initialize
  value=0;

  // Assume no conversion
  if(endptr) *endptr=s;

  // Skip spaces
  while(*s!='\0' && Ascii::isSpace(*s)) s++;

  // Process sign
  if(*s=='-'){
    neg=1;
    s++;
    }
  else if(*s=='+'){
    s++;
    }

  // Figure base if not given
  if(base<2){
    base=10;
    if(s[0]=='0'){
      base=8;
      if(s[1]=='x' || s[1]=='X') base=16;
      }
    }

  // Process 0x if hexadecimal
  if(base==16 && s[0]=='0' && (s[1]=='x' || s[1]=='X')) s+=2;

  // Overflow detection
  cutoff=ULLONG_MAX;
  cutlim=cutoff%base;
  cutoff=cutoff/base;

  // Process digits
  while((c=*s++)!='\0'){

    // Digit value
    if(Ascii::isDigit(c)){
      c=c-'0';
      }
    else if(Ascii::isLetter(c)){
      c=Ascii::toUpper(c)-'A'+10;
      }
    else
      break;

    // Digit value out of range
    if(c>=base) break;

    // Check for overflow
    if(ovf || value>cutoff || (value==cutoff && c>cutlim)){ ovf=1; continue; }

    // Accumulate
    value*=base;
    value+=c;
    any=1;
    }

  // Overflow
  if(ovf){
    value=ULLONG_MAX;
    errno=ERANGE;
    }

  // Negative
  else if(neg){
    value=-value;
    }

  // Return end of number
  if(any){
    if(endptr) *endptr=s;
    }

  // Done
  return value;
  }

#endif
