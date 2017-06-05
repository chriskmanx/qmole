/*
 * CORBA C language mapping tests
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * Author: Phil Dawes <philipd@users.sourceforge.net>
 */

#ifndef EVERYTHING_CONSTANTS_H
#define EVERYTHING_CONSTANTS_H

#include "everything.h"

const CORBA_char * constants_STRING_IN="In string";
const CORBA_char * constants_STRING_INOUT_IN="Inout in string";
const CORBA_char * constants_STRING_INOUT_OUT="Inout out string";
const CORBA_char * constants_STRING_OUT="Out string";
const CORBA_char * constants_STRING_RETN="Retn String";


#define constants_LONG_IN 0x12345678
#define constants_LONG_INOUT_IN 0x34567812
#define constants_LONG_INOUT_OUT 0x56781234
#define constants_LONG_OUT 0x78123456
#define constants_LONG_RETN 0xAABBCCDD

#define constants_LONG_LONG_IN constants_LONG_IN
#define constants_LONG_LONG_INOUT_IN constants_LONG_INOUT_IN
#define constants_LONG_LONG_INOUT_OUT constants_LONG_INOUT_OUT
#define constants_LONG_LONG_OUT constants_LONG_OUT
#define constants_LONG_LONG_RETN constants_LONG_RETN

#define constants_FLOAT_IN ((CORBA_float) 127.13534)
#define constants_FLOAT_INOUT_IN ((CORBA_float) 124.89432)
#define constants_FLOAT_INOUT_OUT ((CORBA_float) 975.12694)
#define constants_FLOAT_OUT ((CORBA_float) 112.54575)
#define constants_FLOAT_RETN ((CORBA_float) 354.23535)

#define constants_DOUBLE_IN ((CORBA_double) 127.13534)
#define constants_DOUBLE_INOUT_IN ((CORBA_double) 124.89432)
#define constants_DOUBLE_INOUT_OUT ((CORBA_double) 975.12694)
#define constants_DOUBLE_OUT ((CORBA_double) 112.54575)
#define constants_DOUBLE_RETN ((CORBA_double) 354.23535)

#define constants_LONG_DOUBLE_IN ((CORBA_long_double) 127.13534)
#define constants_LONG_DOUBLE_INOUT_IN ((CORBA_long_double) 124.89432)
#define constants_LONG_DOUBLE_INOUT_OUT ((CORBA_long_double) 975.12694)
#define constants_LONG_DOUBLE_OUT ((CORBA_long_double) 112.54575)
#define constants_LONG_DOUBLE_RETN ((CORBA_long_double) 354.23535)

const CORBA_char constants_CHAR_IN=0x23;
const CORBA_char constants_CHAR_INOUT_IN=0x45;
const CORBA_char constants_CHAR_INOUT_OUT=0x67;
const CORBA_char constants_CHAR_OUT=0x89;
const CORBA_char constants_CHAR_RETN=0xAC;

const CORBA_octet constants_OCTET_IN=0x13;
const CORBA_octet constants_OCTET_INOUT_IN=0x35;
const CORBA_octet constants_OCTET_INOUT_OUT=0x57;
const CORBA_octet constants_OCTET_OUT=0x79;
const CORBA_octet constants_OCTET_RETN=0xBD;

const CORBA_short constants_SHORT_IN=0x1234;
const CORBA_short constants_SHORT_INOUT_IN=0x3456;
const CORBA_short constants_SHORT_INOUT_OUT=0x5678;
const CORBA_short constants_SHORT_OUT=0x7812;
const CORBA_short constants_SHORT_RETN=0xAABB;


const CORBA_char * constants_SEQ_STRING_IN[] = { "in1","in2","in3","in4" };
const CORBA_char * constants_SEQ_STRING_OUT[] = { "out1","out2","out3","out4" };
const CORBA_char * constants_SEQ_STRING_INOUT_IN[] = { "inout1","inout2","inout3","inout4" };
const CORBA_char * constants_SEQ_STRING_INOUT_OUT[] = { "inout21","inout22","inout23","inout24" }; 
const CORBA_char * constants_SEQ_STRING_RETN[] = { "retn1","retn2","retn3","retn4" };

const CORBA_long constants_SEQ_LONG_IN[] = { constants_LONG_IN,constants_LONG_INOUT_IN,15,7  };
const CORBA_long constants_SEQ_LONG_OUT[] = { constants_LONG_INOUT_IN, constants_LONG_INOUT_OUT,15,7 };
const CORBA_long constants_SEQ_LONG_INOUT_IN[] = { constants_LONG_INOUT_OUT, constants_LONG_OUT,7,15 };
const CORBA_long constants_SEQ_LONG_INOUT_OUT[] = { constants_LONG_OUT, constants_LONG_RETN,8,9 };
const CORBA_long constants_SEQ_LONG_RETN[] = { constants_LONG_RETN, constants_LONG_IN,2,3 };

const CORBA_long constants_SEQ_OCTET_IN[] = { 1, 3, 5, 7 };
const CORBA_long constants_SEQ_OCTET_OUT[] = { 2, 7, 9, 255 };
const CORBA_long constants_SEQ_OCTET_INOUT_IN[] = { 1, 15, 8, 0 };
const CORBA_long constants_SEQ_OCTET_INOUT_OUT[] = { 73, 128, 173, 15 };
const CORBA_long constants_SEQ_OCTET_RETN[] = { 1, 3, 5, 7 };

#endif
