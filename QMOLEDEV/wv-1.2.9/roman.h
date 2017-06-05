#ifndef ROMAN_HEADER
#define ROMAN_HEADER
/* roman.c by Adam Rogoyski (apoc@laker.net) Temperanc on EFNet irc
 * Copyright (C) 1998 Adam Rogoyski
 * Converts Decimal numbers to Roman Numerals and Roman Numberals to
 * Decimals on the command line or in Interactive mode.
 * Uses an expanded Roman Numeral set to handle numbers up to 999999999
 * compile: gcc -o roman roman.c -O2
 * --- GNU General Public License Disclamer ---
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */


long formString (char **, long, char, char);
char *chomp (char *);
char *decimalToRoman (long, char *);
long romanToDecimal (char *);
long value (char);
#endif
