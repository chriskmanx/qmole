/*
 *  flex_parser.h
 *	part of galculator
 *  	(c) 2002-2013 Simon Flöry (simon.floery@rechenraum.com)
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifndef _FLEX_PARSER_H
#define _FLEX_PARSER_H 1

#define FLEX_PARSER_NR_RECURSIONS	10

enum {
	FLEX_PARSER_NUMBER,
	FLEX_PARSER_CONSTANT,
	FLEX_PARSER_FUNCTION,
	FLEX_PARSER_USER_FUNCTION
};

typedef struct {
	G_REAL		value;
	gboolean	error;
} s_flex_parser_result;

s_flex_parser_result flex_parser (const char *string);

#endif /* flex_parser.h */
