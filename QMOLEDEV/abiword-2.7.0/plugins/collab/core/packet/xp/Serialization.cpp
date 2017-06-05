/*
 * AbiCollab - Code to enable the modification of remote documents.
 * Copyright (C) 2007 by Marc Maurer <uwog@uwog.net>
 * Copyright (C) 2007 One Laptop Per Child
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include <stdlib.h>

#include "Serialization.h"

Archive& operator<<( Archive& ar, CompactInt& c )
{
	if (ar.isLoading()) 
	{
		// loading
		unsigned int shift = 0;
		unsigned char byteA = 0; 
		c.Val = 0;
		ar << byteA;
		if( byteA & 0x40 ) {
			shift >>= 6;
			unsigned char byteB = (unsigned char)((shift < 0x80) ? shift : ((shift & 0x7f)+0x80)); 
			ar << byteB;
			if( byteB & 0x80 ) {
				shift >>= 7;
				unsigned char byteC = (unsigned char)((shift < 0x80) ? shift : ((shift & 0x7f)+0x80)); 
				ar << byteC;
				if( byteC & 0x80 ) {
					shift >>= 7;
					unsigned char byteD = (unsigned char)((shift < 0x80) ? shift : ((shift & 0x7f)+0x80)); 
					ar << byteD;
					if( byteD & 0x80 ) {
						shift >>= 7; 
						unsigned char byteE = (unsigned char)(shift);
						ar << byteE;
						c.Val = byteE;
					} 
					c.Val = (c.Val << 7) + (byteD & 0x7f);
				}
				c.Val = (c.Val << 7) + (byteC & 0x7f);
			}
			c.Val = (c.Val << 7) + (byteB & 0x7f); 
		}
		c.Val = (c.Val << 6) + (byteA & 0x3f);
		
		// check if we need to negate
		if (byteA & 0x80) c.Val = -c.Val;
	}
	else
	{
		// saving
		// use absolute value, but remember if original value was negative or not
		unsigned int val = abs(c.Val);
		unsigned char byteA = (unsigned char)(((c.Val>=0) ? 0 : 0x80) + ((val < 0x40) ? val : ((val & 0x3f)+0x40)));
		ar << byteA;
		if( byteA & 0x40 ) {
			val >>= 6;
			unsigned char byteB = (unsigned char)((val < 0x80) ? val : ((val & 0x7f)+0x80));
			ar << byteB;
			if( byteB & 0x80 ) {
				val >>= 7;
				unsigned char byteC = (unsigned char)((val < 0x80) ? val : ((val & 0x7f)+0x80)); 
				ar << byteC;
				if( byteC & 0x80 ) {
					val >>= 7;
					unsigned char byteD = (unsigned char)((val < 0x80) ? val : ((val & 0x7f)+0x80)); 
					ar << byteD;
					if( byteD & 0x80 ) {
						val >>= 7; 
						unsigned char byteE = (unsigned char)(val);
						ar << byteE;
					}
				}
			}
		}
	}
	return ar;
}
