/********************************************************************************
*                                                                               *
*                    G a u s s i a n   J i t t e r   T a b l e                  *
*                                                                               *
*********************************************************************************
* Copyright (C) 1995,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: jitter.h,v 1.9 2006/01/22 17:58:58 fox Exp $                             *
********************************************************************************/

#define JIT8   1

/* Points gaussian distributed in range -0.5<X<0.5, -0.5<Y<0.5 */

#ifdef JIT8
double jitter[][2] = {          // Eight
  {-0.334818,  0.435331},
  { 0.286438, -0.393495},
  { 0.459462,  0.141540},
  {-0.414498, -0.192829},
  {-0.183790,  0.082102},
  {-0.079263, -0.317383},
  { 0.102254,  0.299133},
  { 0.164216, -0.054399}
  };
#endif

#ifdef JIT15
double jitter[][2] = {          // fifteen
  { 0.285561,  0.188437},
  { 0.360176, -0.065688},
  {-0.111751,  0.275019},
  {-0.055918, -0.215197},
  {-0.080231, -0.470965},
  { 0.138721,  0.409168},
  { 0.384120,  0.458500},
  {-0.454968,  0.134088},
  { 0.179271, -0.331196},
  {-0.307049, -0.364927},
  { 0.105354, -0.010099},
  {-0.154180,  0.021794},
  {-0.370135, -0.116425},
  { 0.451636, -0.300013},
  {-0.370610,  0.387504}
  };
#endif
