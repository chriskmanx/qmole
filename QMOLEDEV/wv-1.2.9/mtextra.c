/* wvWare
 * Copyright (C) Caolan McNamara, Dom Lachowicz, and others
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include "wv.h"

/*
 * This is the unicode translation table for "MT Extra", which is 
 * part of Math Type, and for some reason shipped with Word 6.
 *
 * http://support.microsoft.com/support/kb/articles/Q106/3/41.asp
 *
 * The unicode translation table is here:
 * 
 * http://www.mathtype.com/support/tech/encodings/mtextra.htm
 * 
 * Copyright 2001, Sean Young <sean@msxnet.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

U16
wvConvertMTExtraToUnicode (U16 char16)
{
    if (char16 < ' ')
      return char16;

    switch (char16)
      {
      case 0x20: return 0x0020; /* Space */
      case 0x23: return 0x0300; /* Grave embellishment */
      case 0x24: return 0x0302; /* Hat embellishment */
      case 0x25: return 0x0303; /* Tilde embellishment */
      case 0x26: return 0x0307; /* Dot above embellishment */
      case 0x28: return 0x2323; /* Smile */
      case 0x29: return 0x2322; /* Frown */
      case 0x31: return 0xEC00; /* Down-pointing curly bracket left */
      case 0x32: return 0xEC01; /* Down-pointing curly bracket mid */
      case 0x33: return 0xEC02; /* Down-pointing curly bracket right */
      case 0x34: return 0xEC03; /* Horizontal curly bracket extender */
      case 0x35: return 0xEC0B; /* Horizontal square bracket extender */
      case 0x36: return 0xEC04; /* Up-pointing curly bracket left */
      case 0x37: return 0xEC05; /* Up-pointing curly bracket mid */
      case 0x38: return 0xEC06; /* Up-pointing curly bracket right */
      case 0x3A: return 0x223C; /* Tilde operator */
      case 0x3B: return 0x2243; /* Asymptotically equal to */
      case 0x3C: return 0x22B2; /* Normal subgroup of */
      case 0x3D: return 0x226A; /* Much less-than */
      case 0x3E: return 0x22B3; /* Contains as normal subgroup */
      case 0x3F: return 0x226B; /* Much greater-than */
      case 0x40: return 0x225C; /* Delta equal to */
      case 0x41: return 0x2259; /* Estimates */
      case 0x42: return 0x2250; /* Approaches the limit */
      case 0x43: return 0x2210; /* N-ary coproduct */
      case 0x44: return 0x019B; /* Latin small letter lambda with stroke */
      case 0x45: return 0xEC0E; /* Under square bracket left */
      case 0x46: return 0xEC0F; /* Under square bracket right */
      case 0x47: return 0xEC10; /* Over square bracket left */
      case 0x48: return 0xEC11; /* Over square bracket right */
      case 0x49: return 0x2229; /* Intersection */
      case 0x4A: return 0x2127; /* Inverted ohm sign */
      case 0x4B: return 0x2026; /* Horizontal ellipsis */
      case 0x4C: return 0x22EF; /* Math-axis ellipsis */
      case 0x4D: return 0x22EE; /* Vertical ellipsis */
      case 0x4E: return 0x22F0; /* Up right diagonal ellipsis */
      case 0x4F: return 0x22F1; /* Down right diagonal ellipsis */
      case 0x50: return 0x2225; /* Parallel to */
      case 0x51: return 0x2235; /* Because or since */
      case 0x52: return 0x2221; /* Measured angle */
      case 0x53: return 0x2222; /* Spherical angle */
      case 0x55: return 0x222A; /* Union */
      case 0x56: return 0x25B3; /* White up-pointing triangle */
      case 0x57: return 0x25A1; /* White square */
      case 0x58: return 0x25AD; /* White rectangle */
      case 0x59: return 0x25B1; /* White parallelogram */
      case 0x5A: return 0x2197; /* North east arrow */
      case 0x5B: return 0x2199; /* South west arrow */
      case 0x5C: return 0xEB05; /* Double arrow northeast southwest */
      case 0x5D: return 0x2198; /* South east arrow */
      case 0x5E: return 0x2196; /* North west arrow */
      case 0x5F: return 0xEB06; /* Double arrow northwest southeast */
      case 0x60: return 0x2035; /* Reversed prime */
      case 0x61: return 0x21A6; /* Rightwards arrow from bar */
      case 0x62: return 0x2195; /* Up down arrow */
      case 0x63: return 0x21D5; /* Up down double arrow */
      case 0x64: return 0x25CB; /* White circle */
      case 0x65: return 0x2299; /* Circled dot operator */
      case 0x66: return 0x227B; /* Succeeds */
      case 0x67: return 0xE98F; /* Medium dot operator (free radical) */
      case 0x68: return 0x210F; /* Planck constant over two pi */
      case 0x69: return 0xEE00; /* Anticlockwise contour integral loop */
      case 0x6A: return 0xEE01; /* Clockwise contour integral loop */
      case 0x6C: return 0x2113; /* Script small l */
      case 0x6D: return 0x2213; /* Minus-plus sign */
      case 0x6E: return 0xFFFD; /* Unknown or missing character */
      case 0x6F: return 0x2218; /* Composition */
      case 0x70: return 0x227A; /* Precedes */
      case 0x71: return 0xEB1A; /* Left harpoon (barb down) */
      case 0x72: return 0x20D7; /* Right arrow embellishment */
      case 0x73: return 0x20D6; /* Left arrow embellishment */
      case 0x74: return 0x20E1; /* Left right arrow embellishment */
      case 0x75: return 0xEB00; /* Arrow embellishment extender */
      case 0x76: return 0x20D1; /* Right harpoon embellishment (barb up) */
      case 0x77: return 0x20D0; /* Left harpoon embellishment (barb up) */
      case 0x78: return 0xEB19; /* Right harpoon (barb down) */
      case 0x7B: return 0xFE38; /* Under curly bracket */
      case 0x7C: return 0xEC0C; /* Under square bracket */
      case 0x7D: return 0xFE37; /* Over curly bracket */
      case 0x7E: return 0xEC0D; /* Over square bracket */
      case 0x80: return 0x21C4; /* Rightwards arrow over leftwards arrow */
      case 0x81: return 0xEB01; /* Arrow rightwards over small arrow leftwards */
      case 0x82: return 0xEB02; /* Small arrow rightwards over arrow leftwards */
      case 0x83: return 0x21CC; /* Right harpoon over left harpoon */
      case 0x84: return 0xEB03; /* Harpoon right over small harpoon left */
      case 0x85: return 0xEB04; /* Small harpoon right over harpoon left */
      case 0x86: return 0x21C0; /* Right harpoon (barb up) */
      case 0x87: return 0x21BD; /* Left harpoon (barb down) */
      case 0x88: return 0xF8E7; /* Horizontal arrow extender */
      case 0xA1: return 0x211D; /* Blackboard-bold capital R */
      case 0xA2: return 0x2124; /* Blackboard-bold capital Z */
      case 0xA3: return 0x2102; /* Blackboard-bold capital C */
      case 0xA4: return 0x211A; /* Blackboard-bold capital Q */
      case 0xA5: return 0x2115; /* Blackboard-bold capital N */
      case 0xA7: return 0x301A; /* Left white square bracket */
      case 0xA8: return 0x301B; /* Right white square bracket */
      case 0xA9: return 0xEC22; /* Left white square bracket top */
      case 0xAA: return 0xEC23; /* Left white square bracket extender */
      case 0xAB: return 0xEC24; /* Left white square bracket bottom */
      case 0xAC: return 0xEC25; /* Right white square bracket top */
      case 0xAD: return 0xEC26; /* Right white square bracket extender */
      case 0xAE: return 0xEC27; /* Right white square bracket bottom */
      case 0xB0: return 0xEE04; /* Tilde embellishment (two space) */
      case 0xB1: return 0xEE05; /* Tilde embellishment (three space) */
      case 0xB2: return 0xEE06; /* Tilde embellishment (four space) */
      case 0xB5: return 0xEE07; /* Hat embellishment (two space) */
      case 0xB6: return 0xEE08; /* Hat embellishment (three space) */
      case 0xB7: return 0xEE09; /* Hat embellishment (four space) */
      case 0xBA: return 0xEE0A; /* Arc embellishment (two space) */
      case 0xBB: return 0xEE0B; /* Arc embellishment (three space) */
      case 0xBC: return 0xEE0C; /* Arc embellishment (four space) */
      case 0xC0: return 0xEE0D; /* Joint status embellishment */
      case 0xC1: return 0xEE0E; /* Joint status embellishment (left) */
      case 0xC2: return 0xEE0F; /* Joint status embellishment (right) */
      case 0xC3: return 0xEE10; /* Joint status embellishment (extender) */
      case 0xD1: return 0xEE11; /* Integral loop */
      case 0xD2: return 0xEE12; /* Integral loop (double) */
      case 0xD3: return 0xEE13; /* Integral loop (triple) */
      case 0xD5: return 0xEE15; /* Expanding integral loop (double) */
      case 0xD6: return 0xEE16; /* Expanding integral loop (triple) */
      case 0xE7: return 0xF8FF; /* Apple logo */
      case 0xE8: return 0x0160; /* Latin capital letter S with caron */
      case 0xE9: return 0x00DD; /* Latin capital letter Y with acute */
      case 0xEA: return 0x00DE; /* Latin capital letter Thorn */
      case 0xEB: return 0x00D0; /* Latin capital letter Eth */
      case 0xEC: return 0x0161; /* Latin small letter s with caron */
      case 0xED: return 0x00FD; /* Latin small letter y with acute */
      case 0xEE: return 0x00FE; /* Latin small letter thorn */
      case 0xEF: return 0x00F0; /* Latin small letter eth */
      case 0xF0: return 0xFB01; /* Latin small ligature fi */
      case 0xF1: return 0xFB02; /* Latin small ligature fl */
      case 0xF2: return 0x0131; /* Latin small letter dotless i */
      case 0xF3: return 0x00B9; /* Superscript one */
      case 0xF4: return 0x00B2; /* Superscript two */
      case 0xF5: return 0x00B3; /* Superscript three */
      case 0xF6: return 0x00BD; /* Vulgar fraction one half */
      case 0xF7: return 0x00BC; /* Vulgar fraction one quarter */
      case 0xF8: return 0x00BE; /* Vulgar fraction three quarters */
      case 0xF9: return 0x2044; /* Fraction slash */
      case 0xFA: return 0x00A6; /* Broken bar */
      case 0xFB: return 0x02DD; /* Double acute accent */
      case 0xFC: return 0x02D8; /* Breve */
      case 0xFD: return 0x02C7; /* Caron */
      case 0xFE: return 0x02DA; /* Ring above */
      case 0xFF: return 0x02DB; /* Ogonek */
      default: return (0xffff);
      }
}
