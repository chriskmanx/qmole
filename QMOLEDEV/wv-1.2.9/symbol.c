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

U16
wvConvertSymbolToUnicode (U16 char16)
{
    switch (char16)
      {
      case 0x20:
	  return (0x0020);
      case 0x21:
	  return (0x0021);
      case 0x22:
	  return (0x2200);
      case 0x23:
	  return (0x0023);
      case 0x24:
	  return (0x2203);
      case 0x25:
	  return (0x0025);
      case 0x26:
	  return (0x0026);
      case 0x27:
	  return (0x220B);
      case 0x28:
	  return (0x0028);
      case 0x29:
	  return (0x0029);
      case 0x2A:
	  return (0x2217);
      case 0x2B:
	  return (0x002B);
      case 0x2C:
	  return (0x002C);
      case 0x2D:
	  return (0x2212);
      case 0x2E:
	  return (0x002E);
      case 0x2F:
	  return (0x002F);
      case 0x30:
	  return (0x0030);
      case 0x31:
	  return (0x0031);
      case 0x32:
	  return (0x0032);
      case 0x33:
	  return (0x0033);
      case 0x34:
	  return (0x0034);
      case 0x35:
	  return (0x0035);
      case 0x36:
	  return (0x0036);
      case 0x37:
	  return (0x0037);
      case 0x38:
	  return (0x0038);
      case 0x39:
	  return (0x0039);
      case 0x3A:
	  return (0x003A);
      case 0x3B:
	  return (0x003B);
      case 0x3C:
	  return (0x003C);
      case 0x3D:
	  return (0x003D);
      case 0x3E:
	  return (0x003E);
      case 0x3F:
	  return (0x003F);
      case 0x40:
	  return (0x2245);
      case 0x41:
	  return (0x0391);
      case 0x42:
	  return (0x0392);
      case 0x43:
	  return (0x03A7);
      case 0x44:
	  return (0x0394);
      case 0x45:
	  return (0x0395);
      case 0x46:
	  return (0x03A6);
      case 0x47:
	  return (0x0393);
      case 0x48:
	  return (0x0397);
      case 0x49:
	  return (0x0399);
      case 0x4A:
	  return (0x03D1);
      case 0x4B:
	  return (0x039A);
      case 0x4C:
	  return (0x039B);
      case 0x4D:
	  return (0x039C);
      case 0x4E:
	  return (0x039D);
      case 0x4F:
	  return (0x039F);
      case 0x50:
	  return (0x03A0);
      case 0x51:
	  return (0x0398);
      case 0x52:
	  return (0x03A1);
      case 0x53:
	  return (0x03A3);
      case 0x54:
	  return (0x03A4);
      case 0x55:
	  return (0x03A5);
      case 0x56:
	  return (0x03C2);
      case 0x57:
	  return (0x03A9);
      case 0x58:
	  return (0x039E);
      case 0x59:
	  return (0x03A8);
      case 0x5A:
	  return (0x0396);
      case 0x5B:
	  return (0x005B);
      case 0x5C:
	  return (0x2234);
      case 0x5D:
	  return (0x005D);
      case 0x5E:
	  return (0x22A5);
      case 0x5F:
	  return (0x005F);
      case 0x60:
	  return (0xF8E5);
      case 0x61:
	  return (0x03B1);
      case 0x62:
	  return (0x03B2);
      case 0x63:
	  return (0x03C7);
      case 0x64:
	  return (0x03B4);
      case 0x65:
	  return (0x03B5);
      case 0x66:
	  return (0x03C6);
      case 0x67:
	  return (0x03B3);
      case 0x68:
	  return (0x03B7);
      case 0x69:
	  return (0x03B9);
      case 0x6A:
	  return (0x03D5);
      case 0x6B:
	  return (0x03BA);
      case 0x6C:
	  return (0x03BB);
      case 0x6D:
	  return (0x03BC);
      case 0x6E:
	  return (0x03BD);
      case 0x6F:
	  return (0x03BF);
      case 0x70:
	  return (0x03C0);
      case 0x71:
	  return (0x03B8);
      case 0x72:
	  return (0x03C1);
      case 0x73:
	  return (0x03C3);
      case 0x74:
	  return (0x03C4);
      case 0x75:
	  return (0x03C5);
      case 0x76:
	  return (0x03D6);
      case 0x77:
	  return (0x03C9);
      case 0x78:
	  return (0x03BE);
      case 0x79:
	  return (0x03C8);
      case 0x7A:
	  return (0x03B6);
      case 0x7B:
	  return (0x007B);
      case 0x7C:
	  return (0x007C);
      case 0x7D:
	  return (0x007D);
      case 0x7E:
	  return (0x223C);
      case 0xA0:
	  return (0x20AC);
      case 0xA1:
	  return (0x03D2);
      case 0xA2:
	  return (0x2032);
      case 0xA3:
	  return (0x2264);
      case 0xA4:
	  return (0x2215);
      case 0xA5:
	  return (0x221E);
      case 0xA6:
	  return (0x0192);
      case 0xA7:
	  return (0x2663);
      case 0xA8:
	  return (0x2666);
      case 0xA9:
	  return (0x2665);
      case 0xAA:
	  return (0x2660);
      case 0xAB:
	  return (0x2194);
      case 0xAC:
	  return (0x2190);
      case 0xAD:
	  return (0x2191);
      case 0xAE:
	  return (0x2192);
      case 0xAF:
	  return (0x2193);
      case 0xB0:
	  return (0x00B0);
      case 0xB1:
	  return (0x00B1);
      case 0xB2:
	  return (0x2033);
      case 0xB3:
	  return (0x2265);
      case 0xB4:
	  return (0x00D7);
      case 0xB5:
	  return (0x221D);
      case 0xB6:
	  return (0x2202);
      case 0xB7:
	  return (0x2022);
      case 0xB8:
	  return (0x00F7);
      case 0xB9:
	  return (0x2260);
      case 0xBA:
	  return (0x2261);
      case 0xBB:
	  return (0x2248);
      case 0xBC:
	  return (0x2026);
      case 0xBD:
	  return (0xF8E6);
      case 0xBE:
	  return (0xF8E7);
      case 0xBF:
	  return (0x21B5);
      case 0xC0:
	  return (0x2135);
      case 0xC1:
	  return (0x2111);
      case 0xC2:
	  return (0x211C);
      case 0xC3:
	  return (0x2118);
      case 0xC4:
	  return (0x2297);
      case 0xC5:
	  return (0x2295);
      case 0xC6:
	  return (0x2205);
      case 0xC7:
	  return (0x2229);
      case 0xC8:
	  return (0x222A);
      case 0xC9:
	  return (0x2283);
      case 0xCA:
	  return (0x2287);
      case 0xCB:
	  return (0x2284);
      case 0xCC:
	  return (0x2282);
      case 0xCD:
	  return (0x2286);
      case 0xCE:
	  return (0x2208);
      case 0xCF:
	  return (0x2209);
      case 0xD0:
	  return (0x2220);
      case 0xD1:
	  return (0x2207);

	  /*
	     case 0xD2: return(0xF6DA);   
	     case 0xD3: return(0xF6D9);   
	     case 0xD4: return(0xF6DB);   
	   */
	  /* this works the other doesnt */
      case 0xD2:
	  return (0x00AE);
      case 0xD3:
	  return (0x00A9);
      case 0xD4:
	  return (0x2122);

      case 0xD5:
	  return (0x220F);
      case 0xD6:
	  return (0x221A);
      case 0xD7:
	  return (0x22C5);
      case 0xD8:
	  return (0x00AC);
      case 0xD9:
	  return (0x2227);
      case 0xDA:
	  return (0x2228);
      case 0xDB:
	  return (0x21D4);
      case 0xDC:
	  return (0x21D0);
      case 0xDD:
	  return (0x21D1);
      case 0xDE:
	  return (0x21D2);
      case 0xDF:
	  return (0x21D3);
      case 0xE0:
	  return (0x25CA);

	  /*
	     case 0xE1: return(0x2329);   
	     case 0xE2: return(0xF8E8);   
	     case 0xE3: return(0xF8E9);   
	     case 0xE4: return(0xF8EA);   
	   */
	  /* again these are technically incorrect, but they work better */
      case 0xE1:
	  return (0x003C);
      case 0xE2:
	  return (0x00AE);
      case 0xE3:
	  return (0x00A9);
      case 0xE4:
	  return (0x2122);


      case 0xE5:
	  return (0x2211);
      case 0xE6:
	  return (0xF8EB);
      case 0xE7:
	  return (0xF8EC);
      case 0xE8:
	  return (0xF8ED);
      case 0xE9:
	  return (0xF8EE);
      case 0xEA:
	  return (0xF8EF);
      case 0xEB:
	  return (0xF8F0);
      case 0xEC:
	  return (0xF8F1);
      case 0xED:
	  return (0xF8F2);
      case 0xEE:
	  return (0xF8F3);
      case 0xEF:
	  return (0xF8F4);
      case 0xF1:
	  return (0x232A);
      case 0xF2:
	  return (0x222B);
      case 0xF3:
	  return (0x2320);
      case 0xF4:
	  return (0xF8F5);
      case 0xF5:
	  return (0x2321);
      case 0xF6:
	  return (0xF8F6);
      case 0xF7:
	  return (0xF8F7);
      case 0xF8:
	  return (0xF8F8);
      case 0xF9:
	  return (0xF8F9);
      case 0xFA:
	  return (0xF8FA);
      case 0xFB:
	  return (0xF8FB);
      case 0xFC:
	  return (0xF8FC);
      case 0xFD:
	  return (0xF8FD);
      case 0xFE:
	  return (0xF8FE);
      default:
	  return (char16);
      }
}
