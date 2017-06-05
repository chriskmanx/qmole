/* AbiWord
 * Copyright (C) 1998 AbiSource, Inc.
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

#include "string.h"

#include "ie_exp_ISCII.h"
#include "ut_assert.h"
#include "ut_string.h"
#include "pd_Document.h"

/*****************************************************************/
/*****************************************************************/

IE_Exp_ISCII_Sniffer::IE_Exp_ISCII_Sniffer (const char * _name) :
  IE_ExpSniffer(_name)
{
  // 
}

bool IE_Exp_ISCII_Sniffer::recognizeSuffix(const char * szSuffix)
{
	return (!g_ascii_strcasecmp(szSuffix,".isc") || !g_ascii_strcasecmp(szSuffix, ".iscii"));
}

UT_Error IE_Exp_ISCII_Sniffer::constructExporter(PD_Document * pDocument,
													 IE_Exp ** ppie)
{
	IE_Exp_ISCII * p = new IE_Exp_ISCII(pDocument);
	*ppie = p;
	return UT_OK;
}

bool IE_Exp_ISCII_Sniffer::getDlgLabels(const char ** pszDesc,
											const char ** pszSuffixList,
											IEFileType * ft)
{
	*pszDesc = "ISCII Text (.isc, .iscii)";
	*pszSuffixList = "*.isc; *.iscii";
	*ft = getFileType();
	return true;
}

/*****************************************************************/
/*****************************************************************/

PL_Listener * IE_Exp_ISCII::_constructListener(void)
{
	return new ISCII_Listener(getDoc(),this);
}

/*****************************************************************/
/*****************************************************************/

/*!
  Convert wide char to ISCII
 \param data pC Buffer to write converted data to
 \param length Filled with number of bytes converted to
 \param wc Wide character to convert

 This function is designed to be interchangeable with wctomb()
 */
int ISCII_Listener::_wctomb(char * pC, int & length, UT_UCS4Char wc)
{
	if (wc < 0x80)
	{
		*pC = char(wc);
		length = 1;
		return 1;
	}
	switch (wc)
	{
		case 0x0901: *pC = char(0xA1); length = 1; return 1;	// DEVANAGARI SIGN CANDRABINDU
		case 0x0902: *pC = char(0xA2); length = 1; return 1;	// DEVANAGARI SIGN ANUSVARA
		case 0x0903: *pC = char(0xA3); length = 1; return 1;	// DEVANAGARI SIGN VISARGA
		case 0x0905: *pC = char(0xA4); length = 1; return 1;	// DEVANAGARI LETTER A
		case 0x0906: *pC = char(0xA5); length = 1; return 1;	// DEVANAGARI LETTER AA
		case 0x0907: *pC = char(0xA6); length = 1; return 1;	// DEVANAGARI LETTER I
		case 0x0908: *pC = char(0xA7); length = 1; return 1;	// DEVANAGARI LETTER II
		case 0x0909: *pC = char(0xA8); length = 1; return 1;	// DEVANAGARI LETTER U
		case 0x090A: *pC = char(0xA9); length = 1; return 1;	// DEVANAGARI LETTER UU
		case 0x090B: *pC = char(0xAA); length = 1; return 1;	// DEVANAGARI LETTER VOCALIC R
		case 0x090E: *pC = char(0xAB); length = 1; return 1;	// DEVANAGARI LETTER SHORT E
		case 0x090F: *pC = char(0xAC); length = 1; return 1;	// DEVANAGARI LETTER E
		case 0x0910: *pC = char(0xAD); length = 1; return 1;	// DEVANAGARI LETTER AI
		case 0x090D: *pC = char(0xAE); length = 1; return 1;	// DEVANAGARI LETTER CANDRA E
		case 0x0912: *pC = char(0xAF); length = 1; return 1;	// DEVANAGARI LETTER SHORT O
		case 0x0913: *pC = char(0xB0); length = 1; return 1;	// DEVANAGARI LETTER O
		case 0x0914: *pC = char(0xB1); length = 1; return 1;	// DEVANAGARI LETTER AU
		case 0x0911: *pC = char(0xB2); length = 1; return 1;	// DEVANAGARI LETTER CANDRA O
		case 0x0915: *pC = char(0xB3); length = 1; return 1;	// DEVANAGARI LETTER KA
		case 0x0916: *pC = char(0xB4); length = 1; return 1;	// DEVANAGARI LETTER KHA
		case 0x0917: *pC = char(0xB5); length = 1; return 1;	// DEVANAGARI LETTER GA
		case 0x0918: *pC = char(0xB6); length = 1; return 1;	// DEVANAGARI LETTER GHA
		case 0x0919: *pC = char(0xB7); length = 1; return 1;	// DEVANAGARI LETTER NGA
		case 0x091A: *pC = char(0xB8); length = 1; return 1;	// DEVANAGARI LETTER CA
		case 0x091B: *pC = char(0xB9); length = 1; return 1;	// DEVANAGARI LETTER CHA
		case 0x091C: *pC = char(0xBA); length = 1; return 1;	// DEVANAGARI LETTER JA
		case 0x091D: *pC = char(0xBB); length = 1; return 1;	// DEVANAGARI LETTER JHA
		case 0x091E: *pC = char(0xBC); length = 1; return 1;	// DEVANAGARI LETTER NYA
		case 0x091F: *pC = char(0xBD); length = 1; return 1;	// DEVANAGARI LETTER TTA
		case 0x0920: *pC = char(0xBE); length = 1; return 1;	// DEVANAGARI LETTER TTHA
		case 0x0921: *pC = char(0xBF); length = 1; return 1;	// DEVANAGARI LETTER DDA
		case 0x0922: *pC = char(0xC0); length = 1; return 1;	// DEVANAGARI LETTER DDHA
		case 0x0923: *pC = char(0xC1); length = 1; return 1;	// DEVANAGARI LETTER NNA
		case 0x0924: *pC = char(0xC2); length = 1; return 1;	// DEVANAGARI LETTER TA
		case 0x0925: *pC = char(0xC3); length = 1; return 1;	// DEVANAGARI LETTER THA
		case 0x0926: *pC = char(0xC4); length = 1; return 1;	// DEVANAGARI LETTER DA
		case 0x0927: *pC = char(0xC5); length = 1; return 1;	// DEVANAGARI LETTER DHA
		case 0x0928: *pC = char(0xC6); length = 1; return 1;	// DEVANAGARI LETTER NA
		case 0x0929: *pC = char(0xC7); length = 1; return 1;	// DEVANAGARI LETTER NNNA
		case 0x092A: *pC = char(0xC8); length = 1; return 1;	// DEVANAGARI LETTER PA
		case 0x092B: *pC = char(0xC9); length = 1; return 1;	// DEVANAGARI LETTER PHA
		case 0x092C: *pC = char(0xCA); length = 1; return 1;	// DEVANAGARI LETTER BA
		case 0x092D: *pC = char(0xCB); length = 1; return 1;	// DEVANAGARI LETTER BHA
		case 0x092E: *pC = char(0xCC); length = 1; return 1;	// DEVANAGARI LETTER MA
		case 0x092F: *pC = char(0xCD); length = 1; return 1;	// DEVANAGARI LETTER YA
		case 0x095F: *pC = char(0xCE); length = 1; return 1;	// DEVANAGARI LETTER YYA
		case 0x0930: *pC = char(0xCF); length = 1; return 1;	// DEVANAGARI LETTER RA
		case 0x0931: *pC = char(0xD0); length = 1; return 1;	// DEVANAGARI LETTER RRA
		case 0x0932: *pC = char(0xD1); length = 1; return 1;	// DEVANAGARI LETTER LA
		case 0x0933: *pC = char(0xD2); length = 1; return 1;	// DEVANAGARI LETTER LLA
		case 0x0934: *pC = char(0xD3); length = 1; return 1;	// DEVANAGARI LETTER LLLA
		case 0x0935: *pC = char(0xD4); length = 1; return 1;	// DEVANAGARI LETTER VA
		case 0x0936: *pC = char(0xD5); length = 1; return 1;	// DEVANAGARI LETTER SHA
		case 0x0937: *pC = char(0xD6); length = 1; return 1;	// DEVANAGARI LETTER SSA
		case 0x0938: *pC = char(0xD7); length = 1; return 1;	// DEVANAGARI LETTER SA
		case 0x0939: *pC = char(0xD8); length = 1; return 1;	// DEVANAGARI LETTER HA
		case 0x200D: *pC = char(0xD9); length = 1; return 1;	// ZERO WIDTH JOINER
		case 0x093E: *pC = char(0xDA); length = 1; return 1;	// DEVANAGARI VOWEL SIGN AA
		case 0x093F: *pC = char(0xDB); length = 1; return 1;	// DEVANAGARI VOWEL SIGN I
		case 0x0940: *pC = char(0xDC); length = 1; return 1;	// DEVANAGARI VOWEL SIGN II
		case 0x0941: *pC = char(0xDD); length = 1; return 1;	// DEVANAGARI VOWEL SIGN U
		case 0x0942: *pC = char(0xDE); length = 1; return 1;	// DEVANAGARI VOWEL SIGN UU
		case 0x0943: *pC = char(0xDF); length = 1; return 1;	// DEVANAGARI VOWEL SIGN VOCALIC R
		case 0x0946: *pC = char(0xE0); length = 1; return 1;	// DEVANAGARI VOWEL SIGN SHORT E
		case 0x0947: *pC = char(0xE1); length = 1; return 1;	// DEVANAGARI VOWEL SIGN E
		case 0x0948: *pC = char(0xE2); length = 1; return 1;	// DEVANAGARI VOWEL SIGN AI
		case 0x0945: *pC = char(0xE3); length = 1; return 1;	// DEVANAGARI VOWEL SIGN CANDRA E
		case 0x094A: *pC = char(0xE4); length = 1; return 1;	// DEVANAGARI VOWEL SIGN SHORT O
		case 0x094B: *pC = char(0xE5); length = 1; return 1;	// DEVANAGARI VOWEL SIGN O
		case 0x094C: *pC = char(0xE6); length = 1; return 1;	// DEVANAGARI VOWEL SIGN AU
		case 0x0949: *pC = char(0xE7); length = 1; return 1;	// DEVANAGARI VOWEL SIGN CANDRA O
		case 0x094D: *pC = char(0xE8); length = 1; return 1;	// DEVANAGARI SIGN VIRAMA
		case 0x093C: *pC = char(0xE9); length = 1; return 1;	// DEVANAGARI SIGN NUKTA
		case 0x0964: *pC = char(0xEA); length = 1; return 1;	// DEVANAGARI DANDA
		case 0x0966: *pC = char(0xF1); length = 1; return 1;	// DEVANAGARI DIGIT ZERO
		case 0x0967: *pC = char(0xF2); length = 1; return 1;	// DEVANAGARI DIGIT ONE
		case 0x0968: *pC = char(0xF3); length = 1; return 1;	// DEVANAGARI DIGIT TWO
		case 0x0969: *pC = char(0xF4); length = 1; return 1;	// DEVANAGARI DIGIT THREE
		case 0x096A: *pC = char(0xF5); length = 1; return 1;	// DEVANAGARI DIGIT FOUR
		case 0x096B: *pC = char(0xF6); length = 1; return 1;	// DEVANAGARI DIGIT FIVE
		case 0x096C: *pC = char(0xF7); length = 1; return 1;	// DEVANAGARI DIGIT SIX
		case 0x096D: *pC = char(0xF8); length = 1; return 1;	// DEVANAGARI DIGIT SEVEN
		case 0x096E: *pC = char(0xF9); length = 1; return 1;	// DEVANAGARI DIGIT EIGHT
		case 0x096F: *pC = char(0xFA); length = 1; return 1;	// DEVANAGARI DIGIT NINE
		case 0x0950: *pC++ = char(0xA1); *pC = char(0xE9); length = 2; return 1;	// DEVANAGARI OM
		case 0x090C: *pC++ = char(0xA6); *pC = char(0xE9); length = 2; return 1;	// DEVANAGARI LETTER VOCALIC L
		case 0x0961: *pC++ = char(0xA7); *pC = char(0xE9); length = 2; return 1;	// DEVANAGARI LETTER VOCALIC LL
		case 0x0960: *pC++ = char(0xAA); *pC = char(0xE9); length = 2; return 1;	// DEVANAGARI LETTER VOCALIC RR
		case 0x0958: *pC++ = char(0xB3); *pC = char(0xE9); length = 2; return 1;	// DEVANAGARI LETTER QA
		case 0x0959: *pC++ = char(0xB4); *pC = char(0xE9); length = 2; return 1;	// DEVANAGARI LETTER KHHA
		case 0x095A: *pC++ = char(0xB5); *pC = char(0xE9); length = 2; return 1;	// DEVANAGARI LETTER GHHA
		case 0x095B: *pC++ = char(0xBA); *pC = char(0xE9); length = 2; return 1;	// DEVANAGARI LETTER ZA
		case 0x095C: *pC++ = char(0xBF); *pC = char(0xE9); length = 2; return 1;	// DEVANAGARI LETTER DDDHA
		case 0x095D: *pC++ = char(0xC0); *pC = char(0xE9); length = 2; return 1;	// DEVANAGARI LETTER RHA
		case 0x095E: *pC++ = char(0xC9); *pC = char(0xE9); length = 2; return 1;	// DEVANAGARI LETTER FA
		case 0x0962: *pC++ = char(0xDB); *pC = char(0xE9); length = 2; return 1;	// DEVANAGARI VOWEL SIGN VOCALIC L
		case 0x0963: *pC++ = char(0xDC); *pC = char(0xE9); length = 2; return 1;	// DEVANAGARI VOWEL SIGN VOCALIC LL
		case 0x0944: *pC++ = char(0xDF); *pC = char(0xE9); length = 2; return 1;	// DEVANAGARI VOWEL SIGN VOCALIC RR
		case 0x093D: *pC++ = char(0xEA); *pC = char(0xE9); length = 2; return 1;	// DEVANAGARI SIGN AVAGRAHA
		case 0x0965: *pC++ = char(0xEA); *pC = char(0xEA); length = 2; return 1;	// DEVANAGARI DOUBLE DANDA
		case 0x0952: *pC++ = char(0xF0); *pC = char(0xB8); length = 2; return 1;	// DEVANAGARI STRESS SIGN ANUDATTA
		case 0x0970: *pC++ = char(0xF0); *pC = char(0xBF); length = 2; return 1;	// DEVANAGARI ABBREVIATION SIGN
		default: return 0;
	}
	return 0;
}

