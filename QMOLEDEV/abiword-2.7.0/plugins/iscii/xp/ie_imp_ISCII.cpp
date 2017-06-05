/* -*- mode: C++; tab-width: 4; c-basic-offset: 4; -*- */

/* AbiWord
 * Copyright (C) 2001 AbiSource, Inc.
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ut_types.h"
#include "ut_assert.h"
#include "ut_debugmsg.h"
#include "ut_string.h"
#include "ie_imp_ISCII.h"
#include "ie_types.h"
#include "pd_Document.h"
#include "ut_growbuf.h"

/*
 * This file is meant to import ISCII documents.
 * ISCII is a text encoding for Indic scripts.
 * Only Devanagari is supported for now.
 */

/*****************************************************************/
/*****************************************************************/

ImportISCIIStreamFile::ImportISCIIStreamFile(GsfInput *pFile)
	: ImportStreamFile(pFile),
	  m_cLookAhead(0),
	  m_bNeedByte(true)
{
}

/*!
  Get UCS-2 character from stream
 \param ucs Reference to the character

 Get the next UCS character, converting from file's encoding
 */
bool ImportISCIIStreamFile::getRawChar(UT_UCSChar &ucs)
{
	UT_UCS4Char wc = 0;
	unsigned char c;

	if (_get_eof())
		return false;

	// prefetch lookahead byte
	if (m_bNeedByte) 
    {
		if (!_getByte(m_cLookAhead))
		{
			m_cLookAhead = 0;
			_set_eof( true );
		}
		else
			m_bNeedByte = false;
    }
	if (!_get_eof())
	{
		c = m_cLookAhead;
		if (!_getByte(m_cLookAhead))
		{
			m_cLookAhead = 0;
			_set_eof ( true );
		}
		if (c < 0x80)
			wc = c;
		else switch (c)
		{
			case 0xA1:
				if (m_cLookAhead == ISCII_NUKTA)
				{
					wc = 0x0950;			// DEVANAGARI OM
					m_bNeedByte = true;
				}
				else
					wc = 0x0901;			// DEVANAGARI SIGN CANDRABINDU
				break;
			case 0xA2: wc = 0x0902; break; 	// DEVANAGARI SIGN ANUSVARA
			case 0xA3: wc = 0x0903; break; 	// DEVANAGARI SIGN VISARGA
			case 0xA4: wc = 0x0905; break; 	// DEVANAGARI LETTER A
			case 0xA5: wc = 0x0906; break; 	// DEVANAGARI LETTER AA
			case 0xA6:
				if (m_cLookAhead == ISCII_NUKTA)
				{
					wc = 0x090C;			// DEVANAGARI LETTER VOCALIC L
					m_bNeedByte = true;
				}
				else
					wc = 0x0907; 			// DEVANAGARI LETTER I
				break;
			case 0xA7:
				if (m_cLookAhead == ISCII_NUKTA)
				{
					wc = 0x0961;			// DEVANAGARI LETTER VOCALIC LL
					m_bNeedByte = true;
				}
				else
					wc = 0x0908; 			// DEVANAGARI LETTER II
				break;
			case 0xA8: wc = 0x0909; break; 	// DEVANAGARI LETTER U
			case 0xA9: wc = 0x090A; break; 	// DEVANAGARI LETTER UU
			case 0xAA:
				if (m_cLookAhead == ISCII_NUKTA)
				{
					wc = 0x0960;			// DEVANAGARI LETTER VOCALIC RR
					m_bNeedByte = true;
				}
				else
					wc = 0x090B; 			// DEVANAGARI LETTER VOCALIC R
				break;
			case 0xAB: wc = 0x090E; break; 	// DEVANAGARI LETTER SHORT E
			case 0xAC: wc = 0x090F; break; 	// DEVANAGARI LETTER E
			case 0xAD: wc = 0x0910; break; 	// DEVANAGARI LETTER AI
			case 0xAE: wc = 0x090D; break; 	// DEVANAGARI LETTER CANDRA E
			case 0xAF: wc = 0x0912; break; 	// DEVANAGARI LETTER SHORT O
			case 0xB0: wc = 0x0913; break; 	// DEVANAGARI LETTER O
			case 0xB1: wc = 0x0914; break; 	// DEVANAGARI LETTER AU
			case 0xB2: wc = 0x0911; break; 	// DEVANAGARI LETTER CANDRA O
			case 0xB3:
				if (m_cLookAhead == ISCII_NUKTA)
				{
					wc = 0x0958;			// DEVANAGARI LETTER QA
					m_bNeedByte = true;
				}
				else
					wc = 0x0915; 			// DEVANAGARI LETTER KA
				break;
			case 0xB4:
				if (m_cLookAhead == ISCII_NUKTA)
				{
					wc = 0x0959;			// DEVANAGARI LETTER KHHA
					m_bNeedByte = true;
				}
				else
					wc = 0x0916; 			// DEVANAGARI LETTER KHA
				break;
			case 0xB5:
				if (m_cLookAhead == ISCII_NUKTA)
				{
					wc = 0x095A;			// DEVANAGARI LETTER GHHA
					m_bNeedByte = true;
				}
				else
					wc = 0x0917; 			// DEVANAGARI LETTER GA
				break;
			case 0xB6: wc = 0x0918; break; 	// DEVANAGARI LETTER GHA
			case 0xB7: wc = 0x0919; break; 	// DEVANAGARI LETTER NGA
			case 0xB8: wc = 0x091A; break; 	// DEVANAGARI LETTER CA
			case 0xB9: wc = 0x091B; break; 	// DEVANAGARI LETTER CHA
			case 0xBA:
				if (m_cLookAhead == ISCII_NUKTA)
				{
					wc = 0x095B;			// DEVANAGARI LETTER ZA
					m_bNeedByte = true;
				}
				else
					wc = 0x091C; 			// DEVANAGARI LETTER JA
				break;
			case 0xBB: wc = 0x091D; break; 	// DEVANAGARI LETTER JHA
			case 0xBC: wc = 0x091E; break; 	// DEVANAGARI LETTER NYA
			case 0xBD: wc = 0x091F; break; 	// DEVANAGARI LETTER TTA
			case 0xBE: wc = 0x0920; break; 	// DEVANAGARI LETTER TTHA
			case 0xBF:
				if (m_cLookAhead == ISCII_NUKTA)
				{
					wc = 0x095C;			// DEVANAGARI LETTER DDDHA
					m_bNeedByte = true;
				}
				else
					wc = 0x0921; 			// DEVANAGARI LETTER DDA
				break;
			case 0xC0:
				if (m_cLookAhead == ISCII_NUKTA)
				{
					wc = 0x095D;			// DEVANAGARI LETTER RHA
					m_bNeedByte = true;
				}
				else
					wc = 0x0922; 			// DEVANAGARI LETTER DDHA
				break;
			case 0xC1: wc = 0x0923; break; 	// DEVANAGARI LETTER NNA
			case 0xC2: wc = 0x0924; break; 	// DEVANAGARI LETTER TA
			case 0xC3: wc = 0x0925; break; 	// DEVANAGARI LETTER THA
			case 0xC4: wc = 0x0926; break; 	// DEVANAGARI LETTER DA
			case 0xC5: wc = 0x0927; break; 	// DEVANAGARI LETTER DHA
			case 0xC6: wc = 0x0928; break; 	// DEVANAGARI LETTER NA
			case 0xC7: wc = 0x0929; break; 	// DEVANAGARI LETTER NNNA
			case 0xC8: wc = 0x092A; break; 	// DEVANAGARI LETTER PA
			case 0xC9:
				if (m_cLookAhead == ISCII_NUKTA)
				{
					wc = 0x095E;			// DEVANAGARI LETTER FA
					m_bNeedByte = true;
				}
				else
					wc = 0x092B; 			// DEVANAGARI LETTER PHA
				break;
			case 0xCA: wc = 0x092C; break; 	// DEVANAGARI LETTER BA
			case 0xCB: wc = 0x092D; break; 	// DEVANAGARI LETTER BHA
			case 0xCC: wc = 0x092E; break; 	// DEVANAGARI LETTER MA
			case 0xCD: wc = 0x092F; break; 	// DEVANAGARI LETTER YA
			case 0xCE: wc = 0x095F; break; 	// DEVANAGARI LETTER YYA
			case 0xCF: wc = 0x0930; break; 	// DEVANAGARI LETTER RA
			case 0xD0: wc = 0x0931; break; 	// DEVANAGARI LETTER RRA
			case 0xD1: wc = 0x0932; break; 	// DEVANAGARI LETTER LA
			case 0xD2: wc = 0x0933; break; 	// DEVANAGARI LETTER LLA
			case 0xD3: wc = 0x0934; break; 	// DEVANAGARI LETTER LLLA
			case 0xD4: wc = 0x0935; break; 	// DEVANAGARI LETTER VA
			case 0xD5: wc = 0x0936; break; 	// DEVANAGARI LETTER SHA
			case 0xD6: wc = 0x0937; break; 	// DEVANAGARI LETTER SSA
			case 0xD7: wc = 0x0938; break; 	// DEVANAGARI LETTER SA
			case 0xD8: wc = 0x0939; break; 	// DEVANAGARI LETTER HA
			case ISCII_INV: wc = 0x200D; break; 	// ZERO WIDTH JOINER
			case 0xDA: wc = 0x093E; break; 	// DEVANAGARI VOWEL SIGN AA
			case 0xDB:
				if (m_cLookAhead == ISCII_NUKTA)
				{
					wc = 0x0962;			// DEVANAGARI VOWEL SIGN VOCALIC L
					m_bNeedByte = true;
				}
				else
					wc = 0x093F; 			// DEVANAGARI VOWEL SIGN I
				break;
			case 0xDC:
				if (m_cLookAhead == ISCII_NUKTA)
				{
					wc = 0x0963;			// DEVANAGARI VOWEL SIGN VOCALIC LL
					m_bNeedByte = true;
				}
				else
					wc = 0x0940; 			// DEVANAGARI VOWEL SIGN II
				break;
			case 0xDD: wc = 0x0941; break; 	// DEVANAGARI VOWEL SIGN U
			case 0xDE: wc = 0x0942; break; 	// DEVANAGARI VOWEL SIGN UU
			case 0xDF:
				if (m_cLookAhead == ISCII_NUKTA)
				{
					wc = 0x0944;			// DEVANAGARI VOWEL SIGN VOCALIC RR
					m_bNeedByte = true;
				}
				else
					wc = 0x0943; 			// DEVANAGARI VOWEL SIGN VOCALIC R
				break;
			case 0xE0: wc = 0x0946; break; 	// DEVANAGARI VOWEL SIGN SHORT E
			case 0xE1: wc = 0x0947; break; 	// DEVANAGARI VOWEL SIGN E
			case 0xE2: wc = 0x0948; break; 	// DEVANAGARI VOWEL SIGN AI
			case 0xE3: wc = 0x0945; break; 	// DEVANAGARI VOWEL SIGN CANDRA E
			case 0xE4: wc = 0x094A; break; 	// DEVANAGARI VOWEL SIGN SHORT O
			case 0xE5: wc = 0x094B; break; 	// DEVANAGARI VOWEL SIGN O
			case 0xE6: wc = 0x094C; break; 	// DEVANAGARI VOWEL SIGN AU
			case 0xE7: wc = 0x0949; break; 	// DEVANAGARI VOWEL SIGN CANDRA O
			case 0xE8: wc = 0x094D; break; 	// DEVANAGARI SIGN VIRAMA
			case ISCII_NUKTA: wc = 0x093C; break; 	// DEVANAGARI SIGN NUKTA
			case 0xEA:
				if (m_cLookAhead == ISCII_NUKTA)
				{
					wc = 0x093D;			// DEVANAGARI SIGN AVAGRAHA
					m_bNeedByte = true;
				}
				else if (m_cLookAhead == 0xEA)
				{
					wc = 0x0965;			// DEVANAGARI DOUBLE DANDA
					m_bNeedByte = true;
				}
				else
					wc = 0x0964; 			// DEVANAGARI DANDA
				break;
			case ISCII_EXT:
				if (m_cLookAhead == 0xB8)
				{
					wc = 0x0952;			// DEVANAGARI STRESS SIGN ANUDATTA
					m_bNeedByte = true;
				}
				else if (m_cLookAhead == 0xBF)
				{
					wc = 0x0970;			// DEVANAGARI ABBREVIATION SIGN
					m_bNeedByte = true;
				}
				else
					wc = '?';	// TODO Should be UCS_REPLACECHAR
				break;
			case 0xF1: wc = 0x0966; break; 	// DEVANAGARI DIGIT ZERO
			case 0xF2: wc = 0x0967; break; 	// DEVANAGARI DIGIT ONE
			case 0xF3: wc = 0x0968; break; 	// DEVANAGARI DIGIT TWO
			case 0xF4: wc = 0x0969; break; 	// DEVANAGARI DIGIT THREE
			case 0xF5: wc = 0x096A; break; 	// DEVANAGARI DIGIT FOUR
			case 0xF6: wc = 0x096B; break; 	// DEVANAGARI DIGIT FIVE
			case 0xF7: wc = 0x096C; break; 	// DEVANAGARI DIGIT SIX
			case 0xF8: wc = 0x096D; break; 	// DEVANAGARI DIGIT SEVEN
			case 0xF9: wc = 0x096E; break; 	// DEVANAGARI DIGIT EIGHT
			case 0xFA: wc = 0x096F; break; 	// DEVANAGARI DIGIT NINE
			default:
				wc = '?';	// TODO Should be UCS_REPLACECHAR
		}
	}

	ucs = _lookAhead ();
	_lookAhead ( wc );

	return true;
}

/*****************************************************************/
/*****************************************************************/

IE_Imp_ISCII_Sniffer::IE_Imp_ISCII_Sniffer (const char * _name) :
  IE_ImpSniffer(_name)
{
  // 
}

// supported suffixes
static IE_SuffixConfidence IE_Imp_ISCII_Sniffer__SuffixConfidence[] = {
	{ "isc", 	UT_CONFIDENCE_PERFECT 	},
	{ "iscii", 	UT_CONFIDENCE_PERFECT 	},
	{ "", 	UT_CONFIDENCE_ZILCH 	}
};

const IE_SuffixConfidence * IE_Imp_ISCII_Sniffer::getSuffixConfidence ()
{
	return IE_Imp_ISCII_Sniffer__SuffixConfidence;
}

UT_Confidence_t IE_Imp_ISCII_Sniffer::recognizeContents(const char * /*szBuf*/, 
										   UT_uint32 /*iNumbytes*/)
{
  // We don't attempt to identify the contents.
  return UT_CONFIDENCE_ZILCH;
}

UT_Error IE_Imp_ISCII_Sniffer::constructImporter(PD_Document * pDocument,
											   IE_Imp ** ppie)
{
	IE_Imp_ISCII * p = new IE_Imp_ISCII(pDocument);
	*ppie = p;
	return UT_OK;
}

bool IE_Imp_ISCII_Sniffer::getDlgLabels(const char ** pszDesc,
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

IE_Imp_ISCII::IE_Imp_ISCII(PD_Document * pDocument)
	: IE_Imp_Text(pDocument, false)
{
	_setEncoding(0);
}

/*!
  Create a stream of the appropriate type
 \param pStream Pointer to created stream
 \param fp File to construct stream from
 */
UT_Error IE_Imp_ISCII::_constructStream(ImportStream *& pStream, GsfInput * fp)
{
	return (pStream = new ImportISCIIStreamFile(fp)) ? UT_OK : UT_IE_NOMEMORY;
}

/*****************************************************************/
/*****************************************************************/
