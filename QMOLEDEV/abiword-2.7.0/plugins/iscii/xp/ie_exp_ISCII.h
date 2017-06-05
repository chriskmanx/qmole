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

#ifndef IE_EXP_ISCII_H
#define IE_EXP_ISCII_H

#include "ie_exp_Text.h"

// The exporter/writer for ISCII text files.

class IE_Exp_ISCII_Sniffer : public IE_ExpSniffer
{
	friend class IE_Exp;

public:
	IE_Exp_ISCII_Sniffer (const char * name);
	virtual ~IE_Exp_ISCII_Sniffer () {}

	virtual bool recognizeSuffix(const char * szSuffix);
	virtual bool getDlgLabels(const char ** szDesc,
							  const char ** szSuffixList,
							  IEFileType * ft);
	virtual UT_Error constructExporter(PD_Document * pDocument,
									   IE_Exp ** ppie);
};

class IE_Exp_ISCII : public IE_Exp_Text
{
public:
	IE_Exp_ISCII(PD_Document * pDocument) : IE_Exp_Text(pDocument) {}
	virtual ~IE_Exp_ISCII() {}

protected:
	virtual PL_Listener *	_constructListener(void);
};

//////////////////////////////////////////////////////////////////
// a private listener class to help us translate the document
// into a text stream.
//////////////////////////////////////////////////////////////////

class ISCII_Listener : public Text_Listener
{
public:
	ISCII_Listener(PD_Document * pDocument, IE_Exp_Text * pie) : Text_Listener(pDocument, pie) {}
	virtual ~ISCII_Listener() {}

protected:
	virtual int			_wctomb(char * pC, int & length, UT_UCS4Char wc);
};

#endif /* IE_EXP_ISCII_H */
