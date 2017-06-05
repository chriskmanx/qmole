/* AbiSource
 * 
 * Copyright (C) 2002 Dom Lachowicz <cinamod@hotmail.com>
 * Copyright (C) 2004 Robert Staudinger <robsta@stereolyzer.net>
 * 
 * Copyright (C) 2005 INdT
 * Author: Daniel d'Andrada T. de Carvalho <daniel.carvalho@indt.org.br>
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



#ifndef _IE_EXP_OPENDOCUMENT_H_
#define _IE_EXP_OPENDOCUMENT_H_

// External includes
#include <gsf/gsf-output.h>

// AbiWord includes
#include <ie_exp.h>

class IE_Exp_OpenDocument : public IE_Exp
{
public:
    IE_Exp_OpenDocument(PD_Document * pDocument);
    virtual ~IE_Exp_OpenDocument();

protected:
    virtual GsfOutput* _openFile(const char *szFilename);
    virtual UT_Error  _writeDocument(void);
  
private:
    // The OpenDocument Text file.
    GsfOutfile* m_odt;
};

#endif //_IE_EXP_OPENDOCUMENT_H_
