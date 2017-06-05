/*
 * AbiCommand - Abiword plugin for a command line interface
 * Copyright (C) 2002 by Martin Sevior
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

#ifndef PLUGIN_ABICOMMAND_H
#define PLUGIN_ABICOMMAND_H
#include "ut_string_class.h"
class PD_Document;
class AP_UnixFrame;
class AV_View;
class UnixNull_Graphics;
class FL_DocLayout;
class XAP_App;
class UT_UTF8String;

class AbiCommand
{
public:
	                         AbiCommand(void);
	                         AbiCommand(bool bAbiCollab);
	                        ~AbiCommand(void);
	void                     doCommands(void);
	UT_sint32                parseTokens(UT_GenericVector<const UT_UTF8String*> * pToks);
	bool                     printFiles(const UT_GenericVector<const UT_UTF8String*> * pToks);
	bool                     replaceAll(const UT_GenericVector<const UT_UTF8String*> * pToks);
	bool                     replaceNext(const UT_GenericVector<const UT_UTF8String*> * pToks);
	bool                     movePoint(const UT_GenericVector<const UT_UTF8String*> * pToks);
	bool                     deleteText(const UT_GenericVector<const UT_UTF8String*> * pToks);
	bool                     insertText(const UT_GenericVector<const UT_UTF8String*> * pToks);
	bool                     replaceDocument(PD_Document * pDoc);
	bool                     loadDocument(UT_UTF8String & sPathToDoc);
	bool                     newDocument(void);
	PD_Document *            getCurrentDocument();
	void                     clearTokenVector(UT_GenericVector<const UT_UTF8String*> & Toks);
	bool                     invoke(const char * pszCommand);
	bool                     tokenizeString(UT_GenericVector<const UT_UTF8String*> & tok, char * pStr);
	void                     nullUpdate();
	bool                     removeGraphicalView(void);
	void                     deleteCurrentDoc(void);
	bool                     viewDoc(void);
private:
	PD_Document *            m_pCurDoc;
	UT_UTF8String *          m_pCurFile;
	AP_UnixFrame  *          m_pCurFrame;
	AV_View *                m_pCurView;
	UnixNull_Graphics *      m_pG;
	FL_DocLayout *           m_pLayout;
	XAP_App *                m_pApp;
	bool                     m_bViewDoc;
	bool                     m_bRunAsServer;
	UT_uint32                m_iPID;
	bool                     m_bRunAsAbiCollab;
	UT_UTF8String            m_sErrorFile;

};

#endif /* PLUGIN_ABICOMMAND_H */












