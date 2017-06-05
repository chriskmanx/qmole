////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Main interface to create viewer for a given file
////////////////////////////////////////////////////////////////////////////

#ifndef FILEVIEWER_H__
#define FILEVIEWER_H__

//TOFIX??? enums
#define VIEW_AUTO		0
#define VIEW_TEXT		1
#define VIEW_BIN		2
#define VIEW_HEX		3

bool Lister_ViewFile(const char *szFile, int nViewType = VIEW_AUTO, bool bDelete = false);

#endif
