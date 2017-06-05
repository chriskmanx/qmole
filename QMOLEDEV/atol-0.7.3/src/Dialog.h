////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements base class for GTK Dialog window (experimental)
////////////////////////////////////////////////////////////////////////////

#ifndef DIALOG_H__
#define DIALOG_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <gtk/gtk.h>

class Dialog  
{
public:
	Dialog();
	virtual ~Dialog();

	gint ShowModal();
	GtkWidget *GetDialog(){ return m_pDialog; };
	
	virtual void Create(); // = 0;
	virtual void Destroy();

//protected:
	GtkWidget *m_pDialog;
};

#endif // DIALOG_H__
