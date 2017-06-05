////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements base class for GTK Dialog window (experimental)
////////////////////////////////////////////////////////////////////////////

#include "Dialog.h"

Dialog::Dialog()
{
	m_pDialog = NULL;
}

Dialog::~Dialog()
{
	//Destroy();
}

void Dialog::Create()
{
}

gint Dialog::ShowModal()
{
	gint res = gtk_dialog_run(GTK_DIALOG(m_pDialog));
	return res;
}

void Dialog::Destroy()
{
	if(m_pDialog)
		gtk_widget_destroy(m_pDialog);
	m_pDialog = NULL;
}


