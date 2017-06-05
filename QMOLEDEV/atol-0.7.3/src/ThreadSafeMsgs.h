////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implementation for some thread-safe message boxes
////////////////////////////////////////////////////////////////////////////

#ifndef THREADSAFEMSGS_H__
#define THREADSAFEMSGS_H__

#include <gtk/gtk.h>
#include "core/String.h"

int MsgBox_ThrSafe(String strTitle, int nButtons = GTK_BUTTONS_OK, bool bYNC = false);

//wrappers
int MsgOverwrite_ThrSafe(String strTitle);
int MsgCopyError_ThrSafe(String strTitle);
int MsgNameInput_ThrSafe(String strTitle, String &strValue, bool bPassword = false);
int MsgDelete_ThrSafe(String strFile);
int MsgDeleteDir_ThrSafe(String strFile);
int MsgDeleteError_ThrSafe(String strMsg);
int MsgOperationError_ThrSafe(String strMsg);

//
void Refresh_ThrSafe(long list);

#endif //THREADSAFEMSGS_H__
