////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements aplication's main method
//////////////////////////////////////////////////////////////////////////// 

#include "config.h"
#include <gtk/gtk.h>
#include "core/_ftp/xSocket.h"
#include "callbacks.h"
#include "support.h"
#include "core/GuiLanguage.h"
#include "core/IniFile.h"
#include "ThreadSafeGui.h"
#include "DualPanel.h"
#include "MainWindow.h"
#include "core/PluginManager.h"
#include "core/VfsManager.h"
#include "core/PathName.h"
#include "core/System.h"

#ifdef _WIN32
 #include "core/_win/SingleInstance.h"
 CSingleInstance g_instance("Atol");
#else
 #include "core/_unx/SingleInstance.h"
 CSingleInstance g_instance("/Atol");
#endif

int gtkMessageBox(const char *szText, int nButtons = GTK_BUTTONS_OK, int nIcon = GTK_MESSAGE_INFO);

GtkWidget *atol_main = NULL;
bool g_bMinimizeToTray = false;
String g_strEditor;
String g_strTerminal;
DualPanel g_dp;
PluginManager g_PlugManager;
VfsManager g_VfsManager;
OpManager  g_objOpManager;
MainWindow g_wnd;

int gtkMessageBox(const char *szText, int nButtons, int nIcon)
{
	GtkWidget* msgbox;
	msgbox = gtk_message_dialog_new ( (GtkWindow*)atol_main,
		GTK_DIALOG_MODAL,
		(GtkMessageType)nIcon,
		(GtkButtonsType)nButtons,
		szText);
	gint result = gtk_dialog_run (GTK_DIALOG (msgbox));
	gtk_widget_destroy (msgbox);
	return result;
}

int gtkMessageBoxYNC(const char *szText)
{
	GtkWidget* msgbox;
	msgbox = gtk_message_dialog_new ( (GtkWindow*)atol_main,
		GTK_DIALOG_DESTROY_WITH_PARENT,
		(GtkMessageType)GTK_MESSAGE_INFO,
		(GtkButtonsType)GTK_BUTTONS_NONE,
		szText);

	gtk_dialog_add_button(GTK_DIALOG(msgbox), GTK_STOCK_YES,    GTK_RESPONSE_YES);
	gtk_dialog_add_button(GTK_DIALOG(msgbox), GTK_STOCK_NO,     GTK_RESPONSE_NO);
	gtk_dialog_add_button(GTK_DIALOG(msgbox), GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL);

	gint result = gtk_dialog_run (GTK_DIALOG (msgbox));
	gtk_widget_destroy (msgbox);
	return result;
}


const char *get_locale_dir()
{
	static std::string strDir;
#ifdef _WIN32
	strDir  = System::GetAppPath();
	strDir  = PathName::GetParentDirPath(strDir.c_str());
	strDir += "locale\\";
#else
	strDir  = INSTALL_PREFIX;
	strDir += LOCALE_DIR;
#endif
	return strDir.c_str();
}

const char *GetIniFile()
{
	static std::string strDir;
	strDir  = System::GetHomeDir();
	strDir += "/.atol/atol.ini";
	return strDir.c_str();
}

GuiLanguage     g_lang(PACKAGE, get_locale_dir());
ThreadSafeQueue g_gui_pump;

int main (int argc, char *argv[])
{
#ifdef _WIN32
	CoInitialize(NULL);
	OleInitialize(NULL);
	xSocket::InitSockets();
#endif

	//check startup options
	IniFile file;
	file.Load(GetIniFile());

	//initialize language system
	std::string strLocale;
	file.GetValue("Display", "Language", strLocale, "");
	g_lang.Initialize(strLocale.c_str()); 

	//multithread support
	if (!g_thread_supported ()) 
		g_thread_init (NULL);
	gdk_threads_init();	

	g_gui_pump.Init();
 	gdk_threads_enter();

	gtk_init (&argc, &argv);
	
	bool bAllowSingleInst = false;
	file.GetValue("Startup", "AllowSingleInstance", bAllowSingleInst);
	if(bAllowSingleInst && g_instance.ProgramAlreadyStarted())
	{
		gtkMessageBox(_("Another Atol instance detected! Exiting!"));
		return 0;
	}

	g_wnd.Create(argc >= 2 ? argv[1] : NULL, argc >= 3 ? argv[2] : NULL);

	gtk_main ();
	gdk_threads_leave();

#ifdef _WIN32
	OleUninitialize();
#endif 
	return 0;
}

#ifdef _WIN32
#include <windows.h>
int APIENTRY WinMain( HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPSTR lpCmdLine, int nCmdShow)
{
	return main( __argc, __argv );
}
#endif

