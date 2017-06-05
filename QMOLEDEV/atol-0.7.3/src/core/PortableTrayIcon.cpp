////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements portable string class
//////////////////////////////////////////////////////////////////////////// 

#include "PortableTrayIcon.h"
#include "../../res/atol.xpm"
#include <gtk/gtk.h>
#include "../support.h"

#ifdef _WIN32
 #include "../../res/resource.h"
#endif

extern PortableTrayIcon g_tray;
extern GtkWidget *atol_main;

void ShowTrayMenu();
void OnTrayDefaultAction();
void on_tray_restore (GtkMenuItem *menuitem, gpointer user_data);
void on_tray_quit (GtkMenuItem *menuitem, gpointer user_data);
void on_quit1_activate (GtkMenuItem     *menuitem, gpointer  user_data);
static void clicked (GtkWidget *widget, GdkEventButton *event, gpointer data);
 
PortableTrayIcon::PortableTrayIcon()
{
#ifdef _WIN32
#else
	m_tray_icon = NULL;
#endif
}

PortableTrayIcon::~PortableTrayIcon()
{
}

void PortableTrayIcon::Show()
{
#ifdef _WIN32
	m_tray.SetIcon(LoadIcon(GetModuleHandle(NULL), MAKEINTRESOURCE(APP_ICON_ID)));
	m_tray.Show();
#else
	m_tray_icon = egg_tray_icon_new ("Atol");
	m_tips = gtk_tooltips_new ();

	GdkPixbuf *pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&atol_xpm);
	GdkPixbuf *destpix = gdk_pixbuf_scale_simple(pixbuf, 16, 16, GDK_INTERP_NEAREST);
	GtkWidget *icon = gtk_image_new_from_pixbuf(destpix);
	GtkWidget *box = gtk_event_box_new ();
	g_object_unref (G_OBJECT (pixbuf));	//TOFIX???
	g_object_unref (G_OBJECT (destpix));
	 
	gtk_container_add (GTK_CONTAINER (box), icon);
	gtk_container_add (GTK_CONTAINER (m_tray_icon), box);
	gtk_widget_show_all (GTK_WIDGET (m_tray_icon));
	
	g_signal_connect (G_OBJECT (box), "button-press-event", G_CALLBACK (clicked), NULL);
#endif
} 

void PortableTrayIcon::SetTooltip(const char *szTip)
{
#ifdef _WIN32
	m_tray.SetTooltip(szTip);
#else
	//TOFIX set it to icon?
	gtk_tooltips_set_tip (m_tips, (GtkWidget *)m_tray_icon, szTip, NULL);
#endif
}

void PortableTrayIcon::Hide()
{
#ifdef _WIN32
	m_tray.Hide();
#else
	//gtk_widget_destroy (icon);
	gtk_widget_destroy ( (GtkWidget *)m_tray_icon );
#endif
}

static void clicked (GtkWidget *widget, GdkEventButton *event, gpointer data)
{
	switch(event->button) {
    case 1:	//left click
		OnTrayDefaultAction();
        break;
    case 2: //middle click
        break;
    case 3:	//right click
		ShowTrayMenu();
	    break;
    }
}

void ShowTrayMenu()
{
	GtkWidget *menu;
	GtkWidget *menu_item;
	GtkWidget *icon;
	int button, event_time;
	
	menu = gtk_menu_new ();
	//g_signal_connect (menu, "deactivate", G_CALLBACK(gtk_widget_destroy), NULL);

	GtkAccelGroup *accel_group = gtk_accel_group_new();
	gtk_menu_set_accel_group (GTK_MENU (menu), accel_group);

	// ... add menu items with accelerators ... 
	menu_item = gtk_image_menu_item_new_with_mnemonic(_("Restore"));
	g_signal_connect(menu_item, "activate",	G_CALLBACK (on_tray_restore), NULL);
	gtk_menu_append(menu, menu_item);
	gtk_widget_show (menu_item);  // Show the widget 
	icon = gtk_image_new_from_stock (GTK_STOCK_REFRESH, GTK_ICON_SIZE_MENU);
    gtk_widget_show (icon);
    gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (menu_item), icon);

	menu_item = gtk_image_menu_item_new_with_mnemonic(_("Quit"));
	g_signal_connect(menu_item, "activate",	G_CALLBACK (on_tray_quit), NULL);
	gtk_menu_append(menu, menu_item);
	gtk_widget_show (menu_item);  // Show the widget 
	icon = gtk_image_new_from_stock (GTK_STOCK_QUIT, GTK_ICON_SIZE_MENU);
    gtk_widget_show (icon);
    gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (menu_item), icon);
	
	event_time = gtk_get_current_event_time ();
	button = 0;	//FIX: allow mouse button to trigger the submenu
	
	//gtk_widget_show (menu);
	//gtk_widget_grab_focus (menu);

	gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, button, event_time);
}

void OnTrayDefaultAction()
{
	on_tray_restore(NULL, NULL);
}

void on_tray_restore               (GtkMenuItem     *menuitem,
                                    gpointer         user_data)
{
	gtk_widget_show(atol_main);
	g_tray.Hide();
}

void on_tray_quit                  (GtkMenuItem     *menuitem,
                                    gpointer         user_data)
{
	gtk_widget_show(atol_main);
	g_tray.Hide();
	on_quit1_activate(menuitem, user_data);
}

