/*
 * 
 * Copyright (C) 2001, 2002 by Dom Lachowicz
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

#include "xap_Module.h"
#include "xap_App.h"
#include "xap_Frame.h"
#include "fv_View.h"
#include "ap_Menu_Id.h"
#include "ev_Menu_Actions.h"
#include "ev_Menu.h"
#include "ev_Menu_Layouts.h"
#include "ev_Menu_Labels.h"
#include "ev_EditMethod.h"
#include "xap_Menu_Layouts.h"

#include "xap_UnixFrameImpl.h"
#include "xap_UnixDialogHelper.h"

#ifdef USE_FORK_AND_EXEC_METHOD

#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#ifdef ABI_PLUGIN_BUILTIN
#define abi_plugin_register abipgn_gdict_register
#define abi_plugin_unregister abipgn_gdict_unregister
#define abi_plugin_supports_version abipgn_gdict_supports_version
// dll exports break static linking
#define ABI_BUILTIN_FAR_CALL extern "C"
#else
#define ABI_BUILTIN_FAR_CALL ABI_FAR_CALL
ABI_PLUGIN_DECLARE("Gdict")
#endif

static void
GDict_exec (const char * search)
{
  pid_t pid;

  if ( (pid = fork ()) == 0 )
    {
      // child
      const char *args[3];
      args[0] = "--noapplet"; // definitely needed for kde users, for example
      args[1] = search;
      args[2] = 0;
      
      exit ( execvp ( "gnome-dictionary", (char **) args ) );
      // TODO: be smarter with warnings and such
    }
  else if ( pid > 0 )
    {
      // parent
      waitpid ( pid, 0, WNOHANG );
    }
  else
    {
      // couln't spawn
    }
}

#else

#include <gnome.h>
#include <libgdict/gdict-defbox.h>

// i hate global state, but it's so much easier here...
static GtkWidget * gdict_dlg    = 0;
static GtkWidget * gdict_entry  = 0;
static GtkWidget * gdict_defbox = 0;

static void
lookup_button_cb (GtkButton *button, GtkWidget * defbox)
{
  gchar *text;
  GtkWidget * entry = gnome_entry_gtk_entry(GNOME_ENTRY(gdict_entry));

  text = gtk_editable_get_chars (GTK_EDITABLE (entry), 0, -1);
  
  if (text)
    {
      gdict_defbox_lookup (GDICT_DEFBOX (defbox), text);      
      g_free (text);
    }  
}

static void
entry_activate_cb (GtkEditable *editable, GtkWidget* defbox)
{
  gchar *text = 0;
  
  text = gtk_editable_get_chars (editable, 0, -1);
  
  if (text)
    {
      gdict_defbox_lookup (GDICT_DEFBOX (defbox), text);      
      g_free (text); 	
    }
}

static void
close_cb (GtkWidget * w, gpointer data)
{
  gtk_widget_destroy(gdict_defbox);
  gdict_defbox = 0;

  gtk_widget_destroy(gdict_entry);
  gdict_entry  = 0;

  gtk_widget_destroy(gdict_dlg);
  gdict_dlg    = 0;
}

static void
GDict_dlg_create (const char * search)
{
  GtkWidget * vbox;
  GtkWidget * hbox;
  GtkWidget * button;
  GtkWidget * scrolled;
  GtkWidget * close;
  GtkWidget * gtk_entry;
  
  // create the toplevel dialog
  gdict_dlg = gnome_dialog_new ("AbiWord Dictionary", 
				GNOME_STOCK_BUTTON_CLOSE, NULL);
  gtk_window_set_modal (GTK_WINDOW(gdict_dlg), false);
  gtk_widget_set_usize (gdict_dlg, 450, 300);

  // center the dialog and connect focus
  XAP_App * pApp = XAP_App::getApp ();
  XAP_UnixFrame * pFrame = static_cast<XAP_UnixFrame*>(pApp->getLastFocussedFrame ());
  GtkWidget * parent = pFrame->getTopLevelWindow();
  centerDialog(parent, gdict_dlg);

  close = GTK_WIDGET (g_list_last (GNOME_DIALOG (gdict_dlg)->buttons)->data);  
  vbox = GNOME_DIALOG(gdict_dlg)->vbox;
  
  hbox = gtk_hbox_new (FALSE, GNOME_PAD_SMALL);	
  gtk_container_set_border_width (GTK_CONTAINER (hbox), GNOME_PAD_SMALL);
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);
  
  button = gtk_button_new_with_label ("Look Up");
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, FALSE, 0);
  
  gdict_entry = gnome_entry_new(NULL);
  gtk_entry = gnome_entry_gtk_entry(GNOME_ENTRY(gdict_entry));
  gtk_box_pack_start (GTK_BOX (hbox), gdict_entry, TRUE, TRUE, 0);

  scrolled = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_set_border_width (GTK_CONTAINER (scrolled), GNOME_PAD_SMALL); 
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled),
				  GTK_POLICY_AUTOMATIC,
				  GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start (GTK_BOX (vbox), scrolled, TRUE, TRUE, 0);
  
  gdict_defbox = gdict_defbox_new ();
  gtk_widget_show (gdict_defbox);
  gtk_container_add (GTK_CONTAINER (scrolled), gdict_defbox);
  
  g_signal_connect (G_OBJECT (button), "clicked",
		      G_CALLBACK (lookup_button_cb), gdict_defbox);

  g_signal_connect (G_OBJECT (gtk_entry), "activate",
		      G_CALLBACK (entry_activate_cb), gdict_defbox);
#if 0
  g_signal_connect (G_OBJECT (gtk_entry), "changed",
		      G_CALLBACK (entry_activate_cb), gdict_defbox);
#endif  

  g_signal_connect (G_OBJECT (gdict_dlg),
		      "close",
		      G_CALLBACK(close_cb),
		      (gpointer)0);
  
  g_signal_connect (G_OBJECT (close),
		      "clicked",
		      G_CALLBACK(close_cb),
		      (gpointer)0);

  g_signal_connect_after(G_OBJECT(gdict_dlg),
			   "destroy",
			   NULL,
			   NULL);

  gtk_widget_show_all (gdict_dlg);
}

static void 
GDict_exec (const char * search)
{
  if (!gdict_dlg)
    {
      // create the dialog
      GDict_dlg_create (search);
    }
  else
    {
      // raise to the front
      gdk_window_raise (gdict_dlg->window);
    }

  GtkWidget * entry = gnome_entry_gtk_entry (GNOME_ENTRY(gdict_entry));
  gtk_entry_set_text (GTK_ENTRY(entry), search);
  gnome_entry_prepend_history(GNOME_ENTRY(gdict_entry), false, search);
  gdict_defbox_lookup (GDICT_DEFBOX (gdict_defbox), (char*)search);
}

#endif /* USE_FORK_AND_EXEC_METHOD */

//
// GDict_invoke
// -------------------
//   This is the function that we actually call to invoke the thesaurus.
//   It should be called when the user hits the thesaurus key (shift+f7?)
//   or chooses "thesaurus" from a menu.
//
static bool 
GDict_invoke(AV_View* /*v*/, EV_EditMethodCallData */*d*/)
{
  // Get the current view that the user is in.
  XAP_Frame *pFrame = XAP_App::getApp()->getLastFocussedFrame();
  FV_View* pView = static_cast<FV_View*>(pFrame->getCurrentView());
  
  // If the user is on a word, but does not have it selected, we need
  // to go ahead and select that word so that the search/replace goes
  // correctly.
  pView->moveInsPtTo(FV_DOCPOS_EOW_MOVE);
  pView->moveInsPtTo(FV_DOCPOS_BOW);
  pView->extSelTo(FV_DOCPOS_EOW_SELECT);   
  
  // We need to get the utf-8 version of the current word.
  UT_UCS4Char *ucs4ST = NULL;
  pView->getSelectionText(*&ucs4ST);
  if (ucs4ST) {
    UT_UTF8String search(ucs4ST);
    GDict_exec (search.utf8_str());
    FREEP(ucs4ST);
  }
    
  return true;
}

static const char* GDict_MenuLabel = "G&Dict Dictionary";
static const char* GDict_MenuTooltip = "Opens the dictionary";

static void
GDict_removeFromMenus()
{
  // First we need to get a pointer to the application itself.
  XAP_App *pApp = XAP_App::getApp();

  // remove the edit method
  EV_EditMethodContainer* pEMC = pApp->getEditMethodContainer() ;
  EV_EditMethod * pEM = ev_EditMethod_lookup ( "GDict_invoke" ) ;
  pEMC->removeEditMethod ( pEM ) ;
  DELETEP( pEM ) ;

  // now remove crap from the menus
  int frameCount = pApp->getFrameCount();
  XAP_Menu_Factory * pFact = pApp->getMenuFactory();

  pFact->removeMenuItem("Main",NULL,GDict_MenuLabel);
  pFact->removeMenuItem("contextText",NULL,GDict_MenuLabel);
  for(int i = 0;i < frameCount;++i)
    {
      // Get the current frame that we're iterating through.
      XAP_Frame* pFrame = pApp->getFrame(i);
      pFrame->rebuildMenus();
    }
}

static void
GDict_addToMenus()
{
  // First we need to get a pointer to the application itself.
  XAP_App *pApp = XAP_App::getApp();
    
  // Create an EditMethod that will link our method's name with
  // it's callback function.  This is used to link the name to 
  // the callback.
  EV_EditMethod *myEditMethod = new EV_EditMethod(
						  "GDict_invoke",  // name of callback function
						  GDict_invoke,    // callback function itself.
						  0,                      // no additional data required.
						  ""                      // description -- allegedly never used for anything
						  );
  
  // Now we need to get the EditMethod container for the application.
  // This holds a series of Edit Methods and links names to callbacks.
  EV_EditMethodContainer* pEMC = pApp->getEditMethodContainer();
  
  // We have to add our EditMethod to the application's EditMethodList
  // so that the application will know what callback to call when a call
  // to "AiksaurusABI_invoke" is received.
  pEMC->addEditMethod(myEditMethod);
  
  
  // Now we need to grab an ActionSet.  This is going to be used later
  // on in our for loop.  Take a look near the bottom.
  EV_Menu_ActionSet* pActionSet = pApp->getMenuActionSet();
  
  
  // We need to go through and add the menu element to each "frame" 
  // of the application.  We can iterate through the frames by doing
  // XAP_App::getFrameCount() to tell us how many frames there are,
  // then calling XAP_App::getFrame(i) to get the i-th frame.
  
  int frameCount = pApp->getFrameCount();
  XAP_Menu_Factory * pFact = pApp->getMenuFactory();

  //
  // Put it in the context menu.
  //
  XAP_Menu_Id newID = pFact->addNewMenuAfter("contextText",NULL,"Bullets and &Numbering",EV_MLF_Normal);
  pFact->addNewLabel(NULL,newID,GDict_MenuLabel, GDict_MenuTooltip);

  //
  // Also put it under word Wount in the main menu,
  //
  pFact->addNewMenuAfter("Main",NULL,"&Word Count",EV_MLF_Normal,newID);
  
  // Create the Action that will be called.
  EV_Menu_Action* myAction = new EV_Menu_Action(
						newID,                     // id that the layout said we could use
						0,                      // no, we don't have a sub menu.
						1,                      // yes, we raise a dialog.
						0,                      // no, we don't have a checkbox.
						0,
						"GDict_invoke",  // name of callback function to call.
						NULL,                   // don't know/care what this is for
						NULL                    // don't know/care what this is for
						);
  
  // Now what we need to do is add this particular action to the ActionSet
  // of the application.  This forms the link between our new ID that we 
  // got for this particular frame with the EditMethod that knows how to 
  // call our callback function.  
  
  pActionSet->addAction(myAction);
  
  for(int i = 0;i < frameCount;++i)
    {
      // Get the current frame that we're iterating through.
      XAP_Frame* pFrame = pApp->getFrame(i);
      pFrame->rebuildMenus();
    }
}

// -----------------------------------------------------------------------
//
//      Abiword Plugin Interface 
//
// -----------------------------------------------------------------------

ABI_BUILTIN_FAR_CALL
int abi_plugin_register (XAP_ModuleInfo * mi)
{
    mi->name = "GDict plugin";
    mi->desc = "Dictionary support for AbiWord";
    mi->version = ABI_VERSION_STRING;
    mi->author = "Dom Lachowicz <cinamod@hotmail.com>";
    mi->usage = "No Usage";
    
    // Add the dictionary to AbiWord's menus.
    GDict_addToMenus();
    
    return 1;
}


ABI_BUILTIN_FAR_CALL
int abi_plugin_unregister (XAP_ModuleInfo * mi)
{
    mi->name = 0;
    mi->desc = 0;
    mi->version = 0;
    mi->author = 0;
    mi->usage = 0;

    GDict_removeFromMenus () ;

    return 1;
}


ABI_BUILTIN_FAR_CALL
int abi_plugin_supports_version (UT_uint32 /*major*/, UT_uint32 /*minor*/, UT_uint32 /*release*/)
{
    return 1; 
}
