/*
 * Copyright (C) 2004 Luca Padovani <lpadovan@cs.unibo.it>
 * Copyright (C) 2005 Martin Sevior <msevior@physics.unimelb.edu.au>
 * Copyright (C) 2005 Jean Brefort <jean.brefort@normalesup.org>
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
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include "pp_AttrProp.h"
#include "gr_Painter.h"
#include "gr_Image.h"
#include "ie_imp_GOChart.h"
#include "fp_Run.h"
#include "fv_View.h"
#include "ev_EditMethod.h"
#include "ev_Menu_Actions.h"
#include "xap_UnixFrameImpl.h"
#include "xap_Frame.h"
#include "ut_locale.h"
#include "AbiGOChart.h"
#include "gr_UnixImage.h"
#include "gr_UnixCairoGraphics.h"
#include "xap_Menu_Layouts.h"
#include "ap_Menu_Id.h"

#include <gtk/gtkentry.h>
#include <goffice/graph/gog-data-allocator.h>
#include <goffice/graph/gog-series.h>
#include <goffice/graph/gog-guru.h>
#include <goffice/graph/gog-renderer.h>
#include <goffice/graph/gog-data-set.h>
#include <goffice/graph/gog-object-xml.h>
#include <goffice/data/go-data-simple.h>
#include <goffice/utils/go-locale.h>
#include <gsf/gsf-impl-utils.h>
#include <gsf/gsf-output-memory.h>
#include <gsf/gsf-libxml.h>

#define ABI_TYPE_CONTROL_GUI     (abi_control_gui_get_type ())
#define ABI_CONTROL_GUI(obj)     (G_TYPE_CHECK_INSTANCE_CAST ((obj), ABI_TYPE_CONTROL_GUI, AbiControlGUI))
#define ABI_IS_CONTROL_GUI(o)    (G_TYPE_CHECK_INSTANCE_TYPE ((o), ABI_TYPE_CONTROL_GUI))

class AbiGO_LocaleTransactor
{
 public:

  AbiGO_LocaleTransactor (int category, const char * locale);
  ~AbiGO_LocaleTransactor ();

 private:
  
  int mCategory;
  char * mOldLocale;
};

AbiGO_LocaleTransactor::AbiGO_LocaleTransactor (int category, const char * locale)
  : mCategory (category), mOldLocale (0)
{
	mOldLocale = g_strdup(setlocale(category, NULL));
	go_setlocale (category, locale);

	// TODO: win32 may need to free old_locale
}

AbiGO_LocaleTransactor::~AbiGO_LocaleTransactor ()
{
	go_setlocale (mCategory, mOldLocale);
	FREEP(mOldLocale);
}

struct _AbiControlGUI
{
	GObject base;
	char *object_id;
	PD_Document * pDoc;
	GOChartView * pView;
};

typedef struct _AbiControlGUI AbiControlGUI;
typedef GObjectClass AbiControlGUIClass;

static GType abi_control_gui_get_type ();

//
// GOCmdContext interface implementation for AbiControlGUI
//
// we should implement it at least for errors reporting
/*static void
abi_cmd_context_init (GOCmdContextClass *iface)
{
	iface->get_password	    = abi_get_password;
	iface->set_sensitive	    = abi_set_sensitive;
	iface->error.error	    = abi_error_error;
	iface->error.error_info	    = abi_error_error_info;
	iface->progress_set	    = abi_progress_set;
	iface->progress_message_set = abi_progress_message_set;
}*/
	
//
// GogDataAllocator interface implementation for AbiControlGUI
//

static void
abi_data_allocator_allocate (G_GNUC_UNUSED GogDataAllocator *dalloc, G_GNUC_UNUSED GogPlot *plot)
{
//	SheetControlGUI *scg = wbcg_cur_scg (WORKBOOK_CONTROL_GUI (dalloc));
//	sv_selection_to_plot (sc_view (SHEET_CONTROL (scg)), plot);
}

typedef struct {
	GtkEntry *entry;
	GogDataset *dataset;
	int dim_i;
	GogDataType data_type;
} GraphDimEditor;

static void
cb_graph_dim_editor_update (GtkEntry *gee,
			    GraphDimEditor *editor)
{

	/* Ignore changes while we are insensitive. useful for displaying
	 * values, without storing then as Data.  Also ignore updates if the
	 * dataset has been cleared via the weakref handler  */
	if (!GTK_WIDGET_SENSITIVE (gee) || editor->dataset == NULL)
		return;

	GOData *data = NULL;
	double val;
	char sep[2], col_sep[2], *end;
	sep[0] = go_locale_get_arg_sep ();
	col_sep[0] = go_locale_get_col_sep ();
	sep[1] = col_sep[1]= 0;
	char const* str = gtk_entry_get_text (gee);
	if (str == NULL)
		return;
	data = NULL;
	switch (editor->data_type) {
	case GOG_DATA_SCALAR:
		if (!*str)
			break;
		val = g_ascii_strtod (str, &end);
		if (*end == 0)
			data = go_data_scalar_val_new (val);
		else
			data = go_data_scalar_str_new (g_strdup (str), TRUE);
		break;
	case GOG_DATA_VECTOR:
	{
		// try to get values, but on error, get strings
		data = go_data_vector_val_new (NULL, 0, NULL);
		if (go_data_unserialize (data, str, NULL))
			break;
		g_object_unref (data);
		data = go_data_vector_str_new (NULL, 0, NULL);
		if (go_data_unserialize (data, str, NULL))
			break;
		g_object_unref (data);
		data = NULL;
	}
		break;
	case GOG_DATA_MATRIX:
		data = go_data_matrix_val_new (NULL, 0, 0, NULL);
		if (go_data_unserialize (data, str, NULL))
			break;
		g_object_unref (data);
		data = NULL;
	}

	if (!data) {
		/* display "Invalid Data message" */
	} else
		gog_dataset_set_dim (editor->dataset, editor->dim_i, data, NULL);
}

static void
cb_graph_dim_entry_unmap (GtkEntry *gee, GraphDimEditor *editor)
{
	cb_graph_dim_editor_update (gee, /*FALSE, */editor);
}

static void
cb_graph_dim_entry_unrealize (GtkEntry *gee, GraphDimEditor *editor)
{
	cb_graph_dim_editor_update (gee, /*FALSE, */editor);
}

static void
cb_dim_editor_weakref_notify (GraphDimEditor *editor, GogDataset *dataset)
{
	g_return_if_fail (editor->dataset == dataset);
	editor->dataset = NULL;
}

static void
graph_dim_editor_free (GraphDimEditor *editor)
{
	if (editor->dataset)
		g_object_weak_unref (G_OBJECT (editor->dataset),
			(GWeakNotify) cb_dim_editor_weakref_notify, editor);
	g_free (editor);
}

static gpointer
abi_data_allocator_editor (G_GNUC_UNUSED GogDataAllocator *dalloc,
			    GogDataset *dataset, int dim_i, GogDataType data_type)
{
//	AbiControlGUI *acg = ABI_CONTROL_GUI (dalloc);
	GraphDimEditor *editor;
	GOData *val;

	editor = g_new (GraphDimEditor, 1);
	editor->dataset		= dataset;
	editor->dim_i		= dim_i;
	editor->data_type	= data_type;
	editor->entry  		= GTK_ENTRY (gtk_entry_new ());
	g_object_weak_ref (G_OBJECT (editor->dataset),
		(GWeakNotify) cb_dim_editor_weakref_notify, editor);

//	gnm_expr_entry_set_update_policy (editor->entry,
//		GTK_UPDATE_DISCONTINUOUS);

	val = gog_dataset_get_dim (dataset, dim_i);
	if (val != NULL) {
		char *txt = go_data_serialize (val, NULL);
		gtk_entry_set_text (editor->entry, txt);
		g_free (txt);
	}
//	gnm_expr_entry_set_flags (editor->entry,
//		GNM_EE_ABS_COL|GNM_EE_ABS_ROW, GNM_EE_MASK);

/*	g_signal_connect (G_OBJECT (editor->entry),
		"update",
		G_CALLBACK (cb_graph_dim_editor_update), editor);*/
	g_signal_connect (G_OBJECT (editor->entry),
		"activate",
		G_CALLBACK (cb_graph_dim_editor_update), editor);
	g_signal_connect (G_OBJECT (editor->entry),
		"unmap",
		G_CALLBACK (cb_graph_dim_entry_unmap), editor);
	g_signal_connect (G_OBJECT (editor->entry),
		"unrealize",
		G_CALLBACK (cb_graph_dim_entry_unrealize), editor);
	g_object_set_data_full (G_OBJECT (editor->entry),
		"editor", editor, (GDestroyNotify) graph_dim_editor_free);

	return editor->entry;
}

static void
abi_go_plot_data_allocator_init (GogDataAllocatorClass *iface)
{
	iface->allocate   = abi_data_allocator_allocate;
	iface->editor	  = abi_data_allocator_editor;
}

static void
abi_control_gui_init (GObject *object)
{
	AbiControlGUI *control = ABI_CONTROL_GUI (object);
	control->object_id = NULL;
}

static GObjectClass *parent_klass;

static void
abi_control_gui_finalize (GObject *object)
{
	AbiControlGUI *control = ABI_CONTROL_GUI (object);
	if (control->object_id)
		g_free (control->object_id);
	(parent_klass->finalize) (object);
}

static void
abi_control_gui_class_init (GObjectClass *klass)
{
	parent_klass = static_cast<GObjectClass*>(g_type_class_peek_parent (klass));
	klass->finalize = abi_control_gui_finalize;
}

GSF_CLASS_FULL (AbiControlGUI, abi_control_gui,
		NULL, NULL, abi_control_gui_class_init, NULL,
		abi_control_gui_init, G_TYPE_OBJECT, 0,
		GSF_INTERFACE (abi_go_plot_data_allocator_init, GOG_TYPE_DATA_ALLOCATOR);
		/*GSF_INTERFACE (abi_cmd_context_init, GO_CMD_CONTEXT_TYPE)*/)

static void
graph_user_config_free_data (gpointer data,
					  GClosure *closure)
{
	g_object_unref (data);
	closure->data = NULL;
}

static void
cb_update_graph (GogGraph *graph, gpointer data)
{
	g_return_if_fail (GOG_IS_GRAPH (graph));
	AbiControlGUI *acg = ABI_CONTROL_GUI (data);
	AbiGO_LocaleTransactor tn(LC_NUMERIC, "C");
	AbiGO_LocaleTransactor tm(LC_MONETARY, "C");
	GsfOutput* output = gsf_output_memory_new ();
	GsfXMLOut* xml = gsf_xml_out_new (output);
	gog_object_write_xml_sax(GOG_OBJECT (graph), xml, NULL);
	UT_Byte const *bytes = gsf_output_memory_get_bytes (GSF_OUTPUT_MEMORY (output));
	UT_ByteBuf myByteBuf;
	myByteBuf.append(bytes, gsf_output_size (output));
	const char* mimetypeGOChart = "application/x-goffice-graph";
	const char * szProps="embed-type: GOChart";
	if (acg->pView)
	{
		acg->pView->SetGuru (NULL);
		FV_View* pView = acg->pView->getRun ()->getBlock ()->getView ();
		UT_DEBUGMSG(("Doing Embed Update from GOG callback \n"));
		pView->cmdUpdateEmbed(acg->pView->getRun (), &myByteBuf,mimetypeGOChart,szProps);
	}
	else
	{
		XAP_Frame *pFrame = XAP_App::getApp()->getLastFocussedFrame();
		FV_View* pView = static_cast<FV_View*>(pFrame->getCurrentView());
		pView->cmdInsertEmbed(&myByteBuf,pView->getPoint(),mimetypeGOChart,szProps);
	}
	g_object_unref (xml);
	g_object_unref (output);
}

static void
guru_destroyed_cb (GOChartView *pView)
{
	if (pView)
		pView->SetGuru (NULL);
}

//
// AbiGOChart_Create
// -------------------
//   This is the function that we actually call to create the Chart.
//

bool 
AbiGOChart_Create(G_GNUC_UNUSED AV_View* v, G_GNUC_UNUSED EV_EditMethodCallData *d)
{
    XAP_Frame *pFrame = XAP_App::getApp()->getLastFocussedFrame();
	XAP_UnixFrameImpl *pFrameImpl = static_cast<XAP_UnixFrameImpl*>(pFrame->getFrameImpl());
	UT_ByteBuf myByteBuf;

	AbiControlGUI *acg = ABI_CONTROL_GUI(g_object_new (ABI_TYPE_CONTROL_GUI, NULL));

	GogGraph *graph = (GogGraph *) g_object_new(GOG_TYPE_GRAPH, NULL);
	/* by default, create one chart and add it to the graph */
	gog_object_add_by_name(GOG_OBJECT(graph), "Chart", NULL);
	GClosure *closure = g_cclosure_new(G_CALLBACK (cb_update_graph), acg,
					(GClosureNotify) graph_user_config_free_data);
	GtkWidget *dialog = gog_guru(graph, GOG_DATA_ALLOCATOR (acg),
		       NULL /*GO_CMD_CONTEXT (wbcg)*/, closure);
	gtk_window_set_transient_for(GTK_WINDOW(dialog), GTK_WINDOW(pFrameImpl->getTopLevelWindow()));
	g_closure_sink(closure);
	gtk_widget_show_all (dialog);

	g_object_unref (G_OBJECT(graph));

	return true;
}

GR_AbiGOChartItems::GR_AbiGOChartItems(void):
  m_iAPI(0),
  m_bHasSnapshot(false)
{
}

GR_AbiGOChartItems::~GR_AbiGOChartItems(void)
{
}


GR_GOChartManager::GR_GOChartManager(GR_Graphics* pG)
  : GR_EmbedManager(pG), 
    m_CurrentUID(-1),
    m_pDoc(NULL)
{
  m_vecGOChartView.clear();
  m_vecItems.clear();
}

GR_GOChartManager::~GR_GOChartManager()
{ 
     UT_VECTOR_PURGEALL(GR_AbiGOChartItems *,m_vecItems);
     UT_VECTOR_SPARSEPURGEALL(GOChartView *,m_vecGOChartView);
}

GR_EmbedManager * GR_GOChartManager::create(GR_Graphics * pG)
{
  return static_cast<GR_EmbedManager *>(new GR_GOChartManager(pG));
}

const char * GR_GOChartManager::getObjectType(void) const
{
  return "GOChart";
}

GOChartView * GR_GOChartManager::last_created_view = NULL;

void GR_GOChartManager::initialize(void)
{
  // Load the GOChart library
}

/*!
 * Create a specific GOChart view and associate it with a UID.
 */ 
UT_sint32  GR_GOChartManager::_makeGOChartView(void)
{
     last_created_view = new GOChartView(this);
     m_vecGOChartView.addItem(last_created_view);
     return static_cast<UT_sint32>(m_vecGOChartView.getItemCount()-1);
}

void GR_GOChartManager::_loadGOChartXML(UT_sint32 uid, UT_UTF8String & sGOChartBuf)
{
  GOChartView * pGOChartView = m_vecGOChartView.getNthItem(uid);
  UT_return_if_fail(pGOChartView);
  pGOChartView->loadBuffer(sGOChartBuf);
}

void GR_GOChartManager::setDefaultFontSize(UT_sint32 uid, UT_sint32 iSize)
{
  GOChartView * pGOChartView = m_vecGOChartView.getNthItem(uid);
  UT_return_if_fail(pGOChartView);
  pGOChartView->setDefaultFontSize(iSize);
}

UT_sint32 GR_GOChartManager::makeEmbedView(AD_Document * pDoc, UT_uint32 api, G_GNUC_UNUSED const char * szDataID)
{
  if(m_pDoc == NULL)
  {
    m_pDoc = static_cast<PD_Document *>(pDoc);
  }
  else
  {
    UT_ASSERT(m_pDoc == static_cast<PD_Document *>(pDoc));
  }
  UT_sint32 iNew = _makeGOChartView();
  GR_AbiGOChartItems * pItem = new GR_AbiGOChartItems();
  pItem->m_iAPI = api;
  pItem->m_bHasSnapshot = false;
  m_vecItems.addItem(pItem);
  UT_ASSERT(static_cast<UT_sint32>(m_vecItems.getItemCount()) == (iNew+1));
  return iNew;
}

void GR_GOChartManager::makeSnapShot(UT_sint32 uid, UT_Rect & rec)
{
  if(!getGraphics()->queryProperties(GR_Graphics::DGP_SCREEN))
    {
      return;
    }
  GR_AbiGOChartItems * pItem = m_vecItems.getNthItem(uid);
  UT_return_if_fail(pItem);  
  const PP_AttrProp * pSpanAP = NULL;
  PT_AttrPropIndex api = pItem->m_iAPI;
  bool bHaveProp = m_pDoc->getAttrProp(api, &pSpanAP);
  UT_return_if_fail(bHaveProp);
  const char * pszDataID = NULL;
  pSpanAP->getAttribute("dataid", pszDataID);
  if(pItem->m_bHasSnapshot)
    {
       updatePNGSnapshot(static_cast<AD_Document *>(m_pDoc),rec,pszDataID);
    }
  else
    {
       createPNGSnapshot(static_cast<AD_Document *>(m_pDoc),rec,pszDataID);
       pItem->m_bHasSnapshot = true;
    }
}

bool GR_GOChartManager::isDefault(void)
{
  return false;
}


bool GR_GOChartManager::createPNGSnapshot(AD_Document * pDoc, UT_Rect & rec,
					   const char * szDataID)
{
  if(isDefault())
  {
    return false;
  }
  if((rec.width == 0) || (rec.height ==0))
  {
    return false;
  }
  // TODO: use the goffice framework to get a high resolution png.
  GR_Image * pImage = static_cast<GR_UnixCairoGraphics*>(getGraphics())->genImageFromRectangle(rec);
  if(pImage == NULL)
  {
    return false;
  }
  UT_ByteBuf * pBuf = NULL;
  pImage->convertToBuffer(&pBuf);
  UT_UTF8String sID = "snapshot-png-";
  sID += szDataID;
  const char* mimetypePNG = NULL;
  mimetypePNG = g_strdup("image/png");
  pDoc->createDataItem(sID.utf8_str(),false,reinterpret_cast< const UT_ByteBuf *>(pBuf),mimetypePNG,NULL);
  delete pBuf;
  delete pImage;
  return true;
}


bool GR_GOChartManager::updatePNGSnapshot(AD_Document * pDoc, UT_Rect & rec,
					   const char * szDataID)
{
  if(isDefault())
  {
    return false;
  }
  if((rec.width == 0) || (rec.height ==0))
  {
    return false;
  }
  // TODO: use the goffice framework to get a high resolution png.
  GR_Image * pImage = static_cast<GR_UnixCairoGraphics*>(getGraphics())->genImageFromRectangle(rec);
  if(pImage == NULL)
  {
    return false;
  }
  UT_ByteBuf * pBuf = NULL;
  pImage->convertToBuffer(&pBuf);
  UT_UTF8String sID = "snapshot-png-";
  sID += szDataID;
   pDoc->replaceDataItem(sID.utf8_str(),reinterpret_cast< const UT_ByteBuf *>(pBuf));
  delete pBuf;
  delete pImage;
  return true;
}

bool GR_GOChartManager::modify(UT_sint32 uid)
{
  GOChartView * pGOChartView = m_vecGOChartView.getNthItem(uid);
  pGOChartView->modify();
  return false;
}

bool  GR_GOChartManager::isEdittable(G_GNUC_UNUSED UT_sint32 uid)
{
  return true;
}

void GR_GOChartManager::initializeEmbedView(G_GNUC_UNUSED UT_sint32 uid)
{
  // FIXME write code for this.
}

void GR_GOChartManager::loadEmbedData(UT_sint32 uid)
{
  GOChartView * pGOChartView = m_vecGOChartView.getNthItem(uid);
  UT_return_if_fail(pGOChartView);
  const PP_AttrProp * pSpanAP = NULL;
  GR_AbiGOChartItems * pItem = m_vecItems.getNthItem(uid);
  UT_return_if_fail(pItem);  
  PT_AttrPropIndex api = pItem->m_iAPI;
  bool bHaveProp = m_pDoc->getAttrProp(api, &pSpanAP);
  UT_return_if_fail(bHaveProp);
  const char * pszDataID = NULL;
  bool bFoundDataID = pSpanAP->getAttribute("dataid", pszDataID);
  UT_UTF8String sGOChartXML;
  if (bFoundDataID && pszDataID)
  {
       const UT_ByteBuf * pByteBuf = NULL;
       bFoundDataID = m_pDoc->getDataItemDataByName(pszDataID, 
						    const_cast<const UT_ByteBuf **>(&pByteBuf),
						    NULL, NULL);
       if (bFoundDataID)
       {
            UT_UCS4_mbtowc myWC;
            sGOChartXML.appendBuf( *pByteBuf, myWC);
       }
  }
 UT_return_if_fail(bFoundDataID);
 UT_return_if_fail(pszDataID);
  UT_DEBUGMSG(("GO Chart string is... \n %s \n",sGOChartXML.utf8_str()));
  _loadGOChartXML(uid, sGOChartXML);
}

UT_sint32 GR_GOChartManager::getWidth(G_GNUC_UNUSED UT_sint32 uid)
{
    // FIXME write code this draws a square
    return 5000;
}


UT_sint32 GR_GOChartManager::getAscent(G_GNUC_UNUSED UT_sint32 uid)
{
  // FIXME write code this draws a square
  return 5000;
}


UT_sint32 GR_GOChartManager::getDescent(G_GNUC_UNUSED UT_sint32 uid)
{
  // FIXME write code
 return 0;

}

void GR_GOChartManager::setColor(G_GNUC_UNUSED UT_sint32 uid, G_GNUC_UNUSED UT_RGBColor c)
{
  // FIXME write code
}

/*!
 * This method renders the GOChart identified by uid onto the screen
 * rectangle given by rec
 */
void GR_GOChartManager::render(UT_sint32 uid, UT_Rect & rec)
{
  // FIXME write code
  GOChartView * pGOChartView = m_vecGOChartView.getNthItem(uid);
  UT_return_if_fail(pGOChartView);
  pGOChartView->render(rec);
}

void GR_GOChartManager::releaseEmbedView(UT_sint32 uid)
{
  GOChartView * pGOChartView = m_vecGOChartView.getNthItem(uid);
  delete pGOChartView;
  m_vecGOChartView.setNthItem(uid,NULL,NULL); //NULL it out so we don't affect the other uid's
}

bool GR_GOChartManager::convert(G_GNUC_UNUSED UT_uint32 iConType, G_GNUC_UNUSED UT_ByteBuf & From, G_GNUC_UNUSED UT_ByteBuf & To)
{
  return false;
}

void GR_GOChartManager::setRun(UT_sint32 uid, fp_Run *pRun)
{
	GOChartView * pGOChartView = m_vecGOChartView.getNthItem(uid);
	pGOChartView->SetRun (pRun);
}

void GR_GOChartManager::updateData(UT_sint32 uid, UT_sint32 api)
{
	GR_AbiGOChartItems * pItem = m_vecItems.getNthItem(uid);
	UT_return_if_fail(pItem);  
	pItem->m_iAPI = api;
}

EV_EditMouseContext GR_GOChartManager::ChartMenuID = EV_EMC_EMBED;

void GR_GOChartManager::buildContextualMenu ()
{
    XAP_App *pApp = XAP_App::getApp();
    EV_Menu_ActionSet* pActionSet = pApp->getMenuActionSet();
	XAP_Menu_Factory * pFact = pApp->getMenuFactory();
	ChartMenuID = pFact->createContextMenu("AbiGOChart");
	pFact->addNewMenuBefore("AbiGOChart",NULL,0,EV_MLF_Normal,AP_MENU_ID_EDIT_DELETEEMBED);
	pFact->addNewMenuBefore("AbiGOChart",NULL,0,EV_MLF_Normal,AP_MENU_ID_EDIT_COPYEMBED);
	pFact->addNewMenuBefore("AbiGOChart",NULL,0,EV_MLF_Normal,AP_MENU_ID_EDIT_CUTEMBED);
    pFact->addNewMenuBefore("AbiGOChart",NULL,0,EV_MLF_Normal,AP_MENU_ID_FMT_EMBED);
	XAP_Menu_Id separatorID = pFact->addNewMenuAfter("AbiGOChart",NULL,AP_MENU_ID_FMT_EMBED,EV_MLF_Separator);
	pFact->addNewLabel(NULL,separatorID,NULL,NULL);
	// Create the Action that will be called.
	EV_Menu_Action* mySeparatorAction = new EV_Menu_Action(
								   separatorID,          // id that the layout said we could use
		0,                      // no, we don't have a sub menu.
		0,                      // no, we raise a dialog.
		0,                      // no, we don't have a checkbox.
		0,                      // no radio buttons for me, thank you
		NULL,                   // name of callback function to call.
		NULL,                   // don't know/care what this is for
		NULL                    // don't know/care what this is for
		);
	
	  pActionSet->addAction(mySeparatorAction);
}

void GR_GOChartManager::removeContextualMenu ()
{
    XAP_App *pApp = XAP_App::getApp();
	XAP_Menu_Factory * pFact = pApp->getMenuFactory();
	pFact->removeContextMenu (ChartMenuID);
}

GOChartView::GOChartView(GR_GOChartManager * pGOMan): m_pGOMan(pGOMan)
{
	m_Graph = NULL;
	m_Image = NULL;
	m_Renderer = GOG_RENDERER(g_object_new(GOG_TYPE_RENDERER, NULL));
	pix_width = pix_height = 0;
	width = height = 0;
	m_Guru = NULL;
}

GOChartView::~GOChartView(void)
{
	g_object_unref (m_Renderer);
	if (m_Graph)
		g_object_unref (m_Graph);
	if (m_Image)
		delete m_Image;
	if (m_Guru)
		gtk_widget_destroy (m_Guru);
}

void GOChartView::render(UT_Rect & rec)
{
	UT_return_if_fail (m_Graph);
	if((rec.width == 0) || (rec.height ==0))
	{
		return;
	}
	GR_CairoGraphics *pUGG = static_cast<GR_CairoGraphics*>(m_pGOMan->getGraphics());
	cairo_t *cr = pUGG->getCairo ();
	UT_sint32 _width = pUGG->tdu(rec.width);
	UT_sint32 _height = pUGG->tdu(rec.height);
	UT_sint32 x = pUGG->tdu(rec.left);
	UT_sint32 y = pUGG->tdu(rec.top)-pUGG->tdu(rec.height);
	UT_sint32 zoom = pUGG->getZoomPercentage ();
	UT_sint32 real_width = _width * 100 / zoom;
	UT_sint32 real_height = _height * 100 / zoom;
	if (real_width != width || real_height != height)
	{
		width = real_width;
		height = real_height;
		gog_graph_set_size (m_Graph, width, height);
	}
	cairo_save (cr);
	cairo_translate (cr, x, y);
	gog_renderer_render_to_cairo (m_Renderer, cr, _width, _height);
	cairo_new_path (cr); // just in case a path has not been ended
	cairo_restore (cr);
}

void GOChartView::loadBuffer(UT_UTF8String & sGOChartXML)
{
	if (m_Graph)
		g_object_unref (m_Graph);
	AbiGO_LocaleTransactor tn(LC_NUMERIC, "C");
	AbiGO_LocaleTransactor tm(LC_MONETARY, "C");
	xmlDocPtr xml = xmlParseMemory((const char*)sGOChartXML.utf8_str(), sGOChartXML.byteLength());
	m_Graph = GOG_GRAPH (gog_object_new_from_xml (NULL, xml->children, NULL));
	xmlFreeDoc(xml);
	if (m_Graph)
		g_object_set (G_OBJECT (m_Renderer), "model", m_Graph, NULL);
	pix_width = pix_height = 0; // force pixbuf update
}


void GOChartView::setDefaultFontSize(G_GNUC_UNUSED UT_sint32 iSize)
{

}

void GOChartView::modify()
{
	UT_return_if_fail (m_Graph);
    XAP_Frame *pFrame = XAP_App::getApp()->getLastFocussedFrame();
	XAP_UnixFrameImpl *pFrameImpl = static_cast<XAP_UnixFrameImpl*>(pFrame->getFrameImpl());
	AbiControlGUI *acg = ABI_CONTROL_GUI (g_object_new (ABI_TYPE_CONTROL_GUI, NULL));

	acg->pDoc = static_cast<PD_Document *>(pFrame->getCurrentDoc());
	acg->pView = this;

	GClosure *closure = g_cclosure_new (G_CALLBACK (cb_update_graph), acg,
					(GClosureNotify) graph_user_config_free_data);
	GtkWidget *dialog = gog_guru (m_Graph, GOG_DATA_ALLOCATOR (acg),
		       NULL /*GO_CMD_CONTEXT (wbcg)*/, closure);
	gtk_window_set_transient_for(GTK_WINDOW(dialog), GTK_WINDOW(pFrameImpl->getTopLevelWindow()));
	gtk_widget_show_all (dialog);
	g_closure_sink (closure);

	acg->pView->SetGuru (dialog);
	g_signal_connect_swapped (G_OBJECT (dialog), "destroy", G_CALLBACK (guru_destroyed_cb), acg->pView);
}
