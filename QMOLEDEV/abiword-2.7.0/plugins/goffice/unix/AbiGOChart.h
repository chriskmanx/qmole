/* AbiWord
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

#ifndef __gr_AbiGOChartManager_h__
#define __gr_AbiGOChartManager_h__
#include "ut_string_class.h"
#include "gr_EmbedManager.h"
#include "ut_types.h"
#include "ut_vector.h"
#include <goffice/goffice.h>
#include <goffice/graph/gog-graph.h>
#include <goffice/graph/gog-renderer.h>


class GR_Graphics;
class PD_Document;
class AD_Document;
class GOChartView;
class GR_AbiGOChartItems;

class GR_AbiGOChartItems
{
 public:
  GR_AbiGOChartItems();
  virtual ~GR_AbiGOChartItems();
  UT_uint32 m_iAPI;
  bool m_bHasSnapshot;
};

class GR_GOChartManager : public GR_EmbedManager 
{
public:
    GR_GOChartManager(GR_Graphics * pG);
    virtual ~GR_GOChartManager();
    virtual GR_EmbedManager *  create(GR_Graphics * pG);
    virtual const char *   getObjectType(void) const;
    virtual void           initialize(void);
    virtual UT_sint32      makeEmbedView(AD_Document * pDoc, UT_uint32  api, const char * szDataID) ;
    virtual void           setColor(UT_sint32 uid, UT_RGBColor c);
    virtual UT_sint32      getWidth(UT_sint32 uid);
    virtual UT_sint32      getAscent(UT_sint32 uid) ;
    virtual UT_sint32      getDescent(UT_sint32 uid) ;
    virtual void           loadEmbedData(UT_sint32 uid);
    virtual void           setDefaultFontSize(UT_sint32 uid, UT_sint32 iSize);
    virtual void           render(UT_sint32 uid, UT_Rect & rec);
    virtual void           releaseEmbedView(UT_sint32 uid);
    virtual void           initializeEmbedView(UT_sint32 uid);
    virtual void           makeSnapShot(UT_sint32 uid, UT_Rect & rec);
    virtual bool           isDefault(void);
    virtual bool           modify(UT_sint32 uid);
    virtual bool           convert(UT_uint32 iConv, UT_ByteBuf & From, UT_ByteBuf & To);
	virtual void		   updateData(UT_sint32 uid, UT_sint32 api);

    bool           createPNGSnapshot(AD_Document * pDoc, UT_Rect & rec,
					   const char * szDataID);
    bool           updatePNGSnapshot(AD_Document * pDoc, UT_Rect & rec,
					   const char * szDataID);
    virtual bool           isEdittable(UT_sint32 uid);
	virtual void		   setRun (UT_sint32 uid, fp_Run * run);
	EV_EditMouseContext		getContextualMenu(void) const
		{ return ChartMenuID; }
	static void 			buildContextualMenu ();
	static void 			removeContextualMenu ();

	static GOChartView * last_created_view;

private:
    UT_sint32                              _makeGOChartView(void);
    void                                    _loadGOChartXML(UT_sint32 uid, UT_UTF8String & sGOChartBuf);
    UT_sint32                              _getNextUID(void);
    UT_sint32                              m_CurrentUID;
    UT_GenericVector<GOChartView *>        m_vecGOChartView;
    UT_GenericVector<GR_AbiGOChartItems *>    m_vecItems;
    PD_Document *                          m_pDoc;
	static EV_EditMouseContext ChartMenuID;
};

// FIXME wrapper for libGNOMEOFFICEChart object
class GOChartView
{
public:
	GOChartView(GR_GOChartManager  * pGOMan);
	virtual ~GOChartView(void);
	void render(UT_Rect & rec);
	void loadBuffer(UT_UTF8String & sGOChartXM);
	void setDefaultFontSize(UT_sint32 iSize);
	void modify(void);
	void SetRun (fp_Run *pRun) {m_pRun = pRun;}
	fp_Run *getRun () {return m_pRun;}
	void SetGuru (GtkWidget *guru) {m_Guru = guru;}
private:
	GR_GOChartManager  * m_pGOMan;
	GogGraph *m_Graph;
	GogRenderer *m_Renderer;
	GR_Image *m_Image;
	UT_sint32 width, height;
	UT_sint32 pix_width;
	UT_sint32 pix_height;
	fp_Run *m_pRun;
	GtkWidget *m_Guru;
};

bool AbiGOChart_Create(AV_View* v, EV_EditMethodCallData *d);

#endif // __gr_AbiGOChartManager_h__
