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

#ifndef __gr_AbiGOComponentManager_h__
#define __gr_AbiGOComponentManager_h__
#include "gr_EmbedManager.h"
#include <goffice/component/goffice-component.h>
#include <goffice/component/go-component.h>


class GR_Graphics;
class PD_Document;
class AD_Document;
class GOComponentView;
class GR_AbiGOComponentItems;

class GR_AbiGOComponentItems
{
 public:
  GR_AbiGOComponentItems();
  virtual ~GR_AbiGOComponentItems();
  UT_uint32 m_iAPI;
  bool m_bHasSnapshot;
};

class GR_GOComponentManager : public GR_EmbedManager 
{
public:
    GR_GOComponentManager(GR_Graphics * pG, char const *mime_type);
    virtual ~GR_GOComponentManager();
    virtual GR_EmbedManager *  create(GR_Graphics * pG);
    virtual const char *   getObjectType(void) const;
    virtual const char *   getMimeType(void) const;
    virtual const char *   getMimeTypeDescription(void) const;
    virtual const char *   getMimeTypeSuffix(void) const;
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
    virtual bool           isEdittable(UT_sint32 uid);
    virtual bool           isResizeable(UT_sint32 uid);
	virtual void		   setRun (UT_sint32 uid, fp_Run * run);
	virtual void		   updateData(UT_sint32 uid, UT_sint32 api);

    bool           createPNGSnapshot(AD_Document * pDoc, UT_Rect & rec,
					   const char * szDataID);
    bool           updatePNGSnapshot(AD_Document * pDoc, UT_Rect & rec,
					   const char * szDataID);
private:
    UT_sint32                              _makeGOComponentView(void);
    UT_sint32                              _getNextUID(void);
    UT_sint32                              m_CurrentUID;
    UT_GenericVector<GOComponentView *>        m_vecGOComponentView;
    UT_GenericVector<GR_AbiGOComponentItems *>    m_vecItems;
    PD_Document *                          m_pDoc;
	char const *m_MimeType;
	UT_String m_ObjectType;
};

// FIXME wrapper for libGNOMEOFFICEComponent object
class GOComponentView
{
friend class GR_GOComponentManager;
public:
	GOComponentView(GR_GOComponentManager  * pGOMan);
	virtual ~GOComponentView(void);
	void render(UT_Rect & rec);
	void loadBuffer(UT_ByteBuf const *sGOComponentData, char *_mime_type);
	void setDefaultFontSize(UT_sint32 iSize);
	void modify(void);
	void update (void);
	GOComponent *getComponent (void) {return component;}
	bool IsEdittable () {return go_component_is_editable (component);}
	bool IsResizable () {return go_component_is_resizable (component);}
	UT_ByteBuf *exportToSVG ();
	void SetRun (fp_Run *pRun) {m_pRun = pRun;}

private:
	GR_GOComponentManager  * m_pGOMan;
	GOComponent *component;
	char *mime_type;
	GR_Image *m_Image;
	UT_sint32 width, ascent, descent;
	UT_sint32 pix_width, pix_height;
	GdkWindow *window;
	GdkPixbuf *pixbuf;
	GdkWindowAttr attributes;
	fp_Run *m_pRun;
};

bool AbiGOComponent_FileInsert(AV_View* v, EV_EditMethodCallData *d);
bool AbiGOComponent_Create(AV_View* v, EV_EditMethodCallData *d);

#endif // __gr_AbiGOComponentManager_h__
