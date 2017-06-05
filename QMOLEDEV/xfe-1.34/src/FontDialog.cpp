// Font dialog. Taken from the FOX library and only modified for translation purpose

#include "config.h"
#include "i18n.h"

#include <fx.h>

#include "FontDialog.h"

// Map
FXDEFMAP(FontSelector) FontSelectorMap[]=
{
	FXMAPFUNC(SEL_COMMAND,FontSelector::ID_FAMILY,FontSelector::onCmdFamily),
	FXMAPFUNC(SEL_COMMAND,FontSelector::ID_WEIGHT,FontSelector::onCmdWeight),
	FXMAPFUNC(SEL_COMMAND,FontSelector::ID_SIZE,FontSelector::onCmdSize),
	FXMAPFUNC(SEL_COMMAND,FontSelector::ID_SIZE_TEXT,FontSelector::onCmdSizeText),
	FXMAPFUNC(SEL_COMMAND,FontSelector::ID_STYLE,FontSelector::onCmdStyle),
	FXMAPFUNC(SEL_COMMAND,FontSelector::ID_STYLE_TEXT,FontSelector::onCmdStyleText),
	FXMAPFUNC(SEL_COMMAND,FontSelector::ID_CHARSET,FontSelector::onCmdCharset),
	FXMAPFUNC(SEL_UPDATE,FontSelector::ID_CHARSET,FontSelector::onUpdCharset),
	FXMAPFUNC(SEL_COMMAND,FontSelector::ID_SETWIDTH,FontSelector::onCmdSetWidth),
	FXMAPFUNC(SEL_UPDATE,FontSelector::ID_SETWIDTH,FontSelector::onUpdSetWidth),
	FXMAPFUNC(SEL_COMMAND,FontSelector::ID_PITCH,FontSelector::onCmdPitch),
	FXMAPFUNC(SEL_UPDATE,FontSelector::ID_PITCH,FontSelector::onUpdPitch),
	FXMAPFUNC(SEL_UPDATE,FontSelector::ID_SCALABLE,FontSelector::onUpdScalable),
	FXMAPFUNC(SEL_COMMAND,FontSelector::ID_SCALABLE,FontSelector::onCmdScalable),
	FXMAPFUNC(SEL_UPDATE,FontSelector::ID_ALLFONTS,FontSelector::onUpdAllFonts),
	FXMAPFUNC(SEL_COMMAND,FontSelector::ID_ALLFONTS,FontSelector::onCmdAllFonts),
};


// Implementation
FXIMPLEMENT(FontSelector,FXPacker,FontSelectorMap,ARRAYNUMBER(FontSelectorMap))


FontSelector::FontSelector(FXComposite *p,FXObject* tgt,FXSelector sel,unsigned int opts,int x,int y,int w,int h):
        FXPacker(p,opts,x,y,w,h)
{
    target=tgt;
    message=sel;

    // Bottom side
    FXHorizontalFrame *buttons=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X);
    accept=new FXButton(buttons,_("&Accept"),NULL,NULL,0,BUTTON_INITIAL|BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT,0,0,0,0,20,20);
    cancel=new FXButton(buttons,_("&Cancel"),NULL,NULL,0,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT,0,0,0,0,20,20);

    // Left side
    FXMatrix *controls=new FXMatrix(this,3,LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FIX_HEIGHT,0,0,0,160, DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING, DEFAULT_SPACING,0);

    // Font families, to be filled later
    new FXLabel(controls,_("&Family:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_COLUMN);
    family=new FXTextField(controls,10,NULL,0,TEXTFIELD_READONLY|FRAME_SUNKEN|LAYOUT_FILL_X|LAYOUT_FILL_COLUMN);
    FXHorizontalFrame *familyframe=new FXHorizontalFrame(controls,FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_Y|LAYOUT_FILL_X|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW,0,0,0,0, 0,0,0,0);
    familylist=new FXList(familyframe,this,ID_FAMILY,LIST_BROWSESELECT|LAYOUT_FILL_Y|LAYOUT_FILL_X|HSCROLLER_NEVER|VSCROLLER_ALWAYS);

    // Initial focus on list
    familylist->setFocus();

    // Font weights
    new FXLabel(controls,_("&Weight:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_COLUMN);
    weight=new FXTextField(controls,4,NULL,0,TEXTFIELD_READONLY|FRAME_SUNKEN|LAYOUT_FILL_X|LAYOUT_FILL_COLUMN);
    FXHorizontalFrame *weightframe=new FXHorizontalFrame(controls,FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW|LAYOUT_FILL_COLUMN,0,0,0,0, 0,0,0,0);
    weightlist=new FXList(weightframe,this,ID_WEIGHT,LIST_BROWSESELECT|LAYOUT_FILL_Y|LAYOUT_FILL_X|HSCROLLER_NEVER|VSCROLLER_ALWAYS);

    // Font styles
    new FXLabel(controls,_("&Style:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_COLUMN);
    style=new FXTextField(controls,6,NULL,0,TEXTFIELD_READONLY|FRAME_SUNKEN|LAYOUT_FILL_X|LAYOUT_FILL_COLUMN);
    FXHorizontalFrame *styleframe=new FXHorizontalFrame(controls,FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW|LAYOUT_FILL_COLUMN,0,0,0,0, 0,0,0,0);
    stylelist=new FXList(styleframe,this,ID_STYLE,LIST_BROWSESELECT|LAYOUT_FILL_Y|LAYOUT_FILL_X|HSCROLLER_NEVER|VSCROLLER_ALWAYS);

    // Font sizes, to be filled later
    new FXLabel(controls,_("Si&ze:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_COLUMN);
    size=new FXTextField(controls,2,this,ID_SIZE_TEXT,FRAME_SUNKEN|LAYOUT_FILL_X|LAYOUT_FILL_COLUMN);
    FXHorizontalFrame *sizeframe=new FXHorizontalFrame(controls,FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW|LAYOUT_FILL_COLUMN,0,0,0,0, 0,0,0,0);
    sizelist=new FXList(sizeframe,this,ID_SIZE,LIST_BROWSESELECT|LAYOUT_FILL_Y|LAYOUT_FILL_X|HSCROLLER_NEVER|VSCROLLER_ALWAYS);

    FXMatrix *attributes=new FXMatrix(this,2,LAYOUT_SIDE_TOP|LAYOUT_FILL_X,0,0,0,0, DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING, DEFAULT_SPACING,0);

    // Character set choice
    new FXLabel(attributes,_("Character Set:"),NULL,LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN);
    charset=new FXComboBox(attributes,8,this,ID_CHARSET,COMBOBOX_STATIC|FRAME_SUNKEN|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN);
    charset->setNumVisible(10);
    charset->appendItem(_("Any"),(void*)0);
    charset->appendItem(_("West European"),(void*)FONTENCODING_WESTEUROPE);
    charset->appendItem(_("East European"),(void*)FONTENCODING_EASTEUROPE);
    charset->appendItem(_("South European"),(void*)FONTENCODING_SOUTHEUROPE);
    charset->appendItem(_("North European"),(void*)FONTENCODING_NORTHEUROPE);
    charset->appendItem(_("Cyrillic"),(void*)FONTENCODING_CYRILLIC);
    charset->appendItem(_("Arabic"),(void*)FONTENCODING_ARABIC);
    charset->appendItem(_("Greek"),(void*)FONTENCODING_GREEK);
    charset->appendItem(_("Hebrew"),(void*)FONTENCODING_HEBREW);
    charset->appendItem(_("Turkish"),(void*)FONTENCODING_TURKISH);
    charset->appendItem(_("Nordic"),(void*)FONTENCODING_NORDIC);
    charset->appendItem(_("Thai"),(void*)FONTENCODING_THAI);
    charset->appendItem(_("Baltic"),(void*)FONTENCODING_BALTIC);
    charset->appendItem(_("Celtic"),(void*)FONTENCODING_CELTIC);
    charset->appendItem(_("Russian"),(void*)FONTENCODING_KOI8);
    charset->appendItem(_("Central European (cp1250)"),(void*)FONTENCODING_CP1250);
    charset->appendItem(_("Russian (cp1251)"),(void*)FONTENCODING_CP1251);
    charset->appendItem(_("Latin1 (cp1252)"),(void*)FONTENCODING_CP1252);
    charset->appendItem(_("Greek (cp1253)"),(void*)FONTENCODING_CP1253);
    charset->appendItem(_("Turkish (cp1254)"),(void*)FONTENCODING_CP1254);
    charset->appendItem(_("Hebrew (cp1255)"),(void*)FONTENCODING_CP1255);
    charset->appendItem(_("Arabic (cp1256)"),(void*)FONTENCODING_CP1256);
    charset->appendItem(_("Baltic (cp1257)"),(void*)FONTENCODING_CP1257);
    charset->appendItem(_("Vietnam (cp1258)"),(void*)FONTENCODING_CP1258);
    charset->appendItem(_("Thai (cp874)"),(void*)FONTENCODING_CP874);
    charset->appendItem(_("UNICODE"),(void*)FONTENCODING_UNICODE);
    charset->setCurrentItem(0);

    // Set width
    new FXLabel(attributes,_("Set Width:"),NULL,LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN);
    setwidth=new FXComboBox(attributes,9,this,ID_SETWIDTH,COMBOBOX_STATIC|FRAME_SUNKEN|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN);
    setwidth->setNumVisible(10);
    setwidth->appendItem(_("Any"),(void*)0);
    setwidth->appendItem(_("Ultra condensed"),(void*)FXFont::UltraCondensed);
    setwidth->appendItem(_("Extra condensed"),(void*)FXFont::ExtraCondensed);
    setwidth->appendItem(_("Condensed"),(void*)FXFont::Condensed);
    setwidth->appendItem(_("Semi condensed"),(void*)FXFont::SemiCondensed);
    setwidth->appendItem(_("Normal"),(void*)FXFont::NonExpanded);
    setwidth->appendItem(_("Semi expanded"),(void*)FXFont::SemiExpanded);
    setwidth->appendItem(_("Expanded"),(void*)FXFont::Expanded);
    setwidth->appendItem(_("Extra expanded"),(void*)FXFont::ExtraExpanded);
    setwidth->appendItem(_("Ultra expanded"),(void*)FXFont::UltraExpanded);
    setwidth->setCurrentItem(0);

    // Pitch
    new FXLabel(attributes,_("Pitch:"),NULL,LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN);
    pitch=new FXComboBox(attributes,5,this,ID_PITCH,COMBOBOX_STATIC|FRAME_SUNKEN|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN);
    pitch->setNumVisible(3);
    pitch->appendItem(_("Any"),(void*)0);
    pitch->appendItem(_("Fixed"),(void*)FXFont::Fixed);
    pitch->appendItem(_("Variable"),(void*)FXFont::Variable);
    pitch->setCurrentItem(0);

    // Check for scalable
    new FXFrame(attributes,FRAME_NONE|LAYOUT_FILL_COLUMN);
    scalable=new FXCheckButton(attributes,_("Scalable:"),this,ID_SCALABLE,JUSTIFY_NORMAL|TEXT_BEFORE_ICON|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN);

    // Check for all (X11) fonts
    new FXFrame(attributes,FRAME_NONE|LAYOUT_FILL_COLUMN);
    allfonts=new FXCheckButton(attributes,_("All Fonts:"),this,ID_ALLFONTS,JUSTIFY_NORMAL|TEXT_BEFORE_ICON|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN);

    // Preview
    FXVerticalFrame *bottom=new FXVerticalFrame(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING,DEFAULT_SPACING, 0,0);
    new FXLabel(bottom,_("Preview:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X);
    FXHorizontalFrame *box=new FXHorizontalFrame(bottom,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_SUNKEN|FRAME_THICK,0,0,0,0, 0,0,0,0, 0,0);
    FXScrollWindow *scroll=new FXScrollWindow(box,LAYOUT_FILL_X|LAYOUT_FILL_Y);
    preview=new FXLabel(scroll,"ABCDEFGHIJKLMNOPQRSTUVWXYZ\nabcdefghijklmnopqrstuvwxyz\n0123456789",NULL,JUSTIFY_CENTER_X|JUSTIFY_CENTER_Y);
    preview->setBackColor(getApp()->getBackColor());

    strncpy(selected.face,"helvetica",sizeof(selected.face));
    selected.size=90;
    selected.weight=FXFont::Bold;
    selected.slant=0;
    selected.encoding=FONTENCODING_USASCII;
    selected.setwidth=0;
    selected.flags=0;
    previewfont=NULL;
}


// List the fonts when created
void FontSelector::create()
{
    FXPacker::create();
    listFontFaces();
    listWeights();
    listSlants();
    listFontSizes();
}


// Fill the list with face names
void FontSelector::listFontFaces()
{
    FXFontDesc *fonts;
    unsigned int numfonts,f;
    int selindex=-1;
    familylist->clearItems();
    family->setText("");
    if(FXFont::listFonts(fonts,numfonts,FXString::null,0,0,selected.setwidth,selected.encoding,selected.flags))
    {
        FXASSERT(0<numfonts);
        for(f=0; f<numfonts; f++)
        {
            familylist->appendItem(fonts[f].face,NULL,(void*)(FXuval)fonts[f].flags);
            if(strcmp(selected.face,fonts[f].face)==0) selindex=f;
        }
        if(selindex==-1) selindex=0;
        if(0<familylist->getNumItems())
        {
            familylist->setCurrentItem(selindex);
            familylist->makeItemVisible(selindex);
            family->setText(familylist->getItemText(selindex));
            strncpy(selected.face,familylist->getItemText(selindex).text(),sizeof(selected.face));
        }
        FXFREE(&fonts);
    }
}


// Fill the list with font weights
void FontSelector::listWeights()
{
    FXFontDesc *fonts;
    unsigned int numfonts,f,ww,lastww;
    const char *wgt;
    int selindex=-1;
    weightlist->clearItems();
    weight->setText("");
    if(FXFont::listFonts(fonts,numfonts,selected.face,0,0,selected.setwidth,selected.encoding,selected.flags))
    {
        FXASSERT(0<numfonts);
        lastww=0;
        for(f=0; f<numfonts; f++)
        {
            ww=fonts[f].weight;
            if(ww!=lastww)
            {

                // Get text for the weight
                switch(ww)
                {
                case FXFont::Thin:
                    wgt="thin";
                    break;
                case FXFont::ExtraLight:
                    wgt="extra light";
                    break;
                case FXFont::Light:
                    wgt="light";
                    break;
                case FXFont::Normal:
                    wgt="normal";
                    break;
                case FXFont::Medium:
                    wgt="medium";
                    break;
                case FXFont::DemiBold:
                    wgt="demibold";
                    break;
                case FXFont::Bold:
                    wgt="bold";
                    break;
                case FXFont::ExtraBold:
                    wgt="extra bold";
                    break;
                case FXFont::Black:
                    wgt="black";
                    break;
                default:
                    wgt="normal";
                    break;
                }

                // Add it
                weightlist->appendItem(_(wgt),NULL,(void*)(FXuval)ww);

                // Remember if this was the current selection
                if(selected.weight==ww) selindex=weightlist->getNumItems()-1;
                lastww=ww;
            }
        }
        if(selindex==-1) selindex=0;
        if(0<weightlist->getNumItems())
        {
            weightlist->setCurrentItem(selindex);
            weightlist->makeItemVisible(selindex);
            weight->setText(weightlist->getItemText(selindex));
            selected.weight=(unsigned int)(FXuval)weightlist->getItemData(selindex);
        }
        FXFREE(&fonts);
    }
}


// Fill the list with font slants
void FontSelector::listSlants()
{
    FXFontDesc *fonts;
    unsigned int numfonts,f,s,lasts;
    const char *slt;
    int selindex=-1;
    stylelist->clearItems();
    style->setText("");
    if(FXFont::listFonts(fonts,numfonts,selected.face,selected.weight,0,selected.setwidth,selected.encoding,selected.flags))
    {
        FXASSERT(0<numfonts);
        lasts=0;
        for(f=0; f<numfonts; f++)
        {
            s=fonts[f].slant;
            if(s!=lasts)
            {

                // Get text for the weight
                switch(s)
                {
                case FXFont::ReverseOblique:
                    slt="reverse oblique";
                    break;
                case FXFont::ReverseItalic:
                    slt="reverse italic";
                    break;
                case FXFont::Straight:
                    slt="regular";
                    break;
                case FXFont::Italic:
                    slt="italic";
                    break;
                case FXFont::Oblique:
                    slt="oblique";
                    break;
                default:
                    slt="normal";
                    break;
                }

                // Add it
                stylelist->appendItem(_(slt),NULL,(void*)(FXuval)s);

                // Remember if this was the current selection
                if(selected.slant == s) selindex=stylelist->getNumItems()-1;
                lasts=s;
            }
        }
        if(selindex==-1) selindex=0;
        if(0<stylelist->getNumItems())
        {
            stylelist->setCurrentItem(selindex);
            stylelist->makeItemVisible(selindex);
            style->setText(stylelist->getItemText(selindex));
            selected.slant=(unsigned int)(FXuval)stylelist->getItemData(selindex);
        }
        FXFREE(&fonts);
    }
}


// Fill the list with font sizes
void FontSelector::listFontSizes()
{
    const unsigned int sizeint[]=
        {
            60,80,90,100,110,120,140,160,200,240,300,360,420,480,640
        };
    FXFontDesc *fonts;
    unsigned int numfonts,f,s,lasts;
    int selindex=-1;
    sizelist->clearItems();
    size->setText("");
    FXString string;
    if(FXFont::listFonts(fonts,numfonts,selected.face,selected.weight,selected.slant,selected.setwidth,selected.encoding,selected.flags))
    {
        FXASSERT(0<numfonts);
        lasts=0;
        if(fonts[0].flags&FXFont::Scalable)
        {
            for(f=0; f<ARRAYNUMBER(sizeint); f++)
            {
                s=sizeint[f];
                string.format("%.1f",0.1*s);
                sizelist->appendItem(string,NULL,(void*)(FXuval)s);
                if(selected.size == s) selindex=sizelist->getNumItems()-1;
                lasts=s;
            }
        }
        else
        {
            for(f=0; f<numfonts; f++)
            {
                s=fonts[f].size;
                if(s!=lasts)
                {
                    string.format("%.1f",0.1*s);
                    sizelist->appendItem(string,NULL,(void*)(FXuval)s);
                    if(selected.size == s) selindex=sizelist->getNumItems()-1;
                    lasts=s;
                }
            }
        }
        if(selindex==-1) selindex=0;
        if(0<sizelist->getNumItems())
        {
            sizelist->setCurrentItem(selindex);
            sizelist->makeItemVisible(selindex);
            size->setText(sizelist->getItemText(selindex));
            selected.size=(unsigned int)(FXuval)sizelist->getItemData(selindex);
        }
        FXFREE(&fonts);
    }
}


// Preview
void FontSelector::previewFont()
{
    FXFont *old;

    // Save old font
    old=previewfont;

    // Get new font
    previewfont=new FXFont(getApp(),selected);

    // Realize new font
    previewfont->create();

    // Set new font
    preview->setFont(previewfont);

    // Delete old font
    delete old;
}


// Selected font family
long FontSelector::onCmdFamily(FXObject*,FXSelector,void* ptr)
{
    strncpy(selected.face,familylist->getItemText((int)(FXival)ptr).text(),sizeof(selected.face));
    family->setText(selected.face);
    listWeights();
    listSlants();
    listFontSizes();
    previewFont();
    return 1;
}


// Changed weight setting
long FontSelector::onCmdWeight(FXObject*,FXSelector,void* ptr)
{
    selected.weight=(unsigned int)(FXuval)weightlist->getItemData((int)(FXival)ptr);
    weight->setText(weightlist->getItemText((int)(FXival)ptr));
    listSlants();
    listFontSizes();
    previewFont();
    return 1;
}


// User clicked up directory button
long FontSelector::onCmdSize(FXObject*,FXSelector,void* ptr)
{
    selected.size=(unsigned int)(FXuval)sizelist->getItemData((int)(FXival)ptr);
    size->setText(sizelist->getItemText((int)(FXival)ptr));
    previewFont();
    return 1;
}


// User clicked up directory button
long FontSelector::onCmdSizeText(FXObject*,FXSelector,void*)
{
    selected.size=(unsigned int)(10.0*FXFloatVal(size->getText()));
    if(selected.size<60) selected.size=60;
    if(selected.size>2400) selected.size=2400;
    previewFont();
    return 1;
}


// User clicked up directory button
long FontSelector::onCmdStyle(FXObject*,FXSelector,void* ptr)
{
    selected.slant=(unsigned int)(FXuval)stylelist->getItemData((int)(FXival)ptr);
    style->setText(stylelist->getItemText((int)(FXival)ptr));
    listFontSizes();
    previewFont();
    return 1;
}


// Style type in
long FontSelector::onCmdStyleText(FXObject*,FXSelector,void*)
{
    return 1;
}


// Character set
long FontSelector::onCmdCharset(FXObject*,FXSelector,void*)
{
    int index=charset->getCurrentItem();
    unsigned int enc=(unsigned int)(FXuval)charset->getItemData(index);
    selected.encoding=(FXFontEncoding)enc;
    listFontFaces();
    listWeights();
    listSlants();
    listFontSizes();
    previewFont();
    return 1;
}


// Update character set
long FontSelector::onUpdCharset(FXObject*,FXSelector,void*)
{
    charset->setCurrentItem(charset->findItemByData((void*)(FXuval)selected.encoding));
    return 1;
}


// Changed set width
long FontSelector::onCmdSetWidth(FXObject*,FXSelector,void*)
{
    int index=setwidth->getCurrentItem();
    selected.setwidth=(unsigned int)(FXuval)setwidth->getItemData(index);
    listFontFaces();
    listWeights();
    listSlants();
    listFontSizes();
    previewFont();
    return 1;
}


// Update set width
long FontSelector::onUpdSetWidth(FXObject*,FXSelector,void*)
{
    setwidth->setCurrentItem(setwidth->findItemByData((void*)(FXuval)selected.setwidth));
    return 1;
}


// Changed pitch
long FontSelector::onCmdPitch(FXObject*,FXSelector,void*)
{
    int index=pitch->getCurrentItem();
    selected.flags&=~(FXFont::Fixed|FXFont::Variable);
    selected.flags|=(unsigned int)(FXuval)pitch->getItemData(index);
    listFontFaces();
    listWeights();
    listSlants();
    listFontSizes();
    previewFont();
    return 1;
}


// Update pitch
long FontSelector::onUpdPitch(FXObject*,FXSelector,void*)
{
    pitch->setCurrentItem((selected.flags&FXFont::Fixed) ? 1 : (selected.flags&FXFont::Variable) ? 2 : 0);
    return 1;
}


// Scalable toggle
long FontSelector::onCmdScalable(FXObject*,FXSelector,void* ptr)
{
    if(ptr) selected.flags|=FXFont::Scalable;
    else selected.flags&=~FXFont::Scalable;
    listFontFaces();
    listWeights();
    listSlants();
    listFontSizes();
    previewFont();
    return 1;
}


// Update scalable toggle
long FontSelector::onUpdScalable(FXObject*,FXSelector,void*)
{
    scalable->setCheck((selected.flags&FXFont::Scalable)!=0);
    return 1;
}


// All fonts toggle
long FontSelector::onCmdAllFonts(FXObject*,FXSelector,void* ptr)
{
    if(ptr) selected.flags|=FXFont::X11;
    else selected.flags&=~FXFont::X11;
    listFontFaces();
    listWeights();
    listSlants();
    listFontSizes();
    previewFont();
    return 1;
}


// Update all fonts toggle
long FontSelector::onUpdAllFonts(FXObject*,FXSelector,void*)
{
    allfonts->setCheck((selected.flags&FXFont::X11)!=0);
    return 1;
}


// Change font selection
void FontSelector::setFontSelection(const FXFontDesc& fontdesc)
{
    selected=fontdesc;

    // Validate these numbers
    if(selected.encoding>FONTENCODING_UNICODE)
    {
        selected.encoding=FONTENCODING_UNICODE;
    }
    if(selected.slant>FXFont::ReverseOblique)
    {
        selected.slant=FXFont::ReverseOblique;
    }
    if(selected.weight>FXFont::Black)
    {
        selected.weight=FXFont::Black;
    }
    if(selected.setwidth>FXFont::UltraExpanded)
    {
        selected.setwidth=FXFont::UltraExpanded;
    }
    if(selected.size>10000)
    {
        selected.size=10000;
    }

    // Under Windows, this should be OFF
    selected.flags&=~FXFont::X11;

    // Relist fonts
    listFontFaces();
    listWeights();
    listSlants();
    listFontSizes();

    // Update preview
    previewFont();
}


// Change font selection
void FontSelector::getFontSelection(FXFontDesc& fontdesc) const
{
    fontdesc=selected;
}


// Save data
void FontSelector::save(FXStream& store) const
{
    FXPacker::save(store);
    store << family;
    store << familylist;
    store << weight;
    store << weightlist;
    store << style;
    store << stylelist;
    store << size;
    store << sizelist;
    store << charset;
    store << setwidth;
    store << pitch;
    store << scalable;
    store << allfonts;
    store << accept;
    store << cancel;
    store << preview;
    store << previewfont;
}


// Load data
void FontSelector::load(FXStream& store)
{
    FXPacker::load(store);
    store >> family;
    store >> familylist;
    store >> weight;
    store >> weightlist;
    store >> style;
    store >> stylelist;
    store >> size;
    store >> sizelist;
    store >> charset;
    store >> setwidth;
    store >> pitch;
    store >> scalable;
    store >> allfonts;
    store >> accept;
    store >> cancel;
    store >> preview;
    store >> previewfont;
}


// Cleanup
FontSelector::~FontSelector()
{
    delete previewfont;
    family=(FXTextField*)-1L;
    familylist=(FXList*)-1L;
    weight=(FXTextField*)-1L;
    weightlist=(FXList*)-1L;
    style=(FXTextField*)-1L;
    stylelist=(FXList*)-1L;
    size=(FXTextField*)-1L;
    sizelist=(FXList*)-1L;
    charset=(FXComboBox*)-1L;
    setwidth=(FXComboBox*)-1L;
    pitch=(FXComboBox*)-1L;
    scalable=(FXCheckButton*)-1L;
    allfonts=(FXCheckButton*)-1L;
    preview=(FXLabel*)-1L;
    previewfont=(FXFont*)-1L;
    accept=(FXButton*)-1L;
    cancel=(FXButton*)-1L;
}


// Object implementation
FXIMPLEMENT(FontDialog,DialogBox,NULL,0)


// Separator item
FontDialog::FontDialog(FXWindow* owner,const FXString& name,unsigned int opts,int x,int y,int w,int h):
        DialogBox(owner,name,opts|DECOR_TITLE|DECOR_BORDER|DECOR_RESIZE,x,y,w,h,0,0,0,0,4,4)
{
    fontbox=new FontSelector(this,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y);
    fontbox->acceptButton()->setTarget(this);
    fontbox->acceptButton()->setSelector(DialogBox::ID_ACCEPT);
    fontbox->cancelButton()->setTarget(this);
    fontbox->cancelButton()->setSelector(DialogBox::ID_CANCEL);
}


// Save data
void FontDialog::save(FXStream& store) const
{
    DialogBox::save(store);
    store << fontbox;
}


// Load data
void FontDialog::load(FXStream& store)
{
    DialogBox::load(store);
    store >> fontbox;
}


// Change the selected font
void FontDialog::setFontSelection(const FXFontDesc& fontdesc)
{
    fontbox->setFontSelection(fontdesc);
}


// Return the selected font
void FontDialog::getFontSelection(FXFontDesc& fontdesc) const
{
    fontbox->getFontSelection(fontdesc);
}


// Cleanup
FontDialog::~FontDialog()
{
    fontbox=(FontSelector*)-1L;
}
