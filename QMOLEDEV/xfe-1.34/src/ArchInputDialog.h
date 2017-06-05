#ifndef ARCHINPUTDIALOG_H
#define ARCHINPUTDIALOG_H

#include "DialogBox.h"

class XComApp;

class ArchInputDialog : public DialogBox
{
    FXDECLARE(ArchInputDialog)
protected:
    FXTextField*       input;
    FXDataTarget*      string_target;
    FXHorizontalFrame* checkbutton;
	FXPopup*		   popup;
	FXOptionMenu*		optionmenu;
	FXOption* 			option_tgz;	
	FXOption* 			option_zip;	
	FXOption* 			option_7zip;	
	FXOption* 			option_tbz2;	
	FXOption* 			option_txz;	
	FXOption* 			option_tar;	
	FXOption* 			option_taz; 	
	FXOption* 			option_gz ;	
	FXOption* 			option_bz2;	
	FXOption* 			option_xz;	
	FXOption* 			option_z;	

private:
    ArchInputDialog()
    {
        ;
    }
public:
    enum{
        ID_BROWSE_PATH=DialogBox::ID_LAST,
		ID_FORMAT_TAR_GZ,
		ID_FORMAT_ZIP,
		ID_FORMAT_7ZIP,
		ID_FORMAT_TAR_BZ2,
		ID_FORMAT_TAR_XZ,
		ID_FORMAT_TAR,
		ID_FORMAT_TAR_Z,
		ID_FORMAT_GZ,
		ID_FORMAT_BZ2,
		ID_FORMAT_XZ,
		ID_FORMAT_Z,
        ID_LAST
    };
    ArchInputDialog(FXWindow*,FXString);
    virtual void create();
    virtual ~ArchInputDialog();
    long onCmdKeyPress(FXObject*,FXSelector,void*);
	long onCmdBrowsePath(FXObject*,FXSelector,void*);
	long onCmdOption(FXObject*,FXSelector,void*);
	long onUpdOption(FXObject*,FXSelector,void*);
    FXString getText()
    {
        return input->getText();
    }
	void setText(const FXString& text)
	{
		input->setText(text);
	}
    void SelectAll()
    {
        input->setSelection(0,(input->getText()).length());
    };
    void CursorEnd()
    {
        input->onCmdCursorEnd(0,0,0);
    }
};
#endif
