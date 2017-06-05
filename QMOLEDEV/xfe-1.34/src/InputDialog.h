#ifndef INPUTDIALOG_H
#define INPUTDIALOG_H

#include "DialogBox.h"

class XComApp;

class InputDialog : public DialogBox
{
    FXDECLARE(InputDialog)
protected:
    FXTextField*       input;
    FXDataTarget*      string_target;
    FXHorizontalFrame* checkbutton;
private:
    InputDialog()
    {
        ;
    }
public:
    InputDialog(FXWindow*,FXString,FXString,FXString,FXString label="",FXIcon *icon=NULL, FXbool option=FALSE, FXString=FXString::null);
    virtual void create();
    long onCmdKeyPress(FXObject*,FXSelector,void*);
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
