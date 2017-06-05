#ifndef HISTINPUTDIALOG_H
#define HISTINPUTDIALOG_H

#include "DialogBox.h"

// Browse types
enum
{
    HIST_INPUT_FILE,
	HIST_INPUT_EXECUTABLE_FILE,
	HIST_INPUT_FOLDER,
	HIST_INPUT_MIXED
};

class XComApp;

class ComboBox : public FXComboBox
{
    FXDECLARE(ComboBox)
private:
    ComboBox()
    {
        ;
    }
public:
	FXTextField *getTextEntry()
	{
		return field;
	}
	void CursorEnd()
	{
		field->onCmdCursorEnd(0,0,0);
        field->setFocus();
	}
    ComboBox(FXComposite *p,int cols,FXObject* tgt=NULL,FXSelector sel=0,unsigned int opts=COMBOBOX_NORMAL);
    virtual void create();
};

class HistInputDialog : public DialogBox
{
    FXDECLARE(HistInputDialog)
protected:
    FXVerticalFrame* contents;
    FXHorizontalFrame* buttons;
    FXHorizontalFrame* checkbutton;
    ComboBox*          input;
    FXLabel*           label;
    FXDataTarget*      string_target;
	unsigned int			   browsetype;
	FXString           initialdir;
private:
    HistInputDialog()
    {
        ;
    }
public:
    enum{
        ID_BROWSE_PATH=DialogBox::ID_LAST,
        ID_LAST
    };
    HistInputDialog(FXWindow*,FXString,FXString,FXString,FXString label="",FXIcon *ic=NULL, unsigned int browse=HIST_INPUT_FILE, FXbool option=FALSE, FXString=FXString::null);
    virtual void create();
    long onCmdKeyPress(FXObject*,FXSelector,void*);
	long onCmdBrowsePath(FXObject*,FXSelector,void*);
    FXString getText()
    {
        return input->getText();
    }
	void setText(const FXString& text)
	{
		input->setText(text);
	}
    void CursorEnd();
    void SelectAll();
    void appendItem(char *str)
    {
        input->appendItem(str);
    };
    void clearItems()
    {
        input->clearItems();
    };
    FXString getHistoryItem(int pos)
    {
        return input->getItemText(pos);
    }
    int getHistorySize()
    {
        return input->getNumItems();
    }
	void setDirectory(const FXString&);
};
#endif
