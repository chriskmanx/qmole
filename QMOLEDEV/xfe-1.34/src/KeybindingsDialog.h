#ifndef KEYBINDINGSDIALOG_H
#define KEYBINDINGSDIALOG_H

#include "DialogBox.h"

class XComApp;

class KeybindingsDialog : public DialogBox
{
    FXDECLARE(KeybindingsDialog)
protected:
    FXLabel*	keylabel;
private:
    KeybindingsDialog()
    {
        ;
    }
public:
    KeybindingsDialog(FXWindow*,FXString,FXString,FXString,FXIcon *icon=NULL);
    virtual void create();
    long onCmdKeyPress(FXObject*,FXSelector,void*);
    FXString getKey()
    {
        return keylabel->getText();
    }
};
#endif
