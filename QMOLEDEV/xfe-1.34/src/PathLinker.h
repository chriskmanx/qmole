#ifndef PATHLINKER_H
#define PATHLINKER_H

#include <vector>
#include "TextLabel.h"
#include "FileList.h"
#include "DirPanel.h"


class FXAPI PathLinker : public FXHorizontalFrame
{
    FXDECLARE(PathLinker)

public:
#define MAX_LINKS 40
    enum ButtonIds
	{
        ID_START_LINK = FXHorizontalFrame::ID_LAST,
        // Note: Place any additional id's AFTER ID_END_LINK
        ID_END_LINK = ID_START_LINK + MAX_LINKS - 1,
		ID_FOCUS_BUTTON,
        ID_LAST
    };

    PathLinker(FXComposite* a, FileList* flist, DirList* dlist=NULL, unsigned int opts=0);
	PathLinker()
	{}
    virtual void create();
    virtual ~PathLinker();
    long pathButtonPressed(FXObject*, FXSelector, void*);
	long onCmdFocusButton(FXObject*, FXSelector, void*);
	long onUpdPath(FXObject*, FXSelector, void*);
    void setPath(FXString);
	void focus();
	void unfocus();
protected:
    typedef std::vector<FXButton*> vector_FXButton;
    vector_FXButton linkButtons;
    unsigned int nbActiveButtons;
    unsigned int currentButton;
    FXString visitedPath;
	TextLabel* focusButton;
	FXFont* normalFont;
	FXFont* highlightFont;
	FileList* filelist;
	DirList* dirlist;
private:
    void updatePath(FXString, unsigned int);
    void setText(unsigned int, FXString);
};

#endif
