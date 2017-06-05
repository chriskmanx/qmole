#ifndef PROPERTIES_H
#define PROPERTIES_H

#include "DialogBox.h"

class PropertiesBox;

class PermFrame : public FXVerticalFrame
{
    FXDECLARE(PermFrame)
    friend class PropertiesBox;
private:
    PermFrame()
    {}
    FXCheckButton*	ur;
    FXCheckButton*	uw;
    FXCheckButton*	ux;
    FXCheckButton*	gr;
    FXCheckButton*	gw;
    FXCheckButton*	gx;
    FXCheckButton*	or_;
    FXCheckButton*	ow;
    FXCheckButton*	ox;
    FXCheckButton*	suid;
    FXCheckButton*	sgid;
    FXCheckButton*	svtx;
	FXDataTarget	cmd_radiotarget;
	FXDataTarget	flt_radiotarget;
	FXRadioButton*	set;
	FXRadioButton*	clear;
	FXRadioButton*	dironly;
	FXRadioButton*	fileonly;
	FXRadioButton*	all;
	FXRadioButton*	add;
	int			cmd;
	int			flt;
    FXCheckButton*	rec;
    FXCheckButton*	own;
	FXComboBox*		user;
    FXComboBox*		grp;
public:
    PermFrame(FXComposite *parent,FXObject *target);
};

class PropertiesBox : public DialogBox
{
    FXDECLARE(PropertiesBox)

private:
    PropertiesBox()
    {}
	FXLabel*	fileSize;
	FXLabel*	filesSize;
	FXLabel*	filesSizeDetails;
	FXLabel*	location;
	FXLabel*	origlocation;
	FXLabel*	linkto;
	FXLabel*	deletiondate;
    FXLabel*	ext;
    FXString*	files;
	FXLabel*	name_encoding;
    FXString 	source;
	FXString	parentdir;
	FXString	filename;
    FXString 	oldusr;
    FXString 	oldgrp;
    FXString	descr_prev;
    FXString	open_prev;
    FXString	view_prev;
    FXString	edit_prev;
    FXString	bigic_prev;
    FXString	miniic_prev;
    int 		num;
	FXString		trashfileslocation;
	FXString		trashinfolocation;
    FXCheckButton*    ur;
    FXCheckButton*    uw;
    FXCheckButton*    ux;
    FXCheckButton*    gr;
    FXCheckButton*    gw;
    FXCheckButton*    gx;
    FXCheckButton*    or_;
    FXCheckButton*    ow;
    FXCheckButton*    ox;
    FXbool			executable;
#ifdef STARTUP_NOTIFICATION
    FXCheckButton*	snbutton;
    FXGroupBox*		sngroup;
    FXbool			sndisable_prev;
#endif
    FXTextField*	input;
    FXTextField*	username;
    FXTextField*	grpname;
    FXTextField*	open;
    FXTextField*	view;
    FXTextField*	edit;
    FXTextField*	descr;
    FXTextField*	bigic;
    FXTextField*	miniic;
	FXbool			isDirectory;
	FXbool			isMountpoint;
	FXbool			recsize;
    mode_t			mode;
    mode_t			orig_mode;
    PermFrame*		perm;
public:
    enum{
        ID_ACCEPT_SINGLE=DialogBox::ID_LAST,
        ID_ACCEPT_MULT,
		ID_CANCEL,
        ID_SET,
        ID_CLEAR,
        ID_ADD,
		ID_DIRONLY,
		ID_FILEONLY,
		ID_SNDISABLE,
		ID_ALL,
        ID_BIG_ICON,
        ID_MINI_ICON,
        ID_BROWSE_OPEN,
        ID_BROWSE_VIEW,
        ID_BROWSE_EDIT,
		ID_RUSR,
        ID_WUSR,
        ID_XUSR,
        ID_RGRP,
        ID_WGRP,
        ID_XGRP,
        ID_ROTH,
        ID_WOTH,
        ID_XOTH,
		ID_SUID,
		ID_SGID,
		ID_SVTX,
        ID_LAST
    };

public:
    virtual void create();
    PropertiesBox(FXWindow *win,FXString file,FXString path);
    PropertiesBox(FXWindow *win,FXString *files,int num,FXString path);
    long onCmdAcceptSingle(FXObject*,FXSelector,void*);
    long onCmdAcceptMult(FXObject*,FXSelector,void*);
    long onCmdCancel(FXObject*,FXSelector,void*);
    long onCmdCheck(FXObject*,FXSelector,void*);
    long onCmdCommand(FXObject*,FXSelector,void*);
    long onCmdFilter(FXObject*,FXSelector,void*);
    long onCmdBrowseIcon(FXObject*,FXSelector,void*);
    long onCmdBrowse(FXObject*,FXSelector,void*);
	long onUpdSizeAndPerm(FXObject*,FXSelector,void*);
    long onCmdKeyPress(FXObject*,FXSelector,void*);
#ifdef STARTUP_NOTIFICATION
	long onUpdSnDisable(FXObject*,FXSelector,void*);
#endif
};

#endif
