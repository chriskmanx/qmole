#ifndef FILE_H
#define FILE_H


#include <fx.h>
#include "DialogBox.h"
#include "MessageBox.h"


// File operations
enum
{	
	COPY,
	RENAME,
	MOVE,
	SYMLINK,
	DELETE,
	CHMOD,
	CHOWN,
	EXTRACT,
	ARCHIVE,
#if defined(linux)
	PKG_INSTALL,
	PKG_UNINSTALL,
	MOUNT,
	UNMOUNT
#endif
};

// To search visited inodes
struct inodelist
{
    ino_t st_ino;
    inodelist *next;
};


class File : public DialogBox
{
    FXDECLARE(File)
protected:

	// Inline function
	// Force check of timeout for progress dialog (to avoid latency problems)
	int checkTimeout(void)
	{
		if (getApp()->hasTimeout(this,File::ID_TIMEOUT))
		{
			if (getApp()->remainingTimeout(this,File::ID_TIMEOUT)==0)
			{
				getApp()->removeTimeout(this,File::ID_TIMEOUT);
				show(PLACEMENT_OWNER);
					
				getApp()->forceRefresh();
				getApp()->flush();
				return 1;
			}
		}
		return 0;
	}

	void forceTimeout(void);
	void restartTimeout(void);
    FXlong fullread(int fd, unsigned char* ptr, FXlong len);
    FXlong fullwrite(int fd, const unsigned char* ptr, FXlong len);
	unsigned int getOverwriteAnswer(FXString, FXString);
    int copyfile(const FXString& source, const FXString& target, const FXbool preserve_date);
	int copyrec(const FXString& source,const FXString& target,inodelist* inodes, const FXbool preserve_date);
    int copydir(const FXString& source,const FXString& target,struct stat& parentstatus,inodelist* inodes, const FXbool preserve_date);
    int rchmod(char* path, char* file, mode_t mode, const FXbool dironly, const FXbool fileonly);
    int rchown(char* path, char* file, uid_t uid, gid_t gid, const FXbool dironly, const FXbool fileonly);
    FXLabel *uplabel;
    FXLabel *downlabel;
    FXProgressBar *progressbar;
	FXButton *cancelButton;
	FXbool overwrite;
	FXbool overwrite_all;
	FXbool skip_all;
	FXbool cancelled;
	MessageBox *mbox;	
public:
    File()
    {}
	~File();
    void create();
    File(FXWindow *owner, FXString title, const unsigned int operation);

	enum 
	{
		ID_CANCEL_BUTTON=DialogBox::ID_LAST,
		ID_TIMEOUT,
		ID_LAST
	};
	
	FXbool isCancelled()
	{
		return cancelled;		
	}
	
	void hideProgressDialog()
	{
		forceTimeout();
	}
	
	void showProgressDialog()
	{
		restartTimeout();
	}

	int copy(const FXString& source, const FXString& target, const FXbool confirm_dialog=TRUE, const FXbool preserve_date=TRUE);
    int rename(const FXString& source, const FXString& target);
    int move(const FXString& source, const FXString& target, const FXbool restore=FALSE);
    int symlink(const FXString& source, const FXString& target);
    int remove(const FXString& file);

    int chmod(char* path, char* file, mode_t mode, const FXbool rec, const FXbool dironly=FALSE, const FXbool fileonly=FALSE);
	int chown(char* path, char *file, uid_t uid, gid_t gid, const FXbool rec, const FXbool dironly=FALSE, const FXbool fileonly=FALSE);
	int extract(const FXString name, const FXString dir,const FXString cmd);
	int archive(const FXString name, const FXString cmd);

#if defined(linux)
	int mount(const FXString dir, const FXString msg, const FXString cmd, const  unsigned int op);
	int pkgInstall(const FXString name, const FXString cmd);
	int pkgUninstall(const FXString name, const FXString cmd);
#endif
	long onCmdCancel(FXObject*, FXSelector,void*);
    long onTimeout(FXObject*,FXSelector,void*);
};
#endif


