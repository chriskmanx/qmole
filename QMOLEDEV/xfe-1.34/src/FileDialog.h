#ifndef FILEDIALOG_H
#define FILEDIALOG_H

#include "DialogBox.h"
#include "FileList.h"
#include "PathLinker.h"

class FileSelector;
class FileList;


// Additional mode for file selection : same as SELECTFILE_DIRECTORY but with the ability to also select files
enum {
		SELECT_FILE_ANY,             // A single file, existing or not (to save to)
		SELECT_FILE_EXISTING,        // An existing file (to load), but not '.' and '..'
		SELECT_FILE_MULTIPLE,        // Multiple existing files
		SELECT_FILE_MULTIPLE_ALL,    // Multiple existing files or directories, but not '.' and '..'
		SELECT_FILE_DIRECTORY,       // Existing directory, including '.' or '..'
		SELECT_FILE_MIXED,           // An existing file or directory, including '.' and '..'
	};


// File selection widget
class FXAPI FileSelector : public FXPacker
{
    FXDECLARE(FileSelector)
protected:
    FileList      *list;            // File list widget
    FXTextField   *filename;        // File name entry field
    FXComboBox    *filefilter;      // Combobox for pattern list
    FXCheckButton *readonly;        // Open file as read only
    FXButton      *accept;          // Accept button
    FXButton      *cancel;          // Cancel button
    unsigned int         selectmode;      // Select mode
	FXArrowButton   *btnbackhist;     // Back history
	FXArrowButton   *btnforwardhist;  // Forward history
    PathLinker*		  pathlink;
	TextLabel*	      pathtext;
protected:
    FileSelector()
    {}
    virtual void create();
    static FXString patternFromText(const FXString& pattern);
    static FXString extensionFromPattern(const FXString& pattern);
private:
    FileSelector(const FileSelector&);
    FileSelector &operator=(const FileSelector&);
public:
    long onCmdAccept(FXObject*,FXSelector,void*);
    long onCmdFilter(FXObject*,FXSelector,void*);
    long onCmdItemDoubleClicked(FXObject*,FXSelector,void*);
    long onCmdItemClicked(FXObject*,FXSelector,void*);
    long onCmdItemSelected(FXObject*,FXSelector,void*);
    long onCmdItemDeselected(FXObject*,FXSelector,void*);
    long onCmdDirUp(FXObject*,FXSelector,void*);
    long onUpdDirUp(FXObject*,FXSelector,void*);
	long onCmdDirBack(FXObject*,FXSelector,void*);
    long onUpdDirBack(FXObject*,FXSelector,void*);
 	long onCmdDirForward(FXObject*,FXSelector,void*);
    long onUpdDirForward(FXObject*,FXSelector,void*);
	long onCmdDirBackHist(FXObject*,FXSelector,void*);
    long onUpdDirBackHist(FXObject*,FXSelector,void*);
 	long onCmdDirForwardHist(FXObject*,FXSelector,void*);
    long onUpdDirForwardHist(FXObject*,FXSelector,void*);
    long onCmdHome(FXObject*,FXSelector,void*);
    long onCmdWork(FXObject*,FXSelector,void*);
	long onCmdNewDir(FXObject*,FXSelector,void*);
	long onCmdNewFile(FXObject*,FXSelector,void*);
	long onCmdPopupMenu(FXObject*,FXSelector,void*);
public:
    enum {
        ID_FILEFILTER=FXPacker::ID_LAST,
        ID_ACCEPT,
        ID_FILELIST,
        ID_DIR_UP,
		ID_DIR_BACK,
		ID_DIR_FORWARD,
 		ID_DIR_BACK_HIST,
		ID_DIR_FORWARD_HIST,
        ID_HOME,
        ID_WORK,
		ID_NEWDIR,
		ID_NEWFILE,
        ID_LAST
    };
public:

    // Constructor
    FileSelector(FXComposite *p,FXObject* tgt=NULL,FXSelector sel=0,unsigned int opts=0,int x=0,int y=0,int w=0,int h=0);

    // Return a pointer to the "Accept" button
    FXButton *acceptButton() const
    {
        return accept;
    }

    // Return a pointer to the "Cancel" button
    FXButton *cancelButton() const
    {
        return cancel;
    }

    // Change file name
    void setFilename(const FXString& path);

    // Return file name, if any
    FXString getFilename() const;

    // Return array of strings containing the selected file names, terminated
    // by an empty string.  This string array must be freed using delete [].
    // If no files were selected, a NULL is returned.
    FXString* getFilenames() const;

    // Change file pattern
    void setPattern(const FXString& ptrn);

    // Return file pattern
    FXString getPattern() const;

    // Change the list of file patterns shown in the file dialog.
    // Each pattern comprises an optional name, followed by a pattern in
    // parentheses.  The patterns are separated by newlines.
    // For example,
    //
    //  "*\n*.cpp,*.cc\n*.hpp,*.hh,*.h"
    //
    // and
    //
    //  "All Files (*)\nC++ Sources (*.cpp,*.cc)\nC++ Headers (*.hpp,*.hh,*.h)"
    //
    // will set the same three patterns, but the former shows no pattern names.
    void setPatternList(const FXString& patterns);

    // Set list of patterns as name,pattern pairs.
    // The list should be terminated with a final NULL string.
    // (DEPRECATED)
    void setPatternList(const char **ptrns);

    // Return list of patterns
    FXString getPatternList() const;

    // After setting the list of patterns, this call will
    // initially select pattern n as the active one.
    void setCurrentPattern(int n);

    // Return current pattern number
    int getCurrentPattern() const;

    // Get pattern text for given pattern number
    FXString getPatternText(int patno) const;

    // Change pattern text for pattern number
    void setPatternText(int patno,const FXString& text);

    // Change directory
    void setDirectory(const FXString& path);

    // Return directory
    FXString getDirectory() const;

    // Set the inter-item spacing (in pixels)
    void setItemSpace(int s);

    // Return the inter-item spacing (in pixels)
    int getItemSpace() const;

    // Change file list style
    void setFileBoxStyle(unsigned int style);

    // Return file list style
    unsigned int getFileBoxStyle() const;

    // Change file selection mode
    void setSelectMode(unsigned int mode);

    // Return file selection mode
    unsigned int getSelectMode() const
    {
        return selectmode;
    }

    // Show readonly button
    void showReadOnly(FXbool show);

    // Return TRUE if readonly is shown
    FXbool shownReadOnly() const;

    // Set initial state of readonly button
    void setReadOnly(FXbool state);

    // Get readonly state
    FXbool getReadOnly() const;

    // Return TRUE if hidden files are shown
	FXbool shownHiddenFiles() const;

    // Return TRUE if thumbnails are shown
	FXbool shownThumbnails() const;

	// Change show hidden files mode
	void showHiddenFiles(FXbool shown);

	// Change show thumbnails files mode
	void showThumbnails(FXbool shown);

	// Destructor
    virtual ~FileSelector();
};


// File Dialog object
class FXAPI FileDialog : public DialogBox
{
    FXDECLARE(FileDialog)
protected:
    FileSelector *list;
protected:
    FileDialog()
    {}
private:
    FileDialog(const FileDialog&);
    FileDialog &operator=(const FileDialog&);
public:

    // Construct File Dialog Box
    FileDialog(FXWindow* owner,const FXString& name,unsigned int opts=0,int x=0,int y=0,int w=500,int h=300);

    // Change file name
    void setFilename(const FXString& path);

	// Return file name, if any
    FXString getFilename() const;

    // Return empty-string terminated list of selected file names, or NULL if none selected
    FXString* getFilenames() const;

    // Change file pattern
    void setPattern(const FXString& ptrn);

    // Return file pattern
    FXString getPattern() const;

    // Change the list of file patterns shown in the file dialog.
    // Each pattern comprises an optional name, followed by a pattern in
    // parentheses.  The patterns are separated by newlines.
    // For example,
    //
    //  "*\n*.cpp,*.cc\n*.hpp,*.hh,*.h"
    //
    // and
    //
    //  "All Files (*)\nC++ Sources (*.cpp,*.cc)\nC++ Headers (*.hpp,*.hh,*.h)"
    //
    // will set the same three patterns, but the former shows no pattern names.
    void setPatternList(const FXString& patterns);

    // Set list of patterns as name,pattern pairs.
    // The list should be terminated with a final NULL string.
    // (DEPRECATED)
    void setPatternList(const char **ptrns);

    // Return list of patterns
    FXString getPatternList() const;

    // After setting the list of patterns, this call will
    // initially select pattern n as the active one.
    void setCurrentPattern(int n);

    // Return current pattern number
    int getCurrentPattern() const;

    // Get pattern text for given pattern number
    FXString getPatternText(int patno) const;

    // Change pattern text for pattern number
    void setPatternText(int patno,const FXString& text);

    // Change directory
    void setDirectory(const FXString& path);

    // Return directory
    FXString getDirectory() const;

    // Set the inter-item spacing (in pixels)
    void setItemSpace(int s);

    // Return the inter-item spacing (in pixels)
    int getItemSpace() const;

    // Change File List style
    void setFileBoxStyle(unsigned int style);

    // Return File List style
    unsigned int getFileBoxStyle() const;

    // Change file selection mode
    void setSelectMode(unsigned int mode);

    // Return file selection mode
    unsigned int getSelectMode() const;

    // Show readonly button
    void showReadOnly(FXbool show);

    // Return TRUE if readonly is shown
    FXbool shownReadOnly() const;
	
    // Return TRUE if hidden files are shown
	FXbool shownHiddenFiles() const;

    // Return TRUE if thumbnails are shown
	FXbool shownThumbnails() const;

	// Change show hidden files mode
	void showHiddenFiles(FXbool shown);

	// Change show thumbnails files mode
	void showThumbnails(FXbool shown);

	// Set initial state of readonly button
    void setReadOnly(FXbool state);

    // Get readonly state
    FXbool getReadOnly() const;

    // Open existing filename
    static FXString getOpenFilename(FXWindow* owner,const FXString& caption,const FXString& path,const FXString& patterns="*",int initial=0);

    // Open multiple existing files
    static FXString* getOpenFilenames(FXWindow* owner,const FXString& caption,const FXString& path,const FXString& patterns="*",int initial=0);

    // Save to filename
    static FXString getSaveFilename(FXWindow* owner,const FXString& caption,const FXString& path,const FXString& patterns="*",int initial=0);

    // Open directory name
    static FXString getOpenDirectory(FXWindow* owner,const FXString& caption,const FXString& path);

    // Destructor
    virtual ~FileDialog();
};


#endif
