#ifndef FILEDICT_H
#define FILEDICT_H

#include "xfedefs.h"

// Registers stuff to know about the extension
struct FileAssoc
{
    FXString   key;               // Key extension (ex: zip, cpp, ...)	
	FXString   command;           // Command to execute
    FXString   extension;         // Full extension name (ex: ZIP Archive, C++ Source, ...)
    FXString   mimetype;          // Mime type name
    FXIcon    *bigicon;           // Big normal icon
    FXIcon    *bigiconopen;       // Big open icon
    FXIcon    *miniicon;          // Mini normal icon
    FXIcon    *miniiconopen;      // Mini open icon
    FXDragType dragtype;          // Registered drag type
    unsigned int     flags;             // Flags
};


// Icon dictionary
class FXAPI IconDict : public FXDict
{
    FXDECLARE(IconDict)
private:
    FXApp        *app;            // Application object
  	FXIconSource *source;         // Icon source
    FXString      path;           // Where to search icons
protected:
  	IconDict():source(NULL)
  	{}
    virtual void *createData(const void*);
    virtual void deleteData(void*);
private:
    IconDict(const IconDict&);
    IconDict &operator=(const IconDict&);

public:

  // Default icon search path
	static const char defaultIconPath[];

public:

    // Construct an icon dictionary, with given path
    IconDict(FXApp* a,const FXString& p=defaultIconPath);

    // Get application
    FXApp* getApp() const
    {
        return app;
    }

  	// Change icon source
  	void setIconSource(FXIconSource *src)
  	{
  		source=src;
  	}

  	// Return icon source
  	FXIconSource* getIconSource() const
  	{
  		return source;
  	}

    // Set icon search path
    void setIconPath(const FXString& p)
    {
        path=p;
    }

    // Return current icon search path
    FXString getIconPath() const
    {
        return path;
    }

    // Insert unique icon loaded from filename into dictionary
    FXIcon* insert(const char* name)
    {
        return (FXIcon*)FXDict::insert(name,name);
    }

    // Remove icon from dictionary
    FXIcon* remove
        (const char* name)
    {
        return (FXIcon*)FXDict::remove
                   (name);
    }

    // Find icon by name
    FXIcon* find(const char* name)
    {
        return (FXIcon*)FXDict::find(name);
    }

	// Save to stream
	virtual void save(FXStream& store) const;

	// Load from stream
	virtual void load(FXStream& store);
    
    // Destructor
    virtual ~IconDict();
};


/*
* The File Association dictionary associates a file extension
* with a FileAssoc record which contains command name, mime type,
* icons, and other information about the file type.
* The Registry is used as source of the file bindings; an alternative
* Settings database may be specified however.
*/
class FXAPI FileDict : public FXDict
{
    FXDECLARE(FileDict)
private:
    FXApp        *app;            // Application object
    FXSettings   *settings;       // Settings database where to get bindings
    IconDict   *icons;            // Icon table
protected:
    FileDict()
    {}
    virtual void *createData(const void*);
    virtual void deleteData(void*);
private:
    FileDict(const FileDict&);
    FileDict &operator=(const FileDict&);
public:

    // Registry key used to find fallback executable icons
    static const char defaultExecBinding[];

    // Registry key used to find fallback directory icons
    static const char defaultDirBinding[];

    // Registry key used to find fallback document icons
    static const char defaultFileBinding[];
public:

    /*
    * Construct a dictionary mapping file-extension to file associations,
    * using the application registry settings as a source for the bindings.
    */
    FileDict(FXApp* a);

    /*
    * Construct a dictionary mapping file-extension to file associations,
    * using the specified settings database as a source for the bindings.
    */
    FileDict(FXApp* a,FXSettings* db);

    // Get application
    FXApp* getApp() const
    {
        return app;
    }


	// Change icon dictionary
  	void setIconDict(IconDict *icns)
  	{
  		icons=icns;
  	}

  	// Return icon dictionary
  	IconDict* getIconDict() const
  	{
  		return icons;
  	}

    // Set icon search path
    void setIconPath(const FXString& path);

    // Return current icon search path
    FXString getIconPath() const;

    /*
    * Replace file association.
    * The new association is written into the settings database under the
    * FILETYPES section; the format of the association is as follows:
    *
    * <extension> = "<command> ; <type> ; <bigicon> [ : <bigopenicon> ] ; <smallicon> [ : <smalliconopen> ] ; <mimetype>"
    *
    * Where <command> is the command used to launch the application (e.g. "xv %s &"),
    * and <type> is the file type string (e.g. "GIF Image"),
    * <bigicon> and <bigiconopen> are the large icons shown in "Icons" mode,
    * <smallicon> and <smalliconopen> are the small icons shown in "Details" mode,
    * and <mimetype> is the RFC2045 mime type of the file.
    *
    * For example:
    *
    * [FILETYPES]
    * gif="xv %s &;GIF Image;big.xpm:bigopen.xpm;mini.xpm:miniopen.xpm;image/gif"
    * /home/jeroen=";Home;home.xpm;minihome.xpm;application/x-folder"
    *
    */
    FileAssoc* replace(const char* ext,const char* str);

    // Remove file association
    FileAssoc* remove (const char* ext);

    // Find file association already in dictionary
    FileAssoc* find(const char* ext)
    {
        return (FileAssoc*)FXDict::find(ext);
    }

    // Find file association from registry
    FileAssoc* associate(const char* key);

    /*
    * Determine binding for the given file.
    * The default implementation tries the whole filename first,
    * then tries the extensions.
    * For example, for a file "source.tar.gz":
    *
    *  "source.tar.gz",
    *  "tar.gz",
    *  "gz"
    *
    * are tried in succession.  If no association is found the
    * key "defaultfilebinding" is tried as a fallback association.
    * A NULL is returned if no association of any kind is found.
    */
    virtual FileAssoc* findFileBinding(const char* pathname);

    /*
    * Find directory binding from registry.
    * The default implementation tries the whole pathname first,
    * then tries successively smaller parts of the path.
    * For example, a pathname "/usr/people/jeroen":
    *
    *   "/usr/people/jeroen"
    *   "/people/jeroen"
    *   "/jeroen"
    *
    * are tried in succession.  If no bindings are found, the
    * key "defaultdirbinding" is tried as a fallback association.
    * A NULL is returned if no association of any kind is found.
    */
    virtual FileAssoc* findDirBinding(const char* pathname);

    /*
    * Determine binding for the given executable.
    * The default implementation returns the fallback binding associated with
    * the key "defaultexecbinding".
    * A NULL is returned if no association of any kind is found.
    */
    virtual FileAssoc* findExecBinding(const char* pathname);

    // Destructor
    virtual ~FileDict();
};


#endif
