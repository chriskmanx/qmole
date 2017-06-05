// File association tables. Taken from the FOX library and slightly modified.

#include "config.h"
#include "i18n.h"

#include <fx.h>
#include <FXPNGIcon.h>

#include "xfedefs.h"
#include "icons.h"
#include "xfeutils.h"
#include "FileDict.h"

#define COMMANDLEN   256
#define EXTENSIONLEN 128
#define MIMETYPELEN  64
#define ICONNAMELEN  256


// Object implementation
FXIMPLEMENT(IconDict,FXDict,NULL,0)


// Default icon path
const char IconDict::defaultIconPath[]=DEFAULTICONPATH;


// Build icon table
IconDict::IconDict(FXApp* app,const FXString& p):path(p)
{
	source=new FXIconSource(app);
}


// Search for the icon name along the search path, and try to load it
void *IconDict::createData(const void* ptr)
{
	return source->loadIconFile(FXPath::search(path,(const char*)ptr));
}


// Delete the icon
void IconDict::deleteData(void* ptr)
{
	delete ((FXIcon*)ptr);
}


// Save data
void IconDict::save(FXStream& store) const
{
	FXDict::save(store);
	store << source;
	store << path;
}


// Load data
void IconDict::load(FXStream& store)
{
	FXDict::load(store);
	store >> source;
	store >> path;
}


// Destructor
IconDict::~IconDict()
{
	delete source;
	source=(FXIconSource*)-1L;
	clear();
}


// These registry keys are used for default bindings.
const char FileDict::defaultExecBinding[]="defaultexecbinding";
const char FileDict::defaultDirBinding[]="defaultdirbinding";
const char FileDict::defaultFileBinding[]="defaultfilebinding";


// Object implementation
FXIMPLEMENT(FileDict,FXDict,NULL,0)


// Construct an file-extension association table
FileDict::FileDict(FXApp* a):app(a),settings(&a->reg())
{
    icons=new IconDict(a,settings->readStringEntry("SETTINGS","iconpath",DEFAULTICONPATH));
}


// Construct an file-extension association table, and alternative settings database
FileDict::FileDict(FXApp* a,FXSettings* db):app(a),settings(db)
{
    icons=new IconDict(a,settings->readStringEntry("SETTINGS","iconpath",DEFAULTICONPATH));
}


// Create new association from extension
void *FileDict::createData(const void* ptr)
{
	register const char *p=(const char*)ptr;
    register char *q;
    char command[COMMANDLEN];
    char extension[EXTENSIONLEN];
    char mimetype[MIMETYPELEN];
    char bigname[ICONNAMELEN];
    char bignameopen[ICONNAMELEN];
    char mininame[ICONNAMELEN];
    char mininameopen[ICONNAMELEN];
    FileAssoc *fileassoc;

	// Make association record
    fileassoc=new FileAssoc;
	
	// Parse command
    for(q=command; *p && *p!=';' && q<command+COMMANDLEN-1; *q++=*p++)
        ;
    *q='\0';

    // Skip section separator
    if(*p==';')
        p++;

    // Parse extension type
    for(q=extension; *p && *p!=';' && q<extension+EXTENSIONLEN-1; *q++=*p++)
        ;
    *q='\0';

    // Skip section separator
    if(*p==';')
        p++;

    // Parse big icon name
    for(q=bigname; *p && *p!=';' && *p!=':' && q<bigname+ICONNAMELEN-1; *q++=*p++)
        ;
    *q='\0';

    // Skip icon separator
    if(*p==':')
        p++;

    // Parse big open icon name
    for(q=bignameopen; *p && *p!=';' && q<bignameopen+ICONNAMELEN-1; *q++=*p++)
        ;
    *q='\0';

    // Skip section separator
    if(*p==';')
        p++;

    // Parse mini icon name
    for(q=mininame; *p && *p!=';' && *p!=':' && q<mininame+ICONNAMELEN-1; *q++=*p++)
        ;
    *q='\0';

    // Skip icon separator
    if(*p==':')
        p++;

    // Parse mini open icon name
    for(q=mininameopen; *p && *p!=';' && q<mininameopen+ICONNAMELEN-1; *q++=*p++)
        ;
    *q='\0';

    // Skip section separator
    if(*p==';')
        p++;

    // Parse mime type
    for(q=mimetype; *p && *p!=';' && q<mimetype+MIMETYPELEN-1; *q++=*p++)
        ;
    *q='\0';

    // Initialize association data
    fileassoc->command=command;
    fileassoc->extension=extension;
    fileassoc->bigicon=NULL;
    fileassoc->miniicon=NULL;
    fileassoc->bigiconopen=NULL;
    fileassoc->miniiconopen=NULL;
    fileassoc->mimetype=mimetype;
    fileassoc->dragtype=0;
    fileassoc->flags=0;

    // Insert icons into icon dictionary
    if(bigname[0])
        fileassoc->bigicon=fileassoc->bigiconopen=icons->insert(bigname);
    if(mininame[0])
        fileassoc->miniicon=fileassoc->miniiconopen=icons->insert(mininame);

    // Add open icons also; we will fall back on the regular icons in needed
    if(bignameopen[0])
        fileassoc->bigiconopen=icons->insert(bignameopen);
    if(mininameopen[0])
        fileassoc->miniiconopen=icons->insert(mininameopen);

    // Return the binding
    return fileassoc;
}


// Delete association
void FileDict::deleteData(void* ptr)
{
    delete ((FileAssoc*)ptr);
}


// Set icon search path
void FileDict::setIconPath(const FXString& path)
{

    // Replace iconpath setting in registry
    settings->writeStringEntry("SETTINGS","iconpath",path.text());

    // Change it in icon dictionary
    icons->setIconPath(path);
}


// Return current icon search path
FXString FileDict::getIconPath() const
{
    return icons->getIconPath();
}


// Replace or add file association
FileAssoc* FileDict::replace(const char* ext,const char* str)
{
    // Replace entry in registry
    settings->writeStringEntry("FILETYPES",ext,str);

    // Replace record
    return (FileAssoc*)FXDict::replace(ext,str);
}


// Remove file association
FileAssoc* FileDict::remove(const char* ext)
{
	// Delete registry entry for this type
    settings->deleteEntry("FILETYPES",ext);

    // Remove record
    FXDict::remove(ext);

    return NULL;
}


// Find file association using the lower case file extension
FileAssoc* FileDict::associate(const char* key)
{
    register const char* association;
    register FileAssoc* record;
	register char lowkey[MAXPATHLEN];
	
	// Convert the association key to lower case
	// Uses these functions because they seem to be faster than FXString
	strlcpy(lowkey,key,strlen(key)+1);
	strlow(lowkey);
	
    // See if we have an existing record already and stores the key extension
    if((record=find(lowkey))!=NULL)
	{
        record->key=lowkey;
		return record;
	}

    // See if this entry is known in FILETYPES
	association=settings->readStringEntry("FILETYPES",lowkey,"");

    // If not an empty string, make a record for it now and stores the key extension
    if(association[0])
	{
		record=(FileAssoc*)FXDict::insert(lowkey,association);
		record->key=lowkey;
        return record;
	}

    // Not a known file type
    return NULL;
}


// Find file association from registry
FileAssoc* FileDict::findFileBinding(const char* pathname)
{
    register const char *filename=pathname;
    register const char *p=pathname;
    register FileAssoc* record;
	
    while(*p)
    {
        if(ISPATHSEP(*p))
            filename=p+1;
        p++;
    }
	
	if (strlen(filename)==0)
		return associate(defaultFileBinding);

	record=associate(filename);

    if(record)
        return record;

    filename=strchr(filename,'.');
	
    while(filename)
    {
		if (strlen(filename)>1)
			record=associate(filename+1);
		if(record)
			return record;
		filename=strchr(filename+1,'.');
	}
    return associate(defaultFileBinding);
}


// Find directory association from registry
FileAssoc* FileDict::findDirBinding(const char* pathname)
{
    register const char* path=pathname;
    register FileAssoc* record;
    while(*path)
    {
        record=associate(path);
        if(record)
            return record;
        path++;
        while(*path && !ISPATHSEP(*path))
            path++;
    }
    return associate(defaultDirBinding);
}


// Find executable association from registry
FileAssoc* FileDict::findExecBinding(const char* pathname)
{
    return associate(defaultExecBinding);
}


// Destructor
FileDict::~FileDict()
{
    delete icons;
    clear();
    app=(FXApp*)-1;
    icons=(IconDict*)-1;
}
